{-# LANGUAGE DataKinds, DeriveGeneric, ExtendedDefaultRules, GADTs, KindSignatures, MultiWayIf, ScopedTypeVariables, StandaloneDeriving, QuasiQuotes, UnicodeSyntax, ViewPatterns #-}

-- | A module for declarative definition of complex build systems.
-- Ruin tries to provide the build system engineer with a language closer to what
-- he himself might use in his head.

module Development.Ruin
    (
    -- * Target environment
      Arch, OS(..), OSType, OSVersion, Plat(..)

    -- * Source, intermediate & destination file types
    , Type, type_fusing_p, type_extension, retype_file
    , Files(..)

    -- * Transformations
    , Tool(..), exec_tool, ToolKind, ToolAction
    , Chain(..), ChainName, ChainLink(..)

    -- * Context
    , Ctx, CtxExp(..), CtxVal(..), ctxValXInputsP, ctxValXFlagsP
    , CtxMap(..)
    , ctxR, ctxN, ctxRCase, ctxNCase, ctxRIf, ctxNIf, ctxRWhen, ctxNWhen

    -- * Components
    , Component(..), Tag, CompMap(..)
    , Buildable(..), compute_buildables, buildable_output

    -- * Composition
    , Slice(..), Schema(..)

    -- * Parametrisation
    , BuildVar(..), var_stringify
    , VarMap(..), VarEnv(..), VarSpec(..), OptSpec, derive_merged_optmap, derive_optdescrs
    , varspec_add

    -- * Util
    , exec, shell, unsafeExec, unsafeShell, cmd2
    , enHashMap
    , lcstr, lcShow
    ) where

import GHC.Generics (Generic)

import Data.Hashable
import Data.Char (toLower)
import Data.List (intercalate)
import qualified Data.HashMap.Lazy as H
import Data.HashMap.Lazy (HashMap, elems, fromList, toList, (!))

import Development.Shake
import Development.Shake.Command()
import Development.Shake.FilePath

-- import Prelude.Unicode

import System.Process (rawSystem)
import System.Console.GetOpt (OptDescr(..), ArgDescr(..))
import System.Path.Glob (glob)

import System.Process (readProcess, readProcessWithExitCode)

import System.IO.Unsafe (unsafePerformIO)

import Text.PrettyPrint.GenericPretty (pretty, Out(..), doc)
import Text.PrettyPrint (parens, (<+>), ($$), (<>), text)
import Text.Printf (printf)
import Data.Maybe (fromMaybe)

---
--- Silliness
---
eitherConst :: a -> a -> Either b c -> a
eitherConst l r e = either (const l) (const r) e

lookup_chain :: (Hashable a, Eq a) => HashMap a b -> HashMap a b -> a -> Maybe b
lookup_chain primary secondary k = case H.lookup k primary of
                                     Nothing -> H.lookup k secondary
                                     Just v  -> Just v

enHashMap :: (Hashable b, Eq b) => (a -> b) -> [a] -> HashMap b a
enHashMap keyfn xs = fromList $ map (\ x -> (keyfn x, x)) xs

lcstr :: String -> String
lcstr cs = map toLower cs

lcShow :: Show x => x -> String
lcShow x = lcstr $ show x

exec :: String -> [String] -> IO String
exec executable args = readProcess executable args []

shell :: String -> IO String
shell comm = do
  (_, out, err) <- readProcessWithExitCode "sh" ["-c", comm] []
  return $ out ++ err

cmd2 :: String -> [String] -> Action ()
cmd2 comm args = do
    _ <- liftIO $ rawSystem comm args
    return ()

unsafeExec :: String -> [String] -> String
unsafeExec executable args = unsafePerformIO (exec executable args)

unsafeShell :: String -> String
unsafeShell comm = unsafePerformIO (shell comm)

instance (Out a, Out b) => Out (HashMap a b) where
  doc hm = parens $ foldl (\acc (k, v) -> acc $$ doc k <+> text ":" <+> doc v) (text "") $ toList hm
  docPrec _ = doc


--- Source & result specification
class (Eq a, Hashable a, Out a, Show a) => Type a where
    type_fusing_p  :: a -> Bool
    type_extension :: a -> String

retype_file :: Type a => a -> String -> String
retype_file newty f = f -<.> type_extension newty


--- Target specification
class (Eq a, Show a, Generic a, Out a) => Arch a
class (Eq a, Show a, Generic a, Out a) => OSType a where
class (Eq a, Show a, Generic a, Out a) => OSVersion a

data OS a b where
    OS :: (OSType a, OSVersion b) => a -> (Maybe b) -> OS a b

deriving instance Show (OS a b)
instance (Out a, Out b) => Out (OS a b) where
  doc (OS ostype osver) = doc ostype <> text "-" <> case osver of
                                                      Nothing  -> text "*-*"
                                                      Just ver -> doc ver
  docPrec _ = doc
instance Eq  (OS a b) where -- treat Nothing as a wildcard during comparison:
    (OS t1 Nothing) == (OS t2 _)       = t1 == t2
    (OS t1 _)       == (OS t2 Nothing) = t1 == t2
    (OS t1 v1)      == (OS t2 v2)      = t1 == t2 && v1 == v2

data Plat a b c where
    Plat :: (Arch a, OSType b, OSVersion c) => a -> OS b c -> Plat a b c
deriving instance Show (Plat a b c)
instance (Out a, Out b, Out c) => Out (Plat a b c) where
  doc (Plat arch os) = doc arch <> text "-" <> doc os
  docPrec _ = doc
instance Eq  (Plat a b c) where
    (Plat a1 os1)   == (Plat a2 os2)   = a1 == a2 && os1 == os2


--- Source -> target transformation
class (Eq a, Hashable a, Out a, Show a) => ToolKind a

type ToolAction = String -> (Either String [String]) -> [String] -> Action ()

data Tool a b c d e =
    Tool a b b [(Plat c d e, Plat c d e, String)] (Either (String -> String -> String -> [String] -> Action ())
                                                          (String -> String -> [String] -> [String] -> Action ())) deriving (Generic)

instance (Out a, Out b) => Out (Tool a b c d e) where
    doc (Tool kind fty toty _ _) = text "Tool" <+> parens (text "xform:" <+> doc fty <> text "->" <> doc toty <+> doc kind)
    docPrec _ = doc

exec_tool :: (ToolKind a, Type b) => [Tool a b c d e] -> a -> (b, b) -> (Plat c d e, Plat c d e) -> String -> (Either String [String]) -> [String] -> Action ()
exec_tool available_tools want_toolkind (want_tyfrom, want_tyto) (this_plat, for_plat) out ins flags =
    case [ (c, tool_exec)
         | c@(Tool toolkind tyfrom tyto tool_variants _) <- available_tools
         , (on, for, tool_exec)                          <- tool_variants
         , want_toolkind == toolkind && tyfrom == want_tyfrom && tyto == want_tyto && on == this_plat && for == for_plat ] of
      []  -> error (printf "Failed to find a suitable tool: on-plat=%s type=%s to-plat=%s" (lcShow this_plat) (lcShow want_tyfrom) (lcShow for_plat))
      (Tool toolkind _ _ _ fnV, tool_exec):_ ->
          case (fnV, ins) of
            (Left  fn, Left  arg)  -> fn tool_exec out arg flags
            (Right fn, Right args) -> fn tool_exec out args flags
            _                      -> error "Tool/inputs arity mismatch: used %s on an %sary input" (show toolkind) $ eitherConst "un" "multi" ins

-- Chains
-- Because of ambiguities of composition (f.e. both GCCLD and LD can perform CObj -> Executable transforms)
-- we need hints.
-- A Chain is what provides these hints.

class (Eq a, Hashable a, Out a, Show a) => ChainName a

data Chain typ tkind where
    Chain :: (Type typ, ToolKind tkind) => typ -> tkind -> [Chain typ tkind] -> Chain typ tkind -- ProductType -> ApplicableTool -> Children
deriving instance Eq (Chain typ tkind)
deriving instance Show (Chain typ tkind)

data ChainLink typ tkind =
    ChainLink [String] typ String typ tkind ToolAction


-- A condition for extending the build environment
class (Eq a, Hashable a, Out a, Show a) => Tag a

data CtxExp a b c d e where
    AsPartOf  :: Tag a => a -> CtxExp a b c d e
    ForArch   :: Tag a => c -> CtxExp a b c d e
    ForOSType :: Tag a => d -> CtxExp a b c d e
    ForOS     :: Tag a => OS d e -> CtxExp a b c d e
    ForPlat   :: Tag a => Plat c d e -> CtxExp a b c d e
    ForInput  :: Tag a => String -> CtxExp a b c d e                       -- Filename wildcard
    WithTool  :: Tag a => b -> CtxExp a b c d e
    ShellTest :: Tag a => String -> String -> CtxExp a b c d e             -- WARNING: the command is expected to be pure!
    ExecTest  :: Tag a => String -> [String] -> String -> CtxExp a b c d e -- WARNING: ^^^
    Not       :: Tag a => (CtxExp a b c d e) -> CtxExp a b c d e
    And       :: Tag a => [CtxExp a b c d e] -> CtxExp a b c d e
    Or        :: Tag a => [CtxExp a b c d e] -> CtxExp a b c d e
    Always    :: Tag a => CtxExp a b c d e

deriving instance (Tag a, ToolKind b, Arch c, OSType d, OSVersion e) => Show (CtxExp a b c d e)

-- An atom of build environment
data CtxVal a =
    XFlags  a [String] |          -- type flags
    XInputs (Files a)
    deriving (Eq, Show)

data Files a =
    Files     a String [String] | -- type srcRoot wildcards-as-per-System.Path.Glob
    TameFiles a String [String] | -- type srcRoot wildcard-expanded pathnames
    Comp String                 | -- component nesting
    Gen       a
    deriving (Eq, Show)

ctxValXInputsP :: CtxVal a -> Bool
ctxValXInputsP (XFlags _ _)   = False
ctxValXInputsP (XInputs _ )   = True
ctxValXFlagsP  :: CtxVal a -> Bool
ctxValXFlagsP  (XFlags _ _)   = True
ctxValXFlagsP  (XInputs _)    = False

-- XXX: unsafe!
expand_wildfiles :: Type a => Files a -> [String]
expand_wildfiles (Files ty base patterns) = concat $ map expand_pattern patterns
    where expand_pattern p = unsafePerformIO $ glob $ base </> p ++ type_extension ty

expandWilds :: Type a => Files a -> Files a
expandWilds wfs@(Files typ base _) =
    TameFiles typ base $ expand_wildfiles wfs

eval_CtxExp :: ToolKind b => a -> Plat c d e -> b -> Maybe String -> CtxExp a b c d e -> Bool
eval_CtxExp kind plat@(Plat arch os@(OS ostype _)) ckind mFilename expr =
    case expr of
      AsPartOf     expKind      -> expKind   == kind
      ForArch      expArch      -> expArch   == arch
      ForOSType    expOSType    -> expOSType == ostype
      ForOS        expOS        -> expOS     == os
      ForPlat      expPlat      -> expPlat   == plat
      WithTool     expTool      -> expTool   == ckind

      ForInput     expString | Nothing    <- mFilename -> False
                             | Just fname <- mFilename -> fname == expString

      ShellTest    shCmd expect      -> (unsafeShell shCmd) == expect
      ExecTest     argv0 argv expect -> (unsafeExec argv0 argv) == expect

      Not sub   -> not $ eval_CtxExp kind plat ckind mFilename sub
      And subs  -> all  (eval_CtxExp kind plat ckind mFilename) subs
      Or  subs  -> any  (eval_CtxExp kind plat ckind mFilename) subs
      Always    -> True


-- BuildVar:  parametrization atom
--
data BuildVar =
    Var String      |
    List [String]   |
    Flag Bool
    deriving (Eq, Show)

var_stringify :: String -> BuildVar -> String
var_stringify name (Var s)   = printf "--%s '%s'" name s
var_stringify name (List ss) = printf "--%s '%s'" name $ intercalate " " ss
var_stringify name (Flag f)  = if f then "--" ++ name else ""

newtype VarMap  = VarMap (HashMap String BuildVar)
newtype VarEnv = VarEnv (String -> BuildVar, String -> String, String -> [String], String -> Bool)
newtype VarSpec = VarSpec (VarEnv -> VarMap)

make_varenv :: (String -> Maybe BuildVar) -> VarEnv
make_varenv raw_lookup = VarEnv (var, varS, varL, varB)
    where var k  = case raw_lookup k of
                     Nothing       -> error $ printf "Undefined build variable: '%s'" k
                     Just bv       -> bv
          varS k = case raw_lookup k of
                     Nothing       -> error $ printf "Undefined build variable: '%s'" k
                     Just (Var s)  -> s
                     _             -> error $ printf "Build variable '%s' is not of type 'string'"
          varL k = case raw_lookup k of
                     Nothing       -> error $ printf "Undefined build variable: '%s'" k
                     Just (List l) -> l
                     _             -> error $ printf "Build variable '%s' is not of type 'list-of-strings'"
          varB k = case raw_lookup k of
                     Nothing       -> error $ printf "Undefined build variable: '%s'" k
                     Just (Flag b) -> b
                     _             -> error $ printf "Build variable '%s' is not of type 'bool'"

varspec_add :: (VarEnv -> VarMap) -> VarMap -> VarEnv
varspec_add varspec (VarMap params) = ret
    where ret = make_varenv (lookup_chain params $ let (VarMap vars) = varspec ret in vars)

type OptSpec = [(String, BuildVar, String, String)]

derive_merged_optmap :: OptSpec -> VarMap -> VarMap
derive_merged_optmap optspecs (VarMap args) =
    VarMap $ fromList [ (name, fromMaybe bv (H.lookup name args))
                      | (name, bv, _, _) <- optspecs ]

derive_optdescrs :: OptSpec -> [OptDescr (Either String (String, BuildVar))]
derive_optdescrs optspecs =
    [ Option "" [name] (case bvar of
                          List _ -> (ReqArg (\str -> Right $ (name, List [str])) meta)
                          Var  _ -> (ReqArg (\str -> Right $ (name, Var str)) meta)
                          Flag _ -> (NoArg $ Right $ (name, Flag True)))
             desc
    | (name, bvar, meta, desc ) <- optspecs ]


-- A build environment conditionalisation DAG internal representation node
--   represents the sum of contexts of all parents, plus the context for the first
--   matching case branch

data Ctx tag tkind ty arch osty osv =
    Ctx
    [Either String (Ctx tag tkind ty arch osty osv)] -- parent names
    [(CtxExp tag tkind arch osty osv, [CtxVal ty])]  -- cases
    deriving (Show)

newtype CtxMap tag tkind ty arch osty osv = CtxMap (HashMap String (Ctx tag tkind ty arch osty osv))

evalCtx :: (Tag tag, ToolKind tkind, Type ty) => CtxMap tag tkind ty arch osty osv -> tag -> Plat arch osty osv -> tkind -> Maybe String -> Ctx tag tkind ty arch osty osv -> [CtxVal ty]
evalCtx ctxenv@(CtxMap ctxmap) kind plat ckind mFilename (Ctx parents cases) =
    concat $ (map evalParent parents) ++ (take 1 $ filter (not . null) $ map evalCase cases)
        where evalCase (caseCond, caseVals) =
                  if (eval_CtxExp kind plat ckind mFilename caseCond) then caseVals else []
              evalParent (Left pname) = evalCtx ctxenv kind plat ckind mFilename $ case H.lookup pname ctxmap of
                                                                                     Nothing -> error $ printf "Unknown context parent node name: '%s'" pname
                                                                                     Just x  -> x
              evalParent (Right p)    = evalCtx ctxenv kind plat ckind mFilename p

-- syntactic sugar for pretty Ctx creation.  Might go unused at some point.
ctxR :: Tag a => [CtxVal c] -> Ctx a b c d e f
ctxR vals =
    Ctx [] [(Always, vals)]

ctxRWhen :: Tag a => CtxExp a b d e f -> [CtxVal c] -> Ctx a b c d e f
ctxRWhen expr vals =
    Ctx [] [(expr, vals)]

ctxRIf :: Tag a => CtxExp a b d e f -> [CtxVal c] -> [CtxVal c] -> Ctx a b c d e f
ctxRIf expr thens elses =
    Ctx [] $ [(expr, thens)] ++ [(Always, elses)]

ctxRCase :: Tag a => [(CtxExp a b d e f, [CtxVal c])] -> [CtxVal c] -> Ctx a b c d e f
ctxRCase cases elses =
    Ctx [] $ cases ++ [(Always, elses)]

ctxN :: Tag a => [String] -> [CtxVal c] -> Ctx a b c d e f
ctxN parents vals =
    Ctx (map Left parents) [(Always, vals)]

ctxNWhen :: Tag a => [String] -> CtxExp a b d e f -> [CtxVal c] -> Ctx a b c d e f
ctxNWhen parents expr vals =
    Ctx (map Left parents) [(expr, vals)]

ctxNIf :: Tag a => [String] -> CtxExp a b d e f -> [CtxVal c] -> [CtxVal c] -> Ctx a b c d e f
ctxNIf parents expr thens elses =
    Ctx (map Left parents) $ [(expr, thens)] ++ [(Always, elses)]

ctxNCase :: Tag a => [String] -> [(CtxExp a b d e f, [CtxVal c])] -> [CtxVal c] -> Ctx a b c d e f
ctxNCase parents cases elses =
    Ctx (map Left parents) $ cases ++ [(Always, elses)]


--   Component:  a well-known buildable:
--    - name
--    - chain of the build
--    - required kinds
--    - context
--    - invariant content
--    - context-dependent content
--
-- NOTE: we don't need to specify the Chain name here, as it seems derivable
--
data Component a b c d e f g =
    Component {
      cName            :: String,
      cChainName       :: b,
      cKinds           :: [a],
      cCtxNames        :: [String],
      cAlways          :: [CtxVal d],
      cCases           :: [(CtxExp a c e f g, [CtxVal d])]
    } deriving (Show)

newtype CompMap a b c d e f g = CompMap (HashMap String (Component a b c d e f g))


--   Buildable:  a physical build product :: [Slice] -> [Component] -> [Buildable]
--    - component
--    - kind            -- derived from component
--    - platform        -- derived from slice and kind
--    - output filename -- derived from slice and kind (implied as f(bbPath, cName . bbComponent))
--
--  ..hence a Buildable is a <Component * Kind * Slice> product.
--
data Buildable a b c d e f g where
    Buildable :: (Tag a, ChainName b, ToolKind c, Type d, Arch e, OSType f, OSVersion g) => {
        bName       :: String,
        bComponent  :: Component a b c d e f g,
        bCtx        :: Ctx a c d e f g,
        bKind       :: a,
        bPlat       :: Plat e f g,
        bPath       :: String,
        bInputs     :: c -> [CtxVal d],
        bFlagmap    :: c -> String -> [CtxVal d],
        bOutFiles   :: HashMap String ([String], d, String, d, c, ToolAction, Buildable a b c d e f g)
    } -> Buildable a b c d e f g

component_phystype :: (ChainName cname, Type typ) => HashMap cname (Chain typ tkind) -> Component a cname tkind typ e f g -> typ
component_phystype chmap (Component _ chname _ _ _ _) =
    ty where (Chain ty _ _) = chmap ! chname

files_phystype :: (ChainName cname, ToolKind tkind, Type typ) => CompMap a cname tkind typ e f g -> HashMap cname (Chain typ tkind) -> Files typ -> typ
files_phystype (CompMap comap) chmap (Comp compname)    = component_phystype chmap $ comap ! compname
files_phystype _               _     (Gen       ty)     = ty
files_phystype _               _     (Files     ty _ _) = ty
files_phystype _               _     (TameFiles ty _ _) = ty

component_buildable :: (Arch e, OSType f, OSVersion g) => [Buildable a b c d e f g] -> String -> Plat e f g -> Buildable a b c d e f g
component_buildable     buildables    compname  plat =
    case results of
      []    -> error $ printf "No buildable for:  comp=%s  plat=%s." compname (pretty plat)
      x : _ -> x
    where results = [ b
                      | b@(Buildable _ (Component bcompname _ _ _ _ _) _ _ sliceplat _ _ _ _) <- buildables
                      , compname == bcompname && plat == sliceplat ]

buildable_output :: Buildable a b c d e f g -> String
buildable_output (Buildable name _ _ _ _ path _ _ _) =
    path </> name <.> exe

compute_files :: (Type d, Arch e, OSType f, OSVersion g) => [Buildable a b c d e f g] -> Plat e f g -> Files d -> [String]
compute_files buildables to_plat (Comp comp_name)    = [buildable_output $ component_buildable buildables comp_name to_plat]
compute_files _          _       files@(Files _ _ _) = expand_wildfiles files
compute_files _          _       (Gen _)             = undefined

do_forge_chainlinks :: (Tag a, ChainName b, ToolKind tkind, Type typ, Arch e, OSType f, OSVersion g) => CompMap a b tkind typ e f g -> [Buildable a b tkind typ e f g] -> HashMap b (Chain typ tkind) -> [Tool tkind typ e f g] -> (String, Int, Int) -> (Chain typ tkind) -> (Plat e f g, Plat e f g) -> (tkind -> [CtxVal typ]) -> typ -> String -> ([ChainLink typ tkind], [ChainLink typ tkind])
do_forge_chainlinks compenv buildables chains tools (name, depth, idx) (Chain thisty toolkind input_chain) (on_plat, to_plat) xinputs upwardty outdir =
    let id_step chidx   = (printf "%s.%d[%d]" name depth idx, depth + 1, chidx)
        leafp           = null input_chain
        (upward_acc,
         upward_result) = if leafp
                          then ([],
                                [ ChainLink undefined undefined f undefined undefined undefined -- leaf node, source
                                | XInputs files                           <- xinputs toolkind,
                                  files_phystype compenv chains files == thisty,
                                  f                                       <- compute_files buildables to_plat files ])
                          else foldl (\ (accacc, accres) (acc, res) -> (accacc ++ acc, accres ++ res))
                                     ([], [])
                                     [ do_forge_chainlinks compenv buildables chains tools (id_step i) child (on_plat, to_plat) xinputs thisty outdir
                                     | (i, child) <- zip [1..] input_chain ] in -- internal node, intermediate files
    (upward_acc ++ (if leafp || depth == 0
                    then []
                    else upward_result),
     if
     | depth == 0             -> let  in
                                 [ ChainLink  infiles inty   outfile upwardty tkind    xform
                                 | let ChainLink infiles inty _ _ tkind xform:_ = upward_result
                                       outfile = outdir </> name <.> (type_extension upwardty) ]
     | type_fusing_p upwardty -> [ ChainLink (foldl (\inacc (ChainLink _ _ inarg _ _ _) -> inacc ++ [inarg])
                                                    [] upward_result)
                                                     thisty outfile upwardty toolkind chainlink_xform
                                 | let outfile = outdir </> name <.> (type_extension upwardty) ]

     | True                   -> [ ChainLink [infile] thisty outfile upwardty toolkind chainlink_xform
                                 | ChainLink _        _      infile  _        _        _               <- upward_result,
                                   let outfile = (if leafp then outdir else "") </> retype_file upwardty infile ])
     where
       -- chainlink_xform: how to translate this link's files to the parent's
       chainlink_xform  :: String -> (Either String [String]) -> [String] -> Action ()
       chainlink_xform = exec_tool tools toolkind (thisty, upwardty) (on_plat, to_plat)

forge_chainlinks :: (Tag a, ChainName b, ToolKind c, Type d, Arch e, OSType f, OSVersion g) => CompMap a b c d e f g -> [Buildable a b c d e f g] -> HashMap b (Chain d c) -> [Tool c d e f g] -> String -> (Chain d c) -> (Plat e f g, Plat e f g) -> (c -> [CtxVal d]) -> d -> String -> [ChainLink d c]
forge_chainlinks compenv buildables chains tools name chain platxform inputs outtype outdir =
    let (acc, ret) = do_forge_chainlinks compenv buildables chains tools (name, 0, 0) chain platxform inputs outtype outdir in
    acc ++ ret

--   Slice::
--    - component set expansion = [Component] * [component_kinds Component] * [component_plats Component (component_kinds Component)]
--    - required target platforms
--    - output path
--
data Slice a b c d =
    Slice {
      sliceKind  :: a,
      slicePlats :: [(Plat b c d, String)]
    } deriving (Eq, Show, Generic)
instance (Tag a, Arch b, OSType c, OSVersion d) => Out (Slice a b c d)

newtype Schema a b c d = Schema (HashMap a (Slice a b c d))

component_ctx :: (Tag a, ChainName b) => Component a b c d e f g -> Ctx a c d e f g
component_ctx (Component _ _ _ parentCtxs always conds) =
    condsCtx
    where alwaysCtx = ctxN parentCtxs always
          condsCtx  = Ctx [Right alwaysCtx] conds

compute_buildables :: (Tag a, ChainName b, ToolKind c, Type d, Arch e, OSType f, OSVersion g) => Plat e f g -> Schema a e f g -> CompMap a b c d e f g -> HashMap b (Chain d c) -> [Tool c d e f g] -> CtxMap a c d e f g -> [Buildable a b c d e f g]
compute_buildables this_plat (Schema schema) compmap@(CompMap comap) chains tools ctxmap =
    bbles -- this is becase forge_chainlinks needs this list to look up buildable final files
    where bbles = [ b
                  | comp@(Component cmName cmChainName cmTags _ _ _) <- elems comap
                  , tag                                              <- cmTags
                  , let Slice _ compKindOuts        = case H.lookup tag schema of
                                                        Just s  -> s
                                                        Nothing -> error $ printf "Build schema says nothing about kind %s.\nSchema: %s" (show tag) (pretty schema)
                        chain_top@(Chain topty _ _) = chains ! cmChainName
                        ctx                         = component_ctx comp
                  , (slicePlat@(Plat arch _), slicePath)              <- compKindOuts
                  , let name = if -- derive a unique name for the Buildable, according to the power of its Component's [Kind * Plat] product
                               | length cmTags == 1 && length compKindOuts == 1 -> cmName
                               | length cmTags == 1                             -> cmName ++ "-" ++ lcShow arch
                               |                       length compKindOuts == 1 -> cmName ++ "-" ++ lcShow tag
                               | True                                           -> cmName ++ "-" ++ lcShow tag  ++ "-" ++ lcShow arch
                        xinputs tkind      = filter ctxValXInputsP $ evalCtx ctxmap tag slicePlat tkind Nothing     ctx
                        xflags  tkind file = filter ctxValXFlagsP  $ evalCtx ctxmap tag slicePlat tkind (Just file) ctx
                        chainlinks   = forge_chainlinks compmap bbles chains tools name chain_top (this_plat, slicePlat) xinputs topty slicePath
                        out_filemap  = fromList [   (outf, (infs, inty, outf, outty, tkind, tool, b))
                                                | ChainLink infs inty outf outty tkind tool <- chainlinks ]
                        b            = Buildable name comp ctx tag slicePlat slicePath xinputs xflags out_filemap ]
