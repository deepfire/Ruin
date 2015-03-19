{-# LANGUAGE CPP, DataKinds, DeriveDataTypeable, DeriveGeneric, ExtendedDefaultRules, GADTs, KindSignatures, MultiWayIf, ScopedTypeVariables, StandaloneDeriving, TransformListComp, TupleSections, QuasiQuotes, UnicodeSyntax, ViewPatterns #-}

-- | A module for declarative definition of complex build systems.
-- Ruin tries to provide the build system engineer with a language closer to what
-- he himself might use in his head.

module Development.Ruin
    (
    -- * Target environment
      Arch, OS(..), OSType, OSVersion, Plat(..)
    , os_execsuffix

    -- * Source, intermediate & destination file types
    , Type, none, type_fusing_p, type_extension, retype_file
    , Inputs(..), Files(..)

    -- * Transformations
    , Tool(..), exec_tool, ToolKind, notool, ToolActionEither, ToolActionSingle, ToolActionMulti
    , Chain(..), ChainName, ChainMap(..), ChainLink(..)

    -- * Context
    , Ctx, CtxExp(..), CtxVal(..)
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
    , map_to_hashmap, invert_hashmap
    , lcstr, lcShow
    , (%>)
    ) where

import GHC.Generics (Generic)

import Control.Arrow ((***), (&&&))

import Data.Hashable
import Data.Char (toLower)
import Data.Either (lefts)
import Data.Either.Utils (fromRight)
import qualified Data.HashMap.Lazy as H
import Data.HashMap.Lazy (HashMap, elems, fromList, toList, (!))
import Data.List (intercalate)
import Data.Map.Lazy ()
import Data.Monoid (mconcat)
import qualified Data.HashSet as HS
import Data.String.Utils (startswith)
import Data.Tuple (swap)
import Data.Typeable

import Debug.Trace (trace)

import Development.Shake
import Development.Shake.Command()
import Development.Shake.FilePath

import Prelude.Unicode

import System.Process (rawSystem)
import System.Console.GetOpt (OptDescr(..), ArgDescr(..))
import System.Path.Glob (glob)
import System.Path.NameManip (dir_part, filename_part)

import System.Process (readProcess, readProcessWithExitCode)

import System.IO.Unsafe (unsafePerformIO)

import Text.PrettyPrint.GenericPretty (pretty, Out(..), doc)
import Text.PrettyPrint (parens, (<+>), ($$), (<>), text)
import Text.Printf (printf)
import Data.Maybe (fromMaybe)

---
--- Dangerous silliness
---
deriving instance Typeable (Action)

instance (Typeable a, Typeable b) ⇒ Show (a → b) where
    show f = printf "#<fn %s>" (show $ typeOf f)

---
--- Silliness
---
ignt ∷ a → String → c → c
ignt _ _ x = x

eitherConst ∷ a → a → Either b c → a
eitherConst l r e = either (const l) (const r) e

lookup_chain ∷ (Hashable a, Eq a) ⇒ HashMap a b → HashMap a b → a → Maybe b
lookup_chain primary secondary k = case H.lookup k primary of
                                     Nothing → H.lookup k secondary
                                     Just v  → Just v

invert_hashmap ∷ (Hashable a, Hashable b, Eq b) ⇒ HashMap a b → HashMap b a
invert_hashmap = fromList ∘ (map swap) ∘ toList

map_to_hashmap ∷ (Hashable b, Eq b) ⇒ (a → b) → [a] → HashMap b a
map_to_hashmap keyfn xs = fromList $ map (\ x → (keyfn x, x)) xs

lcstr ∷ String → String
lcstr cs = map toLower cs

lcShow ∷ Show x ⇒ x → String
lcShow x = lcstr $ show x

exec ∷ String → [String] → IO String
exec executable args = readProcess executable args []

shell ∷ String → IO String
shell comm = do
  (_, out, err) ← readProcessWithExitCode "sh" ["-c", comm] []
  return $ out ++ err

cmd2 ∷ String → [String] → Action ()
cmd2 comm args = do
  putLoud $ printf "cmd> %s %s" comm $ intercalate " " args
  _ ← liftIO $ rawSystem comm args
  return ()

unsafeExec ∷ String → [String] → String
unsafeExec executable args = unsafePerformIO (exec executable args)

unsafeShell ∷ String → String
unsafeShell comm = unsafePerformIO (shell comm)

instance (Out a, Out b) ⇒ Out (HashMap a b) where
  doc hm = parens $ foldl (\acc (k, v) → acc $$ doc k <+> text ":" <+> doc v) (text "") $ toList hm
  docPrec _ = doc


--- Source & result specification
class (Eq a, Hashable a, Out a, Show a, Typeable a) ⇒ Type a where
    none           ∷ a
    type_fusing_p  ∷ a → Bool
    type_extension ∷ a → String

retype_file ∷ Type a ⇒ a → String → String
retype_file newty f = f -<.> type_extension newty


--- Target specification
class (Eq a, Show a, Generic a, Out a, Hashable a) ⇒ Arch a
class (Eq a, Show a, Generic a, Out a, Hashable a) ⇒ OSType a where
    os_execsuffix ∷ a → String
class (Eq a, Show a, Generic a, Out a, Hashable a) ⇒ OSVersion a

data OS a b where
    OS ∷ (OSType a, OSVersion b) ⇒ a → (Maybe b) → OS a b

deriving instance Show (OS a b)
instance Hashable (OS a b) where
     hashWithSalt s (OS x y)  = s `hashWithSalt` (hash x) `hashWithSalt` (hash y)
instance (Out a, Out b) ⇒ Out (OS a b) where
  doc (OS ostype osver) = doc ostype <> text "-" <> case osver of
                                                      Nothing  → text "*-*"
                                                      Just ver → doc ver
  docPrec _ = doc
instance Eq  (OS a b) where -- treat Nothing as a wildcard during comparison:
    (OS t1 Nothing) == (OS t2 _)       = t1 ≡ t2
    (OS t1 _)       == (OS t2 Nothing) = t1 ≡ t2
    (OS t1 v1)      == (OS t2 v2)      = t1 ≡ t2 && v1 ≡ v2

data Plat a b c where
    Plat ∷ (Arch a, OSType b, OSVersion c) ⇒ a → OS b c → Plat a b c
deriving instance Show (Plat a b c)
instance Hashable (Plat a b c) where
     hashWithSalt s (Plat x y)  = s `hashWithSalt` (hash x) `hashWithSalt` (hash y)
instance (Out a, Out b, Out c) ⇒ Out (Plat a b c) where
  doc (Plat arch os) = doc arch <> text "-" <> doc os
  docPrec _ = doc
instance Eq  (Plat a b c) where
    (Plat a1 os1)   == (Plat a2 os2)   = a1 ≡ a2 && os1 ≡ os2


--- Source → target transformation
class (Eq a, Generic a, Hashable a, Ord a, Out a, Show a, Typeable a) ⇒ ToolKind a where
    notool ∷ ToolKind a ⇒ a

type ToolActionEither  = String → (Either String [String]) → [String] → Action ()
type ToolActionMulti   = String → [String] →                 [String] → Action ()
type ToolActionSingle  = String →  String  →                 [String] → Action ()
type ToolActionSimple  = String → [String] →                            Action ()

data Tool a b c d e =
    Tool a b b [(Plat c d e, Plat c d e, String)] (Either (String → ToolActionSingle) (String → ToolActionMulti))
    deriving (Generic)

instance (Out a, Out b) ⇒ Out (Tool a b c d e) where
    doc (Tool kind fty toty _ _) = text "Tool" <+> parens (text "xform:" <+> doc fty <> text "→" <> doc toty <+> doc kind)
    docPrec _ = doc

exec_tool ∷ (ToolKind a, Type b) ⇒ [Tool a b c d e] → a → (b, b) → (Plat c d e, Plat c d e) → ToolActionEither
exec_tool available_tools want_toolkind (want_tyfrom, want_tyto) (this_plat, for_plat) out ins flags =
    case [ ignt trace (printf "found %s %s→%s for %s" (show toolkind) (show tyfrom) (show tyto) out)
           (c, tool_exec)
         | c@(Tool toolkind tyfrom tyto tool_variants _) ← available_tools
         , (on, for, tool_exec)                          ← tool_variants
         , -- trace (printf "try %s<>%s %s<>%s %s<>%s %s<>%s %s<>%s"
           --               (show toolkind) (show want_toolkind) (show tyfrom) (show want_tyfrom) (show tyto) (show want_tyto) (show on) (show this_plat) (show for) (show for_plat)) $
           toolkind ≡ want_toolkind && tyfrom ≡ want_tyfrom && tyto ≡ want_tyto && on ≡ this_plat && for ≡ for_plat ] of
      []  → error (printf "Failed to find a suitable tool: (%s←%s) on-plat=%s to-plat=%s" (show want_tyto) (show want_tyfrom) (show this_plat) (show for_plat))
      (Tool toolkind _ _ _ fnV, tool_exec):_ →
          case (fnV, ins) of
            (Left  fn, Left  arg)  → fn tool_exec out arg flags
            (Right fn, Right args) → fn tool_exec out args flags
            _                      → error "Tool/inputs arity mismatch: used %s on an %sary input" (show toolkind) $ eitherConst "un" "multi" ins

-- Chains
-- Because of ambiguities of composition (f.e. both GCCLD and LD can perform CObj → Executable transforms)
-- we need hints.
-- A Chain is what provides these hints.

class (Eq a, Hashable a, Out a, Show a) ⇒ ChainName a

data Chain typ tkind where
    Chain ∷ (Type typ, ToolKind tkind) ⇒ typ → tkind → [Chain typ tkind] → Chain typ tkind -- ProductType → ApplicableTool → Children
deriving instance Eq (Chain typ tkind)
deriving instance Show (Chain typ tkind)
newtype ChainMap cn ty tk = ChainMap (HashMap cn (Chain ty tk))

data ChainLink ty tkind =
    ChainLink [String] ty String ty tkind (XQuery ty) ToolActionEither

instance (Show a, Show b) ⇒ Show (ChainLink a b) where
    show (ChainLink infs inty outf outty tkin _ _) = printf "#<LINK (%s<-%s) %s: %s ← %s" (show outty) (show inty) (show tkin) (show outf) (show infs)


-- A condition for extending the build environment
class (Eq a, Hashable a, Out a, Show a, Generic a) ⇒ Tag a

data CtxExp a b c d e where -- tag tkind arch osty osv
    AsPartOf  ∷ (Tag a, Generic a) ⇒ a → CtxExp a b c d e
    ForArch   ∷ (Tag a, Arch c) ⇒ c → CtxExp a b c d e
    ForOSType ∷ (Tag a, OSType d) ⇒ d → CtxExp a b c d e
    ForOS     ∷ (Tag a, OSType d) ⇒ OS d e → CtxExp a b c d e
    ForPlat   ∷ (Tag a) ⇒ Plat c d e → CtxExp a b c d e
    ForInput  ∷ (Tag a, Generic a) ⇒ String → CtxExp a b c d e -- Filename wildcard
    WithTool  ∷ (Tag a, ToolKind b) ⇒ b → CtxExp a b c d e
    ShellTest ∷ (Tag a, Generic a) ⇒ String → String → CtxExp a b c d e -- WARNING: the command is expected to be pure!
    ExecTest  ∷ (Tag a, Generic a) ⇒ String → [String] → String → CtxExp a b c d e -- WARNING: ^^^
    Not       ∷ (Tag a, Generic a) ⇒ (CtxExp a b c d e) → CtxExp a b c d e
    And       ∷ (Tag a, Generic a) ⇒ [CtxExp a b c d e] → CtxExp a b c d e
    Or        ∷ (Tag a, Generic a) ⇒ [CtxExp a b c d e] → CtxExp a b c d e
    Always    ∷ (Tag a, Generic a) ⇒ CtxExp a b c d e

deriving instance (Tag a, ToolKind b, Arch c, OSType d, OSVersion e) ⇒ Eq (CtxExp a b c d e)
deriving instance (Tag a, ToolKind b, Arch c, OSType d, OSVersion e) ⇒ Show (CtxExp a b c d e)
instance Hashable (CtxExp a b c d e) where
     hashWithSalt s (AsPartOf x)     = s `hashWithSalt` (hash x)
     hashWithSalt s (ForArch x)      = s `hashWithSalt` (hash x)
     hashWithSalt s (ForOSType x)    = s `hashWithSalt` (hash x)
     hashWithSalt s (ForOS x)        = s `hashWithSalt` (hash x)
     hashWithSalt s (ForPlat x)      = s `hashWithSalt` (hash x)
     hashWithSalt s (ForInput x)     = s `hashWithSalt` (hash x)
     hashWithSalt s (WithTool x)     = s `hashWithSalt` (hash x)
     hashWithSalt s (ShellTest x y)  = s `hashWithSalt` (hash x) `hashWithSalt` (hash y)
     hashWithSalt s (ExecTest x y z) = s `hashWithSalt` (hash x) `hashWithSalt` (hash y) `hashWithSalt` (hash z)
     hashWithSalt s (Not x)          = s `hashWithSalt` (hash x)
     hashWithSalt s (And x)          = s `hashWithSalt` (hash x)
     hashWithSalt s (Or x)           = s `hashWithSalt` (hash x)
     hashWithSalt s Always           = s

-- An atom of build environment
data CtxVal ty =
    XFlags  ty [String] |          -- type flags
    XInputs (Inputs ty)
    deriving (Eq, Show, Generic, Typeable)
instance (Hashable ty) ⇒ Hashable (CtxVal ty)

xflags_type ∷ CtxVal ty → ty
xflags_type (XFlags ty _) = ty

type XQuery ty = (ty, String) → Bool → [CtxVal ty]

data XIR tk ty where
    XIR ∷ (Typeable tk, Generic tk, ToolKind tk, Type ty) ⇒ XQuery ty → CtxVal ty → XIR tk ty
deriving instance (Typeable tk, Generic tk, ToolKind tk, Type ty) ⇒ Show (XIR tk ty)

ctxval_ξp    ∷ CtxVal d → Bool
ctxval_ξp   (XInputs _ )  = True
ctxval_ξp    _            = False
ctxval_xfp   ∷ CtxVal d → Bool
ctxval_xfp  (XFlags _ _)  = True
ctxval_xfp   _            = False

-- newtype ΞMap tk ty = ΞMap (HashMap (CtxVal ty) (Chain tk ty))

data Inputs ty =
    Srcs ty String [String]              | -- type srcRoot wildcards-as-per-System.Path.Glob
    Comp String                          | -- component nesting
    Gen  ty String (String → Action ())
    deriving (Show, Generic)
instance Eq ty ⇒ Eq (Inputs ty) where
    (Srcs ty1 d1 fs1) == (Srcs ty2 d2 fs2) = ty1 ≡ ty2 && d1 ≡ d2 && fs1 ≡ fs2
    (Comp cn1)        == (Comp cn2)        = cn1 ≡ cn2
    (Gen  ty1 f1 _)   == (Gen  ty2 f2 _)   = ty1 ≡ ty2 && f1 ≡ f2 -- XXX: is this a bug source?
    _                 == _                 = False
instance Hashable ty ⇒ Hashable (Inputs ty) where
    hashWithSalt s (Srcs a b c)  = s `hashWithSalt` (hash a) `hashWithSalt` (hash b) `hashWithSalt` (hash c)
    hashWithSalt s (Comp a)      = s `hashWithSalt` (hash a)
    hashWithSalt s (Gen  a b _)  = s `hashWithSalt` (hash a) `hashWithSalt` (hash b)


data Files a =
    Files     a String [String]   -- type srcRoot wildcards-as-per-System.Path.Glob
    deriving (Eq, Show, Generic)
instance Hashable a ⇒ Hashable (Files a)

eval_CtxExp ∷ (ToolKind tkind, Type ty) ⇒ tag → Plat arch osty osv → tkind → Maybe (ty, String) → CtxExp tag tkind arch osty osv → Bool
eval_CtxExp tag plat@(Plat arch os@(OS ostype _)) tkind mFilename expr =
    case expr of
      AsPartOf     expTag       → expTag    ≡ tag
      ForArch      expArch      → expArch   ≡ arch
      ForOSType    expOSType    → expOSType ≡ ostype
      ForOS        expOS        → expOS     ≡ os
      ForPlat      expPlat      → expPlat   ≡ plat
      WithTool     expTkind     → expTkind  ≡ tkind

      ForInput     expString | Nothing        ← mFilename → False
                             | Just (_, file) ← mFilename → startswith expString file

      ShellTest    shCmd expect      → (unsafeShell shCmd) ≡ expect
      ExecTest     argv0 argv expect → (unsafeExec argv0 argv) ≡ expect

      Not sub   → not $ eval_CtxExp tag plat tkind mFilename  sub
      And subs  → all  (eval_CtxExp tag plat tkind mFilename) subs
      Or  subs  → any  (eval_CtxExp tag plat tkind mFilename) subs
      Always    → True


-- BuildVar:  parametrization atom
--
data BuildVar =
    Var String      |
    List [String]   |
    Flag Bool
    deriving (Eq, Show)

var_stringify ∷ String → BuildVar → String
var_stringify name (Var s)   = printf "--%s '%s'" name s
var_stringify name (List ss) = printf "--%s '%s'" name $ intercalate " " ss
var_stringify name (Flag f)  = if f then "--" ++ name else ""

newtype VarMap  = VarMap (HashMap String BuildVar)
newtype VarEnv = VarEnv (String → BuildVar, String → String, String → [String], String → Bool)
newtype VarSpec = VarSpec (VarEnv → VarMap)

make_varenv ∷ (String → Maybe BuildVar) → VarEnv
make_varenv raw_lookup = VarEnv (var, varS, varL, varB)
    where var k  = case raw_lookup k of
                     Nothing       → error $ printf "Undefined build variable: '%s'" k
                     Just bv       → bv
          varS k = case raw_lookup k of
                     Nothing       → error $ printf "Undefined build variable: '%s'" k
                     Just (Var s)  → s
                     _             → error $ printf "Build variable '%s' is not of type 'string'"
          varL k = case raw_lookup k of
                     Nothing       → error $ printf "Undefined build variable: '%s'" k
                     Just (List l) → l
                     _             → error $ printf "Build variable '%s' is not of type 'list-of-strings'"
          varB k = case raw_lookup k of
                     Nothing       → error $ printf "Undefined build variable: '%s'" k
                     Just (Flag b) → b
                     _             → error $ printf "Build variable '%s' is not of type 'bool'"

varspec_add ∷ (VarEnv → VarMap) → VarMap → VarEnv
varspec_add varspec (VarMap params) = ret
    where ret = make_varenv (lookup_chain params $ let (VarMap vars) = varspec ret in vars)

type OptSpec = [(String, BuildVar, String, String)]

derive_merged_optmap ∷ OptSpec → VarMap → VarMap
derive_merged_optmap optspecs (VarMap args) =
    VarMap $ fromList [ (name, fromMaybe bv (H.lookup name args))
                      | (name, bv, _, _) ← optspecs ]

derive_optdescrs ∷ OptSpec → [OptDescr (Either String (String, BuildVar))]
derive_optdescrs optspecs =
    [ Option "" [name] (case bvar of
                          List _ → (ReqArg (\str → Right $ (name, List [str])) meta)
                          Var  _ → (ReqArg (\str → Right $ (name, Var str)) meta)
                          Flag _ → (NoArg $ Right $ (name, Flag True)))
             desc
    | (name, bvar, meta, desc ) ← optspecs ]


-- A build environment conditionalisation DAG internal representation node
--   represents the sum of contexts of all parents, plus the context for the first
--   matching case branch

data Ctx tag tkind ty arch osty osv =
    Ctx
    [Either String (Ctx tag tkind ty arch osty osv)] -- parent names
    [(CtxExp tag tkind arch osty osv, [CtxVal ty])]  -- cases
    deriving (Eq, Show, Generic)
instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f) ⇒ Hashable (Ctx a b c d e f)
newtype CtxMap tag tk ty arch osty osv = CtxMap (HashMap String (Ctx tag tk ty arch osty osv))

make_xquery ∷ (Tag tag, ToolKind tkind, Type ty, Arch arch, OSType osty, OSVersion osv) ⇒
               CtxMap tag tkind ty arch osty osv → tag → Plat arch osty osv → tkind → Ctx tag tkind ty arch osty osv → XQuery ty
make_xquery ctxmap tag plat tkind ctx =
    xquery
    where xquery ty_f explainp = map fromRight $ eval_Ctx ctxmap tag plat tkind (Just ty_f) explainp ctx

eval_Ctx ∷ (Arch arch, OSType osty, OSVersion osv, Tag tag, ToolKind tkind, Type ty) ⇒
            CtxMap tag tkind ty arch osty osv → tag → Plat arch osty osv → tkind →     -- context (general, not Ctx sense)
            Maybe (ty, String) → Bool →                                                -- record context, turn on explanation mode
            Ctx tag tkind ty arch osty osv → [Either (XIR tkind ty) (CtxVal ty)]       -- argument → return value
eval_Ctx cxm@(CtxMap ctxmap) tag plat tool stage explain_mode ctx_top =
    ret
    where (_, ret)  = eval (HS.empty, []) (Right ctx_top)
          namemap   = invert_hashmap ctxmap
          ctxdesc x = fromMaybe (printf "<unnamed ctx node %s>" (show x)) (H.lookup x namemap)
          find_parent (Right p) = p
          find_parent (Left n)  = case H.lookup n ctxmap of
                                    Nothing → error $ printf "Unknown context parent node name: '%s'" n
                                    Just x  → x
          eval_case ctxval_res val_filter_fn (caseCond, caseVals) =
              [ ctxval_res val | eval_CtxExp tag plat tool stage caseCond,
                                 val           ← caseVals,
                                 val_filter_fn val ]
          eval (seen, acc) (find_parent → this@(Ctx parents cases)) =
              if HS.member this seen then (if explain_mode then trace (printf "skip> %s" $ ctxdesc this) else id)
                                          (seen, acc)
              else (if explain_mode then trace (printf "visit> %s → %s" (ctxdesc this) (show added)) else id) res
                   where (ctxval_res, val_filter_fn)
                               = case stage of
                                   Nothing       → (\val → Left $ XIR xquery val,         ctxval_ξp)
                                        where xquery = make_xquery cxm tag plat tool this
                                   Just (ty, _)  → (\val → Right $ val,                   \x → ctxval_xfp x ∧ (xflags_type x ≡ ty))
                         added = concat (map (eval_case ctxval_res val_filter_fn) cases)
                         res   = foldl eval (HS.insert this seen, added ++ acc) parents

-- syntactic sugar for pretty Ctx creation.  Might go unused at some point.
ctxR ∷ Tag a ⇒ [CtxVal d] → Ctx a c d e f g
ctxR vals =
    Ctx [] [(Always, vals)]

ctxRWhen ∷ Tag a ⇒ CtxExp a c e f g → [CtxVal d] → Ctx a c d e f g
ctxRWhen expr vals =
    Ctx [] [(expr, vals)]

ctxRIf ∷ Tag a ⇒ CtxExp a c e f g → [CtxVal d] → [CtxVal d] → Ctx a c d e f g
ctxRIf expr thens elses =
    Ctx [] $ [(expr, thens)] ++ [(Always, elses)]

ctxRCase ∷ Tag a ⇒ [(CtxExp a c e f g, [CtxVal d])] → [CtxVal d] → Ctx a c d e f g
ctxRCase cases elses =
    Ctx [] $ cases ++ [(Always, elses)]

ctxN ∷ Tag a ⇒ [String] → [CtxVal d] → Ctx a c d e f g
ctxN parents vals =
    Ctx (map Left parents) [(Always, vals)]

ctxNWhen ∷ Tag a ⇒ [String] → CtxExp a c e f g → [CtxVal d] → Ctx a c d e f g
ctxNWhen parents expr vals =
    Ctx (map Left parents) [(expr, vals)]

ctxNIf ∷ Tag a ⇒ [String] → CtxExp a c e f g → [CtxVal d] → [CtxVal d] → Ctx a c d e f g
ctxNIf parents expr thens elses =
    Ctx (map Left parents) $ [(expr, thens)] ++ [(Always, elses)]

ctxNCase ∷ Tag a ⇒ [String] → [(CtxExp a c e f g, [CtxVal d])] → [CtxVal d] → Ctx a c d e f g
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
      cName            ∷ String,
      cChainName       ∷ b,
      cTags            ∷ [a],
      cCtxNames        ∷ [String],
      cAlways          ∷ [CtxVal d],
      cCases           ∷ [(CtxExp a c e f g, [CtxVal d])]
    } |
    Target {
      cName            ∷ String,
      cTags            ∷ [a],
      cType            ∷ d,
      cFile            ∷ String, -- Source
      cDeps            ∷ [String],
      cAction          ∷ ToolActionSimple
    }
    deriving (Show)

newtype CompMap a b c d e f g = CompMap (HashMap String (Component a b c d e f g))

input_type (CompMap comap) chains inp =
    case inp of
      Comp cname       → component_type chains $ comap ! cname
      Gen  ty _ _      → ty
      Srcs ty _ _      → ty


--   Buildable:  a physical build product ∷ [Slice] → [Component] → [Buildable]
--    - component
--    - kind            -- derived from component
--    - platform        -- derived from slice and kind
--    - output filename -- derived from slice and kind (implied as f(bbPath, cName ∘ bbComponent))
--
--  ..hence a Buildable is a <Component * Kind * Slice> product.
--
data Buildable a b c d e f g where
    Buildable ∷ (Tag a, ChainName b, ToolKind c, Type d, Arch e, OSType f, OSVersion g) ⇒ {
        bName       ∷ String,
        bComponent  ∷ Component a b c d e f g,
        bCtx        ∷ Ctx a c d e f g,
        bTag        ∷ a,
        bPlat       ∷ Plat e f g,
        bPath       ∷ String,
        bOutFiles   ∷ HashMap String (ChainLink d c, Buildable a b c d e f g)
    } → Buildable a b c d e f g

match_buildable ∷ (Tag a, Arch e, OSType f, OSVersion g) ⇒ [Buildable a b c d e f g] → String → (Maybe a) → Plat e f g → Buildable a b c d e f g
match_buildable buildables compname tag plat =
    case results of
      []    → error $ printf "No buildable for:  comp=%s  plat=%s." compname (pretty plat)
      x : _ → x
    where results = [ b
                      | b@(Buildable _ comp _ btag sliceplat _ _) ← buildables
                      , (cName comp) ≡ compname && sliceplat ≡ plat && btag ≡ (fromMaybe btag tag) ]

component_type :: (ChainName cn, Type ty) => ChainMap cn ty tk -> Component a cn tk ty e f g -> ty
component_type (ChainMap chmap) (Component _ chname _ _ _ _) = ty where (Chain ty _ _) = chmap ! chname
component_type _                (Target _ _ ty _ _ _)        = ty

buildable_output ∷ Buildable a b c d e f g → String
buildable_output (Buildable name (Component _ _ _ _ _ _) _ _ (Plat _ (OS osty _)) path _) = path </> name <.> os_execsuffix osty
buildable_output (Buildable _    (Target _ _ _ file _ _) _ _ _ _ _)                       = file

type ChainLinkConsCtx = (String, Int, Int)

ξs_chainlinks :: (Tag tag, ChainName cname, ToolKind tkind, Type ty, Arch arch, OSType osty, OSVersion osv) =>
                 CompMap tag cname tkind ty arch osty osv -> ChainMap cname ty tkind -> [Buildable tag cname toolkind ty arch osty osv] ->
                 Plat arch osty osv -> ty -> [XIR tkind ty] ->
                 [(Inputs ty, ChainLink ty tkind)]
-- ^ Compute chainlinks for a given set of XInputs (ξs)
ξs_chainlinks compmap chains buildables to_plat thisty xirs =
    cls
    where cls = [ (inp, ChainLink [] none f inp_ty notool xquery
                               $ case inp of
                                   Gen _ _ actn     → \out _ _ → actn out
                                   _                → \_ _ _   → return ())
                | XIR xquery (XInputs inp)  ← xirs,
                  let inp_ty   = input_type compmap chains inp,
                  inp_ty ≡ thisty,
                  f            ← case inp of
                                   Comp cname       → [buildable_output $ match_buildable buildables cname Nothing to_plat]
                                   Gen  _ outf _    → [outf]
                                   Srcs ty bas pats → concat $ map expand_pattern pats
                                       where expand_pattern p = unsafePerformIO $ glob $ bas </> p ++ type_extension ty ]

do_forge_chainlinks ∷ ∀ tag cname tkind ty arch osty osv . (Tag tag, ChainName cname, ToolKind tkind, Type ty, Arch arch, OSType osty, OSVersion osv) ⇒
                      CompMap tag cname tkind ty arch osty osv → CtxMap tag tkind ty arch osty osv → [Buildable tag cname tkind ty arch osty osv] → ChainMap cname ty tkind → [Tool tkind ty arch osty osv] →
                      tag → ChainLinkConsCtx → Ctx tag tkind ty arch osty osv → Chain ty tkind → (Plat arch osty osv, Plat arch osty osv) → H.HashMap tkind [XIR tkind ty] → ty → String →
                       ([(Inputs ty, ChainLink ty tkind)], [(Inputs ty, ChainLink ty tkind)])
do_forge_chainlinks compmap ctxmap buildables chainmap tools tag (clink_name, depth, idx) ctx_top (Chain thisty tool children_chains) (on_plat, to_plat) tool_XIRmap upwardty outdir =
    let id_step chidx    = (printf "%s.[%d]" clink_name idx, depth + 1, chidx)
        leafp            = null children_chains
        leaf_ins_ephemeral (Gen _ _ _) = False
        leaf_ins_ephemeral _           = True
        clink_xform      = exec_tool tools tool (thisty, upwardty) (on_plat, to_plat)
        (upward_acc,
         upward_result) = if leafp
                          then ([], ξs_chainlinks compmap chainmap buildables to_plat thisty (tool_XIRmap ! tool))
                          else let intrep = [ do_forge_chainlinks compmap ctxmap buildables chainmap tools tag (id_step i) ctx_top chain (on_plat, to_plat) tool_XIRmap thisty outdir
                                            | (i, chain) ← zip [1..] children_chains ]
                                   intrep        ∷ [([(Inputs ty, ChainLink ty tkind)], [(Inputs ty, ChainLink ty tkind)])]
                               in mconcat intrep ∷  ([(Inputs ty, ChainLink ty tkind)], [(Inputs ty, ChainLink ty tkind)])-- trace (if all )
    in
    (-- accumulate promotable results from leaf processing and subchains
     upward_acc ++ let res = filter (if | depth ≡ 0 → const False
                                        | leafp     → not ∘ leaf_ins_ephemeral ∘ fst
                                        | True      → const True)
                             upward_result
                       (inp, link) = unzip res in
                   ignt trace (printf "4   ξ → link @ %s:  %s → %s" (show $ id_step (0-1)) (show inp) (show link)) res,
     -- compute result
     if
     | depth ≡ 0              → [     (ins, ChainLink ifs ity outfile upwardty tk xq xf)
                                | let (ins, ChainLink ifs ity _       _        tk xq xf):_ = upward_result
                                      outfile = outdir </> clink_name <.> (type_extension upwardty) ]
     | type_fusing_p upwardty → [ (ins, ChainLink (foldl (\inacc (_, ChainLink _ _ inarg _ _ _ _) → inacc ++ [inarg])
                                                          [] upward_result)
                                                   thisty outfile upwardty tool xquery clink_xform)
                                | let outfile = outdir </> clink_name <.> (type_extension upwardty)
                                      ins     = Srcs upwardty "" []
                                      xquery  = make_xquery ctxmap tag to_plat tool ctx_top ]
     | True                   → [ (ins, ChainLink [infile] thisty outfile upwardty tool xquery clink_xform)
                                | (ins, ChainLink _        _      infile  _        _    xquery _) ← upward_result,
                                  let outfile = (if leafp then outdir else "") </> retype_file upwardty infile ])

forge_chainlinks ∷ (Tag a, ChainName b, ToolKind c, Type d, Arch e, OSType f, OSVersion g) ⇒ CompMap a b c d e f g → CtxMap a c d e f g → [Buildable a b c d e f g] → ChainMap b d c → [Tool c d e f g] → a → String → Ctx a c d e f g → Chain d c → (Plat e f g, Plat e f g) → H.HashMap c [XIR c d] → d → String → [ChainLink d c]
forge_chainlinks compmap ctxmap bbles chainmap tools tag name ctx_top chain platxform tkind_XIRmap outtype outdir =
    let (acc, ret) = do_forge_chainlinks compmap ctxmap bbles chainmap tools tag (name, 0, 0) ctx_top chain platxform tkind_XIRmap outtype outdir
        res        = acc ++ ret
        all_ins    = [ (x, []) | XIR _ (XInputs x) ← concat $ H.elems tkind_XIRmap]
        inp_links  = H.fromListWith (++) $ (all_ins ++ map (fst &&& (:[]) ∘ snd) res)
        orphan_ins = H.filter null inp_links
    in if not $ H.null orphan_ins
       then error (printf "Orphan inputs: %s" $ intercalate ", " $ map (\i → printf "%s :: %s" (show i) (show $ input_type compmap chainmap i) ∷ String) $ H.keys orphan_ins)
       else map snd res

--   Slice∷
--    - component set expansion = [Component] * [component_kinds Component] * [component_plats Component (component_kinds Component)]
--    - required target platforms
--    - output path
--
data Slice a b c d =
    Slice {
      sliceKind  ∷ a,
      slicePlats ∷ [(Plat b c d, String)]
    } deriving (Eq, Show, Generic)
instance (Tag a, Arch b, OSType c, OSVersion d) ⇒ Out (Slice a b c d)

newtype Schema a b c d = Schema (HashMap a (Slice a b c d))

-- | derive a unique name for the Buildable, according to the power of its Component's [Kind * Plat] product
compute_buildable_name ∷ (Tag a, Arch e) ⇒ Component a b c d e f g → e → a → Int → String
compute_buildable_name (Component cmName _ cmTags _ _ _) arch tag slice_width =
    if | length cmTags ≡ 1 && slice_width ≡ 1 → cmName
       | length cmTags ≡ 1                    → cmName ++ "-" ++ lcShow arch
       |                      slice_width ≡ 1 → cmName ++ "-" ++ lcShow tag
       | True                                 → cmName ++ "-" ++ lcShow tag  ++ "-" ++ lcShow arch
compute_buildable_name (Target cmName _ _ _ _ _) _ _ _ = cmName

component_ctx ∷ (Tag ta, ChainName cn, ToolKind tk, Type ty, Arch ar, OSType ot, OSVersion ov) ⇒ Component ta cn tk ty ar ot ov → (Ctx ta tk ty ar ot ov, ar → ta → Int → CtxMap ta tk ty ar ot ov)
component_ctx comp@(Component _ _ _ parentCtxs always conds) =
    (condsCtx, pre_ctxmap)
    where alwaysCtx = ctxN parentCtxs always
          condsCtx  = Ctx [Right alwaysCtx] conds
          pre_ctxmap arch tag slice_width = CtxMap $ fromList [(name, condsCtx), (name ++ "-common", alwaysCtx)]
              where name = compute_buildable_name comp arch tag slice_width
component_ctx (Target _ _ _ _ _ _) = (Ctx [] [], \_ _ _ → CtxMap H.empty)

component_buildable ∷ (Tag a, ChainName b, ToolKind c, Type d, Arch e, OSType f, OSVersion g) ⇒
                    Plat e f g → [Buildable a b c d e f g] → Component a b c d e f g →
                    Ctx a c d e f g → a → Plat e f g → String →
                    CompMap a b c d e f g → CtxMap a c d e f g → ChainMap b d c → [Tool c d e f g] → Int → Buildable a b c d e f g
component_buildable this_plat bbles comp@(Component _ chain_name _ _ _ _) ctx_top tag for_plat@(Plat arch _) outdir compmap ctxmap chainmap@(ChainMap chmap) tools slice_width =
    b
    where b                           = Buildable name comp ctx_top tag for_plat outdir out_filemap
          name                        = compute_buildable_name comp arch tag slice_width
          chtolis f (Chain ty tk chs) = (f ty tk) : (concat $ map (chtolis f) chs)
          chain_top@(Chain topty _ _) = chmap ! chain_name
          tkinds                      = chtolis (\_ tk -> tk) chain_top
          tkind_XIRmap                = H.fromList $ map (\tkind → (tkind, lefts $ eval_Ctx ctxmap tag for_plat tkind Nothing False ctx_top))
                                                         tkinds
          chainlinks                  = forge_chainlinks compmap ctxmap bbles chainmap tools tag name ctx_top chain_top (this_plat, for_plat) tkind_XIRmap topty outdir
          out_filemap                 = fromList [ (outf, (clink, b))
                                                 | clink@(ChainLink _ _ outf _ _ _ _) ← chainlinks ]
component_buildable _ _ comp@(Target name _ ty file deps act) ctx tag plat outdir _ _ _ _ _ =
    b
    where b = Buildable name comp ctx tag plat outdir
                        $ fromList [(file, (ChainLink deps ty file ty notool (\_ _ → []) (\out (Right ins) _ → act out ins), b))]

compute_buildables ∷ (Tag a, ChainName b, ToolKind c, Type d, Arch e, OSType f, OSVersion g) ⇒ Plat e f g → Schema a e f g → CompMap a b c d e f g → ChainMap b d c → [Tool c d e f g] → CtxMap a c d e f g → [Buildable a b c d e f g]
compute_buildables this_plat (Schema schema) compmap@(CompMap comap) chainmap tools (CtxMap ctxmap) =
    bbles
    where bbxms = [ (b, submap)
                  | comp                                             ← elems comap
                  , tag                                              ← cTags comp
                  , let Slice _ compKindOuts  = case H.lookup tag schema of
                                                  Just s  → s
                                                  Nothing → error $ printf "Build schema says nothing about kind %s.\nSchema: %s" (show tag) (pretty schema)
                        slice_width           = length compKindOuts
                        (ctx, pre_ctx_submap) = component_ctx comp
                  , (plat@(Plat arch _), outdir)                     ← compKindOuts
                  , let b             = component_buildable this_plat bbles comp ctx tag plat outdir compmap full_ctxmap chainmap tools slice_width
                        CtxMap submap = pre_ctx_submap arch tag slice_width ]
          bbles       = map fst bbxms
          full_ctxmap = CtxMap $ foldl H.union ctxmap $ map snd bbxms
