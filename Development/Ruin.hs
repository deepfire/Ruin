{-# LANGUAGE ConstraintKinds, DataKinds, DeriveDataTypeable, DeriveGeneric, ExtendedDefaultRules, FlexibleContexts, GADTs, KindSignatures, MultiWayIf, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TransformListComp, TupleSections, TypeOperators, TypeFamilies, QuasiQuotes, UndecidableInstances, UnicodeSyntax, ViewPatterns #-}

-- | A module for declarative definition of complex build systems.
-- Ruin tries to provide the build system engineer with a language closer to what
-- he himself might use in his head.

module Development.Ruin
    (
    -- * Location kinds & Path
      Locatable(..), LFixed(..)
    , SysExec(..), sysExec, BbleToolExec, toolExec
    , Path(..) 

    -- * The Build System Type
    , Build

    -- * Target environment
    , Arch, OS(..), OSType, OSVersion, Plat(..)
    , os_execsuffix

    -- * Source, intermediate & destination file types
    , Type, none, type_fusing_p, type_extension, retype_file
    , Inputs(..), FlagType(..)

    -- * Transformations
    , DefTool(..), exec_tool, ToolKind, notool, ToolAction, ToolActionSimple
    , Chain(..), ChainName, ChainMap(..), ChainLink(..)

    -- * Context
    , Ctx, CtxExp(..), CtxVal(..)
    , CtxMap(..), XQuery(..)
    , ctxR, ctxN, ctxRCase, ctxNCase, ctxRIf, ctxNIf, ctxRWhen, ctxNWhen

    -- * Components
    , Component(..), Tag, CompMap(..), component_name
    , Buildable(..), compute_buildables, buildable_output

    -- * Composition
    , Slice(..), Schema(..)

    -- * Parametrisation
    , BuildVar(..), var_stringify
    , VarMap(..), VarEnv(..), VarSpec(..), OptSpec, derive_merged_optmap, derive_optdescrs
    , varspec_add

    -- * Util
    , exec, shell, unsafeExec, unsafeShell, cmd
    , map_to_hashmap, invert_hashmap
    , lcstr, lcShow
    , (%>)

    -- * Entry point
    , RuinSpec(..)
    , ruinArgsWith
    ) where

import GHC.Exts (sortWith)
import GHC.Generics (Generic)
import GHC.Prim

import Control.Arrow ((&&&))
import Control.Monad (forM_)

import qualified Data.ByteString          as BS (readFile)    
import Data.Char (toLower)
import Data.Either (lefts)
import Data.Either.Utils (fromRight)
import Data.Functor ((<$>))
import Data.Hashable
import qualified Data.HashMap.Lazy as H
import Data.HashMap.Lazy (HashMap, elems, empty, fromList, keys, member, size, toList, union, (!))
import Data.List (intercalate)
import Data.Map.Lazy ()
import Data.Monoid (mconcat)
import qualified Data.HashSet as HS
import Data.String.Utils (startswith)
import Data.Tuple (swap)
import Data.Typeable

import Debug.Trace (trace)

import qualified Development.Shake         as S (cmd)
import qualified Development.Shake.Command as S (CmdArguments)
import Development.Shake                 hiding (cmd)
import Development.Shake.Command()
import Development.Shake.FilePath

import Prelude.Unicode

import System.Console.GetOpt (OptDescr(..), ArgDescr(..))
import System.Directory (findExecutable)
import System.Path.Glob (glob)
-- import System.Path.NameManip (dir_part, filename_part)

import System.Process (readProcess, readProcessWithExitCode)

import System.IO.Unsafe (unsafePerformIO)

import Text.PrettyPrint.GenericPretty (Out(..), doc)
import Text.PrettyPrint (parens, (<+>), ($$), text)
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

cmd ∷ S.CmdArguments args => args
cmd = S.cmd (Traced "")

unsafeExec ∷ String → [String] → String
unsafeExec executable args = unsafePerformIO (exec executable args)

unsafeShell ∷ String → String
unsafeShell comm = unsafePerformIO (shell comm)

instance (Out a, Out b) ⇒ Out (HashMap a b) where
  doc hm = parens $ foldl (\acc (k, v) → acc $$ doc k <+> text ":" <+> doc v) (text "") $ toList hm
  docPrec _ = doc


-- * A constraint for making user-presentable entities, suitable for a DSL
type family   C a :: Constraint
type instance C a = (Eq a, Generic a, Hashable a, Show a, Typeable a)


--- One to Bind Them All:
class (C (Type a), C (Arch a), C (OSType a), C (OSVersion a), C (ToolKind a), C (ChainName a), C (Tag a))
    ⇒ Build a where
    data Type a      ∷ *
    none             ∷ Type a
    type_fusing_p    ∷ Type a → Bool
    type_extension   ∷ Type a → String
    data Arch a      ∷ *
    data OSType a    ∷ *
    os_execsuffix    ∷ OSType a → String
    data OSVersion a ∷ *
    data ToolKind a  ∷ *
    notool           ∷ ToolKind a
    data ChainName a ∷ *
    data Tag a       ∷ *

deriving instance Typeable (Arch)
deriving instance Typeable (ChainName)
deriving instance Typeable (OSType)
deriving instance Typeable (OSVersion)
deriving instance Typeable (Tag)
deriving instance Typeable (ToolKind)
deriving instance Typeable (Type)


--- Source & result specification

retype_file ∷ (Build a) ⇒ Type a → String → String
retype_file newty f = f -<.> type_extension newty


--- Target specification
data OS a where
    OS ∷ (Build a) ⇒ OSType a → Maybe (OSVersion a) → OS a

deriving instance Show (OS a)
instance Hashable (OS a) where
    hashWithSalt s (OS x y)  = s `hashWithSalt` (hash x) `hashWithSalt` (hash y)
instance Eq  (OS a) where -- treat Nothing as a wildcard during comparison:
    (OS t1 Nothing) == (OS t2 _)       = t1 ≡ t2
    (OS t1 _)       == (OS t2 Nothing) = t1 ≡ t2
    (OS t1 v1)      == (OS t2 v2)      = t1 ≡ t2 ∧ v1 ≡ v2

data Plat a where
    Plat ∷ Build a ⇒ Arch a → OS a → Plat a
deriving instance Show (Plat a)
instance Hashable (Plat a) where
    hashWithSalt s (Plat x y)  = s `hashWithSalt` (hash x) `hashWithSalt` (hash y)
instance Eq  (Plat a) where
    Plat a1 os1 == Plat a2 os2 = a1 ≡ a2 ∧ os1 ≡ os2


--- Source → target transformation
type ToolAction        = String → [String] → [String] → Action ()
type ToolActionSimple  = String → [String] →            Action () -- a flagless option

data DefTool a where
    DefTool ∷ Build a ⇒ ToolKind a → [Type a] → Type a → [(Plat a, Plat a, ToolLoc a)] → (String → ToolAction) -> DefTool a

data Tool a where
    Tool    ∷ Build a ⇒ ToolKind a → (Type a, Type a) → (Plat a, Plat a) → LFixed → (String → ToolAction) -> Tool a

find_tool ∷ Build a ⇒ [DefTool a] → [Buildable a] → ToolKind a → (Type a, Type a) → (Plat a, Plat a) → Tool a
find_tool available_tools buildables want_toolkind (want_tyfrom, want_tyto) (this_plat, for_plat) =
    case [ Tool toolkind (tyfrom, tyto) (on, for) (LFixed $
                                                   case loc of
                                                     SysToolLoc  (PLook SE  name) → case unsafePerformIO $ findExecutable name of
                                                                                          Just f  → f
                                                                                          Nothing → error $ printf "Unable to locate system executable '%s'." name
                                                     CompToolLoc (PLook BTE bble) → buildable_output b where b = match_buildable buildables Nothing (Just this_plat) (Right bble)
                                                                                                                                 "while searching for a generated tool"
                                                     ) act
         | DefTool toolkind tyfroms tyto tool_variants act ← available_tools
         , tyfrom                                          ← tyfroms
         , (on, for, loc)                                  ← tool_variants
         , -- trace (printf "try %s<>%s %s<>%s %s<>%s %s<>%s %s<>%s"
           --               (show toolkind) (show want_toolkind) (show tyfrom) (show want_tyfrom) (show tyto) (show want_tyto) (show on) (show this_plat) (show for) (show for_plat)) $
           toolkind ≡ want_toolkind ∧ tyfrom ≡ want_tyfrom ∧ tyto ≡ want_tyto ∧ on ≡ this_plat ∧ for ≡ for_plat ] of
      []     → error (printf "Failed to find a suitable tool: (%s←%s) on-plat=%s to-plat=%s" (show want_tyto) (show want_tyfrom) (show this_plat) (show for_plat))
      tool:_ → tool

exec_tool ∷ Build a ⇒ Tool a → ToolAction
exec_tool (Tool _ _ _ (LFixed path) fnV) out ins flags = do
  need [path]
  fnV path out ins flags

-- Chains
-- Because of ambiguities of composition (f.e. both GCCLD and LD can perform CObj → Executable transforms)
-- we need hints.
-- A Chain is what provides these hints.


data Chain a where
    Chain ∷ Build a ⇒ Type a → ToolKind a → [Chain a] → Chain a
deriving instance Eq (Chain a)
deriving instance Show (Chain a)
newtype ChainMap a = ChainMap (HashMap (ChainName a) (Chain a))

data ChainLink a where
    ChainLink ∷ Build a ⇒ [String] → Type a → String → Type a → ToolKind a → XQuery a → ToolAction → ChainLink a

instance Show (ChainLink a) where
    show (ChainLink infs inty outf outty tkin _ _) = printf "#<LINK (%s<-%s) %s: %s ← %s>" (show outty) (show inty) (show tkin) (show outf) (show infs)


-- A condition for extending the build environment
data CtxExp a where
    AsPartOf  ∷ Build a ⇒ Tag a → CtxExp a                       -- ^ In the context of building under a specific Tag.
    ForArch   ∷ Build a ⇒ Arch a → CtxExp a                      -- ^ During build for a given Arch.
    ForOSType ∷ Build a ⇒ OSType a → CtxExp a                    -- ^ During build for a given OS.
    ForOS     ∷ Build a ⇒ OS a → CtxExp a                        -- ^ During build for a specific OS version.
    ForPlat   ∷ Build a ⇒ Plat a → CtxExp a                      -- ^ During build for a given Platform.
    ForInput  ∷ Build a ⇒ String → CtxExp a                      -- ^ For sources matching a wildcard.
    WithTool  ∷ Build a ⇒ ToolKind a → CtxExp a                  -- ^ While being transformed by a specific ToolKind.
    ShellTest ∷ Build a ⇒ String → String → CtxExp a             -- ^ When output of a pure shell command matches a provided string.
    ExecTest  ∷ Build a ⇒ String → [String] → String → CtxExp a  -- ^ When output of a pure executable run matches a provided string.
    Not       ∷ Build a ⇒ CtxExp a → CtxExp a
    And       ∷ Build a ⇒ [CtxExp a] → CtxExp a
    Or        ∷ Build a ⇒ [CtxExp a] → CtxExp a
    Always    ∷ Build a ⇒ CtxExp a

deriving instance Eq (CtxExp a)
deriving instance Show (CtxExp a)
instance Hashable (CtxExp a) where
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

data FlagType a where
    ForType ∷ Build a ⇒ Type a     → FlagType a  -- ^ For transforms employing tools of specified kind.
    ForTool ∷ Build a ⇒ ToolKind a → FlagType a  -- ^ For non-fusing transforms: the source type must match;  for fusing transforms: the destination type must match.
deriving instance Eq       (FlagType a)
deriving instance Show     (FlagType a)
deriving instance Typeable (FlagType)
instance Hashable (FlagType a) where
     hashWithSalt s (ForType x) = s `hashWithSalt` (hash x)
     hashWithSalt s (ForTool x) = s `hashWithSalt` (hash x)

data Inputs a where
    Srcs ∷ Build a ⇒ Type a → String → [String]             → Inputs a  -- ^ Tool-transformable input files (aka "sources");
                                                                         --    Second argument is common root, third is wildcards-as-per-System.Path.Glob
    Comp ∷ Build a ⇒ String                                 → Inputs a  -- ^ Component nesting, by name;  note that 'Comp' can't refer to ToolComponents.
    Gen  ∷ Build a ⇒ Type a → String → (String → Action ()) → Inputs a  -- ^ Arbitrary, command-generated leaves.
deriving instance Show     (Inputs a)
deriving instance Typeable (Inputs)
instance Build a ⇒ Eq (Inputs a) where
    (Srcs ty1 d1 fs1) == (Srcs ty2 d2 fs2) = ty1 ≡ ty2 ∧ d1 ≡ d2 ∧ fs1 ≡ fs2
    (Comp cn1)        == (Comp cn2)        = cn1 ≡ cn2
    (Gen  ty1 f1 _)   == (Gen  ty2 f2 _)   = ty1 ≡ ty2 ∧ f1 ≡ f2 -- XXX: is this a bug source?
    _                 == _                 = False
instance Build a ⇒ Hashable (Inputs a) where
    hashWithSalt s (Srcs a b c)  = s `hashWithSalt` (hash a) `hashWithSalt` (hash b) `hashWithSalt` (hash c)
    hashWithSalt s (Comp a)      = s `hashWithSalt` (hash a)
    hashWithSalt s (Gen  a b _)  = s `hashWithSalt` (hash a) `hashWithSalt` (hash b)

-- * An atom of build environment.
data CtxVal a where
    XFlags  ∷ Build a ⇒ FlagType a → [String] → CtxVal a   -- ^ Flags added at the current point of context.
    XInputs ∷ Build a ⇒ Inputs a              → CtxVal a   -- ^ Inputs added at the current point of context.
deriving instance Eq       (CtxVal a)
deriving instance Show     (CtxVal a)
deriving instance Typeable (CtxVal)
instance  Hashable (CtxVal a) where
    hashWithSalt s (XFlags x y) = s `hashWithSalt` (hash x) `hashWithSalt` (hash y)
    hashWithSalt s (XInputs x)  = s `hashWithSalt` (hash x)

-- * Second (flag) phase evaluation entry point.
data XQuery a where
    XQuery ∷ Build a ⇒ ((Type a, String) → Bool → [CtxVal a]) → XQuery a
instance Show     (XQuery a) where
    show (XQuery _) = "#<XQuery>"
deriving instance Typeable (XQuery)

-- Cross-phase context tracking
data XIR a where
    XIR ∷ Build a ⇒ XQuery a → CtxVal a → XIR a
deriving instance Show     (XIR a)
deriving instance Typeable (XIR)

eval_CtxExp ∷ Build a ⇒ Tag a → Plat a → ToolKind a → Maybe (Type a, String) → CtxExp a → Bool
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
data BuildVar
    = Var String        -- ^ Simple string-valued variable
    | List [String]     -- ^ List of strings.
    | Flag Bool         -- ^ Boolean flag variable.
    deriving (Eq, Show)

var_stringify ∷ String → BuildVar → String
var_stringify name (Var s)   = printf "--%s '%s'" name s
var_stringify name (List ss) = printf "--%s '%s'" name $ intercalate " " ss
var_stringify name (Flag f)  = if f then "--" ++ name else ""

newtype VarMap  = VarMap  (HashMap String BuildVar)
newtype VarEnv  = VarEnv  (String → BuildVar, String → String, String → [String], String → Bool)
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

data Ctx a where
    Ctx ∷ Build a
           ⇒ [Either String (Ctx a)]   -- ^ Parent context node names.
           → [(CtxExp a, [CtxVal a])]  -- ^ Constituent cases.
           → Ctx a
deriving instance Eq       (Ctx a)
deriving instance Show     (Ctx a)
instance Hashable (Ctx a) where
    hashWithSalt s (Ctx x y) = s `hashWithSalt` (hash x) `hashWithSalt` (hash y)
newtype CtxMap a = CtxMap (HashMap String (Ctx a))

make_xquery ∷ Build a ⇒ CtxMap a → Tag a → Plat a → ToolKind a → Ctx a → XQuery a
make_xquery ctxmap tag plat tkind ctx =
    XQuery xquery
    where xquery ty_f explainp = map fromRight $ eval_Ctx ctxmap tag plat tkind (Just ty_f) explainp ctx

eval_Ctx ∷ ∀ a . Build a
             ⇒ CtxMap a → Tag a → Plat a → ToolKind a     -- context (general, not Ctx sense)
             → Maybe (Type a, String) → Bool              -- stage specifier, turn on explanation mode
             → Ctx a → [Either (XIR a) (CtxVal a)]        -- argument → return value
eval_Ctx cxm@(CtxMap ctxmap) tag plat tool stage_ explain_mode ctx_top =
    ret
    where (_, ret)  = eval (HS.empty, []) (Right ctx_top)
          namemap   = invert_hashmap ctxmap
          ctxdesc x = fromMaybe (printf "<unnamed ctx node %s>" (show x)) (H.lookup x namemap)
          find_parent (Right p) = p
          find_parent (Left n)  = case H.lookup n ctxmap of
                                    Nothing → error $ printf "Unknown context parent node name: '%s'" n
                                    Just x  → x
          eval_case ctxval_res val_filter_fn (caseCond, caseVals) =
              [ ctxval_res val | eval_CtxExp tag plat tool stage_ caseCond,
                                 val           ← caseVals,
                                 val_filter_fn val ]
          eval (seen, acc) (find_parent → this@(Ctx parents cases)) =
              if HS.member this seen then (if explain_mode then trace (printf "skip> %s" $ ctxdesc this) else id)
                                          (seen, acc)
              else (if explain_mode then trace (printf "visit> %s → %s" (ctxdesc this) (show added)) else id) res
                   where (ctxval_res, val_filter_fn)
                               = case stage_ of
                                   Nothing       → (\val → Left $ XIR xquery val,         ctxval_ξp)
                                        where xquery = make_xquery cxm tag plat tool this
                                   Just (ty, _)  → (\val → Right $ val,                   ctxval_xflags_matchp ty tool)
                         ctxval_ξp                    (XInputs _ )            = True
                         ctxval_ξp                    _                       = False
                         ctxval_xflags_matchp ∷ Build a ⇒ Type a → ToolKind a → CtxVal a → Bool
                         ctxval_xflags_matchp xty _   (XFlags (ForType ty) _) = xty ≡ ty
                         ctxval_xflags_matchp _   xto (XFlags (ForTool to) _) = xto ≡ to
                         ctxval_xflags_matchp _   _   _                       = False
                         added = concat (map (eval_case ctxval_res val_filter_fn) cases)
                         res   = foldl eval (HS.insert this seen, added ++ acc) parents

-- syntactic sugar for pretty Ctx creation.  Might go unused at some point.
ctxR ∷ Build a ⇒ [CtxVal a] → Ctx a
ctxR vals =
    Ctx [] [(Always, vals)]

ctxRWhen ∷ Build a ⇒ CtxExp a → [CtxVal a] → Ctx a
ctxRWhen expr vals =
    Ctx [] [(expr, vals)]

ctxRIf ∷ Build a ⇒ CtxExp a → [CtxVal a] → [CtxVal a] → Ctx a
ctxRIf expr thens elses =
    Ctx [] $ [(expr, thens)] ++ [(Always, elses)]

ctxRCase ∷ Build a ⇒ [(CtxExp a, [CtxVal a])] → [CtxVal a] → Ctx a
ctxRCase cases elses =
    Ctx [] $ cases ++ [(Always, elses)]

ctxN ∷ Build a ⇒ [String] → [CtxVal a] → Ctx a
ctxN parents vals =
    Ctx (map Left parents) [(Always, vals)]

ctxNWhen ∷ Build a ⇒ [String] → CtxExp a → [CtxVal a] → Ctx a
ctxNWhen parents expr vals =
    Ctx (map Left parents) [(expr, vals)]

ctxNIf ∷ Build a ⇒ [String] → CtxExp a → [CtxVal a] → [CtxVal a] → Ctx a
ctxNIf parents expr thens elses =
    Ctx (map Left parents) $ [(expr, thens)] ++ [(Always, elses)]

ctxNCase ∷ Build a ⇒ [String] → [(CtxExp a, [CtxVal a])] → [CtxVal a] → Ctx a
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
data Component a =
    Component {
      cName            ∷ String,
      cChainName       ∷ ChainName a,
      cTags            ∷ [Tag a],
      cCtxNames        ∷ [String],
      cAlways          ∷ [CtxVal a],
      cCases           ∷ [(CtxExp a, [CtxVal a])]
    } |
    ToolComponent {
      cToolName        ∷ ToolKind a,
      cChainName       ∷ ChainName a,
      cTags            ∷ [Tag a],
      cCtxNames        ∷ [String],
      cAlways          ∷ [CtxVal a],
      cCases           ∷ [(CtxExp a, [CtxVal a])]
    } |
    Target {
      cName            ∷ String,
      cTags            ∷ [Tag a],
      cType            ∷ Type a,
      cFile            ∷ String, -- Source
      cDeps            ∷ [CtxVal a],
      cAction          ∷ ToolActionSimple
    }

component_name ∷ Build a ⇒ Component a → CompName a
component_name (ToolComponent ton _ _ _ _ _) = Right ton
component_name comp                          = Left $ cName comp

lookup_component ∷ Build a ⇒ CompMap a → String → Component a
lookup_component (CompMap comap) cname =
    case H.lookup (Left cname) comap of
      Nothing → error $ printf "Unknown component: %s" $ show cname
      Just co → co

type CompName a = Either String (ToolKind a)

newtype CompMap a = CompMap (HashMap (CompName a) (Component a))

input_type ∷ Build a ⇒ CompMap a → ChainMap a → Inputs a → Type a
input_type comap chainmap inp =
    case inp of
      Comp cname       → component_type chainmap $ lookup_component comap cname
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
data Buildable a where
    Buildable ∷ {
        bName       ∷ String,
        bComponent  ∷ Component a,
        bCtx        ∷ Ctx a,
        bTag        ∷ Tag a,
        bPlat       ∷ Plat a,
        bPath       ∷ String,
        bOutFiles   ∷ HashMap String (ChainLink a, Buildable a)
    } → Buildable a

match_buildable ∷ Build a ⇒ [Buildable a] → Maybe (Tag a) → Maybe (Plat a) → CompName a → String → Buildable a
match_buildable buildables maybetag maybeplat compname reason =
    case results of
      []    → error $ printf "Unable to find a matching buildable %s:  comp=%s  tag=%s  plat=%s." reason (show compname) (show maybetag) (show maybeplat)
      x : _ → x
    where results = [ b
                      | b@(Buildable _ comp _ btag sliceplat _ _) ← buildables
                      , (component_name comp) ≡ compname ∧ btag ≡ (fromMaybe btag maybetag) ∧ sliceplat ≡ (fromMaybe sliceplat maybeplat) ]


-- * Path generalisation
data LFixed = LFixed String deriving (Eq, Generic, Show)

class Locatable a where
    type LocName a   ∷ *
    type LocCtx a    ∷ *

data Path a where
    PFixed ∷                   LFixed    → Path LFixed  -- ^ A concrete path -- a pathname.
    PLook  ∷ Locatable b ⇒ b → LocName b → Path b       -- ^ Implied path subject to 'lookup'-based resolution.

data                BbleToolExec a  = BTE
instance Build a ⇒ Locatable (BbleToolExec a) where
    type LocName   (BbleToolExec a) = ToolKind a
    type LocCtx    (BbleToolExec a) = ([Buildable a], Plat a)

data               SysExec = SE
instance Locatable SysExec where
    type LocName   SysExec = String
    type LocCtx    SysExec = ()

sysExec ∷ Build a ⇒ String → ToolLoc a
sysExec name = SysToolLoc $ PLook SE name

toolExec :: Build a ⇒ ToolKind a → ToolLoc a
toolExec tk  = CompToolLoc $ PLook BTE tk

-- class Build a ⇒ ToolLoc a where
--     location
data ToolLoc a where
    SysToolLoc  ∷ Build a ⇒ Path SysExec          → ToolLoc a
    CompToolLoc ∷ Build a ⇒ Path (BbleToolExec a) → ToolLoc a

-- resolve_tool :: Build a ⇒ ToolLoc a → (LFixed, Bool)
-- resolve_tool ctx loc = 

component_type ∷ Build a ⇒ ChainMap a → Component a → Type a
component_type (ChainMap chmap) (Component _ chname _ _ _ _)     = ty where (Chain ty _ _) = chmap ! chname
component_type (ChainMap chmap) (ToolComponent _ chname _ _ _ _) = ty where (Chain ty _ _) = chmap ! chname
component_type _                (Target _ _ ty _ _ _)            = ty

buildable_output ∷ Buildable a → String
buildable_output (Buildable name (Component _ _ _ _ _ _) _ _     (Plat _ (OS osty _)) path _) = path </> name <.> os_execsuffix osty
buildable_output (Buildable name (ToolComponent _ _ _ _ _ _) _ _ (Plat _ (OS osty _)) path _) = path </> name <.> os_execsuffix osty
buildable_output (Buildable _    (Target _ _ _ file _ _) _ _ _ _ _)                           = file

type ChainLinkConsCtx = (String, Int, Int)

component_name_output ∷ Build a ⇒ [Buildable a] → Plat a → String → String
component_name_output buildables to_plat compname =
    buildable_output $ match_buildable buildables Nothing (Just to_plat) (Left compname) "during component lookup"

ξ_files ∷ Build a ⇒ [Buildable a] → Plat a → Inputs a → [String]
ξ_files buildables to_plat ξ =
    case ξ of
      Comp compname    → [component_name_output buildables to_plat compname]
      Gen  _ outf _    → [outf]
      Srcs ty bas pats → concat $ map expand_pattern pats
          where expand_pattern p = unsafePerformIO $ glob $ bas </> p ++ type_extension ty

-- * Compute chainlinks for a given set of XInputs (ξs)
ξs_chainlinks ∷ Build a ⇒ CompMap a → ChainMap a → [Buildable a] → Plat a → Type a → [XIR a] → [(Inputs a, ChainLink a)]
ξs_chainlinks compmap chainmap buildables to_plat thisty xirs =
    cls
    where cls = [ (inp, ChainLink [] none f inp_ty notool xquery
                               $ case inp of
                                   Gen _ _ actn     → \out _ _ → actn out
                                   _                → \_ _ _   → return ())
                | XIR xquery (XInputs inp)  ← xirs,
                  let inp_ty   = input_type compmap chainmap inp,
                  inp_ty ≡ thisty,
                  f                         ← ξ_files buildables to_plat inp ]

do_forge_chainlinks ∷ ∀ a . Build a ⇒
                    CompMap a → CtxMap a → [Buildable a] → ChainMap a → [DefTool a] →
                    Tag a → ChainLinkConsCtx → Ctx a → Chain a → (Plat a, Plat a) → HashMap (ToolKind a) [XIR a] → Type a → String →
                    ([(Inputs a, ChainLink a)], [(Inputs a, ChainLink a)])
do_forge_chainlinks compmap ctxmap buildables chainmap tooldefs tag (clink_name, depth, idx) ctx_top (Chain thisty tkind children_chains) (on_plat, to_plat) tool_XIRmap upwardty outdir =
    let tool             = find_tool tooldefs buildables tkind (thisty, upwardty) (on_plat, to_plat)
        clink_xform      = exec_tool tool
        id_step chidx    = (printf "%s.%d" clink_name idx, depth + 1, chidx)
        leafp            = null children_chains
        leaf_ins_ephemeral (Gen _ _ _) = False
        leaf_ins_ephemeral _           = True
        (acc,
         result) = if leafp
                   then ([], ξs_chainlinks compmap chainmap buildables to_plat thisty (tool_XIRmap ! tkind))
                   else let intrep = [ do_forge_chainlinks compmap ctxmap buildables chainmap tooldefs tag (id_step i) ctx_top chain (on_plat, to_plat) tool_XIRmap thisty outdir
                                     | (i, chain) ← zip [1..] children_chains ]
                            intrep        ∷ [([(Inputs a, ChainLink a)], [(Inputs a, ChainLink a)])]
                        in mconcat intrep ∷  ([(Inputs a, ChainLink a)], [(Inputs a, ChainLink a)])
    in
    (-- accumulate promotable results from leaf processing and subchains
     acc ++ let res = filter (if | depth ≡ 0 → const False
                                 | leafp     → not ∘ leaf_ins_ephemeral ∘ fst
                                 | True      → const True)
                      result
                (inp, link) = unzip res in
            ignt trace (printf "4   ξ → link @ %s:  %s → %s" (show $ id_step (0-1)) (show inp) (show link)) res,
     -- compute result
     if
     | depth ≡ 0              → [     (ins, ChainLink ifs ity outfile upwardty tk xq xf)
                                | let (ins, ChainLink ifs ity _       _        tk xq xf):_ = result
                                      outfile = outdir </> clink_name <.> (type_extension upwardty) ]
     | type_fusing_p upwardty → [ (ins, ChainLink (foldl (\inacc (_, ChainLink _ _ inarg _ _ _ _) → inacc ++ [inarg])
                                                          [] result)
                                                   thisty outfile upwardty tkind xquery clink_xform)
                                | let outfile = outdir </> clink_name <.> (type_extension upwardty)
                                      ins     = Srcs upwardty "" []
                                      xquery  = make_xquery ctxmap tag to_plat tkind ctx_top ]
     | True                   → [ (ins, ChainLink [infile] thisty outfile upwardty tkind xquery clink_xform)
                                | (ins, ChainLink _        _      infile  _        _     _      _) ← result,
                                  let outfile = (if leafp then outdir else "") </> retype_file upwardty infile
                                      xquery  = make_xquery ctxmap tag to_plat tkind ctx_top])

forge_chainlinks ∷ ∀ a . Build a
                     ⇒ CompMap a → CtxMap a → [Buildable a] → ChainMap a → [DefTool a]
                     → Tag a → String → Ctx a → Chain a → (Plat a, Plat a) → H.HashMap (ToolKind a) [XIR a] → Type a → String
                     → [ChainLink a]
forge_chainlinks compmap ctxmap bbles chainmap tools tag name ctx_top chain platxform tkind_XIRmap outtype outdir =
    let (acc, ret) = do_forge_chainlinks compmap ctxmap bbles chainmap tools tag (name, 0, 0) ctx_top chain platxform tkind_XIRmap outtype outdir
        res        = acc ++ ret
        res        ∷ [(Inputs a, ChainLink a)]
        all_inputs = [ (x, []) | XIR _ (XInputs x) ← concat $ H.elems tkind_XIRmap ]
        inp_links  = H.fromListWith (++) $ (all_inputs ++ map (fst &&& (:[]) ∘ snd) res)
        inp_links  ∷ HashMap (Inputs a) [ChainLink a]
        orphan_ins = H.filter null inp_links
    in if not $ H.null orphan_ins
       then error (printf "Orphans inputs detected! (inputs that weren't picked up by any tools):\n   %s" $
                          intercalate "\n   " $ map (\i → printf "%s :: %s" (show i) (show $ input_type compmap chainmap i)) $ H.keys orphan_ins)
       else map snd res

--   Slice∷
--    - component set expansion = [Component] * [component_kinds Component] * [component_plats Component (component_kinds Component)]
--    - required target platforms
--    - output path
--
data Slice a where
    Slice ∷ Build a ⇒ { sliceTag   ∷ Tag a
                      , slicePlats ∷ [(Plat a, String)]
                      } → Slice a
deriving instance Show (Slice a)

newtype Schema a = Schema (HashMap (Tag a) (Slice a))

-- | derive a unique name for the Buildable, according to the power of its Component's [Kind * Plat] product
compute_buildable_name ∷ Build a ⇒ Component a → Arch a → Tag a → Int → String
compute_buildable_name comp arch tag slice_width =
    let product_name name tags = 
          if | length tags ≡ 1 ∧ slice_width ≡ 1 → name
             | length tags ≡ 1                   → name ++ "-" ++ lcShow arch
             |                   slice_width ≡ 1 → name ++ "-" ++ lcShow tag
             | True                              → name ++ "-" ++ lcShow tag  ++ "-" ++ lcShow arch
    in case comp of
         Target        cmName _ _      _ _ _ → cmName
         Component     cmName _ cmTags _ _ _ → product_name cmName cmTags
         ToolComponent cmName _ cmTags _ _ _ → product_name (lcShow cmName) cmTags

component_ctx ∷  Build a ⇒ Component a → (Ctx a, Arch a → Tag a → Int → CtxMap a)
component_ctx comp =
    let compctx parentCtxs always conds = (condsCtx, pre_ctxmap)
            where alwaysCtx = ctxN parentCtxs always
                  condsCtx  = Ctx [Right alwaysCtx] conds
                  pre_ctxmap arch tag slice_width = CtxMap $ fromList [(name, condsCtx), (name ++ "-common", alwaysCtx)]
                      where name = compute_buildable_name comp arch tag slice_width
    in case comp of
         Component     _ _ _ x y z → compctx x y z
         ToolComponent _ _ _ x y z → compctx x y z
         Target _ _ _ _ _ _        → (Ctx [] [], \_ _ _ → CtxMap H.empty)

component_buildable ∷ ∀ a . Build a ⇒ Plat a → [Buildable a] → Component a → Ctx a → Tag a → Plat a → String → CompMap a → CtxMap a → ChainMap a → [DefTool a] → Int
                    → Buildable a
component_buildable this_plat bbles comp ctx_top tag for_plat@(Plat arch _) outdir compmap ctxmap chainmap@(ChainMap chmap) tools slice_width =
    let compbble chain_name = b
            where b                           = Buildable name comp ctx_top tag for_plat outdir out_filemap
                  name                        = compute_buildable_name comp arch tag slice_width
                  chtolis  ∷ (Type a → ToolKind a → ToolKind a) → Chain a → [ToolKind a]
                  chtolis f (Chain ty tk chs) = (f ty tk) : (concat $ map (chtolis f) chs)
                  chain_top@(Chain topty _ _) = chmap ! chain_name
                  tkinds                      = chtolis (\_ tk -> tk) chain_top
                  tkind_XIRmap                = H.fromList $ map (\tkind → (tkind, lefts $ eval_Ctx ctxmap tag for_plat tkind Nothing False ctx_top))
                                                                 tkinds
                  chainlinks                  = forge_chainlinks compmap ctxmap bbles chainmap tools tag name ctx_top chain_top (this_plat, for_plat) tkind_XIRmap topty outdir
                  out_filemap                 = fromList [ (outf, (clink, b))
                                                         | clink@(ChainLink _ _ outf _ _ _ _) ← chainlinks ]
    in case comp of
      Component     _ chain_name _ _ _ _ → compbble chain_name
      ToolComponent _ chain_name _ _ _ _ → compbble chain_name
      Target name _ ty file deps act     → b
          where b = Buildable name comp ctx_top tag for_plat outdir
                    $ fromList [(file, (ChainLink depfiles ty file ty notool (XQuery (\_ _ → [])) (\out ins _ → act out ins), b))]
                depfiles = concat [ ξ_files bbles for_plat ξ | XInputs ξ ← deps]

compute_buildables ∷ Build a ⇒ Plat a → Schema a → CompMap a → ChainMap a → [DefTool a] → CtxMap a → [Buildable a]
compute_buildables this_plat (Schema schema) compmap@(CompMap comap) chainmap tools (CtxMap ctxmap) =
    bbles
    where bbxms = [ (b, submap)
                  | comp                                             ← elems comap
                  , tag                                              ← cTags comp
                  , let Slice _ compKindOuts  = case H.lookup tag schema of
                                                  Just s  → s
                                                  Nothing → error $ printf "Build schema says nothing about kind %s.\nSchema: %s" (show tag) (show schema)
                        slice_width           = length compKindOuts
                        (ctx, pre_ctx_submap) = component_ctx comp
                  , (plat@(Plat arch _), outdir)                     ← compKindOuts
                  , let b             = component_buildable this_plat bbles comp ctx tag plat outdir compmap full_ctxmap chainmap tools slice_width
                        CtxMap submap = pre_ctx_submap arch tag slice_width ]
          bbles       = map fst bbxms
          full_ctxmap = CtxMap $ foldl H.union ctxmap $ map snd bbxms

--
data RuinSpec a where
    RuinSpec ∷ Build a ⇒ {
      ruinFiles      ∷ String
    , ruinSpecFile   ∷ String
    , ruinThisPlat   ∷ Plat a
    , ruinContext    ∷ VarEnv → CtxMap a
    , ruinTools      ∷ [DefTool a]
    , ruinChains     ∷ ChainMap a
    , ruinComponents ∷ VarEnv → CompMap a
    , ruinSchema     ∷ VarEnv → Plat a → Schema a
    , ruinOpts       ∷ OptSpec
    , ruinVars       ∷ VarEnv → VarMap
    , ruinDefaults   ∷ VarEnv → HashMap String String
    , ruinCopies     ∷ VarEnv → HashMap String String
    , ruinSynonyms   ∷ VarEnv → HashMap String String
    } → RuinSpec a

-- ^ Empty RuinOptions:
-- ruinOptions = RuinOptions { ruinFiles      = ".shake"
--                           , ruinSpecFile   = "Build.hs"
--                           , ruinThisPlat   = error "RuinOptions: ruinThisPlat must be specified."
--                           , ruinContext    = const $ CtxMap empty
--                           , ruinTools      = []
--                           , ruinChains     = ChainMap empty
--                           , ruinComponents = const $ CompMap empty
--                           , ruinSchema     = const $ Schema empty
--                           , ruinOpts       = []
--                           , ruinVars       = const $ VarMap empty
--                           , ruinDefaults   = const empty
--                           , ruinCopies     = const empty
--                           , ruinSynonyms   = const empty }

-- * Entry point:
-- main ∷ IO ()
-- main = ruinArgsWith RuinSpec { ruinFiles      = (unsafePerformIO $ S.getEnv "shakedir")
--                              , ruinSpecFile   = "Build.hs"                 -- Or, automagically, using TH: $(LitE . StringL . loc_filename <$> location)
--                              , ruinThisPlat   = YourPlatformType
--                              , ruinContext    = main_ctxspec
--                              , ruinTools      = main_tools
--                              , ruinChains     = main_chains
--                              , ruinComponents = main_compspec
--                              , ruinSchema     = main_schema
--                              , ruinOpts       = main_options
--                              , ruinVars       = main_varspec
--                              , ruinDefaults   = main_defaultspec
--                              , ruinCopies     = main_copyspec
--                              , ruinSynonyms   = main_syntargetspec }
--                     $ \params targets varenv outFileBuildables nameBuildables → do
--     let VarEnv (_, varS, _, varB) = varenv                                 -- Ruin computed the aggregate variable environment
--         isBuildableOutFile f = member f outFileBuildables                  -- Here's how to query the computed buildable database
--         isABuildableName   n = member n nameBuildables                     -- ...
--
--     want $ if
--            | null targets  → ["info"]                                      -- A number of targets is provided by Ruin out of the box
--            | True          → targets
--
--     -- here go the usual Shake rules:
--     phony "recurse" $ do
--       ⊥
ruinArgsWith ∷ Build a ⇒
                   RuinSpec a
                 → ([(String, BuildVar)] → [String] → VarEnv → HashMap String (ChainLink a, (Buildable a)) → HashMap String (Buildable a)
                    → Rules ())
                 → IO ()
ruinArgsWith (RuinSpec shakeDir buildHs ruinThisPlat ruinContext ruinTools ruinChains ruinComponents ruinSchema ruinOpts ruinVars ruinDefaults ruinCopies ruinSynonyms) rules = do
  checksum <- dropWhile (== '-') . show . hashWithSalt 0 <$> BS.readFile buildHs
  shakeArgsWith shakeOptions { shakeFiles   = shakeDir
                             , shakeVersion = "hash-" ++ checksum ++ "-" ++ buildHs }
                (derive_optdescrs ruinOpts) $ \params targets → return $ Just $ do

    let varenv@(VarEnv (var, varS, _, _))
                             = varspec_add ruinVars $ derive_merged_optmap ruinOpts (VarMap $ fromList params)
        schema               = ruinSchema        varenv ruinThisPlat
        ctxmap               = ruinContext       varenv
        compmap              = ruinComponents    varenv
        defaults             = ruinDefaults      varenv
        copies               = ruinCopies        varenv
        syntargets           = ruinSynonyms      varenv
        buildables           = compute_buildables ruinThisPlat schema compmap ruinChains ruinTools ctxmap
    --
        outFileBuildables    = foldl union empty $ map bOutFiles buildables
        nameBuildables       = map_to_hashmap bName buildables
        isBuildableOutFile f = H.member f outFileBuildables
        isABuildableName   n = H.member n nameBuildables

    phony "info" $ do
      putNormal $ printf "; specified options:"
      putNormal $ printf ";    %s" (show params)
      putNormal $ printf "; effective options:"
      forM_ [ name | (name, _, _, _) <- ruinOpts ] $ \opt →
          putNormal $ printf ";   %15s: %s" opt (show $ var opt)
      putNormal $ printf "; targets: %s" (show targets)
      putNormal $ printf "; known buildables:"
      forM_ (map bName buildables) $ \n →
          putNormal $ printf "   %s" n

    phony "info-chainlinks" $ do
      let bbname = varS "buildable"
      putNormal $ printf "Chainlinks of buildable '%s':" bbname
      forM_ [ (name, clink)
            | (Buildable name _ _ _ _ _ ofs)   ← buildables,
              name ≡ bbname,
              (clink@(ChainLink ifi _ _ _ _ _ _), _) ← elems ofs,
              then sortWith by ifi ]
            $ \ (name, clink) →
                putNormal $ printf "%s: %s" name (show clink)

    phony "info-buildable" $ do
      let name                                    = varS "buildable"
          b@(Buildable _ _ _ tag plat _ outfiles) = nameBuildables ! name
      putNormal $ printf "Info for buildable '%s':" name
      putNormal $ printf "        file: %s" $ buildable_output b
      putNormal $ printf "         tag: %s" $ show tag
      putNormal $ printf "        plat: %s" $ show plat
      putNormal $ printf "  # of files: %s" $ show $ size outfiles

    forM_ (keys defaults) $ \file →
        file %> \name → do
          putNormal $ printf "default> %s" name
          need [defaults ! name]
          cmd "cp" (defaults ! name) name "-n" 

    forM_ (keys copies) $ \file →
        file %> \name → do
          putNormal $ printf "copy> %s" name
          need [copies ! name]
          cmd "cp" (copies ! name) name 

    phonys $ \name → pwhen (member name syntargets) $ do
      putLoud $ printf "synonym> %s" name
      need [syntargets ! name]

    isBuildableOutFile ?> \outfile → do
      putLoud $ printf "file>"
      putLoud $ printf "file> %s" outfile
      putLoud $ printf "file>"
      verbosity <- getVerbosity
      let loudp                                                                                  = verbosity >= Loud
          (ChainLink infiles inty _ outty _ (XQuery xquery) tool, Buildable name comp _ _ _ _ _) = outFileBuildables ! outfile
          fusionp                                                                                = type_fusing_p outty
          infilesdesc                                                                            = if length infiles < 5 ∨ loudp then intercalate ", " infiles
                                                                                                   else printf "[..%d files..]" $ length infiles
      need infiles -- XXX: CONFIG_EFFECTIVE
      putNormal $ printf "%s<-%s)  %s <- %s, for %s" (show outty) (show inty) outfile infilesdesc name
      let (query_file, query_type, input, flags) =
              let fgs = concat [ fs
                               | XFlags _ fs ← xquery (query_type, query_file) loudp]
              in case comp of
                   Target _ _ _ _ _ _ → ("",           outty, infiles, [])
                   _ | fusionp        → (outfile,      outty, infiles, fgs)
                   _                  → (infiles !! 0, inty,  infiles, fgs)
      tool outfile input flags

    phonys $ \name → pwhen (isABuildableName name) $ do
      putLoud $ printf "buildable> %s, %s" name $ show $ keys nameBuildables
      putLoud $ printf "buildable> %s, %s" name $ show $ buildable_output $ nameBuildables ! name
      need [buildable_output $ nameBuildables ! name]

    rules params targets varenv outFileBuildables nameBuildables

pwhen ∷ Bool → a → Maybe a
pwhen cond body = if not cond then Nothing else Just body
