module Language.F2JS.DSL where
import Data.String
import Language.F2JS.AST
import Language.F2JS.Util

instance IsString Name where
  fromString = Str
instance IsString Expr where
  fromString = Global . fromString
instance IsString Lit where
  fromString = String
instance Num Lit where
  fromInteger = Double . fromInteger
  (+) = undefined
  (*) = undefined
  (-) = undefined
  abs = undefined
  signum = undefined
instance Num Expr where
  fromInteger = Var . fromInteger
  l + r = (PrimOp Plus `App` l) `App` r
  l * r = (PrimOp Times `App` l) `App` r
  l - r = (PrimOp Minus `App` l) `App` r
  abs = undefined
  signum = undefined

double :: Double -> Lit
double = Double

string :: String -> Lit
string = String

bool :: Bool -> Lit
bool = Bool

-- | Construct a tag for a constructor
tag :: Int -> Tag
tag = Tag

-- | Turn a string into an abstract name
name :: String -> Name
name = Str

-- | DeBruijn variables
var :: Int -> Expr
var = Var

-- | A simple, literal expression
lit :: Lit -> Expr
lit = Lit

-- | A lambda, binds one debruijn variable
lam :: Expr -> Expr
lam = Lam Nothing

-- | A global variable
global :: Name -> Expr
global = Global

record :: [(Name, Expr)] -> Expr
record = Record

letrec :: [Expr] -> Expr -> Expr
letrec bs e = LetRec (map (Bind Nothing) bs) e

prim :: PrimOp -> Expr
prim = PrimOp

proj :: Expr -> Name -> Expr
proj = Proj

match :: Expr -> [(Pat, Expr)] -> Expr
match = Case

app :: Expr -> Expr -> Expr
app = App

con :: Tag -> [Expr] -> Expr
con = Con

litPat :: Lit -> Pat
litPat = LitPat

conPat :: Tag -> Int -> Pat
conPat = ConPat

define :: Name -> Int -> Expr -> Decl
define = TopLevel
