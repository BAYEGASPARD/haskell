module MyProject where
import Data.Maybe ( fromMaybe )


run :: IO ()
run = putStrLn "Welcome to Haskell!"



-- | Expressions having some variables.
data Expr a
  = Lit Integer -- ˆ Integer literal.
  | Var a -- ˆ Variable
  | Add (Expr a) (Expr a) -- ˆ Addition.
  | Mul (Expr a) (Expr a) -- ˆ Multiplication.
  | Negate (Expr a)
  | Abs (Expr a)
  | Signum (Expr a)
  deriving (Show)


instance Functor Expr where
  fmap _ (Lit x) = Lit x
  fmap f (Var x) = Var (f x)
  fmap f (Add x y) = Add (fmap f x) (fmap f y)
  fmap f (Mul x y) = Mul (fmap f x) (fmap f y)
  fmap f (Negate x) = Negate (fmap f x)
  fmap f (Abs x) = Abs (fmap f x)
  fmap f (Signum x) = Signum (fmap f x)
  
  
  
  
-- | Display an expression with variables.
display :: Expr String -> String
display (Lit n) = show n
display (Var s) = s
display (Add epx1 e2) = display epx1 ++ " + " ++ display e2
display (Mul epx1 e2) = "(" ++ display epx1 ++ ") * (" ++ display e2 ++ ")"
display (Negate expr) = "-" ++ display expr
display (Abs expr) = "|" ++ display expr ++ "|"
display (Signum expr) = "signum(" ++ display expr ++ ")"

-- | Num instance for nice syntax.
instance Num (Expr a) where
  (+) = Add
  (*) = Mul
  fromInteger = Lit
  negate = Negate
  abs = Abs
  signum = Signum
  -- NOTE: there are more methods, but we leave them undefined for now

  
-- / Convert from generalised to simple expression
toExpr :: GExpr IExpr a -> Expr a
toExpr (GOp (ILit n))     = Lit n
toExpr (GVar n)           = Var n
toExpr (GOp (IAdd epx1 e2)) = Add (toExpr epx1) (toExpr e2)
toExpr (GOp (IMul epx1 e2)) = Mul (toExpr epx1) (toExpr e2)
toExpr (GOp (INegate expr)) = Negate (toExpr expr)
toExpr (GOp (IAbs expr)) = Abs (toExpr expr)
toExpr (GOp (ISignum expr)) = Signum (toExpr expr)



-- | Evaluate an expression with all variables instantiated.
eval :: Expr Integer -> Integer
eval (Lit n) = n
eval (Var n) = n
eval (Add epx1 e2) = eval epx1 + eval e2
eval (Mul epx1 e2) = eval epx1 * eval e2
eval (Negate expr) = -(eval expr)
eval (Abs expr) = abs (eval expr)
eval (Signum expr) = signum (eval expr)



-- | Evaluate an expression using an associative list
-- to lookup variable values and a default value
-- (in case there is no value in associative list).
evalWith :: Eq var => Int -> [(var, Int)] -> Expr var -> Int
evalWith defaultValue [] _ = defaultValue
evalWith defaultValue vars expr = fromInteger $ eval (fmap getValue expr)
  where
    getValue key = fromIntegral $ fromMaybe defaultValue (lookup key vars)

-- | Display an expression using a given
-- display function for variables.
displayWith :: (var -> String) -> Expr var -> String
displayWith f expr = display $ fmap f expr  

getVal 
  :: Eq a 
  => [(a, Expr a)] 
  -> Expr a 
  -> a 
  -> Expr a
getVal list defaultValue key 
  = fromMaybe defaultValue (lookup key list) 

expandVars 
  :: Expr String 
  -> [(String, Expr String)] 
  -> Expr String 
  -> Expr String
expandVars defaultValue vars 
  = fmap (display . getVal vars defaultValue)

data GExpr f a
  = GVar a -- variable
  | GOp (f (GExpr f a)) -- generalised operation/literal


data IExpr expr
  = ILit Integer
  | IAdd expr expr
  | IMul expr expr
  | INegate expr
  | IAbs expr
  | ISignum expr
  deriving (Show)

instance Functor IExpr where
  fmap _ (ILit x) = ILit x
  fmap f (IAdd x y) = IAdd (f x) (f y)
  fmap f (IMul x y) = IMul (f x) (f y)
  fmap f (IAbs x) = IAbs (f x)
  fmap f (INegate x) = INegate (f x)
  fmap f (ISignum x) = ISignum (f x)

-- / Convert from simple to generalised expression
fromExpr :: Expr a -> GExpr IExpr a
fromExpr (Lit n)     = GOp (ILit n)
fromExpr (Var n)     = GVar n
fromExpr (Add epx1 e2) = GOp (IAdd (fromExpr epx1) (fromExpr e2))
fromExpr (Mul epx1 e2) = GOp (IMul (fromExpr epx1) (fromExpr e2))
fromExpr (Negate expr) = GOp (INegate (fromExpr expr))
fromExpr (Abs expr) = GOp (IAbs (fromExpr expr))
fromExpr (Signum expr) = GOp (ISignum (fromExpr expr))

