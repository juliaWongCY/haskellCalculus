module Calculus where

import Data.Maybe

data UnOp = Neg | Sin | Cos | Log
          deriving (Eq, Ord, Show)

data BinOp = Add | Mul | Div
           deriving (Eq, Ord, Show)

data Exp = Val Double | Id String | UnApp UnOp Exp | BinApp BinOp Exp Exp
         deriving (Eq, Ord, Show)

type Env = [(String, Double)]


-- look up the associated variable
-- Useful for using Id later
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp chr ((x, y) : xs)
  | chr == x  = y
  | otherwise = lookUp chr xs



eval :: Exp -> Env -> Double
eval (Val ex) list            = ex
eval (Id ex) list             = lookUp ex list
eval (UnApp Neg ex) list      = -(eval ex list)
eval (UnApp Sin ex) list      = sin (eval ex list) 
eval (UnApp Cos ex) list      = cos (eval ex list)
eval (UnApp Log ex) list      = log (eval ex list)
eval (BinApp Add ex ex') list = (eval ex list) + (eval ex' list) 
eval (BinApp Mul ex ex') list = (eval ex list) * (eval ex' list)
eval (BinApp Div ex ex') list = (eval ex list) / (eval ex' list)



-- the list is the variable you want to differentiate with respect with
diff :: Exp -> String -> Exp
diff (Val _) _ 
  = 0
diff (Id x) var
  | var == x  = 1
  | otherwise = 0
diff (BinApp Add ex ex') var 
  = (diff ex var) + (diff ex' var)
diff (BinApp Mul ex ex') var 
  = (ex * (diff ex' var)) + ((diff ex var) * ex')
diff (BinApp Div ex ex') var 
  = (((diff ex var) * ex') - ((diff ex' var) * ex)) / (ex' ^ 2)
diff (UnApp Neg ex) var 
  = UnApp Neg (diff ex var)
diff (UnApp Sin ex) var 
  = (cos ex) * diff ex var 
diff (UnApp Cos ex) var 
  = UnApp Neg ((sin ex) * diff ex var)
diff (UnApp Log ex) var 
  = (diff ex var) / ex



-- f = factorial 
-- d = differential 
-- d'= sub. x=0 into the differential
-- p = the power of the variable
maclaurin :: Exp -> Double -> Int -> Double
maclaurin func pt term 
  = sum (zipWith3 (\d' p f -> d' * p / f) d' p f)
  where
    d = iterate (flip diff "x") func
    d'= map (flip eval [("x", 0)]) (take term d)
    f = take term (scanl (*) 1 [1..])
    p = take term ([pt^y | y <- [0..]])


-- Generate a neat representation of the expressions
showExp :: Exp -> String
showExpp ex                
  = ex
showExp (Val ex)           
  = show ex
showExp (Id ex)            
  = ex
showExp (BinApp Add ex ex') 
  = "(" ++ (showExp ex) ++ "+" ++ (showExp ex') ++ ")"
showExp (BinApp Mul ex ex') 
  = "(" ++ (showExp ex) ++ "*" ++ (showExp ex') ++ ")"
showExp (BinApp Div ex ex') 
  = "(" ++ (showExp ex) ++ "/" ++ (showExp ex') ++ ")"
showExp (UnApp Neg ex)    
  = "-" ++ "(" ++ (showExp ex) ++ ")"
showExp (UnApp Log ex)    
  = "log" ++ "(" ++ showExp ex ++ ")"
showExp (UnApp Sin ex)    
  = "sin" ++ "(" ++ showExp ex ++ ")"
showExp (UnApp Cos ex)    
  = "cos" ++ "(" ++ showExp ex ++ ")"


 

---------------------------------------------------------------------------
-- Test cases from the spec.

e1, e2, e3, e4, e5, e6 :: Exp

-- > 5*x
e1 = BinApp Mul (Val 5.0) (Id "x")

-- > x*x + y - 7
e2 = BinApp Add (BinApp Add (BinApp Mul (Id "x") (Id "x")) (Id "y"))
                (UnApp Neg (Val 7.0))

-- > x-y^2/(4*x*y-y^2)::Exp
e3 = BinApp Add (Id "x")
            (UnApp Neg (BinApp Div (BinApp Mul (Id "y") (Id "y"))
            (BinApp Add (BinApp Mul (BinApp Mul (Val 4.0) (Id "x")) (Id "y"))
                        (UnApp Neg (BinApp Mul (Id "y") (Id "y"))))))

-- > -cos x::Exp
e4 = UnApp Neg (UnApp Cos (Id "x"))

-- > sin (1+log(2*x))::Exp
e5 = UnApp Sin (BinApp Add (Val 1.0)
                           (UnApp Log (BinApp Mul (Val 2.0) (Id "x"))))

-- > log(3*x^2+2)::Exp
e6 = UnApp Log (BinApp Add (BinApp Mul (Val 3.0) (BinApp Mul (Id "x") (Id "x")))
                           (Val 2.0))

----------------------------------------------------------------------
-- EXTENSION: Uncomment and complete these...

instance Num Exp where
  (+) = BinApp Add
  (*) = BinApp Mul
  negate = UnApp Neg
  fromInteger = Val . fromInteger
  abs x = error "abs not implement"
  signum x = error "signum not implement"
  
 
  
instance Fractional Exp where
  (/) = BinApp Div

instance Floating Exp where
  sin = UnApp Sin 
  cos = UnApp Cos
  log = UnApp Log
  
-- instance (Eq a, Num a) => Num (Maybe a) where

-- instance (Eq a, Fractional a) => Fractional (Maybe a) where

-- diff2 :: Exp -> String -> Maybe Exp



-- The following makes it much easier to input expressions, e.g. sin x, log(x*x) etc.

x, y :: Exp
x = Id "x"
y = Id "y"
