module Expr(Expr, T, parse, fromString, value, toString) where

{-
   An expression of type Expr is a representation of an arithmetic expression 
   with integer constants and variables. A variable is a string of upper- 
   and lower case letters. The following functions are exported
   
   parse :: Parser Expr
   fromString :: String -> Expr
   toString :: Expr -> String
   value :: Expr -> Dictionary.T String Int -> Int
   
   parse is a parser for expressions as defined by the module Parser.
   It is suitable for use in parsers for languages containing expressions
   as a sublanguage.
   
   fromString expects its argument to contain an expression and returns the 
   corresponding Expr. 
  
   toString converts an expression to a string without unneccessary 
   parentheses and such that fromString (toString e) = e.
  
   value e env evaluates e in an environment env that is represented by a
   Dictionary.T Int.  
-}
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary

-- Julia Bäcklund 
-- Erik Dahlberg

data Expr = Num Integer | Var String | Add Expr Expr
       | Sub Expr Expr | Mul Expr Expr | Div Expr Expr | Exp Expr Expr
         deriving Show

type T = Expr

var, num, factor, term, expr :: Parser Expr

term', expr' :: Expr -> Parser Expr

var = word >-> Var

num = number >-> Num

mulOp = lit '*' >-> (\_ -> Mul) !
        lit '/' >-> (\_ -> Div)

addOp = lit '+' >-> (\_ -> Add) !
        lit '-' >-> (\_ -> Sub)

expOp = lit '^' >-> (\_ -> Exp)

bldOp e (oper,e') = oper e e'

factor = num !
         var !
         lit '(' -# expr #- lit ')' !
         err "illegal factor"

expon' :: Expr -> Parser Expr
expon' e = expOp # expon >-> bldOp e #> expon' ! return e

expon :: Parser Expr
expon = factor #> expon'

term' e = mulOp # expon >-> bldOp e #> term' ! return e

term = expon #> term'

expr' e = addOp # term >-> bldOp e #> expr' ! return e

expr = term #> expr'

parens cond str = if cond then "(" ++ str ++ ")" else str

shw :: Int -> Expr -> String
shw prec (Num n) = show n
shw prec (Var v) = v
shw prec (Add t u) = parens (prec>5) (shw 5 t ++ "+" ++ shw 5 u)
shw prec (Sub t u) = parens (prec>5) (shw 5 t ++ "-" ++ shw 6 u)
shw prec (Mul t u) = parens (prec>6) (shw 6 t ++ "*" ++ shw 6 u)
shw prec (Div t u) = parens (prec>6) (shw 6 t ++ "/" ++ shw 7 u)
shw prec (Exp t u) = parens (prec>7) (shw 7 t ++ "^" ++ shw 8 u)

value :: Expr -> Dictionary.T String Integer -> Integer
value (Num n) _ = n
value (Var v) dict = case Dictionary.lookup v dict of
        Nothing -> error ("Variable not in dictionary")
        Just b -> b
value (Add t1 t2) dict = value t1 dict + value t2 dict
value (Sub t1 t2) dict = value t1 dict - value t2 dict
value (Mul f1 f2) dict = value f1 dict * value f2 dict
value (Div f1 f2) dict = if value f2 dict == 0 then error "Division by zero" else div (value f1 dict) (value f2 dict)
value (Exp f1 f2) dict = value f1 dict ^ value f2 dict
-- value (Exp t1 t2) dict = value t1 dict ^ value t2 dict

instance Parse Expr where
    parse = expr
    toString = shw 0
