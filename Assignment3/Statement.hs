module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr

-- Julia Bäcklund 
-- Erik Dahlberg

type T = Statement
data Statement =
    Assignment String Expr.T | Skip | While Expr.T Statement | Begin [Statement] | Read String | Write Expr.T |
    If Expr.T Statement Statement | Comment String
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skipState = accept "skip" # require ";" >-> buildSkip
buildSkip _ = Skip

beginState = accept "begin" -# iter parse #- require "end" >-> buildBegin
buildBegin = Begin

ifState = accept "if" -# Expr.parse # require "then" -# parse # require "else" -# parse >-> buildIf
buildIf ((exp, stat1), stat2) = If exp stat1 stat2

whileState = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (exp, stat) = While exp stat

readState = accept "read" -# word #- require ";" >-> buildRead
buildRead = Read

writeState = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite = Write

-- commentState = accept "--" -# Parser.comments #- require "\n" >-> buildComment
commentState = accept "--" -# Parser.comments #- require "\n" >-> buildComment
buildComment = Comment

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (If cond thenStmts elseStmts: stmts) dict input =
    if Expr.value cond dict > 0
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec (While exp state : states) dict input =
    if Expr.value exp dict > 0
    then exec (state : While exp state : states) dict input
    else exec states dict input
exec (Skip : states) dict input = exec states dict input
exec (Begin s: states) dict input = exec (s ++ states) dict input
exec ((Read exp) : states) _ [] = error "Input to read is empty"
exec ((Read exp): states) dict (i:input) = exec states (Dictionary.insert (exp, i) dict) input
exec ((Write exp): states) dict input = Expr.value exp dict : exec states dict input
exec (Assignment str exp : states) dict input = exec states (Dictionary.insert (str, Expr.value exp dict) dict) input
exec (Comment str: states) dict input = exec states dict input

indent n = take (n*4) (repeat ' ') -- use \t for tabs

toStringHelper :: T -> Int -> String
toStringHelper (Read k) ind = indent ind ++ "read " ++ k ++ ";\n"
toStringHelper (Write k) ind = indent ind ++ "write " ++ Expr.toString k ++ ";\n"
toStringHelper (Begin states) ind = indent ind ++ "begin\n" ++ concatMap (`toStringHelper` (ind+1)) states ++ "\n" ++ indent ind ++ "end\n"
toStringHelper (Assignment str exp) ind = indent ind ++ str ++ " := " ++ Expr.toString exp ++ ";\n"
toStringHelper Skip ind = indent ind ++ "skip;\n"
toStringHelper (If cond thenStmts elseStmts) ind = indent ind ++ "if " ++ Expr.toString cond ++ " then\n" ++ toStringHelper thenStmts (ind+1) ++ "\n" ++ indent ind ++ "else\n" ++ toStringHelper elseStmts (ind+1) ++ "\n"
toStringHelper (While exp states) ind = indent ind ++ "while " ++ Expr.toString exp ++ " do\n" ++ toStringHelper states (ind+1)
toStringHelper (Comment com) ind = indent ind ++ com ++ "\n"
--toStringHelper (Comment com) ind = ""

filtrera :: String -> String  -- v := -- nasjdksahdjadjhdkd \n -- wadjshahdjahdkahdkhdh\n 1;
filtrera [] = ""
filtrera [s] = [s]
filtrera (s1:s2:rest)
        |  s1 == '-' && s2 == '-' = filtrera (waitForNewLine rest)
        | otherwise = s1 : filtrera (s2:rest)

waitForNewLine :: String -> String
waitForNewLine [] = ""
waitForNewLine (s1:rest)
    | s1 == '\n' = rest
    | otherwise = waitForNewLine rest

instance Parse Statement where
  parse = (assignment ! skipState ! beginState ! ifState ! whileState ! readState ! writeState ! commentState) . filtrera
  --parse = assignment ! skipState ! beginState ! ifState ! whileState ! readState ! writeState ! commentState
  toString stat = toStringHelper stat 0
