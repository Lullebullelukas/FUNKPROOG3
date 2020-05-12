module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T | Skip | Begin [Statement] | Read String | Write Expr.T | While Expr.T Statement 
    | Comment | If Expr.T Statement Statement
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skip = accept "skip" #- require ";" >-> buildSkip
buildSkip _ = Skip

begin = accept "begin" -# iter parse #- require "end" >-> buildBegin
buildBegin v = Begin v  

if' = accept "if" -# Expr.parse #- require "then" # parse #- accept "else" # parse >-> buildIf
buildIf ((v1,v2) ,v3) = If v1 v2 v3 


while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (v, e) = While v e 

read = accept "read" -# word #- require ";" >-> buildRead 
buildRead v = Read v  

write = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite v  = Write v

comment = accept "--" -# iter word #- require "/n" >-> \_ -> Comment

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec ((Assignment string expr): stmts) dict input = exec stmts (Dictionary.insert (string, Expr.value expr dict) dict) input
exec (Skip:stmts) dict input = exec stmts dict input
exec ((Begin strings): stmts) dict input = exec (strings++stmts) dict input 

exec ((If cond thenStmts elseStmts): stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

exec ((While cond stmt): stmts) dict input = 
    if (Expr.value cond dict)>0
    then exec ((While cond stmt):stmts) dict input
    else exec stmts dict input

exec ((Read string):stmts) dict (i:input) = exec stmts (Dictionary.insert (string,i) dict) input
exec ((Write expr):stmts) dict input = (Expr.value expr dict):(exec stmts dict input)
exec (Comment:stmts) dict input = exec stmts dict input

instance Parse Statement where
  parse = assignment ! skip ! begin ! if' ! while ! Statement.read ! write 
  -- toString = ""

indent i = concat $ replicate (2*i) " "
shw :: Integer -> Statement  -> String
shw int (Assignment name expr) = (indent int) ++ name ++ ":=" ++ (Expr.toString expr) ++ "\n"
shw int Skip = indent int ++ "skip; \n"
shw int (Begin stmts) = indent int ++ "begin \n" ++ concatMap (shw (int+1) stmts)







