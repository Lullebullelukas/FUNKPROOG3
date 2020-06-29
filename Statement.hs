module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T | Skip | Begin [Statement] | Read String | Write Expr.T | While Expr.T Statement 
    | Comment String | If Expr.T Statement Statement
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skip = accept "skip" #- require ";" >-> buildSkip
buildSkip _ = Skip

begin = accept "begin" -# iter parse #- require "end" >-> buildBegin
buildBegin = Begin   

if' = accept "if" -# Expr.parse #- require "then" # parse #- accept "else" # parse >-> buildIf
buildIf ((v1,v2) ,v3) = If v1 v2 v3 


while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (v, e) = While v e 

read = accept "read" -# word #- require ";" >-> buildRead 
buildRead = Read   

write = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite = Write


-- comment = accept "--" -# iter (char ? (/= '\n')) #- require "\n" >-> Comment

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec ((Assignment string expr): stmts) dict input = exec stmts (Dictionary.insert (string, Expr.value expr dict) dict) input
exec (Skip:stmts) dict input = exec stmts dict input
exec ((Begin strings): stmts) dict input = exec (strings++stmts) dict input 

exec ((If cond thenStmts elseStmts): stmts) dict input = 
    if Expr.value cond dict > 0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

exec ((While cond stmt): stmts) dict input = 
    if Expr.value cond dict > 0
    then exec (stmt:(While cond stmt):stmts) dict input
    else exec stmts dict input

exec ((Read string):stmts) dict (i:input) = exec stmts (Dictionary.insert (string,i) dict) input
exec ((Write expr):stmts) dict input = (Expr.value expr dict):(exec stmts dict input)
exec ((Comment strings):stmts) dict input = exec stmts dict input

instance Parse Statement where
  parse = assignment ! skip ! begin ! if' ! while ! Statement.read ! write 
  toString = shw 0 

indent :: Int -> String
indent i = concat $ replicate (2*i) " "

shw :: Int -> Statement  -> String
shw ind (Assignment name expr) = indent ind ++ name ++ " := " ++ Expr.toString expr ++ "; \n"
shw ind Skip = indent ind ++ "skip;"
shw ind (Begin stmts) = indent ind ++ "begin \n" ++ concatMap (shw (ind+2)) stmts ++ indent(ind+1) ++ "end \n"  
shw ind (If cond thenStmts elseStmts) = indent ind ++ "if " ++ Expr.toString cond ++ " then \n" ++ indent (ind+1) ++ toString thenStmts ++ "\n" ++ indent ind ++ "else \n" ++ indent (ind+1) ++ toString elseStmts
shw ind (While cond stmts) = indent ind ++ "while " ++ Expr.toString cond ++ " do \n" ++ indent (ind+1) ++ toString stmts 

shw ind (Read string) = indent ind ++ "read " ++ string ++ ";" ++ "\n"
shw ind (Write expr) = indent ind ++ "write " ++ Expr.toString expr ++ "; \n" 
--shw ind (Comment string) = indent ind ++ "--" ++ string ++ "\n"
--commented out comments and deleted the call from parse



