-- Adapted from https://cs.pomona.edu/~michael/courses/csci131s18/lec/Lec09.html

import Data.Char
import Control.Applicative

import qualified Data.Map as Map
import Data.Map (Map)

import System.Exit

type VarName = String


------------------ Data types for types: --------------NEW

data Type =
     BoolType
   | IntType
   | LambdaType Type Type
   | FailureType 
    deriving (Show, Eq)

type TypeStore = Map.Map VarName Type

-----------------------------------------------------------

type Store = Map.Map VarName Int

data AExp =
   Var String
 | Num Int
 | Plus AExp AExp
 | Times AExp AExp 
 | Neg AExp
 | Div AExp AExp
 | Lambda String Type AExp                    --NEW
 | App AExp AExp                              --NEW
 | Let String AExp AExp                       --NEW
 | IfExp (BExp AExp) AExp AExp                --NEW
 | Rec String Type String Type AExp AExp      --NEW
  deriving (Show, Eq)

data BExp a =
    Bool Bool -- Boolean atom
  | Equal a a
  | Lt a a
  | Not (BExp a)
  | Or (BExp a) (BExp a)
  | And (BExp a) (BExp a)
    deriving (Show, Eq)

data Stmt a b =
    Skip
  | Assign VarName a
  | Seq (Stmt a b) (Stmt a b)
  | If (b a) (Stmt a b) (Stmt a b)
  | While (b a) (Stmt a b)
  deriving (Show, Eq, Ord)

newtype Parser a = Parser { parse :: String -> Maybe (a,String) }

instance Functor Parser where
  fmap f p = Parser $ \s ->
    case parse p s of
      Nothing -> Nothing
      Just (v,s') -> Just (f v,s')

instance Applicative Parser where
  pure a = Parser $ \s -> Just (a,s)
  f <*> a = Parser $ \s ->  -- f :: Parser (a -> b), a :: Parser a
    case parse f s of
      Nothing -> Nothing
      Just (g,s') -> parse (fmap g a) s' -- g :: a -> b, fmap g a :: Parser b

instance Alternative Parser where
  empty = Parser $ \s -> Nothing
  p1 <|> p2 = Parser $ \s ->
    case parse p1 s of
      Just (a,s') -> Just (a,s')
      Nothing -> parse p2 s

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where f [] = Nothing
        f (x:xs) = if p x then Just (x,xs) else Nothing

spaces :: Parser ()
spaces = many (satisfy isSpace) *> pure ()

int :: Parser Int
int = read <$> some (satisfy isDigit)

num :: Parser Int
num = spaces *> int

aterm, afactor, aatom, avar, aapp, alambda, arec :: Parser AExp
aterm   =    Plus <$> afactor <* tplus <*> aterm
         <|> afactor
afactor =    Times <$> aatom <* ttimes <*> afactor 
         <|> alambda                                       --NEW
alambda = Lambda <$> (tlambda *> str)                      --NEW
                     <*> (tcolon *> typeparse)             --NEW
                     <*> (tlbrace *> aterm <* trbrace)     --NEW
         <|> alet                                          --NEW
alet = Let <$> (tlet *> str)                               --NEW
               <*> (teq *> aexp)                           --NEW
               <*> (tin *> aexp <* tend)                   --NEW
       <|> aif                                             --NEW
aif = IfExp <$> (tif *> bexp)
                <*> (tthen *> aexp)
                <*> (telse *> aexp <* tend)                --NEW
       <|> arec
arec = Rec <$> (trec *> str)
                 <*> (teq *> typeparse)
                 <*> (tlambda *> str)              --NEW
                 <*> (tcolon *> typeparse)         --NEW
                 <*> (tlbrace *> aterm <* trbrace) --NEW
                 <*> (tin *> aterm <* tend)        --NEW
         <|> aatom
aatom   =    Num <$> num 
         <|> (tlpar *> aterm <* trpar)
         <|> avar
avar   = Var <$> str
         <|> aapp                                          --NEW
aapp   = App <$> (tlbrak *> aterm) <*>  (aterm <* trbrak)  --NEW

aexp = aterm

string :: Parser String                   
string = some (satisfy isAlpha)           

str :: Parser String                  
str = spaces *> string

splitKey :: String -> String -> (Bool, String, String)
splitKey target actual = let (value, rest) = splitAt (length target) actual
                         in if value == target then (True, target, rest)
                            else (False, [], [])
boolean :: Parser Bool
boolean = Parser $ \s ->
  let (success, value, rest) = splitKey "true" s
  in if success then Just (True, rest) 
     else let (success', value', rest') = splitKey "false" s
          in if success' then Just (False, rest')
          else Nothing

bool :: Parser Bool
bool = spaces *> boolean

--------------------NEW----------------
typeparse :: Parser Type
typeparse =  pure IntType <* tint
         <|> pure BoolType <* tbool
         <|> LambdaType <$> (tlpar *> typeparse) <*> (tyields *> typeparse <* trpar)
---------------------------------------

kw' :: String -> Parser String
kw' target =  Parser $ \actual ->
  let (success, value', rest') = splitKey target actual
  in if success then Just (value',rest') else Nothing

kw :: String -> Parser String
kw s = spaces *> (kw' s)

trec    = kw "REC"                        --NEW
tint    = kw "int"                        --NEW
tbool   = kw "bool"                       --NEW
tcolon  = kw ":"                          --NEW
tlambda = kw "\\"                         --NEW
tyields = kw "->"                         --NEW
tdot    = kw "."                          --NEW
tlbrak  = kw "["                          --NEW
trbrak  = kw "]"                          --NEW
tlbrace = kw "{"                          --NEW
trbrace = kw "}"                          --NEW
tlet    = kw "LET"                        --NEW
tin     = kw "IN"                         --NEW
tq      = kw "?"                          --NEW
tassign = kw ":="
tand    = kw "AND"
tdo     = kw "DO"
telse   = kw "ELSE"
tend    = kw "END"
teq     = kw "="
tif     = kw "IF"
tlpar   = kw "("
tneq    = kw "!="
tor     = kw "OR"
tplus   = kw "+"
trpar   = kw ")"
tsemi   = kw ";"
tskip   = kw "SKIP"
tthen   = kw "THEN"
ttimes  = kw "*"
tlt     = kw "<"
twhile  = kw "WHILE"

bterm, bfactor, batom, beq :: Parser (BExp AExp)
bterm   =    Or <$> bfactor <* tor <*> bterm
         <|> bfactor
bfactor =    And <$> batom <* tand <*> bfactor 
         <|> batom
batom =      Bool <$> bool
         <|> beq
beq =        Equal <$> aterm <* teq   <*> aterm
         <|> Not <$> (Equal <$> aterm <* tneq  <*> aterm)
         <|> Lt  <$> aterm <* tlt   <*> aterm

bexp = bterm

---- Statements

stmt' :: Parser (Stmt AExp BExp)
stmt' = Assign <$> str <* tassign <*> aterm

         <|> If     <$> (tif *> bexp)  <*> (tthen *> stmt) <*> (telse *> stmt) <* tend

         <|> While  <$> (twhile *> bexp) <*> (tdo *> stmt) <* tend

         <|> pure Skip <* tskip

stmt :: Parser (Stmt AExp BExp)
stmt  = Seq <$> stmt' <* tsemi <*> stmt <|> stmt'

----------- PARSE TESTING UTILITIES ---------------------NEW

testAExp :: String -> Maybe (AExp, String) -> IO()
testAExp str expected =
  let result = parse aexp str in
  do
    putStrLn ("Testing: " ++ str)
    print result
    print expected
    if expected == result
    then putStrLn("                    PASSED")
    else do
       putStrLn("                    FAILED")
       exitFailure
testBExp :: String -> Maybe ((BExp AExp),String) -> IO()
testBExp str expected =
  let result = parse bexp str in
  do
    putStrLn ("Testing: " ++ str)
    print result
    print expected
    if expected == result
    then putStrLn("                    PASSED")
    else do
       putStrLn("                    FAILED")
       exitFailure

tests :: String -> Maybe ((Stmt AExp BExp),String) -> IO()
tests str expected =
  let result = parse stmt str in
  do
    putStrLn ("Testing: " ++ str)
    print result
    print expected
    if expected == result
    then putStrLn("                    PASSED")
    else  do
       putStrLn("                    FAILED")
       exitFailure

-----------------------------Typechecking ----------------------NEW

-------- FIND TYPE OF ABSTRACT PROGRAMS (already parsed) --------NEW

findTypeAExp :: AExp -> TypeStore -> (Type, TypeStore)
findTypeAExp a store =
  case a of
    Var x -> (Map.findWithDefault FailureType x store, store)
    Num x -> (IntType, store)
    Plus x y ->
      let (t, store') = (findTypeAExp x store) in
        if t /= IntType 
        then (FailureType, store')
        else let (t', store'') = (findTypeAExp y store') in
             if t' /= IntType
             then (FailureType, store'')
             else (IntType, store'')
    Lambda s t a ->
      let (t', store') = findTypeAExp a (Map.insert s t store) 
      in (LambdaType t t', store)
         -- Why not store' ?
    App f x ->
       let (t,store') = (findTypeAExp f store) 
       in case t of
           LambdaType t1 t2 -> 
             let (t',store'') = (findTypeAExp x store) 
             in if t1 == t'
                then (t2, store)
                else (FailureType, store)
                -- store store'  store''  ????
           _ -> (FailureType, store)
    _ -> (FailureType, store)

findTypeBExp :: (BExp AExp) -> TypeStore -> (Type, TypeStore)
findTypeBExp b store =
  case b of
    Bool x -> (BoolType, store)
    Or x y -> let (t, store') = (findTypeBExp x store) in
              if t /= BoolType
              then (FailureType, store')
              else let (t', store'') = (findTypeBExp y store') in
                if t' /= BoolType
                then (FailureType, store'')
                else (BoolType, store'')
    Lt x y -> let (t, store') = (findTypeAExp x store) in
              if t /= IntType
              then (FailureType, store')
              else let (t', store'') = (findTypeAExp y store') in
                if t' /= IntType
                then (FailureType, store'')
                else (BoolType, store'')
    _ -> (FailureType, store)

-------- FIND TYPE OF STRING BY PARSING FIRST -----------------NEW
-------- if parse complete, then find type from abstract program
-------- Mainly for testing the above

findTypeStringAExp :: String -> TypeStore -> (Type, TypeStore)
findTypeStringAExp str store =
  case (parse aexp str) of
  Nothing -> (FailureType, store)
  Just(a,s) -> if s /= ""
               then (FailureType, store)
               else findTypeAExp a store

findTypeStringBExp :: String -> TypeStore -> (Type, TypeStore)
findTypeStringBExp str store =
  case (parse bexp str) of
  Nothing -> (FailureType, store)
  Just(b,s) -> if s /= ""
               then (FailureType, store)
               else findTypeBExp b store

---------------- CHECK TYPE OF STMT ------------------NEW

-- Front end:
typeCheck :: String -> Bool
typeCheck str = fst (typeCheckStringStmt str Map.empty)

-- For Debugging:
getStore :: String -> TypeStore
getStore str = snd (typeCheckStringStmt str Map.empty)

typeCheckStringStmt :: String -> TypeStore-> (Bool, TypeStore)
typeCheckStringStmt str store =
  case (parse stmt str) of
  Nothing -> (False, store)
  Just(b,s) -> if s /= ""
               then (False, store)
               else typeCheckStmt b store

typeCheckStmt :: (Stmt AExp BExp) -> TypeStore -> (Bool, TypeStore)
typeCheckStmt program store =
  case program of
    Skip -> (True, store)
    Assign var val ->
      let (valType, store') = findTypeAExp val store
      in if valType /= FailureType
         then (True, Map.insert var valType store')
         else (False, store)
    Seq s1 s2 ->
      let (s1Good, store') = typeCheckStmt s1 store
      in if not s1Good
         then (False, store')
         else let (s2Good, store'')  = typeCheckStmt s2 store'
         in (s2Good, store'')
    If b s1 s2 ->
      let (bType, store') = findTypeBExp b store in
      if bType /= BoolType
      then (False, store')
      else let (s1Good, store'') = typeCheckStmt s1 store' in
           if not s1Good
           then (False, store'')
           else let (s2Good, store''') = typeCheckStmt s1 store''
           in (s2Good, store''')
           -- Do you see a problem here?
    While b s ->
      let (bType, store') = findTypeBExp b store in 
      if bType /= BoolType
      then (False, store')
      else let (sGood, store'') = typeCheckStmt s store' in
          (sGood, store'')   

--------  TYPECHECK TESTING UTILITIES -------------------NEW

testTypeAExp :: String -> Type -> TypeStore -> IO()
testTypeAExp str expected  store =
  let (result,store') = findTypeStringAExp str store in
  do
    putStrLn("Finding type of: " ++ str)
    putStr("Result:    ")
    print result
    putStr("Expected:  ")
    print expected
    if expected == result
    then putStrLn("                    PASSED")
    else do
           putStrLn("                    FAILED")
           exitFailure

testTypeBExp :: String -> Type -> TypeStore -> IO()
testTypeBExp str expected store =
  let (result,store') = findTypeStringBExp str store in
  do
    putStrLn ("Finding type of: " ++ str)
    putStr("Result:    ")
    print result
    putStr("Expected:  ")
    print expected
    if expected == result
    then putStrLn("                    PASSED")
    else  do
       putStrLn("                    FAILED")
       exitFailure

testTypeCheck :: String -> Bool -> IO()
testTypeCheck str expected =
  let p = parse stmt str in
    let result = typeCheck str in
      do
        putStrLn ("Typechecking:")
        print str
        putStrLn ("Parse:")
        print p
        putStr("Result:   ")
        print result
        putStr("Expected: ")
        print expected
        if expected == result
        then putStrLn("                    PASSED")
        else do
          putStrLn("                    FAILED")
          exitFailure



---------------------------  Testing -------------------------NEW
main :: IO()
main = do 

  putStrLn("\n-------------------Test Parsing------------------\n")

  testAExp "3 + 4" (Just(Plus (Num 3) (Num 4),""))
  testAExp "3 + x" (Just (Plus (Num 3) (Var "x"),""))
  testBExp "true" (Just (Bool True,""))
  testBExp "false" (Just (Bool False,""))
  testBExp "false OR true AND true"
        (Just (Or (Bool False) (And (Bool True) (Bool True)),""))
  testBExp "x = 5 AND y < 7"
        (Just (And (Equal (Var "x") (Num 5)) (Lt (Var "y") (Num 7)),""))
  testBExp "3 != 5"
        (Just (Not (Equal (Num 3) (Num 5)),""))
  tests "x := 5"
        (Just (Assign "x" (Num 5),""))
  tests "WHILE true DO x := 3 END"
        (Just (While (Bool True) (Assign "x" (Num 3)),""))
  tests "IF x != 0 THEN y := 10 ELSE y := x END"
        (Just (If (Not (Equal (Var "x") (Num 0)))
                  (Assign "y" (Num 10))
                  (Assign "y" (Var "x")),""))
  tests "SKIP" (Just (Skip,""))
  tests "SKIP; SKIP" (Just (Seq Skip Skip,""))


  testAExp "[a b]" (Just (App (Var "a") (Var "b"),""))
  testAExp "[12 23]" (Just (App (Num 12) (Num 23),""))
  testAExp "\\x : int { 99 }" (Just (Lambda "x" IntType (Num 99),""))
  testAExp "\\x : (int -> int) { x + y }"
        (Just (Lambda "x" (LambdaType IntType IntType) (Plus (Var "x") (Var "y")),""))
  testAExp "\\x : (int -> (int -> int)) { x + y }"
        (Just (Lambda "x"
         (LambdaType IntType 
           (LambdaType IntType IntType)) (Plus (Var "x") (Var "y")),""))
  testAExp "\\x : ((int -> int) -> int) { x + y }"
        (Just (Lambda "x" 
               (LambdaType (LambdaType IntType IntType) IntType)
                      (Plus (Var "x") (Var "y")),""))
  testAExp "\\x : ((int -> int) -> (int -> int)) { x + y }"
        (Just (Lambda "x"
           (LambdaType (LambdaType IntType IntType) 
               (LambdaType IntType IntType)) (Plus (Var "x") (Var "y")),""))

  testAExp "[\\x : int  {x + x} 3+4]" 
        (Just (App (Lambda "x" IntType (Plus (Var "x") (Var "x"))) (Plus (Num 3) (Num 4)),""))

  testAExp "[[a b] [c d]]" (Just (App (App (Var "a") (Var "b")) (App (Var "c") (Var "d")),""))
  testAExp "[[4 5] [6 7]]" (Just (App (App (Num 4) (Num 5)) (App (Num 6) (Num 7)),""))


  putStrLn("\n---------------Test Type Checking---------------\n")

  testTypeAExp "99" IntType Map.empty
  testTypeAExp "99 + 123" IntType Map.empty
  testTypeAExp "4 + 5 + 3" IntType Map.empty
  testTypeAExp "x" IntType (Map.fromList [("x",IntType)])
  testTypeAExp "x + 99" IntType (Map.fromList [("x",IntType)])

  testTypeBExp "true" BoolType Map.empty
  testTypeBExp "true OR false OR true" BoolType Map.empty
  testTypeBExp "3 < 4" BoolType Map.empty
  testTypeBExp "3 < 4 OR 5 < 2" BoolType Map.empty
  
  testTypeCheck "SKIP" True
  testTypeCheck "SKIP;SKIP;SKIP" True
  testTypeCheck "x := 3;y := x + 3" True

  testTypeAExp "\\x : int { 3 }" (LambdaType IntType IntType) Map.empty
  testTypeAExp "\\x : (int -> int) { 4 }" 
    (LambdaType (LambdaType IntType IntType) IntType) Map.empty
  testTypeAExp "\\x : (int -> int) { \\y : int { 8 }}"
     (LambdaType (LambdaType IntType IntType) (LambdaType IntType IntType) ) Map.empty

  testTypeAExp "[\\x : int { 7 }  8]"  IntType Map.empty
  testTypeAExp "[\\x : (int -> int) { 7 }  8]"  FailureType Map.empty
  testTypeAExp "[\\x : (int -> int) { 7 } \\x : int { x + 1 }]"
            IntType Map.empty
  testTypeAExp "[\\x : int { \\y : int { x+y } } 5 ]"
            (LambdaType IntType IntType) Map.empty

  testTypeCheck "x := 3; WHILE x < 2 DO z := x; w := x+z END" True

  testTypeCheck "x := 3; z := [\\y : int { x + y } 5]; w := x + z" True
  let p = "x := 3;" ++
          "foo := \\y : int { x + y };" ++
          "z := [foo 99];" ++
          "xx := [foo [foo 12] + x]"
  testTypeCheck p True 
  let q = "id := \\x:int{x};" ++
          "f := \\x:int{\\y:int{x+y}};" ++
          "w := [[f 3] 4]"
  testTypeCheck q True
  print (getStore q)
  
  putStrLn("--------------------Some FAILURES---------------------")
  putStrLn("----------------No boolean variables (does not parse):---------")
  testTypeCheck "x := 3;WHILE x DO y := 3 END" False
  putStrLn("----- z has not been assigned (parses, no typecheck):----------")
  let p' = "x := 3;y := x + z"
  testTypeCheck p' False
  print (getStore p')
  putStrLn("---------- y out of scope ---------")
  let p'' = "x := 3; z := [\\y : int { x + y } 5]; w := x + y"
  testTypeCheck p'' False
  print (getStore p'')
  putStrLn("---------- wrong type for addition ------")
  let p''' = "x := 3; y := \\x : int {4} ; z := x + y" 
  testTypeCheck p''' False
  print (getStore p''')
  putStrLn("---------wrong function type for application ----")
  testTypeCheck "x := [5 6]" False
  testTypeCheck "x := 9; y := [x 4]" False
  putStrLn("---------wrong argument type for application---")
  testTypeCheck "f := \\x:(int->int){22}; y := [f 4]" False
  putStrLn("---------wrong result type for application---")
  testTypeCheck "f:= \\x:int{22}; y := [[f 3] 8]" False
  testTypeCheck (
    "id := \\x:(int->int){x};" ++
    "f := \\x:(int->(int->int)){\\y:(int->int){x+y}};" ++
    "w := [id [f 3]]"
    )
    False

  putStrLn("----------------LET for homework-------------")
  testAExp "LET x = 2 IN x END" (Just (Let "x" (Num 2) (Var "x"),""))
  testAExp "3 + LET x = 2*x IN (x+x) END + 99"
       (Just (Plus (Num 3)
             (Plus (Let "x"
                        (Times (Num 2) (Var "x"))
                        (Plus (Var "x") (Var "x")))
                   (Num 99)),""))
  testAExp "[\\x:int{x} LET x=a IN x END + 99]"
        (Just (App
          (Lambda "x" IntType (Var "x")) 
          (Plus (Let "x" (Var "a") (Var "x")) (Num 99)),""))

  putStrLn("--------------IfExp for homework ----------")
  testAExp "IF 3 < 4 THEN 8 ELSE 9 END"
        (Just (IfExp (Lt (Num 3) (Num 4)) (Num 8) (Num 9),""))
  testAExp "3 + IF x < y THEN 5 ELSE 6 END + 9"
        (Just (Plus (Num 3) 
              (Plus (IfExp (Lt (Var "x") (Var "y")) (Num 5) (Num 6)) 
                    (Num 9)),""))
  testAExp "[\\x:int{x} IF true THEN 5 ELSE a END + 9]"
        (Just 
          (App 
            (Lambda "x" IntType (Var "x")) 
            (Plus (IfExp (Bool True) (Num 5) (Var "a")) (Num 9)),""))

  putStrLn("--------------RecExp for homework ----------")
  testAExp "REC f = int \\x:int {[f x+1]} IN [f 3] END"
     (Just (Rec "f" IntType "x" IntType 
              (App (Var "f") (Plus (Var "x") (Num 1))) 
              (App (Var "f") (Num 3)),""))
  testAExp ("REC f = int \\x:int {IF x < 1 THEN 1 ELSE x * [f x+1] END} " ++
            "IN [f 5] END")
    (Just (Rec "f" IntType "x" IntType
        (IfExp (Lt (Var "x") (Num 1)) 
               (Num 1)
               (Times (Var "x") (App (Var "f") (Plus (Var "x") (Num 1)))))
        (App (Var "f") (Num 5)),""))
