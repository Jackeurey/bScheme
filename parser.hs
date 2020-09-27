module Parser 
(Val(..), Syntax(..), analyze,
 val,var,assignVar,assignVal,
 defineVar,defineVal,ifPred,ifCon,isAlt,
 appOp,appArgs,beginSeq, 
 numberVal, listVal, 
 lambdaParams, lambdaBody, isTrue) where 

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDef =
  emptyDef {Token.commentLine   = ";",
            Token.commentStart  = "{-",
            Token.commentEnd    = "-}",
            Token.identStart    = letter,
            Token.identLetter   = (oneOf "+-?><[]/\\" <|> alphaNum),
            Token.reservedNames = ["if", "quote", "define", "lambda", 
                                   "set!", "list", "#t", "#f", "begin",
                                   "let", "else"]
          }

lexer      = Token.makeTokenParser languageDef -- Uses the languageDef to create a lexer
identifier = Token.identifier lexer            -- parse identifier
reserved   = Token.reserved lexer              -- parse a reserved name
parens     = Token.parens lexer                -- deals with paranthesis
integer    = Token.integer lexer 
whiteSpace = Token.whiteSpace lexer
float      = Token.float lexer

--The value types the language is able to pass and return (not counting functions)
data Val = Number Double  
         | String String
         | Boolean Bool
         | Symbol String
         | List [Val]
         | Lambda [Syntax] Syntax
         | Null

instance Eq Val where
  (Number x) == (Number y)     = x == y 
  (String x) == (String y)     = x == y
  (Boolean x) == (Boolean y)   = x == y
  (Symbol x) == (Symbol y)     = x == y
  (List x) == (List y)         = x == y
  (Lambda _ _) == (Lambda _ _) = False
  Null == Null                 = True
  _ == _                       = False

instance Show Val where
  show (Number x)    = show x
  show (Boolean x)   = show x
  show (Symbol x)    = '\'' : x
  show (List (x:xs)) = "\'(" ++ (foldl (\a b -> a ++ " " ++ show b) (show x) xs) ++ ")"
  show (List [])     = "'()"
  show (String x)    = "\"" ++ x ++ "\""
  show Null          = "Null"
  show (Lambda x s)  = "(Lambda (" ++ show x ++ ") " ++ show s ++ ")"
  
-- Syntax represents the data structure that represents the AST
data Syntax = Val Val
            | Var String
            | Assign Syntax Syntax
            | Define Syntax Syntax
            | If Syntax Syntax Syntax
            | App Syntax [Syntax]
            | Begin [Syntax] 
            | Prim Syntax [Syntax]
            | Error String
            deriving Eq

instance Show Syntax where
  show (Val x)      = show x
  show (Var x)      = "(Var " ++ x ++ ")"
  show (Error x)    = "(Error " ++ x ++ ")" 
  show (Assign s x) = "(Assign " ++ show s ++ " " ++ show x ++ ")"
  show (Define s x) = "(Define " ++ show s ++ " " ++ show x ++ ")"
  show (If p c a)   = "(If " ++ show p ++ " " ++ show c ++ " " ++ show a ++ ")"
  show (App s x)    = "(App " ++ show s ++ " " ++ show x ++ ")"
  show (Begin seq)  = "(Begin " ++ show seq ++ ")"
  show (Prim s x)   = "(Prim " ++ show s ++ " " ++ show x ++ ")"

-- Parsers ----------------------------------------------------------------------------------------
analyze :: String -> Syntax
analyze source = either (Error . show) id $ parse parseProgram "" source

-- Syntax Parsers ---------------------------------------------------------------------------------
parseProgram :: Parser Syntax
parseProgram = sepEndBy parseSyntax whiteSpace >>= \pgm -> return (Begin pgm)

parseSyntax :: Parser Syntax
parseSyntax = try parseMacro  <|>
              try parseDefine <|>
              try parseAssign <|>
              try parseIf     <|>
              try parseBegin  <|>
              try parseVar    <|>
              try parsePrim   <|>
              try parseApp    <|>
              parseVal    

parseMacro :: Parser Syntax
parseMacro = try parseLet <|> try parseCond
  where parseLet = parens 
          $ do reserved "let"
               binds <- parens $ sepEndBy (parens $ (,) <$> parseVar <*> parseSyntax) whiteSpace
               body  <- parseSyntax
               let (params,args) = unzip binds 
               return $ App (Val (Lambda params body)) args
        parseCond = parens
          $ do reserved "cond"
               clauses <- sepEndBy (parens $ (,) <$> parsePred <*> parseSyntax) whiteSpace
               return . condToIf $ clauses
          where condToIf (x:[]) = If (fst x) (snd x) (Val Null)
                condToIf (x:xs) = If (fst x) (snd x) (condToIf xs)
                parseElse = (reserved "else") >>= \ _ -> return $ Val . Boolean $ True
                parsePred = parseElse <|> parseSyntax

parseBegin :: Parser Syntax
parseBegin = parens
  $ do reserved "begin"
       syx <- sepEndBy parseSyntax whiteSpace
       return $ Begin syx

parseDefine :: Parser Syntax
parseDefine = try defineProc <|> try defineVal
 where defineVal = parens 
        $ do reserved "define"
             var <- identifier
             val <- parseSyntax
             return $ Define (Var var) val
       defineProc = parens
        $ do reserved "define"
             (proc:params) <- parens $ sepEndBy parseVar whiteSpace
             body          <- sepEndBy parseSyntax whiteSpace
             return $ Define proc (Val (Lambda params (Begin body)))

parseAssign :: Parser Syntax
parseAssign = parens
  $ do reserved "set!" 
       var <- identifier
       val <- parseSyntax
       return $ Assign (Var var) val
            
parseIf :: Parser Syntax
parseIf = parens 
  $ do reserved "if"
       prd <- parseSyntax
       con <- parseSyntax
       alt <- optionMaybe parseSyntax
       return $ If prd con $ isAlt alt
  where isAlt (Nothing) = (Val Null)
        isAlt (Just x)  = x

parsePrim :: Parser Syntax
parsePrim = parens 
  $ do prim <- whiteSpace *> primVar <* whiteSpace
       args <- sepEndBy parseSyntax whiteSpace
       return $ Prim (Var prim) args
  where primVar = try (string "+")      <|>
                  try (string "-")      <|>
                  try (string "id")     <|>
                  try (string "null?")  <|> 
                  try (string "/")      <|> 
                  try (string "*")      <|>
                  try (string "cons")   <|>
                  try (string "car")    <|>
                  try (string "cdr")

parseApp :: Parser Syntax
parseApp = parens
  $ do oper <- parseSyntax
       args <- sepEndBy parseSyntax whiteSpace
       return $ App oper args

parseVar :: Parser Syntax
parseVar = identifier >>= \x -> return $ Var x

parseVal :: Parser Syntax
parseVal = parseVal' >>= \x -> return $ Val x

-- Value Parsers ----------------------------------------------------------------------------------
parseVal' :: Parser Val 
parseVal' = try parseNum <|> try parseBol <|> try parseStr <|> try parseSym <|> try parseLambda

parseNum :: Parser Val 
parseNum = try negFloat <|> try posFloat <|> intParse
  where intParse = integer >>= \x -> return . Number . fromIntegral $ x
        negFloat = (char '-' >> float) >>= \x -> return . Number . negate $ x
        posFloat = float >>= \x -> return . Number $ x
 
parseSym :: Parser Val        
parseSym = try sym <|> try parseLst <|> try val 
  where sym = (char '\'') >> identifier >>= \x -> return . Symbol $ x
        val = (char '\'') >> parseVal'

parseLst :: Parser Val
parseLst = char '\'' >> (parens $ sepEndBy parseVal' whiteSpace) >>= \xs -> return $ List xs
  where sym = identifier >>= \x -> return $ Symbol x    
        lst = List <$> (parens $ sepEndBy parse' whiteSpace)
        parse' = sym <|> lst <|> parseVal'       
      
parseBol :: Parser Val
parseBol = try true <|> try false
  where true = reserved "#t" >>= \x -> return . Boolean $ True
        false = reserved "#f" >>= \x -> return . Boolean $ False
  
parseStr :: Parser Val
parseStr = char '\"'  >> (manyTill anyChar  (char '\"')) >>= \str -> return $ String str

parseLambda :: Parser Val
parseLambda = parens
  $ do reserved "lambda"
       params <- parens $ sepEndBy parseVar whiteSpace
       body   <- sepEndBy parseSyntax whiteSpace
       return $ (Lambda params (Begin body))

-- Syntax Selectors -------------------------------------------------------------------------------
val :: Syntax -> Val
val (Val v) = v

var :: Syntax -> String
var (Var v) = v

assignVar :: Syntax -> Syntax
assignVar (Assign v _) = v

assignVal :: Syntax -> Syntax
assignVal (Assign _ v) = v

defineVar :: Syntax -> Syntax
defineVar (Define v _) = v

defineVal :: Syntax -> Syntax
defineVal (Define _ v) = v 

ifPred :: Syntax -> Syntax
ifPred (If p _ _) = p

ifCon :: Syntax -> Syntax
ifCon  (If _ c _) = c

isAlt :: Syntax -> Syntax
isAlt  (If _ _ a) = a

appOp :: Syntax -> Syntax
appOp (App op _)     = op

appArgs :: Syntax -> [Syntax]
appArgs (App _ args) = args

beginSeq :: Syntax -> [Syntax] 
beginSeq (Begin seq) = seq

-- Value Selectors --------------------------------------------------------------------------------
numberVal :: Val -> Double
numberVal (Number x) = x

listVal :: Val -> [Val]
listVal (List xs) = xs

lambdaParams :: Val -> [String]
lambdaParams (Lambda ps _) = map var ps

lambdaBody :: Val -> Syntax 
lambdaBody (Lambda _ b) = b

-- Predicates -------------------------------------------------------------------------------------
isTrue x = x /= (Val (Boolean False))
