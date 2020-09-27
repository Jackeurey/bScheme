import Parser
import Environment
import Control.Monad.Trans.State
import qualified Data.Map as Map
import System.Environment hiding (lookupEnv)

type Exp = State (Env Val) Syntax

--lifts a string into type Exp
liftA src = state $ \s -> ((analyze src),s)

evalVal :: Exp -> Exp
evalVal exp = exp 


evalVar :: Exp -> Exp
evalVar exp = get >>= \env -> exp >>= \(Var v) -> return . Val $ lookupEnv v env

evalDefine :: Exp -> Exp
evalDefine exp = do (Define (Var v) x) <- exp
                    x'                 <- eval (return x) 
                    modify (defineEnv v  (val x'))
                    return (Val Null)

evalAssign :: Exp -> Exp
evalAssign exp = do (Assign (Var v) x) <- exp
                    x'                 <- eval (return x)
                    modify (modEnv v (val x'))
                    return (Val Null)         

evalIf :: Exp -> Exp
evalIf exp = do (If p c a) <- exp
                p'         <- eval (return p)
                if isTrue p'
                then eval (return c)
                else eval (return a)

evalBegin :: Exp -> Exp
evalBegin exp = do exp >>= \(Begin seq) -> evalSeq seq
  where evalSeq (x:xs) = foldl foldproc (eval (return x)) xs
        foldproc b a   = b >> (eval (return a))

evalApp :: Exp -> Exp
evalApp exp = do (App op as) <- exp
                 op'         <- eval (return op)
                 as'         <- mapM (\x -> eval (return x)) as
                 modify (extendEnv (lambdaParams (val op')) (map val as'))
                 value       <- eval . return . lambdaBody . val $ op'
                 modify dropFrame
                 return value

evalPrim :: Exp -> Exp
evalPrim exp = do (Prim (Var v) as) <- exp
                  as' <- mapM (\x -> eval (return x)) as
                  let f = (Map.!) primEnv v
                  return . Val $ f (map val as')


primEnv = Map.fromList [("id", \[x] -> x),
                        ("car", \[List (x:xs)] -> x),
                        ("cdr", \[List (x:xs)] -> List xs),
                        ("null?", \[List xs] -> Boolean . null $ xs),
                        ("cons", \[x,(List xs)] -> List (x:xs)),
                        ("+", \xs -> Number $ sum (map numberVal xs)),
                        ("*", \xs -> Number $ product (map numberVal xs)),
                        ("-", \xs -> let (x':xs') = map numberVal xs
                                     in Number $ foldl (-) x' xs')]
                
eval :: Exp -> Exp                 
eval exp = case evalState exp [] of
              Val _      -> evalVal exp
              Var _      -> evalVar exp
              Define _ _ -> evalDefine exp
              Assign _ _ -> evalAssign exp
              If _ _ _   -> evalIf exp
              Begin _    -> evalBegin exp
              App _ _    -> evalApp exp
              Prim _ _   -> evalPrim exp

run :: String -> Env Val -> (Syntax,Env Val)
run src env = runState (eval (liftA src)) env

envLoader :: String -> Val 
envLoader src = val . defineVal . head . beginSeq . analyze $ src

priLoader :: String -> Val
priLoader src = val . head . beginSeq . analyze $ src

initEnv = Map.fromList [("id", priLoader "(lambda (x) (id x))"),
                        ("car", priLoader "(lambda (x) (car x))"),
                        ("cdr", priLoader "(lambda (x) (cdr x))"),
                        ("null?", priLoader "(lambda (x) (null? x))"),
                        ("cons", priLoader "(lambda (x xs) (cons x xs))"),
                        ("+", priLoader "(lambda (xs) (+ xs))"),
                        ("map", envLoader "(define (map f lst) (if (null? lst) '() (cons (f (car lst)) (map f (cdr lst)))))"),
                        ("foldr", envLoader "(define (foldr f x xs) (if (null? xs) x (f (car xs) (foldr f x (cdr xs)))))")]  
                        : makeEnv

repl :: Env Val -> IO ()
repl init = do putStr "bScheme: " 
               src <- getLine
               let (value,env) = run src init
               putStrLn . show $ value
               repl env

runFile :: String -> IO ()
runFile src = do pgm <- readFile src
                 putStrLn . show . fst $ run pgm initEnv 

main :: IO ()
main = do args <- getArgs
          if null args
          then repl initEnv
          else runFile (head args)
              