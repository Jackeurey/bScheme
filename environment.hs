module Environment (Env, lookupEnv, modEnv, defineEnv, extendEnv, makeEnv, dropFrame) where

import qualified Data.Map.Lazy as Map
import Control.Monad.State

type Env a = [Map.Map String a]

--goes through each map and checks if the variable exists in the scope, error otherwise
lookupEnv :: String -> Env a -> a
lookupEnv key []       = error $ "variable \"" ++ key ++ "\" " ++ "  not bound in the environment!"
lookupEnv key (x:xs) = case Map.lookup key x of
                        Nothing  -> lookupEnv key xs
                        Just val -> val 

makeEnv :: Env a
makeEnv = [Map.empty]

--Adds variable to current scope
defineEnv :: String -> a -> Env a -> Env a
defineEnv key val (x:xs) = Map.insert key val x : xs

--Like lookup only if the variable exists, it changes it
modEnv :: String -> a -> Env a -> Env a
modEnv _ _ []         = error "variable not bound in the environment!"
modEnv key val (x:xs) = case Map.lookup key x of
                          Nothing  -> x : modEnv key val xs
                          Just val ->  Map.insert key val x : xs

--Simulates the environment model of lisp procedure application
extendEnv :: [String] -> [a] -> Env a -> Env a
extendEnv ps as env = Map.fromAscList (zip ps as) : env

addFrame :: Env a -> Env a
addFrame env = Map.empty : env

dropFrame :: Env a -> Env a
dropFrame env = tail env