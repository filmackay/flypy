module IR.CPS
( Term(..)
, Kont(..)
, Fun(..)
, Prim(..)
) where

import IR.IR

import Data.String.Utils

-- CPS Intermediate Representation
data Term = -- let x = val in c
            LetVal Pos Var Val Term

            -- (LetCont [k_i] c): let k_i x = body in c
          | LetCont [Kont] Term

            -- (LetAbs [f_i] c): let f_i x = body in c
          | LetAbs [Fun] Term

            -- (AppK pos k a) : invoke continuation
            -- This corresponds to a return statement
          | AppK Pos Var [Var]

            -- (App pos f k h [a_i]): function application with continuation k and
            --                        error handler h
          | App Pos Var Var Var [Var]

            -- (PrimOp pos x (Add ...) c): primitive operation
            -- (e.g. let x = a + b in c)
          | PrimOp Pos Var Prim Term
          deriving (Eq)


-- (Cont pos k [arg_i] body): let k x = body
data Kont = Cont Pos Var [Var] Term
          deriving (Eq)

-- (Fun pos f [arg_i] body): let f x' = body
data Fun  = Lam Pos Var [Var] Term
          deriving (Eq)

-- String utilities
indent :: String -> Int -> String
indent s n = indentLines (splitLines s) n

splitLines :: String -> [String]
splitLines = split "\n"

indentLines :: [String] -> Int -> String
indentLines lines n = join "\n" [ ws ++ line | line <- lines ]
                    where ws = concat $ replicate n " "

-- IR Pretty printing

instance Show Term where
    show (LetVal _ var val cont) = unwords ["let", var, "=", show val,
                                            "in", show cont]
    show (LetCont cs body) = unwords ["let ",
                                      lstrip (indentLines (map show cs) 4),
                                      "in", show body]
    show (AppK pos f args) = f ++ " " ++ (unwords args)

instance Show Kont where
    show (Cont pos k args body) = unwords ["(", k, unwords args, " = ",
                                           show body, ")"]
