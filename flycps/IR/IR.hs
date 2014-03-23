module IR.IR
( Pos
, emptypos
, Var
, Val(..)
, Prim(..)
) where

import Data.String.Utils

-- | (Rudimentary position where term originates from
data Pos = Pos { fname :: String, line :: Int, col :: Int }
         deriving (Eq)

instance Show Pos where
    show (Pos fname line col) = join ":" [fname, show line, show col]

emptypos = Pos "" 0 0


-- | Assignment variable
type Var = String

-- | Constant values
data Val = IntVal Int
         | FloatVal Float
         | VectorVal [Val] -- SIMD Vector
         | None
         deriving (Eq)

instance Show Val where
    show (IntVal i) = show i
    show (FloatVal f) = show f
    show (VectorVal v) = show v
    show None = "None"


-- | Primitive operations
data Prim = Add Var Var
          | Mul Var Var
          | Sub Var Var
          | Div Var Var

          -- Record projection
          | Getattr Var String
          | Setattr Var String Var
          deriving (Eq)

instance Show Prim where
    show (Add a b) = (show a) ++ " + " ++ (show b)
    show (Mul a b) = (show a) ++ " * " ++ (show b)
    show (Sub a b) = (show a) ++ " - " ++ (show b)
    show (Div a b) = (show a) ++ " / " ++ (show b)

    show (Getattr obj attr) = (show obj) ++ "." ++ attr
    show (Setattr obj attr val) = ((show obj) ++ "." ++ attr ++ " = " ++
                                   (show val))