module IR.SSA
( Function
, Block
, Label
, Value
, Instr
) where

import IR.IR

data Function = Function String [Value]
data Block = Block Label [Value]
data Value = Op Var Instr

type Label = String

data Instr = -- SSA: φ([block_i], [val_i])
             Phi [Label] [Var]

             -- Control flow
           | CBranch Var Label Label
           | Jump Label
           | Const Val

            -- (PrimOp pos x (Add ...)): primitive operation
            -- (e.g. let x = a + b in c)
          | PrimOp Pos Var Prim
