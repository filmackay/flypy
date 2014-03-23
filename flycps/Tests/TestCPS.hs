import IR.IR
import IR.CPS

main = do {
    print (let appk = (AppK emptypos "k" ["x"]) :: Term
               term = LetCont [Cont emptypos "k" ["a"] appk] appk
           in LetVal emptypos "x" (IntVal 10) term)
}