{-
Aluno    : Gabriel da Silva Corvino Nogueira
Matricula: 180113330
-}

module Interpreter where

import Data.Generics
import AbsLI
import Prelude 
import Data.Maybe
import Memo

type Context k v = [(k,v)]                                
type RContext = (VContext, FContext)
type VContext = Context Ident Exp
type FContext = Context Ident Function



evalP :: Program -> Integer
evalP (Prog fs) =  eval ([],(updatecF [] fs)) (Call (Ident "main") [])   


eval :: RContext -> Exp -> Integer
eval context x = case x of
    EAdd exp0 exp  -> eval context exp0  +  eval context exp
    ESub exp0 exp  -> eval context exp0  -  eval context exp
    EMul exp0 exp  -> eval context exp0  *  eval context exp
    EDiv exp0 exp  -> eval context exp0 `div` eval context exp
    EInt n         -> n
    -- dica: considere alteracao na alternativa abaixo
    EVar id        -> eval context (fromJust (lookupMemo id (fst context)))
    EIf e1 e2 e3   -> if ( eval context e1 /= 0 )
                        then (eval context e2)
                        else (eval context e3)
    -- dica: considere alteracao na alternativa abaixo                  
    Call id@(Ident f_name) lexp   -> eval (paramBindings,contextFunctions) exp 
                          where Fun _ decls exp = fromJust (lookupMemo id ( snd context))
                                paramBindings = zip decls (map (evalVars contextVariables) lexp) 
                                contextVariables = fst context
                                contextFunctions = snd context


evalVars :: VContext -> Exp -> Exp
evalVars contextV = everywhere (mkT $ evalVar contextV)
  where evalVar ctx (EVar id) = fromJust (lookupMemo id ctx) 
        evalVar _ exp = exp

-- evalVars contextV x = case x of
--   EAdd exp exp' -> EAdd (evalVars contextV exp) (evalVars contextV exp')
--   ESub exp exp' -> ESub (evalVars contextV exp) (evalVars contextV exp')
--   EMul exp exp' -> EMul (evalVars contextV exp) (evalVars contextV exp')
--   EDiv exp exp' -> EDiv (evalVars contextV exp) (evalVars contextV exp')
--   EInt n        -> EInt n
--   EVar id       -> fromJust (lookupMemo id contextV)
--   EIf  e1 e2 e3 -> EIf (evalVars contextV e1) (evalVars contextV e2) (evalVars contextV e3)
--   Call id  lexp -> Call id (map (evalVars contextV) lexp)



updatecF :: FContext -> [Function] -> FContext
updatecF ctx [] = ctx
updatecF ctx (f@(Fun id _ _):fs) = updatecF (updateMemo ctx id f) fs


{-
  main () {
    fat (5)
  }
  
  fat (n) {
    if (n) 
       then n * fat (n - 1) 
       else 1
  }
-}

fat =  Prog [Fun (Ident "main") [] (Call (Ident "fat") [EInt 5]),
             Fun (Ident "fat") [Ident "n"] 
                                         (EIf (EVar (Ident "n")) 
                                           (EMul (EVar (Ident "n")) 
                                                  (Call (Ident "fat") [ESub (EVar (Ident "n")) (EInt 1)])) 
                                           (EInt 1))]

testCaseFat = evalP fat == 120


{-
 main () {
   fib (8)
}
 fib (n) {
   if (n) then 
      if (n - 1) 
        then fib (n - 1) + fib (n - 2) 
        else 1 
    else 1
}
-}

fibo = Prog [Fun (Ident "main") [] (Call (Ident "fib") [EInt 8]),
             Fun (Ident "fib") [Ident "n"] 
                        (EIf (EVar (Ident "n")) 
                                (EIf (ESub (EVar (Ident "n")) (EInt 1)) 
                                    (EAdd (Call (Ident "fib") [ESub (EVar (Ident "n")) (EInt 1)]) 
                                          (Call (Ident "fib") [ESub (EVar (Ident "n")) (EInt 2)])) 
                                    (EInt 1)) 
                              (EInt 1))]

testCaseFibo = evalP fibo == 34

-- testCaseSuiteResults deve ser true 
testCaseSuiteResults  = testCaseFat && testCaseFibo

