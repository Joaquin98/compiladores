{-|
Module      : CEK
Description : Evaluacion con maquina abstracta.
-}
module CEK (eval) where
import Lang
import Common
import Subst (substN)
import MonadPCF 
import PPrint (ppName)

data Frame =  KArg Env Term
            | KFun Clos 
            | KCond Env Term Term
            | KSucc
            | KPred
            | KLet Env Term 
            deriving (Show)

type Kont = [Frame]

data Val = VNat Const | VClos Clos
    deriving (Show)

type Env = [Val]

data Clos = CFun Env Name Ty Term
          | CFix Env Name Ty Name Ty Term
          deriving (Show)


search :: MonadPCF m => Term -> Env -> Kont -> m Val
search (UnaryOp i Pred t)            p k = search t p (KPred:k) 
search (UnaryOp i Succ t)            p k = search t p (KSucc:k)
search (IfZ i c t1 t2)               p k = search c p ((KCond p t1 t2):k)
search (App i t u)                   p k = search t p ((KArg p u):k) 
search (V i (Bound x))               p k = destroy (p!!x) k
search (V i (Free name))             p k = do mt <- lookupDecl name
                                              case mt of    
                                                Nothing -> failPCF $ "Error de ejecución: variable no declarada: " ++ ppName name
                                                Just t -> search t p k
search (Const i n)                   p k = destroy (VNat n) k 
search (Lam i name ty t)             p k = destroy (VClos (CFun p name ty t)) k
search (Fix i name1 ty1 name2 ty2 t) p k = destroy (VClos (CFix p name1 ty1 name2 ty2 t)) k
search (LetIn i n ty t t')           p k = search t p ((KLet p t'):k)


destroy :: MonadPCF m => Val -> Kont -> m Val
destroy v []                                   = return v
destroy (VNat (CNat 0)) (KPred:k)              = destroy (VNat (CNat 0)) k 
destroy (VNat (CNat n)) (KPred:k)              = destroy (VNat (CNat (n-1))) k 
destroy (VNat (CNat n)) (KSucc:k)              = destroy (VNat (CNat (n+1))) k 
destroy (VNat (CNat 0)) ((KCond p t1 t2):k)    = search t1 p k 
destroy (VNat (CNat n)) ((KCond p t1 t2):k)    = search t2 p k
destroy (VClos c) ((KArg p t):k)               = search t p ((KFun c):k)
destroy v ((KFun (CFun p x ty t)):k)           = search t (v:p) k
destroy v ((KFun cf@(CFix p f fty x xty t)):k) = search t (v:((VClos cf):p)) k
destroy v ((KLet p t'):k)                      = search t' (v:p) k
destroy v ks                                   = failPCF $ "Error DESTROY " ++ show(v) ++ show(ks)
 
valToTerm :: Val -> Term 
valToTerm (VNat n) = Const NoPos n
valToTerm (VClos (CFun [] x ty t)) = Lam NoPos x ty t
valToTerm (VClos (CFun p x ty t)) = replace p (Lam NoPos x ty t)
valToTerm (VClos (CFix [] f fty x xty t)) = Fix NoPos f fty x xty t  
valToTerm (VClos (CFix p f fty x xty t)) = replace p (Fix NoPos f fty x xty t)  

replace :: Env -> Term -> Term
replace p t = substN (reverse (map valToTerm p)) t
-- replace p t = substN (map valToTerm p) t

--(fun x -> (fun y -> x + y) ) 3
--x -> 3
-- (fun (x:Nat) -> fun (y:Nat) -> x) 3
-- fun x -> suma x 1

eval :: MonadPCF m => Term -> m Term
eval t = do v <- search t [] []
            return (valToTerm v) 
