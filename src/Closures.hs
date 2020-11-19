module Closures (runCC,printIrDecls) where
import Lang
import Control.Monad.Writer
import Control.Monad.State.Lazy
import Subst
import Data.List

-- AGREGADO --
---------------------------------------------------------------------------
freeVars' :: Term -> [Name]
freeVars' t = filter (\name -> isPrefixOf "__" name) (freeVars t)

namesToIrTms :: [Name] -> [IrTm]
namesToIrTms = map (\name -> IrVar name)

printIrDecls :: [IrDecl] -> IO ()
printIrDecls []     = putStrLn ""
printIrDecls (d:ds) = do putStrLn $ show(d)
                         printIrDecls ds
---------------------------------------------------------------------------


makeTerm :: Name -> [Name] -> IrTm -> Int -> IrTm
makeTerm closN [] t n         = t 
makeTerm closN (var:vars) t n = IrLet var (IrAccess (IrVar closN) n) (makeTerm closN vars t (n+1)) 

makeTermR :: Name -> [Name] -> IrTm -> Int -> IrTm
makeTermR closN (f:vars) t n = IrLet f (IrVar closN) (makeTerm closN vars t n)

closureConvert :: Term -> StateT Int (Writer [IrDecl]) IrTm
closureConvert (V _ (Free n))          = return $ IrVar n
closureConvert (Const _ const)         = return $ IrConst const
closureConvert (Lam _ name _ t)        = 
    do n <- get
       modify (+3)
       let varName = "__" ++ name ++ show(n)
           funName = "__" ++ show(n+1)
           cloName = "__clo" ++ show(n+2) 
           fVars = freeVars' t in 
        do t' <- closureConvert (open varName t)
           tell $ [IrFun funName 2 [cloName,varName] (makeTerm cloName fVars t' 1)]
           return $ MkClosure funName (namesToIrTms fVars)          
closureConvert (App _ f e)             = 
    do f' <- closureConvert f 
       e' <- closureConvert e
       n <- get
       modify (+1)
       let name = ("__e" ++ (show n))
       return $ IrLet name f' (IrCall (IrAccess (IrVar name) 0) [(IrVar name),e']) 
closureConvert (BinaryOp _ op t1 t2)   = do ct1 <- closureConvert t1
                                            ct2 <- closureConvert t2 
                                            return $ IrBinaryOp op ct1 ct2 
closureConvert (Fix _ funN _ varN _ t) = 
    do n <- get
       modify (+4)
       let varName = "__" ++ varN ++ show(n)
           funName = "__" ++ show(n+1)
           cloName = "__clo" ++ show(n+2)
           funRName = "__" ++ funN ++ show(n+3) 
           fVars = freeVars' t in 
        do t' <- closureConvert (openN [funRName,varName] t) -- en que orden el open???
           tell $ [IrFun funName 2 [cloName,varName] (makeTermR cloName (funRName:fVars) t' 1)] 
           return $ MkClosure funName (namesToIrTms fVars)  -- está bien????   

closureConvert (IfZ _ c t1 t2)         = do cc <- closureConvert c
                                            ct1 <- closureConvert t1
                                            ct2 <- closureConvert t2
                                            return $ IrIfZ cc ct1 ct2 
closureConvert (LetIn _ name _ t1 t2)  = do ct1 <- closureConvert t1
                                            n <- get
                                            modify (+1)
                                            let newName = "__" ++ name ++ (show n)
                                            do ct2 <- closureConvert (open newName t2)
                                               return $ IrLet newName ct1 ct2

-- esta bien?????'
runCC' :: [Decl Term Ty] -> StateT Int (Writer [IrDecl]) ()
runCC' []     = return ()
runCC' (d:ds) = do t <- closureConvert (declBody d)
                   tell $ [IrVal (declName d) t]   
                   runCC' ds

runCC :: [Decl Term Ty] -> [IrDecl]
runCC ds     = let (_,decls) = (runWriter (runStateT (runCC' (reverse ds)) 0))
               in decls
                         
                      
