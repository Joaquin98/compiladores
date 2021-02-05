module Closures (runCC,printIrDecls) where
import Lang
import Control.Monad.Writer
import Control.Monad.State.Lazy
import Subst
import Data.List
import Data.Set (toList, fromList)


-- Retorna un nombre fresco.
getName :: String -> StateT Int (Writer [IrDecl]) String
getName name = do n <- get
                  modify (+1)
                  return $ "__" ++ name ++ show(n)  

-- Dado un termino, retorna las variables libres que se encuentran en él.
freeVars' :: Term -> StateT Int (Writer [IrDecl]) [Name]
freeVars' t =  return $ toList $ fromList $ filter (\name -> isPrefixOf "__" name) (freeVars t)

namesToIrTms :: [Name] -> [IrTm]
namesToIrTms = map (\name -> IrVar name)

-- Muestra las declaraciones intermedias con espacios entre ellas.
printIrDecls :: [IrDecl] -> IO ()
printIrDecls []     = putStrLn ""
printIrDecls (d:ds) = do putStrLn $ show(d)
                         printIrDecls ds

-- Dados el nombre de la clausura y en termino dentro de la funcion, 
-- crea la función top level. 
makeTerm :: Name -> [Name] -> IrTm -> Int -> IrTm
makeTerm closN [] t n         = t 
makeTerm closN (var:vars) t n = IrLet var (IrAccess (IrVar closN) n) (makeTerm closN vars t (n+1)) 

-- Similar al anterior pero es para funciones recursivas (let f = clos). 
makeTermR :: Name -> [Name] -> IrTm -> Int -> IrTm
makeTermR closN (f:vars) t n = IrLet f (IrVar closN) (makeTerm closN vars t n)


closureConvert :: Term -> StateT Int (Writer [IrDecl]) IrTm
-- [[x]] = x
closureConvert (V _ (Free n))          = return $ IrVar n

-- [[c]] = c
closureConvert (Const _ const)         = return $ IrConst const

-- [[fun x -> t]] = MackeClosure codef [v1,v2,v3,...,vn]
-- vi son las variables libres. 
-- codef (clos,x) = let v1 = clos[1], v2 = clos[2], ...  in [[t]] 
closureConvert (Lam _ name _ t)        = 
    do varName <- getName name
       funName <- getName ""
       cloName <- getName "clo"
       fVars <- freeVars' t
       t' <- closureConvert (open varName t)
       tell $ [IrFun funName 2 [cloName,varName] (makeTerm cloName fVars t' 1)]
       return $ MkClosure funName (namesToIrTms fVars) 

-- [[f e]] = let clos = [[f]] in clos[0] (clos,[[e]]) 
-- clos[0] (clos,[[e]]) es la aplicación (IrCall)
closureConvert (App _ f e)             = 
    do f' <- closureConvert f 
       e' <- closureConvert e
       name <- getName "e"
       return $ IrLet name f' (IrCall (IrAccess (IrVar name) 0) [(IrVar name),e']) 

-- [[op(x1,x2)]] = op ([[x1]],[[x2]])
closureConvert (BinaryOp _ op t1 t2)   = do ct1 <- closureConvert t1
                                            ct2 <- closureConvert t2 
                                            return $ IrBinaryOp op ct1 ct2 

-- Igual que con Lam pero, con 
-- codef (clos,x) = let f = clos,v1 = clos[1], v2 = clos[2], ...  in [[t]] 
closureConvert (Fix _ funN _ varN _ t) = 
    do varName <- getName varN
       funName <- getName ""
       cloName <- getName "clo"
       funRName <- getName funN
       fVars <- freeVars' t
       t' <- closureConvert (openN [funRName,varName] t) 
       tell $ [IrFun funName 2 [cloName,varName] (makeTermR cloName (funRName:fVars) t' 1)] 
       return $ MkClosure funName (namesToIrTms fVars)    

-- Ifz c t1 t1 = Ifz [[c]] [[t1]] [[t2]]
closureConvert (IfZ _ c t1 t2)         = do cc <- closureConvert c
                                            ct1 <- closureConvert t1
                                            ct2 <- closureConvert t2
                                            return $ IrIfZ cc ct1 ct2 

closureConvert (LetIn _ name _ t1 t2)  = do ct1 <- closureConvert t1
                                            newName <- getName name
                                            ct2 <- closureConvert (open newName t2)
                                            return $ IrLet newName ct1 ct2


runCC' :: [Decl Term Ty] -> StateT Int (Writer [IrDecl]) ()
runCC' []     = return ()
runCC' (d:ds) = do t <- closureConvert (declBody d)
                   tell $ [IrVal (declName d) t]   
                   runCC' ds

runCC :: [Decl Term Ty] -> [IrDecl]
runCC ds     = let (_,decls) = (runWriter (runStateT (runCC' (reverse ds)) 0))
               in decls
                         
                      
