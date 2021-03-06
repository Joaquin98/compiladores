module Optimizations where
import Lang 
import Control.Monad.State
import Subst
import Common

change :: Bool -> State (Bool, Int) ()
change b = modify (\(_,n) -> (b,n))

getChange :: State (Bool, Int) Bool
getChange = do (b,n) <- get
               return b

---------------------------------------------------------------------
-------------------------- CONSTANT FOLDING -------------------------
---------------------------------------------------------------------
getNat :: Const -> Int
getNat (CNat n) = n

reduce :: Term -> Term
reduce (BinaryOp i Add x (Const _ (CNat 0)))       = x                    -- x + 0 = x
reduce (BinaryOp i Add (Const _ (CNat 0)) x)       = x                    -- 0 + x = x
reduce (BinaryOp i Diff x (Const _ (CNat 0)))      = x                    -- x - 0 = x
reduce (BinaryOp i Diff (Const _ (CNat 0)) x)      = (Const i (CNat 0))   -- 0 - x = 0
reduce (BinaryOp i Add (Const _ c1) (Const _ c2))  = (Const i (CNat ((getNat c1) + (getNat c2))))
reduce (BinaryOp i Diff (Const _ c1) (Const _ c2)) = (Const i (CNat ((getNat c1) - (getNat c2))))
reduce x = x


constFolding' :: Term -> State (Bool, Int) Term
-- casos optimizacion
constFolding' (BinaryOp i op tm1 tm2)        = do change True
                                                  tm1' <- constFolding' tm1
                                                  tm2' <- constFolding' tm2
                                                  return $ reduce $ BinaryOp i op tm1' tm2'
-- casos recursivos
constFolding' (V i var)                      = return $ V i var
constFolding' (Const i c)                    = return $ Const i c
constFolding' (Lam i name ty tm)             = do tm'  <- constFolding' tm
                                                  return $ Lam i name ty tm'
constFolding' (App i tm1 tm2)                = do tm1' <- constFolding' tm1
                                                  tm2' <- constFolding' tm2
                                                  return $ App i tm1' tm2'
constFolding' (Fix i fname fty aname aty tm) = do tm'  <- constFolding' tm
                                                  return $ Fix i fname fty aname aty tm'
constFolding' (IfZ i c tm1 tm2)              = do c'   <- constFolding' c
                                                  tm1' <- constFolding' tm1
                                                  tm2' <- constFolding' tm2
                                                  return $ IfZ i c' tm1' tm2'
constFolding' (LetIn i name ty tm1 tm2)      = do tm1' <- constFolding' tm1
                                                  tm2' <- constFolding' tm2
                                                  return $ LetIn i name ty tm1' tm2'

constFolding :: [Decl Term Ty] -> State (Bool, Int) [Decl Term Ty]
constFolding [] = return []
constFolding (d:ds) = do body <- constFolding' (declBody d)
                         rest <- constFolding ds
                         return $ (Decl (declPos d) (declName d) (declType d) body) : rest


---------------------------------------------------------------------
----------------------------- DEADCODE ------------------------------
---------------------------------------------------------------------

usedDecls :: [Decl Term Ty] -> [Name]
usedDecls decls = foldr (++) [] (map (freeVars . declBody) decls) 

removeUnused :: [Decl Term Ty] -> [Name] -> State (Bool, Int) [Decl Term Ty]
removeUnused [x] names    = return $ [x]
removeUnused (d:ds) names = case elem (declName d) names of
                                False -> do change True
                                            removeUnused ds names
                                True  -> do ds' <- removeUnused ds names
                                            return $ d:ds'

deadDecls :: [Decl Term Ty] -> State (Bool, Int) [Decl Term Ty]
deadDecls ds = removeUnused ds (usedDecls ds)


deadIf' :: Term -> State (Bool, Int) Term
-- casos optimizacion
deadIf' (IfZ i c tm1 tm2) = do change True
                               c'   <- deadIf' c
                               tm1' <- deadIf' tm1
                               tm2' <- deadIf' tm2
                               case c' of
                                 Const _ (CNat 0) -> return tm1' -- ifz 0 then t1 else t2 = t1
                                 Const _ (CNat n) -> return tm2' -- ifz n then t1 else t2 = t2
                                 _                -> return $ IfZ i c' tm1' tm2' 
-- casos recursivos
deadIf' (V i var)                      = return $ V i var
deadIf' (Const i c)                    = return $ Const i c
deadIf' (Lam i name ty tm)             = do tm'  <- deadIf' tm
                                            return $ Lam i name ty tm'
deadIf' (App i tm1 tm2)                = do tm1' <- deadIf' tm1
                                            tm2' <- deadIf' tm2
                                            return $ App i tm1' tm2'
deadIf' (BinaryOp i op tm1 tm2)        = do tm1' <- deadIf' tm1
                                            tm2' <- deadIf' tm2
                                            return $ BinaryOp i op tm1' tm2'
deadIf' (Fix i fname fty aname aty tm) = do tm'  <- deadIf' tm
                                            return $ Fix i fname fty aname aty tm'
deadIf' (LetIn i name ty tm1 tm2)      = do tm1' <- deadIf' tm1
                                            tm2' <- deadIf' tm2
                                            return $ LetIn i name ty tm1' tm2'


deadIf :: [Decl Term Ty] -> State (Bool, Int) [Decl Term Ty]
deadIf []     = return []
deadIf (d:ds) = do body <- deadIf' (declBody d)
                   rest <- deadIf ds
                   return $ (Decl (declPos d) (declName d) (declType d) body) : rest

deadCode :: [Decl Term Ty] -> State (Bool, Int) [Decl Term Ty]
deadCode ds = do ifs <- deadIf ds
                 deadDecls ifs


---------------------------------------------------------------------
----------------------------- EXPANSION -----------------------------
---------------------------------------------------------------------

size :: Term -> Int
size (BinaryOp i op tm1 tm2)        = 1 + (size tm1) + (size tm2)
size (V i var)                      = 1
size (Const i c)                    = 1
size (Lam i name ty tm)             = 1 + (size tm)
size (App i tm1 tm2)                = 1 + (size tm1) + (size tm2)
size (Fix i fname fty aname aty tm) = 1 + (size tm)
size (IfZ i c tm1 tm2)              = 1 + (size c) + (size tm1) + (size tm2)
size (LetIn i name ty tm1 tm2)      = 1 + (size tm1) + (size tm2)

height :: Term -> Int
height (BinaryOp i op tm1 tm2)        = 1 + (max (height tm1) (height tm2))
height (V i var)                      = 1
height (Const i c)                    = 1
height (Lam i name ty tm)             = 1 + (height tm)
height (App i tm1 tm2)                = 1 + (max (height tm1) (height tm2))
height (Fix i fname fty aname aty tm) = 1 + (height tm)
height (IfZ i c tm1 tm2)              = 1 + (max (height c)  (max (height tm1) (height tm2)))
height (LetIn i name ty tm1 tm2)      = 1 + (max (height tm1) (height tm2))

maxSize :: Int
maxSize = 20

shortDecl :: [Decl Term Ty] -> Maybe (Decl Term Ty)
shortDecl []                                   = Nothing   
shortDecl [x]                                  = Nothing
shortDecl (d:ds) | size (declBody d) < maxSize = Just d
                 | otherwise                   = shortDecl ds


getNewVar :: State (Bool, Int) String
getNewVar = do (b,n) <- get
               modify (\(b,n) -> (b,n+1))
               return $ "_linexp" ++ show(n)


-- EXPANSIÓN DE DECLARACIONES CORTAS:

expDecl' :: (Decl Term Ty) -> Term -> State (Bool, Int) Term
-- casos optimización
expDecl' d (App i (V j (Free name)) (V k var))          | name == (declName d) = case (declBody d) of
                                                                                    (Lam _ nameVar ty tm) -> do change True
                                                                                                                return $ subst (V k var) tm
                                                                                    fix                   -> return fix
expDecl' d (App i (V j (Free name)) (Const k (CNat n))) | name == (declName d) = case (declBody d) of
                                                                                    (Lam _ nameVar ty tm) -> do change True
                                                                                                                return $ subst (Const k (CNat n)) tm
                                                                                    fix                   -> return fix
expDecl' d (App i (V j (Free name)) tm2)                | name == (declName d) = do newVar <- getNewVar
                                                                                    case (declBody d) of 
                                                                                       (Lam k nameVar ty tm) -> do change True
                                                                                                                   (expDecl' d (LetIn NoPos newVar ty tm2 tm))
                                                                                       fix                   -> return fix  
expDecl' d (V i (Free name))                            | name == (declName d) = do change True
                                                                                    return (declBody d)  

-- casos recursivos  
-- nos fijamos si en tm1 cambia algo porque si se reduce podemos volver a reducir
-- si lo hacemos siempre sin checkear, puede quedar en loop infinito                                                                                               
expDecl' d (App i tm1 tm2)                = do b1 <- getChange
                                               change False
                                               tm1' <- expDecl' d tm1
                                               b2 <- getChange
                                               change $ b1 || b2
                                               tm2' <- expDecl' d tm2
                                               if b2 then expDecl' d $ App i tm1' tm2'
                                                     else return $ App i tm1' tm2'
expDecl' d (V i var)                      = return $ V i var
expDecl' d (Const i c)                    = return (Const i c)
expDecl' d (Lam i name ty tm)             = do tm'  <- expDecl' d tm
                                               return $ Lam i name ty tm'
expDecl' d (BinaryOp i op tm1 tm2)        = do tm1' <- expDecl' d tm1
                                               tm2' <- expDecl' d tm2
                                               return $ BinaryOp i op tm1' tm2'
expDecl' d (IfZ i c tm1 tm2)              = do c'   <- expDecl' d c
                                               tm1' <- expDecl' d tm1
                                               tm2' <- expDecl' d tm2
                                               return $ IfZ i c' tm1' tm2'
expDecl' d (Fix i fname fty aname aty tm) = do tm'  <- expDecl' d tm
                                               return $ Fix i fname fty aname aty tm'
expDecl' d (LetIn i name ty tm1 tm2)      = do tm1' <- expDecl' d tm1
                                               tm2' <- expDecl' d tm2
                                               return $ LetIn i name ty tm1' tm2'


expDecl :: (Decl Term Ty) -> [Decl Term Ty] -> State (Bool, Int) [Decl Term Ty]
expDecl decl [] = return []
expDecl decl (d:ds) | (declName decl) == (declName d)  = expDecl decl ds
                     | otherwise                        = do list <- expDecl decl ds
                                                             expBody <- expDecl' decl (declBody d)
                                                             return $ (Decl (declPos d) (declName d) (declType d) (expBody)) : list

expDecls :: [Decl Term Ty] -> State (Bool, Int) [Decl Term Ty]
expDecls ds = case shortDecl ds of
                      Nothing -> return ds
                      Just d  -> do explist <- expDecl d ds
                                    expDecls explist


-- EXPANSIÓN DE EXPRESIONES INTERNAS:
-- expandimos llamadas a funciones Lam 
--y Lets que tengan una variable o constante

expInt' ::  Term -> State (Bool, Int) Term
-- casos optimización
expInt' (App i (Lam _ name ty tm) (V k var))          = do change True
                                                           return $ subst (V k var) (tm)
expInt' (App i (Lam _ name ty tm) (Const k (CNat n))) = do change True
                                                           return $ subst (Const k (CNat n)) (tm)
expInt' (App i (Lam _ name ty tm) tm2)                = do newVar <- getNewVar 
                                                           change True
                                                           expInt' (LetIn NoPos newVar ty tm2 tm)                                                                       
expInt' (LetIn i name ty (V k var) tm2)               = do change True
                                                           return $ subst (V k var) (tm2)
expInt' (LetIn i name ty (Const k (CNat n)) tm2)      = do change True
                                                           return $ subst (Const k (CNat n)) (tm2)
-- casos recursivos
-- nos fijamos si en tm1 cambia algo porque si se reduce podemos volver a reducir
-- si lo hacemos siempre sin checkear, puede quedar en loop infinito
expInt' (App i tm1 tm2)                               = do b1 <- getChange
                                                           change False
                                                           tm1' <- expInt' tm1
                                                           b2 <- getChange
                                                           change $ b1 || b2 
                                                           tm2' <- expInt' tm2
                                                           if b2 then expInt' $ App i tm1' tm2'
                                                                 else return $ App i tm1' tm2'    
-- nos fijamos si en tm1 cambia algo porque si se reduce podemos volver a reducir
-- si lo hacemos siempre sin checkear, puede quedar en loop infinito                                                                                                                        
expInt' (LetIn i name ty tm1 tm2)                     = do b1 <- getChange
                                                           change False
                                                           tm1' <- expInt' tm1
                                                           b2 <- getChange
                                                           change $ b1 || b2
                                                           tm2' <- expInt' tm2
                                                           if b2 then expInt' $ LetIn i name ty tm1' tm2' 
                                                                 else return $ LetIn i name ty tm1' tm2'                                                                                                    
expInt' (V i var)                                     = return $ V i var
expInt' (Const i c)                                   = return (Const i c)
expInt' (Lam i name ty tm)                            = do tm'  <- expInt' tm
                                                           return $ Lam i name ty tm'
expInt' (BinaryOp i op tm1 tm2)                       = do tm1' <- expInt' tm1
                                                           tm2' <- expInt' tm2
                                                           return $ BinaryOp i op tm1' tm2'
expInt' (IfZ i c tm1 tm2)                             = do c'   <- expInt' c
                                                           tm1' <- expInt' tm1
                                                           tm2' <- expInt' tm2
                                                           return $ IfZ i c' tm1' tm2'
expInt' (Fix i fname fty aname aty tm)                = do tm'  <- expInt' tm
                                                           return $ Fix i fname fty aname aty tm'


expInt :: [Decl Term Ty] -> State (Bool, Int) [Decl Term Ty]
expInt [] = return []
expInt (d:ds) = do list <- expInt ds
                   expBody <- expInt' (declBody d)
                   return $ (Decl (declPos d) (declName d) (declType d) (expBody)) : list


---------------------------------------------------------------------
------------------------ BUCLE OPTIMIZACIONES -----------------------
---------------------------------------------------------------------

maxIterations :: Int
maxIterations = 5


optRound :: [Decl Term Ty] -> State (Bool, Int) [Decl Term Ty]
optRound ds = do dc   <- deadCode ds
                 expd <- expDecls dc
                 expi <- expInt expd
                 constFolding expi


optimize' :: Int -> [Decl Term Ty] -> State (Bool, Int) [Decl Term Ty]
optimize' n ds | n == maxIterations = return ds
               | otherwise          = do res   <- optRound ds  
                                         (b,_) <- get
                                         if b then do change False
                                                      optimize' (n+1) res
                                              else return ds
                                       

optimize :: [Decl Term Ty] -> [Decl Term Ty]
optimize ds = fst $ runState (optimize' 0 ds) (False, 0)


-- Ver comentarios de Clousures (hay que ver si son útiles)
-- Opt: ver tema tamaño para eliminar

-- Las recursivas.
-- Le agregamo el let y exp de apps de lam internos.
-- Casos prueba.

