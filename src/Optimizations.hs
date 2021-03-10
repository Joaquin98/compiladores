module Optimizations (optimize) where
import Lang 
import Control.Monad.State
import Subst
import Common

-- El estado de esta mónada tiene 2 componentes. Un Bool indica si se porodujeron cambios en 
-- el programa al optimizar. Un Int lleva la cuenta de las variables frescas.

-- Permite setear el Bool de cambio. 
change :: Bool -> State (Bool, Int) ()
change b = modify (\(_,n) -> (b,n))


-- Permite obtener el Bool de cambio.
getChange :: State (Bool, Int) Bool
getChange = do (b,n) <- get
               return b

---------------------------------------------------------------------
-------------------------- CONSTANT FOLDING -------------------------
---------------------------------------------------------------------

-- Reducimos las operaciones entre constantes y las operaciones que puedan definirse por
-- propiedades aritméticas.

getNat :: Const -> Int
getNat (CNat n) = n

-- Toma el término a reducir (siempre será una BinaryOp) y realiza la operación o pone
-- el resultado que las propiedades indiquen.
reduce :: Term -> Term
reduce (BinaryOp i Add x (Const _ (CNat 0)))       = x                    -- x + 0 = x
reduce (BinaryOp i Add (Const _ (CNat 0)) x)       = x                    -- 0 + x = x
reduce (BinaryOp i Diff x (Const _ (CNat 0)))      = x                    -- x - 0 = x
reduce (BinaryOp i Diff (Const _ (CNat 0)) x)      = (Const i (CNat 0))   -- 0 - x = 0
reduce (BinaryOp i Add (Const _ c1) (Const _ c2))  = (Const i (CNat ((getNat c1) + (getNat c2))))
reduce (BinaryOp i Diff (Const _ c1) (Const _ c2)) = (Const i (CNat ((getNat c1) - (getNat c2))))
reduce x = x

-- Función auxiliar de constFolding que se llama recursivamente y en el caso de que el
-- término sea un BinaryOp llama a reduce.
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

-- Hace constFolding' para cada delcaración del programa
constFolding :: [Decl Term Ty] -> State (Bool, Int) [Decl Term Ty]
constFolding [] = return []
constFolding (d:ds) = do body <- constFolding' (declBody d)
                         rest <- constFolding ds
                         return $ (Decl (declPos d) (declName d) (declType d) body) : rest


---------------------------------------------------------------------
----------------------------- DEADCODE ------------------------------
---------------------------------------------------------------------

-- La eliminación de deadcode consiste de dos etapas. En la primera 
-- cuando las condiciones de los if son constantes, los remplaza por 
-- la rama que va a persistir. En la segunda se eliminan todas las
-- declaraciones que no sean referenciadas en ninguna parte del 
-- programa.

-- Obtiene todas las declaraciones referenciadas en alguna parte del 
-- programa.
usedDecls :: [Decl Term Ty] -> [Name]
usedDecls decls = foldr (++) [] (map (freeVars . declBody) decls) 

-- Se encarga de eliminar las declaraciones no referenciadas.
removeUnused :: [Decl Term Ty] -> [Name] -> State (Bool, Int) [Decl Term Ty]
removeUnused [x] names    = return $ [x]
removeUnused (d:ds) names = case elem (declName d) names of
                                False -> do change True
                                            removeUnused ds names
                                True  -> do ds' <- removeUnused ds names
                                            return $ d:ds'

deadDecls :: [Decl Term Ty] -> State (Bool, Int) [Decl Term Ty]
deadDecls ds = removeUnused ds (usedDecls ds)

-- Función auxiliar de deadIf que reeemplaza los if que tienen condiciones 
-- constantes por la rama que persiste, dentro de un termino.
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

-- Aplica deadIf' a cada declaración.
deadIf :: [Decl Term Ty] -> State (Bool, Int) [Decl Term Ty]
deadIf []     = return []
deadIf (d:ds) = do body <- deadIf' (declBody d)
                   rest <- deadIf ds
                   return $ (Decl (declPos d) (declName d) (declType d) body) : rest

-- Aplica ambas etapas de eliminación de deadcode
deadCode :: [Decl Term Ty] -> State (Bool, Int) [Decl Term Ty]
deadCode ds = do ifs <- deadIf ds
                 deadDecls ifs


---------------------------------------------------------------------
----------------------------- EXPANSION -----------------------------
---------------------------------------------------------------------

-- Hacemos dos tipos de expansión en línea:
--    - La primera expande declaraciones cortas, ya sean variables libres 
--    normales o funciones que se llaman.
--    - La segunda expande las aplicaciones de funciones Lam y los LetIn
--    cuya variable guarde simplemente otra variable o una constante.
--    No tiene sentido que haya un LetIn para esto ya que no requiere
--    operación, y esto puede producirse debido a otras optimizaciones. 

totalNodes :: Term -> Int
totalNodes (BinaryOp i op tm1 tm2)        = 1 + (totalNodes tm1) + (totalNodes tm2)
totalNodes (V i var)                      = 1
totalNodes (Const i c)                    = 1
totalNodes (Lam i name ty tm)             = 1 + (totalNodes tm)
totalNodes (App i tm1 tm2)                = 1 + (totalNodes tm1) + (totalNodes tm2)
totalNodes (Fix i fname fty aname aty tm) = 1 + (totalNodes tm)
totalNodes (IfZ i c tm1 tm2)              = 1 + (totalNodes c) + (totalNodes tm1) + (totalNodes tm2)
totalNodes (LetIn i name ty tm1 tm2)      = 1 + (totalNodes tm1) + (totalNodes tm2)

height :: Term -> Int
height (BinaryOp i op tm1 tm2)        = 1 + (max (height tm1) (height tm2))
height (V i var)                      = 1
height (Const i c)                    = 1
height (Lam i name ty tm)             = 1 + (height tm)
height (App i tm1 tm2)                = 1 + (max (height tm1) (height tm2))
height (Fix i fname fty aname aty tm) = 1 + (height tm)
height (IfZ i c tm1 tm2)              = 1 + (max (height c)  (max (height tm1) (height tm2)))
height (LetIn i name ty tm1 tm2)      = 1 + (max (height tm1) (height tm2))

-- Es el máximo tamaño que puede tener una declaración a expandir.
-- El tamaño puede ajustarse según conveniencia.
maxSize :: Int
maxSize = 20

-- Determina si el cuerpo de una declaración es lo suficientemente
-- corto para ser expandido según la función de tamaño que se le pase.
shortDecl :: [Decl Term Ty] -> (Term -> Int) -> Maybe (Decl Term Ty)
shortDecl []  size                                  = Nothing   
shortDecl [x] size                                  = Nothing
shortDecl (d:ds) size | size (declBody d) < maxSize = Just d
                      | otherwise                   = shortDecl ds size
      
-- Retorna nombre de variable fresca.
-- Utiliza el Int del estado de la mónada.
-- Empiezan con "_" para que no se confundan con las variables
-- definidas por el usuario o por la conversion de clausuras.
getNewVar :: State (Bool, Int) String
getNewVar = do (b,n) <- get
               modify (\(b,n) -> (b,n+1))
               return $ "_linexp" ++ show(n)


-- EXPANSIÓN DE DECLARACIONES CORTAS:

-- Función auxiliar de expDecl que expande la declaración dada recursivamente 
-- en un término.
expDecl' :: (Decl Term Ty) -> Term -> State (Bool, Int) Term
-- casos optimización
expDecl' d (App i (V j (Free name)) (V k var))          | name == (declName d) = case (declBody d) of
                                                                                    (Lam ii nameVar ty tm) -> do change True
                                                                                                                 return $ subst (V k var) tm
--                                                                                   fix                   -> return fix
                                                                                    fix                   -> return (App i fix (V k var))
expDecl' d (App i (V j (Free name)) (Const k (CNat n))) | name == (declName d) = case (declBody d) of
                                                                                    (Lam ii nameVar ty tm) -> do change True
                                                                                                                 return $ subst (Const k (CNat n)) tm
                                                                                    fix                   -> return (App i fix (Const k (CNat n)))
expDecl' d (App i (V j (Free name)) tm2)                | name == (declName d) = do newVar <- getNewVar
                                                                                    case (declBody d) of 
                                                                                       (Lam k nameVar ty tm) -> do change True
                                                                                                                   (expDecl' d (LetIn NoPos newVar ty tm2 tm))
                                                                                       fix                   -> return (App i fix  tm2)
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
expDecl' d (Lam i name ty tm)             = do vname <- getNewVar 
                                               tm'  <-  expDecl' d (open vname tm)
                                               return $ Lam i name ty (close vname tm')
expDecl' d (BinaryOp i op tm1 tm2)        = do tm1' <- expDecl' d tm1
                                               tm2' <- expDecl' d tm2
                                               return $ BinaryOp i op tm1' tm2'
expDecl' d (IfZ i c tm1 tm2)              = do c'   <- expDecl' d c
                                               tm1' <- expDecl' d tm1
                                               tm2' <- expDecl' d tm2
                                               return $ IfZ i c' tm1' tm2'
expDecl' d (Fix i fname fty aname aty tm) = do vfname <- getNewVar
                                               vaname <- getNewVar 
                                               tm'  <- expDecl' d (openN [vfname, vaname] tm)
                                               return $ Fix i fname fty aname aty (closeN [vfname, vaname] tm')
expDecl' d (LetIn i name ty tm1 tm2)      = do tm1' <- expDecl' d tm1
                                               vname <- getNewVar  
                                               tm2' <- expDecl' d (open vname tm2)
                                               return $ LetIn i name ty tm1' (close vname tm2')

-- Expande una declaración dada en todas las demás declaraciones.
expDecl :: (Decl Term Ty) -> [Decl Term Ty] -> State (Bool, Int) [Decl Term Ty]
expDecl decl [] = return []
expDecl decl (d:ds) | (declName decl) == (declName d)  = expDecl decl ds
                     | otherwise                        = do list <- expDecl decl ds
                                                             expBody <- expDecl' decl (declBody d)
                                                             return $ (Decl (declPos d) (declName d) (declType d) (expBody)) : list

-- Busca una declaración con el cuerpo suficientemente pequeño
-- (suando shortDecl) y la expande con expDecl. Hace esto hasta que
-- no haya más declaraciones cortas. Usamos totalNodes (cantidad de
-- nodos) para el tamaño, pero puede cambiarse por height si se prefiere.
expDecls :: [Decl Term Ty] -> State (Bool, Int) [Decl Term Ty]
expDecls ds = case shortDecl ds totalNodes of
                      Nothing -> return ds
                      Just d  -> do explist <- expDecl d ds
                                    expDecls explist


-- EXPANSIÓN DE EXPRESIONES INTERNAS:
-- expandimos llamadas a funciones Lam 
-- y Lets que tengan una variable o constante

-- Función auxiliar de expInt que expande las llamadas y los 
-- lets internos (no dependen de una Decl) recursivamente en un término.
expInt' ::  Term -> State (Bool, Int) Term
-- casos optimización
expInt' (App i (Lam ii name ty tm) (V k var))          = do change True
                                                            return $ subst (V k var) tm
expInt' (App i (Lam ii name ty tm) (Const k (CNat n))) = do change True
                                                            return $ subst (Const k (CNat n)) tm
expInt' (App i (Lam ii name ty tm) tm2)                = do newVar <- getNewVar 
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
expInt' (LetIn i name ty tm1 tm2)                     = do vname <- getNewVar 
                                                           b1 <- getChange
                                                           change False
                                                           tm1' <- expInt' tm1
                                                           b2 <- getChange
                                                           change $ b1 || b2
                                                           tm2' <- expInt' (open vname tm2)
                                                           if b2 then expInt' $ LetIn i name ty tm1' (close vname tm2') 
                                                                 else return $ LetIn i name ty tm1' (close vname tm2')                                                                                                    
expInt' (V i var)                                     = return $ V i var
expInt' (Const i c)                                   = return (Const i c)
expInt' (Lam i name ty tm)                            = do vname <- getNewVar 
                                                           tm'  <- expInt' (open vname tm)
                                                           return $ Lam i name ty (close vname tm')
expInt' (BinaryOp i op tm1 tm2)                       = do tm1' <- expInt' tm1
                                                           tm2' <- expInt' tm2
                                                           return $ BinaryOp i op tm1' tm2'
expInt' (IfZ i c tm1 tm2)                             = do c'   <- expInt' c
                                                           tm1' <- expInt' tm1
                                                           tm2' <- expInt' tm2
                                                           return $ IfZ i c' tm1' tm2'
expInt' (Fix i fname fty aname aty tm)                = do vaname <- getNewVar 
                                                           vfname <- getNewVar 
                                                           tm'  <-  expInt' (openN [vfname, vaname] tm)
                                                           return $ Fix i fname fty aname aty (closeN [vfname, vaname] tm')

-- Expande las llamadas y los lets internos (no dependen de una Decl)
-- en todas las declaraciones del programa.
expInt :: [Decl Term Ty] -> State (Bool, Int) [Decl Term Ty]
expInt [] = return []
expInt (d:ds) = do list <- expInt ds
                   expBody <- expInt' (declBody d)
                   return $ (Decl (declPos d) (declName d) (declType d) (expBody)) : list


---------------------------------------------------------------------
------------------------ BUCLE OPTIMIZACIONES -----------------------
---------------------------------------------------------------------

-- Máximo de veces que se llama al bucle de optimizaciones. 
-- Puede cambiarse según se prefiera.
maxIterations :: Int
maxIterations = 5

-- Ronda de optimizaciones, que aplica las siguientes optimizaciones
-- a una lista de declaraciones (programa):
--    1) Eliminación de deadCode.
--    2) Expansión de declaraciones.
--    3) Expansión interna.
--    4) Constant Folding.
optRound :: [Decl Term Ty] -> State (Bool, Int) [Decl Term Ty]
optRound ds = do dc   <- deadCode ds
                 expd <- expDecls dc
                 expi <- expInt expd
                 constFolding expi

-- Función auxiliar de optimize que lleva un contador de la cantidad
-- de "rondas de optimización" que se hicieron. Llama a optRound y cuando
-- sale se fija si se produjo algún cambio (con el Bool del estado de la
-- Mónada). Si no hubo cambios corta. Si hubo cambios llama a optRound de 
-- nuevo aumentando el contador mientras que este no haya llegado al máximo.
optimize' :: Int -> [Decl Term Ty] -> State (Bool, Int) [Decl Term Ty]
optimize' n ds | n == maxIterations = return ds
               | otherwise          = do res   <- optRound ds  
                                         (b,_) <- get
                                         if b then do change False
                                                      optimize' (n+1) res
                                              else return ds
                                       
-- Realiza la optimización de todo el programa. Función que se exporta.
optimize :: [Decl Term Ty] -> [Decl Term Ty]
optimize ds = fst $ runState (optimize' 0 ds) (False, 0)





