module Optimizations where
import Lang 
import Control.Monad.State
import Subst
import Common
{-
data Tm info var ty = 
    V info var
  | Const info Const
  | Lam info Name ty (Tm info var ty)
  | App info (Tm info var ty) (Tm info var ty)
  | BinaryOp info BinaryOp (Tm info var ty) (Tm info var ty)
  | Fix info Name ty Name ty (Tm info var ty)
  | IfZ info (Tm info var ty) (Tm info var ty) (Tm info var ty)
  | LetIn info Name ty (Tm info var ty) (Tm info var ty)
  deriving (Show, Functor)
-}

isConst :: Term -> Bool
isConst (Const _ _) = True
isConst _           = False

getNat :: Const -> Int
getNat (CNat n) = n

reduce :: Term -> Term
reduce (BinaryOp i Add x (Const _ (CNat 0)))       = x
reduce (BinaryOp i Add (Const _ (CNat 0)) x)       = x
reduce (BinaryOp i Diff x (Const _ (CNat 0)))      = x
reduce (BinaryOp i Diff (Const _ (CNat 0)) x)      = (Const i (CNat 0))
reduce (BinaryOp i Add (Const _ c1) (Const _ c2))  = (Const i (CNat ((getNat c1) + (getNat c2))))
reduce (BinaryOp i Diff (Const _ c1) (Const _ c2)) = (Const i (CNat ((getNat c1) - (getNat c2))))
reduce x = x

-- x + 0 = 0
-- 0 + x = x
-- x - 0 = x
-- 4 + 5 = 9
-- 5 - 3 = 2
constFolding' :: Term -> Term
-- casos optimizacion
constFolding' (BinaryOp i op tm1 tm2)        = reduce $ BinaryOp i op (constFolding' tm1) (constFolding' tm2)
-- casos recursivos
constFolding' (V i var)                      = V i var
constFolding' (Const i c)                    = (Const i c)
constFolding' (Lam i name ty tm)             = Lam i name ty (constFolding' tm)
constFolding' (App i tm1 tm2)                = App i (constFolding' tm1) (constFolding' tm2)
constFolding' (Fix i fname fty aname aty tm) = Fix i fname fty aname aty (constFolding' tm)
constFolding' (IfZ i c tm1 tm2)              = IfZ i (constFolding' c) (constFolding' tm1) (constFolding' tm2)
constFolding' (LetIn i name ty tm1 tm2)      = LetIn i name ty (constFolding' tm1) (constFolding' tm2)

constFolding :: [Decl Term Ty] -> [Decl Term Ty]
constFolding [] = []
constFolding (d:ds) = 
  (Decl (declPos d) (declName d) (declType d) (constFolding' (declBody d))) : (constFolding ds)



usedDecls :: [Decl Term Ty] -> [Name]
usedDecls decls = foldr (++) [] (map (freeVars . declBody) decls) 

removeUnused :: [Decl Term Ty] -> [Name] -> [Decl Term Ty]
removeUnused [x] names    = [x]
removeUnused (d:ds) names = case elem (declName d) names of
                                False -> removeUnused ds names
                                True  -> d : (removeUnused ds names)

deadDecls :: [Decl Term Ty] -> [Decl Term Ty]
deadDecls ds = removeUnused ds (usedDecls ds)

-- ifz 0 then t1 else t2 = t1
-- ifz n then t1 else t2 = t2
deadCode' :: Term -> Term
-- casos optimizacion
deadCode' (IfZ i c tm1 tm2) = let c'   = deadCode' c
                                  tm1' = deadCode' tm1
                                  tm2' = deadCode' tm2
                                in case c' of
                                    Const _ (CNat 0) -> tm1'
                                    Const _ (CNat n) -> tm2'
                                    _                -> IfZ i c' tm1' tm2' 
-- casos recursivos
deadCode' (V i var)                      = V i var
deadCode' (Const i c)                    = (Const i c)
deadCode' (Lam i name ty tm)             = Lam i name ty (deadCode' tm)
deadCode' (App i tm1 tm2)                = App i (deadCode' tm1) (deadCode' tm2)
deadCode' (BinaryOp i op tm1 tm2)        = BinaryOp i op (deadCode' tm1) (deadCode' tm2)
deadCode' (Fix i fname fty aname aty tm) = Fix i fname fty aname aty (deadCode' tm)
deadCode' (LetIn i name ty tm1 tm2)      = LetIn i name ty (deadCode' tm1) (deadCode' tm2)

deadIf :: [Decl Term Ty] -> [Decl Term Ty]
deadIf []     = []
deadIf (d:ds) = 
  (Decl (declPos d) (declName d) (declType d) (deadCode' (declBody d))) : (deadIf ds)


deadCode :: [Decl Term Ty] -> [Decl Term Ty]
deadCode ds = deadDecls (deadIf ds)

{-
2) ConstFolding
3) DeadCode

1) Expansion

3) DeadCode
2) ConstFolding
4) ConstFolding
5) Epresiones Comunesx

0) Exp duplicadas
1) Deadcode + funcion no usada
2) Expansion
3) ConstFolding

Hasta que no se haga ninguna modificación o hasta un limite de ciclos.

4) Deadcode
5) ConstFolding
-}


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


-- Fijarse que se use el name
getNewVar :: String -> State Int String
getNewVar name = do n <- get
                    modify (\n -> (n+1))
                    return $ "linexp" ++ name ++ show(n)

-- Fix.
-- Varias variables.
-- Expandir Lam que también solucionaría el tema de varias varibles.
-- Los let tiene nombres iguales si se hacen en distintas llamadas a expansion.
expDecls' :: (Decl Term Ty) -> Term -> State Int Term
expDecls' d (App i (V j (Free name)) (V k var))          | name == (declName d) = case (declBody d) of
                                                                                    (Lam _ nameVar ty tm) -> return $ subst (V k var) tm
                                                                                    fix                   -> return fix
expDecls' d (App i (V j (Free name)) (Const k (CNat n))) | name == (declName d) = case (declBody d) of
                                                                                    (Lam _ nameVar ty tm) -> return $ subst (Const k (CNat n)) tm
                                                                                    fix                   -> return fix
expDecls' d (App i (V j (Free name)) tm2)                | name == (declName d) = do newVar <- getNewVar ""
                                                                                     case (declBody d) of 
                                                                                       (Lam k nameVar ty tm) -> (expDecls' d (LetIn NoPos newVar ty tm2 tm))
                                                                                       fix                   -> return fix  
expDecls' d (V i (Free name))                            | name == (declName d) = return (declBody d)                                                                                                   
expDecls' d (App i tm1 tm2)                = do tm1' <- expDecls' d tm1
                                                tm2' <- expDecls' d tm2
                                                return $ App i tm1' tm2'
expDecls' d (V i var)                      = return $ V i var
expDecls' d (Const i c)                    = return (Const i c)
expDecls' d (Lam i name ty tm)             = do tm'  <- expDecls' d tm
                                                return $ Lam i name ty tm'
expDecls' d (BinaryOp i op tm1 tm2)        = do tm1' <- expDecls' d tm1
                                                tm2' <- expDecls' d tm2
                                                return $ BinaryOp i op tm1' tm2'
expDecls' d (IfZ i c tm1 tm2)              = do c'   <- expDecls' d c
                                                tm1' <- expDecls' d tm1
                                                tm2' <- expDecls' d tm2
                                                return $ IfZ i c' tm1' tm2'
expDecls' d (Fix i fname fty aname aty tm) = do tm'  <- expDecls' d tm
                                                return $ Fix i fname fty aname aty tm'
expDecls' d (LetIn i name ty tm1 tm2)      = do tm1' <- expDecls' d tm1
                                                tm2' <- expDecls' d tm2
                                                return $ LetIn i name ty tm1' tm2'

expDecls :: (Decl Term Ty) -> [Decl Term Ty] -> State Int [Decl Term Ty]
expDecls decl [] = return []
expDecls decl (d:ds) | (declName decl) == (declName d)  = expDecls decl ds
                     | otherwise                        = do list <- expDecls decl ds
                                                             expBody <- expDecls' decl (declBody d)
                                                             return $ (Decl (declPos d) (declName d) (declType d) (expBody)) : list



expInt' ::  Term -> State Int Term
expInt' (App i (Lam _ name ty tm) (V k var))          = return $ subst (V k var) (tm)
expInt' (App i (Lam _ name ty tm) (Const k (CNat n))) = return $ subst (Const k (CNat n)) (tm)
expInt' (App i (Lam _ name ty tm) tm2)                = do newVar <- getNewVar ""
                                                           expInt' (LetIn NoPos newVar ty tm2 tm)                                                                       
expInt' (App i tm1 tm2)                               = do tm1' <- expInt' tm1
                                                           tm2' <- expInt' tm2
                                                           return $ App i tm1' tm2'
expInt' (LetIn i name ty (V k var) tm2)               = return $ subst (V k var) (tm2)
expInt' (LetIn i name ty (Const k (CNat n)) tm2)      = return $ subst (Const k (CNat n)) (tm2)
expInt' (LetIn i name ty tm1 tm2)                     = do tm1' <- expInt' tm1
                                                           tm2' <- expInt' tm2
                                                           return $ LetIn i name ty tm1' tm2'
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


expInt :: [Decl Term Ty] -> State Int [Decl Term Ty]
expInt [] = return []
expInt (d:ds) = do list <- expInt ds
                   expBody <- expInt' (declBody d)
                   return $ (Decl (declPos d) (declName d) (declType d) (expBody)) : list




{-}
Buscar si se puede hacer más eficiente la búsqueda de Expresiones Comunes

let x = 5 in f x
let x = 5 in let varnueva = x in f varnueva
let x = 5 in let x = 8 in x +x
Varios argumentos?

f y = let x =8 in x + x



0) Exp duplicadas
1) Deadcode + funcion no usada
2) Expansion
3) ConstFolding
-}

maxIterations :: Int
maxIterations = 20

expansionD :: [Decl Term Ty] -> [Decl Term Ty]
expansionD ds = case shortDecl ds of
                      Nothing -> ds
                      Just d  -> expansionD $ fst (runState (expDecls d ds) 0) 

expansionI :: [Decl Term Ty] -> [Decl Term Ty]
expansionI ds = fst (runState (expInt ds) 0) 

optRound :: [Decl Term Ty] -> [Decl Term Ty]
optRound ds = constFolding $ expansionI $ expansionD $ deadCode ds




optimize' :: Int -> [Decl Term Ty] -> [Decl Term Ty]
optimize' n ds | n == maxIterations = ds
               | otherwise          = let res = optRound ds in 
                                        if res == ds then ds else optimize' (n+1) res
                                        -- == MUY COSTOSO? VALE LA PENA UNA MONADA DONDE TENGA INFO DE SI SE MODIFICO ALGO?




optimize :: [Decl Term Ty] -> [Decl Term Ty]
optimize ds = optimize' 0 ds

{-}
- Expansión: Ver lo de la sustitucion en el let (variables ligadas y no ligadas).
- duplExp: Buscar el algoritmo para implementar.
-}

-- Muy costoso ==.
-- Ver si se pierde el numero de los let.