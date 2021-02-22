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
import Lang (freeVars)

isConst :: Term -> Bool
isConst (Const _ _) = True
isConst _           = False

getNat :: Const -> Int
getNat CNat n = n

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
usedDecls decls = foldr (++) [] (map freeVars decls) 

removeUnused :: [Decl Term Ty] -> [Name] -> [Decl Term Ty]
removeUnused [] names     = []
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

short :: [Decl Term Ty] -> Maybe (Decl Term Ty)
short []                        = Nothing
short (d:ds) | size d < maxSize = Just d
             | otherwise        = short ds


expansion' :: (Decl Term Ty) -> Term -> State Int Term
expansion' d tm1@(App i (V i (Free name)) tm2)  | name == (declName d) = App i (expansion' d tm1) (expansion' d tm2)
                                                | otherwise            = App i (expansion' d tm1) (expansion' d tm2)
expansion' d (App i tm1 tm2)                    = App i (expansion' d tm1) (expansion' d tm2)
expansion' d (V i var)                          = V i var
expansion' d (Const i c)                        = (Const i c)
expansion' d (Lam i name ty tm)                 = Lam i name ty (expansion' d tm)
expansion' d (BinaryOp i op tm1 tm2)            = BinaryOp i op (expansion' d tm1) (expansion' d tm2)
expansion' d (IfZ i c tm1 tm2)                  = IfZ i (expansion' d c) (expansion' d tm1) (expansion' d tm2)
expansion' d (Fix i fname fty aname aty tm)     = Fix i fname fty aname aty (expansion' d tm)
expansion' d (LetIn i name ty tm1 tm2)          = LetIn i name ty (expansion' d tm1) (expansion' d tm2)

expansion :: (Decl Term Ty) -> [Decl Term Ty] -> State Int [Decl Term Ty]
expansion decl [] = []
expansion decl (d:ds) | (declName decl) == (declName d) = expansion ds
                      | otherwise                       = 
                        (Decl (declPos d) (declName d) (declType d) (expansion' decl (declBody d))) : (expansion ds)
             

Buscar si se puede hacer más eficiente la búsqueda de Expresiones Comunes

let x = 5 in f x
let x = 5 in let varnueva = x in f varnueva
let x = 5 in let x = 8 in x +x
Varios argumentos?

f y = let x =8 in x +x

maxIterations :: Int
maxIterations = 5

0) Exp duplicadas
1) Deadcode + funcion no usada
2) Expansion
3) ConstFolding

round :: [Decl Term Ty] -> [Decl Term Ty]
round ds = constFolding $ expansion $ deadCode $ duplExp ds

optimize' :: Int -> [Decl Term Ty] -> [Decl Term Ty]
optimize' n ds | n == maxIterations = ds
               | otherwise          = let res = round ds in 
                                        if res == ds then ds else optimize' (n+1) res
                                        -- == MUY COSTOSO? VALE LA PENA UNA MONADA DONDE TENGA INFO DE SI SE MODIFICO ALGO?

optimize :: [Decl Term Ty] -> [Decl Term Ty]
optimize ds = optimize' 0 ds

- Expansión: Ver lo de la sustitucion en el let (variables ligadas y no ligadas).
- duplExp: Buscar el algoritmo para implementar.