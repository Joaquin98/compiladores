{-|
Module      : Elab
Description : Elabora un término fully named a uno locally closed.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

Este módulo permite elaborar términos y declaraciones para convertirlas desde
fully named (@NTerm) a locally closed (@Term@) 
-}

module Elab ( elab, elab_decl ) where

import Lang
import Subst

{-

elabType :: MonadPCF m => m(SDecl SMNTerm MultiBind STy) -> m(SDecl SMNTerm MultiBind NTy)
elabType = do decl <- 

              return 

{-
data STy = 
      DTy Name -- Declaracion de tipo
    | SNatTy 
    | SFunTy STy STy
    deriving (Show,Eq)
-}

convertType :: STy -> NTy
convertType (DTy name) = Ntype "" NatTy
convertType SNatTy = Ntype "" NatTy
convertType  = do

-}
{-
data SDecl term bind ty =
    DTer Pos Name [(bind, ty)] STy Bool term 
  | DType Pos Name ty
  deriving (Show,Functor)
-}

expBindersD :: SDecl SMNTerm MultiBind STy -> SDecl SMNTerm UnaryBind STy
expBindersD (DTer i name bs ty b term) = DTer i name (binderExp bs) ty b term
expBindersD (DType i name ty) = DType i name ty



desugarD :: SDecl SMNTerm UnaryBind STy -> Either (SDecl SMNTerm UnaryBind STy) (Decl SMNTerm STy)
desugarD (DTer i name [] ty False term) = Right (Decl i name ty term)
desugarD (DTer i name b:bs ty False term) = let ts = getFunType (b:bs) 
                                            in Right (Decl i name (SFunTy ts ty) (SLam i (b:bs) term))
  
desugarD (DTer i name [(v, t)] ty True term) = Right (Decl i name (SFunTy t ty) (SFix i name (SFunTy t ty) v t term))
desugarD (DTer i name b:bs ty True term) = let ts = getFunType bs
                                               new = (DTer i name [b] (SFunTy ts ty) True (SLam i bs term)) 
                                           in  desugar new

desugarD d@(DType i name ty) = Left d


- parser :  [SDecl SMNTerm MultiBind STy]
- expBinders : [SDecl SMNTerm UnaryBind STy]
- desugarD : [Either (SDecl SMNTerm UnaryBind STy) (Decl SMNTerm STy)]
- 


binderExp :: [([Name], Ty)] -> [(Name, Ty)]
binderExp [] = []
binderExp (([x], t):bs) = (x, t): (binderExp bs)
binderExp ((x:xs, t):bs) = (x, t): (binderExp ((xs, t) : bs))

expBinders :: MNTerm -> UNTerm
expBinders (SV info var) = SV info var
expBinders (SConst info const) = SConst info const
expBinders (SLam info binds t) = SLam info (binderExp binds) (expBinders t)
expBinders (SApp info t1 t2) = SApp info (expBinders t1) (expBinders t2)
expBinders (SUnaryOpApp info op t) = SUnaryOpApp info op (expBinders t)
expBinders (SUnaryOp info unaryOp) = SUnaryOp info unaryOp  
expBinders (SFix info n1 t1 n2 t2 t) = SFix info n1 t1 n2 t2 (expBinders t)
expBinders (SIfZ info c t1 t2) = SIfZ info (expBinders c) (expBinders t1) (expBinders t2)
expBinders (SLetIn info name binds ty t t') = SLetInFun info name (binderExp binds) ty (expBinders t) (expBinders t')
expBinders (SRec info name binds ty t t') = SRec info name (binderExp binds) ty (expBinders t) (expBinders t')


getFunType :: [(Name, Ty)] -> Ty
getFunType [(n, t)] = t
getFunType ((n, t):bs) = FunTy t (getFunType bs)



desugar :: UNTerm -> NTerm
desugar (SV info var) = V info var 
desugar (SConst info c) = Const info c 
desugar (SLam info binds t) = foldr (\(name,ty) ti -> Lam info name ty ti) (desugar t) binds 
desugar (SApp info t1 t2) = App info (desugar t1) (desugar t2)
desugar (SUnaryOpApp info op t) = UnaryOp info op (desugar t)
desugar (SUnaryOp info op) = Lam info "x" NatTy (UnaryOp info op (V info "x"))
desugar (SFix info n1 t1 n2 t2 t) = Fix info n1 t1 n2 t2 (desugar t)
desugar (SIfZ info c t1 t2) = IfZ info (desugar c) (desugar t1) (desugar t2)
desugar (SLetIn info name [] ty t t') = App info (Lam info name ty (desugar t')) (desugar t)
desugar (SLetIn info name binds ty t t') = let funTy = (FunTy (getFunType binds) ty) 
                                                  newTerm = SLetIn info name funTy (SLam info binds t) t'                                         
                                              in desugar newTerm
desugar (SRec info name [(ni,ti)] ty t t') = let fixTerm = SFix info name (FunTy ti ty) ni ti t 
                                                 newTerm = SLetIn info name (FunTy ti ty) fixTerm t'
                                               in desugar newTerm 
desugar (SRec info name binds ty t t') = let funTy = (FunTy (getFunType (tail binds)) ty) 
                                             newTerm = SRec info name [head binds] funTy (SLam info (tail binds) t) t' 
                                         in desugar newTerm
                                            


-- | 'elabLN' transforma variables ligadas en índices de de Bruijn
-- en un término dado. 
elabLN :: NTerm -> Term
elabLN (V p v)               = V p (Free v)
elabLN (Const p c)           = Const p c
elabLN (Lam p v ty t)        = Lam p v ty (close v (elabLN t))
elabLN (App p h a)           = App p (elabLN h) (elabLN a)
elabLN (Fix p f fty x xty t) = Fix p f fty x xty (closeN [f, x] (elabLN t))
elabLN (IfZ p c t e)         = IfZ p (elabLN c) (elabLN t) (elabLN e)
elabLN (UnaryOp i o t)       = UnaryOp i o (elabLN t)

elab_decl :: Decl MNTerm -> Decl Term
elab_decl = fmap elab

elab :: MNTerm -> Term
elab = elabLN . desugar . expBinders
