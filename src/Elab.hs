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

binderExp :: [([Name], Ty)] -> [(Name, Ty)]
binderExp [] = []
binderExp (([x], t):bs) = (x, t): (binderExp bs)
binderExp ((x:xs, t):bs) = (x, t): (binderExp ((xs, t) : bs))

expandBinders :: MNTerm -> UNTerm
expandBinders (SV info var) = SV info var
expandBinders (SConst info const) = SConst info const
expandBinders (SLam info binds t) = SLam info (binderExp binds) (expandBinders t)
expandBinders (SApp info t1 t2) = SApp info (expandBinders t1) (expandBinders t2)
expandBinders (SUnaryOpApp info op t) = SUnaryOpApp info op (expandBinders t)
expandBinders (SUnaryOp info unaryOp) = SUnaryOp info unaryOp  
expandBinders (SFix info n1 t1 n2 t2 t) = SFix info n1 t1 n2 t2 (expandBinders t)
expandBinders (SIfZ info c t1 t2) = SIfZ info (expandBinders c) (expandBinders t1) (expandBinders t2)
expandBinders (SLetIn info name binds ty t t') = SLetInFun info name (binderExp binds) ty (expandBinders t) (expandBinders t')
expandBinders (SRec info name binds ty t t') = SRec info name (binderExp binds) ty (expandBinders t) (expandBinders t')


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
elab = elabLN . desugar . expandBinders
