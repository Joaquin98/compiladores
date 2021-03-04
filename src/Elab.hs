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

module Elab ( elab, elabD, rmvSynTerm, styToTy) where

import Lang
import Subst
import MonadPCF
import Common

{-
convertType :: STy -> NTy
convertType (DTy name) = Ntype "" NatTy
convertType SNatTy = Ntype "" NatTy
convertType  = do
-}

-- Reemplaza los sinonimos de tipos en un termino por lo que representa. 
rmvSynTerm :: MonadPCF m => SNTerm -> m (NTerm)
rmvSynTerm (V i name)                = return (V i name)  
rmvSynTerm (Const i c)               = return (Const i c)  
rmvSynTerm (Lam i name ty t)         = do nty  <- styToTy ty
                                          tTy  <- rmvSynTerm t
                                          return (Lam i name nty tTy)  
rmvSynTerm (App i t1 t2)             = do t1Ty <- rmvSynTerm t1
                                          t2Ty <- rmvSynTerm t2
                                          return (App i t1Ty t2Ty)
--rmvSynTerm (UnaryOp i op t) = do tTy <- rmvSynTerm t
--                                 return (UnaryOp i op tTy)
rmvSynTerm (BinaryOp i op t1 t2)     = do t1Ty <- rmvSynTerm t1
                                          t2Ty <- rmvSynTerm t2
                                          return (BinaryOp i op t1Ty t2Ty)                                   
rmvSynTerm (Fix i n1 sty1 n2 sty2 t) = do ty1  <- styToTy sty1
                                          ty2  <- styToTy sty2  
                                          tTy  <- rmvSynTerm t
                                          return (Fix i n1 ty1 n2 ty2 tTy)
rmvSynTerm (IfZ i c t1 t2)           = do cTy  <- rmvSynTerm c
                                          t1Ty <- rmvSynTerm t1
                                          t2Ty <- rmvSynTerm t2
                                          return (IfZ i cTy t1Ty t2Ty)
rmvSynTerm (LetIn i n ty t t')       = do nty  <- styToTy ty
                                          tTy  <- rmvSynTerm t
                                          tTy' <- rmvSynTerm t'
                                          return (LetIn i n nty tTy tTy')

{-}
-- Transforma una lista de binders con tipos con sinonimos en una con
-- tipos sin sinonimos.
rmvSynBinds :: MonadPCF m => [(MultiBind,STy)] -> m ([(MultiBind,Ty)])
rmvSynBinds [] = return [] 
rmvSynBinds ((ns,sty):bs) = do ty <- styToTy sty
                               bsTy <- rmvSynBinds bs  
                               return ((ns,ty):bsTy)
-}

-- Convierte un tipo con sinonimos en uno sin sinonimos.
styToTy :: MonadPCF m => STy -> m (Ty)
styToTy (DTy name)         = do mty <- lookupSynTy name
                                case mty of 
                                    Nothing -> failPosPCF NoPos $ name ++" no existe el tipo"
                                    Just ty ->  return (NTy name ty)
styToTy (SNatTy)           = return NatTy
styToTy (SFunTy sty1 sty2) = do ty1 <- (styToTy sty1)
                                ty2 <- (styToTy sty2)
                                return (FunTy ty1 ty2)


binderExp :: [([Name], a)] -> [(Name, a)]
binderExp []             = []
binderExp (([x], t):bs)  = (x, t): (binderExp bs)
binderExp ((x:xs, t):bs) = (x, t): (binderExp ((xs, t) : bs))

expBindersD :: SDecl SMNTerm MultiBind STy -> SDecl SMNTerm UnaryBind STy
expBindersD (DTer i name bs ty b term) = DTer i name (binderExp bs) ty b term
expBindersD (DType i name ty)          = DType i name ty

unaryToMulti :: [(UnaryBind, a)] -> [(MultiBind, a)] 
unaryToMulti = map (\(ni,ti)->([ni],ti))

desugarD :: SDecl SMNTerm UnaryBind STy -> Either (SDecl SMNTerm UnaryBind STy) (Decl SMNTerm STy)
desugarD (DTer i name [] ty False term)      = Right (Decl i name ty term)
desugarD (DTer i name (b:bs) ty False term)  = let ts = getFunType (b:bs) ty
                                                in Right (Decl i name ts (SLam i (unaryToMulti(b:bs)) term))
  
desugarD (DTer i name [(v, t)] ty True term) = Right (Decl i name (SFunTy t ty) (SFix i name (SFunTy t ty) v t term))
desugarD (DTer i name (b:bs) ty True term)   = let ts = getFunType bs ty
                                                   new = (DTer i name [b] ts True (SLam i (unaryToMulti bs) term)) 
                                                in  desugarD new

desugarD d@(DType i name ty) = Left d


elabD :: SDecl SMNTerm MultiBind STy -> Either (SDecl SMNTerm UnaryBind STy) (Decl SMNTerm STy)
elabD = desugarD . expBindersD 

{-
- parser :  [SDecl SMNTerm MultiBind STy]
- expBinders : [SDecl SMNTerm UnaryBind STy]
- desugarD : [Either (SDecl SMNTerm UnaryBind STy) (Decl SMNTerm STy)]
-}


expBinders :: SMNTerm -> SUNTerm
expBinders (SV info var)                    = SV info var
expBinders (SConst info const)              = SConst info const
expBinders (SLam info binds t)              = SLam info (binderExp binds) (expBinders t)
expBinders (SApp info t1 t2)                = SApp info (expBinders t1) (expBinders t2)
expBinders (SUnaryOpApp info op t)          = SUnaryOpApp info op (expBinders t)
expBinders (SUnaryOp info unaryOp)          = SUnaryOp info unaryOp  
expBinders (SBinaryOp info binaryop t1 t2)  = SBinaryOp info binaryop (expBinders t1) (expBinders t2) 
expBinders (SFix info n1 t1 n2 t2 t)        = SFix info n1 t1 n2 t2 (expBinders t)
expBinders (SIfZ info c t1 t2)              = SIfZ info (expBinders c) (expBinders t1) (expBinders t2)
expBinders (SLetIn info name binds ty t t') = SLetIn info name (binderExp binds) ty (expBinders t) (expBinders t')
expBinders (SRec info name binds ty t t')   = SRec info name (binderExp binds) ty (expBinders t) (expBinders t')

{-
getFunType :: [(Name, Ty)] -> Ty
getFunType [(n, t)] = t
getFunType ((n, t):bs) = FunTy t (getFunType bs)
-}

getFunType :: [(Name, STy)] -> STy -> STy
getFunType [(n, t)] ty    = SFunTy t ty
getFunType ((n, t):bs) ty = SFunTy t (getFunType bs ty)


unaryToBinary :: UnaryOp -> BinaryOp
unaryToBinary Succ = Add
unaryToBinary Pred = Diff 

desugar :: SUNTerm -> SNTerm
desugar (SV info var)                      = V info var 
desugar (SConst info c)                    = Const info c 
desugar (SLam info binds t)                = foldr (\(name,ty) ti -> Lam info name ty ti) (desugar t) binds 
desugar (SApp info t1 t2)                  = App info (desugar t1) (desugar t2)
desugar (SUnaryOpApp info op t)            = BinaryOp info (unaryToBinary op) (desugar t) (Const info (CNat 1))
desugar (SUnaryOp info op)                 = Lam info "x" SNatTy (desugar (SUnaryOpApp info op (SV info "x")))
desugar (SBinaryOp info op t1 t2)          = BinaryOp info op (desugar t1) (desugar t2) 
desugar (SFix info n1 t1 n2 t2 t)          = Fix info n1 t1 n2 t2 (desugar t)
desugar (SIfZ info c t1 t2)                = IfZ info (desugar c) (desugar t1) (desugar t2)
desugar (SLetIn info name [] ty t t')      = LetIn info name ty (desugar t) (desugar t')
desugar (SLetIn info name binds ty t t')   = let funTy   = (getFunType binds ty) 
                                                 newTerm = SLetIn info name [] funTy (SLam info binds t) t'                                         
                                              in desugar newTerm
desugar (SRec info name [(ni,ti)] ty t t') = let fixTerm = SFix info name (SFunTy ti ty) ni ti t 
                                                 newTerm = SLetIn info name [] (SFunTy ti ty) fixTerm t'
                                              in desugar newTerm 
desugar (SRec info name binds ty t t')     = let funTy   = (getFunType (tail binds) ty) 
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
--elabLN (UnaryOp i o t)       = UnaryOp i o (elabLN t)
elabLN (BinaryOp i o t1 t2)  = BinaryOp i o (elabLN t1)(elabLN t2)
elabLN (LetIn p n ty t t')   = LetIn p n ty (elabLN t) (close n (elabLN t'))


elab :: MonadPCF m => SMNTerm -> m Term
elab t = let t1 = expBinders t      -- SUNTerm
             t2 = desugar t1        -- SNTerm
         in do t3 <- rmvSynTerm t2  -- NTerm
               return (elabLN t3)   -- Term