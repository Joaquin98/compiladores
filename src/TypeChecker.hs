{-|
Module      : Typechecker
Description : Chequeo de tipos de términos y declaraciones.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}
module TypeChecker (
   tc,
   tcDecl 
   ) where

import Lang
import Global
import MonadPCF
import PPrint
import Subst


-- | 'tc' chequea y devuelve el tipo de un término 
-- Si el término no está bien tipado, lanza un error
-- usando la interfaz de las mónadas @MonadPCF@.
tc :: MonadPCF m => Term         -- ^ término a chequear
                 -> [(Name,Ty)]  -- ^ entorno de tipado
                 -> m Ty         -- ^ tipo del término
tc (V p (Bound _)) _ = failPosPCF p "typecheck: No deberia haber variables Bound"
tc (V p (Free n)) bs = case lookup n bs of
                           Nothing -> failPosPCF p $ "Variable no declarada "++ppName n
                           Just ty -> return ty 
tc (Const _ (CNat n)) _ = return NatTy
tc (UnaryOp p u t) bs = do 
      ty <- tc t bs
      expect NatTy ty t
tc (IfZ p c t t') bs = do
       tyc  <- tc c bs
       expect NatTy tyc c
       tyt  <- tc t bs
       tyt' <- tc t' bs
       expect tyt tyt' t'
tc (Lam p v (NTy n ty) t) bs = tc (Lam p v ty t) bs
tc (Lam p v ty t) bs = do
         ty' <- tc (open v t) ((v,ty):bs)
         return (FunTy ty ty')
tc (App p t u) bs = do
         tyt <- tc t bs
         (dom,cod) <- domCod t tyt
         tyu <- tc u bs
         expect dom tyu u
         return cod
tc (Fix p f (NTy n fty) x xty t) bs = tc (Fix p f fty x xty t) bs
tc (Fix p f fty x (NTy n xty) t) bs = tc (Fix p f fty x xty t) bs         
tc (Fix p f fty x xty t) bs = do
         (dom, cod) <- domCod (V p (Free f)) fty
         when (not (eq dom xty)) $ do
           failPosPCF p "El tipo del argumento de un fixpoint debe coincidir con el \
                        \dominio del tipo de la función" 
         let t' = openN [f, x] t
         ty' <- tc t' ((x,xty):(f,fty):bs)
         expect cod ty' t'
         return fty

--Nat
--NTy "hola" (Nat)

eq :: Ty -> Ty -> Bool 
eq NatTy NatTy = True
eq (FunTy t1 t2) (FunTy t1' t2') = (eq t1 t1') && (eq t2 t2')  
eq (NTy _ t1) t2 = eq t1 t2
eq t1 (NTy _ t2) = eq t1 t2 
eq _ _ = False

-- | @'typeError' t s@ lanza un error de tipo para el término @t@ 
typeError :: MonadPCF m => Term   -- ^ término que se está chequeando  
                        -> String -- ^ mensaje de error
                        -> m a
typeError t s = failPosPCF (getInfo t) $ "Error de tipo en "++pp t++"\n"++s
 
-- | 'expect' chequea que el tipo esperado sea igual al que se obtuvo
-- y lanza un error si no lo es.
expect :: MonadPCF m => Ty    -- ^ tipo esperado
                     -> Ty    -- ^ tipo que se obtuvo
                     -> Term  -- ^ término que se está chequeando
                     -> m Ty
expect ty ty' t = if (eq ty ty') then return ty 
                               else typeError t $ 
              "Tipo esperado: "++ ppTy ty
            ++"\npero se obtuvo: "++ ppTy ty'

-- | 'domCod chequea que un tipo sea función
-- | devuelve un par con el tipo del dominio y el codominio de la función
domCod :: MonadPCF m => Term -> Ty -> m (Ty, Ty)
domCod t (FunTy d c) = return (d, c)
domCod t ty = typeError t $ "Se esperaba un tipo función, pero se obtuvo: " ++ ppTy ty

-- | 'tcDecl' chequea el tipo de una declaración
-- y la agrega al entorno de tipado de declaraciones globales
tcDecl :: MonadPCF m  => Decl Term Ty -> m ()
tcDecl (Decl p n (NTy _ ty) t) = tcDecl (Decl p n ty t)
tcDecl (Decl p n ty t) = do
    --chequear si el nombre ya está declarado
    mty <- lookupTy n
    case mty of
        Nothing -> do  --no está declarado 
                  s <- get
                  tyR <- tc t (tyEnv s)
                  if (eq ty tyR)
                  then addTy n ty
                  else failPosPCF p $ n ++" error de tipo: se esperaba " ++ show(ty) ++ " se encontro " ++ show(tyR)
        Just _  -> failPosPCF p $ n ++" ya está declarado"
