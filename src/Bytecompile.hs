{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : Byecompile
Description : Compila a bytecode. Ejecuta bytecode.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

Este módulo permite compilar módulos a la BVM. También provee una implementación de la BVM 
para ejecutar bytecode.
-}
module Bytecompile
  (Bytecode, runBC, bcWrite, bcRead) -- bytecompileModule
 where

import Lang 
import Subst
import MonadPCF

import qualified Data.ByteString.Lazy as BS hiding (putStrLn)
import Data.Binary ( Word32, Binary(put, get), decode, encode )
import Data.Binary.Put ( putWord32le )
import Data.Binary.Get ( getWord32le, isEmpty )

type Opcode = Int
type Bytecode = [Int]

newtype Bytecode32 = BC { un32 :: [Word32] }

{- Esta instancia explica como codificar y decodificar Bytecode de 32 bits -}
instance Binary Bytecode32 where
  put (BC bs) = mapM_ putWord32le bs
  get = go 
    where go =  
           do
            empty <- isEmpty
            if empty
              then return $ BC []
              else do x <- getWord32le
                      BC xs <- go
                      return $ BC (x:xs)

{- Estos sinónimos de patrón nos permiten escribir y hacer
pattern-matching sobre el nombre de la operación en lugar del código
entero, por ejemplo:
 
   f (CALL : cs) = ...

 Notar que si hubieramos escrito algo como
   call = 5
 no podríamos hacer pattern-matching con `call`.

 En lo posible, usar estos códigos exactos para poder ejectutar un
 mismo bytecode compilado en distintas implementaciones de la máquina.
-}
pattern RETURN   = 1
pattern CONST    = 2
pattern ACCESS   = 3
pattern FUNCTION = 4
pattern CALL     = 5
pattern SUCC     = 6
pattern PRED     = 7
pattern IFZ      = 8
pattern FIX      = 9
pattern STOP     = 10
pattern JUMP     = 11
pattern SHIFT    = 12
pattern DROP     = 13
pattern PRINT    = 14

type Env = [Val]
data Val = I Int | Fun Env Bytecode | RA Env Bytecode deriving (Show)

-- C(ifz c t1 t2) = C(t2); C(t1); C(c); IFZ;
-- <IFZ:k | e | c:t1:t2:s> -> <k|e|t1:s>

{-
data Tm info var ty = 
    V info var
  | Const info Const
  | Lam info Name ty (Tm info var ty)
  | App info (Tm info var ty) (Tm info var ty)
  | UnaryOp info UnaryOp (Tm info var ty)
  | Fix info Name ty Name ty (Tm info var ty)
  | IfZ info (Tm info var ty) (Tm info var ty) (Tm info var ty)
  deriving (Show, Functor)
-}

bc :: MonadPCF m => Term -> m Bytecode
bc (V _ (Bound i))    = return [ACCESS,i] 
bc (Const _ (CNat n)) = return [CONST,n]
bc (Lam _ _ _ t)      = do tc <- bc t
                           return ([FUNCTION, (length tc) + 1] ++ tc ++ [RETURN])  
bc (App _ f e)        = do fc <- bc f
                           ec <- bc e
                           return (fc ++ ec ++ [CALL]) 
bc (UnaryOp _ Succ t) = do t' <- bc t
                           return (t' ++ [SUCC])
bc (UnaryOp _ Pred t) = do t' <- bc t
                           return (t' ++ [PRED])
bc (Fix _ _ _ _ _ t)  = do tc <- bc t
                           return ([FUNCTION, (length tc) + 1] ++ tc ++ [RETURN,FIX])  
bc (IfZ _ c t1 t2)    = do t2c <- bc t2
                           t1c <- bc t1
                           cc  <- bc c
                           return (t2c ++ t1c ++ cc ++ [IFZ])


--bytecompileModule :: MonadPCF m => Module -> m Bytecode
--bytecompileModule mod = error "implementame"

-- | Toma un bytecode, lo codifica y lo escribe un archivo 
bcWrite :: Bytecode -> FilePath -> IO ()
bcWrite bs filename = BS.writeFile filename (encode $ BC $ fromIntegral <$> bs)

---------------------------
-- * Ejecución de bytecode
---------------------------

-- | Lee de un archivo y lo decodifica a bytecode
bcRead :: FilePath -> IO Bytecode
bcRead filename = map fromIntegral <$> un32  <$> decode <$> BS.readFile filename

runBC :: MonadPCF m => Bytecode -> m ()
runBC c = runBC' c [] []

runBC' :: MonadPCF m => Bytecode -> [Val] -> [Val] -> m ()
runBC' (STOP:c) _ _                   = return ()
runBC' (PRINT:c) e (v:s)              = do printPCF (show v)
                                           runBC' c e s
runBC' (ACCESS:i:c) e s               = runBC' c e ((e!!i):s)
runBC' (CONST:n:c) e s                = runBC' c e ((I n):s)
runBC' (FUNCTION:len:c) e s           = runBC' (drop len c) e ((Fun e c):s)
runBC' (RETURN:c) e (v:(Fun re ra):s) = runBC' ra re (v:s)
runBC' (CALL:c) e (v:(Fun fe fc):s)   = runBC' fc (v:fe) ((RA e c):s)  
runBC' (SUCC:c) e ((I n):s)           = runBC' c e ((I (n+1)):s)
runBC' (PRED:c) e ((I 0):s)           = runBC' c e ((I 0):s) 
runBC' (PRED:c) e ((I n):s)           = runBC' c e ((I (n-1)):s) 
runBC' (IFZ:c) e ((I 0):v1:v2:s)      = runBC' c e (v1:s)
runBC' (IFZ:c) e ((I n):v1:v2:s)      = runBC' c e (v2:s)
runBC' (FIX:c) e ((Fun fe fc):s)      = let efix = (Fun efix fc):fe
                                        in runBC' c e ((Fun efix fc):s)
