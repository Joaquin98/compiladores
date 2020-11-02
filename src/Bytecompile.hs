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
  (Bytecode, runBC, bcWrite, bcRead, bytecompileModule) 
 where

import Lang 
import Subst
import MonadPCF
import Common

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
--pattern SUCC     = 6
pattern ADD      = 6
--pattern PRED     = 7
pattern DIFF     = 7
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
bc t = do bytecode <- bc' t
          return(bytecode ++ [PRINT,STOP])

bc' :: MonadPCF m => Term -> m Bytecode
bc' (V _ (Bound i))     = return [ACCESS,i] 
bc' (Const _ (CNat n))  = return [CONST,n]
bc' (Lam _ _ _ t)       = do tc <- bc' t
                             return ([FUNCTION, (length tc) + 1] ++ tc ++ [RETURN])  
bc' (App _ f e)         = do fc <- bc' f
                             ec <- bc' e
                             return (fc ++ ec ++ [CALL]) 
--bc' (UnaryOp _ Succ t)  = do t' <- bc' t
--                             return (t' ++ [SUCC])
--bc' (UnaryOp _ Pred t)  = do t' <- bc' t
--                             return (t' ++ [PRED])
bc' (BinaryOp _ Add t1 t2) = do t1' <- bc' t1
                                t2' <- bc' t2
                                return (t1'++t2'++[ADD])
bc' (BinaryOp _ Diff t1 t2) = do t1' <- bc' t1
                                 t2' <- bc' t2
                                 return (t1'++t2'++[DIFF])
bc' (Fix _ _ _ _ _ t)   = do tc <- bc' t
                             return ([FUNCTION, (length tc) + 1] ++ tc ++ [RETURN,FIX])  
bc' (IfZ _ c t1 t2)     = do t2c <- bc' t2
                             t1c <- bc' t1
                             cc  <- bc' c
                             return (cc ++ [IFZ, (length t1c)+2] ++ t1c ++ [JUMP, (length t2c)] ++ t2c)
bc' (LetIn _ _ _ t1 t2) = do t1c <- bc' t1
                             t2c <- bc' t2
                             return (t1c ++ [SHIFT] ++ t2c ++ [DROP])
bc' x                   = do printPCF $ show x
                             return ([10])

--c IFZ (lt1) t1 JUMP (lt2) t2

convertModule :: [Decl Term Ty] -> Term
convertModule [d] = declBody d
convertModule (d:ds) = 
   (LetIn NoPos (declName d) (declType d) (declBody d) (close (declName d) (convertModule ds)))


bytecompileModule :: MonadPCF m => [Decl Term Ty] -> m Bytecode
bytecompileModule [] = return ([10])
bytecompileModule mod = do printPCF $  show (convertModule (reverse mod))
                           bc (convertModule (reverse mod))

-- | Toma un bytecode, lo codifica y lo escribe un archivo 
bcWrite :: Bytecode -> FilePath -> IO ()
bcWrite bs filename = BS.writeFile filename (encode $ BC $ fromIntegral <$> bs)

---------------------------
-- * Ejecución de bytecode
---------------------------

-- | Lee de un archivo y lo decodifica a bytecode
bcRead :: FilePath -> IO Bytecode
bcRead filename = map fromIntegral <$> un32  <$> decode <$> BS.readFile filename

runBC :: Bytecode -> IO ()
runBC c = runBC' c [] []

runBC' :: Bytecode -> [Val] -> [Val] -> IO ()
runBC' (STOP:c) _ _                   = return ()
runBC' (PRINT:c) e (v:s)              = do putStrLn (show v)
                                           runBC' c e s
runBC' (ACCESS:i:c) e s               = runBC' c e ((e!!i):s)
runBC' (CONST:n:c) e s                = runBC' c e ((I n):s)
runBC' (FUNCTION:len:c) e s           = runBC' (drop len c) e ((Fun e c):s)
runBC' (RETURN:c) e (v:(RA re ra):s)  = runBC' ra re (v:s)
runBC' (CALL:c) e (v:(Fun fe fc):s)   = runBC' fc (v:fe) ((RA e c):s)  
runBC' (ADD:c) e ((I n2):(I n1):s)    = runBC' c e ((I (n1+n2)):s)
runBC' (DIFF:c) e ((I n2):(I n1):s)   = runBC' c e ((I (max 0 (n1-n2))):s) 
runBC' (IFZ:lt1:c) e ((I 0):s)        = runBC' c e s
runBC' (IFZ:lt1:c) e ((I n):s)        = runBC' (JUMP:lt1:c) e s
runBC' (JUMP:lt:c) e s                = runBC' (drop lt c) e s
runBC' (FIX:c) e ((Fun fe fc):s)      = let efix = (Fun efix fc):fe
                                        in runBC' c e ((Fun efix fc):s)
runBC' (SHIFT:c) e (v:s)              = runBC' c (v:e) s
runBC' (DROP:c) (v:e) s               = runBC' c e s
runBC' c e s = do putStrLn (show c)
                  return ()
{-
4 -> [12 ... ] [] [(Fun [] [3 0 6...])]
12 -> [3 ...] [(Fun [] [3 0 6...])] []
3 -> [2 ...] [(Fun [] [3 0 6...])] [(Fun [] [3 0 6...])]
2 -> [5 ..] [(Fun [] [3 0 6...])] [1 : (Fun [] [3 0 6...])]
5 -> [3 0 6 ... ] [1] [(RA [(Fun [] [3 0 6...])] [13 ...])]
3 -> [6 ...] [1] [ 1 : (RA [(Fun [] [3 0 6...])] [13 ...])]
3 -> [1 ...] [1] [ 2 : (RA [(Fun [] [3 0 6...])] [13 ...])]
-}