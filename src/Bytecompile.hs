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
import Data.Binary (Word32, Binary(put, get), decode, encode)
import Data.Binary.Put (putWord32le)
import Data.Binary.Get (getWord32le, isEmpty)

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
--pattern SUCC     = 6 (lo eliminamos del lenguaje interno)
pattern ADD      = 6
--pattern PRED     = 7 (lo eliminamos del lenguaje interno)
pattern DIFF     = 7
pattern IFZ      = 8
pattern FIX      = 9
pattern STOP     = 10
pattern JUMP     = 11
pattern SHIFT    = 12
pattern DROP     = 13
pattern PRINT    = 14
pattern TAILCALL = 15

type Env = [Val]
data Val = I Int | Fun Env Bytecode | RA Env Bytecode deriving (Show)

-- Corre el bc' y le agrega al final una instrucción para
-- que imprima en pantalla el resultado final.
bc :: MonadPCF m => Term -> m Bytecode
bc t = do bytecode <- bc' t
          return(bytecode ++ [PRINT,STOP])

-- Función mutuamente recusiva con bc' que colabora en la 
-- generación de bytecode. Agregada cuando se implementaron 
-- las tailcall.
tcompile :: MonadPCF m => Term -> m Bytecode
tcompile (App _ f e)         = do fc <- bc' f
                                  ec <- bc' e
                                  return (fc ++ ec ++ [TAILCALL]) 
tcompile (IfZ _ c t1 t2)     = do t2c <- tcompile t2
                                  t1c <- tcompile t1
                                  cc  <- bc' c
                                  return (cc ++ [IFZ, (length t1c)+2] ++ t1c ++ [JUMP, (length t2c)] ++ t2c)
tcompile (LetIn _ _ _ t1 t2) = do t1c <- bc' t1
                                  t2c <- tcompile t2
                                  return (t1c ++ [SHIFT] ++ t2c )                            
tcompile t                   = do tc <- bc' t
                                  return(tc ++ [RETURN])         

-- Función auxiliar de bc que toma un término y lo convierte
-- en una secuencia de instrucciones de bytecode.
bc' :: MonadPCF m => Term -> m Bytecode
bc' (V _ (Bound i))         = return [ACCESS,i] 
bc' (Const _ (CNat n))      = return [CONST,n]
bc' (Lam _ _ _ t)           = do tc <- tcompile t
                                 return ([FUNCTION, (length tc)] ++ tc)  
bc' (App _ f e)             = do fc <- bc' f
                                 ec <- bc' e
                                 return (fc ++ ec ++ [CALL]) 
bc' (BinaryOp _ Add t1 t2)  = do t1' <- bc' t1
                                 t2' <- bc' t2
                                 return (t1'++t2'++[ADD])
bc' (BinaryOp _ Diff t1 t2) = do t1' <- bc' t1
                                 t2' <- bc' t2
                                 return (t1'++t2'++[DIFF])
bc' (Fix _ _ _ _ _ t)       = do tc <- bc' t
                                 return ([FUNCTION, (length tc) + 1] ++ tc ++ [RETURN,FIX])  
bc' (IfZ _ c t1 t2)         = do t2c <- bc' t2
                                 t1c <- bc' t1
                                 cc  <- bc' c
                                 return (cc ++ [IFZ, (length t1c)+2] ++ t1c ++ [JUMP, (length t2c)] ++ t2c)
bc' (LetIn _ _ _ t1 t2)     = do t1c <- bc' t1
                                 t2c <- bc' t2
                                 return (t1c ++ [SHIFT] ++ t2c ++ [DROP])
bc' x                       = do printPCF $ show x
                                 return ([10])

-- eliminados del lenguaje interno
--bc' (UnaryOp _ Succ t)  = do t' <- bc' t
--                             return (t' ++ [SUCC])
--bc' (UnaryOp _ Pred t)  = do t' <- bc' t
--                             return (t' ++ [PRED])


-- Toma el progarama (una lista de declaraciones) y lo convierte en un solo termino
-- reemplazando las declaraciones por letIn. 
convertModule :: [Decl Term Ty] -> Term
convertModule [d]    = declBody d
convertModule (d:ds) = 
   (LetIn NoPos (declName d) (declType d) (declBody d) (close (declName d) (convertModule ds)))

-- Convierte un programa a bytecode.
bytecompileModule :: MonadPCF m => [Decl Term Ty] -> m Bytecode
bytecompileModule []  = return ([STOP])
bytecompileModule mod = do printPCF $  show (convertModule (reverse mod))
                           bc (convertModule mod)

-- | Toma un bytecode, lo codifica y lo escribe un archivo 
bcWrite :: Bytecode -> FilePath -> IO ()
bcWrite bs filename = BS.writeFile filename (encode $ BC $ fromIntegral <$> bs)

---------------------------
-- * Ejecución de bytecode
---------------------------

-- | Lee de un archivo y lo decodifica a bytecode
bcRead :: FilePath -> IO Bytecode
bcRead filename = map fromIntegral <$> un32  <$> decode <$> BS.readFile filename

-- Corre runBC' inicializando las dos listas como vacías.
runBC :: Bytecode -> IO ()
runBC c = runBC' c [] []

-- Evalua un código Bytecode valiéndose de dos listads de valores.
-- Una es el entorno que se va generando al correr las instrucciones
-- cuando aparecen variables.
-- La otra es la pila.
runBC' :: Bytecode -> [Val] -> [Val] -> IO ()
runBC' (STOP:c) _ _                     = return ()
runBC' (PRINT:c) e (v:s)                = do putStrLn (show v)
                                             runBC' c e s
runBC' (ACCESS:i:c) e s                 = runBC' c e ((e!!i):s)
runBC' (CONST:n:c) e s                  = runBC' c e ((I n):s)
runBC' (FUNCTION:len:c) e s             = runBC' (drop len c) e ((Fun e c):s)
runBC' (RETURN:c) e (v:(RA re ra):s)    = runBC' ra re (v:s)
runBC' (CALL:c) e (v:(Fun fe fc):s)     = runBC' fc (v:fe) ((RA e c):s)  
runBC' (TAILCALL:c) e (v:(Fun fe fc):s) = runBC' fc (v:fe) s  
runBC' (ADD:c) e ((I n2):(I n1):s)      = runBC' c e ((I (n1+n2)):s)
runBC' (DIFF:c) e ((I n2):(I n1):s)     = runBC' c e ((I (max 0 (n1-n2))):s) 
runBC' (IFZ:lt1:c) e ((I 0):s)          = runBC' c e s
runBC' (IFZ:lt1:c) e ((I n):s)          = runBC' (JUMP:lt1:c) e s
runBC' (JUMP:lt:c) e s                  = runBC' (drop lt c) e s
runBC' (FIX:c) e ((Fun fe fc):s)        = let efix = (Fun efix fc):fe
                                          in runBC' c e ((Fun efix fc):s)
runBC' (SHIFT:c) e (v:s)                = runBC' c (v:e) s
runBC' (DROP:c) (v:e) s                 = runBC' c e s
runBC' c e s                            = do putStrLn (show c)
                                             return ()
