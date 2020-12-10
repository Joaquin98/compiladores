module CIR where

import Lang ( BinaryOp, Name, UnaryOp )
import Data.List (intercalate)

newtype Reg = Temp String
  deriving Show

data Val = R Reg | C Int | G Name
  deriving Show

type Loc = String

data Inst =
    Assign Reg Expr
  | Store Name Expr
  deriving Show

data Expr =
    BinOp BinaryOp Val Val
  -- | UnOp UnaryOp Val
  | Phi [(Loc, Val)]
  | Call Val [Val]
  | MkClosure Loc [Val]
  | V Val
  | Access Val Int
  deriving Show

data Terminator =
    Jump Loc
  | CondJump Cond Loc Loc
  | Return Val
  deriving Show

data Cond =
    Eq Val Val
  deriving Show

type BasicBlock = (Loc, [Inst], Terminator)
type Blocks = [BasicBlock]

type CanonFun = (String, [String], [BasicBlock])
type CanonVal = String -- Sólo el nombre, tipo puntero siempre
newtype CanonProg = CanonProg [Either CanonFun CanonVal]

print :: (Blocks, [Inst], Val) -> String
print (bs, is, v) =
  concatMap printBlock bs ++ show is ++ "\n" ++ show v ++ "\n"

printBlock :: BasicBlock -> String
printBlock (loc, is, t) =
  loc ++ ":\n" ++
  concatMap (\i -> "  " ++ show i ++ "\n") is ++
  show t ++ "\n"

instance Show CanonProg where
  show (CanonProg prog) = concatMap pr1 prog where
    pr1 (Left (f, args, blocks)) =
      f ++ "(" ++ intercalate ", " args ++ ") {\n"
        ++ concatMap printBlock blocks ++ "}\n\n"

    pr1 (Right v) =
      "declare " ++ v ++ "\n\n"


runCanon' :: IrDecl -> StateT (Int,Loc,[Inst]) (Writer Blocks) (Either CanonFun CanonVal)
runCanon' (IrVal name tm)         = return $ Right name
runCanon' (IrFun name _ args tm)  = do setLoc name
                                       tm' <- irToBlocks tm
                                       closeBlocks blablabla
                                       (m, bls) <- listen
                                       
{-
do tm' <- irToBlocks tm
addInst $ Store name tm'
return ()
-}

decls se usa dos veces para main y para mapear runCanon'
runCanon :: IrDecls -> CanonProg
runCanon decls = map runCanon' decls
                 filter decls Val
                 hacer main


main:
  val1 = tm'
  val2 = ...
  -----



Lista bloques, bloque parcial, nombres frescos registros.

(Etiqueta, [])

-- Fijarse que se use el name
getNewReg :: String -> StateT (Int,Loc,[Inst]) (Writer Blocks) Reg
getNewReg name = do (n,_,_) <- get
                    modify (\(a,b,c) -> (a+1,b,c))
                    return $ Temp $ "reg" ++ name ++ show(n)

getNewLoc :: String -> StateT (Int,Loc,[Inst]) (Writer Blocks) String
getNewLoc name = do (n,_,_) <- get
                    modify (\(a,b,c) -> (a+1,b,c))
                    return $ name ++ show(n)

addInst :: Inst -> StateT (Int,Loc,[Inst]) (Writer Blocks) ()
addInst i = do modify (\(n,l,is) -> ((n,l,is ++ [i])))
               return ()             

setLoc :: Loc -> StateT (Int,Loc,[Inst]) (Writer Blocks) ()
setLoc newL = do modify (\(n,l,is) -> (n,newL,is))      
                 return ()      
                 
closeBlock :: Terminator -> StateT (Int,Loc,[Inst]) (Writer Blocks) ()
closeBlock ter = do (n,l,is) <- get
                    modify (\(n,l,is) -> (n,"",[]))
                    tell $ [(l,is,ter)]
                    return ()


irToBlocks :: IrTm -> StateT (Int,Loc,[Inst]) (Writer Blocks) Val
irToBlocks (IrVar name)           = return (G name) -- ??
irToBlocks (IrCall tm tms)        = do r1  <- getNewReg ""
                                       tm' <- irToBlocks tm
                                       tms' <- mapM irToBlocks tms
                                       addInst $ Assign r1 (Call tm' tms')
                                       return $ R r1
irToBlocks (IrConst (CNat n))     = return (C n)
irToBlocks (IrBinaryOp op t1 t2)  = do r1  <- getNewReg ""
                                       r2  <- getNewReg ""
                                       r3  <- getNewReg ""
                                       t1' <- irToBlocks t1
                                       t2' <- irToBlocks t1  
                                       addInst $ Assign r1 (V t1')
                                       addInst $ Assign r2 (V t2')
                                       addInst $ Assign r3 (BinOp op (R r1) (R r2))
                                       return (R r3)
irToBlocks (IrLet name t1 t2)     = do r1  <- getNewReg ""
                                       r2  <- getNewReg ""
                                       t1' <- irToBlocks t1
                                       addInst $ Store name (V t1')
                                       t2' <- irToBlocks t2
                                       addInst $ Assign r2 (V t1')
                                       return $ R r2
irToBlocks (IrIfZ c t1 t2)        = do lEntry <- getNewLoc "entry"
                                       lThen  <- getNewLoc "then"
                                       lElse  <- getNewLoc "else"
                                       lCont  <- getNewLoc "ifcont"
                                       closeBlock (Jump lEntry)
                                       setLoc lEntry
                                       c' <- irToBlocks c
                                       rc <- getNewReg "cond"
                                       addInst (Assign rc c')
                                       closeBlock $ CondJump (Eq c' (C 0)) lThen lElse
                                       setLoc lThen
                                       t1' <- irToBlocks t1
                                       rt1 <- getNewReg "then"
                                       addInst (Assign rt1 t1')
                                       closeBlock $ Jump lCont
                                       setLoc lThen
                                       t2' <- irToBlocks t2
                                       rt2 <- getNewReg "else"
                                       addInst (Assign rt1 t2')
                                       closeBlock $ Jump lCont
                                       setLoc lCont
                                       rCont <- getNewReg "cont"
                                       addInst $ Assign rCont $ Phi [(lThen,(R rt1)),(lElse,(R rt2))]
                                       return $ R rCont
irToBlocks (MkClosure name tms)   = do r1   <- getNewReg ""
                                       tms' <- mapM irToBlocks tms
                                       addInst $ Assign r1 (MkClosure name tms')
                                       return $ R r1
irToBlocks (IrAccess tm n)        = do r1   <- getNewReg ""
                                       tm' <- irToBlocks tm
                                       addInst $ Assign r1 (Access tm' n)
                                       return $ R r1

Preguntas:

Al convertir un termino hay varios casos:
- Solo nombre, tipo puntero?? Entendemos que es el nombre 
  donde antes se hizo un store.
- En el caso que la funcion de conversion no sea monadica 
  como se hace en la conversión del IrVal para guardar el 
  tm convertido en el puntero indicado por la variable.
- En el caso que sea un if se convertiría a varios bloques.
  Suponiendo que se tiene una mónada para ir guardando los 
  bloques nuevos. Como se haría el salto desde el bloque que
  llama al entry (y se acaba de cerrar) al bloque ifcont nuevo.
- Cuales serían las funciones a implementar, y los tipos de 
  retorno de cada una.


Preguntas:

- Para mejorar la nota, se tienen que implementar si o si las 
  optimizaciones o se puede hacer otra cosa.
- Cuando hay que guardar en registros y cuando no? 
  Si el irToBlocks ya nos devuelve un Val, hace falta siempre
  ponerlo en un registro y devolver el (R r1) o se puede usar
  directo el valor que devuelve ?
- Como haríamos los LetIn, si usamo Store para guardar el valor de
  t1 en name entonces quedaría global y no local como debería ser.
- Dar vuelta la monada y usar runWriter. Extraer lista de bloques
  y reinicarla sin afectar el state.
  Usar listen para sacar lo de adentro y writer () [] para reiniciar.
- En el main se hacen todos los store de las declaraciones de valores??
- Que pasaría si usaramos solo StateT en lugar de StateT y Writer?

