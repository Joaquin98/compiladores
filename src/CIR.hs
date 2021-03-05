module CIR where

import Lang ( BinaryOp,UnaryOp(..), Const(CNat), Name, UnaryOp, IrDecl(IrVal,IrFun),IrDecls,IrTm(IrVar,IrCall,IrConst,IrBinaryOp,IrLet,IrIfZ,MkClosure,IrAccess))
import Data.List (intercalate,isPrefixOf)
import Control.Monad.Writer
import Control.Monad.State.Lazy


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
  | UnOp UnaryOp Val
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

{-
  Canonicaliza una sola declaración del lenguaje intermedio.
-}
runCanon' :: IrDecl -> StateT (Int,Loc,[Inst]) (Writer Blocks) CanonVal
runCanon' (IrVal name tm)         = return name
runCanon' (IrFun name _ args tm)  = do setLoc name
                                       tm' <- irToBlocks tm
                                       closeBlock (Return tm')
                                       return ""

{-
  Canonicaliza una lista de declaraciones, manteniendo el estado
  (número necesario para obtener nombres frescos) entre las
  canonicalizaciones de cada elemento.

  Es necesaria la segunda linea de vals?
-}
runSavingState :: [IrDecl] -> (Int,Loc,[Inst]) -> [Either CanonFun CanonVal]
runSavingState (fun@(IrFun name _ args tm):xs) initS  = let funC = runCanon' fun 
                                                            ((val,state),list) = runWriter (runStateT funC initS)
                                                        in ((Left (name,args,list)):runSavingState xs state)
runSavingState vals initS                             = let ((val,state),list) = runWriter (runStateT (makeMain vals) initS)
                                                            ((val2,state2),list2) = runWriter (runStateT (mapM runCanon' vals) initS)
                                                        in (map (\x -> Right x) val2) ++ [Left ("pcfmain",[],list)]

{-
  Declara cada una de las variables dentro del main.
-}
makeMain' :: IrDecls -> StateT (Int,Loc,[Inst]) (Writer Blocks) Val
makeMain' [(IrVal name tm)]    = do tm' <- irToBlocks tm
                                    r  <- getNewReg ""
                                    addInst $ Assign r (UnOp Print tm')
                                    addInst $ Store name (V (R r))
                                    return tm'
makeMain' ((IrVal name tm):xs) = do tm' <- irToBlocks tm
                                    addInst $ Store name (V tm')
                                    makeMain' xs


{-
  Abre y cierra el main, delegando el resto a makeMain'.
-}
makeMain :: IrDecls -> StateT (Int,Loc,[Inst]) (Writer Blocks) ()
makeMain decls = do setLoc "pcfmain"
                    val <- makeMain' decls
                    closeBlock (Return val)

{-
  Toma la lista de declaraciones y deja los valores al final.
  De forma que primero se creen los bloques de las funciones
  y luego se puede crear el main con los valores.
-}
valsAtBottom :: IrDecls -> IrDecls -> IrDecls -> IrDecls
valsAtBottom [] funs vals                              = funs ++ vals
valsAtBottom (val@(IrVal name tm):xs) funs vals        = valsAtBottom xs funs (val:vals)
valsAtBottom (fun@(IrFun name _ args tm):xs) funs vals = valsAtBottom xs (fun:funs) vals


{-
  Ordena las declaraciones para luego crear los bloques de cada
  funciones y al final el main que declara las variables.
-}
runCanon :: IrDecls -> CanonProg
runCanon decls = let orderedDecls = valsAtBottom decls [] []
                     funsBlocks   = runSavingState orderedDecls (0,"",[])
                 in CanonProg funsBlocks 


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

{-
  Convierte un termino en bloques.
-}
irToBlocks :: IrTm -> StateT (Int,Loc,[Inst]) (Writer Blocks) Val
irToBlocks (IrVar name)              | isPrefixOf "__" name = return (R (Temp name))
                                     | otherwise            = return (G name) 
irToBlocks (IrCall tm tms)           = do r1  <- getNewReg ""
                                          tm' <- irToBlocks tm
                                          tms' <- mapM irToBlocks tms
                                          addInst $ Assign r1 (Call tm' tms')
                                          return $ R r1
irToBlocks (IrConst (CNat n))        = do r <- getNewReg ""
                                          addInst $ Assign r (V (C n))
                                          return (R r)
irToBlocks (IrBinaryOp op t1 t2)     = do r  <- getNewReg ""
                                          t1' <- irToBlocks t1
                                          t2' <- irToBlocks t2  
                                          addInst $ Assign r (BinOp op t1' t2')
                                          return (R r)
irToBlocks (IrLet name t1 t2)        = do t1' <- irToBlocks t1
                                          addInst $ Assign (Temp name) (V t1')
                                          t2' <- irToBlocks t2
                                          return $ t2'
irToBlocks (IrIfZ c t1 t2)           = do lEntry <- getNewLoc "entry"
                                          lThen  <- getNewLoc "then"
                                          lElse  <- getNewLoc "else"
                                          lCont  <- getNewLoc "ifcont"
                                          closeBlock (Jump lEntry)
                                          setLoc lEntry
                                          c' <- irToBlocks c
                                          closeBlock $ CondJump (Eq c' (C 0)) lThen lElse
                                          setLoc lThen
                                          t1' <- irToBlocks t1
                                          (_, actualLocThen, _) <- get
                                          closeBlock $ Jump lCont
                                          setLoc lElse
                                          t2' <- irToBlocks t2
                                          (_, actualLocElse, _) <- get
                                          closeBlock $ Jump lCont
                                          setLoc lCont
                                          rCont <- getNewReg "cont"
                                          addInst $ Assign rCont $ Phi [(actualLocThen, t1'),(actualLocElse, t2')]
                                          return $ R rCont
irToBlocks (Lang.MkClosure name tms) = do r1   <- getNewReg ""
                                          tms' <- mapM irToBlocks tms
                                          addInst $ Assign r1 (CIR.MkClosure name tms')
                                          return $ R r1
irToBlocks (IrAccess tm n)           = do r1   <- getNewReg ""
                                          tm' <- irToBlocks tm
                                          addInst $ Assign r1 (Access tm' n)
                                          return $ R r1
