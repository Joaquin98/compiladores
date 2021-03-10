{-# LANGUAGE DeriveFunctor #-}

{-|
Module      : Lang
Description : AST de términos, declaraciones y tipos
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

Definiciones de distintos tipos de datos:
  - AST de términos
  - Declaraciones
  - Tipos
  - Variables

-}

module Lang where

import Common ( Pos )

-- | AST de Tipos
data Ty = 
      NTy Name Ty
    | NatTy 
    | FunTy Ty Ty
    deriving (Show,Eq)

data STy = 
      DTy Name -- Declaracion de tipo
    | SNatTy 
    | SFunTy STy STy
    deriving (Show,Eq)

type Name = String

data Const = CNat Int
  deriving (Show,Eq)

data UnaryOp = Succ | Pred | Print
  deriving (Show,Eq)

data BinaryOp = Add | Diff
  deriving (Show,Eq)

-- | tipo de datos de declaraciones, parametrizado por el tipo del cuerpo de la declaración
data Decl a b =
    Decl { declPos :: Pos, declName :: Name, declType:: b, declBody :: a}
  deriving (Show,Functor,Eq)

data SDecl term bind ty =
    DTer Pos Name [(bind, ty)] STy Bool term 
  | DType Pos Name ty
  deriving (Show,Functor)


-- | AST de los términos. 
--   - info es información extra que puede llevar cada nodo. 
--     Por ahora solo la usamos para guardar posiciones en el código fuente.
--   - var es el tipo de la variables. Es 'Name' para fully named y 'Var' para locally closed. 
data Tm info var ty = 
    V info var
  | Const info Const
  | Lam info Name ty (Tm info var ty)
  | App info (Tm info var ty) (Tm info var ty)
--  | UnaryOp info UnaryOp (Tm info var ty) (eliminado del lenguaje interno)
  | BinaryOp info BinaryOp (Tm info var ty) (Tm info var ty)
  | Fix info Name ty Name ty (Tm info var ty)
  | IfZ info (Tm info var ty) (Tm info var ty) (Tm info var ty)
  | LetIn info Name ty (Tm info var ty) (Tm info var ty)
  deriving (Show, Functor, Eq)

-- AST de los términos antes del desugar.
-- Permite variar los bindings porque aceptamos multibinding y luego lo
-- convertimos a unary binding (usando el mismo AST).
data STm info var bind ty = 
    SV info var
  | SConst info Const
  | SLam info [(bind,ty)] (STm info var bind ty)
  | SApp info (STm info var bind ty) (STm info var bind ty)
  | SUnaryOpApp info UnaryOp (STm info var bind ty)
  | SUnaryOp info UnaryOp
  | SBinaryOp info BinaryOp (STm info var bind ty) (STm info var bind ty) 
  | SFix info Name ty Name ty (STm info var bind ty)
  | SIfZ info (STm info var bind ty) (STm info var bind ty) (STm info var bind ty)
  | SLetIn info Name [(bind, ty)] ty (STm info var bind ty) (STm info var bind ty)
  | SRec info Name [(bind, ty)] ty (STm info var bind ty) (STm info var bind ty)
  deriving (Show, Functor)

-- Tipos de binding
type UnaryBind = Name
type MultiBind = [Name]

-- SMN : Sugar-Typed Multi-Binded Named
-- Términos azucarados, tipos azucarados, variables con nombres y multibinding.
type SMNTerm = STm Pos Name MultiBind STy

-- SUN : Sugar-Typed Unary-Binded Named
-- Términos azucarados, tipos azucarados,  variables nomrbes y unary binding.
type SUNTerm = STm Pos Name UnaryBind STy

-- SNTerm : Sugar-Typed Named
-- Términos desazucarados, tipos azucarados, variables con nombres.
type SNTerm = Tm Pos Name STy

-- NTerm : Named 
-- Términos desazucarados, variables con nombres, tipos desazucarados
type NTerm = Tm Pos Name Ty  -- ^ 'Tm' tiene 'Name's como variables ligadas y libres, guarda posición

-- Term
-- Términos desazucarados, variables ligadas con índice de Bruijn
-- Términos del lenguaje interno.
type Term = Tm Pos Var Ty    -- ^ 'Tm' con índices de De Bruijn como variables ligadas, different type of variables, guarda posición


data Var = 
    Bound !Int
  | Free Name
  deriving (Show,Eq)


data IrTm = IrVar Name
          | IrCall IrTm [IrTm]
          | IrConst Const
          | IrBinaryOp BinaryOp IrTm IrTm
          | IrLet Name IrTm IrTm
          | IrIfZ IrTm IrTm IrTm
          | MkClosure Name [IrTm]
          | IrAccess IrTm Int
  deriving (Show)


data IrDecl = IrVal { irDeclName :: Name , irDeclDef :: IrTm } 
            | IrFun { irDeclName :: Name, irDeclArity :: Int, irDeclArgNames :: [Name], irDeclBody :: IrTm }
    deriving (Show)

type IrDecls = [IrDecl]

-- | Obtiene la info en la raíz del término.
getInfo :: Tm info var ty -> info
getInfo (V i _)           = i
getInfo (Const i _)       = i
getInfo (Lam i _ _ _)     = i
getInfo (App i _ _ )      = i
getInfo (BinaryOp i _ _ _)= i
getInfo (Fix i _ _ _ _ _) = i
getInfo (IfZ i _ _ _)     = i
getInfo (LetIn i _ _ _ _) = i
--getInfo (UnaryOp i _ _)   = i (eliminado del lenguaje interno)

-- | Obtiene las variables libres de un término.
freeVars :: Tm info Var ty -> [Name]
freeVars (V _ (Free v))      = [v]
freeVars (V _ _)             = []
freeVars (Lam _ _ _ t)       = freeVars t
freeVars (App _ l r)         = freeVars l ++ freeVars r
freeVars (BinaryOp _ _ t t') = freeVars t ++ freeVars t'
freeVars (Fix _ _ _ _ _ t)   = freeVars t
freeVars (IfZ _ c t e)       = freeVars c ++ freeVars t ++ freeVars e
freeVars (Const _ _)         = []
freeVars (LetIn _ n ty t t') = freeVars t ++ freeVars t'
--freeVars (UnaryOp _ _ t)     = freeVars t (eliminado del lenguaje interno)