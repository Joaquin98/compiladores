{-|
Module      : Parse
Description : Define un parser de términos PCF0 a términos fully named.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}

module Parse (tm, Parse.parse, decl, runP, P, program, declOrTm) where

import Prelude hiding ( const )
import Lang
import Common
import Text.Parsec hiding (runP, parse)
import Data.Char ( isNumber, ord )
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language ( GenLanguageDef(..), emptyDef )

type P = Parsec String ()

-----------------------
-- Lexer
-----------------------
-- | Analizador de Tokens
lexer :: Tok.TokenParser u
lexer = Tok.makeTokenParser $
        emptyDef {
         commentLine    = "#",
         reservedNames = ["let", "fun", "fix", "then", "else", 
                          "succ", "pred", "ifz", "Nat", "in", "rec", "type"],
         reservedOpNames = ["->",":","="]
        }

whiteSpace :: P ()
whiteSpace = Tok.whiteSpace lexer

natural :: P Integer 
natural = Tok.natural lexer

parens :: P a -> P a
parens = Tok.parens lexer

identifier :: P String
identifier = Tok.identifier lexer

reserved :: String -> P ()
reserved = Tok.reserved lexer

reservedOp :: String -> P ()
reservedOp = Tok.reservedOp lexer

-----------------------
-- Parsers
-----------------------

num :: P Int
num = fromInteger <$> natural

var :: P Name
var = identifier 

getPos :: P Pos
getPos = do pos <- getPosition
            return $ Pos (sourceLine pos) (sourceColumn pos)

tyatom :: P STy
tyatom = (reserved "Nat" >> return SNatTy)
         <|> do name <- var 
                return (DTy name)
         <|> parens typeP

typeP :: P STy
typeP = try (do 
          x <- tyatom
          reservedOp "->"
          y <- typeP
          return (SFunTy x y))
      <|> tyatom
          


          
const :: P Const
const = CNat <$> num

unaryOpApp :: P SMNTerm
unaryOpApp = do
  i <- getPos
  foldr (\(w, r) rest -> try (do 
                                 reserved w
                                 a <- atom
                                 return (r a))
                                 <|> rest) parserZero (mapping i)
  where
   mapping i = [
       ("succ", SUnaryOpApp i Succ)
     , ("pred", SUnaryOpApp i Pred)
    ]


unaryOpNApp :: P SMNTerm
unaryOpNApp = do
  i <- getPos
  foldr (\(w, r) rest -> try (do 
                                 reserved w
                                 return r)
                                 <|> rest) parserZero (mapping i)
  where
   mapping i = [
       ("succ", SUnaryOp i Succ)
     , ("pred", SUnaryOp i Pred)
    ]
    
unaryOp :: P SMNTerm
unaryOp = unaryOpApp <|> unaryOpNApp 

atom :: P SMNTerm
atom =     (flip SConst <$> const <*> getPos)
       <|> flip SV <$> var <*> getPos
       <|> parens tm

lam :: P SMNTerm
lam = do i <- getPos
         reserved "fun"
         bs <-  mbinding
         reservedOp "->"
         t <- tm
         return (SLam i bs t)

-- Nota el parser app también parsea un solo atom.
app :: P SMNTerm
app = (do i <- getPos
          f <- atom
          args <- many atom
          return (foldl (SApp i) f args))

ifz :: P SMNTerm
ifz = do i <- getPos
         reserved "ifz"
         c <- tm
         reserved "then"
         t <- tm
         reserved "else"
         e <- tm
         return (SIfZ i c t e)

binding :: P (UnaryBind, STy)
binding = do v <- var
             reservedOp ":"
             ty <- typeP
             return (v, ty)


mbinding :: P [(MultiBind, STy)]
mbinding = many (parens $ do vs <- many var
                             reservedOp ":"
                             ty <- typeP
                             return (vs, ty))
           <|> return []


fix :: P SMNTerm
fix = do i <- getPos
         reserved "fix"
         (f, fty) <- parens binding
         (x, xty) <- parens binding
         reservedOp "->"
         t <- tm
         return (SFix i f fty x xty t)

fun :: P (Name,[(MultiBind, STy)],STy)
fun = do nf <- var  
         bs <- mbinding
         reservedOp ":"
         ty <- typeP
         return (nf,bs,ty)


recP :: P Bool
recP = (reserved "rec" >> return True)
       <|> return False   

letP :: P ((Name, [(MultiBind, STy)], STy), Bool, SMNTerm)
letP = do reserved "let"
          b <- recP
          f <- fun
          reservedOp "="
          t <- tm
          return (f, b, t)


letIn :: P SMNTerm
letIn = do i <- getPos
           ((v, bs, ty), b, t) <- letP
           reserved "in"
           t' <- tm
           if not b then return (SLetIn i v bs ty t t')
                else if bs == [] then parserZero
                else return (SRec i v bs ty t t')
{-
letRec :: SMNTerm
letRec = do i <- getPos
           reserved "let"
           reserved "rec"
           (v, bs, ty) <- fun
           reservedOp "="
           t <- tm
           reserved "in"
           t' <- tm
           if bs /= [] then return (SRec i v bs ty t t')
                       else parserZero
-}
-- | Parser de términos
tm :: P SMNTerm
tm = app <|> lam <|> ifz <|> unaryOp <|> fix <|> letIn 

-- | Parser de declaraciones
declLet :: P (SDecl SMNTerm MultiBind STy)
declLet = do 
     i <- getPos
     ((v, bs, ty), b, t) <- letP
     return (DTer i v bs ty b t)

declT :: P (SDecl SMNTerm MultiBind STy)
declT = do i <- getPos
           reserved "type"
           v <- var
           reservedOp "="
           ty <- typeP
           return (DType i v ty)

decl :: P (SDecl SMNTerm MultiBind STy)
decl = try (do t <- declLet
               return t)
       <|> (do t <- declT
               return t)

-- | Parser de programas (listas de declaraciones) 
program :: P [(SDecl SMNTerm MultiBind STy)]
program = many decl

-- | Parsea una declaración o un término
-- Útil para las sesiones interactivas
declOrTm :: P (Either (SDecl SMNTerm MultiBind STy) SMNTerm)
declOrTm =  try (Right <$> tm) <|> (Left <$> decl)

-- Corre un parser, chequeando que se pueda consumir toda la entrada
runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s

--para debugging en uso interactivo (ghci)
parse :: String -> SMNTerm
parse s = case runP tm s "" of
            Right t -> t
            Left e -> error ("no parse: " ++ show s)

--  parse  "let rec f (x z y : Nat) : Nat = x in f 4 " 
--  runP declOrTm  "let rec f (x z y : Nat) : Nat = x in f 4 " ""