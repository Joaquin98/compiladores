{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Main
Description : Compilador de PCF.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}

module Main where

import System.Console.Haskeline ( defaultSettings, getInputLine, runInputT, InputT )
import Control.Monad.Catch (MonadMask)

--import Control.Monad
import Control.Monad.Trans
import Data.List (nub,  intersperse, isPrefixOf )
import Data.Char ( isSpace )
import Control.Exception ( catch , IOException )
import System.Environment ( getArgs )
import System.IO ( stderr, hPutStr )

import Global ( GlEnv(..) )
import Errors
import Lang
import Parse ( P, tm, program, declOrTm, runP )
import Elab ( elab , elabD, rmvSynTerm, styToTy)
--import Eval ( eval )
import CEK (eval)
import PPrint ( pp , ppTy )
import MonadPCF
import TypeChecker ( tc, tcDecl )
import Closures (runCC)


-----------
import Options.Applicative
import Bytecompile

data Mode = Interactive 
          | Typecheck
          | Bytecompile
          | Run
          | ClosureConvert

-- Parser de banderas
parseMode :: Parser Mode
parseMode = 
     flag' Typecheck (long "typechek" <> short 't' <> help "Solo chequea tipos") 
     <|>  flag' Bytecompile (long "bytecompile" <> short 'c' <> help "Compila a la BVM")
     <|>  flag' ClosureConvert (long "cc" <> help "Convierte a clausuras")
     <|>  flag' Run (long "run" <> short 'r' <> help "Ehecuta bytecode en la BVM")
     <|>  flag Interactive Interactive (long "interactive" <> short 'i' <> help "Ejecuta de forma interactiva")

-- | Parser de opciones general, consiste de un modo y una lista de archivos a procesar
parseArgs :: Parser (Mode,[FilePath])
parseArgs = (,) <$> parseMode <*> many (argument str (metavar "FILES..."))


main :: IO () 
main = execParser opts >>= go 
       where 
        opts = info (parseArgs <**> helper) (fullDesc <> progDesc "Compilador de PCF" <> header "Compilador de PCF de la materia Compiladores 2020")
       
        go :: (Mode,[FilePath]) -> IO ()
        go (Interactive,files) = do runPCF (runInputT defaultSettings (main' files))
                                    return ()
        go (Typecheck,files) = undefined
        go (Bytecompile,files) = do a <- runPCF (runInputT defaultSettings (compileToBytecode files))
                                    case a of
                                      Right (Just bytecode) -> do putStrLn $ show bytecode
                                                                  bcWrite bytecode "a.bytecode"
                                                                  return ()
                                      _ -> return ()
        go (Run,files) = do bytecode <- bcRead (head files)
                            runBC bytecode
                            return ()
        go (ClosureConvert,files) = do a <- runPCF (runInputT defaultSettings (compileToClosures files))
                                       case a of
                                         Right (Just closures) -> do putStrLn $ show closures
                  
                  
compileToClosures :: (MonadPCF m, MonadMask m) => [String] -> InputT m (Maybe [IrDecl])
compileToClosures (arg:args) = do lift $ catchErrors $ makeClosures arg
          
makeClosures ::  MonadPCF m => String -> m ([IrDecl])
makeClosures f = do
    printPCF ("Abriendo "++f++"...")
    let filename = reverse(dropWhile isSpace (reverse f))
    x <- liftIO $ catch (readFile filename)
               (\e -> do let err = show (e :: IOException)
                         hPutStr stderr ("No se pudo abrir el archivo " ++ filename ++ ": " ++ err ++"\n")
                         return "")
    decls <- parseIO filename program x
    mapM_ handleDecl' decls
    s <- get
    let closures = runCC (glb s) in
      return $ reverse closures

------------
-- AGREGADO (NO COPIADO DEL APUNTE) 

compileToBytecode :: (MonadPCF m, MonadMask m) => [String] -> InputT m (Maybe Bytecode)
compileToBytecode (arg:args) = do lift $ catchErrors $ compileFile' arg
          
compileFile' ::  MonadPCF m => String -> m (Bytecode)
compileFile' f = do
    printPCF ("Abriendo "++f++"...")
    let filename = reverse(dropWhile isSpace (reverse f))
    x <- liftIO $ catch (readFile filename)
               (\e -> do let err = show (e :: IOException)
                         hPutStr stderr ("No se pudo abrir el archivo " ++ filename ++ ": " ++ err ++"\n")
                         return "")
    decls <- parseIO filename program x
    mapM_ handleDecl' decls
    s <- get
    bytecode <- bytecompileModule (glb s)
    return bytecode
    
{-
declNames :: [Decl Term Ty] -> [Name]
declNames ds = map (\d -> (declName d)) ds

toLetIn :: [Decl Term Ty] -> Term
toLetIn [l] =  (declBody l)
toLetIn (l:ls) =  App (declPos l) (Lam (declPos l) (declName l) (declType l) (declBody l)) (toLetIn ls)
-}

handleDecl' :: MonadPCF m => SDecl SMNTerm MultiBind STy -> m ()
handleDecl' decl = do let ed = elabD decl
                      case ed of
                       Right d -> handleTermDecl' d
                       Left d -> handleTypeDecl d

-- Maneja la ejecucion de las declaraciones de terminos.
handleTermDecl' ::  MonadPCF m => Decl SMNTerm STy -> m ()
handleTermDecl' (Decl p name sty termSty) = do
        ty <- styToTy sty
        tt <- elab termSty
        tcDecl (Decl p name ty tt)  
        addDecl (Decl p name ty tt) 



-- FIN PARTE NUEVA 


prompt :: String
prompt = "PCF> "
{-
main :: IO ()
main = do args <- getArgs
          runPCF (runInputT defaultSettings (main' args))
          return ()
-}        
main' :: (MonadPCF m, MonadMask m) => [String] -> InputT m ()
main' args = do
        lift $ catchErrors $ compileFiles args
        s <- lift $ get
        when (inter s) $ liftIO $ putStrLn
          (  "Entorno interactivo para PCF0.\n"
          ++ "Escriba :? para recibir ayuda.")
        loop  
  where loop = do
           minput <- getInputLine prompt
           case minput of
               Nothing -> return ()
               Just "" -> loop
               Just x -> do
                       c <- liftIO $ interpretCommand x
                       b <- lift $ catchErrors $ handleCommand c
                       maybe loop (flip when loop) b
 
compileFiles ::  MonadPCF m => [String] -> m ()
compileFiles []     = return ()
compileFiles (x:xs) = do
        modify (\s -> s { lfile = x, inter = False })
        compileFile x
        compileFiles xs

compileFile ::  MonadPCF m => String -> m ()
compileFile f = do
    printPCF ("Abriendo "++f++"...")
    let filename = reverse(dropWhile isSpace (reverse f))
    x <- liftIO $ catch (readFile filename)
               (\e -> do let err = show (e :: IOException)
                         hPutStr stderr ("No se pudo abrir el archivo " ++ filename ++ ": " ++ err ++"\n")
                         return "")
    decls <- parseIO filename program x
    mapM_ handleDecl decls

parseIO ::  MonadPCF m => String -> P a -> String -> m a
parseIO filename p x = case runP p x filename of
                  Left e  -> throwError (ParseErr e)
                  Right r -> return r

handleDecl :: MonadPCF m => SDecl SMNTerm MultiBind STy -> m ()
handleDecl decl = do let ed = elabD decl
                     case ed of
                      Right d -> handleTermDecl d
                      Left d -> handleTypeDecl d

-- Maneja la ejecucion de las declaraciones de terminos.
handleTermDecl ::  MonadPCF m => Decl SMNTerm STy -> m ()
handleTermDecl (Decl p name sty termSty) = do
        ty <- styToTy sty
        tt <- elab termSty
        tcDecl (Decl p name ty tt)    
        te <- eval tt
        addDecl (Decl p name ty te) 

-- Maneja la ejecucion de las declaraciones de sinonimos de tipos.
handleTypeDecl :: MonadPCF m => SDecl SMNTerm UnaryBind STy -> m ()
handleTypeDecl (DType i name sty) = do ty <- styToTy sty
                                       mty <- lookupSynTy name
                                       case mty of 
                                         Just _ -> failPosPCF i $ name ++" ya existe como sinonimo de tipo"
                                         Nothing ->  addSynTy name ty
                                                      

data Command = Compile CompileForm
             | Print String
             | Type String
             | Browse
             | Quit
             | Help
             | Noop

data CompileForm = CompileInteractive  String
                 | CompileFile         String

data InteractiveCommand = Cmd [String] String (String -> Command) String

-- | Parser simple de comando interactivos
interpretCommand :: String -> IO Command
interpretCommand x
  =  if isPrefixOf ":" x then
       do  let  (cmd,t')  =  break isSpace x
                t         =  dropWhile isSpace t'
           --  find matching commands
           let  matching  =  filter (\ (Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
           case matching of
             []  ->  do  putStrLn ("Comando desconocido `" ++ cmd ++ "'. Escriba :? para recibir ayuda.")
                         return Noop
             [Cmd _ _ f _]
                 ->  do  return (f t)
             _   ->  do  putStrLn ("Comando ambigüo, podría ser " ++
                                   concat (intersperse ", " [ head cs | Cmd cs _ _ _ <- matching ]) ++ ".")
                         return Noop

     else
       return (Compile (CompileInteractive x))

commands :: [InteractiveCommand]
commands
  =  [ Cmd [":browse"]      ""        (const Browse) "Ver los nombres en scope",
       Cmd [":load"]        "<file>"  (Compile . CompileFile)
                                                     "Cargar un programa desde un archivo",
       Cmd [":print"]       "<exp>"   Print          "Imprime un término y sus ASTs sin evaluarlo",
       Cmd [":type"]        "<exp>"   Type           "Chequea el tipo de una expresión",
       Cmd [":quit",":Q"]        ""        (const Quit)   "Salir del intérprete",
       Cmd [":help",":?"]   ""        (const Help)   "Mostrar esta lista de comandos" ]

helpTxt :: [InteractiveCommand] -> String
helpTxt cs
  =  "Lista de comandos:  Cualquier comando puede ser abreviado a :c donde\n" ++
     "c es el primer caracter del nombre completo.\n\n" ++
     "<expr>                  evaluar la expresión\n" ++
     "let <var> = <expr>      definir una variable\n" ++
     unlines (map (\ (Cmd c a _ d) ->
                   let  ct = concat (intersperse ", " (map (++ if null a then "" else " " ++ a) c))
                   in   ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d) cs)

-- | 'handleCommand' interpreta un comando y devuelve un booleano
-- indicando si se debe salir del programa o no.
handleCommand ::  MonadPCF m => Command  -> m Bool
handleCommand cmd = do
   s@GlEnv {..} <- get
   case cmd of
       Quit   ->  return False
       Noop   ->  return True
       Help   ->  printPCF (helpTxt commands) >> return True
       Browse ->  do  printPCF (unlines [ name | name <- reverse (nub (map declName glb)) ])
                      return True
       Compile c ->
                  do  case c of
                          CompileInteractive e -> compilePhrase e
                          CompileFile f        -> put (s {lfile=f}) >> compileFile f
                      return True
       Print e   -> printPhrase e >> return True
       Type e    -> typeCheckPhrase e >> return True

compilePhrase ::  MonadPCF m => String -> m ()
compilePhrase x =
  do
    dot <- parseIO "<interactive>" declOrTm x
    case dot of 
      Left d  -> handleDecl d
      Right t -> handleTerm t

handleTerm ::  MonadPCF m => SMNTerm -> m ()
handleTerm t = do
         tt <- elab t
         s <- get
         ty <- tc tt (tyEnv s)
         te <- eval tt
         printPCF (pp te ++ " : " ++ ppTy ty)

printPhrase   :: MonadPCF m => String -> m ()
printPhrase x =
  do
    xSTy <- parseIO "<interactive>" tm x
    ex <- elab xSTy
    t  <- case xSTy of 
            (SV p f) -> maybe ex id <$> lookupDecl f
            _       -> return ex  
    printPCF "NTerm:"
    printPCF (show xSTy)
    printPCF "\nTerm:"
    printPCF (show t)

typeCheckPhrase :: MonadPCF m => String -> m ()
typeCheckPhrase x = do
         tSTy <- parseIO "<interactive>" tm x
         tt <- elab tSTy
         s <- get
         ty <- tc tt (tyEnv s)
         printPCF (ppTy ty)


--(fix (suma : Nat -> Nat -> Nat) (x : Nat) -> fun (y : Nat) -> ifz y then x else suma (succ x) (pred y)) 2 3 