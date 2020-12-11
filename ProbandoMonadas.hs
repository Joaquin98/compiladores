import Control.Monad.Writer
import Control.Monad.State.Lazy

main :: IO ()
main = putStrLn $ show $ funn [1..10] 0
    
funn :: [Int] -> Int -> ([[String]],Int)
funn [] init = ([], init)
funn (x:xs) init = let ((val,state),list) = runWriter (runStateT fun init)
                   in (list:(fst (funn xs state)),snd (funn xs state))

fun :: StateT Int (Writer [String]) ()
fun = do tell ["hola"]
         modify (1+)
         tell ["chau"]
         return ()

{-
Resultado:
([["hola","chau"],["hola","chau"],["hola","chau"],["hola","chau"],
["hola","chau"],["hola","chau"],["hola","chau"],["hola","chau"],
["hola","chau"],["hola","chau"]],10)
-}