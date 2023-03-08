import GHC.IO.FD (stdout)
import System.Random


juego1 :: IO()
juego1 =
    do
        putStrLn "Piensa en un numero entre el 1 y el 100"
        adivina 1 100
        putStrLn "Fin del juego"

adivina :: Int -> Int -> IO()
adivina a b =
    do
        putStr ("Es " ++ show conjetura ++ "? [mayor/menor/exacto]")
        s <- getLine
        case s of
            "mayor" -> adivina (conjetura+1) b
            "menor" -> adivina a (conjetura-1)
            "exacto" -> return()
            _       -> adivina a b
    where
        conjetura = (a+b) `div` 2
    



juego2 :: IO()
juego2 =
    do
        hSetBuffering stdout NoBuffering
        n <- randomRIO (1::Int, 100)
        putStrLn "Tienes que adivinar un numero entre 1 y 100"
        
adivina1 :: Int -> IO()
adivina1 n =
            do
                putStr "Escribe un numero: "
                c <- getLine
                let x = read c
                case (compare x n) of
                    LT -> do putStrLn "Es alto"
                             adivina1 n 
                    GT -> do putStrLn "Es bajo"
                             adivina1 n 
                    EQ -> do putStrLn "Exacto"                   


