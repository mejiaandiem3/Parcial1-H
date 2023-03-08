import GHC.IO.FD (stdout)
import System.Random

juego1 :: IO()
juego1 =
    do  
        putStrLn "Piensa un numero entre el 1 y el 100"
        adivina 1 100 1
        putStrLn "Fin del juego"

adivina :: Int -> Int -> Int -> IO()
adivina a b c =
    do 
        putStr ("Es "++ show conjetura ++ "?[mayor/menor/exacto]")
        s <- getLine
        case s of
            "mayor" -> adivina (conjetura+1) b (c+1)
            "menor" -> adivina a (conjetura-1) (c+1)
            "exacto" -> putStrLn ("Numero de intentos: " ++ show c)
            _        -> adivina a b c

    where
        conjetura = (a+b)  `div` 2


juego2 :: IO()
juego2 = 
    do
        n <- randomRIO (1::Int, 100)
        putStrLn "Tienes que adivinar un numero entre el 1 y 100"
        adivina1 n 1

adivina1 :: Int -> Int -> IO()
adivina1 n s =
    do
        putStr "Escribe un numero: "
        c <- getLine
        let co = s
        let x = read c
        case (compare x n) of
            LT -> do putStrLn "Mas alto"
                     adivina1 n (co+1)
            GT -> do putStrLn "Mas bajo"
                     adivina1 n (co+1)
            EQ -> do putStrLn "Exacto"
                     putStrLn ("Numero de intentos: " ++ show co)

                     
            
