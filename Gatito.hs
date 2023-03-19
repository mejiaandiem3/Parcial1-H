


--Andrea Mejia 192310177

--Juego de TIC TAC TOE
----------------------------------------------------------------------------------------------------------------------------------------------
--Se hace el uso de las librerias
import Data.List
import System.IO 


--Definir la constante: Es el maximo nivel de profundidad del arbol de analisis del juego. Cuanto mayor sea la profundidadDeBusqueda, mejor juega el computador pero su velocidad es menor.
--Por defecto es 6
profundidadDeBusqueda :: Int
profundidadDeBusqueda = 6

--Definir el tipo de dato Posicion para representar una posicion del tablero. Cada posicion es un entero del 1 a 9
type Posicion = Int
--Definir el tipo de datos Posiciones para representar listas de posiciones
type Posiciones = [Posicion]

--Definir el tipo de datos Tablero para representar los tableros. El tablero de la forma (Tab xs os) representa un tablero donde xs es la lista de posiciones
--donde estan colocadas las fichas del primer jugador y os la del segundo jugador.
data Tablero = Tab Posiciones Posiciones 
    deriving Show

--Definir la constante para representar el tablero inicial
tableroInicial::Tablero
tableroInicial = Tab [] []

--Definir la funcion tal que (turnoDeX t) se verifica si en el tablero t le toca mover al jugador X
turnoDeX :: Tablero -> Bool
turnoDeX (Tab xs os) =
    length xs == length os

--Definir la funcion tal que (pone t p) es el tablero obtenido poniendo en la posicion p del tablero t una ficha del jugador al que le corresponder colocar
pone :: Tablero -> Posicion -> Tablero
pone (Tab xs os) p =
    if turnoDeX (Tab xs os)
    then (Tab (p:xs) os)
    else (Tab xs (p:os))
    
--Definir la funcion tal que (completo t) se verifica si el tablero t esta completo; es decir, se han colocado las 9 fichas
completo :: Tablero -> Bool
completo (Tab xs os) =
    length xs + length os == 9

--Definir la funcion tal que (subconjunto s1 s2) se verifica si s1 es un subconjunto de s2
subconjunto::Posiciones -> Posiciones -> Bool
subconjunto s1 s2 =
    all (`elem` s2) s1

--Definir la funcion tal que (tineLinea ps) se verifica si la lista de posiciones ps contiene una linea horizontal, vertical o diagonal
tieneLinea :: Posiciones -> Bool
tieneLinea ps =
    subconjunto [1,2,3] ps || subconjunto [4,5,6] ps || subconjunto [7,8,9] ps ||
    subconjunto [1,4,7] ps || subconjunto [2,5,8] ps || subconjunto [3,6,9] ps ||
    subconjunto [1,5,9] ps || subconjunto [3,5,7] ps

--Definir la funcion tal que (tieneGanador t) se verifica si el tablero t tiene un ganador; es decir, alguno de los dos jugadores ha conseguido una linea
tieneGanador :: Tablero -> Bool
tieneGanador (Tab xs os) = tieneLinea xs || tieneLinea os

--Definir el tipo de datos Arbol para representar los arboles compuestos por nodos con una lista de hijos
data Arbol a = Nodo a [Arbol a]

--Definir la funcion tal que (muestraArbol t) es una cadena que representa el arbol t para una mejor visualizacion.
muestraArbol :: Show t => Arbol t -> String
muestraArbol (Nodo x xs) =
    show x ++ '\n' : (unlines . map(" "++) . concatMap (lines. show)) xs

instance Show a => Show (Arbol a) where
    show = muestraArbol

--Definir la funcion tal que (posicionesLibres t) es la lista de las posiciones libres del tablero t.
posicionesLibres :: Tablero -> Posiciones
posicionesLibres (Tab xs os) = 
    [1..9] \\ (xs++os)

--Definir la funcion tal que (siguientesTableros t) es la lista de tableros obtenidos colocando una pieza en cada una de las posiciones libres de t.
siguientesTableros :: Tablero -> [Tablero]
siguientesTableros t =
    if tieneGanador t
    then []
    else map (pone t) (posicionesLibres t)

--Definir la funcion tal que (construyeArbol t) es sel arbol de juego corresopondiente al tablero t
construyeArbol :: Tablero -> Arbol Tablero
construyeArbol t =
    Nodo t (map construyeArbol (siguientesTableros t))

--Definir el tipo Valor para representar el valor de lso tableros. Los valores son numeros enteros
type Valor = Int

--Definir la funcion tal que (valores vts) es la lista de valores de la lista de arboles de tableros valorados vts
valores :: [Arbol (Valor,Tablero)] -> [Valor]
valores vts =
    [v | Nodo (v,_) _ <- vts]


--Definir la funcion tal que (maximiza at) es el arbol de tableros maximamente valorados correspondiente al arbol de tableros at
maximiza :: Arbol Tablero -> Arbol (Valor,Tablero)
maximiza (Nodo t []) = Nodo (if tieneGanador t then -1 else 0,t)[]
maximiza (Nodo t ts) = Nodo (maximum (valores vts),t) vts
    where vts = map minimiza ts
--Definir la funcion tal que (minimiza at) es el arbol de tableros minimamente valorados - correspondiente al arbol de tableros at.
minimiza :: Arbol Tablero -> Arbol (Valor, Tablero)
minimiza (Nodo t []) = Nodo (if tieneGanador t then 1 else 0,t) []
minimiza (Nodo t ts) = Nodo (minimum (valores vts),t) vts
    where vts = map maximiza ts

--Definir la funcion tal que (poda n a) es el arbol obtenido podando el arbol a a apartir de la prfoundidad n.
poda :: Int -> Arbol a -> Arbol a
poda n (Nodo x xs) =
    Nodo x (if n==0
            then []
            else (map (poda (n-1))xs))
--Definir la funcion tal que (selecciona avts) es el tablero del primer hijo de la raiz del arbol de tableros valorados avts cuyo valor es igual que la raiz
selecciona :: Arbol(Valor,Tablero) -> Tablero
selecciona (Nodo (v,_)ts) =
    head [t | Nodo (v',t) _ <- ts, v'==v]

--Definir la funcion tal que (mejorMovimiento t) es el tablero correspondiente al mejor movimiento a partir del tablero t
mejorMovimiento :: Tablero -> Tablero
mejorMovimiento =
    selecciona.maximiza.poda profundidadDeBusqueda . construyeArbol



--Definir la funcion tal que (muestraPosicion t p) es el contenido de la posicion p del tablero t; es decir, X si p esta en la lista de las xs; [] si p esta en la lista de la os y la cadena de p, en otro caso.
muestraPosicion :: Tablero -> Posicion -> String
muestraPosicion (Tab xs os) p
    | p `elem` xs = "X"
    | p `elem` os = "O"
    | otherwise = show p

--Definir la funcion tal que (muestraLinea t ps) es la cadena correspondiente al contenido de las posiciones ps en el tablero t separadas por la barra vertical
muestraLinea :: Tablero -> [Posicion] -> String
muestraLinea t =
    concat.intersperse "|" . map(muestraPosicion t)

--Definir la funcion tal que (muestraTablero t) es la cadena correspondiente al tablero t
muestraTablero :: Tablero -> String
muestraTablero t =
    muestraLinea t [1..3] ++ "\n-+-+-\n" ++
    muestraLinea t [4..6] ++ "\n-+-+-\n" ++
    muestraLinea t [7..9]


----------------------------------------------------------------------------------------------------------------------------------------------
--Definir la función que controle el juego siguiendo los siguientes pasos:
--1. Activa la escritura inmediata en la pantalla.
--    2. Escribe el nombre del juego.
--    3. Escribe el tablero inicial.
--    4. Pregunta al humano si desea comenzar el juego.
--    5. Para y lee la respuesta.
--    6. Comprueba si la respuesta es afirmativa.
--    7. En el caso que la respuesta sea afirmativa, realiza un
--       movimiento del jugador humano.  
--    8. En el caso que la respuesta sea negativa, realiza un movimiento
--       de la computadora. 

main :: IO()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Tres en raya"
    putStrLn (muestraTablero tableroInicial)
    putStrLn "Comienza el juego? (s/n)"
    l <- getLine
    if head l `elem` ['s','S']
        then humano tableroInicial
        else computadora tableroInicial


----------------------------------------------------------------------------------------------------------------------------------------------



--Definir la funcion tal que (humano t) realiza el movimiento del jugador humano a partir del tablero t. Consta de los siguientes pasos:
-- Ejercicio 28. Definir la función
--    humano :: Tablero -> IO ()
-- tal que (humano t) realiza el movimiento del jugador humano a partir
-- del tablero t. Consta de los siguientes pasos:
--    1. Pregunta la posición en donde desea colocar la ficha.
--    2. Lee la posición en donde desea colocar la ficha.
--    3. Calcula el tablero t' correspondiente a colocar la ficha en la
--       posición elegida.
--    4. Muestra el tablero t'.
--    5. Decide si t' tiene ganador.
--       5.a. En caso afirmativo, escribe que el jugador humano ha ganado.
--       5.b. En caso negativo, decide si el tablero está completo
--            5.b.1. En caso afirmativo, escribe que hay empate.
--            5.b.2. En caso negativo, pasa el turno a la computadora
--                   con tablero t'. 
-- 
-- Nota: No se comprueba la corrección de la posición elegida (es decir, 
-- si es un número entre 1 y 9 y no hay ficha en esa posición).

humano :: Tablero -> IO()
humano t = do
    putStr "\nIndica el lugar donde colocar la ficha:"
    l <- getLine
    let t' = pone t (read l :: Posicion)
    putStrLn (muestraTablero t')
    if tieneGanador t'
        then putStrLn "Has Ganadao."
        else if (completo t')
                then putStrLn "Empate."
            else computadora t'


----------------------------------------------------------------------------------------------------------------------------------------------
--Definir la funcion tal que (computadora t) realiza el movimiento de la computadora a partir del tablero t. Consta de los siguientes pasos:
--    1. Escribe la jugada de la computadora 
--    2. Calcula el tablero t' correspondiente al mejor movimiento en
--       t. 
--    3. Escribe t'.
--    4. Decide si t' tiene ganador.
--       4.a. En caso afirmativo, escribe que la computadora ha ganado.
--       4.b. En caso negativo, decide si el tablero está completo.
--            4.b.1. En caso afirmativo, escribe que hay empate.
--            4.b.2. En caso negativo, pasa el turno al humano con
--                   tablero t'. 

computadora :: Tablero -> IO()
computadora t = do
    putStrLn "\nMi jugada:"
    let t' = mejorMovimiento t
    putStrLn (muestraTablero t')
    if tieneGanador t'
        then putStrLn "He ganado."
        else if (completo t')
            then putStrLn "Empate."
            else humano t'


