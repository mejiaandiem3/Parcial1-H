import Data.List
import System.IO

profundidadDeBusqueda :: Int
profundidadDeBusqueda = 6

type posicion = Int
type posiciones = [posicion]

data tablero = Tab posiciones posiciones
                deriving show


tableroinicial :: tablero
tableroInicial = Tab [] []

turnodex :: tablero -> Bool
turnodex (Tab xs os) =
    (length xs == length os)




pone :: tablero -> posicion -> tablero
pone (Tab xs os) p =
    if turnoDeX (Tab xs os)
    then (Tab (p:xs) os)
    else (Tab xs (p:os))

completo :: tablero -> Bool
completo (Tab xs os) =
    length xs + length os == 9


subconjunto :: Posiciones -> posiciones -> Bool
subconjunto s1 s2 =
    all (`elem` s2) s1


tienelinea :: posiciones -> Bool
tienelinea ps = 
    subconjunto [1,2,3] ps || subconjunto [4,5,6] ps || subconjunto [7,8,9] ps ||
    subconjunto [1,4,7] ps || subconjunto [2,5,8] ps || subconjunto [3,6,9] ps ||
    subconjunto [1,5,9] ps || subconjunto [3,5,7] ps

tieneganador :: tablero -> Bool
tieneganador (Tab xs os) =
    tienelinea xs || tienelinea os


data arbol a = Nodo a [arbol a]

muestraarbol (Nodo x xs)=
    show x ++ '\n' : (unlines . map (" "++) . concatMap(lines . show))

instance Show a => Show (Arbol a) where 
    show = muestraarbol

posicioneslibres :: tablero ->posiciones
posicioneslibres (Tab xs os) =
    [1..9] \\ (xs++os)

siguientestableros :: tablero -> [tablero]
siguientestableros t =
    if tieneganador t
    then []
    else map (pone t) (posicioneslibres t)

construyearbol