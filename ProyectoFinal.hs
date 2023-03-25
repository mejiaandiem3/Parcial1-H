import System.IO 

main :: IO ()
main = do
-- initializing list with an initial value
   let list=[]
-- invoking inputStrings functions
   inputStrings list



inputStrings :: [String] -> IO()
inputStrings  xs = do
-- getting user input
   print("Introduce un numero o (n/N) para salir")
   input <- getLine
   if (input `elem` ["n","N"])
   then do
    (print (crearDeLista (reverse (sToI(xs)))))
    (print (inorden (crearDeLista (sToI(xs)))))
   else (inputStrings (input:xs))

sToI :: [String] -> [Int]
sToI xr = map read xr 
--mapea la lista de strin para converitr cada uno en int


-- Estructura del árbol
data Abb a = Vacio 
    | Nodo a (Abb a) (Abb a) 
   deriving (Show,Read)


-- Crea un nuevo árbol a partir de una lista
-- crearDeLista (Lista de números)
crearDeLista :: (Ord a) => [a] -> Abb a
crearDeLista [] = Vacio
crearDeLista (raiz:sub) = Nodo raiz (crearDeLista (filter (<= raiz) sub)) (crearDeLista (filter (> raiz) sub))
--Raiz es la cabeza de la lista y lo restante es sub
--crea un nodo con la cabeza y enseguida se crean los otros nodos a partir de nuevo de la funcion usando un filtro en base a la raiz



-- Devuelve una lista de los nodos en recorrido inorden (izquierda, raíz, derecha)
-- inorden (Árbol)
inorden :: (Ord a) => Abb a -> [a]
inorden Vacio = []
inorden (Nodo actual izq der) = inorden izq ++ [actual] ++ inorden der
--Recorre primero el izquiero hasta al fondo y luego el numero y luego derecha



   


   