mmes :: Integer -> String
mmes x 
     | x == 01 = "enero"
     | x == 02 = "febrero"
     | x == 03 = "marzo"
     | x == 04 = "abril"
     | x == 05 = "mayo"
     | x == 06 = "junio"
     | x == 07 = "julio"
     | x == 08 = "agosto"
     | x == 09 = "septiembre"
     | x == 10 = "octubre"
     | x == 11 = "noviembre"
     | x == 12 = "diciembre"

fechan :: Integer -> Integer -> Integer -> String
fechan dia mes anio = show dia ++ " de " ++ mmes mes ++ " del " ++ show anio
