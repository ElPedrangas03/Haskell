import Data.Char (toUpper)
import Data.List (foldl')
-- Parte 1 de la tarea

-- Función para aplicar un descuento a un precio
aplicarDescuento :: Double -> Double -> Double
aplicarDescuento precio descuento = precio - (precio * (descuento/100))

-- Función para aplicar el IVA a un precio
aplicarIVA :: Double -> Double
aplicarIVA precio = precio * 1.16

-- Función que recibe un diccionario de precios y porcentajes y una función para aplicar descuentos o IVA
calcularPrecioFinal :: [(Float, Float)] -> (Float -> Float -> Float) -> Float
calcularPrecioFinal basket function = sum [function price discount | (price, discount) <- basket]

-- Parte 2 de la tarea

-- Definición de la función aplicaFuncionLista
aplicaFuncionLista :: (a -> b) -> [a] -> [b]
aplicaFuncionLista _ []     = []             -- Caso base: lista vacía
-- Aplicar la función a cada elemento y recursión sobre el resto de la lista
aplicaFuncionLista f (x:xs) = f x : aplicaFuncionLista f xs  

-- Definición de la función cuadrado
cuadrado :: Num a => a -> a
cuadrado n = n * n

-- Parte 3 de la tarea

-- Partimos la palabra que nos den y regresamos las palabras y longitud de cada una
partirPalabras :: String -> [(String, Int)]
partirPalabras salida = map (\w -> (w, length w)) (words salida)

-- Parte 4 de la tarea

-- Convertimos lo que tengamos de calificacion al texto correspondiente
convertirCalificacion :: Float -> String
convertirCalificacion nota
    |   nota >= 95 = "Excelente"
    |   nota >= 85 = "Notable"
    |   nota >= 75 = "Bueno"
    |   nota >= 70 = "Suficiente"
    |   otherwise = "Desempeño insuficiente"

-- Regresamos la coleccion con las asignaturas en mayusculas y su respectiva calificacion
calificaciones :: [(String, Float)] -> [(String, String)]
calificaciones = map (\(asignatura, nota) -> (map toUpper asignatura, convertirCalificacion nota))

-- Parte 5 de la tarea

-- Tomamos el modulo de un vector dado
moduloDeVector :: Floating a => [a] -> a
moduloDeVector = sqrt . sum . map (^2)

-- Parte 6 de la tarea

-- Función para calcular la media de una lista de números
media :: (Fractional a) => [a] -> a
media xs = sum xs / fromIntegral (length xs)

-- Función para calcular la desviación estándar de una lista de números
desviacionEstandar :: (Floating a) => [a] -> a
desviacionEstandar xs =
  let m = media xs
      n = fromIntegral $ length xs
      squaredDiffs = map (\x -> (x - m) ^ 2) xs
  in sqrt (sum squaredDiffs / n)

-- Función para determinar si un valor es atípico en función de la media y la desviación estándar
atipico :: (Floating a, Ord a) => [a] -> a -> Bool
atipico muestra n =
  let mediaMuestra = media muestra
      desviacion = desviacionEstandar muestra
      puntuacion = (n - mediaMuestra) / desviacion
  in puntuacion < -3 || puntuacion > 3

-- Función para filtrar los datos atípicos de una muestra
datosAtipicos :: (Floating a, Ord a) => [a] -> [a]
datosAtipicos muestra = filter (atipico muestra) muestra