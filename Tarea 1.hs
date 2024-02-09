import Data.Char (toUpper)
import Data.List (foldl')
-- Parte 1 de la tarea

-- Función para aplicar un descuento a un precio
-- Entrada: 100 30
-- Salida: 70
aplicarDescuento :: Double -> Double -> Double
aplicarDescuento precio descuento = precio - (precio * (descuento/100))

-- Función para aplicar el IVA a un precio
-- Entrada: 100 16
-- Salida: 116
aplicarIVA :: Double -> Double -> Double
aplicarIVA precio descuento = precio + (precio * (descuento/100))

-- Función que recibe un diccionario de precios y porcentajes y una función para aplicar descuentos o IVA
-- Entrada: [(100, 30), (200, 25)] aplicarDescuento
-- Salida: 220
calcularPrecioFinal :: [(Float, Float)] -> (Float -> Float -> Float) -> Float
calcularPrecioFinal basket function = sum [function price discount | (price, discount) <- basket]

-- Parte 2 de la tarea

-- Función para aplicar una función a cada elemento de una lista
-- Entrada: (cuadrado, [1, 2, 3, 4])
-- Salida: [1, 4, 9, 16]
aplicaFuncionLista :: (a -> b) -> [a] -> [b]
aplicaFuncionLista _ []     = []             
aplicaFuncionLista f (x:xs) = f x : aplicaFuncionLista f xs  

-- Función para calcular el cuadrado de un número
-- Entrada: 3
-- Salida: 9
cuadrado :: Num a => a -> a
cuadrado n = n * n

-- Parte 3 de la tarea

-- Función para dividir una cadena de texto en palabras y devolver una lista de tuplas con cada palabra y su longitud
-- Entrada: "Hola mundo"
-- Salida: [("Hola", 4), ("mundo", 5)]
partirPalabras :: String -> [(String, Int)]
partirPalabras salida = map (\w -> (w, length w)) (words salida)


-- Parte 4 de la tarea

-- Función para convertir una calificación numérica en un texto descriptivo
-- Entrada: 85
-- Salida: "Notable"
convertirCalificacion :: Float -> String
convertirCalificacion nota
    |   nota >= 95 = "Excelente"
    |   nota >= 85 = "Notable"
    |   nota >= 75 = "Bueno"
    |   nota >= 70 = "Suficiente"
    |   otherwise = "Desempeño insuficiente"

-- Función para convertir las calificaciones de una lista de tuplas de (asignatura, nota) a (asignatura en mayúsculas, calificación)
-- Entrada: [("Matemáticas", 80), ("Física", 90)]
-- Salida: [("MATEMÁTICAS", "Notable"), ("FÍSICA", "Excelente")]
calificaciones :: [(String, Float)] -> [(String, String)]
calificaciones = map (\(asignatura, nota) -> (map toUpper asignatura, convertirCalificacion nota))

-- Parte 5 de la tarea

-- Función para calcular el módulo de un vector
-- Entrada: [3, 4]
-- Salida: 5.0
moduloDeVector :: Floating a => [a] -> a
moduloDeVector = sqrt . sum . map (^2)

-- Parte 6 de la tarea

-- Función para filtrar los valores atípicos en una muestra
-- Entrada: [1, 2, 3, 4, 5, 1000]
-- Salida: [1000]
datosAtipicos :: (Floating a, Ord a) => [a] -> [a]
datosAtipicos muestra = filter (atipico muestra) muestra

-- Función para determinar si un valor es atípico en función de la media y la desviación estándar
atipico :: (Floating a, Ord a) => [a] -> a -> Bool
atipico muestra n =
  let mediaMuestra = media muestra
      desviacion = desviacionEstandar muestra
      puntuacion = (n - mediaMuestra) / desviacion
  in puntuacion < -3 || puntuacion > 3

-- Función para calcular la media de una lista de números
-- Entrada: [1, 2, 3, 4, 5]
-- Salida: 3.0
media :: (Fractional a) => [a] -> a
media xs = sum xs / fromIntegral (length xs)

-- Función para calcular la desviación estándar de una lista de números
-- Entrada: [1, 2, 3, 4, 5]
-- Salida: 1.4142135623730951
desviacionEstandar :: (Floating a) => [a] -> a
desviacionEstandar xs =
  let m = media xs
      n = fromIntegral $ length xs
      squaredDiffs = map (\x -> (x - m) ^ 2) xs
  in sqrt (sum squaredDiffs / n)
