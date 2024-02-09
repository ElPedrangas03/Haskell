import Data.Char (toUpper)
-- Parte 1 de la tarea

-- Calcular el seno
-- Entrada: 1.0
-- Salida: 0.8414709848078965
calcularSeno :: Double -> Double
calcularSeno x = sin x

-- Calcular el coseno
-- Entrada: 1.0
-- Salida: 0.5403023058681398
calcularCoseno :: Double -> Double
calcularCoseno x = cos x

-- Calcular la tangente
-- Entrada: 1.0
-- Salida: 1.5574077246549023
calcularTangente :: Double -> Double
calcularTangente x = tan x

-- Calcular el exponencial de un numero
-- Entrada: 1.0
-- Salida: 2.718281828459045
calcularExponencial :: Double -> Double
calcularExponencial x = exp x

-- Calcular el logaritmo neperiano a un numero
-- Entrada: 10.0
-- Salida: 2.302585092994046
calcularLogaritmoNeperiano :: Double -> Double
calcularLogaritmoNeperiano x = log x

-- Funcion que recibe el metodo a usar y el numero maximo a iterar
-- Entrada: calcularSeno 1
-- Salida: [(1.0, 0.8414709848078965)]
calcularFuncion :: (Double -> Double) -> Int -> [(Int, Double)]
calcularFuncion f n = [(x, f $ fromIntegral x) | x <- [1..n]]

-- Parte 2 de la tarea

-- Función para filtrar los elementos de una lista según una función booleana
-- Entrada: (esPar, [1, 2, 3, 4, 5, 6])
-- Salida: [2, 4, 6]
-- Donde "esPar" puede ser cualquier metodo de comparacion de booleanos
filtraLista :: (a -> Bool) -> [a] -> [a]
filtraLista _ [] = []
filtraLista f (x:xs)
    | f x       = x : filtraLista f xs
    | otherwise = filtraLista f xs

-- Función para verificar si un número es par
-- Entrada: 2
-- Salida: True
esPar :: Int -> Bool
esPar n = n `mod` 2 == 0

-- Parte 3 de la tarea

-- Funcion para poder convertir una calificacion a su correspondiente a texto
-- Entrada: 90
-- Salida: Notable
convertirCalificaciones :: Double -> String
convertirCalificaciones nota
    | nota >= 95 = "Excelente"
    | nota >= 85 = "Notable"
    | nota >= 75 = "Bueno"
    | nota >= 70 = "Suficiente"
    | otherwise  = "Desempeño insuficiente"

-- Funcion que recibe una lista de calificaciones y regresa otra lista, pero con su
-- texto correspondiente
-- Entrada: [90, 60]
-- Salida: ["Notable", "Desempeño insuficiente"]
calificaciones :: [Double] -> [String]
calificaciones = map convertirCalificaciones

-- Parte 4 de la tarea

-- Función para convertir las calificaciones de una lista de tuplas de (asignatura, nota) a (asignatura en mayúsculas, calificación)
-- Entrada: [("Matemáticas", 80), ("Física", 90)]
-- Salida: [("MATEMÁTICAS", "Notable"), ("FÍSICA", "Excelente")]
calificacionesDiccionario :: [(String, Double)] -> [(String, String)]
calificacionesDiccionario = map (\(asignatura, nota) -> (map toUpper asignatura, convertirCalificaciones nota))

-- Parte 5 de la tarea

-- Definición de un tipo Inmueble que representa las características de un inmueble.
type Inmueble = (Int, Int, Int, Bool, Char)

-- Definición de un tipo Presupuesto para representar el presupuesto máximo disponible.
type Presupuesto = Int

-- Función precioInmueble calcula el precio de un inmueble según ciertas características y la zona.
-- Entrada: Una tupla que contiene el año de construcción, metros cuadrados, número de habitaciones, 
--          si tiene garaje o no (True/False) y la zona ('A' o 'B').
-- Salida: El precio del inmueble calculado según la fórmula
precioInmueble :: Inmueble -> Float
precioInmueble (año, metros, habitaciones, garaje, zona)
    | zona == 'A' = fromIntegral (metros * 1000 + habitaciones * 5000 + if garaje then 15000 else 0) * (1 - fromIntegral (2024 - año) / 100)
    | zona == 'B' = fromIntegral (metros * 1000 + habitaciones * 5000 + if garaje then 15000 else 0) * (1 - fromIntegral (2024 - año) / 100) * 1.5
    | otherwise = 0.0

-- Función buscarInmuebles filtra una lista de inmuebles según el presupuesto máximo dado.
-- Entrada: Una lista de tuplas que representan los inmuebles, y un presupuesto máximo.
-- Salida: Una lista de inmuebles cuyo precio es menor o igual al presupuesto, con el precio de cada inmueble.
buscarInmuebles :: [Inmueble] -> Presupuesto -> [Inmueble]
buscarInmuebles inmuebles presupuesto = filter (\x -> precioInmueble x <= fromIntegral presupuesto) inmuebles