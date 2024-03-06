-- Importamos los módulos necesarios
import NLP.Tokenize

-- Función para tokenizar una oración
tokenizeSentence :: String -> [String]
tokenizeSentence sentence = simpleWordTokenizer sentence

-- Ejemplo de uso
main :: IO ()
main = do
    let sentence = "Hola, este es un ejemplo de tokenización en Haskell."
    putStrLn "Oración original:"
    putStrLn sentence
    putStrLn "Tokens:"
    print $ tokenizeSentence sentence

