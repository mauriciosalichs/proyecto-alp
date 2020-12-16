module ParameterChecker where

import Text.Read (readMaybe)

checkNumber :: String -> Either String String
checkNumber s = case (readMaybe s :: Maybe Double) of
                   Nothing -> Left $ "ADVERTENCIA: "++s++" no es un tamaño válido. Se usará el tamaño por defecto."
                   Just m -> Right s

checkColor :: String -> Either String String
checkColor "rojo" = Right "red"
checkColor "verde" = Right "green"
checkColor "azul" = Right "blue"
checkColor "amarillo" = Right "yellow"
checkColor "negro" = Right "black"
checkColor "blanco" = Right "white"
checkColor s = Left $ "ADVERTENCIA: "++s++" no es un color traducible. El nombre debe ingresarse en inglés."

checkAllign :: String -> Either String String
checkAllign "izquierda" = Right "left"
checkAllign "derecha" = Right "right"
checkAllign "centrado" = Right "center"
checkAllign "centro" = Right "center"
checkAllign "justificado" = Right "justify"
checkAllign a = Left $ "ADVERTENCIA: "++a++" no es una alineación válida. Se usará la alineación por defecto."
