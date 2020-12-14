module ParameterChecker where

import Text.Read (readMaybe)

checkNumber :: String -> IO (Maybe String)
checkNumber s = case (readMaybe s :: Maybe Double) of
                   Nothing -> do putStrLn $ "ADVERTENCIA: "++s++" no es un tamaño válido. Se usará el tamaño por defecto."
                                 return Nothing
                   Just m -> return $ Just s

checkColor :: String -> IO String
checkColor "rojo" = return "red"
checkColor "verde" = return "green"
checkColor "azul" = return "blue"
checkColor "amarillo" = return "yellow"
checkColor "negro" = return "black"
checkColor "blanco" = return "white"
checkColor s = do putStrLn $ "ADVERTENCIA: "++s++" no es un color traducible. El nombre debe ingresarse en inglés."
                  return s

checkAllign :: String -> IO (Maybe String)
checkAllign "izquierda" = return $ Just "left"
checkAllign "derecha" = return $ Just "right"
checkAllign "centrado" = return $ Just "center"
checkAllign "centro" = return $ Just "center"
checkAllign "justificado" = return $ Just "justify"
checkAllign a = do putStrLn $ "ADVERTENCIA: "++a++" no es una alineación válida. Se usará la alineación por defecto."
                   return $ Just a
