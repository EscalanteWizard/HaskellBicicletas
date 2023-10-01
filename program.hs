--Librerías utilizadas
import System.IO
import Data.List.Split (splitOn)
import System.Directory (doesFileExist)
{-
Entradas: un string
Salidas: El string recibido parseado en las comas
Restricciones: el string debe contener comas
Objetivo: Parsear una cadena de texto en sus comas
-}
parsearLinea :: String -> [String]
parsearLinea linea = splitOn "," linea
{-
Entradas: El contenido de un documento de texto
Salidas: El contenido del documento convertido en una lista, donde cada posicion de la lista es un renglón del documento cuyo texto fue ingresado
Restricciones: El documento debe contener varios renglones para que tenga sentido aplicar la función
Objetivo: Parsear un documento en sus renglones
-}
parsearDocumento :: String -> [[String]]
parsearDocumento contenido = map (splitOn ",") (lines contenido)
{-
Entradas: La ruta de un documento de texto
Salidas: Devuelve la ruta ingresada por el usuario como una variable
Restricciones: Debe ser una ruta válida
Objetivo: Obtener la ruta de un documento de texto
-}
pedirRuta :: IO String
pedirRuta = do
    putStrLn "Ingresa la ruta del archivo:"
    getLine
{-
Entradas: La ruta de un documento de texto
Salidas: Devuelve el contenido del archivo en la ruta ingresada por el usuario
Restricciones: Debe ser una ruta válida
Objetivo: Obtener el contenido del documento de texto indicado
-}
obtenerContenido :: String -> IO String
obtenerContenido ruta = do
    contenido <- readFile ruta
    return contenido
{-
Entradas: La lista con la información del comercio
Salidas: imprime la información de la empresa de bicicletas
Restricciones: La lista recibida debe tener el formato adecuado
Objetivo: Imprimir la información de la empresa de bicicletas
-}
imprimirInfoComercial :: [String] -> IO ()
imprimirInfoComercial [nombre, web, main, tarifaPedal, tarifaElectrica, tarifaGasolina] = do
  putStrLn $ "Nombre: " ++ nombre
  putStrLn $ "Web: " ++ web
  putStrLn $ "Main: " ++ main
  putStrLn $ "Tarifa Pedal: " ++ tarifaPedal
  putStrLn $ "Tarifa Electrica: " ++ tarifaElectrica
  putStrLn $ "Tarifa Gasolina: " ++ tarifaGasolina
imprimirInfoComercial _ = putStrLn "La lista no tiene el formato esperado."
{-
Entradas: La ruta de la información de la empresa de bicicletas
Salidas: Imprime en pantalla la información del comercio
Restricciones: Debe ser una ruta válida
Objetivo: Mostrar la información de la empresa de bicicletas
-}
infoComercial :: IO ()
infoComercial = do
    putStrLn "******Información de la empresa******"
    let ruta = "infoEmpresa.txt"
    contenido <- readFile ruta
    contenidoParseado <- return (parsearLinea contenido)
    imprimirInfoComercial contenidoParseado
    menuOperativas
{-
Entradas: La lista con la información de un parqueo de bicicletas
Salidas: imprime la información de cada parqueo de bicicletas
Restricciones: La lista recibida debe tener el formato adecuado
Objetivo: Imprimir la información de un parqueo de bicicletas
-}
imprimirInfoParqueo :: [String] -> IO ()
imprimirInfoParqueo [codigo, nombre, ubicacion, provincia, x, y] = do
  putStrLn "\n"
  putStrLn $ "Codigo: " ++ codigo
  putStrLn $ "Nombre: " ++ nombre
  putStrLn $ "Ubicacion: " ++ ubicacion
  putStrLn $ "Provincia: " ++ provincia
  putStrLn $ "Latitud: " ++ x
  putStrLn $ "Longitud: " ++ y
imprimirInfoParqueo _ = putStrLn "La lista no tiene el formato esperado."
{-
Entradas: La lista con la información de todos los parqueos de bicicletas
Salidas: Imprime la información de cada uno de los parqueos de bibicletas
Restricciones: La lista recibida debe tener el formato adecuado
Objetivo: Imprimir la información de todos los parques de la lista de bicicletas
-}
imprimirInfoParqueos :: [[String]] -> IO ()
imprimirInfoParqueos parqueos = mapM_ imprimirInfoParqueo parqueos
{-
Entradas: La ruta de la información de los parqueos de bicicletas
Salidas: Carga la información de los parqueos desde un archivo txt e imprime la información de los parqueos en pantalla
Restricciones: Debe ser una ruta válida
Objetivo: Mostrar la información de los parqueos de bicicletas
-}
cargarParqueos :: IO ()
cargarParqueos = do
    putStrLn "\n"
    putStrLn "******Cargar Parqueos******"
    ruta <- pedirRuta
    archivoExiste <- doesFileExist ruta
    if archivoExiste
        then do
            contenido <- obtenerContenido ruta
            contenidoParseado <- return (parsearDocumento contenido)
            imprimirInfoParqueos contenidoParseado
            menuOperativas
        else do
            putStrLn "La ruta ingresada no corresponde a un archivo válido."
            cargarParqueos
{-
Entradas: Un caracter que representa la selección hecha por el usuario
Salidas: Dependiendo de la selección del usuario el sistema desplegará una funcionalidad u otra
Restricciones: El usuario debe seleccionar una opción válida
Objetivo: Desplegar el menú de opciones operativas para el usuario "administrador"
-}
menuOperativas :: IO ()
menuOperativas = do
    putStrLn "\n"
    putStrLn "Menú de Opciones Operativas"
    putStrLn "1. Información Comercial"
    putStrLn "2. Cargar y Mostrar Parqueos"
    putStrLn "3. Mostrar y Asignar Bicicletas"
    putStrLn "4. Cargar Usuarios"
    putStrLn "5. Estadísticas"
    putStrLn "6. Volver (Volver al Menú Principal)"
    putStrLn "Ingrese el número de la opción deseada:"
    opcion <- getLine
    case opcion of
        "1" -> infoComercial               
        "2" -> cargarParqueos
        "3" -> putStrLn "Has seleccionado Mostrar y Asignar Bicicletas"
        "4" -> putStrLn "Has seleccionado Cargar Usuarios"
        "5" -> putStrLn "Has seleccionado Estadísticas"
        "6" -> menuPrincipal
        _ -> do
            putStrLn "Opción inválida. Por favor, ingrese un número válido."
            menuOperativas
{-
Entradas: Un caracter que representa la selección hecha por el usuario
Salidas: Dependiendo de la selección del usuario el sistema desplegará una funcionalidad u otra
Restricciones: El usuario debe seleccionar una opción válida
Objetivo: Desplegar el menú de opciones generales para el usuario general
-}
menuGenerales :: IO ()
menuGenerales = do
    putStrLn "\n"
    putStrLn "Menú de Opciones Generales"
    putStrLn "1. Consultar Bicicletas"
    putStrLn "2. Alquilar"
    putStrLn "3. Facturar"
    putStrLn "4. Volver (Volver al Menú Principal)"
    putStrLn "Ingrese el número de la opción deseada:"
    opcion <- getLine
    case opcion of
        "1" -> putStrLn "Has seleccionado Consultar Bicicletas"
        "2" -> putStrLn "Has seleccionado Alquilar"
        "3" -> putStrLn "Has seleccionado Facturar"
        "4" -> menuPrincipal
        _ -> do
            putStrLn "Opción inválida. Por favor, ingrese un número válido."
            menuGenerales
{-
Entradas: Un caracter que representa la selección hecha por el usuario
Salidas: Dependiendo de la selección del usuario el sistema desplegará una funcionalidad u otra
Restricciones: El usuario debe seleccionar una opción válida
Objetivo: Desplegar el menú principal que se muestra en el programa
-}
menuPrincipal :: IO ()
menuPrincipal = do
    putStrLn "\n"
    putStrLn "Menú Principal"
    putStrLn "1. Opciones Operativas"
    putStrLn "2. Opciones Generales"
    putStrLn "3. Salir del Programa"
    putStrLn "Ingrese el número de la opción deseada:"
    opcion <- getLine
    case opcion of
        "1" -> menuOperativas
        "2" -> menuGenerales
        "3" -> putStrLn "Saliendo del programa..."
        _ -> do
            putStrLn "Opción inválida. Por favor, ingrese un número válido."
            menuPrincipal

main :: IO ()
main = do
    menuPrincipal