--Librerías utilizadas
import System.IO
import Data.List.Split (splitOn)
import System.Directory (doesFileExist)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
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
Entradas: La lista con todos los parqueos del sistema
Salidas: Toma cada parqueo y envía su información a la funcion imprimirInfoParqueo para que imprima la información del parqueo con el formato deseado
Restricciones: La lista recibida debe tener el formato adecuado
Objetivo: Imprimir la información de todos los parqueos
-}
imprimirListaParqueos :: [[String]] -> IO ()
imprimirListaParqueos listaParqueos = mapM_ imprimirInfoParqueo listaParqueos
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
            imprimirListaParqueos contenidoParseado
            menuOperativas
        else do
            putStrLn "La ruta ingresada no corresponde a un archivo válido."
            cargarParqueos
{-
Entradas: La lista con la información de todas las bicicletas del sistema
Salidas: imprime la información de cada bicicleta
Restricciones: La lista recibida debe tener el formato adecuado
Objetivo: Imprimir la información de una bicicleta
-}
imprimirInfoBicicletas :: [String] -> IO ()
imprimirInfoBicicletas [codigo, tipo] = do  --Bicicletas y el tipo de torque
  putStrLn "\n"
  putStrLn $ "Codigo: " ++ codigo
  putStrLn $ "Tipo: " ++ tipo
imprimirInfoBicicletas _ = putStrLn "La lista de bicicletas no tiene el formato esperado."
{-
Entradas: La lista con la información de todas las bicicletas
Salidas: Pasa la lista de todas las bicicletas a una función para imprimir cada bicicleta
Restricciones: La lista recibida debe tener el formato adecuado
Objetivo: Imprimir la información de todas las bicicletas del sistema
-}
imprimirListaBicicletas :: [[String]] -> IO ()  --Bicicletas y el tipo de torque
imprimirListaBicicletas bicicletas = mapM_ imprimirInfoBicicletas bicicletas
{-
Entradas: La ruta de todas las bicicletas del sistema
Salidas: Imprime la información de todas las bicicletas del sistema
Restricciones: n\a
Objetivo: Imprimir la información de todas las bicicletas del sistema
-}
verBicisSistema :: IO ()  --Bicicletas y el tipo de torque
verBicisSistema = do
    putStrLn "\n"
    putStrLn "****** Mostrando Bicicletas del sistema******"
    let ruta = "bicicletas.txt"
    contenido <- obtenerContenido ruta
    contenidoParseado <- return (parsearDocumento contenido)
    imprimirListaBicicletas contenidoParseado
    menuOperativas
{-
Entradas: La lista con la información de una ubicación de bicicletas
Salidas: imprime la información de cada ubicación de bicicleta
Restricciones: La lista recibida debe tener el formato adecuado
Objetivo: Imprimir la información de una ubicación de bicicleta
-}
imprimirInfoUbicacion :: [String] -> IO ()  --Bicicletas y su ubicacion
imprimirInfoUbicacion [codigoBicicleta, ubicacion] = do
  putStrLn "\n"
  putStrLn $ "Codigo Bicicleta: " ++ codigoBicicleta
  putStrLn $ "Ubicacion: " ++ ubicacion
imprimirInfoUbicacion _ = putStrLn "La lista de bicicletas no tiene el formato esperado."
{-
Entradas: La lista con la ubicación de todas las bicicletas
Salidas: Pasa la lista de todas las ubicaciones de bicicletas a una función para imprimir la información de cada ubicación
Restricciones: La lista recibida debe tener el formato adecuado
Objetivo: Imprimir la información de todas las ubicaciones de bicicletas
-}
imprimirListaUbicaciones :: [[String]] -> IO ()   --Bicicletas y su ubicacion
imprimirListaUbicaciones ubicaciones = mapM_ imprimirInfoUbicacion ubicaciones
{-
Entradas: La ruta de la información de todas las bicicletas del sistema
Salidas: Muestra en pantalla la información de todas las bicicletas del sistema
Restricciones: El archivo de bicicletas debe existir en el directorio
Objetivo: Mostrar la información de todas las bicicletas del sistema
-}
imprimirUbicacionesBicicletas :: IO ()   --Bicicletas y su ubicacion
imprimirUbicacionesBicicletas = do
    putStrLn "\n"
    putStrLn "****** Mostrando Bicicletas y ubicacion******"
    let ruta = "ubicacionesBicicletas.txt"
    contenido <- obtenerContenido ruta
    contenidoParseado <- return (parsearDocumento contenido)
    imprimirListaUbicaciones contenidoParseado
{-
Entradas: La lista de todas las bicicletas y sus ubicaciones
Salidas: Retorna la lista de bicicletas cuya ubicación sea "transito"
Restricciones: La lista recibida no debe estar vacía
Objetivo: Mostrar todas las bicicletas en estado de transito
-}
filtrarPorTransito :: [[String]] -> [[String]]
filtrarPorTransito listaBicicletas = filter (\[_, ubicacion] -> ubicacion == "transito") listaBicicletas
{-
Entradas: n\a
Salidas: Muestra la información de todas las bicicletas en transito
Restricciones: n\a
Objetivo: Mostrar todas las bicicletas en estado de transito
-}
imprimirBicicletasTransito :: IO ()
imprimirBicicletasTransito = do
    putStrLn "\n"
    putStrLn "****** Mostrando Bicicletas en tránsito ******"
    let ruta = "ubicacionesBicicletas.txt"
    contenido <- obtenerContenido ruta
    let contenidoParseado = parsearDocumento contenido
    let bicicletasEnTransito = filtrarPorTransito contenidoParseado
    imprimirListaUbicaciones bicicletasEnTransito
{-
Entradas: La lista de todos los parqueos del sistema y el nombre de un parque a buscar
Salidas: Retorna el parqueo cuyo nombre calza con el nombre de parqueo indicado
Restricciones: La lista recibida no debe estar vacía
Objetivo: Obtener el parqueo con el nombre indicado
-}
buscarParqueoPorNombre :: String -> [[String]] -> [String]
buscarParqueoPorNombre _ [] = []
buscarParqueoPorNombre nombreParqueo (parqueo:restoParqueos)
    | nombreParqueo == parqueo !! 1 = parqueo
    | otherwise = buscarParqueoPorNombre nombreParqueo restoParqueos
{-
Entradas: El codigo de un parqueo y La lista de todas las ubicaciones del sistema
Salidas: La lista de todas las ubicaciones del sistema que tengan como codigo del parqueo (segunda posicion de cada ubicacion) el codigo del parqueo indicado
Restricciones: El codigo de parqueo debe ser de un parqueo válido y La lista recibida no debe estar vacía
Objetivo: Obtener todas las ubicaciones de bicicleta que tengan en su ubicacion el codigo del parqueo indicado
-}
filtrarUbicacionesPorCodigoParqueo :: String -> [[String]] -> [[String]]
filtrarUbicacionesPorCodigoParqueo _ [] = []
filtrarUbicacionesPorCodigoParqueo codigoParqueo (ubicacion:restoUbicaciones)
    | codigoParqueo == ubicacion !! 1 = ubicacion : filtrarUbicacionesPorCodigoParqueo codigoParqueo restoUbicaciones
    | otherwise = filtrarUbicacionesPorCodigoParqueo codigoParqueo restoUbicaciones
{-
Entradas: El nombre del parqueo del que se deben consultar las bicicletas
Salidas: Muestra todas las bicicletas del parque indicado
Restricciones: Debe existir un parqueo con el nombre indicado
Objetivo: Mostrar la información de todas las bicicletas en el parque indicado
-}
consultarBicisParqueo :: String -> IO ()
consultarBicisParqueo nombreParqueo = do
    putStrLn "\n"
    putStrLn "****** Mostrando Bicicletas En parqueo******"
    let rutaParqueos = "parqueos.txt"
    contenidoParqueos <- obtenerContenido rutaParqueos  --leer el documento con los parqueos
    let listaParqueos = parsearDocumento contenidoParqueos --convertir el contenido en una lista de parqueos
    let parqueoEncontrado = buscarParqueoPorNombre nombreParqueo listaParqueos
    if null parqueoEncontrado
        then do
            putStrLn "No se encontró ningún parqueo con ese nombre"
            mostrarAsignarBicicletas
        else do
            let rutaUbicaciones = "ubicacionesBicicletas.txt"
            contenidoUbicaciones <- obtenerContenido rutaUbicaciones --leer el documento de ubicaciones
            let listaUbicaciones = parsearDocumento contenidoUbicaciones  --convertir el documento de ubicaciones en una lista de ubicaciones
            let codigoParqueo = parqueoEncontrado !! 0  --obtenemos el codigo del parqueo
            let listaUbicacionesDeEseParqueo = filtrarUbicacionesPorCodigoParqueo codigoParqueo listaUbicaciones
            imprimirListaUbicaciones listaUbicacionesDeEseParqueo
{-
Entradas: El usuario indica un nombre de parqueo
Salidas: Dependiendo del nombre del usuario:
        # muestra todas las bicicletas del sistema y sus ubicaciones
        transito muestra todas las bicicletas en transito
        nombreParqueo muestra todas las bicicletas en el parque indicado
Restricciones: Si el usuario indica un nombre que no se encuentra en la lista de parqueos entonces muestra un mensaje de error
Objetivo: Consultar las ubicaciones de cada bicicleta
-}
mostrarAsignarBicicletas :: IO ()
mostrarAsignarBicicletas = do
    putStrLn "\n"
    putStrLn "******Mostrar y asignar bicicletas******"
    putStrLn "Indique el nombre del parqueo: "
    nombreParqueo <- getLine
    if nombreParqueo == "#"
        then imprimirUbicacionesBicicletas
    else if nombreParqueo == "transito"
        then imprimirBicicletasTransito
    else
        consultarBicisParqueo nombreParqueo
    menuOperativas
{-
Entradas: La lista con la información de un usuario
Salidas: imprime la información de cada usuario
Restricciones: La lista recibida debe tener el formato adecuado
Objetivo: Imprimir la información de un usuario
-}
imprimirInfoUsuario :: [String] -> IO ()
imprimirInfoUsuario [cedula, nombre] = do
  putStrLn "\n"
  putStrLn $ "Cedula: " ++ cedula
  putStrLn $ "Nombre: " ++ nombre
imprimirInfoUsuario _ = putStrLn "La lista no tiene el formato esperado."
{-
Entradas: La lista con todos los usuarios del sistema
Salidas: Toma cada usuario y envía su información a la funcion imprimirInfoUsuario para que imprima la información del usuario con el formato deseado
Restricciones: La lista recibida debe tener el formato adecuado
Objetivo: Imprimir la información de todos los usuarios
-}
imprimirListaUsuarios :: [[String]] -> IO ()
imprimirListaUsuarios listaUsuarios = mapM_ imprimirInfoUsuario listaUsuarios
{-
Entradas: La ruta de la información de los usuarios
Salidas: Carga la información de los usuarios y los muestra en pantalla
Restricciones: Debe ser una ruta válida
Objetivo: Mostrar la información de los usuarios del sistema
-}
cargarUsuarios :: IO ()
cargarUsuarios = do
    putStrLn "\n"
    putStrLn "******Cargar Usuarios******"
    ruta <- pedirRuta
    archivoExiste <- doesFileExist ruta
    if archivoExiste
        then do
            contenido <- obtenerContenido ruta
            contenidoParseado <- return (parsearDocumento contenido)
            imprimirListaUsuarios contenidoParseado
            menuOperativas
        else do
            putStrLn "La ruta ingresada no corresponde a un archivo válido."
            cargarUsuarios
{-
Entradas: Un caracter que representa la selección hecha por el usuario
Salidas: Dependiendo de la selección del usuario el sistema desplegará una funcionalidad u otra
Restricciones: El usuario debe seleccionar una opción válida
Objetivo: Desplegar el menú de opciones operativas para el usuario "administrador"
-}
menuOperativas :: IO ()
menuOperativas = do
    putStrLn "\n"
    putStrLn "******Menú de Opciones Operativas******"
    putStrLn "1. Información Comercial"
    putStrLn "2. Cargar y Mostrar Parqueos"
    putStrLn "3. Ver bicicletas del sistema"
    putStrLn "4. Mostrar y Asignar Bicicletas"
    putStrLn "5. Cargar Usuarios"
    putStrLn "6. Estadísticas"
    putStrLn "7. Volver (Volver al Menú Principal)"
    putStrLn "Ingrese el número de la opción deseada:"
    opcion <- getLine
    case opcion of
        "1" -> infoComercial               
        "2" -> cargarParqueos
        "3" -> verBicisSistema
        "4" -> mostrarAsignarBicicletas
        "5" -> cargarUsuarios
        "6" -> putStrLn "Has seleccionado Estadísticas"
        "7" -> menuPrincipal
        _ -> do
            putStrLn "Opción inválida. Por favor, ingrese un número válido."
            menuOperativas
{-
Entradas: Un caracter
Salidas: true si el caracter ingresado es un numero, false si no lo es
Restricciones: debe recibirse un valor
Objetivo: Determinar si un caracter es un numero o ono lo es
-}
esNumero :: String -> Bool
esNumero num = all isDigit num
{-
Entradas: Una linea de caracteres que representa un numero
Salidas: El numero en formato double con la palabra reservada "Just" al inicio
Restricciones: debe recibirse un valor numérico
Objetivo: Convertir de char a numero double
-}
convertirANumero :: Char -> Maybe Double
convertirANumero c = case reads [c] of
  [(n, "")] -> Just n
  _ -> Nothing
{-
Entradas: Una linea de caracteres que representa un numero
Salidas: El numero en formato double 
Restricciones: debe recibirse un valor numérico
Objetivo: Convertir de char a numero double
-}
obtenerNumero :: Char -> Double
obtenerNumero c = fromMaybe 0 (convertirANumero c)
{-
Entradas: Cuatro números double
Salidas: la distancia euclidiana de los cuatro numeros recibidos
Restricciones: los parametros deben ser numericos
Objetivo: Calcular la distancia euclidiana entre dos puntos demarcados por pares ordenados
-}
calcularDistancia :: Double -> Double -> Double -> Double -> Double
calcularDistancia x1 y1 x2 y2 = sqrt ((x2 - x1)^2 + (y2 - y1)^2)
{-
Entradas: La latitud de la ubicacion del usuario, la longitud de la ubicacion del usuario, la distancia menor entre un parqueo y el punto indicado por el usuario y la lista de todos los parqueos
Salidas: Retorna el parqueo Mas cercano al punto indicado
Restricciones: los parametros deben ser numericos y listas
Objetivo: Conseguir el parqueo mas cercano al punto donde se encuentra el usuario que realiza la consulta
-}
conseguirParqueoMasCercano :: Double -> Double -> Double -> [String] -> [[String]] -> [String]
conseguirParqueoMasCercano latitudUsuario longitudUsuario distanciaMenor parqueoMasCercano [] = parqueoMasCercano
conseguirParqueoMasCercano latitudUsuario longitudUsuario distanciaMenor parqueoMasCercano listaParqueos = do
    let primerParqueo = listaParqueos !! 0
    let latitudPrimerParqueo = obtenerNumero (primerParqueo !! 4 !! 0)
    let longitudPrimerParqueo = obtenerNumero (primerParqueo !! 5 !! 0)
    let distanciaPrimerParqueo = calcularDistancia latitudUsuario longitudUsuario latitudPrimerParqueo longitudPrimerParqueo
    if distanciaMenor > distanciaPrimerParqueo
        then do
            let nuevaDistanciaMenor = distanciaPrimerParqueo
            let nuevoParqueoMasCercano = primerParqueo
            let nuevaListaParqueos = tail listaParqueos
            conseguirParqueoMasCercano latitudUsuario longitudUsuario nuevaDistanciaMenor nuevoParqueoMasCercano nuevaListaParqueos
        else do
            let nuevaListaParqueos = tail listaParqueos
            conseguirParqueoMasCercano latitudUsuario longitudUsuario distanciaMenor parqueoMasCercano nuevaListaParqueos
{-
Entradas: El usuario indica dos ejes, latitud y longitud
Salidas: El sistema despliega la información del parqueo más cercano a esa posición y muestra todas las bicicletas en ese parqueo
Restricciones: Los valores ingresados por el usuario deben ser numéricos
Objetivo: Mostrar el parqueo más cercano y las bicicletas en dicho parqueo
-}
obtenerParqueoMasCercano :: String -> String -> IO ()
obtenerParqueoMasCercano x y = do
    if esNumero x && esNumero y
        then do
            let ruta = "parqueos.txt"
            contenido <- obtenerContenido ruta
            listaParqueos <- return (parsearDocumento contenido)
            let primerParqueo = head listaParqueos
            let restoParqueos = tail listaParqueos
            let latitudPrimerParqueo = obtenerNumero (primerParqueo !! 4 !! 0)
            let longitudPrimerParqueo = obtenerNumero (primerParqueo !! 5 !! 0)
            let latitudUsuario = obtenerNumero (head x)
            let longitudUsuario = obtenerNumero (head y)
            let distancia = calcularDistancia latitudUsuario longitudUsuario latitudPrimerParqueo longitudPrimerParqueo
            let parqueoMasCercano = conseguirParqueoMasCercano latitudUsuario longitudUsuario distancia primerParqueo restoParqueos
            imprimirInfoParqueo parqueoMasCercano
            let nombreParqueo = parqueoMasCercano !! 1
            consultarBicisParqueo nombreParqueo
        else do
            putStrLn "Los valores ingresados deben ser numericos"
            consultarBicicletas
{-
Entradas: El usuario indica dos ejes, latitud y longitud
Salidas: El sistema despliega la información del parqueo más cercano a esa posición y muestra todas las bicicletas en ese parqueo
Restricciones: Los valores ingresados por el usuario deben ser numéricos
Objetivo: Mostrar el parqueo más cercano y las bicicletas en dicho parqueo
-}
consultarBicicletas :: IO ()   --consultar parqueo mas cercano a una ubicacion
consultarBicicletas = do
    putStrLn "\n"
    putStrLn "******Consultando bicicletas mas cercanas a su ubicación******"
    putStrLn "Ingrese su latitud (eje X): "
    latitudX <- getLine
    putStrLn "Ingrese su longitud (eje Y): "
    longitudY <- getLine
    obtenerParqueoMasCercano latitudX longitudY
    menuGenerales
{-
Entradas: La cedula de un usuario
Salidas: True si la cedula ingresada se encuentra entre las cédeulas de los usuarios del sistema, false si no
Restricciones: n\a
Objetivo: Verificaar que existe la cedula del usuario indicada en el sistema
-}
verificarCedulaUsuario :: String -> IO Bool
verificarCedulaUsuario cedula = do
    contenidoUsuarios <- obtenerContenido "usuarios.txt"
    let listaUsuarios = parsearDocumento contenidoUsuarios
    return (any (\usuario -> head usuario == cedula) listaUsuarios)
{-
Entradas: El codigo de un parqueo
Salidas: True si el codigo ingresado se encuentra entre los codigos de los parqueos del sistema, false si no
Restricciones: n\a
Objetivo: Verificaar que existe el codigo del parqueo en el sistema
-}
verificarCodigoParqueo :: String -> IO Bool
verificarCodigoParqueo codigoParqueo = do
    contenidoParqueos <- obtenerContenido "parqueos.txt"
    let listaParqueos = parsearDocumento contenidoParqueos
    return (any (\parqueo -> head parqueo == codigoParqueo) listaParqueos)

obtenerListaDeUbicacionesPorCodigoParqueo :: String -> IO [[String]]
obtenerListaDeUbicacionesPorCodigoParqueo codigoParqueo = do
    let rutaUbicaciones = "ubicacionesBicicletas.txt"
    contenidoUbicaciones <- obtenerContenido rutaUbicaciones
    let listaUbicaciones = parsearDocumento contenidoUbicaciones
    return (filter (\ubicacion -> codigoParqueo == last ubicacion) listaUbicaciones)

obtenerListaDeBicisEnListaUbicaciones :: [[String]] -> IO [[String]]
obtenerListaDeBicisEnListaUbicaciones listaUbicaciones = do
    let ruta = "bicicletas.txt"
    contenidoBicicletas <- obtenerContenido ruta
    let listaBicicletas = parsearDocumento contenidoBicicletas
    return (filter (\bicicleta -> any (\ubicacion -> head ubicacion == head bicicleta) listaUbicaciones) listaBicicletas)

obtenerListaDeBicicsEnParqueoPorCodigo :: String -> IO [[String]]
obtenerListaDeBicicsEnParqueoPorCodigo codigoParqueo = do
    ubicacionesConEseCodigoParqueo <- obtenerListaDeUbicacionesPorCodigoParqueo codigoParqueo
    bicicletasEnEseParqueo <- obtenerListaDeBicisEnListaUbicaciones ubicacionesConEseCodigoParqueo
    return bicicletasEnEseParqueo

{-
Entradas: El codigo de un parqueo de bicicletas
Salidas: Muestra en pantalla todas las bicicletas en dicho parqueo
Restricciones: El codigo del parqueo debe ser un parqueo válido dentro del sistema
Objetivo: Mostrar todas las bicicletas (codigo y tipo) en el parqueo del codigo suministrado
-}
consultarBicicletasEnParqueoPorCodigo :: String -> IO ()
consultarBicicletasEnParqueoPorCodigo codigoParqueo = do
    putStrLn "Mostrando las bicicletas en el parqueo indicado"
    listaDeBicicsEnParqueo <- obtenerListaDeBicicsEnParqueoPorCodigo codigoParqueo
    imprimirListaBicicletas listaDeBicicsEnParqueo
{-
Entradas: El usuario debe indicar cedula, codigo de parqueo de salida, codigo de parqueo de llegada
Salidas: Permite al usuario generar un alquiler y guardarlo en el sistema
Restricciones: La cedula indicada por el usuario debe ser una cedula valida dentro del sistema, los identificadores de salida, llegada y bicicleta deben ser codigos identificadores validos en el sistema
Objetivo: Generar una factura dentro del sistema
-}
alquilar :: IO ()
alquilar = do
    putStrLn "\n"
    putStrLn "******Generando alquiler******"
    putStrLn "Indique su cedula: "
    cedula <- getLine
    cedulaValida <- verificarCedulaUsuario cedula
    if cedulaValida
        then do
            putStrLn "Ingrese el código el parqueo de salida: "
            codigoParqueoSalida <- getLine
            putStrLn "Ingrese el código del parqueo de llegada"
            codigoParqueoLlegada <- getLine
            parqueoSalidaValido <- verificarCodigoParqueo codigoParqueoSalida
            parqueoLlegadaValido <- verificarCodigoParqueo codigoParqueoLlegada
            if parqueoSalidaValido && parqueoLlegadaValido
                then do
                    consultarBicicletasEnParqueoPorCodigo codigoParqueoSalida
                else do
                    putStrLn "Debe ingresar codigos de parqueo válidos"
                    alquilar
        else do
            putStrLn "La cedula ingresada no corresponde a una cedula válida"
            alquilar
    menuGenerales
{-
Entradas: Un caracter que representa la selección hecha por el usuario
Salidas: Dependiendo de la selección del usuario el sistema desplegará una funcionalidad u otra
Restricciones: El usuario debe seleccionar una opción válida
Objetivo: Desplegar el menú de opciones generales para el usuario general
-}
menuGenerales :: IO ()
menuGenerales = do
    putStrLn "\n"
    putStrLn "******Menú de Opciones Generales******"
    putStrLn "1. Consultar Bicicletas"
    putStrLn "2. Alquilar"
    putStrLn "3. Facturar"
    putStrLn "4. Volver (Volver al Menú Principal)"
    putStrLn "Ingrese el número de la opción deseada:"
    opcion <- getLine
    case opcion of
        "1" -> consultarBicicletas
        "2" -> alquilar
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
    putStrLn "******Menú Principal******"
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