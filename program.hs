{-
Entradas: Un caracter que representa la selección hecha por el usuario
Salidas: Dependiendo de la selección del usuario el sistema desplegará una funcionalidad u otra
Restricciones: El usuario debe seleccionar una opción válida
Objetivo: Desplegar el menú de opciones operativas para el usuario "administrador"
-}
menuOperativas :: IO ()
menuOperativas = do
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
        "1" -> putStrLn "Has seleccionado Información Comercial"
        "2" -> putStrLn "Has seleccionado Cargar y Mostrar Parqueos"
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