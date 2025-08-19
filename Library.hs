-- ST0244 - Programming Languages and Computing Paradigms
-- PRACTICE I - Library Lending Manager (Haskell)
-- EAFIT University - August 2025

import Data.Time.Clock
import Data.List (find)
import System.IO
import Control.Exception
import Control.Concurrent (threadDelay)
import Data.Maybe (isNothing)

-- ===============================
-- Modelo de datos
-- ===============================

-- Un préstamo de libro (en curso si 'devolucion' es Nothing)
data Prestamo = Prestamo
  { bookId     :: String      -- ID único del libro
  , prestamo   :: UTCTime     -- instante del préstamo
  , devolucion :: Maybe UTCTime -- instante de devolución (Nothing si sigue prestado)
  } deriving (Show, Read)

-- ===============================
-- Persistencia en archivo
-- ===============================

archivoBD :: FilePath
archivoBD = "Library.txt"

-- Guarda la lista completa en disco (1 préstamo por línea con 'show')
guardarBiblioteca :: [Prestamo] -> IO ()
guardarBiblioteca ps = do
  let contenido = unlines (map show ps)
  resultado <- reintentar 5 (writeFile archivoBD contenido)
  case resultado of
    Left ex  -> putStrLn $ "Error guardando la biblioteca: " ++ show ex
    Right _  -> putStrLn $ "Biblioteca guardada en el archivo " ++ archivoBD ++ "."

-- Carga la lista completa desde disco (si no existe o hay error, retorna [])
cargarBiblioteca :: IO [Prestamo]
cargarBiblioteca = do
  e <- try (readFile archivoBD) :: IO (Either IOException String)
  case e of
    Left _ -> return [] -- archivo ausente o fallo de lectura
    Right contenido ->
      -- Cada línea es un 'show' de Prestamo; `read` lo invierte
      return $ map read (lines contenido)

-- Reintento con backoff simple
reintentar :: Int -> IO a -> IO (Either IOException a)
reintentar 0 accion = catch (accion >>= \x -> return (Right x))
                           (\(ex :: IOException) -> return (Left ex))
reintentar n accion = do
  r <- catch (accion >>= \x -> return (Right x))
             (\(ex :: IOException) -> return (Left ex))
  case r of
    Left _ -> do
      threadDelay 1000000  -- 1s
      reintentar (n - 1) accion
    Right v -> return (Right v)

-- ===============================
-- Lógica de negocio (funciones separadas)
-- ===============================

-- 1) Registrar préstamo (Check Out)
-- Evita duplicar préstamo activo del mismo ID.
registrarPrestamo :: String -> UTCTime -> [Prestamo] -> Either String [Prestamo]
registrarPrestamo idLibro ahora ps =
  case buscarPorIdActivo idLibro ps of
    Just _  -> Left "Ese libro ya está prestado (aún sin devolución registrada)."
    Nothing -> Right (Prestamo idLibro ahora Nothing : ps)

-- 2) Buscar por ID (sólo si sigue prestado)
buscarPorIdActivo :: String -> [Prestamo] -> Maybe Prestamo
buscarPorIdActivo idLibro ps =
  find (\p -> bookId p == idLibro && isNothing (devolucion p)) ps

-- 3) Calcular duración del préstamo
-- Si ya fue devuelto, usa prestamo..devolucion; si no, usa prestamo..ahora.
duracionPrestamo :: Prestamo -> UTCTime -> NominalDiffTime
duracionPrestamo p ahora =
  case devolucion p of
    Just tDev -> diffUTCTime tDev (prestamo p)
    Nothing   -> diffUTCTime ahora (prestamo p)

-- 4) Listar libros actualmente prestados (cargar desde archivo y filtrar activos)
listarPrestadosDesdeArchivo :: IO [Prestamo]
listarPrestadosDesdeArchivo = do
  ps <- cargarBiblioteca
  return (filter (isNothing . devolucion) ps)

-- 5) Registrar devolución (Check In). Devuelve lista actualizada o error.
registrarDevolucion :: String -> UTCTime -> [Prestamo] -> Either String [Prestamo]
registrarDevolucion idLibro ahora ps =
  case buscarPorIdActivo idLibro ps of
    Nothing -> Left "No existe un préstamo activo con ese ID."
    Just _  -> Right (map cerrar ps)
  where
    cerrar p | bookId p == idLibro && isNothing (devolucion p) = p { devolucion = Just ahora }
             | otherwise                                       = p

-- ===============================
-- Utilidades de presentación
-- ===============================

mostrarPrestamoLinea :: Prestamo -> String
mostrarPrestamoLinea p =
  "ID: " ++ bookId p
  ++ " | Prestamo: " ++ show (prestamo p)
  ++ " | Devolucion: " ++ case devolucion p of
                            Nothing -> "en curso"
                            Just t  -> show t

-- Bonito para segundos → "Xh Ym Zs"
formatearSegundos :: NominalDiffTime -> String
formatearSegundos dt =
  let sTotal = floor dt :: Integer
      (h, r1) = sTotal `divMod` 3600
      (m, s)  = r1 `divMod` 60
  in show h ++ "h " ++ show m ++ "m " ++ show s ++ "s"

-- ===============================
-- Menú interactivo
-- ===============================

menu :: IO ()
menu = do
  putStrLn "=========================================="
  putStrLn "  SISTEMA DE PRESTAMOS DE BIBLIOTECA"
  putStrLn "=========================================="
  putStrLn "1. Registrar préstamo (Check Out)"
  putStrLn "2. Registrar devolución (Check In)"
  putStrLn "3. Buscar libro por ID (si sigue prestado)"
  putStrLn "4. Listar libros actualmente prestados (desde archivo)"
  putStrLn "5. Salir"
  putStr "Seleccione una opción: "
  hFlush stdout

-- Ciclo principal (mantiene la lista en memoria y guarda tras cambios)
loop :: [Prestamo] -> IO ()
loop ps = do
  menu
  opcion <- getLine
  case opcion of
    "1" -> do
      putStr "Ingrese el ID único del libro: "
      hFlush stdout
      idLibro <- getLine
      ahora <- getCurrentTime
      case registrarPrestamo idLibro ahora ps of
        Left msg -> putStrLn ("Error: " ++ msg) >> loop ps
        Right ps' -> do
          putStrLn $ "Préstamo registrado para ID " ++ idLibro ++ "."
          guardarBiblioteca ps'
          loop ps'

    "2" -> do
      putStr "Ingrese el ID del libro a devolver: "
      hFlush stdout
      idLibro <- getLine
      ahora <- getCurrentTime
      case registrarDevolucion idLibro ahora ps of
        Left msg -> putStrLn ("Error: " ++ msg) >> loop ps
        Right ps' -> do
          putStrLn $ "Devolución registrada para ID " ++ idLibro ++ "."
          guardarBiblioteca ps'
          loop ps'

    "3" -> do
      putStr "Ingrese el ID del libro a buscar: "
      hFlush stdout
      idLibro <- getLine
      case buscarPorIdActivo idLibro ps of
        Nothing -> putStrLn "No hay préstamo activo con ese ID."
        Just p  -> do
          ahora <- getCurrentTime
          let dur = duracionPrestamo p ahora
          putStrLn $ "Encontrado. " ++ mostrarPrestamoLinea p
          putStrLn $ "Duración del préstamo: " ++ formatearSegundos dur
      loop ps

    "4" -> do
      putStrLn "Cargando y mostrando préstamos actualmente activos desde archivo..."
      activos <- listarPrestadosDesdeArchivo
      if null activos
        then putStrLn "No hay libros actualmente prestados."
        else mapM_ (putStrLn . mostrarPrestamoLinea) activos
      -- Mantener ps (no se sobreescribe la memoria con archivo para evitar
      -- pisar cambios que aún no se han guardado desde este proceso).
      loop ps

    "5" -> putStrLn "¡Hasta luego!"
    _   -> putStrLn "Opción no válida." >> loop ps

-- ===============================
-- main
-- ===============================

main :: IO ()
main = do
  ps <- cargarBiblioteca
  putStrLn $ "Registros cargados: " ++ show (length ps)
  loop ps

