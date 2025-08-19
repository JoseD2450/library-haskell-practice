📚 ST0244 – Practice I: Library Lending Manager (Haskell)
👥 Integrantes

Jose David Acevedo

💻 Plataforma(s) utilizadas

GHC 9.x (Glasgow Haskell Compiler)

Sistema operativo: Windows 10 / 11

Editor de código: Visual Studio Code con extensión de Haskell

📂 Archivos en este repositorio

Library.hs → Código fuente en Haskell del sistema de préstamos de libros.

Library.txt → Archivo de persistencia de datos con registros de ejemplo.

README.md → Este archivo con documentación del proyecto.

⚙️ Cómo ejecutar el proyecto

Compilar el programa con:

ghc -O2 Library.hs -o library


Ejecutar:

library.exe   # en Windows

📖 Funcionalidades implementadas

Registrar préstamo (Check Out): Pide ID del libro y guarda hora actual.

Registrar devolución (Check In): Marca la devolución con hora actual.

Buscar libro por ID: Si está prestado, muestra info y duración del préstamo.

Calcular duración: Usa diffUTCTime para saber cuánto tiempo lleva el libro prestado.

Listar préstamos activos: Lee Library.txt y muestra los libros actualmente en préstamo.

Persistencia: Los datos se guardan y cargan desde Library.txt en la raíz del proyecto.

📄 Ejemplo de Library.txt
Prestamo {bookId = "BK-001", prestamo = 2025-08-10 15:00:00 UTC, devolucion = Nothing}
Prestamo {bookId = "BK-002", prestamo = 2025-08-11 18:30:20 UTC, devolucion = Just 2025-08-12 10:05:00 UTC}

🎥 Video de demostración
