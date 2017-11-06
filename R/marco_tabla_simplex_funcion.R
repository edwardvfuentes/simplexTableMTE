#' Title Generacion de marcos con texto y regiones para el problema lineal planteado
#'
#' @param lp Objeto tipo lpExtPtr proveniente del paquete lpSolveAPI y que debe estar resuelto con la funcion solve antes de ser introducido
#' @param wb Donde se coloca un objeto Workbook de XLConnect
#' @param iteracion Numero de la tabla que se va a generar dentro del archivo .xlsx
#'
#' @return Establece las regiones de cada una de las partes del problema de programacion lineal y los marcos indicativos textuales.
#' @export
#'
#' @examples
#' print("Introduciremos ejemplos posteriormente, disculpa las molestias.")
texteitor <- function(lp, wb, iteracion){

  reiteros <- (-4 - nrow(lp)) + ((4 + nrow(lp)) * iteracion)

  #La primera matriz
  matriz_restr_start <- matrix(0, nrow = nrow(lp), ncol = ncol(lp))
  for(j in 1:ncol(lp)){
    for(i in 1:nrow(lp)){
      matriz_restr_start[i,j] <- lpSolveAPI::get.mat(lp, i = i, j = j)
    }
  }

  #Definir matriz de lados derechos
  matriz_rhs <- get.rhs(lp)

  #Definir el vector de coeficientes de la matriz objetivo
  coef_objetivo <- matrix(numeric(ncol(lp)), nrow = 1)
  for(i in 1:ncol(lp)){
    coef_objetivo[i] <- lpSolveAPI::get.mat(lp, i = 0, j = i)
  }

  #Las variables xj y z_obj
  x_text <- rep("x", ncol(lp))
  x_text <- paste0(x_text, 1:ncol(lp))
  x_text <- c(x_text, "z_obj")
  x_text <- t(matrix(x_text))

  #cB y xB
  cBxB <- c("cB", rep("", ncol(lp)),"xB")
  cBxB <- t(matrix(cBxB))

  #El indicador zj-cj y los coeficientes de la base
  zjx <- matrix(c("zj-cj", "",x_text[c(-1:-diff(dim(lp)),-length(x_text))]))

  #Escribir los marcos textuales de las tablas
  XLConnect::writeWorksheet(wb, data = x_text, sheet = "Sheet1", startRow = 1 + reiteros, startCol = 3, header = FALSE)
  XLConnect::writeWorksheet(wb, data = cBxB, sheet = "Sheet1", startRow = 3 + reiteros, startCol = 2, header = FALSE)
  XLConnect::writeWorksheet(wb, data = zjx, sheet = "Sheet1", startRow = 2 + reiteros, startCol = 1, header = FALSE)

  #Definir las coordenadas de columnas para la región rhs y la objetivo
  coord_rhs_obj <- paste0("Sheet1!", LETTERS[2 + ncol(lp) + 1])

  #Crear las regiones para cada tabla
  regiones_origen <- c(2, 2, 4, 4, 4) - (4 + nrow(lp)) + ((4 + nrow(lp)) * iteracion)
  regiones_origen_letra <- c("Sheet1!C", coord_rhs_obj, "Sheet1!C", coord_rhs_obj, "Sheet1!B")
  regiones_raw <- c("costes_redux", "objetivo", "restricciones", "rhs", "base_coefs")
  listado_tablas <- list(-coef_objetivo, 0, matriz_restr_start, matriz_rhs, matrix(numeric(3)))


  for(j in 1:length(regiones_raw)){
    XLConnect::createName(wb, paste0(regiones_raw[j], iteracion), paste0(regiones_origen_letra[j], regiones_origen[j]))
  }


  for(j in 1:length(regiones_raw)){
    XLConnect::writeNamedRegion(wb, name = paste0(regiones_raw[j], iteracion), data = listado_tablas[j], header = FALSE)
  }
  regiones_origen <- regiones_origen + (4 + nrow(lp))

}
