#' Generacion de marcos con texto y regiones para el problema lineal planteado
#'
#' @param lp Objeto tipo lpExtPtr proveniente del paquete lpSolveAPI y que debe estar resuelto con la funcion solve antes de ser introducido
#' @param wb Donde se coloca un objeto Workbook de XLConnect
#' @param iteracion Numero de la tabla que se va a generar dentro del archivo .xlsx
#'
#' @import lpSolveAPI
#' @import XLConnect
#' @import purrr
#' @import dplyr
#'
#' @return Establece las regiones de cada una de las partes del problema de programacion lineal y los marcos indicativos textuales.
#'
#' @examples
#' print("Introduciremos ejemplos posteriormente, disculpa las molestias.")
texteitor <- function(lp, wb, iteracion, vector_coef_obj = coef_obj_gen(lp)){
  LETRAS <- LETTERS
  for(i in seq(length(LETTERS))){
    LETRAS <- c(LETRAS, paste0(LETTERS[i], LETTERS))
  }

  reiteros <- (-5 - nrow(lp)) + ((5 + nrow(lp)) * iteracion)
  modo <- lp.control(lp)$sense

  #Calculamos la matriz de restricciones iniciales y los coeficientes cB
  matriz_restr_start <- matriz_restr_calc(lp)
  index_cb <- index_cb_calc(lp, matriz_restr_start)

  #Definir matriz de lados derechos
  matriz_rhs <- get.rhs(lp)

  #Crear una matriz cb para uso de la M grande
  matriz_cb <- matrix(numeric(nrow(lp)))

  for(i in 1:nrow(matriz_cb)){
    matriz_cb[i,] <- get.mat(lp, 0, index_cb[i])
  }

  #Determining which coefficients are better suited for the task
  if (modo == "minimize") {
    cual_mejor <-
      which(matriz_cb > max(vector_coef_obj[-index_cb]) & matriz_cb > 0)
    matriz_M <-
      matrix(apply(matriz_restr_start[cual_mejor, ], 2, sum), nrow = 1)

  } else {
    cual_mejor <-
      which(matriz_cb < min(vector_coef_obj[-index_cb]) & matriz_cb < 0)
    matriz_M <-
      matrix(apply(matriz_restr_start[cual_mejor, ], 2, sum), nrow = 1)

  }


  #Las variables xj y z_obj
  x_text <- rep("x", ncol(lp)) %>%
    paste0(1:ncol(lp)) %>%
    c("z_obj") %>%
    matrix() %>%
    t()

  #cB y xB
  cBxB <- c("cB", rep("", ncol(lp)),"xB") %>%
    matrix() %>%
    t()

  #The big M
  M <- "M"

  #El indicador zj-cj y los coeficientes de la base
  zjx <- matrix(c("zj-cj", "", x_text[as.numeric(index_cb)]))

  #Reunimos textos y parámetros en una lista
  frame_text_list <- list(
    list(data = x_text, startRow = 1, startCol = 3),
    list(data = cBxB, startRow = 4, startCol = 2),
    list(data = M, startRow = 2, startCol = 1),
    list(data = zjx, startRow = 3, startCol = 1)
  )

  #Escribir los marcos textuales de las tablas dentro del workbook
  for(i in frame_text_list){
    writeWorksheet(wb,
                   data = i$data, sheet = "Sheet1",
                   startRow = i$startRow + reiteros,
                   startCol = i$startCol,
                   header = FALSE)
  }

  #Definir las coordenadas de columnas para la región rhs y la objetivo
  coord_rhs_obj <- paste0("Sheet1!", LETRAS[2 + ncol(lp) + 1])

  #Crear las regiones para cada tabla
  regiones_origen <- c(2, 3, 2, 3, 5, 5, 5) - (5 + nrow(lp)) + ((5 + nrow(lp)) * iteracion)
  regiones_origen_letra <- c("Sheet1!C","Sheet1!C", coord_rhs_obj, coord_rhs_obj, "Sheet1!C", coord_rhs_obj, "Sheet1!B")
  regiones_raw <- c("M_grande", "costes_redux", "M_objetivo", "objetivo", "restricciones", "righthand", "base_coefs")
  listado_tablas <- list(matriz_M, -vector_coef_obj, 0, 0, matriz_restr_start, matriz_rhs, matriz_cb)

  for(j in 1:length(regiones_raw)){
    createName(wb, paste0(regiones_raw[j], iteracion), paste0(regiones_origen_letra[j], regiones_origen[j]))
    writeNamedRegion(wb, name = paste0(regiones_raw[j], iteracion), data = listado_tablas[j], header = FALSE)
  }

  #regiones_origen <- regiones_origen + (5 + nrow(lp))

  if(iteracion >= 2){
    matriz_cb <- readNamedRegion(wb, name = paste0("base_coefs", iteracion), header = FALSE)
   # cual_mejor <- which(matriz_cb > max(vector_coef_obj[-index_cb]) & matriz_cb > 0)

  }


}
