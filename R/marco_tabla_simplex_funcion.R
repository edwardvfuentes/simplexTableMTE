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
texteitor <- function(lp, wb, iteracion, vector_coef_obj = coef_obj_gen(lp)){

  reiteros <- (-5 - nrow(lp)) + ((5 + nrow(lp)) * iteracion)

  matriz_restr_start <- matrix(0, nrow = nrow(lp), ncol = ncol(lp))
  for(j in 1:ncol(lp)){
    for(i in 1:nrow(lp)){
      matriz_restr_start[i,j] <- lpSolveAPI::get.mat(lp, i = i, j = j)
    }
  }

  #Definir matriz de lados derechos
  matriz_rhs <- get.rhs(lp)

  #Definir los coeficientes cB
  vectorino <- diag(nrow(lp))
  index_cb <- numeric(nrow(lp))

  for(j in 1:nrow(vectorino)){
    for(i in 1:ncol(matriz_restr_start)){
      if(sum(matrix(vectorino[,j]) == matriz_restr_start[,i]) == nrow(matriz_restr_start))
        index_cb[j] <- (1:ncol(matriz_restr_start))[i]
    }
  }

  matriz_cb <- matrix(numeric(nrow(lp)))

  for(i in 1:nrow(matriz_cb)){
    matriz_cb[i,] <- get.mat(lp, 0, index_cb[i])
    }

  # #Definir el vector de coeficientes de la matriz objetivo
  # coef_objetivo <- matrix(numeric(ncol(lp)), nrow = 1)
  # for(i in 1:ncol(lp)){
  #   coef_objetivo[i] <- lpSolveAPI::get.mat(lp, i = 0, j = i)
  # }

  #Definir el vector de las M grandes. Tenemos que identificar las artificiales
  #diciendole a R cuales de los coeficientes, que sean mayores que 0, son
  #mayores que el mayor de los coeficientes de la funcion objetivo original.

  #Mal mal. matriz_cb en este entorno siempre va a contener los Cienes de la funcion objetivo. Hay que leer la columna cB en la iteracion de ahora



  #¿Siguen habiendo artificiales dentro de la base?


  cual_mejor <- which(matriz_cb > max(vector_coef_obj[-index_cb]) & matriz_cb > 0)

  matriz_M <- matrix(apply(matriz_restr_start[cual_mejor,], 2, sum), nrow = 1)



  #Las variables xj y z_obj
  x_text <- rep("x", ncol(lp))
  x_text <- paste0(x_text, 1:ncol(lp))
  x_text <- c(x_text, "z_obj")
  x_text <- t(matrix(x_text))

  #cB y xB
  cBxB <- c("cB", rep("", ncol(lp)),"xB")
  cBxB <- t(matrix(cBxB))

  #La big M
  M <- "M"

  #El indicador zj-cj y los coeficientes de la base
  zjx <- matrix(c("zj-cj", "", x_text[as.numeric(index_cb)]))

  #Escribir los marcos textuales de las tablas
  XLConnect::writeWorksheet(wb, data = x_text, sheet = "Sheet1", startRow = 1 + reiteros, startCol = 3, header = FALSE)
  XLConnect::writeWorksheet(wb, data = cBxB, sheet = "Sheet1", startRow = 4 + reiteros, startCol = 2, header = FALSE)
  XLConnect::writeWorksheet(wb, data = M, sheet = "Sheet1", startRow = 2 + reiteros, startCol = 1, header = FALSE)
  XLConnect::writeWorksheet(wb, data = zjx, sheet = "Sheet1", startRow = 3 + reiteros, startCol = 1, header = FALSE)

  #Definir las coordenadas de columnas para la región rhs y la objetivo
  coord_rhs_obj <- paste0("Sheet1!", LETTERS[2 + ncol(lp) + 1])

  #Crear las regiones para cada tabla
  regiones_origen <- c(2, 3, 2, 3, 5, 5, 5) - (5 + nrow(lp)) + ((5 + nrow(lp)) * iteracion)
  regiones_origen_letra <- c("Sheet1!C","Sheet1!C", coord_rhs_obj, coord_rhs_obj, "Sheet1!C", coord_rhs_obj, "Sheet1!B")
  regiones_raw <- c("M_grande","costes_redux", "M_objetivo", "objetivo", "restricciones", "rhs", "base_coefs")
  listado_tablas <- list(matriz_M, -vector_coef_obj, 0,0, matriz_restr_start, matriz_rhs, matriz_cb)


  for(j in 1:length(regiones_raw)){
    XLConnect::createName(wb, paste0(regiones_raw[j], iteracion), paste0(regiones_origen_letra[j], regiones_origen[j]))
  }


  for(j in 1:length(regiones_raw)){
    XLConnect::writeNamedRegion(wb, name = paste0(regiones_raw[j], iteracion), data = listado_tablas[j], header = FALSE)
  }
  regiones_origen <- regiones_origen + (5 + nrow(lp))

  if(iteracion >= 2){
    matriz_cb <- XLConnect::readNamedRegion(wb, name = paste0("base_coefs", iteracion),header = FALSE)

    cual_mejor <- which(matriz_cb > max(vector_coef_obj[-index_cb]) & matriz_cb > 0)

  }

  return(index_cb)

}
