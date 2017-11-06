#' Title Generacion de formulas de Simplex dentro del archivo .xlsx
#'
#' @param mat_celdas Matriz con las coordenadas de la region donde se desean calcular las formulas.
#' @param pivot_list Objeto que contiene las coordenadas del pivote, tanto numericas como textuales.
#'
#' @return Devuelve la formulas pertinentes para la region, siguiendo la regla del cuadrado y teniendo en cuenta aquella celda donde se encuentre el pivote.
#' @export
#'
#' @examples
#' print("Introduciremos ejemplos posteriormente, disculpa las molestias.")
formula_celdas <- function(mat_celdas, pivot_list){

  pivot_row <- pivot_list$Fila
  pivot_col <- pivot_list$Columna
  pivot_var <- pivot_list$Coordenada

  #PLANTILLA para celdas que estén en la misma fila
  cel_same <- paste0("", "/", pivot_var)

  mat_formulas <- matrix(character(max(dim(mat_celdas))), nrow = nrow(mat_celdas), ncol = ncol(mat_celdas))

  if(nrow(mat_formulas) > 1 & ncol(mat_formulas) > 1){
    #Generación matrices vacías

    #Aqui se rellenan las filas pertenecientes al pivote
    mat_formulas[pivot_row,] <- paste0(mat_celdas[pivot_row, ], rep(cel_same, ncol(mat_formulas)))

    rows_change <- 1:nrow(mat_celdas)
    rows_change <- rows_change[-pivot_row]

    #Aquí se rellenarán las otras filas que no sean las del pivote
    #Generación de las fórmulas de la matriz de restricciones
    for(i in rows_change){
      mat_formulas[i,] <- paste0(mat_celdas[i,], "-(", mat_celdas[i,pivot_col], "*",mat_celdas[pivot_row,],")/", pivot_var )
    }
  }

  #Para lados derechos
  if(nrow(mat_formulas) > 1 & ncol(mat_formulas) == 1){
    rows_change <- 1:nrow(mat_celdas)
    rows_change <- rows_change[-pivot_row]
    split_mat <- stringr::str_split(mat_celdas, "", n = 2, simplify = T)
    split_piv <- stringr::str_split(stringr::str_replace_all(pivot_var, "\\$", ""), "", n = 2,simplify = T)
    mat_formulas[pivot_row,] <- paste0(mat_celdas[pivot_row,], cel_same)
    for(i in rows_change){
      mat_formulas[i,] <- paste0(mat_celdas[i,],
                                 "-(", paste0(split_mat[i, 1], split_piv[2]),
                                 "*", paste0(split_piv[1], split_mat[i, 2]),
                                 ")/", pivot_var)
    }
  }

  #Para costes reducidos
  if(ncol(mat_formulas) > 1 & nrow(mat_formulas) == 1){
    split_mat <- stringr::str_split(mat_celdas, "", n = 2, simplify = T)
    split_piv <- stringr::str_split(stringr::str_replace_all(pivot_var, "\\$", ""), "", n = 2, simplify = T)
    for(i in 1:length(mat_formulas)){
      mat_formulas[,i] <- paste0(mat_celdas[,i], "-(", paste0(split_mat[i, 1], split_piv[2]), "*",paste0(split_piv[1], split_mat[i, 2]),")/", pivot_var )
    }
  }

  #Para el valor de la funcion objetivo
  if(ncol(mat_formulas) == nrow(mat_formulas)){
    split_mat <- stringr::str_split(mat_celdas, "", n = 2, simplify = T)
    split_piv <- stringr::str_split(stringr::str_replace_all(pivot_var, "\\$", ""), "", n = 2, simplify = T)
    mat_formulas <- paste0(mat_celdas, "-(", paste0(split_mat[1], split_piv[2]), "*",paste0(split_piv[1], split_mat[2]),")/", pivot_var )
  }


  return(mat_formulas)
}



