#' Title Calculo del pivote de una iteracion de Simplex
#'
#' @param mat_restr Matriz numerica de restricciones del problema Simplex a resolver
#' @param mat_restr_celdas Matriz de coordenadas de la matriz de restricciones del archivo .xlsx
#' @param cost_reducidos Vector de los costes reducidos generados en una iteracion de Simplex durante el criterio de entrada
#'
#' @return Una lista con tres elementos: Fila, Columna y Coordenada
#' @export
#'
#' @examples
#' print("Introduciremos ejemplos posteriormente, disculpa las molestias.")
pivot_calc <- function(mat_restr, mat_restr_celdas, cost_reducidos, mat_M, mat_rhs, cual_M){

  #Criterio de entrada(seleccion de columna para el pivote)

  if(length(cual_M) > 0){
    cols <- 1:length(mat_M)
    exclusion <- -tail(cols, length(cual_M))
    if(sum(mat_M[exclusion] > 0.000001) == 0){
      message("No existen positivos entre las M grandes. Se termina el algoritmo (No hay solucion factible)")
      return(NULL)

    } else {
      col_pivote <- which(mat_M == max(mat_M[exclusion]))
    }

  }else{

    if(sum(sign(cost_reducidos) == 1) <= 0){
      message("No existen positivos entre los costes reducidos. Se termina el algoritmo (Optimo finito alcanzado)")
      return(NULL)

    } else {
      col_pivote <- which(cost_reducidos == max(cost_reducidos))
    }


    #Criterio de salida
    if(sum(sign(mat_restr[,col_pivote]) > 0) <= 0){
      message("No existen positivos en la variable. Se termina el algoritmo (No hay optimo finito)")
      return(NULL)

    }
  }


  cociente_pivote <- mat_rhs/mat_restr[,col_pivote]
  positivos_pivote <- cociente_pivote[cociente_pivote >= 0]

  #Determinacion del row
  letras_min <- rownames(cociente_pivote)[which(cociente_pivote == min(cociente_pivote[cociente_pivote > 0]))]
  row_pivote <- which(rownames(cociente_pivote) == sort(letras_min)[1])



  #Ahora deberemos elaborar las fórmulas para la tabla de Excel
  var_pivote <- mat_restr_celdas[row_pivote, col_pivote]
  var_pivote <- stringr::str_split(var_pivote, "", n = 2, simplify = TRUE)
  var_pivote <- paste0("$",var_pivote[1],"$",var_pivote[2])

  return(list(Fila = row_pivote, Columna = col_pivote, Coordenada = var_pivote))

}

