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
pivot_calc <- function(mat_restr, mat_restr_celdas, cost_reducidos){

  if(sum(sign(cost_reducidos) == 1) <= 0){
    warning("No existen positivos entre los costes reducidos. Se termina el algoritmo (Optimo finito alcanzado)")
    return(NULL)

  } else {
    col_pivote <- which(cost_reducidos == max(cost_reducidos))
  }

  if(sum(sign(mat_restr[,col_pivote]) > 0) <= 0){
    warning("No existen positivos en la variable. Se termina el algoritmo (No hay optimo finito)")
    return(NULL)

  }

  positivos_pivote <- mat_restr[sign(mat_restr[,col_pivote]) == 1,col_pivote]
  row_pivote <- which(mat_restr[,col_pivote] == min(positivos_pivote))

  #Ahora deberemos elaborar las fórmulas para la tabla de Excel
  var_pivote <- mat_restr_celdas[row_pivote, col_pivote]
  var_pivote <- stringr::str_split(var_pivote, "", n = 2, simplify = TRUE)
  var_pivote <- paste0("$",var_pivote[1],"$",var_pivote[2])

  return(list(Fila = row_pivote, Columna = col_pivote, Coordenada = var_pivote))
}

