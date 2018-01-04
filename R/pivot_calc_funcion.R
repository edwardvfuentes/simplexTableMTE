#' Calculo del pivote de una iteracion de Simplex
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
pivot_calc <- function(mat_restr, mat_restr_celdas, cost_reducidos, mat_M, mat_rhs, cual_M, workbook, lp){


  #Criterio de entrada (seleccion de columna para el pivote)
    prim_base <- XLConnect::readNamedRegion(workbook, "base_coefs1", header = FALSE)
    prim_base <- unlist(prim_base)
    fobjetivo <- coef_obj_gen(lp)
    modo <- lpSolveAPI::lp.control(lp)$sense
    if(modo == "minimize"){
      #¿Cuales son los M grandes, si existen?
      coincid <- match(fobjetivo, prim_base[prim_base > 0])

      #¿Hay M grandes en la fila M?
      if (sum(is.na(coincid)) != length(coincid)) {

      #Identifica aquellos coeficientes de la función objetivo que no son M grandes
        excluidos <- which(!is.na(coincid))
        #Si no hay elementos positivos en la fila M, juzgar segun zj cj
        if (sum(mat_M[-excluidos] > 0.000001) == 0) {
          #sum(sign(cost_reducidos) == 1) <= 0
          if (all(cost_reducidos <= 10e-14)) {
            message(
              "No existen positivos entre los costes reducidos. Se termina el algoritmo (Optimo finito alcanzado)"
            )
            return(NULL)

          } else {
            col_pivote <- which(cost_reducidos > max(cost_reducidos) - 0.00001)[1]
          }

        } else {
          col_pivote <- which(mat_M > max(mat_M[-excluidos]) - 0.000001)[1]
        }

      } else {
        if (all(cost_reducidos <= 10e-14)) {
          message(
            "No existen positivos entre los costes reducidos. Se termina el algoritmo (Optimo finito alcanzado)"
          )
          return(NULL)

        } else {
          col_pivote <- which(cost_reducidos > max(cost_reducidos) - 0.000001)[1]

        }

      }

#Para la maximizacion
    } else {
      coincid <- match(fobjetivo, prim_base[prim_base < 0])
      if (sum(is.na(coincid)) != length(coincid)) {
        excluidos <- which(!is.na(coincid))
        if (sum(mat_M[-excluidos] > 0.000001) == 0) {
          if (sum(sign(cost_reducidos) == -1) <= 0) {
            message(
              "No existen negativos entre los costes reducidos. Se termina el algoritmo (Optimo finito alcanzado)"
            )
            return(NULL)

          } else {
            col_pivote <- which(cost_reducidos == min(cost_reducidos))[1]
          }

        } else {
          col_pivote <- which(mat_M == max(mat_M[-excluidos]))[1]
        }

      } else {
        if (sum(sign(cost_reducidos) == -1) <= 0) {
          message(
            "No existen negativos entre los costes reducidos. Se termina el algoritmo (Optimo finito alcanzado)"
          )
          return(NULL)

        } else {
          col_pivote <- which(cost_reducidos == min(cost_reducidos))[1]

        }

      }

    }



    #Criterio de salida
    if (sum(sign(mat_restr[, col_pivote]) > 0) <= 0) {
      message("No existen positivos en la variable. Se termina el algoritmo (No hay optimo finito)")
      return(NULL)
    }




  denominador <- mat_restr[,col_pivote]
  names(denominador) <- rownames(mat_rhs)
  denominador <- as.data.frame(denominador)



  cociente_pivote <- mat_rhs/denominador
  cociente_pivote_filt <- subset(cociente_pivote, denominador > 0)




  #Determinacion de la fila


    letras_min <- rownames(cociente_pivote_filt)[cociente_pivote_filt == min(cociente_pivote_filt, na.rm = T)]
    row_pivote <- which(rownames(cociente_pivote) == stringr::str_sort(letras_min, numeric = T)[1])



  #Ahora deberemos elaborar las fórmulas para la tabla de Excel
  var_pivote <- mat_restr_celdas[row_pivote, col_pivote]
  var_pivote <- stringr::str_split(var_pivote, "", n = 2, simplify = TRUE)
  var_pivote <- paste0("$",var_pivote[1],"$",var_pivote[2])

  return(list(Fila = row_pivote, Columna = col_pivote, Coordenada = var_pivote))

}

