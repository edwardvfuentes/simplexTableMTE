#' Calculate index of cb coefficients
#'
#' Returns the index for the basis coefficients inside a Simplex problem
#'
#' This function identifies all those coefficients which are a part of the
#' initial basis from the specified linear problem
#'
#' @param lp An object of class \code{lpExtPtr} from the lpSolveAPI package
#' @param matriz_restr_start The initial restriction matriz from the Simplex problem
#'
#' @import dplyr
#' @return A numeric vector for indexing purposes
#'
#'
#' @examples
index_cb_calc <- function(lp, matriz_restr_start){
  #Definir los coeficientes cB
  ##Indices de los cB
  vectorino <- diag(nrow(lp))

  index_cb <- expand.grid(j = 1:nrow(vectorino),
                          i = 1:ncol(matriz_restr_start)) %>%
    transpose() %>%
    map_dbl(function(x) {
      ifelse(sum(matrix(vectorino[, x$j]) == matriz_restr_start[, x$i]) == nrow(matriz_restr_start),
             x$i,
             0)
    }) %>%
    subset(. > 0)

  return(index_cb)
}
