#' Extract the initial restriction matrix from a lpExtPtr object
#'
#' Takes a simplex problem defined by lpSolveAPI and then computes the initial
#' restriction matrix
#'
#' @param lp An object of class \code{lpExtPtr} from the lpSolveAPI package
#'
#' @return
#' @import dplyr
#'
#' @examples
matriz_restr_calc <- function(lp){

  matriz_restr_start <- expand.grid(i = 1:nrow(lp), j = 1:ncol(lp)) %>%
    transpose() %>%
    map_dbl(function(x) get.mat(lp, i = x$i, j = x$j)) %>%
    matrix(nrow(lp), ncol(lp))

  return(matriz_restr_start)
}
