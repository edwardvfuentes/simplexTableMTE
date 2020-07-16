#' Obtencion del vector de coeficientes del problema.
#'
#' Extrae en un vector numérico los coeficientes de un problema definido como objeto \code{lpExtrPtr}
#'
#' @param lp Un objeto de programacion lineal generado conel paquete lpSolveAPI
#'
#' @return Devuelve un vector de tantos elementos como columnas tenga el problema
#' @import lpSolveAPI
#'
#' @examples
#' print("Ya meteremos algo!")
coef_obj_gen <- function(lp){
  coef_objetivo <- matrix(numeric(ncol(lp)), nrow = 1)
  for(i in 1:ncol(lp)){
    coef_objetivo[i] <- get.mat(lp, i = 0, j = i)
  }
  return(coef_objetivo)

}
