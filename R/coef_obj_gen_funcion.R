#' Title Obtencion del vector de coeficientes del problema.
#'
#' @param lp Un objeto de programacion lineal generado conel paquete lpSolveAPI
#'
#' @return Devuelve un vector de tantos elementos como columnas tenga el problema
#' @export
#'
#' @examples
#' print("Ya meteremos algo!")
coef_obj_gen <- function(lp){
  coef_objetivo <- matrix(numeric(ncol(lp)), nrow = 1)
  for(i in 1:ncol(lp)){
    coef_objetivo[i] <- lpSolveAPI::get.mat(lp, i = 0, j = i)
  }
  return(coef_objetivo)

}
