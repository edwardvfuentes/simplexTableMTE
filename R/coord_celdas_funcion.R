#' Obtencion de coordenadas de celdas en una hoja de cálculo
#'
#' Utiliza las regiones creadas dentro del workbook con las funciones writeNamedRegion del paquete XLConnect
#'
#' @param wb Donde se coloca un objeto Workbook de XLConnect
#' @param regionName Nombre de la region del archivo .xlsx de la que se desea extraer las coordenadas
#'
#' @return Devuelve las coordenadas de todas las celdas que componen la region solicitada en el archivo .xlsx.
#' @import XLConnect
#'
#' @examples
#' print("Introduciremos ejemplos posteriormente, disculpa las molestias.")
coord_celdas <- function(wb, regionName){

  LETRAS <- LETTERS
  for(i in seq(length(LETTERS))){
    LETRAS <- c(LETRAS, paste0(LETTERS[i], LETTERS))
  }

  #Elaboración de la matriz de celdas para las restricciones (con coordenadas de Excel)
  restr_coord <- getReferenceCoordinatesForName(wb, regionName)
  restr_rows <- restr_coord[1,1]:restr_coord[2,1]
  restr_cols <- LETRAS[restr_coord[1,2]:restr_coord[2,2]]

  matriz_restr_celdas <- matrix(character(length(restr_rows) * length(restr_cols)),
                                  ncol = length(restr_cols))

  for(i in 1:length(restr_cols)){
    matriz_restr_celdas[,i] <- paste0(restr_cols[i], restr_rows)
  }
  return(matriz_restr_celdas)
}

