#'Resolución de un problema simplex en formato tabla .xlsx
#'
#'Genera cada una de las tablas hasta la solución óptima de un problema Simplex
#'y lo exporta en un archifo de formato .xlsx
#'
#'@param lp Objeto tipo \code{lpExtPtr} proveniente del paquete
#'  \code{lpSolveAPI} y que debe estar resuelto con la funcion solve antes de
#'  ser introducido
#'@param wb Donde se coloca un objeto Workbook de XLConnect
#'
#'@return Rellena un archivo .xlsx, previamente creado con loadWorkbook, con la
#'  resolucion de un problema de Simplex.
#'@import XLConnect
#'@import stringr
#'@export
#'
#'@details Esta función requiere de dos objetos: Uno lpExtPtr que esté resuelto
#'  con la funcion \link[lpSolveAPI]{solve}, y otro de tipo workbook, ambos
#'  provenientes de los paquetes lpSolveAPI y XLConnect
#'
#'  Nótese que el problema de programación lineal introducido debe estar
#'  necesariamente en forma estándar, porque de momento no contiene un argumento
#'  que permita elegir la base inicial con la que comenzar la tabla de Simplex.
#'  En el apartado ejemplos se resuelve un problema sencillo en forma estándar
#'  con variables de holgura y soluciones enteras.
#'
#'  Finalmente, el .xlsx devuelto contendrá una fila de Ms para el supuesto de
#'  que se utilice el método de la M grande. La función detecta de forma
#'  automática cuales de las variables son artificiales, comparando los
#'  coeficientes de las variables de la base inicial que sean mayores que cero,
#'  con aquellos coeficientes de las variables iniciales mayores que cero
#'  (suponiendo que las variables artificiales tengan un coeficiente
#'  significativamente grande).
#'
#' @examples
#'#Cargamos un problema de programación lineal y lo resolvemos
#'library(lpSolveAPI)
#'library(XLConnect)
#'
#'problem_79_es <- make.lp(3,5)
#'set.objfn(problem_79_es, c(-2,-5, 0, 0, 0))
#'set.rhs(problem_79_es, c(7,3,5))
#'set.constr.type(problem_79_es, c(rep("=", 3)))
#'set.row(problem_79_es,1, c(-6, -2, 1, 0, 0))
#'set.row(problem_79_es,2, c(-8, -4, 0, 1, 0))
#'set.row(problem_79_es,3, c(1, 4, 0, 0, 1))
#'
#'solve(problem_79_es)
#'
#'#Procedemos a generar el objeto workbook y a utilizar la función. Como resultado,
#'#debería aparecer un objeto .xlsx en la carpeta del directorio de trabajo con el nombre
#'#indicado (en este caso, "Miproblema79.xlsx").
#'
#'problem79 <- loadWorkbook("Miproblema79.xlsx", create = TRUE)
#'tablaSimplex(problem_79_es, wb = problem79)
#'saveWorkbook(problem79)
tablaSimplex <- function(lp, wb, max_iteracion = 100){
  createSheet(wb, "Sheet1")

  iteracion <- 1

  #Generación de varios rangos de letras para las columnas de Excel (no llega a más de 700 variables)
  LETRAS <- LETTERS

  for(i in seq(LETTERS)){
    LETRAS <- c(LETRAS, paste0(LETTERS[i], LETTERS))
  }

  #Elaboración del marco textual
  texteitor(lp, wb, iteracion)

  #Cálculo de los coeficientes cB y extracción de los coeficientes objetivos
  index_cb_inicial <- index_cb_calc(lp, matriz_restr_start = matriz_restr_calc(lp))
  coef_vector_obj <- coef_obj_gen(lp)

  nombres_regiones <- getDefinedNames(wb)[!str_detect(getDefinedNames(wb), "base")]

  indices <- 1:6
  inicio <- 4

  #Espacio para determinar el color de la celda del pivote
  estilocelda <- createCellStyle(wb, "estilocelda")
  setFillBackgroundColor(estilocelda, color = XLC$"COLOR.LIGHT_GREEN")
  setFillForegroundColor(estilocelda, color = XLC$"COLOR.LIGHT_GREEN")
  setFillPattern(estilocelda, fill = XLC$"FILL.SOLID_FOREGROUND")

  while(iteracion <= max_iteracion){
    costes <- readNamedRegion(wb, str_c("costes_redux", iteracion), header = FALSE)
    m_restr <- readNamedRegion(wb, str_c("restricciones", iteracion), header = FALSE)
    celda_restr <- coord_celdas(wb, str_c("restricciones", iteracion))
    rhs_num <- readNamedRegion(wb, str_c("righthand", iteracion), header = FALSE)
    base_vars <- readWorksheet(wb, "Sheet1", startRow = inicio + 1, endRow = inicio + nrow(lp), startCol = 1, endCol = 1, header = FALSE)
    rownames(rhs_num) <- unlist(base_vars)
    m_cb <-readNamedRegion(wb, paste0("base_coefs", iteracion), header = FALSE)
    M_grand <- readNamedRegion(wb, str_c("M_grande", iteracion), header = FALSE)
    M_cual <- which(m_cb > max(coef_vector_obj[-index_cb_inicial]) & m_cb > 0)

    pivote <- pivot_calc(m_restr, celda_restr, costes, M_grand, rhs_num, cual_M = M_cual, wb, lp)
     if(!is.null(pivote)){
       message(paste("Procesando iteracion", iteracion, "..."))
      #Coloreamos la celda del pivote
      setCellStyle(wb, sheet = "Sheet1", row = inicio + pivote$Fila, col = 2 + pivote$Columna, cellstyle = estilocelda)
      iteracion <- iteracion + 1

      #Creamos nuevo marco y cálculo de cb
      texteitor(lp, wb, iteracion)
     # index_cb_inicial <- index_cb_calc(lp, matriz_restr_start = matriz_restr_calc(lp))

      for(x in indices){
        nombres_regiones <- getDefinedNames(wb)[!str_detect(getDefinedNames(wb), "base")]
        celda <- coord_celdas(wb, nombres_regiones[x])
        formula <- formula_celdas(celda, pivote)
        coord <- getReferenceCoordinatesForName(wb, nombres_regiones[x + 6])
        times <- nrow(as.matrix(formula))
        setCellFormula(wb, sheet = "Sheet1", row = coord[1,1]:coord[2,1], col = sort(rep(coord[1,2]:coord[2,2], times)), formula = formula)
        }

      } else {
        return("Algoritmo finalizado.")
      }
    #¿Sección para actualizar la columna cB y las variables de la base?
    var_base <- readWorksheet(wb, "Sheet1", startRow = 1, endRow = 1, startCol = 3, endCol = 3 + ncol(lp), simplify = T, header = FALSE)

    #Dibujamos la columna de las variables de la base de la tabla anterior
    writeWorksheet(wb, base_vars, "Sheet1", startRow = inicio + nrow(lp) + 6, startCol = 1, header = FALSE)

    #Metemos la var de entrada en la columna izquierda
    writeWorksheet(wb, var_base[pivote$Columna], "Sheet1", startRow =  inicio + nrow(lp) + 5 + pivote$Fila, startCol = 1, header = FALSE)

    #Actualizamos la columna cB con los coeficientes de la base de la iteracion anterior
    cb_prev <- readNamedRegion(wb, paste0("base_coefs", iteracion - 1), header = FALSE)
    writeNamedRegion(wb, data = cb_prev, name = paste0("base_coefs", iteracion), header = FALSE)

    #Actualizamos la columna cB con el coeficiente de la variable que ha entrado
    writeWorksheet(wb, get.mat(lp, 0, pivote$Columna), "Sheet1", startRow = inicio + nrow(lp) + 5 + pivote$Fila, startCol = 2, header = FALSE)

    indices <- indices  + 6
    inicio <- inicio + nrow(lp) + 5
  }

}

