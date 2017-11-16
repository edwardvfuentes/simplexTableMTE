#' Transformador de problemas primales a duales
#'
#' @param lp Un objeto lpPtrExtr completo, resuelto o no.
#'
#' @return La version dual del objeto introducido.
#' @export
#'
#' @examples
#'library(lpSolveAPI)
#'problem_80 <- make.lp(3,4)
#'
#'set.objfn(problem_80, c(-8, -1, 0, 7))
#'set.rhs(problem_80, c(2, 1, 8))
#'set.constr.type(problem_80, c("=", ">=", "<="))
#'set.row(problem_80, 1, c(2, 5, -4, 1))
#'set.row(problem_80, 2, c(7, 0, 6, -7))
#'set.row(problem_80, 3, c(-7, -8, 3, -6))
#'
#'solve(problem_80)
#'problem_80
#'invert_prob(problem_80)
invert_prob <- function(lp){
  matriz <- matrix(numeric(prod(dim(lp))), nrow = nrow(lp), ncol = ncol(lp))
  rev_dim <- rev(dim(lp))
  new_lp <- make.lp(rev_dim[1], rev_dim[2])
  for(j in 1:ncol(lp)){
    for(i in 1:nrow(lp)){
      matriz[i,j] <- get.mat(lp, i = i, j = j)
    }
  }
  matriz <- t(matriz)

  #Definir nuevo modelo, restricciones y demases.
  set.rhs(new_lp, as.numeric(coef_obj_gen(lp)))
  set.objfn(new_lp, get.rhs(lp))
  for(i in 1:nrow(new_lp)){
    set.row(new_lp, i, matriz[i,])
  }


  if(lp.control(lp)$sense == "minimize"){
    mod <- lp.control(new_lp, sense = "maximize")
  }

  if(lp.control(lp)$sense == "maximize"){
    mod <- lp.control(new_lp, sense = "minimize")
  }

  for(i in 1:ncol(lp)){
    if(get.bounds(lp)$lower[1] == 0){
      set.constr.type(new_lp, rep("<=", nrow(new_lp)))
    }
  }

  for(i in 1:nrow(lp)){
    if(get.constr.type(lp)[i] == "=" ){
      set.bounds(new_lp, lower = Inf, upper = Inf, columns = i)
    }
    if(get.constr.type(lp)[i] == "<=" ){
      set.bounds(new_lp, lower = Inf, upper = 0, columns = i)
    }
    if(get.constr.type(lp)[i] == ">=" ){
      set.bounds(new_lp, lower = 0, upper = Inf, columns = i)
    }
  }


  return(new_lp)
}

