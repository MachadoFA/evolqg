#' Remove Size Variation
#'
#' Removes the effect of a size vector in a covariance matrix.
#'
#' @param cov.matrix Covariance matrix
#' @param isometric If TRUE, the isometric size vector of is removed. If FALSE (default), the first principal component is removed.
#' @details Function sets the first eigenvalue to zero.
#' @return Altered covariance matrix with no variation on former first principal component
#' @author Diogo Melo, Guilherme Garcia, Fabio Machado
#' @export
#' @examples
#' cov.matrix <- RandomMatrix(10, 1, 1, 10)
#' no.size.cov.matrix <- RemoveSize(cov.matrix)
#' eigen(cov.matrix)
#' eigen(no.size.cov.matrix)
#' @keywords size
RemoveSize <- function (cov.matrix, isometric=FALSE) {
  if (isometric) {
    k<-nrow(cov.matrix)
    size.vector <- rep(1/sqrt(k),times=k)
    size.var <- Evolvability(cov.matrix,size.vector)
    size.factor <- size.vector * sqrt(size.var)
  } else {
    cov.matrix.svd  <-  svd(cov.matrix)
    size.eigen.vector <- cov.matrix.svd$u[, 1]
    size.eigen.value <- cov.matrix.svd$d[1]
    size.factor <- size.eigen.vector * sqrt(size.eigen.value)
  }
  
  cov.matrix.size.removed <- cov.matrix - size.factor %*% t(size.factor)
  return (cov.matrix.size.removed)
}
