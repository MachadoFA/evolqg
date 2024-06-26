#' Integration measure based on eigenvalue dispersion
#'
#' Calculates integration indexes based on eigenvalue dispersion of covariance 
#' or correlation matrices. 
#'
#' @param matrix Covariance/correlation matrix
#' @param sd Logical. Default is FALSE. If TRUE, estimates eigenvalue standard 
#' deviation. If FALSE, estimate the eigenvalue variance.
#' @param rel Logical. If TRUE, scales eigenvalue dispersion value by the 
#' theoretical maximum.
#' @param sample Default is NULL. If a integer is provided, function calculates 
#' the expected integration value for that particular sample size and returns 
#' value as a deviation from the expected.
#' @param keep.positive Logical. If TRUE, non-positive eigenvalues are removed from calculation
#' @details This function quantifies morphological integration as the dispersion 
#' of eigenvalues in a matrix. It takes either a
#' covariance or a correlation matrix as input, and there is no need to discern 
#' between them.The output will depend on the combination of parameters 
#' specified during input. 
#' 
#' As default, the function calculates the relative eigenvalue variance of the 
#' matrix, which expresses the eigenvalue variance as a ratio between the 
#' actual variance and the theoretical maximum for a matrix of the same size 
#' and same amount of variance (same trace), following Machado et al. (2019). If 
#' sd=TRUE, the dispersion is measured with the standard deviation of 
#' eigenvalues instead of the variance (Pavlicev, 2009). If the sample size is 
#' provided, the function automatically calculates the expected integration 
#' value for a matrix of the same size but with no integration (e.g. a matrix 
#' with all eigenvalues equal). In that case, the result is given as a deviation 
#' from the expected and is invariant to sample size (Wagner, 1984).
#' 
#' @return Integration index based on eigenvalue dispersion.
#' @export
#' @author Fabio Andrade Machado
#' @author Diogo Melo
#' @seealso \code{\link{CalcR2}}, \code{\link{CalcICV}}
#' @references Machado, Fabio A., Alex Hubbe, Diogo Melo, Arthur Porto, and 
#' Gabriel Marroig. 2019. "Measuring the magnitude of morphological 
#' integration: The effect of differences in morphometric representations and 
#' the inclusion of size." Evolution 33:402–411.
#' @references Pavlicev, Mihaela, James M. Cheverud, and Gunter P. Wagner. 2009.
#' "Measuring Morphological Integration Using Eigenvalue Variance." Evolutionary
#'  Biology 36(1):157-170.
#' @references Wagner, Gunther P. 1984. "On the eigenvalue distribution of 
#' genetic and phenotypic dispersion matrices: evidence for a nonrandom 
#' organization of quantitative character variation." Journal of Mathematical 
#' Biology 21(1):77–95.
#' @examples
#' cov.matrix <- RandomMatrix(10, 1, 1, 10)
#' # calculates the relative eigenvalue variance of a covariance matrix
#' CalcEigenVar(cov.matrix)
#' 
#' # calculates the relative eigenvalue variance of a correlation matrix
#' CalcEigenVar(cov2cor(cov.matrix))
#' 
#' # calculates the relative eigenvalue standard deviation of a covariance 
#' # matrix
#' CalcEigenVar(cov.matrix, sd=TRUE)
#' 
#' # calculates the absolute eigenvalue variance of a covariance matrix
#' CalcEigenVar(cov.matrix, rel=FALSE)
#' 
#' # to evaluate the effect of sampling error on integration
#' x<-mvtnorm::rmvnorm(10, sigma=cov.matrix)
#' sample_cov.matrix<-var(x)
#' 
#' # to contrast values of integration obtained from population covariance 
#' # matrix
#' CalcEigenVar(cov.matrix)
#' # with the sample integration
#' CalcEigenVar(sample_cov.matrix)
#' # and with the integration measured corrected for sampling error
#' CalcEigenVar(sample_cov.matrix,sample=10)
#' 
#' @keywords correlation
#' @keywords integration
#' @keywords eigenvalues

CalcEigenVar<-function(matrix, sd=FALSE, rel=TRUE, sample=NULL, 
                       keep.positive=TRUE) {
  if(!isSymmetric(matrix)) stop("covariance matrix must be symmetric.")
  
  eigenv <- eigen(matrix)$values
  if(keep.positive) eigenv <- eigenv[which(zapsmall(eigenv)>0)]
  m <- mean(eigenv)
  n <- length(eigenv)
  sqrd <- (eigenv-m)^2
  obs <- sum(sqrd)/n
  max <- (n-1)*sum(eigenv)^2/n^2
  
  if (!is.null(sample)) {
    obs <- obs-(max/sample)
    max <- max+(max/sample)
  }
  
  if (sd) {
    obs <- sqrt(obs)
    max <- sqrt(max)
  }
  
  if (rel){
    Evar <- obs/max
  } else Evar <- obs
  
  return(Evar)
}
