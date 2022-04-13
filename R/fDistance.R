#' @export
fDistance <- function(
    mStartCoordinates,
    mEndCoordinates = NULL
){

  if ( is.null(mEndCoordinates) ) {
    mEndCoordinates = matrix(rep(0, ncol(mStartCoordinates)), 1)
  }

  vnDistance = 0

  for ( i in seq(ncol(mStartCoordinates)) ) {

    vnDistance = vnDistance + ( ( mStartCoordinates[,i] - mEndCoordinates[,i]) ^ 2 )

  }

  vnDistance = vnDistance ^ 0.5

  return(vnDistance)

}
