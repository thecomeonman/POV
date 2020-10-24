#' @export
fDistance <- function(
    vnx, vny, vnendx, vnendy
){

  distResult = (
      ( (vnx - vnendx) ^ 2 ) +
      ( (vny - vnendy) ^ 2 )
  ) ^ 0.5

  return(distResult)

}
