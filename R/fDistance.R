#' @export
fDistance <- function(
    vnx, vny, vnz = 0, vnendx, vnendy,
    vnendz = 0
){

  distResult = (
      ( (vnx - vnendx) ^ 2 ) +
      ( (vny - vnendy) ^ 2 ) +
      ( (vnz - vnendz) ^ 2 )
  ) ^ 0.5

  return(distResult)

}
