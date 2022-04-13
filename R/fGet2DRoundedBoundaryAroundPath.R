#' Very suboptimal code to draw a polygon around a set of x,y values - like
#' a bubble around a path
#' @export
fGet2DRoundedBoundaryAroundPath = function(
   vx,
   vy,
   radius = 1
) {

   vnBoundaryCandidates = cbind(
      c(sapply(
         vx,
         function(x) {
            x + ( cos(seq(0,2*pi,2*pi/100)) * radius)
         }
      )),
      c(sapply(
         vy,
         function(y) {
            y + ( sin(seq(0,2*pi,2*pi/100)) * radius)
         }
      ))
   )

   vnBoundaryCandidates = vnBoundaryCandidates[
      chull(vnBoundaryCandidates[,1], vnBoundaryCandidates[,2]),
   ]

   vnBoundaryCandidates

}

vx = runif(10)
vy = runif(10)
