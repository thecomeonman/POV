#' Mainly to draw arrowheads
#' @param baselength the bigger it is, the wider the base of the arrow
#' @export
fGetTriangleAlongLineSegment = function(
   x = 75,
   xend = 88.5,
   y = 8,
   yend = 17.6,
   baselength = 2
) {

   if ( x != xend ) {

      slope = (y - yend) / (x - xend)

      if ( slope == 0 ) {
         slopeperpendicular = Inf
      } else {
         slopeperpendicular = -1/slope
      }

   } else {

      slopeperpendicular = 0

   }

   c = y - ( slopeperpendicular * x )

   rbind(
      # cbind((x + (baselength/2)), c + ( slopeperpendicular * (x + (baselength/2)) )),
      # cbind((x - (baselength/2)), c + ( slopeperpendicular * (x - (baselength/2)) )),
      c(x + (cos(atan(slopeperpendicular)) * baselength),y + (sin(atan(slopeperpendicular)) * baselength)),
      c(x - (cos(atan(slopeperpendicular)) * baselength),y - (sin(atan(slopeperpendicular)) * baselength)),
      cbind(xend,yend)
   )

}
