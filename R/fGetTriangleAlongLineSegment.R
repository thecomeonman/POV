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

   slope = (y - yend) / (x - xend)

   slopeperpendicular = -1/slope

   c = y - ( slopeperpendicular * x )

   rbind(
      cbind((x + (baselength/2)), c + ( slopeperpendicular * (x + (baselength/2)) )),
      cbind((x - (baselength/2)), c + ( slopeperpendicular * (x - (baselength/2)) )),
      cbind(xend,yend)
   )

}



