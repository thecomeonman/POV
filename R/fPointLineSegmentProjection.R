#' http://stackoverflow.com/questions/849211/shortest-distance-between-a-point-and-a-line-segment

#' @param bReturnNormalisedPosition T - a 0 to 1 value is returned to 
#' for where the point lies between the start point and end point, F - actual
#' coordinates are returned
#'
#' @export
fPointLineSegmentProjection = function(
   vnX,
   vnY,
   vnStartX,
   vnStartY,
   vnEndX,
   vnEndY,
   bReturnNormalisedPosition = F
) {

   iTotalProjections = max(
      length(vnStartX),
      length(vnStartY),
      length(vnEndX),
      length(vnEndY),
      length(vnX),
      length(vnY)
   )

   vnExtensions = vector(
      mode = "numeric",
      length = iTotalProjections
   )

   # if line segment start == line segment end then euclid distance
   vnDistanceBetweenStartEnd = fDistance(
      vnx = vnStartX,
      vny = vnStartY,
      vnendx = vnEndX,
      vnendy = vnEndY
   )

   vnZeroDistances = vnDistanceBetweenStartEnd == 0

   vnExtensions[vnZeroDistances] = 0

   if ( any ( !vnZeroDistances ) ) {

      # else use the location of the projection of point to start,end
      # start + Extension ( start - end ) is being solved for Extension with
      #  [(point-start) . (end-start)] / |end - start|^2
      vnExtensions[!vnZeroDistances] = (
         ((vnX[!vnZeroDistances] - vnStartX[!vnZeroDistances]) * (vnEndX[!vnZeroDistances] - vnStartX[!vnZeroDistances])) + ((vnY[!vnZeroDistances] - vnStartY[!vnZeroDistances]) * (vnEndY[!vnZeroDistances] - vnStartY[!vnZeroDistances]))
      ) / (vnDistanceBetweenStartEnd[!vnZeroDistances] ^ 2)

      vnExtensions[!vnZeroDistances] = pmax(0, pmin(1, vnExtensions[!vnZeroDistances]))

   }

   if ( bReturnNormalisedPosition == F ){

      mProjectionCoordinates = cbind(
         vnStartX  + (vnExtensions * (vnEndX - vnStartX)),
         vnStartY  + (vnExtensions * (vnEndY - vnStartY))
      )

   }

   if ( bReturnNormalisedPosition == T ){
      return ( vnExtensions )
   } else {
      return ( mProjectionCoordinates )
   }

}
