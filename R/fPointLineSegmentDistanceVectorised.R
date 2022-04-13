# http://stackoverflow.com/questions/849211/shortest-distance-between-a-point-and-a-line-segment

#' @export
fPointLineSegmentDistanceVectorised = function(
   vnX,
   vnY,
   vnStartX,
   vnStartY,
   vnEndX,
   vnEndY
) {

   iTotalDistances = max(
      length(vnStartX),
      length(vnStartY),
      length(vnEndX),
      length(vnEndY),
      length(vnX),
      length(vnY)
   )

   vnDistances = vector(
      mode = "numeric",
      length = iTotalDistances
   )

   # if line segment start == line segment end then euclid distance
   vnDistanceBetweenStartEnd = fDistance(
      mStartCoordinates = cbind(vnStartX,vnStartY),
      mEndCoordinates = cbind(vnEndX,vnEndY)
   )

   vnZeroDistances = vnDistanceBetweenStartEnd == 0

   if ( any ( vnZeroDistances ) ) {

      vnDistances[vnZeroDistances] = fDistance(
         mStartCoordinates = cbind(vnX[vnZeroDistances],vnY[vnZeroDistances]),
         mEndCoordinates = cbind(vnStartX[vnZeroDistances],vnStartY[vnZeroDistances])
      )

   }

   if ( any ( !vnZeroDistances ) ) {

      # else use the location of the projection of point to start,end
      # start + Extension ( start - end ) is being solved for Extension with
      #  [(point-start) . (end-start)] / |end - start|^2
      vnExtensions = (
         ((vnX[!vnZeroDistances] - vnStartX[!vnZeroDistances]) * (vnEndX[!vnZeroDistances] - vnStartX[!vnZeroDistances])) + ((vnY[!vnZeroDistances] - vnStartY[!vnZeroDistances]) * (vnEndY[!vnZeroDistances] - vnStartY[!vnZeroDistances]))
      ) / (vnDistanceBetweenStartEnd[!vnZeroDistances] ^ 2)

      vnExtensions = pmax(0, pmin(1, vnExtensions))

      vnDistances[!vnZeroDistances] = fDistance(
         mStartCoordinates = cbind(vnX[!vnZeroDistances],vnY[!vnZeroDistances]),
         mEndCoordinates = cbind(
            vnStartX[!vnZeroDistances]  + (vnExtensions * (vnEndX[!vnZeroDistances] - vnStartX[!vnZeroDistances])),
            vnStartY[!vnZeroDistances]  + (vnExtensions * (vnEndY[!vnZeroDistances] - vnStartY[!vnZeroDistances]))
         )
      )

   }


   return ( vnDistances )

}
