#' @example
#' xy1_plane_angle_rad = pi/2
#' xz2_plane_angle_rad = pi/2
#' mCoordinates = cbind(c(1,2),c(1,2),c(1,2))
#' mOriginCoordinates = cbind(0,0,0)
#' @export
fGetReorientedCoordinates = function (
   mCoordinates,
   mOriginCoordinates,
   plane_angle_rad = 0,
   plane = 'xy'
) {

   mRelativeCoordinates = cbind(
      'x' = mCoordinates[,'x'] - mOriginCoordinates[,'x'],
      'y' = mCoordinates[,'y'] - mOriginCoordinates[,'y'],
      'z' = mCoordinates[,'z'] - mOriginCoordinates[,'z']
   )

   if ( nchar(plane) == 2 ) {

      axes = strsplit(plane,'')[[1]]

      # x axis is 0, y axis is pi/2

      angles = atan2(mRelativeCoordinates[,axes[2]], mRelativeCoordinates[,axes[1]])

      lengths = ( ( mRelativeCoordinates[,axes[2]] ^ 2 )  + ( mRelativeCoordinates[,axes[1]] ^ 2 ) ) ^ 0.5

      angles_changed = angles + plane_angle_rad

      mRelativeCoordinates[,axes[2]] = lengths * sin(angles_changed)
      mRelativeCoordinates[,axes[1]] = lengths * cos(angles_changed)

      mRelativeCoordinates = cbind(
         'x' = mRelativeCoordinates[,1] + mOriginCoordinates[,1],
         'y' = mRelativeCoordinates[,2] + mOriginCoordinates[,2],
         'z' = mRelativeCoordinates[,3] + mOriginCoordinates[,3]
      )

   }

   mRelativeCoordinates

}
