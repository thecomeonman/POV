#' @example
#' xy1_plane_angle_rad = pi/2
#' xz2_plane_angle_rad = pi/2
#' mCoordinates = cbind(c(1,2),c(1,2),c(1,2))
#' mOriginCoordinates = cbind(0,0,0)
#' @export
fGetReorientedCoordinates = function (
   mCoordinates,
   mOriginCoordinates,
   xy1_plane_angle_rad,
   xz2_plane_angle_rad
) {

   mRelativeCoordinates = cbind(
      mCoordinates[,1] - mOriginCoordinates[,1],
      mCoordinates[,2] - mOriginCoordinates[,2],
      mCoordinates[,3] - mOriginCoordinates[,3]
   )

   # x axis is 0, y axis is pi/2

   xy_angle = atan2(mRelativeCoordinates[,2], mRelativeCoordinates[,1])

   xy_lengths = ( ( mRelativeCoordinates[,1] ^ 2 )  + ( mRelativeCoordinates[,2] ^ 2 ) ) ^ 0.5

   xy_angle_changed = xy_angle + xy1_plane_angle_rad

   mRelativeCoordinates[,1] = xy_lengths * cos(xy_angle_changed)
   mRelativeCoordinates[,2] = xy_lengths * sin(xy_angle_changed)

   # x axis is 0, z axis is pi/2

   xz_angle = atan2(mRelativeCoordinates[,3], mRelativeCoordinates[,1])

   xz_lengths = ( ( mRelativeCoordinates[,1] ^ 2 )  + ( mRelativeCoordinates[,3] ^ 2 ) ) ^ 0.5

   xz_angle_changed = xz_angle + xz2_plane_angle_rad

   mRelativeCoordinates[,1] = xz_lengths * cos(xz_angle_changed)
   mRelativeCoordinates[,3] = xz_lengths * sin(xz_angle_changed)

   mRelativeCoordinates

}
