#' Function to give orthogonal x and y axis vectors on a plane
#' @export
fGetOrthogonalAxes = function(
   mOriginCoordinates,
   mScreenCoordinates,
   mVectorPointingUpOnScreen
) {

   mYAxisVectorOnScreen = mScreenCoordinates + ( mVectorPointingUpOnScreen / ( sum(mVectorPointingUpOnScreen^2) ^ 0.5 ) )

   # this is the plane on which to project the data
   # normal vector = (a,b,c)
   # a(x - x1) + b(y - y1) + c(z - z1) = 0
   # This ia plane perpendicular to the origin - screen vector with
   # the screen coordinates being one of the points on this plane
   nScreenPlaneCoefficients = c(
      mScreenCoordinates - mOriginCoordinates,
      sum(
         ( mScreenCoordinates - mOriginCoordinates ) * mScreenCoordinates
      )
   )

   mYAxisVectorNew = fGetProjectionsOnPlane(
      mYAxisVectorOnScreen,
      mOriginCoordinates,
      nScreenPlaneCoefficients
   )

   mYAxisVectorNew = matrix(mYAxisVectorNew, ncol = 3)
   mYAxisVectorNew = mYAxisVectorNew - mScreenCoordinates
   mYAxisVectorNew = mYAxisVectorNew / sum(mYAxisVectorNew^2) ^ 0.5

   mVectorPointingUpOnScreenNew = mScreenCoordinates - mOriginCoordinates
   mVectorPointingUpOnScreenNew = mVectorPointingUpOnScreenNew / sum(mVectorPointingUpOnScreenNew^2) ^ 0.5

   mXAxisVectorNew = fCrossProduct(mVectorPointingUpOnScreenNew, mYAxisVectorNew)
   mXAxisVectorNew = mXAxisVectorNew / sum(mXAxisVectorNew^2) ^ 0.5

   list(
      mXAxisVector = mXAxisVectorNew,
      mYAxisVector = mYAxisVectorNew
   )

}
