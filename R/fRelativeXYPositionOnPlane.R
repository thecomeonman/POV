#' Transforms 3d Points on a plane to the relative 2d coordinates from a specified origin
#' @param mCoordinates the raw coordinates [x,y,z] with any number of rows
#' @param mScreenCoordinates a point on the plane
#' @param mYAxis The Y axis vector [x,y,z] with one row
#' @param mXAxis The X axis vector [x,y,z] with one row
#' @example
#' # project of (1,2,1) on the z = 1 XY plane
#' fRelativeXYPositionOnPlane(cbind(1,2,1), c(0,0,1),cbind(0,1,0),cbind(1,0,0))
#' @export
fRelativeXYPositionOnPlane = function (
    mCoordinates,
    mScreenCoordinates,
    mYAxis,
    mXAxis
) {

    if ( all(mYAxis == 0)) {
        mYAxis = matrix(c(0,1,0), ncol = 3)
    }

    if ( all(mXAxis == 0)) {
        mXAxis = matrix(c(1,0,0), ncol = 3)
    }

    mPlaneVectors = mCoordinates - matrix( rep(mScreenCoordinates, nrow(mCoordinates)), ncol = 3, byrow = T )

    vnYCoord = mPlaneVectors %*% t(mYAxis)
    vnXCoord = mPlaneVectors %*% t(mXAxis)

    mResult = cbind(
        x = vnXCoord,
        y = vnYCoord
    )

    colnames(mResult) = c('x','y')

    return ( mResult )

}
