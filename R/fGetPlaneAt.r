#' Getting plane coefficients from two vectors and a point that lie on the plane
#' @param mOriginCoordinates a point on the plane
#' @param mNormalVector vector perpedicular to the plane
#' @param mInPlaneVector1 a vector in the plane
#' @param mInPlaneVector2 a vector in the plane != mInPlaneVector1
#' You need to supply mOriginCoordinates and either mNormalVector or
#' mInPlaneVector1, mInPlaneVector2 to identify a plane
#' @example
#' fGetPlaneAt(mOriginCoordinates = cbind(1,1,1), mNormalVector = cbind(1,0,0))
#' fGetPlaneAt(mOriginCoordinates = cbind(1,1,1), mInPlaneVector1 = cbind(0,0,1), mInPlaneVector2 = cbind(0,1,0))
#' @return [a,b,c,d] such that ax + by + cz = d
#' @export
fGetPlaneAt = function(
    mOriginCoordinates,
    mNormalVector = NULL,
    mInPlaneVector1 = NULL,
    mInPlaneVector2 = NULL
) {

    if ( any(is.null(mNormalVector)) ) {

        mNormalVector = fCrossProduct(
            ( mInPlaneVector1 / ( sum(mInPlaneVector1^2) ^ 0.5 ) ),
            mInPlaneVector2
        )

    }

    if ( all(mNormalVector == 0 ) ) {

        stop('Two parallel vectors')

    }

    vnPlaneCoefficients = c(
       mNormalVector,
       sum(
           mNormalVector * mOriginCoordinates
       )
    )

    return (
        vnPlaneCoefficients
    )

}
