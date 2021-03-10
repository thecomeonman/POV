#' Getting plane coefficients from two vectors and a point that lie on the plane
#' @return [a,b,c,d] such that ax + by + cz = d
#' @export
fGetPlaneAt = function(
    mOriginCoordinates,
    mNormalVector = NULL,
    mVector1 = NULL,
    mVector2 = NULL
    # nScreenPlaneCoefficients
) {

    if ( all(!is.null(mNormalVector)) ) {

        return (
            c(
               -mNormalVector,
               sum(
                   -mNormalVector * mOriginCoordinates
               )
            )
        )

    }

    mAnotherDivisionPlaneAxisVector = fCrossProduct(
        ( mVector1 / ( sum(mVector1^2) ^ 0.5 ) ),
        mVector2
    )

    # if the above two vectors are parallel, i.e. viewing direction is along z axis
    # Don't know what a good default to this is
    if ( all(mAnotherDivisionPlaneAxisVector == 0 ) ) {

        # nDivisionPlaneCoefficients = c(
        #     nScreenPlaneCoefficients[1:3],
        #     nScreenPlaneCoefficients[4] - cbind(mOriginCoordinates, 1) %*% nScreenPlaneCoefficients
        # )
        stop('Two parallel vectors')

    } else {

        mAnotherDivisionPlaneAxisVector = ( mAnotherDivisionPlaneAxisVector / sum(mAnotherDivisionPlaneAxisVector ^ 2 ) ^ 0.5 )
        nDivisionPlaneCoefficients = fCrossProduct(
            ( mVector1 / ( sum(mVector1^2) ^ 0.5 ) ),
            mAnotherDivisionPlaneAxisVector
        )
        nDivisionPlaneCoefficients = c(
            nDivisionPlaneCoefficients,
            -sum(nDivisionPlaneCoefficients * mOriginCoordinates)
        )
    }

    nDivisionPlaneCoefficients

}
