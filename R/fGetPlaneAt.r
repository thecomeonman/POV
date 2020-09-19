#' @export
fGetPlaneAt = function(
    mOriginCoordinates,
    mVector1,
    mVector2
    # nScreenPlaneCoefficients
) {

    # nDivisionPlaneCoefficients = nScreenPlaneCoefficients, right? Why isn't that working?
    mAnotherDivisionPlaneAxisVector = fCrossProduct(
        ( mVector1 / ( sum(mVector1^2) ^ 0.5 ) ),
        mVector2
    )

    # if the above two vectors are parallel, i.e. viewing direction is along z axis
    # for now assuming this case can never happen
    if ( sum(mAnotherDivisionPlaneAxisVector) == 0 ) {
        print(mAnotherDivisionPlaneAxisVector)
    }

    #     nDivisionPlaneCoefficients = c(
    #         nScreenPlaneCoefficients[1:3],
    #         nScreenPlaneCoefficients[4] - cbind(mOriginCoordinates, 1) %*% nScreenPlaneCoefficients
    #     )

    # } else {

        mAnotherDivisionPlaneAxisVector = ( mAnotherDivisionPlaneAxisVector / sum(mAnotherDivisionPlaneAxisVector ^ 2 ) ^ 0.5 )
        nDivisionPlaneCoefficients = fCrossProduct(
            ( mVector1 / ( sum(mVector1^2) ^ 0.5 ) ), 
            mAnotherDivisionPlaneAxisVector
        )
        nDivisionPlaneCoefficients = c(
            nDivisionPlaneCoefficients,
            -sum(nDivisionPlaneCoefficients * mOriginCoordinates)
        )
    # }

    nDivisionPlaneCoefficients

}
