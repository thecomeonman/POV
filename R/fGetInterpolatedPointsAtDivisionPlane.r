#' Handles the logic needed by the mViewBeginsFromCoordinates parm in
#' fGetTransformedCoordinates to get the points at the dividing plane
#' @param mCoordinates matrix with three columns [x,y,z] with >= one rows
#' @param nDivisionPlaneCoefficients matrix with four columns [a,b,c,d] such that
#' the equation of the plan is ax + by + cz + d = 0. This is the plane across which
#' the data could be spread across
#' @param bOriginDestinationInPositiveDirection boolean. Dictates which side
#' of the plane should be treated as behind and which front
#' @param iTreatAs Explained in mViewBeginsFromCoordinates in fGetTransformedCoordinates.
#' @param viInputPoints if you're tracking the order of the points already then
#' you can pass that vector and it will be updated. Can be left null if you don't
#' care for tracking the order of the points.
#' @export
fGetInterpolatedPointsAtDivisionPlane = function(
    mCoordinates,
    nDivisionPlaneCoefficients,
    bOriginDestinationInPositiveDirection,
    iTreatAs,
    viInputPoints = NULL
) {

    if ( is.null(viInputPoints) ) {
        viInputPoints = seq(nrow(mCoordinates))
    }

    repeat {

        vbCoordinatesToTransform = c(
            bOriginDestinationInPositiveDirection == (
                cbind(mCoordinates, 1) %*% nDivisionPlaneCoefficients >= 0
            )
        )

        if ( all(vbCoordinatesToTransform[!is.na(vbCoordinatesToTransform)]) ) {
            break
        }

        viFrontOfScreenStretches = cumsum(
            c(
                1,
                abs(
                    diff(
                        # sapply(
                        #     vbCoordinatesToTransform,
                        #     function(x) ifelse(is.na(x),-1,x)
                        # )
                        na.locf(vbCoordinatesToTransform)
                    ) != 0
                )
            )
        )
        
        viCoordinatesToTransform = table(viFrontOfScreenStretches[!vbCoordinatesToTransform])
        viCoordinatesToTransform = which(
            viFrontOfScreenStretches == as.integer(names(viCoordinatesToTransform)[1])
        )

        # in case it's just one point
        viCoordinatesToTransform = range(viCoordinatesToTransform)


        if (
            # at the start of a path
            viCoordinatesToTransform[2] == 1 &
            iTreatAs != 3
        ) {

            iNextPoint = viCoordinatesToTransform[2] + 1

            vnDistancesFromPlane = cbind(mCoordinates[c(viCoordinatesToTransform[2], iNextPoint),], 1) %*% nDivisionPlaneCoefficients

            mReplacementPoints = mCoordinates[viCoordinatesToTransform[2], ] - ( diff(mCoordinates[c(iNextPoint, viCoordinatesToTransform[2]),]) * ( 0.000000000001 + ( abs(vnDistancesFromPlane[1]) / ( abs(vnDistancesFromPlane[1]) + abs(vnDistancesFromPlane[2]) ) ) ) )

            viReplacementPointSequence = viInputPoints[c(viCoordinatesToTransform[2])]

        } else if (
            # at the end of a path
            viCoordinatesToTransform[1] == nrow(mCoordinates) &
            iTreatAs != 3
        ) {

            iPrevPoint = viCoordinatesToTransform[1] - 1

            vnDistancesFromPlane = cbind(
                mCoordinates[c(iPrevPoint, viCoordinatesToTransform[1]),],
            1) %*% nDivisionPlaneCoefficients

            mReplacementPoints = mCoordinates[viCoordinatesToTransform[1], ] - ( diff(mCoordinates[c(iPrevPoint, viCoordinatesToTransform[1]),]) * ( 0.000000000001 +  ( abs(vnDistancesFromPlane[2]) / ( abs(vnDistancesFromPlane[1]) + abs(vnDistancesFromPlane[2]) ) ) ) )

            viReplacementPointSequence = viInputPoints[c(viCoordinatesToTransform[1])]

        } else {
            # all other conditions

            iPrevPoint = viCoordinatesToTransform[1] - 1
            iNextPoint = viCoordinatesToTransform[2] + 1

            iPrevPoint[iPrevPoint == 0] = nrow(mCoordinates)
            iNextPoint[iNextPoint == nrow(mCoordinates) + 1] = 1

            vnDistancesFromPlane = cbind(mCoordinates[c(iPrevPoint, viCoordinatesToTransform, iNextPoint),], 1) %*% nDivisionPlaneCoefficients

            mReplacementPoints = rbind(
                mCoordinates[viCoordinatesToTransform[1], ] - ( diff(mCoordinates[c(iPrevPoint, viCoordinatesToTransform[1]),]) * ( 0.000000000001 + ( abs(vnDistancesFromPlane[2]) / ( abs(vnDistancesFromPlane[1]) + abs(vnDistancesFromPlane[2]) ) ) ) ),
                cbind(NA,NA,NA),
                mCoordinates[viCoordinatesToTransform[2], ] - ( diff(mCoordinates[c(iNextPoint, viCoordinatesToTransform[2]),]) * ( 0.000000000001 + ( abs(vnDistancesFromPlane[3]) / ( abs(vnDistancesFromPlane[3]) + abs(vnDistancesFromPlane[4]) ) ) ) )
            )

            viReplacementPointSequence = c(
                viInputPoints[viCoordinatesToTransform[1]],
                NA,
                viInputPoints[viCoordinatesToTransform[2]]
            )

        }

        if ( viCoordinatesToTransform[1] == 1 ) {

            viInputPoints = c(
                viReplacementPointSequence,
                viInputPoints[-viCoordinatesToTransform]
            )
            mCoordinates = rbind(
                mReplacementPoints,
                mCoordinates[-viCoordinatesToTransform, ]
            )


        } else if ( viCoordinatesToTransform[2] == length(vbCoordinatesToTransform) ) {

            viInputPoints = c(
                viInputPoints[-viCoordinatesToTransform],
                viReplacementPointSequence
            )
            mCoordinates = rbind(
                mCoordinates[-viCoordinatesToTransform, ],
                mReplacementPoints
            )

        } else {

            viInputPoints = c(
                viInputPoints[1:(viCoordinatesToTransform[1]-1)],
                viReplacementPointSequence,
                viInputPoints[(viCoordinatesToTransform[2]+1):nrow(mCoordinates)]
            )

            mCoordinates = rbind(
                mCoordinates[1:(viCoordinatesToTransform[1]-1), ],
                mReplacementPoints,
                mCoordinates[(viCoordinatesToTransform[2]+1):nrow(mCoordinates),]
            )

        }

    }

    return (
        list(
            mCoordinates = mCoordinates,
            viInputPoints = viInputPoints
        )
    )

}