#' Projections of 3d points on a plane
#' @param mCoordinates matrix with three columns [x,y,z] with >= one rows
#' @param mOriginCoordinates three column, one row matrix specifying coordinates
#' of where the scene is being viewed from
#' @param nScreenPlaneCoefficients [a,b,c,d] such that ax + by + cz = d the
#' plane on which mCoordinates should be projected
#' @export
fGetProjectionsOnPlane = function(
    mCoordinates,
    mOriginCoordinates,
    nScreenPlaneCoefficients
) {

    mLHSBase = matrix(nScreenPlaneCoefficients[1:3], ncol = 3)
    mRHSBase = matrix(nScreenPlaneCoefficients[4], ncol = 1)

    for ( iCoordinatesRow in seq(nrow(mCoordinates)) ) {

        # shouldn't try to interpolate the NA filler points
        # between two interpolated points
        # should move this check to the parent function though and not keep it
        # here
        bEvaluate = ifelse(
            is.na(mCoordinates[iCoordinatesRow, 1]),
            F,
            T
        )

        if ( bEvaluate ) {

            vnPointToProject = mCoordinates[iCoordinatesRow,]

            if ( all(!is.na(vnPointToProject))) {

                if (

                    # on screen plane
                    sum(nScreenPlaneCoefficients[1:3] * vnPointToProject) == nScreenPlaneCoefficients[4]

                ) {

                    mSolution = vnPointToProject

                } else if (

                    # on origin plane
                    # all ( ( vnPointToProject - mOriginCoordinates ) * ( nScreenPlaneCoefficients - mOriginCoordinates ) == 0 )
                    abs(sum(c(vnPointToProject,1) * nScreenPlaneCoefficients) - sum(c(mOriginCoordinates,1) * nScreenPlaneCoefficients)) < 0.0000000001

                ) {

                    mSolution = cbind(NA, NA, NA)

                } else {

                    mLHS = mLHSBase
                    mRHS = mRHSBase

                    viCoefficientsToIncorporate = which(abs(vnPointToProject - mOriginCoordinates) > 0.0000000001)

                    for ( iCoeff in setdiff(1:3, viCoefficientsToIncorporate) ) {

                        mLHSIncrement = matrix(c(0,0,0), ncol = 3)
                        mLHSIncrement[iCoeff] = 1

                        mRHSIncrement = mOriginCoordinates[,iCoeff]

                        mLHS = rbind(
                            mLHS,
                            mLHSIncrement
                        )

                        mRHS = rbind(
                            mRHS,
                            mRHSIncrement
                        )

                    }

                    if ( length(viCoefficientsToIncorporate) >= 2 ) {

                        mCoeffCombinations = t(combn(viCoefficientsToIncorporate, 2))

                        for ( iCoefficientCombnRow in seq(pmin(2,nrow(mCoeffCombinations))) ) {

                            mLHSIncrement = matrix(c(0,0,0), ncol = 3)

                            iIndex1 = mCoeffCombinations[iCoefficientCombnRow, 1]
                            iIndex2 = mCoeffCombinations[iCoefficientCombnRow, 2]

                            # ( x - x0 ) / a = ( y - y0)  / b
                            # a = x1 - x0
                            # b = y1 - y0
                            # bx - ay = bx0 - ay0
                            b = ( mOriginCoordinates[,iIndex2] - vnPointToProject[iIndex2] )
                            a = ( mOriginCoordinates[,iIndex1] - vnPointToProject[iIndex1] )
                            mLHSIncrement[iIndex1] = + b
                            mLHSIncrement[iIndex2] = - a

                            mRHSIncrement = (
                                ( + b * ( vnPointToProject[iIndex1] ) ) +
                                ( - a * ( vnPointToProject[iIndex2] ) )
                            )

                            mLHS = rbind(
                                mLHS,
                                mLHSIncrement
                            )

                            mRHS = rbind(
                                mRHS,
                                mRHSIncrement
                            )

                        }

                    }

                    # print(mLHS)
                    # print(mRHS)
                    mSolution = t(solve(mLHS, mRHS))

                }

            } else {

                mSolution = cbind(NA, NA, NA)

            }

        } else {

            mSolution = cbind(NA, NA, NA)

        }

        if ( exists('mSolutions') ) {

            mSolutions = rbind(
                mSolutions,
                mSolution
            )

        } else {

            mSolutions = mSolution

        }


    }

    rownames(mSolutions) = NULL
    return ( mSolutions )

}
