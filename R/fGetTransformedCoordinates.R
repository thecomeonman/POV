#' Transforming coordinate spaces
#'
#' @param mCoordinates matrix with three columns - x,y,z coorinates.
#' if mCoordinates is an isolated point then it is treated
#' as an isolated point, if it is two points, it is treated as a line segment,
#' but 3 or more points are treated as closed polygons unless iTreatAs = F.
#' Recommendation is to ensure start point is repeated as end point in mCoordinates
#' and iTreatAs = F. That way, where the polygon / path went behind the
#' screen, there will be a marker specifying that the line has to be broken there
#' when plotting or anywhere else
#' @param mOriginCoordinates three column, one row matrix specifying coordinates of where
#' the scene is being viewed from
#' @param mScreenCoordinates three column, one row matrix specifying coordinates of
#' where the screen on which the scene is being projected on. Also dictates the
#' direction along which which the scene is being viewed from
#' @param iTreatAs 1 for isolated points, 2 for path, 3 for polygon
#' @param mViewBeginsFromCoordinates if NULL, all objects in front of origin, where
#' front is the direciton in which the screen coordinates are, are projected. If
#' a set of coordinates is given then all objects in front of that coordinate, where
#' front is the direciton in which the screen coordinates are, are retained, and
#' other objects are treated as behind the view. If this coordinate is on the opposite
#' side of the origin as the screen then you'll delete all objects and get an empty
#' array in the results
#' @return If inputs are valid, a matrix with 2 columns for the x,y coordinates
#' on the screen + optional columns linking continuous stretches in front of
#' the screening plane which can be used in geom_path(aes(group = V3)). If invalid
#' inputs then you'll get a NULL.
#' @import zoo
#' @export
fGetTransformedCoordinates = function (
    mCoordinates,
    mOriginCoordinates,
    mScreenCoordinates,
    mViewBeginsFromCoordinates = NULL,
    mZAxisVector = c(0,0,1),
    iTreatAs = 3
) {

    # print('~~~~')


    # some input parameters that need to be pre computed
    if ( T ) {


        # This is needed only for y axis of final projection
        mYAxisVectorOnScreen = mScreenCoordinates + ( mZAxisVector / ( sum(mZAxisVector^2) ^ 0.5 ) )


        # this is the plane on which to project the data
        # normal vector = (a,b,c)
        # a(x - x1) + b(y - y1) + c(z - z1) = 0
        nScreenPlaneCoefficients = c(
            mScreenCoordinates - mOriginCoordinates,
            sum( 
                ( mScreenCoordinates - mOriginCoordinates ) * mScreenCoordinates 
            )
        )


        # We can't let points all the way till on the screen plane be visualised because
        # the coordinates for them will be ~inf. So we'll only include points which
        # are at least a little ahead of mScreenCoordinates from the direction of mOriginCoordinates
        if ( is.null(mViewBeginsFromCoordinates) ) {

            nDivisionPlaneCoefficients = fGetPlaneAt(
                mOriginCoordinates,
                mZAxisVector,
                mScreenCoordinates - mOriginCoordinates
                # nScreenPlaneCoefficients
            )

        } else {

            nDivisionPlaneCoefficients = fGetPlaneAt(
                mViewBeginsFromCoordinates,
                mZAxisVector,
                mScreenCoordinates - mViewBeginsFromCoordinates
                # nScreenPlaneCoefficients
            )

        }

        bOriginDestinationInPositiveDirection = sum(nDivisionPlaneCoefficients * c(mOriginCoordinates, 1)) < 0

    }



    # dropping repeat points
    if ( T ) {

        viInputPoints = seq(nrow(mCoordinates))

        # removing repeat points
        if ( nrow(mCoordinates) > 1 & iTreatAs %in% c(2,3) ) {

            viPointsToKeep = c(
                T, 
                !rowSums(
                    matrix(
                        apply(
                            mCoordinates, 
                            2, 
                            diff
                        ), 
                        ncol = 3
                    ) ^ 2
                ) == 0
            )

            viInputPoints = viInputPoints[viPointsToKeep]
            mCoordinates = mCoordinates[viPointsToKeep,]

            if ( iTreatAs == 3 & nrow(mCoordinates) > 1 ) {

                if (
                    all(
                        mCoordinates[1,] == mCoordinates[nrow(mCoordinates),]
                    )
                ) {
                    viInputPoints = viInputPoints[-nrow(mCoordinates)]
                    mCoordinates = mCoordinates[-nrow(mCoordinates),]
                }

            }

        }

    }



    if ( T ) {
        
        # print('m0')
        # print(head(mCoordinates))
        # browser()
        viPointsToKeep = fRemovePointsBehindDividingPlane(
            mCoordinates = mCoordinates,
            nDivisionPlaneCoefficients = nDivisionPlaneCoefficients,
            bOriginDestinationInPositiveDirection = bOriginDestinationInPositiveDirection,
            iTreatAs = iTreatAs
        )

        # print('return')
        # print(viPointsToKeep)

        if ( length(viPointsToKeep) == 0 ) {
            
            return ( NULL )

        }

        mCoordinates = mCoordinates[viPointsToKeep,]
        viInputPoints = viInputPoints[viPointsToKeep]
        mCoordinates = matrix(mCoordinates, ncol = 3)




        # interpolating the connecting points between the adjacent behind screen - in front of screen points
        # such that the connecting point is a point on the screen
        # if there are two consecutive behind points then adding a place holder for an inbetween point between the two behind points
        # so that the polygon closes elegantly
        
        lReturn = fGetInterpolatedPointsAtDivisionPlane(
            mCoordinates = mCoordinates,
            nDivisionPlaneCoefficients = nDivisionPlaneCoefficients,
            bOriginDestinationInPositiveDirection = bOriginDestinationInPositiveDirection,
            iTreatAs = iTreatAs,
            viInputPoints = viInputPoints
        )
        
        viInputPoints = lReturn$viInputPoints
        mCoordinates = lReturn$mCoordinates
        rm(lReturn)


    }



    # pojrection on screen for points in front
    if ( T ) {

        # adding a vertical vector for knowing which way points up
        # can parameterise this also maybe
        mCoordinates = rbind(
            mCoordinates,
            mYAxisVectorOnScreen
        )

        mLHSBase = matrix(nScreenPlaneCoefficients[1:3], ncol = 3)
        mRHSBase = matrix(nScreenPlaneCoefficients[4], ncol = 1)
        mSolutions = matrix(c(0,0,0), ncol = 3)

        for ( iCoordinatesRow in seq(nrow(mCoordinates)) ) {

            # shouldn't try to interpolate the NA filler points
            # between two interpolated points
            bEvaluate = ifelse(
                is.na(mCoordinates[iCoordinatesRow, 1]), 
                F, 
                T
            )

            if ( bEvaluate ) {

                vnPointToProject = mCoordinates[iCoordinatesRow,]

                if ( all(!is.na(vnPointToProject))) {

                    if ( all(vnPointToProject == mScreenCoordinates ) ) {

                        mSolution = mScreenCoordinates

                    } else if ( all(vnPointToProject == mOriginCoordinates ) ) {

                        mSolution = mOriginCoordinates

                    } else if (
                        sum(nScreenPlaneCoefficients[1:3] * vnPointToProject) == nScreenPlaneCoefficients[4]
                    ) {

                        mSolution = vnPointToProject

                    } else if (
                        all ( ( vnPointToProject - mOriginCoordinates ) * ( mScreenCoordinates - mOriginCoordinates ) == 0 )
                    ) {

                        mSolution = cbind(NA, NA, NA)

                    } else {

                        mLHS = mLHSBase
                        mRHS = mRHSBase

                        viCoefficientsToIncorporate = which(vnPointToProject != mOriginCoordinates)

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

                        mSolution = t(solve(mLHS, mRHS))

                    }

                } else {

                    mSolution = cbind(NA, NA, NA)

                }

            } else {

                mSolution = cbind(NA, NA, NA)

            }

            mSolutions = rbind(
                mSolutions,
                mSolution
            )


        }

        mSolutions = mSolutions[-1,]
        mYAxis = mSolutions[nrow(mSolutions), ]
        mSolutions = matrix(mSolutions[-nrow(mSolutions), ], ncol = 3)
        mCoordinates = matrix(mCoordinates[-nrow(mCoordinates), ], ncol = 3)

    }

    # mCoordinates
    # mSolutions
    # mYAxis

    # rotating projections WRT to screening plane so as to get points in
    # two coordinates
    if ( T ) {

        mYAxisVectorNew = mYAxis - mScreenCoordinates
        if ( all(mYAxisVectorNew == 0)) {
            mYAxisVectorNew = matrix(c(0,1,0), ncol = 3)
        }
        mYAxisVectorNew = mYAxisVectorNew / sum(mYAxisVectorNew^2) ^ 0.5

        mZAxisVectorNew = mScreenCoordinates - mOriginCoordinates
        mZAxisVectorNew = mZAxisVectorNew / sum(mZAxisVectorNew^2) ^ 0.5

        mXAxisVectorNew = fCrossProduct(mZAxisVectorNew, mYAxisVectorNew)
        mXAxisVectorNew = mXAxisVectorNew / sum(mXAxisVectorNew^2) ^ 0.5

        mPlaneVectors = mSolutions - matrix( rep(mScreenCoordinates, nrow(mSolutions)), ncol = 3, byrow = T )

        vnYCoord = mPlaneVectors %*% t(mYAxisVectorNew)
        vnXCoord = mPlaneVectors %*% t(mXAxisVectorNew)

    }

    mResult = cbind(
        x = vnXCoord,
        y = vnYCoord
    )

    colnames(mResult) = c('x','y')

    # the placeholders between the two behind points being filled with a value
    # that lies just a little below the lowest point in the viz so it can be
    # chopped off with a ylim
    if ( F ) {

        mResult[
            which(is.na(mResult[,1])),
            2
        ] = min(mResult[,2], na.rm = T) - ( 0.0000001 * diff(range(mResult[,2], na.rm = T)) )

        mResult[
            which(is.na(mResult[,1])),
            1
        ] = 0

    } else {

        viPointsToFillIn = which(is.na(mResult[, 1]))

        if ( length(viPointsToFillIn) ) {

            if ( iTreatAs == 3 ) {

                viPointsHowToFillInMax = mResult[viPointsToFillIn - 1, 2] > mean(range(mResult[,2], na.rm = T))

                mResult[viPointsToFillIn[!viPointsHowToFillInMax], 2] =
                    min(mResult[,2], na.rm = T) -
                    # ( 0.0000001 * diff(range(mResult[,2], na.rm = T)) )
                    0

                mResult[viPointsToFillIn[viPointsHowToFillInMax], 2] =
                    max(mResult[,2], na.rm = T) +
                    # ( 0.0000001 * diff(range(mResult[,2], na.rm = T)) )
                    0

                mResult[viPointsToFillIn, 1] = ( mResult[viPointsToFillIn - 1, 1] + mResult[viPointsToFillIn + 1, 1] ) / 2

            } else if ( iTreatAs == 2 ) {

                mResult = cbind(mResult[,c('x','y')], group = cumsum(is.na(mResult[,1])))
                mResult = mResult[-viPointsToFillIn,]
                viInputPoints = viInputPoints[-viPointsToFillIn]
            }
        }


        if (
            iTreatAs == 2 &
            !'group' %in% colnames(mResult)
        ) {
            mResult = cbind(mResult[,c('x','y')], group = 1)
        }

    }

    # browser()
    row.names(mResult) = NULL
    mResult = cbind(
        mResult,
        inputOrder = viInputPoints
    )

    mResult

}
