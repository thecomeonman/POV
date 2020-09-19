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
                viCoordinatesToTransform[2] == 1 &
                iTreatAs != 3
            ) {

                iNextPoint = viCoordinatesToTransform[2] + 1

                vnDistancesFromPlane = cbind(mCoordinates[c(viCoordinatesToTransform[2], iNextPoint),], 1) %*% nDivisionPlaneCoefficients

                mReplacementPoints = mCoordinates[viCoordinatesToTransform[2], ] - ( diff(mCoordinates[c(iNextPoint, viCoordinatesToTransform[2]),]) * ( 0.000000000001 + ( abs(vnDistancesFromPlane[1]) / ( abs(vnDistancesFromPlane[1]) + abs(vnDistancesFromPlane[2]) ) ) ) )

                viReplacementPointSequence = viInputPoints[c(viCoordinatesToTransform[2])]

            } else if (
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

            if ( iCoordinatesRow == 1 + length(vbCoordinatesToTransform) ) {
                bEvaluate = T
            } else {
                bEvaluate = vbCoordinatesToTransform[iCoordinatesRow]
            }

            bEvaluate = ifelse(is.na(bEvaluate), F, bEvaluate)

            if ( bEvaluate ) {

                nOtherPoint = mCoordinates[iCoordinatesRow,]

                if ( all(!is.na(nOtherPoint))) {
                    if ( all(nOtherPoint == mScreenCoordinates ) ) {

                        mSolution = mScreenCoordinates

                    } else if ( all(nOtherPoint == mOriginCoordinates ) ) {

                        mSolution = mOriginCoordinates

                    } else if (
                        sum(nScreenPlaneCoefficients[1:3] * nOtherPoint) == nScreenPlaneCoefficients[4]
                    ) {

                        mSolution = nOtherPoint

                    } else if (
                        all ( ( nOtherPoint - mOriginCoordinates ) * ( mScreenCoordinates - mOriginCoordinates ) == 0 )
                    ) {

                        mSolution = cbind(NA, NA, NA)

                    } else {

                        mLHS = mLHSBase
                        mRHS = mRHSBase

                        viCoefficientsToIncorporate = which(nOtherPoint != mOriginCoordinates)

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
                                b = ( mOriginCoordinates[,iIndex2] - nOtherPoint[iIndex2] )
                                a = ( mOriginCoordinates[,iIndex1] - nOtherPoint[iIndex1] )
                                mLHSIncrement[iIndex1] = + b
                                mLHSIncrement[iIndex2] = - a

                                mRHSIncrement = (
                                    ( + b * ( nOtherPoint[iIndex1] ) ) +
                                    ( - a * ( nOtherPoint[iIndex2] ) )
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
