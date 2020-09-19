
#' @export
fRemovePointsBehindDividingPlane = function(
    mCoordinates,
    nDivisionPlaneCoefficients,
    bOriginDestinationInPositiveDirection,
    iTreatAs
) {

    # print('m1')
    # print(head(mCoordinates))


    # retaining at most two points behind the screen for each stretch of points
    # behind the screen so that closed polygons from the points ahead of the screen
    # can be computed
    vbCoordinatesToTransform = c(
        bOriginDestinationInPositiveDirection == (
            cbind(mCoordinates, 1) %*% nDivisionPlaneCoefficients >= 0
        )
    )

    # print('m3')
    # print(head(mCoordinates))

    # return empty dataset if all points are behind screen
    if ( all(!vbCoordinatesToTransform) ) {

        # print('setting to zero length')
        viPointsToKeep = c()

    } else {

        # if being treated as isolated points then just drop the points not in the
        # view, else treat it as a path or a polyogn and from every stretch of
        # points outside the view, retain the first and last point and drop the
        # inbetween n - 2 points
        if ( iTreatAs == 1 ) {

            viPointsToKeep = which(vbCoordinatesToTransform)

        } else {

            # drawing continuous links between consecutive sets of points that 
            # are in front or behind a screen
            viFrontOfScreenStretches = cumsum(
                c(
                    1,
                    abs(
                        diff(vbCoordinatesToTransform)
                    )
                )
            )

            # dropping the n-2 points in between and keeping the first and last
            # point of every stretch that's behind the screen. If the first or 
            # the last stretch are behind then dropping n -1 points. Basically
            # keeping only points adjacent to a point in front of the screen becase
            # they are needed for the interpolation and reomving all other points
            # behind the screen
            # print('m2')
            # print(head(mCoordinates))
            # print(nrow(mCoordinates))
            # print(seq(nrow(mCoordinates)))
            viPointsToKeep = setdiff(
                seq(nrow(mCoordinates)),
                unlist(
                    c(
                        lapply(
                            unique(
                                viFrontOfScreenStretches[
                                    vbCoordinatesToTransform == F
                                ]
                            ),
                            function( iChunk ) {

                                viIndices = which(viFrontOfScreenStretches == iChunk)

                                if ( length(viIndices) > 1 ) {

                                    if ( iChunk == 1 ) {

                                        viToRetain = viIndices[length(viIndices)]

                                    } else if ( iChunk == max(viFrontOfScreenStretches) ) {

                                        viToRetain = viIndices[1]

                                    } else {

                                        viToRetain = range(viIndices)

                                    }

                                    viIndicesToRemove = setdiff(viIndices, viToRetain)

                                } else {

                                    viIndicesToRemove = -1
                                }

                                viIndicesToRemove

                            }
                        )
                    )
                )
            )

            # print(seq(nrow(mCoordinates)))
            
            viPointsToKeep = sort(viPointsToKeep)


            # if the start and the end stretches of the coordinates are both behind the polygon
            # then compress them into a single stretch and cycle the polygon coordinates
            # such that the starting coordinate is ahead of the screen.
            # THIS CHANGES THE ORDER OF POINTS for closed polygons
            # if the first and last stretches are both behind the screen
            if ( all(!vbCoordinatesToTransform[viFrontOfScreenStretches %in% range(viFrontOfScreenStretches)]) ) {

                # looping the polygon such that the last point is now the
                # second last and the first is now the last
                if ( iTreatAs == 3 ) {

                    viPointsToKeep = c(viPointsToKeep[-1],viPointsToKeep[1])

                }

            }

            
        }

    }

    # print('returning')
    # print(viPointsToKeep)

    return ( viPointsToKeep )

}