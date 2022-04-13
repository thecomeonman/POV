#' Function to convert stereo or more vision to 3D
#' @export
fGet3DFromMultiple2D = function(
   lProjectionList,
   iResolution = 1000
) {

   # Each element of lProjectionList is a list with the elements:
   # mOriginCoordinates, mScreenCoordinates, mProjection

   for ( iProjectionElement in seq(length(lProjectionList)) ) {

      mScreenCoordinates = lProjectionList[[iProjectionElement]]$mScreenCoordinates
      mOriginCoordinates = lProjectionList[[iProjectionElement]]$mOriginCoordinates
      mProjection = lProjectionList[[iProjectionElement]]$mProjection

      lNewAxes = fGetOrthogonalAxes(
         mOriginCoordinates = mOriginCoordinates,
         mScreenCoordinates = mScreenCoordinates,
         mVectorPointingUpOnScreen = mVectorPointingUpOnScreen
      )

      lProjectionList[[iProjectionElement]]$dtLineEndPoints = rbindlist(lapply(
         seq(nrow(lProjectionList[[iProjectionElement]]$mProjection)),
         function (iProjectionRow) {

            setnames(
               data.table(
                  (
                     lProjectionList[[iProjectionElement]]$mProjection[iProjectionRow, 'x'] * lNewAxes$mXAxisVector
                  ) +
                     (
                        lProjectionList[[iProjectionElement]]$mProjection[iProjectionRow, 'y'] * lNewAxes$mYAxisVector
                     ) +
                     mScreenCoordinates
               ),
               c('x','y','z')
            )
         }
      ))

   }


   iResolution = 10000
   dtSolutions = rbindlist(lapply(
      1:nrow(lProjectionList[[1]]$mProjection),
      function(iProjectionRow) {

         print(iProjectionRow)

         mCandidates = cbind(
            lProjectionList[[1]]$mOriginCoordinates[,1] + (
               ( (0:iResolution)/iResolution ) *
                  ( lProjectionList[[1]]$dtLineEndPoints[iProjectionRow,x] - lProjectionList[[1]]$mOriginCoordinates[,1] )
            ),

            lProjectionList[[1]]$mOriginCoordinates[,2] + (
               ( (0:iResolution)/iResolution ) *
                  ( lProjectionList[[1]]$dtLineEndPoints[iProjectionRow,y] - lProjectionList[[1]]$mOriginCoordinates[,2] )
            ),

            lProjectionList[[1]]$mOriginCoordinates[,3] + (
               ( (0:iResolution)/iResolution ) *
                  ( lProjectionList[[1]]$dtLineEndPoints[iProjectionRow,z] - lProjectionList[[1]]$mOriginCoordinates[,3] )
            )
         )

         mDistanceToOtherLines = matrix(
            sapply(
               2:(length(lProjectionList)),
               function(iProjectionElement) {

                  if ( F ) {

                     mDeviations = cbind(
                        (mCandidates[,1] - lProjectionList[[iProjectionElement]]$mOriginCoordinates[,1]) /
                           (lProjectionList[[iProjectionElement]]$dtLineEndPoints[iProjectionRow,x] - lProjectionList[[iProjectionElement]]$mOriginCoordinates[,1]),
                        (mCandidates[,2] - lProjectionList[[iProjectionElement]]$mOriginCoordinates[,2]) /
                           (lProjectionList[[iProjectionElement]]$dtLineEndPoints[iProjectionRow,y] - lProjectionList[[iProjectionElement]]$mOriginCoordinates[,2]),
                        (mCandidates[,3] - lProjectionList[[iProjectionElement]]$mOriginCoordinates[,3]) /
                           (lProjectionList[[iProjectionElement]]$dtLineEndPoints[iProjectionRow,z] - lProjectionList[[iProjectionElement]]$mOriginCoordinates[,3])
                     )

                     mDeviations = matrix(
                        mDeviations[,
                                    c(
                                       (lProjectionList[[iProjectionElement]]$dtLineEndPoints[iProjectionRow,x] - lProjectionList[[iProjectionElement]]$mOriginCoordinates[,1]) != 0,
                                       (lProjectionList[[iProjectionElement]]$dtLineEndPoints[iProjectionRow,y] - lProjectionList[[iProjectionElement]]$mOriginCoordinates[,2]) != 0,
                                       (lProjectionList[[iProjectionElement]]$dtLineEndPoints[iProjectionRow,z] - lProjectionList[[iProjectionElement]]$mOriginCoordinates[,3]) != 0
                                    )
                        ],
                        nrow = 1+iResolution
                     )

                     if ( ncol(mDeviations) > 1 ) {

                        apply(
                           mDeviations,
                           1,
                           function(x) {
                              ifelse(
                                 max(x) > 1,
                                 Inf,
                                 sd(x)
                              )
                           }
                        )

                     } else {

                        0

                     }

                  }

                  # change to line segment distance
                  APB = fCrossProduct(
                     cbind(
                        mCandidates[,1] - lProjectionList[[iProjectionElement]]$dtLineEndPoints[iProjectionRow,x],
                        mCandidates[,2] - lProjectionList[[iProjectionElement]]$dtLineEndPoints[iProjectionRow,y],
                        mCandidates[,3] - lProjectionList[[iProjectionElement]]$dtLineEndPoints[iProjectionRow,z]
                     ),
                     cbind(
                        mCandidates[,1] - lProjectionList[[iProjectionElement]]$mOriginCoordinates[,1],
                        mCandidates[,2] - lProjectionList[[iProjectionElement]]$mOriginCoordinates[,2],
                        mCandidates[,3] - lProjectionList[[iProjectionElement]]$mOriginCoordinates[,3]
                     )
                  )

                  (rowSums(APB ^ 2) ^ 0.5)

               }
            ),
            ncol = (length(lProjectionList)-1)
         )

         setnames(
            data.table(t(mCandidates[which.min(rowSums(mDistanceToOtherLines)),])),
            c('x','y','z')
         )

      }
   ))


   if ( F ) {

      vnProportions = rep(0.5, length(lProjectionList))

      fGetInterpolatedPoint = function(
         vnProportions,
         lProjectionList,
         iWhichPoint
      ) {

         dtInterpolatedPoints = rbindlist(lapply(
            seq(length(lProjectionList)),
            function(i) {

               rbindlist(
                  lapply(
                     iWhichPoint,
                     function (j) {

                        data.table(
                           lProjectionList[[i]]$mOriginCoordinates +
                              (
                                 vnProportions[i] *
                                    ( lProjectionList[[i]]$dtLineEndPoints[j] - lProjectionList[[i]]$mOriginCoordinates )
                              )
                        )[,
                          SNO := j
                        ]
                     }
                  )
               )


            }
         ))

         dtInterpolatedPoints

      }

      fEvaluateInterpolatedPoint = function(
         x,
         lProjectionList,
         iWhichPoint
      ) {

         dtInterpolatedPoints = fGetInterpolatedPoint(
            vnProportions = x,
            lProjectionList,
            iWhichPoint
         )

         -max(
            dtInterpolatedPoints[, sd(V1), SNO][, V1],
            dtInterpolatedPoints[, sd(V2), SNO][, V1],
            dtInterpolatedPoints[, sd(V3), SNO][, V1]
         )

      }

      lSolutions = lapply(
         1:nrow(lProjectionList[[1]]$mProjection),
         function(iWhichPoint) {
            print(iWhichPoint)
            GA <- ga(
               type = "real-valued",
               fitness = function(x) { fEvaluateInterpolatedPoint(x,lProjectionList,iWhichPoint)},
               lower = rep(0, length(lProjectionList)),
               upper = rep(1, length(lProjectionList))
            )
            GA
         }
      )

      rbindlist(
         lapply(
            seq(length(lSolutions)),
            function(x) {
               data.table(t(colMeans(
                  fGetInterpolatedPoint(
                     vnProportions = lSolutions[[x]]@solution,
                     lProjectionList,
                     x
                  )
               )))
            }
         )
      )

   }

   dtSolutions

}
