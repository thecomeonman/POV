#' Will definitely have lots of edge cases, very WIP
#' @import ggplot2
#' @import data.table
#' @import png
#' @export
fGetTextCoordinates = function(
   cString = NULL,
   nNormaliseOutput = T,
   bDoOutlines = F
) {

   cFile = paste0(tempfile(),'.png')

   p1 = ggplot() +
      geom_text(aes(x = 0, y = 0, label = cString), size = 10, color = 'black') +
      # geom_text(aes(x = 0, y = 0, label = cString), size = 10, color = 'black', fontface = 'bold') +
      # geom_text(aes(x = 0, y = 0, label = cString), size = 10, color = 'white') +
      theme(
         panel.grid = element_blank(),
         axis.title = element_blank(),
         axis.ticks = element_blank(),
         axis.text = element_blank(),
         panel.background = element_blank()
      )

   ggsave(
      p1,
      file = cFile,
      width = nchar(cString) * 0.5,
      height = length(strsplit(cString, '\n')[[1]]) * 1,
      dpi = 300

   )

   mImageData = readPNG(cFile)

   mImageDataBlack = (
      mImageData[,,1] < 0.5 &
      mImageData[,,2] < 0.5 &
      mImageData[,,3] < 0.5
   )

   dimImageDataBlack = dim(mImageDataBlack)
   # mImageDataEdge = matrix(T, ncol = dimImageDataBlack[2] - 2, nrow = dimImageDataBlack[1] - 2)
   # mImageDataEdge

   mImageDataBlackSubset = mImageDataBlack[
      2:(dimImageDataBlack[1]-1),
      2:(dimImageDataBlack[2]-1)
   ]

   if ( bDoOutlines ) {

      mImageDataBlackSubset = mImageDataBlackSubset & sum(
         ( mImageDataBlackSubset != mImageDataBlack[1:(dimImageDataBlack[1]-2),1:(dimImageDataBlack[2]-2)] ) +
         ( mImageDataBlackSubset != mImageDataBlack[1:(dimImageDataBlack[1]-2),2:(dimImageDataBlack[2]-1)] ) +
         ( mImageDataBlackSubset != mImageDataBlack[1:(dimImageDataBlack[1]-2),3:(dimImageDataBlack[2]-0)] ) +
         ( mImageDataBlackSubset != mImageDataBlack[2:(dimImageDataBlack[1]-1),1:(dimImageDataBlack[2]-2)] ) +
         # ( mImageDataBlackSubset != mImageDataBlack[2:(dimImageDataBlack[1]-1),2:(dimImageDataBlack[2]-1)] ) +
         ( mImageDataBlackSubset != mImageDataBlack[2:(dimImageDataBlack[1]-1),3:(dimImageDataBlack[2]-0)] ) +
         ( mImageDataBlackSubset != mImageDataBlack[3:(dimImageDataBlack[1]-0),1:(dimImageDataBlack[2]-2)] ) +
         ( mImageDataBlackSubset != mImageDataBlack[3:(dimImageDataBlack[1]-0),2:(dimImageDataBlack[2]-1)] ) +
         ( mImageDataBlackSubset != mImageDataBlack[3:(dimImageDataBlack[1]-0),3:(dimImageDataBlack[2]-0)] )
      ) > 3

   }

   dtEdges = rbindlist(
      lapply(
         (seq(nrow(mImageDataBlackSubset))),
         function(iRow) {

            x = which(mImageDataBlackSubset[iRow,])

            if ( length(x) ) {

               dtEdges = data.table(
                  y = -iRow,
                  x = x
               )

            } else {
               dtEdges = data.table()
            }

            dtEdges

         }
      )
   )

   dtEdges[, x := x - min(x)]
   dtEdges[, y := y - min(y)]

   if ( F ) {

      p1 = ggplot() + geom_point(data = dtEdges,aes(x=x,y =y)) + coord_fixed()
      print(p1)

   }

   if ( T ) {

      iEdgeID = 1L
      dtEdges = dtEdges[order(x)]
      dtEdges[, SNO := .I]
      dtEdges[, EdgeID := as.integer(NA)]

      # does one edge at a time, inefficient but low RAM footprint
      repeat {

         if ( dtEdges[, all(!is.na(EdgeID))] ) {

            break

         }

         dtEdges[
            which(is.na(EdgeID))[1],
            EdgeID := iEdgeID
         ]

         repeat {

            dtEdges = merge(
               dtEdges,
               dtEdges[
                  EdgeID == iEdgeID,
                  list(
                     x = c((x-1):(x+1), (x-1):(x+1), (x-1):(x+1))[-5],
                     y = c((y-1),(y-1),(y-1),y,y,y,(y+1),(y+1),(y+1))[-5]
                  ),
                  SNO
               ][, 1, list(x,y)][,
                  list(x,y,EdgeIDNew = iEdgeID)
               ],
               c('x','y'),
               all.x = T
            )

            if ( dtEdges[, any(is.na(EdgeID) & !is.na(EdgeIDNew))] ) {
               dtEdges[!is.na(EdgeIDNew), EdgeID := EdgeIDNew]
            } else {
               break
            }

            dtEdges[, EdgeIDNew := NULL]

         }

         dtEdges[, EdgeIDNew := NULL]

         iEdgeID = iEdgeID + 1

      }

   }

   if ( nNormaliseOutput ) {

      dtBaselineEdges = fGetTextCoordinates(
         cString = 'I',
         nNormaliseOutput = F
      )
      UppercaseY = dtBaselineEdges[, diff(range(y))]

      dtBaselineEdges = fGetTextCoordinates(
         cString = 'u',
         nNormaliseOutput = F
      )
      LowercaseY = dtBaselineEdges[, diff(range(y))]

      dtBaselineEdges = fGetTextCoordinates(
         cString = 'g',
         nNormaliseOutput = F
      )
      LowercaseLongY = dtBaselineEdges[, diff(range(y))]

      dtEdges[, y := y - ( LowercaseLongY - LowercaseY )]
      dtEdges[, y := y / ( UppercaseY )]
      dtEdges[, x := x / ( UppercaseY )]

   }

   dtEdges

}
