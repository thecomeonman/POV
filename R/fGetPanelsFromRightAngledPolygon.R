#' @example
#' dt_tile_breaks = fGetPanelsFromRightAngledPolygon(
#'    mPolygonCoordinates = cbind(c(2,2,1),c(0,1,0),c(1,5,1)),
#'    panel_resolution_long = 0.2 # hori or veri limit to this size
#' )
#'ggplot(dt_tile_breaks) +
#'   geom_polygon(
#'      aes(x = x, y = y, group = panel_index),
#'      color = 'black', fill = 'blue', alpha = 0.1
#'   ) +
#'   coord_fixed()
#' @export
fGetPanelPointsFromRightAngledPolygon = function(
   mPolygonCoordinates,
   panel_resolution_long,
   panel_resolution_short = NULL
) {

   print('WARNING: works only for right angled triangles and rectangles right now!')

   if ( is.null(panel_resolution_short) ) {
      panel_resolution_short = panel_resolution_long
   }

   side_lengths = rowSums(
      ( mPolygonCoordinates - mPolygonCoordinates[c(2:nrow(mPolygonCoordinates),1),] ) ^ 2
   ) ^ 0.5

   # shortest side needs to be first for triangle
   # doesn't matter for rectangle
   if ( nrow(mPolygonCoordinates) == 3 ) {

      if ( which.max(side_lengths) == 2 ) {

         mPolygonCoordinates = mPolygonCoordinates[c(3,1,2),]
         side_lengths = side_lengths[c(3,1,2)]

      }

   }

   splits = ceiling(side_lengths/panel_resolution_long)
   # because otherwise the triangle has uneven size panels
   # on the hypotenus because the shorter side connects n to n + 1
   # which doesn't map to a diff of 1 on the longer side index. And
   # mapping it to the longer sides n to n + x index means you violate
   # the panel_resolution_long
   if ( nrow(mPolygonCoordinates) == 3 ) {

      splits[1] = max(splits[2], splits[1])
      splits[2] = splits[1]

   } else if ( nrow(mPolygonCoordinates) == 4 ) {

      if ( side_lengths[1] < side_lengths[2] ) {
         splits[1] = ceiling(side_lengths[1] / panel_resolution_short)
      } else {
         splits[2] = ceiling(side_lengths[2] / panel_resolution_short)
      }

   }

   dt_splits_1 = data.table(
      t(t(
         c(0:splits[1]) %*% t(
            ( mPolygonCoordinates[1,] - mPolygonCoordinates[2,] ) / splits[1]
         )
      ) + mPolygonCoordinates[2,])
   )
   dt_splits_1[, panel_index_1 := .I]
   dt_splits_2 = data.table(
      t(t(
         c(0:splits[2]) %*% t(
            ( mPolygonCoordinates[3,] - mPolygonCoordinates[2,] ) / splits[2]
         )
      ) + mPolygonCoordinates[2,])
   )
   dt_splits_1[, split_2 := .I]

   dt_tile_breaks = expand.grid(
      panel_index_1 = 0:splits[1],
      panel_index_2 = 0:splits[2]
   )

   dt_tile_breaks = data.table(dt_tile_breaks)

   # chop off opp side triangle
   if ( nrow(mPolygonCoordinates) == 3 ) {

      dt_tile_breaks = dt_tile_breaks[
         panel_index_2 + panel_index_1 <= splits[1]
         # panel_index_2 + panel_index_1 < splits[1]
         # panel_index_2 < panel_index_1 * (splits[2]/splits[1])
      ]

   }

   side_1_split = ( mPolygonCoordinates[1,] - mPolygonCoordinates[2,] ) / splits[1]
   side_2_split = ( mPolygonCoordinates[3,] - mPolygonCoordinates[2,] ) / splits[2]

   dt_tile_breaks[, x := mPolygonCoordinates[2,1] + ( side_1_split[1] * panel_index_1 )]
   dt_tile_breaks[, y := mPolygonCoordinates[2,2] + ( side_1_split[2] * panel_index_1 )]
   dt_tile_breaks[, z := mPolygonCoordinates[2,3] + ( side_1_split[3] * panel_index_1 )]
   dt_tile_breaks[, x := x + ( side_2_split[1] * panel_index_2 )]
   dt_tile_breaks[, y := y + ( side_2_split[2] * panel_index_2 )]
   dt_tile_breaks[, z := z + ( side_2_split[3] * panel_index_2 )]

   dt_tile_breaks

}

#' @export
fGetPanelsFromPanelPoints = function(
   dt_tile_breaks,
   keep_raw_indices = F
) {

   dt_tile_breaks = merge(
      dt_tile_breaks,
      dt_tile_breaks[,
                     list(
                        point_seq = 1:4,
                        panel_index_1 = c(panel_index_1, panel_index_1, panel_index_1+1,panel_index_1+1),
                        panel_index_2 = c(panel_index_2, panel_index_2+1, panel_index_2+1,panel_index_2)
                     ),
                     list(
                        split_1_x = panel_index_1,
                        split_2_x = panel_index_2
                     )
      ],
      c('panel_index_1','panel_index_2')
   )

   dt_tile_breaks[, panel_index := .GRP, list(split_1_x, split_2_x)]
   if ( keep_raw_indices ) {
      setnames(
         dt_tile_breaks,
         c('split_1_x', 'split_2_x'),
         c('panel_index_1', 'panel_index_2')
      )
   } else {
      dt_tile_breaks[, c('split_1_x', 'split_2_x') := NULL]
   }

   dt_tile_breaks = merge(
      dt_tile_breaks,
      dt_tile_breaks[, .N, list(panel_index)][N >= 3],
      c('panel_index')
   )

   dt_tile_breaks[, N := NULL]

   setorder(dt_tile_breaks, panel_index, point_seq)

   dt_tile_breaks

}
