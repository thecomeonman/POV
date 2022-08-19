#' Identifies the blocks in a grid through which a line segment goes
#'
#' @param dt_ls a data.table with x, y, xend, yend, and optionally id
#' @param x_lim the span of x coordinates on your grid - c(min x, max x)
#' @param y_lim the span of x coordinates on your grid - c(min y, max y)
#' @param x_gap the spacing between blocks of the grid on the x dim
#' @param y_gap the spacing between blocks of the grid on the y dim
#' @examples
#'

#'library(data.table)
#'dt_ls = rbind(
#'   data.table(x = -27, y = 7, xend = -37, yend = 22, id = 1),
#'   data.table(x = 27, y = 7, xend = 37, yend = 22, id = 2),
#'   data.table(x = 27, y = -7, xend = 37, yend = -17, id = 3),
#'   data.table(x = -27, y = -7, xend = -37, yend = -17, id = 4),
#'   data.table(x = -25, y = 25, xend = -10, yend = 35, id = 5),
#'   data.table(x = 25, y = 25, xend = 10, yend = 35, id = 6),
#'   data.table(x = 25, y = -25, xend = 10, yend = -35, id = 7),
#'   data.table(x = -25, y = -25, xend = -10, yend = -35, id = 8),
#'   data.table(x = -8, y = -20, xend = 8, yend = -20, id = 9),
#'   data.table(x = 0, y = 0, xend = 0, yend = 20, id = 10)
#')
#'dt_grids_ls = fGetBlocksThroughWhichLineSegmentsPass(
#'   dt_ls = dt_ls,
#'   x_lim = c(-60,60),
#'   y_lim = c(-40,40),
#'   x_gap = 5,
#'   y_gap = 5
#')
#'
#'
#'library(ggplot2)
#'ggplot() +
#'   geom_tile(
#'      data = dt_grids_ls,
#'      aes(x = x_block, y = y_block),
#'      fill = 'cyan',
#'      color = 'black'
#'   ) +
#'   geom_segment(
#'      data = dt_ls,
#'      aes(x = x, y = y, xend = xend, yend = yend),
#'      color = 'red',
#'      arrow=arrow(type="closed",ends="last",length=unit(0.1,"cm"))
#'   ) +
#'   coord_fixed()
#'
#' @import data.table
#' @export

fGetBlocksThroughWhichLineSegmentsPass =  function(
   dt_ls,
   x_lim = c(-60,60),
   y_lim = c(-40,40),
   x_gap = 5,
   y_gap = 5
) {

   if ( !'id' %in% colnames(dt_ls) ) {
      dt_ls[, id := .I]
   }

   x_cuts = seq(x_lim[1], x_lim[2] - x_gap, x_gap)
   y_cuts = seq(y_lim[1], y_lim[2] - y_gap, y_gap)
   # x_cuts = seq(x_lim[1]+(x_gap/2),x_lim[2]-(x_gap/2),x_gap)
   # y_cuts = seq(y_lim[1]+(y_gap/2),y_lim[2]-(y_gap/2),y_gap)

   dt_ls_x_cuts = rbindlist(
      lapply(
         x_cuts,
         function ( x_cut ) {

            dt_ls[,
               y_intersection := (  ( x_cut - x ) * ( y - yend ) / ( x - xend ) ) + y
            ]

            dt_ls_cut = dt_ls[
               (
                  ( ( x_cut - x ) * ( x_cut - xend ) ) <= 0 # x and xend are on opp sides of x-cut
               ) &
               (
                  ( ( y_intersection - y ) * ( y_intersection - yend ) ) <= 0 # similarly y
               ) & {
                  !is.infinite(y_intersection) # if x == xend
               },
               list(
                  y,
                  yend,
                  id,
                  y_intersection
               )
            ]

            if ( nrow(dt_ls_cut) > 0 ) {

               dt_ls_cut = dt_ls_cut[
                  ( y == yend ) |
                  (
                     ( y_intersection != y ) &
                     ( y_intersection != yend )
                  )
               ]

               dt_ls_cut = dt_ls_cut[,list(id,y_intersection)]
               dt_ls_cut[, x_cut := x_cut]
               # dt_ls_cut = rbind(
               #    dt_ls_cut,
               #    copy(dt_ls_cut)[, x_cut := x_cut + x_gap][, next_y_intersection := y_intersection][, y_intersection := NA],
               #    fill = T
               # )

            } else {
               dt_ls_cut = data.table()
            }

            dt_ls_cut

         }
      ),
      fill = T
   )


   dt_ls_y_cuts = rbindlist(
      lapply(
         y_cuts,
         function ( y_cut ) {

            dt_ls[,
                      x_intersection := ( ( y_cut - y ) * ( x - xend ) / ( y - yend ) ) + x
            ]

            dt_ls_cut = dt_ls[
               (
                  ( ( y_cut - y ) * ( y_cut - yend ) ) <= 0
               ) &
                  (
                     ( ( x_intersection - x ) * ( x_intersection - xend ) ) <= 0
                  ) & {
                     !is.infinite(x_intersection)
                  },
               list(
                  x,
                  xend,
                  id,
                  x_intersection
               )
            ]


            if ( nrow(dt_ls_cut) > 0 ) {

               dt_ls_cut = dt_ls_cut[
                  ( x == xend ) |
                     (
                        ( x_intersection != x ) &
                           ( x_intersection != xend )
                     )
               ]

               dt_ls_cut = dt_ls_cut[,list(id,x_intersection)]

               dt_ls_cut[, y_cut := y_cut]
               # dt_ls_cut = rbind(
               #    dt_ls_cut,
               #    copy(dt_ls_cut)[, y_cut := y_cut + y_gap][, next_x_intersection := x_intersection][, x_intersection := NA],
               #    fill = T
               # )

            } else {
               dt_ls_cut = data.table()
            }

            dt_ls_cut

         }
      ),
      fill = T
   )

   dt_blocks = data.table(expand.grid(x_cuts, y_cuts))

   setnames(dt_blocks, c('x','y'))

   dt_passing_through = rbindlist(lapply(
      seq(nrow(dt_blocks)),
      function( block_index ) {

         block = dt_blocks[block_index, c(x,y)]

         dt_through = data.table()
         dt_through_increment = dt_ls_x_cuts[
            ( x_cut == block[1] ) &
               ( y_intersection >= block[2] ) &
               ( y_intersection <= block[2] + y_gap ),
            list(id,x_cut,y_intersection)
         ]

         if ( nrow(dt_through_increment) ) {

            setnames(
               dt_through_increment,
               c('x_cut','y_intersection'),
               c('x','y')
            )
            dt_through_increment[, y_block := block[2] + (y_gap/2)]

            dt_through_increment = dt_through_increment[,
               list(x_block = block[1] + c(x_gap/2,-x_gap/2)),
               list(id,y_block,x,y)
            ]

            dt_through = rbind(dt_through,dt_through_increment)

         }

         dt_through_increment = dt_ls_y_cuts[
            ( y_cut == block[2] ) &
               ( x_intersection >= block[1] ) &
               ( x_intersection <= block[1] + x_gap ),
            list(id,y_cut,x_intersection)
         ]

         if ( nrow(dt_through_increment) ) {

            setnames(
               dt_through_increment,
               c('x_intersection','y_cut'),
               c('x','y')
            )
            dt_through_increment[, x_block := block[1] + (x_gap/2)]

            dt_through_increment = dt_through_increment[,
              list(y_block = block[2] + c(y_gap/2,-y_gap/2)),
              list(id,x_block,x,y)
            ]

            dt_through = rbind(dt_through,dt_through_increment)

         }

         dt_through

      }
   ), fill = T)

   dt_passing_through = dt_passing_through[,
                                           list(
                                              passing_through = 1
                                           ),
                                           list(id, x, y, x_block, y_block)
   ]


   dt_passing_through = dt_passing_through[,
                                           list(
                                              passing_through = sum(passing_through)
                                           ),
                                           list(id, x_block, y_block)
   ]


   dt_passing_through = merge(
      dt_passing_through,
      dt_ls[, list(id,x,y,xend,yend)],
      'id'
   )

   dt_passing_through = rbind(
      dt_passing_through[
         passing_through > 1
      ][, comment := 'end' ],
      dt_passing_through[
         (
            abs(x - x_block) <= (x_gap/2) &
               abs(y - y_block) <= (y_gap/2)
         ) |
         (
            abs(xend - x_block) <= (x_gap/2) &
               abs(yend - y_block) <= (y_gap/2)
         )
      ][, comment := 'through' ]
   )

   dt_passing_through = dt_passing_through[, list(id, comment, x_block, y_block)]

   dt_passing_through

}
