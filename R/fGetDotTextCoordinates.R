#' @export
mark_jersey_no = function(
   jersey_no,
   x_panel_split = c(3,1,3),
   y_panel_split = c(6,7,8)
) {

   dt_panels = data.table()

   tens = jersey_no %/% 10
   ones = jersey_no %% 10

   ones_offset = 2
   if ( tens != 0 ) {

      ones_offset = 4

      dt_panels = rbind(
         dt_panels,
         mark_jersey_digit(
            tens,
            x_panel_split = 3,
            y_panel_split = 7
         )[,
           panel_index_2 := panel_index_2 + y_panel_split[1]
         ]
      )

   }

   dt_panels = rbind(
      dt_panels,
      mark_jersey_digit(
         ones,
         x_panel_split = 3,
         y_panel_split = 7
      )[,
        panel_index_1 := panel_index_1 + ones_offset
      ][,
        panel_index_2 := panel_index_2 + y_panel_split[1]
      ]
   )

   dt_panels

}

mark_jersey_digit = function(
   jersey_digit,
   x_panel_split = 3,
   y_panel_split = 7
) {

   dt_panels = data.table()

   # top h bar
   if ( jersey_digit %in% c(0,2,3,5,6,7,8,9) ) {
      dt_panels = rbind(
         dt_panels,
         data.table(
            panel_index_1 = 0:2,
            panel_index_2 = 7
         )
      )
   }

   # left v bar
   if ( jersey_digit %in% c(0,4,5,6,8,9) ) {
      dt_panels = rbind(
         dt_panels,
         data.table(
            panel_index_1 = 0,
            panel_index_2 = 7:4
         )
      )
   }

   # centre v bar
   if ( jersey_digit %in% c(1) ) {
      dt_panels = rbind(
         dt_panels,
         data.table(
            panel_index_1 = 1,
            panel_index_2 = 7:4
         )
      )
   }

   # right v bar
   if ( jersey_digit %in% c(0,2,3,4,7,8,9) ) {
      dt_panels = rbind(
         dt_panels,
         data.table(
            panel_index_1 = 2,
            panel_index_2 = 7:4
         )
      )
   }

   # centre h bar
   if ( jersey_digit %in% c(2,3,4,5,6,8,9) ) {
      dt_panels = rbind(
         dt_panels,
         data.table(
            panel_index_1 = 0:2,
            panel_index_2 = 4
         )
      )
   }

   # left v bar
   if ( jersey_digit %in% c(0,2,6,8) ) {
      dt_panels = rbind(
         dt_panels,
         data.table(
            panel_index_1 = 0,
            panel_index_2 = 4:1
         )
      )
   }

   # centre v bar
   if ( jersey_digit %in% c(1) ) {
      dt_panels = rbind(
         dt_panels,
         data.table(
            panel_index_1 = 1,
            panel_index_2 = 4:1
         )
      )
   }

   # right v bar
   if ( jersey_digit %in% c(0,3,4,5,6,7,8,9) ) {
      dt_panels = rbind(
         dt_panels,
         data.table(
            panel_index_1 = 2,
            panel_index_2 = 4:1
         )
      )
   }

   # bottom h bar
   if ( jersey_digit %in% c(0,2,3,5,6,8) ) {
      dt_panels = rbind(
         dt_panels,
         data.table(
            panel_index_1 = 0:2,
            panel_index_2 = 1
         )
      )
   }

   dt_panels

}
