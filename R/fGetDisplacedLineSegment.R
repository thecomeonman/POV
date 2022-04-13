#' @example
#' 
#' line_segment = c(
#'   x1 = 0, y1 = 0,
#'   x2 = 1, y2 = 1
#' )
#' 
#' line_segment_top = get_moved_line_segment_coordinates(
#'   x1 = line_segment['x1'], y1 = line_segment['y1'],
#'   x2 = line_segment['x2'], y2 = line_segment['y2'],
#'   distance = 1
#' )
#' 
#' @export
fGetDisplacedLineSegment = function(
  x1,y1,
  x2,y2,
  distance
) {
  
  # x1 y1 x2 y2 need to be equal length vectors
  
  m_theta = atan2(( y2 - y1 ), ( x2 - x1 ))
  m_2_theta = (m_theta + (pi/2))
  
  x_displacement = distance * cos(m_2_theta)
  y_displacement = distance * sin(m_2_theta)
  
  x_equals = x1 == x2
  y_equals = y1 == y2
  
  x_displacement[x_equals & y_equals] = NA
  y_displacement[x_equals & y_equals] = NA
  
  x_displacement[x_equals & !y_equals] = distance
  y_displacement[x_equals & !y_equals] = 0
  
  x_displacement[!x_equals & y_equals] = 0
  y_displacement[!x_equals & y_equals] = distance
  
  list(
    'x1' = x1 + x_displacement, 
    'y1' = y1 + y_displacement,
    'x2' = x2 + x_displacement, 
    'y2' = y2 + y_displacement
  )
  
}