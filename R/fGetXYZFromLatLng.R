#' @export
fGetXYZFromLatLng = function(
  lng_deg, lat_deg, 
  r = 6371
) {
  
  x = cos((lng_deg)*pi/180) * r * cos((lat_deg)*pi/180) 
  y = sin((lng_deg)*pi/180) * r * cos((lat_deg)*pi/180)
  z = sin((lat_deg)*pi/180) * r
  
  return ( list(x,y,z) )
  
}