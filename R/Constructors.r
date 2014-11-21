#Constructor functions,
#wrap the call to "new"
#function name matches the class name (lowercase)

#constructor for class GeoPoint
geopoint <- function(lat=NA_real_,lon=NA_real_){
  #cat("~~~ geopoint:constructor ~~~\n");
  new(Class="GeoPoint",lat=lat,lon=lon)
}

#constructor for class WayPoint
waypoint <- function(time=NA,lat=NA_real_,lon=NA_real_,heading=NA_real_,
                     depth=NA_real_,speed=NA_real_,depth_unit="m",speed_unit="knots"){
  #cat("~~~ waypoint:constructor ~~~\n");
  new(Class="WayPoint",
      time=(if (is.na(time)) {strptime("1jan1900 00:00:00","%d%b%Y %H:%M:%S")} else {time}),
      lat=lat,lon=lon,
      heading=heading,depth=depth,speed=speed,
      depth_unit=depth_unit,speed_unit=speed_unit)
}
