#the GeoPoint class
#a simple geographic point, currently just a latitude and longitude
#in decimal degrees.
#Southern and Western hemispheres have a negative sign

#' @include GenericS4.r
#'
#' An S4 class to represent a geographical point
#'
#' @slot lat A length-one numeric vector representing latitude
#' @slot lon A length-one numeric vector representing longitude
#'
#'
setClass(
  "GeoPoint",
  representation(lat = "numeric",lon = "numeric"),
  prototype(lat = NA_real_, lon = NA_real_),
  validity=function(object){
    if (is.na(object@lat)) {return("[GeoPoint: validation] argument lat is mandatory")}
    if (is.na(object@lon)) {return("[GeoPoint: validation] argument lon is mandatory")}
    if (abs(object@lat)>90) {return("[GeoPoint: validation] invalid lat value")}
    if (abs(object@lon)>90) {return("[GeoPoint: validation] invalid lon value")}
    return(TRUE)}
)

#accessor methods
setMethod(
  f = "getLat",
  signature = "GeoPoint",
  definition = function(object){
    #cat("~~~ GeoPoint:getLat ~~~\n");
    return(object@lat);
  }
);

setMethod(
  f = "setLat",
  signature = "GeoPoint",
  definition = function(object,value){
    #cat("~~~ GeoPoint:setLat ~~~\n");
    object@lat <- value;
    return(object);
  }
);

setMethod(
  f = "getLon",
  signature = "GeoPoint",
  definition = function(object){
    #cat("~~~ GeoPoint:getLon ~~~\n");
    return(object@lon);
  }
);

setMethod(
  f = "setLon",
  signature = "GeoPoint",
  definition = function(object,value){
    #cat("~~~ GeoPoint:setLon ~~~\n");
    object@lon <- value;
    return(object);
  }
);


#summary method
setMethod(
  f = "summary",
  signature = "GeoPoint",
  definition = function(object){
    #cat("~~~ GeoPoint:summary ~~~\n");
    cat("lat",object@lat,",lon",object@lon,"\n");
    }
);

#show method
setMethod(
  f = "show",
  signature = "GeoPoint",
  definition = function(object){
    print(object@lat);
    print(object@lon);
  }
);
