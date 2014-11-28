#the WayPoint class
#' An S4 class to represent a waypoint
#'
#' @details
#' WayPointS4 class documentation
#'
#' @include GeoPointS4.r
#'
setClass(
  "WayPoint",
  representation(time = "POSIXlt",
                 heading = "numeric",
                 depth = "numeric",
                 speed = "numeric",
                 depth_unit = "character",
                 speed_unit = "character"),
  contains="GeoPoint",
  prototype(time=NA,
            heading=NA_real_,
            depth=NA_real_,
            speed=NA_real_,
            depth_unit="m",
            speed_unit="knots"),
  validity=function(object){
    #cat("~~~ WayPoint:inspector ~~~\n");
    if (!is.na(object@heading)){
      if(object@heading<0 | object@heading>360){
        return(paste("[WayPoint: validation] invalid heading value (",object@heading,")",sep=""));
      }
    }
    if (!is.na(object@depth)) {
      if(object@depth<0){
        return(paste("[WayPoint: validation] invalid depth value (",object@depth,")",sep=""));
      }
    }
    return(TRUE)
  }
)


#initialize method
setMethod(
  f = "initialize",
  signature = "WayPoint",
  definition = function(.Object,lat,lon,time,heading,depth,speed,depth_unit,speed_unit){
    #cat("~~~ WayPoint:initializer ~~~\n");
    .Object@lat <- lat
    .Object@lon <- lon
    .Object@time <- time
    .Object@heading <- heading
    .Object@depth <- depth
    .Object@speed <- speed
    .Object@depth_unit <- depth_unit
    .Object@speed_unit <- speed_unit
    validObject(.Object);
    return(.Object);
  }
);


#accessor methods
setMethod(
  f = "getLat",
  signature = "WayPoint",
  definition = function(object){
    #cat("~~~ WayPoint:getLat ~~~\n");
    return(object@lat);
  }
);

setMethod(
  f = "setLat",
  signature = "WayPoint",
  definition = function(object,value){
    #cat("~~~ WayPoint:setLat ~~~\n");
    object@lat <- value;
    return(object);
  }
);

setMethod(
  f = "getLon",
  signature = "WayPoint",
  definition = function(object){
    #cat("~~~ WayPoint:getLon ~~~\n");
    return(object@lon);
  }
);

setMethod(
  f = "setLon",
  signature = "WayPoint",
  definition = function(object,value){
    #cat("~~~ WayPoint:setLon ~~~\n");
    object@lon <- value;
    return(object);
  }
);

setMethod(
  f = "getHeading",
  signature = "WayPoint",
  definition = function(object){
    #cat("~~~ WayPoint:getHeading ~~~\n");
    return(object@heading);
  }
);

setMethod(
  f = "setHeading",
  signature = "WayPoint",
  definition = function(object,value){
    #cat("~~~ WayPoint:setHeading ~~~\n");
    object@heading <- value;
    return(object);
  }
);

setMethod(
  f = "getDepth",
  signature = "WayPoint",
  definition = function(object){
    #cat("~~~ WayPoint:getDepth ~~~\n");
    return(object@depth);
  }
);

setMethod(
  f = "setDepth",
  signature = "WayPoint",
  definition = function(object,value){
    #cat("~~~ WayPoint:setDepth ~~~\n");
    object@depth <- value;
    return(object);
  }
);

setMethod(
  f = "getSpeed",
  signature = "WayPoint",
  definition = function(object){
    #cat("~~~ WayPoint:getSpeed ~~~\n");
    return(object@speed);
  }
);

setMethod(
  f = "setSpeed",
  signature = "WayPoint",
  definition = function(object,value){
    #cat("~~~ WayPoint:setSpeed ~~~\n");
    object@speed <- value;
    return(object);
  }
);

setMethod(
  f = "getTime",
  signature = "WayPoint",
  definition = function(object){
    #cat("~~~ WayPoint:getTime ~~~\n");
    return(object@time);
  }
);

setMethod(
  f = "setTime",
  signature = "WayPoint",
  definition = function(object,value){
    #cat("~~~ WayPoint:setTime ~~~\n");
    object@time <- value;
    return(object);
  }
);

setMethod(
  f = "getDepthUnit",
  signature = "WayPoint",
  definition = function(object){
    #cat("~~~ WayPoint:getDepthUnit ~~~\n");
    return(object@depth_unit);
  }
);

setMethod(
  f = "setDepthUnit",
  signature = "WayPoint",
  definition = function(object,value){
    #cat("~~~ WayPoint:setDepthUnit ~~~\n");
    object@depth_unit <- value;
    return(object);
  }
);

setMethod(
  f = "getSpeedUnit",
  signature = "WayPoint",
  definition = function(object){
    #cat("~~~ WayPoint:getSpeedUnit ~~~\n");
    return(object@speed_unit);
  }
);

setMethod(
  f = "setSpeedUnit",
  signature = "WayPoint",
  definition = function(object,value){
    #cat("~~~ WayPoint:setSpeedUnit ~~~\n");
    object@speed_unit <- value;
    return(object);
  }
);

#summary method
setMethod(
  f = "summary",
  signature = "WayPoint",
  definition = function(object){
    cat("lat",object@lat,",lon",object@lon,"\n");
  }
);
