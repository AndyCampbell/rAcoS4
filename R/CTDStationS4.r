#' An S4 class to represent a CTD station
#'
#' @slot code CTD station code
#'
#' @include GeoPointS4.r
#'
setClass(
  "CTDStation",
  representation(code = "character",
                 cruise_code = "character",
                 time = "POSIXlt",
                 depth = "numeric",
                 depth_unit = "character"),
  contains=c("GeoPoint"),
  prototype(code = NA_character_,
            cruise_code = NA_character_,
            time = NULL,
            depth = 0,
            depth_unit = "m"),
  validity=function(object){
    #cat("~~~ CTDStation:inspector ~~~\n");
    if (length(object@code)==0){
      stop("[CTDStation: validation] CTD code is mandatory");
      }
    if (length(object@cruise_code)==0){
      stop("[CTDStation: validation] Cruise code is mandatory");
    }
    return(TRUE);

  }
);

#initialize method
setMethod(
  f = "initialize",
  signature = "CTDStation",
  definition = function(.Object,lat,lon,code,cruise_code,time,depth,depth_unit){
    #cat("~~~ CTDStation:initializer ~~~\n");
    .Object@lat <- lat
    .Object@lon <- lon
    .Object@code <- code
    .Object@cruise_code <- cruise_code
    .Object@time <- time
    .Object@depth <- depth
    .Object@depth_unit <- depth_unit
    #call the inspector
    validObject(.Object);
    return(.Object);
  }
);


#accessor methods

#CTD code (readonly)
setMethod(
  f = "getCode",
  signature = "CTDStation",
  definition = function(object){
    cat("~~~ CTDStation:getCode ~~~\n");
    return(object@code);
  }
);

#Cruise Code (readonly)
setMethod(
  f = "getCruiseCode",
  signature = "CTDStation",
  definition = function(object){
    #cat("~~~ CTDStation:getCruiseCode ~~~\n");
    return(object@cruise_code);
  }
);

#Time
setMethod(
  f = "getTime",
  signature = "CTDStation",
  definition = function(object){
    #cat("~~~ CTDStation:getTime ~~~\n");
    return(object@time);
  }
);

setMethod(
  f = "setTime",
  signature = "CTDStation",
  definition = function(object,value){
    #cat("~~~ CTDStation:setTime ~~~\n");
    object@time <- value;
    return(object);
  }
);

#Depth
setMethod(
  f = "getDepth",
  signature = "CTDStation",
  definition = function(object){
    #cat("~~~ CTDStation:getDepth ~~~\n");
    return(object@depth);
  }
);

setMethod(
  f = "setDepth",
  signature = "CTDStation",
  definition = function(object,value){
    #cat("~~~ CTDStation:setDepth ~~~\n");
    object@depth <- value;
    return(object);
  }
);

#Depth Unit
setMethod(
  f = "getDepthUnit",
  signature = "CTDStation",
  definition = function(object){
    #cat("~~~ CTDStation:getDepthUnit ~~~\n");
    return(object@depth_unit);
  }
);

setMethod(
  f = "setDepthUnit",
  signature = "CTDStation",
  definition = function(object,value){
    #cat("~~~ CTDStation:setDepthUnit ~~~\n");
    object@depth_unit <- value;
    return(object);
  }
);
