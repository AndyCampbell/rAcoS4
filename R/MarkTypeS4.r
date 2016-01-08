#'
#' An S4 class to represent an acoustic mark type
#'
#' @details
#' MarkTypeS4 class documentation
#'

setClass(
  "MarkType",
  representation(name = "character",
                 cruise_code = "character",
                 NASC_name = "character",
                 species = "character",
                 include = "logical",
                 haul_assignment = "character",
                 mixed_with = "character",
                 hauls = "character"),
  prototype(name = NA_character_,
            cruise_code = NA_character_,
            NASC_name = NA_character_,
            species = NA_character_,
            include = NA,
            haul_assignment = NA_character_,
            mixed_with = NA_character_,
            hauls = NA_character_),
  validity = function(object){

    #cat("~~~ MarkType:inspector ~~~\n");

    if (length(object@name)==0){
      stop("[MarkType: validation] name is mandatory")
    }

    return(TRUE)

  }

);

#initialize method
setMethod(
  f = "initialize",
  signature = "MarkType",
  definition = function(.Object,name,cruise_code,NASC_name,species,include,
                        haul_assignment,mixed_with=NA_character_,hauls){
    #cat("~~~ MarkType:initializer ~~~\n")
    .Object@name <- name
    .Object@cruise_code <- cruise_code
    .Object@NASC_name <- NASC_name
    .Object@species <- species
    .Object@include <- include
    .Object@haul_assignment <- haul_assignment
    .Object@mixed_with <- mixed_with
    .Object@hauls <- hauls

    #call the inspector
    validObject(.Object)
    return(.Object)
  }
)

setMethod(
  f = "getSpecies",
  signature = "MarkType",
  definition = function(object){
    return(object@species)
  }
)

setMethod(
  f = "getMatureCodes",
  signature = "MarkType",
  definition = function(object){
    #return the maturity codes for mature fish

    #find the code from the species details
    return(c(
      getMatureCodes(Species[[which(toupper(lapply(Species,getName))==toupper(object@species))]]),
      getSpentCodes(Species[[which(toupper(lapply(Species,getName))==toupper(object@species))]]))
    )
    }
)

setMethod(
  f = "getMixedSpecies",
  signature = "MarkType",
  definition = function(object){
    return(object@mixed_with)
  }
)

setMethod(
  f = "getName",
  signature = "MarkType",
  definition = function(object){
    return(object@name)
  }
)

setMethod(
  f = "getNASCName",
  signature = "MarkType",
  definition = function(object){
    return(object@NASC_name)
  }
)

setMethod(
  f = "getIncludeInEstimates",
  signature = "MarkType",
  definition = function(object){
    return(object@include)
  }
);

setMethod(
  f = "assignHaul",
  signature = "MarkType",
  definition = function(object,pos){

    #return the appropriate Haul number to assign
    #pos is the geopoint of the mark position

    dist <- rep(10e6,length(object@hauls))
    names(dist) <- object@hauls

    #loop over hauls assigned for this marktype
    for (i in 1:length(object@hauls)){

      val <- isValid(object@hauls[i])

      #retrieve the haul position
      if (val[1]) {

        shoot <- shootPos(object@hauls[i])

        dist[i] <- ToKm(shoot[1], shoot[2],
                      getLat(pos), getLon(pos))

        #dist[i] <- distVincentyEllipsoid(c(shootPos(object@hauls[i])@lon,
        #                             shootPos(object@hauls[i])@lat),
        #                             c(pos@lon,pos@lat))
      } else {
        stop(val[2])
      }

    }

    #return haul code for nearest haul to pos
    return(names(sort(dist))[1])

  }
)

setMethod(
  f = "abundance",
  signature = "MarkType",
  definition = function(object){

    #TO DO - return the total abundance at length for this MarkType

    return(0)
  }

)

ToKm <- function(Pt1y, Pt1x, Pt2y, Pt2x){

  #Pt=c(-Lat,Long)
  # converts 2 -lat,longs into a km dist.
  #  1.852 km/nmile

  Pt1y <- as.double(Pt1y)
  Pt1x < -as.double(Pt1x)
  Pt2x <- as.double(Pt2x)
  Pt2y <- as.double(Pt2y)
  a1 <- 111.14-.28*(cos(-2*Pt1y*pi/180)+cos(-2*Pt2y*pi/180))
  a0 <- 55.71*(cos(-Pt1y*pi/180)+cos(-Pt2y*pi/180))-.25*(cos(3*Pt1y*pi/180)+cos(3*Pt2y*pi/180))
  dist <- (a1*(Pt2y-Pt1y))^2+(a0*(Pt2x-Pt1x))^2
  xxx <- dist>0 & !is.na(dist)
  dist[xxx] <- dist[xxx]^0.5

  return(dist)

}
