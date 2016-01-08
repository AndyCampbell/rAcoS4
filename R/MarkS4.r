#' An S4 class to represent an acoustic mark
#'
#' @details
#' MarkS4 class documentation
#'
#' @include WayPointS4.r
#'
setClass(
  "Mark",
  representation(cruise_code = "character",
                 stratum_code = "character",
                 transect_code = "character",
                 school_length = "numeric",
                 cell_length = "numeric",
                 NASC = "numeric",
                 position = "WayPoint",
                 marktype_name = "character",
                 haul_code = "character",
                 LF = "list",
                 CS = "numeric",
                 nearest_transect = "character"),
  prototype(cruise_code = NA_character_,
            stratum_code = NA_character_,
            transect_code = NA_character_,
            school_length = NA_real_,
            cell_length = NA_real_,
            NASC = NA_real_,
            position = NULL,
            marktype_name = NA_character_,
            haul_code = NA_character_,
            LF = list(),
            CS = NA_real_,
            nearest_transect = NA_character_),
  validity = function(object){

    #cat("~~~ Mark:inspector ~~~\n");

    if (length(object@cruise_code)==0){
      stop("[Mark: validation] cruise code is mandatory");
    }

    return(TRUE);

  }

);

#initialize method
setMethod(
  f = "initialize",
  signature = "Mark",
  definition = function(.Object,cruise_code,stratum_code,transect_code,
                        school_length,cell_length,NASC,position,marktype_name,
                        haul_code,LF=list(),CS,nearest_transect){
    #cat("~~~ Mark:initializer ~~~\n");
    .Object@cruise_code <- cruise_code
    .Object@stratum_code <- stratum_code
    .Object@transect_code <- transect_code
    .Object@school_length <- school_length
    .Object@cell_length <- cell_length
    .Object@NASC <- NASC
    .Object@position <- position
    .Object@marktype_name <- marktype_name
    .Object@haul_code <- haul_code
    #.Object@LF <- LF
    .Object@CS <- CS
    .Object@nearest_transect <- nearest_transect

    #call the inspector
    validObject(.Object);
    return(.Object);
  }
);

#accessor methods - many of these should be read only - they should be set by the costructor on loading the data
#and should not require modification

#Cruise Code
setMethod(
  f = "getCruiseCode",
  signature = "Mark",
  definition = function(object){
    return(object@cruise_code);
  }
);

# setMethod(
#   f = "setCruiseCode",
#   signature = "Mark",
#   definition = function(object,value){
#     object@cruise_code<-value;
#     return(object);
#   }
# );

#Stratum Code
setMethod(
  f = "getStratumCode",
  signature = "Mark",
  definition = function(object){
    return(object@stratum_code);
  }
);

# setMethod(
#   f = "setStratumCode",
#   signature = "Mark",
#   definition = function(object,value){
#     object@stratum_code<-value;
#     return(object);
#   }
# );

#Transect Code
setMethod(
  f = "getTransectCode",
  signature = "Mark",
  definition = function(object){
    return(object@transect_code);
  }
);

# setMethod(
#   f = "setTransectCode",
#   signature = "Mark",
#   definition = function(object,value){
#     object@transect_code<-value;
#     return(object);
#   }
# );

#NASC
setMethod(
  f = "getNASC",
  signature = "Mark",
  definition = function(object){
    return(object@NASC);
  }
);

# setMethod(
#   f = "setNASC",
#   signature = "Mark",
#   definition = function(object,value){
#     object@NASC <- value;
#     return(object);
#   }
# );

#Haul Code - need to be able to set this through code
setMethod(
  f = "getHaulCode",
  signature = "Mark",
  definition = function(object){
    return(object@haul_code);
  }
);

setMethod(
  f = "setHaulCode",
  signature = "Mark",
  definition = function(object,value){
    object@haul_code<-value;
    return(object);
  }
)


#School Length
setMethod(
  f = "getSchoolLength",
  signature = "Mark",
  definition = function(object){
    return(object@school_length);
  }
);

setMethod(
  f = "setSchoolLength",
  signature = "Mark",
  definition = function(object,value){
    object@school_length<-value;
    return(object);
  }
)

#LF
setMethod(
  f = "getLF",
  signature = "Mark",
  definition = function(object){
    return(object@LF);
  }
);

setMethod(
  f = "setLF",
  signature = "Mark",
  definition = function(object,value){
    object@LF<-value;
    return(object);
  }
)


#Cell Length
setMethod(
  f = "getCellLength",
  signature = "Mark",
  definition = function(object){
    return(object@cell_length);
  }
);

#Cell Lengths
setMethod(
  f = "getCellLengths",
  signature = "Mark",
  definition = function(object){
    return(rep(object@cell_length,length(object@LF[[1]])));
  }
);

#acoustic cross-section
setMethod(
  f = "getCS",
  signature = "Mark",
  definition = function(object){
    return(object@CS);
  }
);


# setMethod(
#   f = "crossSection",
#   signature = "TargetSpecies",
#   definition = function(object,LF){
#
#     #TO DO - sort out the length offset hardcoded below...
#
#     #return the acoustic backscattering cross section
#     #for this species/length
#     TSL <- object@ts_a * log10(as.numeric(names(LF))+0.25) + object@ts_b;
#     CSL <- 10^(TSL/10);
#     CSL <- 4*pi*CSL;
#
#     return(sum(LF*CSL));
#
#   }
#
# );

setMethod(
  f = "setCS",
  signature = "Mark",
  definition = function(object,Species,Target){

    #calculate the cross-section
    #Species is the list of survey species (required for target strength details)

    #cat(object@marktype_name,"\n")

    #get mark type details
    for (i in 1:length(MarkTypes)){
      if (MarkTypes[[i]]@NASC_name == object@marktype_name) {mt<-i}
    }

    #cat("mt index = ",mt,"\n")
    #cat("MarkTypes[[i]]@single_species=",MarkTypes[[i]]@single_species,"\n")

    #if (MarkTypes[[mt]]@single_species) {
    if (any(is.na(MarkTypes[[mt]]@mixed_with))) {

      #cat("single species\n")

      #get target strength parameters
      for (j in 1:length(Species)){
        if (toupper(Species[[j]]@species)==toupper(Target)) {
          ts_a<-Species[[j]]@ts_a
          ts_b<-Species[[j]]@ts_b
        }
      }

      #TO DO - sort out the offset below
      tsl <- ts_a*log10(as.numeric(names(object@LF[[Target]]))+0.25)+ts_b
      csl <- 10^(tsl/10)
      csl <- csl*4*3.141593

      #target proportions
      ptar <- object@LF[[Target]]

      #normalise (should be anyway)
      ptar <- ptar/sum(ptar)

      #overall cross-section
      cs <- sum(ptar*csl)
      object@CS <- cs

    } else {

      #cat("mixture\n")

      #initialise cross-section
      cs<-0

      #loop over species
      for (i in 1:length(object@LF)){

        #species name
        spe.name <- names(object@LF)[i]

        #get target strength parameters
        for (j in 1:length(Species)){
          if (toupper(Species[[j]]@species)==toupper(spe.name)) {
            ts_a<-Species[[j]]@ts_a
            ts_b<-Species[[j]]@ts_b
          }
        }

        #cat("ts_a=",ts_a,"\n")
        #cat("ts_b=",ts_b,"\n")
        #cat(as.numeric(names(object@LF[[spe.name]])),"\n")

        #TO DO - sort out the offset below
        tsl <- ts_a*log10(as.numeric(names(object@LF[[spe.name]]))+0.25)+ts_b
        #cat("tsl=",tsl,"\n")
        csl<-10^(tsl/10)
        csl<-csl*4*3.141593
        #cat("csl=",csl,"\n")

        #target proportions
        ptar<-object@LF[[spe.name]]

        #cat("ptar=",ptar,"\n")
        #cat("sum(ptar)=",sum(ptar),"\n")
        #cat("sum(ptar*csl)=",sum(ptar*csl),"\n")

        #overall cross-section
        cs <- cs + sum(ptar*csl)
        object@CS<-cs

      }
    }

    return(object);
  }
);

#mark type
setMethod(
  f = "getMarkType",
  signature = "Mark",
  definition = function(object){
    return(object@marktype_name);
  }
);

#position
setMethod(
  f = "getPosition",
  signature = "Mark",
  definition = function(object){
    return(object@position);
  }
);


#Processing Functions

setMethod(
  f = "abundance",
  signature = "Mark",
  definition = function(object,target){

    #return the abundance at length for the mark
    #the 1 is for AREA - check this
    #return the abundance in millions

    LF <- object@LF[[target]]
    Area <- 1

    ret <- (object@NASC/object@CS*LF*Area)/1000/1000;
    #ret <- (object@NASC/object@CS*(object@LF/sum(object@LF))*1)/1000/1000;

    #add length offset
    names(ret) = as.character(as.numeric(names(LF)) + 0.25);

    return(ret);

  }
);

setMethod(
  f = "biomass",
  signature = "Mark",
  definition = function(object,LW,target){

    LF<-object@LF[[target]]

    #TO DO - sort out the length offset hardcoded below....
    #return the biomass at length in tonnes for the mark
    W <- LW[[1]]*(as.numeric(names(LF))+0.25)^LW[[2]];
    #return biomass
    return(W*abundance(object,target));
  }
);

#summary method
setMethod(
  f = "summary",
  signature = "Mark",
  definition = function(object){
    cat("************************************\n");
    cat("Cruise:",object@cruise_code,"\n");
    cat("Stratum:",object@stratum_code,"\n");
    cat("Transect:",object@transect_code,"\n");
    cat("Name:",object@marktype_name,"\n");
    cat("NASC:",object@NASC,"\n");
    cat("Length:",object@cell_length,"\n");
    cat("Position:\n");
    cat(summary(object@position),"\n");
    cat("Assigned Haul:",object@haul_code,"\n");
    cat("************************************\n");
  }
);

#assign transect method
setMethod(
  f = "assignTransect",
  signature = "Mark",
  definition = function(object,tran){
    #find the nearest transect
    #calculate the distance of the mark from the endpoints of each transect
    #sum these distances and subtract the transect length
    #the transect with the lowest value will the the closest to the mark

    #temporary data frame
    tdf <- data.frame("Code"=unlist(lapply(tran,getCode)),
                      "LenKm"=unlist(lapply(tran,getLengthKm)),
                      stringsAsFactors=FALSE)
    tdf$DistStart <- fTo_Km(object@position@lat,
                          object@position@lon,
                          unlist(lapply(lapply(tran,getStartPos),getLat)),
                          unlist(lapply(lapply(tran,getStartPos),getLon)))
    tdf$DistEnd <- fTo_Km(object@position@lat,
                        object@position@lon,
                        unlist(lapply(lapply(tran,getEndPos),getLat)),
                        unlist(lapply(lapply(tran,getEndPos),getLon)))
    tdf$Proximity <- tdf$DistStart + tdf$DistEnd - tdf$LenKm

    object@nearest_transect <- tdf$Code[which(tdf$Proximity==min(tdf$Proximity))]

    return(object)

  }
)

setMethod(
  f = "getAbdAtLen",
  signature = "Mark",
  definition = function(object,marktypes){
    #abundance at length for the mark in millions

    if (any(object@LF[[toupper(marktypes)]]>0)) {
      if (object@CS>0){
        ret<-object@LF[[toupper(marktypes)]]*object@NASC/(1e6*object@CS)
        names(ret)<-as.character(as.numeric(names(object@LF[[toupper(marktypes)]]))+0.25)
        return(ret)
      } else {
        return(NULL)
      }
    }
    return(NULL)
  }
)


fTo_Km <- function(Pt1y,Pt1x,Pt2y,Pt2x){

  #Pt=c(-Lat,Long)
  # converts 2 -lat,longs into a km dist.
  #  1.852 km/nmile
  Pt1y<-as.double(Pt1y)
  Pt1x<-as.double(Pt1x)
  Pt2x<-as.double(Pt2x)
  Pt2y<-as.double(Pt2y)
  a1<-111.14-.28*(cos(-2*Pt1y*pi/180)+cos(-2*Pt2y*pi/180))
  a0<-55.71*(cos(-Pt1y*pi/180)+cos(-Pt2y*pi/180))-.25*(cos(3*Pt1y*pi/180)+cos(3*Pt2y*pi/180))
  dist<-(a1*(Pt2y-Pt1y))^2+(a0*(Pt2x-Pt1x))^2
  #browser()
  xxx<-dist>0 & !is.na(dist)
  dist[xxx]<- dist[xxx]^0.5

  #dist[dist>0]<- dist[dist>0]^0.5
  return(dist)
}

