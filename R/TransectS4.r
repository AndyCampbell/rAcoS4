#the Transect class
#a transect consists of a start and end position and associated
#date/times. They are assigned a code and belong to a stratum

#' An S4 class to represent an acoustic survey transect
#'
#' @details
#' TransectS4 class documentation
#'
#'
setClass(
  "Transect",
  representation(code = "character",
                 stratum_code = "character",
                 cruise_code = "character",
                 start_pos = "GeoPoint",
                 end_pos = "GeoPoint",
                 distance_km = "numeric",
                 distance_nm = "numeric",
                 start_time = "POSIXlt",
                 end_time = "POSIXlt",
                 marks = "numeric",
                 mean_abundance = "numeric",
                 mean_biomass = "numeric",
                 abd_at_len = "list",
                 abd_at_age = "list",
                 abd_at_mat = "list",
                 bio_at_len = "list",
                 bio_at_age = "list",
                 bio_at_mat = "list"),
prototype(code = NA_character_,
          stratum_code = NA_character_,
          cruise_code = NA_character_,
          start_pos = NULL,
          end_pos = NULL,
          distance_km = NA_real_,
          distance_nm = NA_real_,
          start_time = NULL,
          end_time = NULL,
          marks = NA_real_,
          mean_abundance = NA_real_,
          mean_biomass = NA_real_,
          abd_at_len = NULL,
          abd_at_age = NULL,
          abd_at_mat = NULL,
          bio_at_len = NULL,
          bio_at_age = NULL,
          bio_at_mat = NULL),
  validity = function(object){

    #cat("~~~ Transect:inspector ~~~\n");

    if (length(object@code)==0){
      stop("[Transect: validation] Transect code is mandatory");
    }
    if (length(object@stratum_code)==0){
      stop("[Transect: validation] Stratum code is mandatory");
    }
    if (length(object@cruise_code)==0){
      stop("[Haul: validation] Cruise code is mandatory");
    }

    return(TRUE);

  }

);

#initialize method
setMethod(
  f = "initialize",
  signature = "Transect",
  definition = function(.Object,code,stratum_code,cruise_code,start_pos,end_pos,
                        distance_km,distance_nm,start_time,end_time,marks=0,
                        mean_abundance=0,mean_biomass=0,
                        abd_at_len=list(),abd_at_age=list(),abd_at_mat=list(),
                        bio_at_len=list(),bio_at_age=list(),bio_at_mat=list()){

    #cat("~~~ Transect:initializer ~~~\n");
    .Object@code <- code
    .Object@stratum_code <- stratum_code
    .Object@cruise_code <- cruise_code
    .Object@start_pos <- start_pos
    .Object@end_pos <- end_pos
    .Object@start_time <- start_time
    .Object@end_time <- end_time

    #Ian Doonan's code (replace with VincentyEllipsoid call once testing is complete)
    .Object@distance_km <- ToKm(start_pos@lat,start_pos@lon,end_pos@lat,end_pos@lon);
    #in nm
    .Object@distance_nm <- .Object@distance_km/1.852;

    .Object@marks <- marks;
    .Object@mean_abundance <- mean_abundance;
    .Object@mean_biomass <- mean_biomass;

    .Object@abd_at_len <- abd_at_len;
    .Object@abd_at_age <- abd_at_age;
    .Object@abd_at_mat <- abd_at_mat;
    .Object@bio_at_len <- bio_at_len;
    .Object@bio_at_age <- bio_at_age;
    .Object@bio_at_mat <- bio_at_mat;

    #call the inspector
    validObject(.Object);
    return(.Object);
  }
);



#accessor methods

#getCode (readonly)
setMethod(
  f = "getCode",
  signature = "Transect",
  definition = function(object){
    #cat("~~~ Transect:getCode ~~~\n");
    return(object@code);
  }
);

#getLengthKm
setMethod(
  f = "getLengthKm",
  signature = "Transect",
  definition = function(object){
    #cat("~~~ Transect:getLengthKm ~~~\n");
    return(object@distance_km);
  }
);

#getStartPos
setMethod(
  f = "getStartPos",
  signature = "Transect",
  definition = function(object){
    #cat("~~~ Transect:getStartPos ~~~\n");
    return(object@start_pos);
  }
);

#getEndPos
setMethod(
  f = "getEndPos",
  signature = "Transect",
  definition = function(object){
    #cat("~~~ Transect:getEndPos ~~~\n");
    return(object@end_pos);
  }
);

#getStratumCode (readonly)
setMethod(
  f = "getStratumCode",
  signature = "Transect",
  definition = function(object){
    #cat("~~~ Transect:getStratumCode ~~~\n");
    return(object@stratum_code);
  }
);

#getCruiseCode (readonly)
setMethod(
  f = "getCruiseCode",
  signature = "Transect",
  definition = function(object){
    #cat("~~~ Transect:getCruiseCode ~~~\n");
    return(object@cruise_code);
  }
);

setMethod(
  f = "getMeanAbundance",
  signature = "Transect",
  definition = function(object,marktype=NULL){

    if (is.null(marktype)) {
      if (any(object@mean_abundance>0)){
        return(object@mean_abundance[object@mean_abundance>0]);
      }
    } else {
      if (!is.na(object@mean_abundance[marktype])){
        if (object@mean_abundance[marktype]>0){
          return(object@mean_abundance[marktype])
        } else {
          return(0);
        }
      }
    }
    return(0);
  }
)

setMethod(
  f = "getAbdAtLen",
  signature = "Transect",
  definition = function(object,marktypes){

    #if no marktype is supplied include all available
    if(missing(marktypes)){marktypes <- names(object@abd_at_len)}

    if (is.null(marktypes)) return(NULL);

    ret <- object@abd_at_len[[marktypes[1]]]

    if (length(marktypes)>1){
      for(i in 2:length(marktypes)){
        ret <- ret + object@abd_at_len[[marktypes[i]]]
      }
    }

    if (any(ret>0)) {
      return(ret);
    }

    return(NULL);
  }
)

setMethod(
  f = "setAbdAtLen",
  signature = "Transect",
  definition = function(object,name,value){
    object@abd_at_len[[name]]<-value;
    #fill the mean_abundance slot
    object@mean_abundance[name]<-sum(value);
    return(object);
  }
)

setMethod(
  f = "getAbdAtAge",
  signature = "Transect",
  definition = function(object,marktypes){

    #if no marktype is supplied include all available
    if(missing(marktypes)){marktypes <- names(object@abd_at_age)}

    if (is.null(marktypes)) return(NULL);

    ret <- object@abd_at_age[[marktypes[1]]]

    if (length(marktypes)>1){
      for(i in 2:length(marktypes)){
        ret <- ret + object@abd_at_age[[marktypes[i]]]
      }
    }

    if (any(ret>0)) {
      return(ret);
    }

    return(NULL);

  }
)

setMethod(
  f = "getAgeRange",
  signature = "Transect",
  definition = function(object){
    if (length(object@abd_at_age)>0) {
      return(range(as.numeric(unlist(lapply(object@abd_at_age,names)))));
    }
    return(NULL);
  }
)

setMethod(
  f = "setAbdAtAge",
  signature = "Transect",
  definition = function(object,name,value){
    object@abd_at_age[[name]]<-value;
    return(object);
  }
)

# setMethod(
#   f = "getAbdAtMat",
#   signature = "Transect",
#   definition = function(object,groups){
#
#     if (any(object@abd_at_mat>0)) {
#
#       #if groups are missing, return all data
#       if(missing(groups)) return(object@abd_at_mat);
#
#       ret<-vector("numeric",length(groups));
#       names(ret) <- names(groups);
#
#       for (i in seq(length(groups))){
#         ret[i] <- 0;
#         if (any(object@abd_at_mat[groups[[i]]]>0)) {
#           ret[i] <- sum(object@abd_at_mat[groups[[i]]])
#         }
#       }
#
#       return(ret);
#
#     }
#
#     if(missing(groups)) return(0);
#
#     ret<-vector("numeric",length(groups));
#     names(ret) <- names(groups);
#
#     for (i in seq(length(groups))){
#       ret[i] <- 0;
#     }
#
#     return(ret);
#
#   }
# )


setMethod(
  f = "setAbdAtMat",
  signature = "Transect",
  definition = function(object,name,value){
    object@abd_at_mat[[name]]<-value;
    return(object);
  }
)


setMethod(
  f = "getMeanBiomass",
  signature = "Transect",
  definition = function(object,marktype=NULL){

    if (is.null(marktype)) {
      if (any(object@mean_biomass>0)){
        return(object@mean_biomass[object@mean_biomass>0]);
      }
    } else {
      if (!is.na(object@mean_biomass[marktype])){
        if (object@mean_biomass[marktype]>0){
          return(object@mean_biomass[marktype])
        } else {
          return(0);
        }
      }
    }
    return(0);

  }
)

# setMethod(
#   f = "getBioAtLen",
#   signature = "Transect",
#   definition = function(object,name){
#     if (any(object@bio_at_len[[name]]>0)) {
#       return(object@bio_at_len[[name]]);
#     }
#     return(NULL);
#   }
# )

setMethod(
  f = "getBioAtLen",
  signature = "Transect",
  definition = function(object,marktypes){

    #if no marktype is supplied include all available
    if(missing(marktypes)){marktypes <- names(object@bio_at_len)}

    if (is.null(marktypes)) return(NULL);

    ret <- object@bio_at_len[[marktypes[1]]]

    if (length(marktypes)>1){
      for(i in 2:length(marktypes)){
        ret <- ret + object@bio_at_len[[marktypes[i]]]
      }
    }

    if (any(ret>0)) {
      return(ret);
    }

    return(NULL);
  }
)


setMethod(
  f = "setBioAtLen",
  signature = "Transect",
  definition = function(object,name,value){
    object@bio_at_len[[name]]<-value;
    #fill the mean_abundance slot
    object@mean_biomass[name]<-sum(value);
    return(object);
  }
)

setMethod(
  f = "getBioAtAge",
  signature = "Transect",
  definition = function(object,marktypes){
    if (any(object@bio_at_age>0)) {
      return(object@bio_at_age);
    }
    return(NULL);
  }
)

setMethod(
  f = "setBioAtAge",
  signature = "Transect",
  definition = function(object,name,value){
    object@bio_at_age[[name]]<-value;
    return(object);
  }
)


# setMethod(
#   f = "getBioAtMat",
#   signature = "Transect",
#   definition = function(object){
#     if (any(object@bio_at_mat>0)) {
#       return(object@bio_at_mat);
#     }
#     return(NULL);
#   }
# )

setMethod(
  f = "setBioAtMat",
  signature = "Transect",
  definition = function(object,name,value){
    object@bio_at_mat[[name]]<-value;
    return(object);
  }
)



setMethod(
  f = "getNumMarks",
  signature = "Transect",
  definition = function(object){
    return(object@marks[!names(object@marks)==""]);
    #return(object@marks);
  }
)

setMethod(
  f = "setNumMarks",
  signature = "Transect",
  definition = function(object,name,value){
    object@marks[name]<-value;
    return(object);
  }
)


setMethod(
  f = "getTrackLength_nm",
  signature = "Transect",
  definition = function(object){
    return(object@distance_nm);
  }
);

setMethod(
  f = "getCellLengths",
  signature = "Transect",
  definition = function(object,name){
    return(rep(object@distance_nm,length(object@abd_at_len[[name]])));
  }
);

#summary method
setMethod(
  f = "summary",
  signature = "Transect",
  definition = function(object){
    cat("************************************\n");
    cat("Transect:",object@code,"\n");
    cat("Cruise:",object@cruise_code,"\n");
    cat("Stratum:",object@stratum_code," (",getICESarea(Strata[[which(lapply(Strata,getCode)==as.character(object@stratum_code))]]),")\n");
    cat("Transect length:",object@distance_nm,"nm\n");
    cat("Start Position:\n");
    cat(summary(object@start_pos));
    cat("End Position:\n");
    cat(summary(object@end_pos));
    #cat("Contains marktypes:",unlist(lapply(unlist(Transects[lapply(Transects,getStratumCode)==object@code]),getCode)),"\n");
    cat("Contains marktypes:\n")
    if (!is.null(names(object@marks))) {
      for (i in 1:length(names(object@marks))){
        if (nchar(names(object@marks)[i]) > 0) {
          cat("\t",names(object@marks)[i],"(",object@marks[i],")\n")
        }
      }
    } else {
      cat("\tNone\n")
    }

    cat("Mean Abundance:\n")
    if (!is.null(names(object@marks))) {
      for (i in 1:length(names(object@marks))){
        if (nchar(names(object@marks)[i])>0) {
          cat("\t",object@mean_abundance[names(object@marks)[i]],"(",names(object@marks)[i],")\n")
        }
      }
    } else {
      cat("\t0\n")
    }

    cat("Mean Biomass:\n")
    if (!is.null(names(object@marks))) {
      for (i in 1:length(names(object@marks))){
        if (nchar(names(object@marks)[i])>0) {
          cat("\t",object@mean_biomass[names(object@marks)[i]],"(",names(object@marks)[i],")\n")
        }
      }
    } else {
      cat("\t0\n")
    }
    cat("************************************\n");
  }
);

