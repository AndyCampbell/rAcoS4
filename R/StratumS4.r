#the Stratum class
#a type, boundary, cruise and stratumcode define a stratum
#additionally an area is calculated on initialization

#' An S4 class to represent an acoustic survey stratum
#'
#' @details
#' StratumS4 class documentation
#'
#'
setClass(
  "Stratum",
  representation(code = "character",
                 cruise_code = "character",
                 type = "character",
                 boundary_lat = "numeric",
                 boundary_lon = "numeric",
                 area = "numeric",
                 ICESarea = "character",
                 mean_abundance = "numeric",
                 mean_biomass = "numeric",
                 abd_at_len = "list",
                 abd_at_age = "list",
                 abd_at_mat = "list",
                 bio_at_len = "list",
                 bio_at_age = "list",
                 bio_at_mat = "list"),
  prototype(code = NA_character_,
            cruise_code = NA_character_,
            type = NA_character_,
            boundary_lat = NA_real_,
            boundary_lon = NA_real_,
            area = NA_real_,
            ICESarea = NA_character_,
            mean_abundance = NA_real_,
            mean_biomass = NA_real_,
            abd_at_len = NULL,
            abd_at_age = NULL,
            abd_at_mat = NULL,
            bio_at_len = NULL,
            bio_at_age = NULL,
            bio_at_mat = NULL),
  validity = function(object){

    #cat("~~~ Stratum:inspector ~~~\n");

    if (length(object@code)==0){
      stop("[Stratum: validation] Stratum code is mandatory");
    }

    if (length(object@cruise_code)==0){
      stop("[Stratum: validation] Cruise code is mandatory");
    }

    if (!(length(object@boundary_lat)==length(object@boundary_lon))) {
      stop("Lengths of boundary latitude and longitude vectors should be equal");
    }

    return(TRUE);

  }

);

#initialize method
setMethod(
  f = "initialize",
  signature = "Stratum",
  definition = function(.Object,code,cruise_code,type,boundary_lat,boundary_lon,
                        area = 0,ICESarea,mean_abundance = 0,mean_biomass = 0,
                        abd_at_len = list(),abd_at_age = list(),abd_at_mat = list(),
                        bio_at_len = list(),bio_at_age = list(),bio_at_mat = list()){

    #cat("~~~ Stratum:initializer ~~~\n");
    .Object@code <- code
    .Object@cruise_code <- cruise_code
    .Object@type <- type
    .Object@boundary_lat <- boundary_lat
    .Object@boundary_lon <- boundary_lon
    #Ian's code for now
    .Object@area <- AreaLongLat(boundary_lon,boundary_lat)/(1.852*1.852)
    .Object@ICESarea <- ICESarea
    .Object@mean_abundance <- mean_abundance
    .Object@mean_biomass <- mean_biomass

    .Object@abd_at_len <- abd_at_len
    .Object@abd_at_age <- abd_at_age
    .Object@abd_at_mat <- abd_at_mat
    .Object@bio_at_len <- bio_at_len
    .Object@bio_at_age <- bio_at_age
    .Object@bio_at_mat <- bio_at_mat

    #call the inspector
    validObject(.Object);
    return(.Object);
  }
);


#accessor methods

#getNorthernLimit (readonly)
setMethod(
  f = "getNorthernLimit",
  signature = "Stratum",
  definition = function(object){
    #cat("~~~ Stratum:getNorthenLimit ~~~\n");
    return(max(object@boundary_lat))
  }
);

#getSouthernLimit (readonly)
setMethod(
  f = "getSouthernLimit",
  signature = "Stratum",
  definition = function(object){
    #cat("~~~ Stratum:getSouthernLimit ~~~\n");
    return(min(object@boundary_lat))
  }
);

#getWesternLimit (readonly)
setMethod(
  f = "getWesternLimit",
  signature = "Stratum",
  definition = function(object){
    #cat("~~~ Stratum:getWesternLimit ~~~\n");
    return(min(object@boundary_lon))
  }
);

#getEasternLimit (readonly)
setMethod(
  f = "getEasternLimit",
  signature = "Stratum",
  definition = function(object){
    #cat("~~~ Stratum:getEasternLimit ~~~\n");
    return(max(object@boundary_lon))
  }
);

#getCode (readonly)
setMethod(
  f = "getCode",
  signature = "Stratum",
  definition = function(object){
    #cat("~~~ Stratum:getCode ~~~\n");
    return(object@code);
  }
);


#getCruiseCode (readonly)
setMethod(
  f = "getCruiseCode",
  signature = "Stratum",
  definition = function(object){
    #cat("~~~ Stratum:getCruiseCode ~~~\n");
    return(object@cruise_code);
  }
);


#summary method
setMethod(
  f = "summary",
  signature = "Stratum",
  definition = function(object,visible=TRUE,report=FALSE){

    #if visible, print the details to the screen,
    #otherwise suppress them. Info is returned invisibly
    #if report is true function returns line for insertion in report table

    #repLine is a line for the table 9 report. It contains the following
    #1 Stratum Name
    #2 Num of transects
    #3 Num of schools
    #4 Number of Definite schools
    #5 Number of Mixed schools
    #6 Number of Probable schools
    #7 Percentage of transects with zero marks
    #8 Definite biomass (in thousands of tonnes)
    #9 Mixed biomass (in thousands of tonnes)
    #10 Probable biomass (in thousands of tonnes)
    #11 Total biomass (in thousands of tonnes)
    #12 Total SSB (in thousands of tonnes)
    #13 Abundance (in millions)

    stratumCode <- object@code
    cruiseCode <- object@cruise_code
    stratumType <- object@type
    ICESarea <- object@ICESarea
    stratumArea <- object@area
    trackLength <- getTrackLength_nm(object)
    contTrans <- names(Transects[lapply(Transects,getStratumCode)==object@code])
    numTrans <- length(contTrans)

    #initialise totals to zero
    marktotals <- 0
    abd_at_age <- 0
    bio_at_age <- 0
    abd_at_mat <- 0
    bio_at_mat <- 0
    bio <- 0
    abd <- 0
    ssb <- 0
    ssb_abd <- 0

    repLine <- stratumCode

    #contains transects
    tr<-Transects[lapply(Transects,getStratumCode)==object@code]

    #number of marks and their types on each transect
    tr.marks <- lapply(tr,getNumMarks)
    #set any possibly mark counts to zero as we don't want to include them here

    for (m in 1:length(tr.marks)){
      if (sum(grepl("Possibly",names(tr.marks[[m]])))>0) {
        full.name <- names(tr.marks[[m]])[grepl("Possibly",names(tr.marks[[m]]))]
        tr.marks[[m]][full.name]<-0
      }
    }

    #find total numbers
    totalMarks <- lapply(tr.marks,sum)


    #append number of transects to report line
    repLine <- paste(repLine,length(tr),sep=",")

    #find total marks by type
    mark.names<-c()
    for (i in 1:length(tr)){mark.names<-unique(c(mark.names,names(tr[[i]]@marks[nchar(names(tr[[i]]@marks))>0])))}

    if (length(mark.names)>0) {

      totals<-bio<-abd<-ssb<-ssb_abd<-vector("integer",length=length(mark.names))
      names(totals)<-names(bio)<-names(abd)<-names(ssb)<-names(ssb_abd)<-mark.names

      #biomass,abundance by mark type
      for (j in mark.names){
        bio[j] <- object@mean_biomass[j]*object@area
        abd[j] <- object@mean_abundance[j]*object@area
        ssb[j] <- sum(object@bio_at_mat[[j]][getMatureCodes(MarkTypes[[j]])])*object@area
        ssb_abd[j] <- sum(object@abd_at_mat[[j]][getMatureCodes(MarkTypes[[j]])])*object@area
      }

      for (i in 1:length(tr)){
        for (j in 1:length(names(totals))){
          if (!is.na(tr[[i]]@marks[names(totals)[j]])) {
            totals[names(totals)[j]]<-totals[names(totals)[j]]+tr[[i]]@marks[names(totals)[j]]
            #bio[names(totals)[j]]<-object@mean_biomass[names(totals)[j]]*object@area
            #abd[names(totals)[j]]<-object@mean_abundance[names(totals)[j]]*object@area
          }
        }
      }

      marktotals<-totals

      #at age/mat details
      #should be ok adding these lists as they will be of the same length (num/bio per age/mat)
      if ((length(object@abd_at_age))>0) {

        abd_at_age <- object@abd_at_age[[1]]
        bio_at_age <- object@bio_at_age[[1]]
        abd_at_mat <- object@abd_at_mat[[1]]
        bio_at_mat <- object@bio_at_mat[[1]]

        if (length(mark.names)>1) {
          for (i in 2:length(mark.names)){
            abd_at_age <- abd_at_age + object@abd_at_age[[i]]
            bio_at_age <- bio_at_age + object@bio_at_age[[i]]
            abd_at_mat <- abd_at_mat + object@abd_at_mat[[i]]
            bio_at_mat <- bio_at_mat + object@bio_at_mat[[i]]
          }
        }

        #multiply by stratum area to get totals
        abd_at_age <- object@area*abd_at_age
        bio_at_age <- object@area*bio_at_age
        abd_at_mat <- object@area*abd_at_mat
        bio_at_mat <- object@area*bio_at_mat

      } else {

        #non aged species
        abd_at_age <- NA
        bio_at_age <- NA
        abd_at_mat <- NA
        bio_at_mat <- NA

      }
    }

    #append total number of schools to report line (less number of possibles)
    repLine <- paste(repLine,sum(marktotals) - sum(marktotals[grepl('Possibly',names(marktotals))]),sep=",")
    #append number of definite schools (must have 'Definitely' in name)
    repLine <- paste(repLine,sum(marktotals[grepl('Definitely',names(marktotals))]),sep=",")
    #append number of mixed schools (must have 'Mix' in name)
    repLine <- paste(repLine,sum(marktotals[grepl('Mix',names(marktotals))]),sep=",")
    #append number of probable schools (must have 'Probably' in name)
    repLine <- paste(repLine,sum(marktotals[grepl('Probably',names(marktotals))]),sep=",")
    #append percentage of zero transects
    repLine <- paste(repLine,sprintf("%.0f",100.0*sum(totalMarks==0)/length(totalMarks)),sep=",")
    #append definite biomass in thousand t
    if (sum(grepl('Definitely',names(bio)))>0) {
      repLine <- paste(repLine,sprintf("%.1f",bio[grepl('Definitely',names(bio))]/1e3),sep=",")
    } else {
      repLine <- paste(repLine,sprintf("%.1f",0),sep=",")
    }
    #append mix biomass
    if (sum(grepl('Mix',names(bio)))>0) {
      repLine <- paste(repLine,sprintf("%.1f",bio[grepl('Mix',names(bio))]/1e3),sep=",")
    } else {
      repLine <- paste(repLine,sprintf("%.1f",0),sep=",")
    }
    #append probably biomass
    if (sum(grepl('Probably',names(bio)))>0) {
      repLine <- paste(repLine,sprintf("%.1f",bio[grepl('Probably',names(bio))]/1e3),sep=",")
    } else {
      repLine <- paste(repLine,sprintf("%.1f",0),sep=",")
    }
    #append total biomass,ssb,abundance
    cond <- grepl('Definitely',names(bio)) | grepl('Mix',names(bio)) | grepl('Probably',names(bio))
    if (sum(cond)>0) {
      repLine <- paste(repLine,sprintf("%.1f",sum(bio[cond])/1e3),sep=",")
      repLine <- paste(repLine,sprintf("%.1f",sum(ssb[cond])/1e3),sep=",")
      repLine <- paste(repLine,sprintf("%.3f",sum(abd[cond])),sep=",")
    } else {
      repLine <- paste(repLine,sprintf("%.1f",0),sep=",")
      repLine <- paste(repLine,sprintf("%.1f",0),sep=",")
      repLine <- paste(repLine,sprintf("%.1f",0),sep=",")
    }


    #print details to console if visible flag is true
    if(visible){

      cat("************************************\n");
      cat("Stratum:",stratumCode,"\n");
      cat("Cruise Code:",cruiseCode,"\n");
      #cat("Stratum Type:",stratumType,"\n");
      cat("Name/ICES Area:",ICESarea,"\n");
      cat("Stratum Area:",stratumArea,"nm^2\n");
      cat("Total track length:",trackLength,"nm\n");
      cat("Contains ",numTrans," transects: (",paste(contTrans,collapse=","),")\n",sep="");
      cat("Marks:\n")

      if (length(mark.names)>0) {
        for (i in 1:length(totals)){cat(totals[i],names(totals)[i],"\n")}
      } else {
        cat("0\n")
      }

      #Total abundance
      if (!is.null(names(object@mean_abundance))) {
        cat("Total Abundance:",sum(abd_at_age),"million\n")
        for (i in 1:length(names(object@mean_abundance))){
          if (nchar(names(object@mean_abundance)[i])>0) {
            cat("\t",object@mean_abundance[names(object@mean_abundance)[i]]*object@area,"(",names(object@mean_abundance)[i],")\n")
          }
        }
      }

      #Total biomass
      if (!is.null(names(object@mean_biomass))) {
        cat("Total Biomass:",sum(bio_at_age)/1000,"Mt\n")
        for (i in 1:length(names(object@mean_biomass))){
          if (nchar(names(object@mean_biomass)[i])>0) {
            cat("\t",object@mean_biomass[names(object@mean_biomass)[i]]*object@area/1000,"(",names(object@mean_biomass)[i],")\n")
          }
        }
      }

      #Total SSB
      cat("************************************\n");

    }

    if(!report){
      #invisibly return details
      invisible(list(stratum = stratumCode,cruisecode = cruiseCode,stratumType = stratumType,
                     stratumArea = stratumArea,ICESarea = ICESarea,trackLength = trackLength,
                     transects = contTrans,marks = marktotals,bio = bio,abd = abd,ssb = ssb,
                     abd_at_age = abd_at_age,bio_at_age = bio_at_age,
                     abd_at_mat = abd_at_mat,bio_at_mat = bio_at_mat,
                     ssb_abd = ssb_abd))
    } else {
      repLine
    }

  }
);


setMethod(
  f = "getTrackLength_nm",
  signature = "Stratum",
  definition = function(object){
    #return the total transect length for this stratum in nm
    return(sum(unlist(lapply(Transects[lapply(Transects,getStratumCode)==object@code],getTrackLength_nm))));
  })


setMethod(
  f = "getMeanAbundance",
  signature = "Stratum",
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
          return(0)
        }
      }
    }
    return(0)

  }
)

setMethod(
  f = "getAbdAtLen",
  signature = "Stratum",
  definition = function(object,marktypes){

    #if no marktype is supplied include all available
    if(missing(marktypes)){marktypes <- names(object@abd_at_len)}

    if (is.null(marktypes)) return(NULL);

    if (length(object@abd_at_len)==0) return(NULL);

    #find first non-null mark type slot
    found <- FALSE
    i<-1

    while (!found & i<=length(marktypes)) {
      if (!is.null(object@abd_at_len[[marktypes[[i]]]])){
        found<-TRUE
      } else {
        i<-i+1
      }
    }

    if (found) {

      ret <- object@abd_at_len[[marktypes[i]]]*object@area

      #add other mark types
      if (length(marktypes)>i){
        for(ii in (i+1):length(marktypes)){

          if (!is.null(object@abd_at_len[[marktypes[ii]]])) {

            #need to be careful if vectors are of different lengths
            #unique names (lengths)
            unames <- sort(unique(c(names(ret),names(object@abd_at_len[[marktypes[ii]]]))))

            newret <- vector("numeric",length(unames))
            newret <- rep(0,length(unames))
            names(newret) <- unames

            for (i in unames){
              if (!is.na(ret[i])) newret[i]<-ret[i]
              if (!is.na(object@abd_at_len[[marktypes[ii]]][i])) newret[i]<-newret[i] + object@abd_at_len[[marktypes[ii]]][i]*object@area
            }

            ret <- newret

          }

        }

      }

      if (any(ret>0)) {
        return(ret);
      }

    }

    return(NULL);


  }
)

setMethod(
  f = "setAbdAtLen",
  signature = "Stratum",
  definition = function(object,name,value){
    object@abd_at_len[[name]]<-value;
    object@mean_abundance[name]<-sum(value)
    return(object);
  }
)

setMethod(
  f = "getAbdAtAge",
  signature = "Stratum",
  definition = function(object,marktypes){

    #if no marktype is supplied include all available
    if(missing(marktypes)){marktypes <- names(object@abd_at_age)}

    if (is.null(marktypes)) return(NULL);

    if (length(object@abd_at_age)==0) return(NULL);

    #find first non-null mark type slot
    found <- FALSE
    i<-1

    while (!found & i<=length(marktypes)) {
      if (!is.null(object@abd_at_age[[marktypes[[i]]]])){
        found<-TRUE
      } else {
        i<-i+1
      }
    }

    if (found) {

      ret <- object@abd_at_age[[marktypes[i]]]*object@area

      #add other mark types
      if (length(marktypes)>i){
        for(ii in (i+1):length(marktypes)){
          if (!is.null(object@abd_at_age[[marktypes[ii]]])) {
            ret <- ret + object@abd_at_age[[marktypes[ii]]]*object@area
          }
        }
      }

      if (any(ret>0)) {
        return(ret);
      }

    }

    return(NULL);
  }
)

setMethod(
  f = "setAbdAtAge",
  signature = "Stratum",
  definition = function(object,name,value){
    object@abd_at_age[[name]]<-value;
    return(object);
  }
)

# setMethod(
#   f = "getAbdAtMat",
#   signature = "Stratum",
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
  f = "getAbdAtMat",
  signature = "Stratum",
  definition = function(object,marktypes,matgroups){

    #return abundance at maturity for selected marktypes
    #if no marktypes provided, do all
    #if no matgroups provided return as is
    #otherwise matgroups is a list of maturities and their associated stages

     #if there are no detections in this strata just return NULL
     if (!(any(unlist(object@abd_at_mat)>0))) {return(NULL)}

     #aggregate the necessary mark types
     #if no marktype is supplied include all available
     if(missing(marktypes)){marktypes <- names(object@abd_at_mat)}

     #find first non-null mark type slot
     found <- FALSE
     i<-1

     while (!found & i<=length(marktypes)) {
      if (!is.null(object@abd_at_mat[[marktypes[[i]]]])){
        found<-TRUE
      } else {
        i<-i+1
      }
    }

    if (found) {

      tmp <- object@abd_at_mat[[marktypes[i]]]*object@area

      #add other mark types
      if (length(marktypes)>i){
        for(ii in (i+1):length(marktypes)){
          if (!is.null(object@abd_at_mat[[marktypes[ii]]])) {
            tmp <- tmp + object@abd_at_mat[[marktypes[ii]]]*object@area
          }
        }
      }

      #now reorganise into maturity groups if they have been provided

      if (missing(matgroups)) {

        return(tmp);

      } else {

        ret <- vector("numeric",length(matgroups))
        names(ret) <- names(matgroups)

        for (l in 1:length(matgroups)){
          ret[[l]]<-sum(tmp[matgroups[[l]]])
        }

        return(ret);

      }
    }

    return(NULL);

  }
)

# setMethod(
#   f = "getAbdAtMatReport",
#   signature = "Stratum",
#   definition = function(object){
#     ret<-c(object@code,object@abd_at_mat);
#     names(ret)<-c("Stratum",names(object@abd_at_mat));
#     return(ret);
#   }
# )

setMethod(
  f = "setAbdAtMat",
  signature = "Stratum",
  definition = function(object,name,value){
    object@abd_at_mat[[name]]<-value;
    return(object);
  }
)


setMethod(
  f = "getMeanBiomass",
  signature = "Stratum",
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
#   signature = "Stratum",
#   definition = function(object,name){
#     if (any(object@bio_at_len[[name]]>0)) {
#       return(object@bio_at_len[[name]]);
#     }
#     return(NULL);
#   }
# )


setMethod(
  f = "getBioAtLen",
  signature = "Stratum",
  definition = function(object,marktypes){

    #if no marktype is supplied include all available
    if(missing(marktypes)){marktypes <- names(object@bio_at_len)}

    if (is.null(marktypes)) return(NULL);

    if (length(object@bio_at_len)==0) return(NULL);

    #find first non-null mark type slot
    found <- FALSE
    i<-1

    while (!found & i<=length(marktypes)) {
      if (!is.null(object@bio_at_len[[marktypes[[i]]]])){
        found<-TRUE
      } else {
        i<-i+1
      }
    }

    if (found) {

      ret <- object@bio_at_len[[marktypes[i]]]*object@area

      #add other mark types
      if (length(marktypes)>i){
        for(ii in (i+1):length(marktypes)){

          if (!is.null(object@bio_at_len[[marktypes[ii]]])) {

            #need to be careful if vectors are of different lengths
            #unique names (lengths)
            unames <- sort(unique(c(names(ret),names(object@bio_at_len[[marktypes[ii]]]))))

            newret <- vector("numeric",length(unames))
            newret <- rep(0,length(unames))
            names(newret) <- unames

            for (i in unames){
              if (!is.na(ret[i])) newret[i]<-ret[i]
              if (!is.na(object@bio_at_len[[marktypes[ii]]][i])) newret[i]<-newret[i] + object@bio_at_len[[marktypes[ii]]][i]*object@area
            }

            ret <- newret

          }

        }

      }

      if (any(ret>0)) {
        return(ret);
      }

    }

    return(NULL);


  }
)






setMethod(
  f = "setBioAtLen",
  signature = "Stratum",
  definition = function(object,name,value){
    object@bio_at_len[[name]]<-value;
    object@mean_biomass[name]<-sum(value);
    return(object);
  }
)

# setMethod(
#   f = "getBioAtAge",
#   signature = "Stratum",
#   definition = function(object){
#     if (any(object@bio_at_age>0)) {
#       return(object@bio_at_age);
#     }
#     return(NULL);
#   }
# )

setMethod(
  f = "getBioAtAge",
  signature = "Stratum",
  definition = function(object,marktypes){

    #if no marktype is supplied include all available
    if(missing(marktypes)){marktypes <- names(object@bio_at_age)}

    if (is.null(marktypes)) return(NULL);

    if (length(object@bio_at_age)==0) return(NULL);

    #find first non-null mark type slot
    found <- FALSE
    i<-1

    while (!found & i<=length(marktypes)) {
      if (!is.null(object@bio_at_age[[marktypes[[i]]]])){
        found<-TRUE
      } else {
        i<-i+1
      }
    }

    if (found) {

      ret <- object@bio_at_age[[marktypes[i]]]*object@area/1e3

      #add other mark types
      if (length(marktypes)>i){
        for(ii in (i+1):length(marktypes)){
          if (!is.null(object@bio_at_age[[marktypes[ii]]])) {
            ret <- ret + object@bio_at_age[[marktypes[ii]]]*object@area/1e3
          }
        }
      }

      if (any(ret>0)) {
        return(ret);
      }

    }

    return(NULL);
  }
)






setMethod(
  f = "setBioAtAge",
  signature = "Stratum",
  definition = function(object,name,value){
    object@bio_at_age[[name]]<-value;
    return(object);
  }
)


# setMethod(
#   f = "getBioAtMat",
#   signature = "Stratum",
#   definition = function(object){
#     if (any(object@bio_at_mat>0)) {
#       return(object@bio_at_mat);
#     }
#     return(NULL);
#   }
# )

setMethod(
  f = "getBioAtMat",
  signature = "Stratum",
  definition = function(object,marktypes,matgroups){

    #return biomass at maturity for selected marktypes
    #if no marktypes provided, do all
    #if no matgroups provided return as is
    #otherwise matgroups is a list of maturities and their associated stages

    #if there are no detections in this strata just return NULL
    if (!(any(unlist(object@bio_at_mat)>0))) return(NULL)

    #aggregate the necessary mark types
    #if no marktype is supplied include all available
    if(missing(marktypes)){marktypes <- names(object@bio_at_mat)}

    #find first non-null mark type slot
    found <- FALSE
    i<-1

    while (!found & i<=length(marktypes)) {
      if (!is.null(object@bio_at_mat[[marktypes[[i]]]])){
        found<-TRUE
      } else {
        i<-i+1
      }
    }

    if (found) {

      tmp <- object@bio_at_mat[[marktypes[i]]]*object@area/1e3

      #add other mark types
      if (length(marktypes)>i){
        for(ii in (i+1):length(marktypes)){
          if (!is.null(object@bio_at_mat[[marktypes[ii]]])) {
            tmp <- tmp + object@bio_at_mat[[marktypes[ii]]]*object@area/1e3
          }
        }
      }

      #now reorganise into maturity groups if they have been provided

      if (missing(matgroups)) {

        return(tmp);

      } else {

        ret <- vector("numeric",length(matgroups))
        names(ret) <- names(matgroups)

        for (l in 1:length(matgroups)){
          ret[[l]]<-sum(tmp[matgroups[[l]]])
        }

        return(ret);

      }
    }

    return(NULL);

  }
)



# setMethod(
#   f = "getBioAtMat",
#   signature = "Stratum",
#   definition = function(object,groups){
#
#     if (any(object@bio_at_mat>0)) {
#
#       #if groups are missing, return all data
#       if(missing(groups)) return(object@bio_at_mat);
#
#       ret<-vector("numeric",length(groups));
#       names(ret) <- names(groups);
#
#       for (i in seq(length(groups))){
#         ret[i] <- 0;
#         if (any(object@bio_at_mat[groups[[i]]]>0)) {
#           ret[i] <- sum(object@bio_at_mat[groups[[i]]])
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
  f = "setBioAtMat",
  signature = "Stratum",
  definition = function(object,name,value){
    object@bio_at_mat[[name]]<-value;
    return(object);
  }
)


# setMethod(
#   f = "abundance",
#   signature = "Stratum",
#   definition = function(object){
#     if (any(object@mean_abundance>0)) {
#       return(object@mean_abundance);
#     }
#     return(NULL);
#   }
# );
#
# setMethod(
#   f = "biomass",
#   signature = "Stratum",
#   definition = function(object,LW){
#
#     #return biomass
#
#     if (any(object@mean_biomass>0)){
#       return(object@mean_biomass);
#     }
#
#     return(NULL);
#
#   }
#
# );

setMethod(
  f = "getArea",
  signature = "Stratum",
  definition = function(object){

    #return stratum area
    return(object@area);

  }
);

setMethod(
  f = "getAgeRange",
  signature = "Stratum",
  definition = function(object){
    if (length(object@abd_at_age)>0) {
      return(range(as.numeric(unlist(lapply(object@abd_at_age,names)))));
    }
    return(NULL);
  }
);

setMethod(
  f = "getICESarea",
  signature = "Stratum",
  definition = function(object){

    #return ICES area
    return(object@ICESarea);

  }
);

#stratum plot method
setMethod(
  f = "plot",
  signature = "Stratum",
  definition = function(x,y,filename,transects="missing",ctds="missing",
                        hauls="missing",sa="missing",...){

    #tran - transects to include on the plot. Only those within
    #the strata are included

    #if a filename has been provided, open the device
    if(!missing(filename)) {
      png(filename=filename,width=960,height=960);
    }

    #boundary limits
    lmt <- list("N" = max(x@boundary_lat),
                "S" = min(x@boundary_lat),
                "E" = max(x@boundary_lon),
                "W" = min(x@boundary_lon));

    #cat("before",lmt$W,lmt$E,"\n")

    #move the western and eastern boundaries out to the next half degree
    if (lmt$W<0) {lmt$W<-floor(lmt$W/0.5)*0.5} else {lmt$W<-ceiling(lmt$W/0.5)*0.5}
    if (lmt$E<0) {lmt$E<-ceiling(lmt$E/0.5)*0.5} else {lmt$E<-floor(lmt$E/0.5)*0.5}

    #ensure plots are at least 1deg wide
    if (abs(abs(lmt$W)-abs(lmt$E))<1) {lmt$E <- lmt$W + 1}

    #move the northern and southern boundaries out to the next half degree
    if (lmt$N<0) {lmt$N<-floor(lmt$N/0.5)*0.5} else {lmt$N<-ceiling(lmt$N/0.5)*0.5}
    if (lmt$S<0) {lmt$S<-ceiling(lmt$S/0.5)*0.5} else {lmt$S<-floor(lmt$S/0.5)*0.5}

    #ensure plots are at least 0.5deg high
    if (abs(abs(lmt$N)-abs(lmt$S))<0.5) {lmt$N <- lmt$S + 1}

    #cat("after",lmt$W,lmt$E,"\n")

    plot(x@boundary_lon,
         x@boundary_lat,
         xlim=c(lmt$W,lmt$E),
         ylim=c(lmt$S,lmt$N),
         xlab="Longitude",
         ylab="Latitude",
         main=filename);

    polygon(x@boundary_lon,x@boundary_lat);

    #coastline
    #find out which coastline segments need to be printed
     toplot <- lapply(coast,function(x)(
       sum(x$Lat>=lmt$S & x$Lat<=lmt$N & x$Lon>=lmt$W & x$Lon<=lmt$E)>0)
     )

     if (sum(unlist(toplot))>0) {
       for (i in which(toplot==TRUE)) {
         if (coast[[i]]$fill==TRUE) {
           polygon(coast[[i]]$Lon,coast[[i]]$Lat,col="green")
         } else {
           polygon(coast[[i]]$Lon,coast[[i]]$Lat,col="white")
         }
       }
     }

    #start and end time limits
    t.earliest <- Sys.time()
    #subtract (approx) 100 years just to be sure
    t.latest <- Sys.time()-3e9

    #transects
    if (!missing(transects)){

      #select transects with the appropriate stratum code
      t<-which(unlist(lapply(transects,getStratumCode))==x@code);

      for (i in seq_along(t)){

        t.earliest <- min(transects[[t[i]]]@start_time,t.earliest)
        t.latest <- max(transects[[t[i]]]@end_time,t.latest)

        lines(
          x=c(transects[[t[i]]]@start_pos@lon,transects[[t[i]]]@end_pos@lon),
          y=c(transects[[t[i]]]@start_pos@lat,transects[[t[i]]]@end_pos@lat),
          lwd=4,
          col="grey")
        text(x=(transects[[t[i]]]@start_pos@lon + transects[[t[i]]]@end_pos@lon)/2,
             y=((transects[[t[i]]]@start_pos@lat + transects[[t[i]]]@end_pos@lat)/2)+0.025,
             labels = transects[[t[i]]]@code)
      }
    }

    #vessel track
    if (exists("Track")){

      #track fragments within plot limits
      toplot <- lapply(Track,function(x)(
        sum(x$Lat>=lmt$S & x$Lat<=lmt$N & x$Lon>=lmt$W & x$Lon<=lmt$E)>0)
      )

      if (sum(unlist(toplot))>0) {
        for (i in which(toplot==TRUE)){

          Track[[i]]$POSIX <- as.POSIXlt(strptime(paste(Track[[i]]$Date,Track[[i]]$Time),format="%d/%m/%Y %H:%M:%S"))

          lines(Track[[i]]$Lon[Track[[i]]$POSIX>t.earliest & Track[[i]]$POSIX<t.latest],
                Track[[i]]$Lat[Track[[i]]$POSIX>t.earliest & Track[[i]]$POSIX<t.latest],col="red");
        }
      }
    }

    #SA
    if (!missing(sa)){
      if (!is.null(sa)) {
        for (i in 1:length(sa)){
          points(sa[[i]]@position@lon,sa[[i]]@position@lat,pch=20,col="black")
        }
      }
    }

    #redo the strata bounds so as not to be hidden by coastline
    polygon(x@boundary_lon,x@boundary_lat,density=0);
    points(x@boundary_lon,x@boundary_lat)

    dev.off();

  }

);

