#the Cruise class
#contains cruise metadata (name, code, start and end dates)
#' An S4 class to represent an acoustic survey cruise
#'
#' @details
#' CruiseS4 class documentation
#'
#'
setClass(
  "Cruise",
  representation(countrycode = "character",
                 code = "character",
                 name = "character",
                 desc = "character",
                 vesselname = "character",
                 callsign = "character",
                 start_date = "POSIXlt",
                 end_date = "POSIXlt",
                 target_common = "character",
                 target_scientific = "character"),

  prototype(countrycode = NA_character_,
            code = NA_character_,
            name = NA_character_,
            desc = NA_character_,
            vesselname = NA_character_,
            callsign = NA_character_,
            start_date = NA,
            end_date = NA,
            target_common = NA_character_,
            target_scientific = NA_character_),

  validity=function(object){

    #cat("~~~ Cruise:inspector ~~~\n");

    errors <- character()

    #country code is mandatory
    if (nchar(object@countrycode)==0){
      msg <- "[Cruise: validation] a country code is mandatory"
      errors <- c(errors,msg)
    }

    #code is mandatory
    if (nchar(object@code)==0){
      msg <- "[Cruise: validation] a cruise code is mandatory"
      errors <- c(errors,msg)
    }

    #cruise name is mandatory
    if (nchar(object@name)==0){
      msg <- "[Cruise: validation] a cruise name is mandatory"
      errors <- c(errors,msg)
    }

    #vesselname is mandatory
    if (nchar(object@vesselname)==0){
      msg <- "[Cruise: validation] a vesselname is mandatory"
      errors <- c(errors,msg)
    }

    #callsign is mandatory
    if (nchar(object@callsign)==0){
      msg <- "[Cruise: validation] a callsign is mandatory"
      errors <- c(errors,msg)
    }

#     if (is.na(object@start_date)){
#       msg <- "[Cruise: validation] a cruise start date is mandatory"
#       errors <- c(errors,msg)
#     }
#     if (nchar(object@end_date)==0){
#       stop("[Cruise: validation] a cruise end date is mandatory");
#     }

    if (nchar(object@target_common)==0){
      msg <- "[Cruise: validation] a cruise target species (common) is mandatory"
      errors <- c(errors,msg)
    }

    if (nchar(object@target_scientific)==0){
      msg <- "[Cruise: validation] a cruise target species (scientific) is mandatory"
      errors <- c(errors,msg)
    }

    if (length(errors) == 0) TRUE else errors

  }
)

#initialize method
setMethod(
  f = "initialize",
  signature = "Cruise",
  definition = function(.Object,countrycode,code,name,desc,vesselname,callsign,
                        start_date,end_date,target_common,target_scientific){
    #cat("~~~ Cruise:initializer ~~~\n");
    .Object@countrycode <- countrycode
    .Object@code <- code
    .Object@name <- name
    .Object@desc <- desc
    .Object@vesselname <- vesselname
    .Object@callsign <- callsign
    .Object@start_date <- start_date
    .Object@end_date <- end_date
    .Object@target_common <- target_common
    .Object@target_scientific <- target_scientific
    #call the inspector
    validObject(.Object);
    return(.Object);
  }
);

#accessor methods
setMethod(
  f = "getCountryCode",
  signature = "Cruise",
  definition = function(object){
    #cat("~~~ Cruise:getCountryCode ~~~\n");
    return(object@countrycode);
  }
);

setMethod(
  f = "getCode",
  signature = "Cruise",
  definition = function(object){
    #cat("~~~ Cruise:getCode ~~~\n");
    return(object@code);
  }
);

# setMethod(
#   f = "getCruiseCode",
#   signature = "Cruise",
#   definition = function(object){
#     #cat("~~~ Cruise:getCruiseCode ~~~\n");
#     return(object@code);
#   }
# );

setMethod(
  f = "getName",
  signature = "Cruise",
  definition = function(object){
    #cat("~~~ Cruise:getName ~~~\n");
    return(object@name);
  }
);

# setMethod(
#   f = "setName",
#   signature = "Cruise",
#   definition = function(object,value){
#     cat("~~~ Cruise:setName ~~~\n");
#     object@name <- value;
#     return(object);
#   }
# );

setMethod(
  f = "getDesc",
  signature = "Cruise",
  definition = function(object){
    #cat("~~~ Cruise:getDesc ~~~\n");
    return(object@desc);
  }
);

# setMethod(
#   f = "setDesc",
#   signature = "Cruise",
#   definition = function(object,value){
#     cat("~~~ Cruise:setDesc ~~~\n");
#     object@desc <- value;
#     return(object);
#   }
# );

setMethod(
  f = "getVesselName",
  signature = "Cruise",
  definition = function(object){
    #cat("~~~ Cruise:getVesselName ~~~\n");
    return(object@vesselname);
  }
);

# setMethod(
#   f = "setVessel",
#   signature = "Cruise",
#   definition = function(object,value){
#     cat("~~~ Cruise:setVessel ~~~\n");
#     object@vessel <- value;
#     return(object);
#   }
# );

setMethod(
  f = "getCallSign",
  signature = "Cruise",
  definition = function(object){
    #cat("~~~ Cruise:getCallSign ~~~\n");
    return(object@callsign);
  }
);

setMethod(
  f = "getStartDate",
  signature = "Cruise",
  definition = function(object){
    #cat("~~~ Cruise:getStartDate ~~~\n");
    return(object@start_date);
  }
);

# setMethod(
#   f = "setStartDate",
#   signature = "Cruise",
#   definition = function(object,value){
#     cat("~~~ Cruise:setCruiseStartDate ~~~\n");
#     object@start_date <- value;
#     return(object);
#   }
# );

setMethod(
  f = "getEndDate",
  signature = "Cruise",
  definition = function(object){
    #cat("~~~ Cruise:getEndDate ~~~\n");
    return(object@end_date);
  }
);

# setMethod(
#   f = "setEndDate",
#   signature = "Cruise",
#   definition = function(object,value){
#     cat("~~~ Cruise:setCruiseEndDate ~~~\n");
#     object@end_date <- value;
#     return(object);
#   }
# );

setMethod(
  f = "getTargetCommon",
  signature = "Cruise",
  definition = function(object){
    #cat("~~~ Cruise:getTargetCommon ~~~\n");
    return(object@target_common);
  }
);

#summary method
# setMethod(
#   f = "summary",
#   signature = "Cruise",
#   definition = function(object){
#     cat("Cruise: ",object@code," (",object@name,"-",object@desc,")",sep="","\n");
#   }
# );

#print method
# setMethod(
#   f = "print",
#   signature = "Cruise",
#   definition = function(object){
#     cat("Cruise print method\n");
#   }
# );

#plot method
setMethod(
  f = "plot",
  signature = "Cruise",
  definition = function(x, y, filename, strata = "missing", transects = "missing",
                        ctds = "missing", hauls = "missing", sa = "missing", track = "missing",
                        srboundaries = FALSE, bathycontours = "missing", sasummary = FALSE,
                        leg.pos = "bottomright", printBio = FALSE, Nlim = "missing",
                        Slim = "missing", Wlim = "missing", Elim = "missing", ...){

    LonLookup <- c("D8"="-11.5", "D9"="-10.5", "E0"="-9.5", "E1"="-8.5", "E2"="-7.5", "E3"="-6.5",
                   "E4"="-5.5", "E5"="-4.5")
    LatLookup <- c("36"="53.75", "37"="54.25", "38"="54.75", "39"="55.25", "40"="55.75", "41"="56.25",
                   "42"="56.75", "43"="57.25", "44"="57.75", "45"="58.25", "46"="58.75", "47"="59.25",
                   "48"="59.75", "49"="60.25")

    #printBio - if set, output the results to each strata

    #if a filename has been provided, open the device
    if(!missing(filename)) {
      #png(filename=filename,width=480,height=600);
      png(filename=filename,width=960,height=1200);
    }

    #boundary limits
    lmt <- list("N" = {if (!missing(Nlim)){Nlim} else {getNorthernLimit(x)}},
                "S" = {if (!missing(Slim)){Slim} else {getSouthernLimit(x)}},
                "E" = {if (!missing(Elim)){Elim} else {getEasternLimit(x)}},
                "W" = {if (!missing(Wlim)){Wlim} else {getWesternLimit(x)}})

    #create an empty plot
    plot(0, 0,
         xlim = c(lmt$W,lmt$E),
         ylim = c(lmt$S,lmt$N),
         type = "n",
         xlab = "Longitude",
         ylab = "Latitude",
         axes = FALSE,
         cex.lab = 1.5)

    if (srboundaries) {
      abline(v=seq(-50,50),lty=1,col='grey')
      abline(h=seq(20,90,by=0.5),lty=1,col='grey')
    }

    if (!missing(bathycontours)){
      #isolate data to plot
      toplot<-lapply(bathymetry,function(x){x$depth%in%bathycontours});

      #add bathymetry
      if (sum(unlist(toplot))>0) {
        for (i in which(toplot==TRUE)){
          lines(bathymetry[[i]]$x,bathymetry[[i]]$y,col="grey");
        }
      }
    }

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

    #strata boundaries if strata object is provided and the biomass results are not to be printed
    if (!missing(strata) & !printBio){
      for (i in 1:length(strata)){polygon(strata[[i]]@boundary_lon,strata[[i]]@boundary_lat)}
    }

    #transects
    if (!missing(transects)){
      for (i in 1:length(transects)){
        lines(
          x=c(transects[[i]]@start_pos@lon,transects[[i]]@end_pos@lon),
          y=c(transects[[i]]@start_pos@lat,transects[[i]]@end_pos@lat),
          lwd=1,
          col="red"
        )
      }
    }

    #vessel track
    if(!missing(track)) {
      toplot <- lapply(track,function(x)(
        sum(x$Lat>=lmt$S & x$Lat<=lmt$N & x$Lon>=lmt$W & x$Lon<=lmt$E)>0)
      )

      if (sum(unlist(toplot))>0) {
        for (i in which(toplot==TRUE)){
          lines(track[[i]]$Lon,track[[i]]$Lat,col="red");
        }
      }
    }

    #CTD Positions
    if (!missing(ctds)){
      if (!is.null(ctds)) {
        for (i in 1:length(ctds)){
          points(ctds[[i]]@lon,ctds[[i]]@lat,pch=20,col="orange",cex=2)
        }
      }
    }

    #Haul Positions
    if (!missing(hauls)){
      if (!is.null(hauls)){
        if (length(hauls)>0) {
          for (i in 1:length(hauls)){
            points(hauls[[i]]@shoot_wp@lon,hauls[[i]]@shoot_wp@lat,pch=20,col="red",cex=2)
            text(hauls[[i]]@shoot_wp@lon,hauls[[i]]@shoot_wp@lat+0.1,hauls[[i]]@code,cex=0.85,col="red")
          }
        }
      }
    }

    #SA registrations
    if (!missing(sa)){

      if (sasummary) {

        #summary plot as used in the report

        #Def & Prob Herring
        sa1=sa[toupper(lapply(sa,getMarkType))==toupper("Def Herring") | toupper(lapply(sa,getMarkType))==toupper("Probably Herring")]

        NASC <- unlist(lapply(sa1,getNASC))
        sa100<-sa1[NASC<=100]
        sa500<-sa1[NASC>100 & NASC<=500]
        sa1000<-sa1[NASC>500 & NASC<=1000]
        sa2000<-sa1[NASC>1000 & NASC<=2000]
        sa9999<-sa1[NASC>2000]

        if (length(sa100)>0) {
          for (i in 1:length(sa100)){points(sa100[[i]]@position@lon,sa100[[i]]@position@lat,pch=20,col="red",cex=1)}
        }
        if (length(sa500)>0) {
          for (i in 1:length(sa500)){points(sa500[[i]]@position@lon,sa500[[i]]@position@lat,pch=20,col="red",cex=2)}
        }
        if (length(sa1000)>0) {
          for (i in 1:length(sa1000)){points(sa1000[[i]]@position@lon,sa1000[[i]]@position@lat,pch=1,col="red",cex=2)}
        }
        if (length(sa2000)>0) {
          for (i in 1:length(sa2000)){points(sa2000[[i]]@position@lon,sa2000[[i]]@position@lat,pch=10,col="red",cex=2)}
        }
        if (length(sa9999)>0){
          for (i in 1:length(sa9999)){points(sa9999[[i]]@position@lon,sa9999[[i]]@position@lat,pch=10,col="red",cex=3)}
        }

        #Herring in a mix
        sa1=sa[toupper(lapply(sa,getMarkType))==toupper("Herring in a mix")]

        NASC <- unlist(lapply(sa1,getNASC))
        sa100<-sa1[NASC<=100]
        sa500<-sa1[NASC>100 & NASC<=500]
        sa1000<-sa1[NASC>500 & NASC<=1000]
        sa2000<-sa1[NASC>1000 & NASC<=2000]
        sa9999<-sa1[NASC>2000]

        if (length(sa100)>0) {
          for (i in 1:length(sa100)){points(sa100[[i]]@position@lon,sa100[[i]]@position@lat,pch=20,col="green",cex=1)}
        }
        if (length(sa500)>0) {
          for (i in 1:length(sa500)){points(sa500[[i]]@position@lon,sa500[[i]]@position@lat,pch=20,col="green",cex=2)}
        }
        if (length(sa1000)>0) {
          for (i in 1:length(sa1000)){points(sa1000[[i]]@position@lon,sa1000[[i]]@position@lat,pch=1,col="green",cex=2)}
        }
        if (length(sa2000)>0) {
          for (i in 1:length(sa2000)){points(sa2000[[i]]@position@lon,sa2000[[i]]@position@lat,pch=10,col="green",cex=2)}
        }
        if (length(sa9999)>0){
          for (i in 1:length(sa9999)){points(sa9999[[i]]@position@lon,sa9999[[i]]@position@lat,pch=10,col="green",cex=3)}
        }

      } else {

        #plot by mark type

          NASC <- unlist(lapply(sa,getNASC))
          sa100<-sa[NASC<=100]
          sa500<-sa[NASC>100 & NASC<=500]
          sa1000<-sa[NASC>500 & NASC<=1000]
          sa2000<-sa[NASC>1000 & NASC<=2000]
          sa9999<-sa[NASC>2000]

          if (length(sa100)>0) {
            for (i in 1:length(sa100)){points(sa100[[i]]@position@lon,sa100[[i]]@position@lat,pch=20,col="red",cex=1)}
          }
          if (length(sa500)>0) {
            for (i in 1:length(sa500)){points(sa500[[i]]@position@lon,sa500[[i]]@position@lat,pch=20,col="red",cex=2)}
          }
          if (length(sa1000)>0) {
            for (i in 1:length(sa1000)){points(sa1000[[i]]@position@lon,sa1000[[i]]@position@lat,pch=1,col="red",cex=2)}
          }
          if (length(sa2000)>0) {
            for (i in 1:length(sa2000)){points(sa2000[[i]]@position@lon,sa2000[[i]]@position@lat,pch=10,col="red",cex=2)}
          }
          if (length(sa9999)>0){
            for (i in 1:length(sa9999)){points(sa9999[[i]]@position@lon,sa9999[[i]]@position@lat,pch=10,col="red",cex=3)}
          }
      }

      legend(leg.pos,
             c("<=100","101-500","501-1000","1001-2000",">2000"),
             pch=c(20,20,1,10,10),
             col="red",
             title="NASC",
             pt.cex=c(1,2,2,2,3),
             cex=1.5,
             bty="n")
    }

    if (srboundaries) {

      #top axis with SR names
      #SR names (D1,D2,D3 etc) are position on the 1/2 degree of longitude
      SR.Lon <- c('D0','D1','D2','D3','D4','D5','D6','D7','D8','D9','E0','E1','E2','E3','E4','E5','E6','E7','E8','E9',
                  'F0','F1','F2','F3','F4','F5','F6','F7','F8','F9','G0','G1','G2','G3','G4','G5','G6','G7','G8','G9','H0','H1','H2','H3','H4',
                  'H5','H6','H7','H8','H9','I0','I1','I2','I3','I4','I5','I6','I7','I8','I9')
      SR.Lon.At <- seq(from=-19.5,by=1,length=length(SR.Lon))
      axis(side=3,labels=SR.Lon,at=SR.Lon.At,tick=FALSE,padj=0,cex.axis=1.5)

      #RHS axis with SR names
      #SR names (09,10,11,12 etc) are positioned on the 1/4 and 3/4 degree of latitude
      SR.Lat <- c('01','02','03','04','05','06','07','08','09',as.character(seq(10,80)))
      SR.Lat.At <- seq(36.25,by=0.5,length=length(SR.Lat))
      axis(side=4,labels=SR.Lat,at=SR.Lat.At,tick=FALSE,hadj=0.5,las=1,cex.axis=1.5)

    }

    #add results
    if (printBio & !missing(strata)){

      t<-lapply(strata,summary,visible=FALSE)

      for (i in 1:length(t)){

        SRy<-substring(t[[i]]$stratum,3,4)
        SRx<-substring(t[[i]]$stratum,5,6)

        Lat <- as.numeric(LatLookup[SRy])
        Lon <- as.numeric(LonLookup[SRx])

        text(Lon,Lat+0.1,paste(round(sum(t[[i]]$ssb)),"kt",sep=""),cex=1.5)
        text(Lon,Lat-0.1,paste(round(sum(t[[i]]$abd),1),"x10^6",sep=""),cex=1.5)
      }

    }


    #LHS axis
    axis(side=2,las=1,cex.axis=1.5)

    #Bottom axis
    axis(side=1,cex.axis=1.5)

    #redraw bounding box
    box();

    dev.off();

  }
);

#getNorthernLimit
setMethod(
  f = "getNorthernLimit",
  signature = "Cruise",
  definition = function(object){
    #cat("~~~ Cruise:getNorthernLimit ~~~\n");
    return(max(unlist(lapply(Strata,getNorthernLimit))))
  }
)

#getSouthernLimit
setMethod(
  f = "getSouthernLimit",
  signature = "Cruise",
  definition = function(object){
    #cat("~~~ Cruise:getSouthernLimit ~~~\n");
    return(min(unlist(lapply(Strata,getSouthernLimit))))
  }
)

#getWesternLimit
setMethod(
  f = "getWesternLimit",
  signature = "Cruise",
  definition = function(object){
    #cat("~~~ Cruise:getWesternLimit ~~~\n");
    return(min(unlist(lapply(Strata,getWesternLimit))))
  }
)

#getEasternLimit
setMethod(
  f = "getEasternLimit",
  signature = "Cruise",
  definition = function(object){
    #cat("~~~ Cruise:getEasternLimit ~~~\n");
    return(max(unlist(lapply(Strata,getEasternLimit))))
  }
)

#summary method
setMethod(
  f = "summary",
  signature = "Cruise",
  definition = function(object,visible=TRUE){

    #if visible, print the details to the screen,
    #otherwise suppress them. Info is returned invisibly

    cruiseCode <- object@code
    cruiseName <- object@name
    cruiseDesc <- object@desc

    #print details to console if visible flag is true
    if(visible){

      cat("************************************\n");
      cat("Cruise Code:",cruiseCode,"\n");
      cat("Cruise Name:",cruiseName,"\n");
      if(nchar(cruiseDesc)>0){cat("Cruise Description:",cruiseDesc,"\n");}
      cat("************************************\n");

    }

    #invisibly return details
    invisible(list(cruisecode = cruiseCode,cruiseName = cruiseName))

  }
);
