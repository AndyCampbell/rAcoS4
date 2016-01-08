#the Haul class
#' An S4 class to represent a haul
#'
#' @details
#' HaulS4 class documentation
#'
#' @include WayPointS4.r
#'
setClass(
  "Haul",
  representation(code = "character",
                 cruise_code = "character",
                 valid = "logical",
                 wirelength = "numeric",
                 towspeed = "numeric",
                 trawldepth = "numeric",
                 shoot_wp = "WayPoint",
                 haul_wp = "WayPoint",
                 species = "list"),
  prototype(code = NA_character_,
            cruise_code = NA_character_,
            valid = NA,
            wirelength = NA_real_,
            towspeed = NA_real_,
            trawldepth = NA_real_,
            shoot_wp = NULL,
            haul_wp = NULL,
            species = NULL),
  validity = function(object){

    #cat("~~~ Haul:inspector ~~~\n");

    if (length(object@code)==0){
      stop("[Haul: validation] Haul code is mandatory");
    }
    if (length(object@cruise_code)==0){
      stop("[Haul: validation] Cruise code is mandatory");
    }

    #for each species, the bio vectors (length, weight, age, sex and maturity)
    #must be of the same length
    if (length(names(object@species)) > 0) {
      for (i in 1:length(names(object@species))) {
        if (length(unique(c(length(object@species[[i]]$length),
                            length(object@species[[i]]$weight),
                            length(object@species[[i]]$age),
                            length(object@species[[i]]$sex),
                            length(object@species[[i]]$maturity))))==1) {
          TRUE;
        } else {
          #report error
          stop(paste("Error with ",names(object@species)[i]," in haul",object@code,sep=""));
        }
      }
    }
    #default return value
    #TRUE;
  }
);

#initialize method
setMethod(
  f = "initialize",
  signature = "Haul",
  definition = function(.Object,code,cruise_code,valid,wirelength,towspeed,trawldepth,shoot_wp,haul_wp,species){
    #cat("~~~ Haul:initializer ~~~\n");
    .Object@code <- code
    .Object@cruise_code <- cruise_code
    .Object@valid <- valid
    .Object@wirelength <- wirelength
    .Object@towspeed <- towspeed
    .Object@trawldepth <- trawldepth
    .Object@shoot_wp <- shoot_wp
    .Object@haul_wp <- haul_wp
    .Object@species <- species
    #call the inspector
    validObject(.Object);
    return(.Object);
  }
);

#accessor methods
#Haul code (readonly)
setMethod(
  f = "getCode",
  signature = "Haul",
  definition = function(object){
    #cat("~~~ Haul:getCode ~~~\n");
    return(object@code);
  }
);

#Cruise Code (readonly)
setMethod(
  f = "getCruiseCode",
  signature = "Haul",
  definition = function(object){
    #cat("~~~ Haul:getCruiseCode ~~~\n");
    return(object@cruise_code);
  }
);

#Species list (all species in haul)
setMethod(
  f = "getSpecies",
  signature = "Haul",
  definition = function(object){
    return(object@species)
  }
)


#CatchWeight
setMethod(
  f = "getCatchWeight",
  signature = "Haul",
  definition = function(object,species){
    return(object@species[[toupper(species)]][['tot.wgt']])
  }
)

#SubSampleWeight
setMethod(
  f = "getSubSampleWeight",
  signature = "Haul",
  definition = function(object,species){
    return(object@species[[toupper(species)]][['sub.samp.wgt']])
  }
)


#Species list (species with LF data)
setMethod(
  f = "getLFSpecies",
  signature = "Haul",
  definition = function(object){
    return(object@species[which(lapply(lapply(object@species,"[[","num.at.len"),length)>0)])
  }
)

#Species list (species with biological data)
setMethod(
  f = "getBioSpecies",
  signature = "Haul",
  definition = function(object){
    return(object@species[which(lapply(lapply(object@species,"[[","length"),length)>0)])
  }
)

#Haul duration (in minutes)
setMethod(
  f = "getDuration",
  signature = "Haul",
  definition = function(object){
    return(as.numeric(difftime(object@haul_wp@time,object@shoot_wp@time),units="mins"))
  }
)

#Haul warp length
setMethod(
  f = "getWireLength",
  signature = "Haul",
  definition = function(object){
    return(object@wirelength)
  }
)

#Mean haul depth
setMethod(
  f = "getDepth",
  signature = "Haul",
  definition = function(object){
    return(object@trawldepth)
  }
)

#Mean haul tow speed
setMethod(
  f = "getSpeed",
  signature = "Haul",
  definition = function(object){
    return(object@towspeed)
  }
)

setMethod(
  f = "getLFRange",
  signature = "Haul",
  definition = function(object,species){
    if (length(object@species) > 0) {
      spe <- toupper(species);
      if (match(spe,toupper(names(object@species)),0)>0) {
        if (length(object@species[[spe]]$len.class)>0) {
          return(range(object@species[[spe]]$len.class));
        }
      }
    }
  }
);

setMethod(
  f = "getLW",
  signature = "Haul",
  definition = function(object,species){
    #species in Haul is in upper case
    spe <- toupper(species);
    return(list(len=object@species[[spe]][['length']],
                wgt=object@species[[spe]][['weight']]));
  }
);

setMethod(
  f = "getBio",
  signature = "Haul",
  definition = function(object,species){
    #species in Haul is in upper case
    spe <- toupper(species);
    return(list(len=object@species[[spe]][['length']],
                wgt=object@species[[spe]][['weight']],
                age=object@species[[spe]][['age']],
                sex=object@species[[spe]][['sex']],
                maturity=object@species[[spe]][['maturity']],
                index=object@species[[spe]][['index']]));
  }
);

setMethod(
  f = "getLA",
  signature = "Haul",
  definition = function(object,species){
    if (length(object@species) > 0) {
      #species in Haul is in upper case
      spe <- toupper(species);
      if (match(spe,names(object@species),0)>0) {
        #do not return NAs
        return(list(len=object@species[[spe]][['length']][!(is.na(object@species[[spe]][['length']]) | is.na(object@species[[spe]][['age']]))],
                    age=object@species[[spe]][['age']][!(is.na(object@species[[spe]][['length']]) | is.na(object@species[[spe]][['age']]))]));
      }
    }
    #default return
    return(list(len=NULL,age=NULL));
  }
);

setMethod(
  f = "getLM",
  signature = "Haul",
  definition = function(object,species){
    if (length(object@species) > 0) {
      #species in Haul is in upper case
      spe <- toupper(species);
      if (match(spe,names(object@species),0)>0) {
        #do not return NAs
        return(list(len=object@species[[spe]][['length']][!(is.na(object@species[[spe]][['length']]) | is.na(object@species[[spe]][['maturity']]))],
                    mat=object@species[[spe]][['maturity']][!(is.na(object@species[[spe]][['length']]) | is.na(object@species[[spe]][['maturity']]))]));
      }
    }
    #default return
    return(list(len=NULL,mat=NULL));
  }
);

isValid <- function(haul.code) {

  #return a logical and message indicating if the haul is valid or not

  for (i in 1:length(Hauls)){
    if (getCode(Hauls[[i]]) == haul.code) {
      if (Hauls[[i]]@valid) {
        return(c(TRUE,""))
      } else
        return(c(FALSE,paste0("Cannot assign haul ",haul.code," as it is invalid")))
    }
  }

  #if here then didn't find haul
  return(c(FALSE,paste0("haul ",haul.code," not available, please check haul assignments and available hauls")))

}

shootPos <- function(haul.code){
  #return the shoot position of the haul with the supplied code
  for (i in 1:length(Hauls)){
    if (getCode(Hauls[[i]]) == haul.code) {
      return(c(Hauls[[i]]@shoot_wp@lat,Hauls[[i]]@shoot_wp@lon));
    }
  }
}

getEndTime <- function(haul.code){
  #return haul time for identified haul
  for (i in 1:length(Hauls)){
    if (getCode(Hauls[[i]]) == haul.code) {
      return(c(Hauls[[i]]@haul_wp@time));
    }
  }
}


getStartTime <- function(haul.code){
  #return shoot time for identified haul
  for (i in 1:length(Hauls)){
    if (getCode(Hauls[[i]]) == haul.code) {
      return(c(Hauls[[i]]@shoot_wp@time));
    }
  }
}

getHaulLF <- function(haul.code,species,raised=FALSE){

  num <- NA

  #return the LF for the haul/species specified
  for (i in 1:length(Hauls)){

    if (getCode(Hauls[[i]]) == haul.code) {

      if (length(Hauls[[i]]@species[[species]]$num.at.len)>0) {

        num <- Hauls[[i]]@species[[species]]$num.at.len

        if (raised) {
          num <- num*Hauls[[i]]@species[[species]]$tot.wgt/Hauls[[i]]@species[[species]]$sub.samp.wgt
        }

        names(num) <- Hauls[[i]]@species[[species]]$len.class
      }

      return(num)

    }

  }

  return(num)

}

#getLFProp <- function(haul.code,species,all.species=FALSE){
getLFProp <- function(haul.code,species,mix.species=c()){
  #return the LF proportions for the haul/species specified
  #if all.species is TRUE then takes any other species into consideration
  #(as in with mixed hauls), otherwise only the species identified is used

  code <- as.character(haul.code)

  #species list
  spe<-c(species,mix.species)
  spe<-toupper(spe)
  #consider only those present in the haul
  spe <- spe[spe %in% names(Hauls[[code]]@species)]
  #spe <- names(Hauls[[code]]@species)

  #for (i in 1:length(spe)){cat("spe[",i,"]=",spe[i],"\n")}

  if (length(spe)==1) {
    LFs <- list(getHaulLF(haul.code=code,spe[1],raised=TRUE))
    names(LFs) <- spe
  } else {
    #get the LFs for each species
    #LFs <- sapply(spe,getHaulLF,haul.code=code,raised=TRUE)
    LFs <- lapply(as.list(spe),getHaulLF,haul.code=code,raised=TRUE)
    names(LFs) <- spe
  }

  if (!is.null(unlist(lapply(LFs,names)))) {

    #len.range <- range(as.numeric(names(LFs)))
    len.range <- range(as.numeric(unlist(lapply(LFs,names))))

    #generate a sequence of lengths
    #use the half cm for now
    #TO DO - code for varying length intervals?
    #or will 0.5 do for all
    l<-seq(len.range[1],len.range[2],0.5)

    #create return list
    ret<-vector("list",length(spe))
    names(ret)<-c(spe)

  #   if (length(spe)==1) {
  #     ret[[1]] <- LFs
  #   } else {
      for (i in 1:length(LFs)){
        padded<-rep(0,length=length(l))
        names(padded)=l
        for(j in 1:length(l)){
          if(l[j]%in%names(LFs[[i]])){
            padded[j]<-LFs[[i]][[as.character(l[j])]]
          }
        }
        ret[[i]]<-padded
  #    }
    }

    #if all species then make sure all LFs sum to
    #unity and not individually
    if(length(spe)>1){
    #if(all.species){
      #collectively normalised
      tot<-sum(unlist(lapply(ret,sum)))
      for (j in 1:length(LFs)){
        ret[[j]]<-ret[[j]]/tot
      }
    } else {
      #individually normalised
      for (j in 1:length(LFs)){
        ret[[j]]<-ret[[j]]/sum(ret[[j]])
      }
    }

    return(ret)

  }

  return(NULL)

#   #get the required species LF
#   LF <- getHaulLF(haul.code,species);
#
#   if (all.species) {
#
#     for (i in 1:length(Hauls)){
#
#       if (getCode(Hauls[[i]]) == haul.code) {
#
#         spe <- names(Hauls[[i]]@species);
#
#         #get the LF range
#         minL<-999
#         maxL<-0
#
#         for (j in 1:length(spe)){
#           minL<-min(minL,min(Hauls[[i]]@species[[spe[j]]]$len.class))
#           maxL<-max(maxL,max(Hauls[[i]]@species[[spe[j]]]$len.class))
#         }
#
#         lgth<-seq(minL,maxL,by=0.5)
#
#         #create a matrix to hold the LFs
#         tmat<-matrix(0,nrow=length(lgth),ncol=length(spe),
#                      dimnames=list(as.character(lgth),spe))
#
#         for (j in 1:length(spe)){
#
#           #length class
#           lc<-Hauls[[i]]@species[[spe[j]]]$len.class
#           #raised number
#           num<-Hauls[[i]]@species[[spe[j]]]$num.at.len
#           num<-num*Hauls[[i]]@species[[spe[j]]]$tot.wgt/Hauls[[i]]@species[[spe[j]]]$sub.samp.wgt
#
#           for (k in 1:length(lc)){
#             #cat(j,k,lc[k],"\n")
#             row.idx<-which(rownames(tmat)==as.character(lc[k]))
#             #cat(row.idx,"\n")
#             tmat[row.idx,j]<-num[k]
#           }
#
#         }
#
#         #cat(tmat/sum(tmat),"\n")
#
#         return(tmat[,which(colnames(tmat)==species)]/sum(tmat))
#
#       }
#     }
#   }
#
#   return (LF/sum(LF));

}

#summary method
setMethod(
  f = "summary",
  signature = "Haul",
  definition = function(object,visible=TRUE,report=FALSE){

    #if visible, print the details to the screen,
    #otherwise suppress them. Info is returned invisibly
    #if report is true function returns line for insertion in report table

    #report line is a comma separated string with
    #haul number
    #haul date
    #haul time
    #haul latitude
    #haul longitude
    #target depth (to do)
    #bottom depth (to do)
    #bulk catch
    #sampled catch

    #haul num
    repLine = object@code
    #haul date
    repLine <- paste(repLine,strftime(object@haul_wp@time,"%d/%m/%Y"),sep=",")
    #haul time
    repLine <- paste(repLine,strftime(object@haul_wp@time,"%H:%M"),sep=",")
    #haul latitude
    repLine <- paste(repLine,sprintf("%.1f",object@haul_wp@lat),sep=",")
    #haul longitude
    repLine <- paste(repLine,sprintf("%.1f",object@haul_wp@lon),sep=",")
    #target depth
    repLine <- paste(repLine,"NA",sep=",")
    #bottom depth
    repLine <- paste(repLine,"NA",sep=",")
    #bulk catch
    bulkCatch <- sum(lapply(Samples,"[",Samples$HaulNo==object@code)$TotalWeight,na.rm=TRUE)
    repLine <- paste(repLine,sprintf("%.1f",bulkCatch),sep=",")
    #sample catch
    sampledCatch <- sum(lapply(Samples,"[",Samples$HaulNo==object@code)$SampleWeight,na.rm=TRUE)
    repLine <- paste(repLine,sprintf("%.1f",sampledCatch),sep=",")

    #find top 4 species
    top4 <- sort(unlist(lapply(lapply(Species,getName),function(x){sum(lapply(Samples,"[",Samples$SpeciesName==x)$TotalWeight)})),decreasing=TRUE)
    if (length(top4)>4){
      t4Names <- names(top4)[1:4]
    } else {
      t4Names <- names(top4)
    }

    haulSamples <- sum(lapply(Samples,"[",Samples$HaulNo==object@code)$TotalWeight,na.rm=T)

    if (haulSamples==0){

      repLine = paste(repLine,"0.0,0.0,0.0,0.0,0.0",sep=",")

    } else {

      #percentage topmost
      #species samples
      speSamples<-lapply(Samples,"[",Samples$SpeciesName==toupper(Species[[t4Names[1]]]@species))
      pctT1 <- 100.0*(sum(lapply(speSamples,"[",speSamples$HaulNo==object@code)$TotalWeight,na.rm=T)/haulSamples)
      repLine <- paste(repLine,sprintf("%.1f",pctT1),sep=",")

      #percentage next
      speSamples<-lapply(Samples,"[",Samples$SpeciesName==toupper(Species[[t4Names[2]]]@species))
      pctT2 <- 100.0*(sum(lapply(speSamples,"[",speSamples$HaulNo==object@code)$TotalWeight,na.rm=T)/haulSamples)
      repLine <- paste(repLine,sprintf("%.1f",pctT2),sep=",")

      #percentage next
      speSamples<-lapply(Samples,"[",Samples$SpeciesName==toupper(Species[[t4Names[3]]]@species))
      pctT3 <- 100.0*(sum(lapply(speSamples,"[",speSamples$HaulNo==object@code)$TotalWeight,na.rm=T)/haulSamples)
      repLine <- paste(repLine,sprintf("%.1f",pctT3),sep=",")

      #percentage next
      speSamples<-lapply(Samples,"[",Samples$SpeciesName==toupper(Species[[t4Names[4]]]@species))
      pctT4 <- 100.0*(sum(lapply(speSamples,"[",speSamples$HaulNo==object@code)$TotalWeight,na.rm=T)/haulSamples)
      repLine <- paste(repLine,sprintf("%.1f",pctT4),sep=",")

      #remainder
      repLine <- paste(repLine,sprintf("%.1f",100.0-pctT1-pctT2-pctT3-pctT4),sep=",")

    }

    if (report){

      if (visible){
        cat(repLine,"\n")
      }

      return(repLine)

    } else if (visible) {
      cat("************************************\n");
      cat("Haul code:",object@code,"\n");
      cat("Cruise:",object@cruise_code,"\n");
      cat("Shoot:\n");
      cat(summary(object@shoot_wp));
      cat("Haul:\n");
      cat(summary(object@haul_wp));
      cat(names(object@species),"\n");
      cat("************************************\n");
      invisible(list(number = object@code,date = 0,time = 0,
                     lat = 0,lon = 0,tgtDepth = 0,
                     depth = 0,bulk = 0,sampled = 0))
    } else {

      #invisibly return details
      invisible(list(number = object@code, date = 0, time = 0,
                     lat = 0, lon = 0, tgtDepth = 0,
                     depth = 0, bulk = 0, sampled = 0))
    }

  }

);

