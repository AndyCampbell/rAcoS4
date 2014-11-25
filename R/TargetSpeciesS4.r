#TargetSpecies class
#class with species specific details
#including target strength, len bin, maturity codes

setClass(
  "TargetSpecies",
  representation(species = "character",
                 common_name = "character",
                 LF_bin_size = "numeric",
                 ts_a = "numeric",
                 ts_b = "numeric",
                 ts_LFint = "numeric",
                 imm_codes = "character",
                 mat_codes = "character",
                 spt_codes = "character",
                 LW = "list",
                 est_abd = "logical",
                 est_by_age = "logical",
                 est_by_mat = "logical"),
  prototype(species = NA_character_,
            common_name = NA_character_,
            LF_bin_size = NA_real_,
            ts_a = NA_real_,
            ts_b = NA_real_,
            ts_LFint = NA_real_,
            imm_codes = NA_character_,
            mat_codes = NA_character_,
            spt_codes = NA_character_,
            LW = list(),
            est_abd = FALSE,
            est_by_age = FALSE,
            est_by_mat = FALSE),
  validity = function(object){
    
    #cat("~~~ TargetSpecies:inspector ~~~\n");
    
    if (length(object@species)==0){
      stop("[TargetSpecies: validation] species is mandatory");
    }
    if (length(object@LF_bin_size)==0){
      stop("[TargetSpecies: validation] LF Bin Size is mandatory");
    }
    
    return(TRUE);
    
  }
  
);

#initialize method
setMethod(
  f = "initialize",
  signature = "TargetSpecies",
  definition = function(.Object,species,common_name,LF_bin_size=1,ts_a,ts_b=0,
                        ts_LFint=0,imm_codes,mat_codes,spt_codes,
                        LW,est_abd=FALSE,est_by_age=FALSE,est_by_mat=FALSE){
    #cat("~~~ TargetSpecies:initializer ~~~\n");
    #cat("imm_codes=",imm_codes,"\n")
    .Object@species <- species
    .Object@common_name <- common_name
    .Object@LF_bin_size <- LF_bin_size
    .Object@ts_a <- ts_a
    .Object@ts_b <- ts_b
    .Object@ts_LFint <- ts_LFint
    .Object@imm_codes <- imm_codes
    .Object@mat_codes <- mat_codes
    .Object@spt_codes <- spt_codes
    .Object@est_abd <- est_abd
    .Object@est_by_age <- est_by_age
    .Object@est_by_mat <- est_by_mat
    
    #call the inspector
    validObject(.Object);
    return(.Object);
  }
);

#summary method
setMethod(
  f = "summary",
  signature = "TargetSpecies",
  definition = function(object,visible=TRUE){
    
    #if visible, print the details to the screen,
    #otherwise suppress them. Info is returned invisibly
    
    speciesName <- object@species
    speciesCommon <- object@common_name
    
    #print details to console if visible flag is true
    if(visible){
      
      cat("************************************\n");
      cat("Species:",speciesName,"\n");
      cat("Common Name:",speciesCommon,"\n");    
      cat("************************************\n");
      
    }
    
    #invisibly return details
    invisible(list(speciesName = speciesName,commonName = speciesCommon))
    
  }
);

setMethod(
  f = "getName",
  signature = "TargetSpecies",
  definition = function(object){
    
    #return the name of the species
    
    return(toupper(object@species));
    
  }
);

setMethod(
  f = "setLW",
  signature = "TargetSpecies",
  definition = function(object,value){
    object@LW<-value;
    return(object);
  }
);

setMethod(
  f = "getLW",
  signature = "TargetSpecies",
  definition = function(object,species="missing"){
    return(object@LW);
  }
);

setMethod(
  f = "getEstAbd",
  signature = "TargetSpecies",
  definition = function(object){
    return(object@est_abd);
  }
);

setMethod(
  f = "getEstByAge",
  signature = "TargetSpecies",
  definition = function(object){
    return(object@est_by_age);
  }
);

setMethod(
  f = "getEstByMat",
  signature = "TargetSpecies",
  definition = function(object){
    return(object@est_by_mat);
  }
);


setMethod(
  f = "getMatureCodes",
  signature = "TargetSpecies",
  definition = function(object){
    return(object@mat_codes);
  }
);

setMethod(
  f = "getSpentCodes",
  signature = "TargetSpecies",
  definition = function(object){
    return(object@spt_codes);
  }
);

setMethod(
  f = "getImmatureCodes",
  signature = "TargetSpecies",
  definition = function(object){
    return(object@imm_codes);
  }
);

setMethod(
  f = "getTargetCommon",
  signature = "TargetSpecies",
  definition = function(object){
    return(object@common_name);
  }
);

setMethod(
  f = "plot",
  signature = "TargetSpecies",
  definition = function(x,y,filename,...){
  
    if (length(x@LW)>0) {
      
      #if a filename has been provided, open the device
      if(!missing(filename)) {
        png(filename=filename,width=480,height=480);
      }
      
      #length-weight plot for species
      plot(x@LW$len,x@LW$wgt,type="p",pch=3,
           xlab="Length (cm)",ylab="Weight (g)",
           main=x@common_name,bty="n")
      mtext(x@species,side=3,line=0)
      
      pl<-seq(from=min(x@LW$len,na.rm=TRUE),to=max(x@LW$len,na.rm=TRUE),length.out=1000)
      lines(pl,x@LW$a*pl^x@LW$b,lwd=1,col="red")
          
      dev.off()
    } 
    
  }
);