#generics for Acoustic Survey Data Processing S4 classes
#Andrew Campbell, Marine Institute, Galway

#getLat,setLat
#generics for retrieving/setting the decimal latitude
setGeneric(name="getLat",def=function(object){standardGeneric("getLat")})
setGeneric(name="setLat",def=function(object,value){standardGeneric("setLat")})

#getLon,setLon
#generics for retrieving/setting the decimal longitude
setGeneric(name="getLon",def=function(object){standardGeneric("getLon")})
setGeneric(name="setLon",def=function(object,value){standardGeneric("setLon")})

#getHeading,setHeading
#generics for retrieving/setting the heading (e.g. of a waypoint)
setGeneric(name="getHeading",def=function(object){standardGeneric("getHeading")})
setGeneric(name="setHeading",def=function(object,value){standardGeneric("setHeading")})

#getDepth,setDepth
#generics for retrieving/setting the depth
setGeneric(name="getDepth",def=function(object){standardGeneric("getDepth")})
setGeneric(name="setDepth",def=function(object,value){standardGeneric("setDepth")})

#getPosition
setGeneric(name="getPosition",def=function(object){standardGeneric("getPosition")})

#getDepthUnit,setDepthUnit
#generics for retrieving/setting the depth unit
setGeneric(name="getDepthUnit",def=function(object){standardGeneric("getDepthUnit")})
setGeneric(name="setDepthUnit",def=function(object,value){standardGeneric("setDepthUnit")})

#getSpeed,setSpeed
#generics for retrieving/setting the speed
setGeneric(name="getSpeed",def=function(object){standardGeneric("getSpeed")})
setGeneric(name="setSpeed",def=function(object,value){standardGeneric("setSpeed")})

#getSpeedUnit,setSpeedUnit
#generics for retrieving/setting the speed unit
setGeneric(name="getSpeedUnit",def=function(object){standardGeneric("getSpeedUnit")})
setGeneric(name="setSpeedUnit",def=function(object,value){standardGeneric("setSpeedUnit")})

#getTime,setTime
#generics for retrieving/setting the time
setGeneric(name="getTime",def=function(object){standardGeneric("getTime")})
setGeneric(name="setTime",def=function(object,value){standardGeneric("setTime")})

#getCode
#generic for retrieving the code of a cruise/stratum/transect
setGeneric(name="getCode",def=function(object){standardGeneric("getCode")})
setGeneric(name="getFAOCode",def=function(object){standardGeneric("getFAOCode")})
setGeneric(name="getCruiseCode",def=function(object){standardGeneric("getCruiseCode")})

#getCountryCode
#generic for retrieving the country code of a cruise/stratum/transect
setGeneric(name="getCountryCode",def=function(object){standardGeneric("getCountryCode")})

#getStratumCode
setGeneric(name="getStratumCode",def=function(object){standardGeneric("getStratumCode")})

#getTransectCode
setGeneric(name="getTransectCode",def=function(object){standardGeneric("getTransectCode")})

#getHaulCode, setHaulCode
setGeneric(name="getHaulCode",def=function(object){standardGeneric("getHaulCode")})
setGeneric(name="setHaulCode",def=function(object,value){standardGeneric("setHaulCode")})

#getName
#generic for retrieving the code of a cruise/stratum/transect
setGeneric(name="getName",def=function(object,length){standardGeneric("getName")})

#getDesc
#generic for retrieving the description of an object
setGeneric(name="getDesc",def=function(object){standardGeneric("getDesc")})

#getVessel
#generic to return vessel details
setGeneric(name="getVesselName",def=function(object){standardGeneric("getVesselName")})
setGeneric(name="getCallSign",def=function(object){standardGeneric("getCallSign")})

#getStartDate, setStartDate, getEndDate, setEndDate, getStartPos, getEndPos
#generic to get/set start and end dates, positions
setGeneric(name="getStartDate",def=function(object){standardGeneric("getStartDate")})
setGeneric(name="setStartDate",def=function(object,value){standardGeneric("setStartDate")})
setGeneric(name="getEndDate",def=function(object){standardGeneric("getEndDate")})
setGeneric(name="setEndDate",def=function(object,value){standardGeneric("setEndDate")})
setGeneric(name="getStartPos",def=function(object){standardGeneric("getStartPos")})
setGeneric(name="getEndPos",def=function(object){standardGeneric("getEndPos")})
setGeneric(name="getDuration",def=function(object){standardGeneric("getDuration")})
setGeneric(name="getWireLength",def=function(object){standardGeneric("getWireLength")})
setGeneric(name="getDepth",def=function(object){standardGeneric("getDepth")})
setGeneric(name="getSpeed",def=function(object){standardGeneric("getSpeed")})

#getTargetCommon
#get common name for target species
setGeneric(name="getTargetCommon",def=function(object){standardGeneric("getTargetCommon")})

#getSpecies
setGeneric(name="getSpecies",def=function(object){standardGeneric("getSpecies")})
setGeneric(name="getLFSpecies",def=function(object){standardGeneric("getLFSpecies")})
setGeneric(name="getBioSpecies",def=function(object){standardGeneric("getBioSpecies")})

#Catch
setGeneric(name="getCatchWeight",def=function(object,species){standardGeneric("getCatchWeight")})
setGeneric(name="getSubSampleWeight",def=function(object,species){standardGeneric("getSubSampleWeight")})

#Maturity Codes
setGeneric(name="getMatureCodes",def=function(object){standardGeneric("getMatureCodes")})
setGeneric(name="getSpentCodes",def=function(object){standardGeneric("getSpentCodes")})
setGeneric(name="getImmatureCodes",def=function(object){standardGeneric("getImmatureCodes")})

#get Geographical Limit generic functions
setGeneric(name="getNorthernLimit",def=function(object){standardGeneric("getNorthernLimit")})
setGeneric(name="getSouthernLimit",def=function(object){standardGeneric("getSouthernLimit")})
setGeneric(name="getEasternLimit",def=function(object){standardGeneric("getEasternLimit")})
setGeneric(name="getWesternLimit",def=function(object){standardGeneric("getWesternLimit")})

#generic to get/set biological data for hauls, targetSpecies, Marks
setGeneric(name="getLFRange",def=function(object,species){standardGeneric("getLFRange")})
setGeneric(name="getLW",def=function(object,species){standardGeneric("getLW")})
setGeneric(name="setLW",def=function(object,value){standardGeneric("setLW")})
setGeneric(name="getLA",def=function(object,species){standardGeneric("getLA")})
setGeneric(name="getLM",def=function(object,species){standardGeneric("getLM")})
setGeneric(name="getLF",def=function(object){standardGeneric("getLF")})
setGeneric(name="setLF",def=function(object,value){standardGeneric("setLF")})
setGeneric(name="setCS",def=function(object,Species,Target){standardGeneric("setCS")})
setGeneric(name="getCS",def=function(object){standardGeneric("getCS")})
setGeneric(name="getMarkType",def=function(object){standardGeneric("getMarkType")})
setGeneric(name="getBio",def=function(object,species){standardGeneric("getBio")})

#getTrackLength_nm
#versions implemented for mark, transect, stratum, cruise
setGeneric(name="getTrackLength_nm",def=function(object){standardGeneric("getTrackLength_nm")})

#getLengthKm
setGeneric(name="getLengthKm",def=function(object){standardGeneric("getLengthKm")})

#getArea
#versions implemented for Stratum
setGeneric(name="getArea",def=function(object){standardGeneric("getArea")})

#getICESarea
#versions implemented for Stratum
setGeneric(name="getICESarea",def=function(object){standardGeneric("getICESarea")})

#abundance
setGeneric(name="getMeanAbundance",def=function(object,marktype=NULL){standardGeneric("getMeanAbundance")})
setGeneric(name="getAbdAtLen",def=function(object,marktypes){standardGeneric("getAbdAtLen")})
setGeneric(name="setAbdAtLen",def=function(object,name,value){standardGeneric("setAbdAtLen")})
setGeneric(name="getAbdAtAge",  def=function(object,marktypes){standardGeneric("getAbdAtAge")})
setGeneric(name="setAbdAtAge",def=function(object,name,value){standardGeneric("setAbdAtAge")})
setGeneric(name="getAbdAtMat",def=function(object,marktypes,matgroups){standardGeneric("getAbdAtMat")})
setGeneric(name="setAbdAtMat",def=function(object,name,value){standardGeneric("setAbdAtMat")})

#biomass
setGeneric(name="getMeanBiomass",def=function(object,marktype=NULL){standardGeneric("getMeanBiomass")})
setGeneric(name="getBioAtLen",def=function(object,marktypes){standardGeneric("getBioAtLen")})
setGeneric(name="setBioAtLen",def=function(object,name,value){standardGeneric("setBioAtLen")})
setGeneric(name="getBioAtAge",def=function(object,marktypes){standardGeneric("getBioAtAge")})
setGeneric(name="setBioAtAge",def=function(object,name,value){standardGeneric("setBioAtAge")})
setGeneric(name="getBioAtMat",def=function(object,marktypes,matgroups){standardGeneric("getBioAtMat")})
setGeneric(name="setBioAtMat",def=function(object,name,value){standardGeneric("setBioAtMat")})

#age range
setGeneric(name="getAgeRange",def=function(object,name){standardGeneric("getAgeRange")})

#getNumMarks, setNumMarks
setGeneric(name="getNumMarks",def=function(object){standardGeneric("getNumMarks")})
setGeneric(name="setNumMarks",def=function(object,name,value){standardGeneric("setNumMarks")})

#getCellLength(s)
setGeneric(name="getCellLengths",def=function(object,name){standardGeneric("getCellLengths")})
setGeneric(name="getCellLength",def=function(object){standardGeneric("getCellLength")})

#getMixedSpecies
setGeneric(name="getMixedSpecies",def=function(object){standardGeneric("getMixedSpecies")})

#getNASCName
setGeneric(name="getNASCName",def=function(object){standardGeneric("getNASCName")})

#getNASC
setGeneric(name="getNASC",def=function(object){standardGeneric("getNASC")})

#getIncludeInEstimates
setGeneric(name="getIncludeInEstimates",def=function(object){standardGeneric("getIncludeInEstimates")})

#target species options (could use same generics as mark types?)
setGeneric(name="getEstAbd",def=function(object){standardGeneric("getEstAbd")})
setGeneric(name="getEstByAge",def=function(object){standardGeneric("getEstByAge")})
setGeneric(name="getEstByMat",def=function(object){standardGeneric("getEstByMat")})

#assignHaul,Transect (merge these two?)
setGeneric(name="assignHaul",def=function(object,pos){standardGeneric("assignHaul")})
setGeneric(name="assignTransect",def=function(object,tran){standardGeneric("assignTransect")})

#abundance/biomass
#calculate abundance/biomass
#versions implemented for Mark,Transect,Stratum,MarkType
setGeneric(name="abundance",def=function(object,target){standardGeneric("abundance")})
setGeneric(name="biomass",def=function(object,LW,target){standardGeneric("biomass")})

#getSchoolLength, setSchoolLength
setGeneric(name="getSchoolLength",def=function(object){standardGeneric("getSchoolLength")})
setGeneric(name="setSchoolLength",def=function(object,value){standardGeneric("setSchoolLength")})

