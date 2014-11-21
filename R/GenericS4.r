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
setGeneric(name="getCruiseCode",def=function(object){standardGeneric("getCruiseCode")})

#getName
#generic for retrieving the code of a cruise/stratum/transect
setGeneric(name="getName",def=function(object,length){standardGeneric("getName")})

#getDesc
#generic for retrieving the description of an object
setGeneric(name="getDesc",def=function(object){standardGeneric("getDesc")})

#getVessel
#generic to return vessel details
setGeneric(name="getVessel",def=function(object){standardGeneric("getVessel")})

#getStartDate, setStartDate, getEndDate, setEndDate
#generic to get/set start and end dates
setGeneric(name="getStartDate",def=function(object){standardGeneric("getStartDate")})
setGeneric(name="setStartDate",def=function(object,value){standardGeneric("setStartDate")})
setGeneric(name="getEndDate",def=function(object){standardGeneric("getEndDate")})
setGeneric(name="setEndDate",def=function(object,value){standardGeneric("setEndDate")})

#getTargetCommon
#get common name for target species
setGeneric(name="getTargetCommon",def=function(object){standardGeneric("getTargetCommon")})

#get Geographical Limit generic functions
setGeneric(name="getNorthernLimit",def=function(object){standardGeneric("getNorthernLimit")})
setGeneric(name="getSouthernLimit",def=function(object){standardGeneric("getSouthernLimit")})
setGeneric(name="getEasternLimit",def=function(object){standardGeneric("getEasternLimit")})
setGeneric(name="getWesternLimit",def=function(object){standardGeneric("getWesternLimit")})






