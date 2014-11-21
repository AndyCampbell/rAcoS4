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
