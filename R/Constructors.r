#Constructor functions,
#wrap the call to "new"
#function name matches the class name (lowercase)

#constructor for class GeoPoint
geopoint <- function(lat=NA_real_,lon=NA_real_){
  #cat("~~~ geopoint:constructor ~~~\n");
  new(Class="GeoPoint",lat=lat,lon=lon)
}

#constructor for class WayPoint
waypoint <- function(time=NA,lat=NA_real_,lon=NA_real_,heading=NA_real_,
                     depth=NA_real_,speed=NA_real_,depth_unit="m",speed_unit="knots"){
  #cat("~~~ waypoint:constructor ~~~\n");
  new(Class="WayPoint",
      time=(if (is.na(time)) {strptime("1jan1900 00:00:00","%d%b%Y %H:%M:%S")} else {time}),
      lat=lat,lon=lon,
      heading=heading,depth=depth,speed=speed,
      depth_unit=depth_unit,speed_unit=speed_unit)
}

cruise <- function(code,name=NA_character_,desc=NA_character_,
                   vessel=NA_character_,start_date=NA,end_date=NA,
                   target_common=NA_character_,target_scientific=NA_character_){

  #cat("~~~ cruise:constructor ~~~\n");
  new(Class="Cruise",code=code,name=name,desc=desc,vessel=vessel,
      start_date=start_date,end_date=end_date,target_common=target_common,
      target_scientific=target_scientific)
}

ctdstation <- function(lat,lon,code,cruise_code,
                       time=as.POSIXlt(strptime("0000-00-00 00:00:00","%Y-%m-%d %H:%M:%S")),
                       depth=0,depth_unit="m"){
  #cat("~~~ CTDStation:constructor ~~~\n");
  new(Class="CTDStation",lat=lat,lon=lon,code=code,cruise_code=cruise_code,
      time=time,depth=depth,depth_unit=depth_unit)
}

haul <- function(code,cruise_code,valid=TRUE,shoot_wp=NULL,haul_wp=NULL,species=NULL){
  #cat("~~~ Haul:constructor ~~~\n");
  new(Class="Haul",code=code,cruise_code=cruise_code,valid=valid,
      shoot_wp=shoot_wp,haul_wp=haul_wp,species=species)
}

transect <- function(code,stratum_code,cruise_code,start_pos,end_pos,
                     start_time,end_time){
  #cat("~~~ Transect:constructor ~~~\n");
  new(Class="Transect",code = code,stratum_code = stratum_code,cruise_code = cruise_code,
      start_pos = start_pos,end_pos = end_pos,start_time = start_time,end_time = end_time)
}

stratum <- function(code,cruise_code,type,boundary_lat,boundary_lon,ICESarea){
  #cat("~~~ Stratum:constructor ~~~\n");
  new(Class="Stratum",code = code,cruise_code = cruise_code,type = type,
      boundary_lat = boundary_lat,boundary_lon = boundary_lon,
      ICESarea = ICESarea)
}

marktype <- function(name,cruise_code,NASC_name,species,include,haul_assignment,
                     mixed_with=NA_character_,hauls){
  #cat("~~~ MarkType:constructor ~~~\n");
  new(Class = "MarkType",name = name,cruise_code = cruise_code,NASC_name = NASC_name,
      species = species,include = include,haul_assignment = haul_assignment,
      mixed_with = mixed_with,hauls = hauls)
}
