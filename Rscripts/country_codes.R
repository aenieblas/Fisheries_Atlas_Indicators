## country_codes.R

## sets up the metadata df for countries

setwd('/home/ae/Documents/PERSONAL/FAO/BlueCloud/Indicators/Fisheries_Atlas_Indicators_Shiny')
dirData<-paste0(getwd(),'/data')


country_codes<-function(path=dirData){
# set up the area/regions using FishStatJ datafiles

# catch gives UN_CODE linked to FAO_AREA_CODE
catch<-read.csv(paste0(path,'/TS_FI_CAPTURE.csv'),stringsAsFactors = FALSE)
cdf<-ddply(catch,.(COUNTRY,FISHING_AREA), function(x) data.frame(q=mean(x$QUANTITY)))
country_df<-unique(cdf[c('COUNTRY','FISHING_AREA')])

# countries links the UN_CODE to the ISO3 and COUNTRY_NAME
countries<-read.csv(paste0(path,'/CL_FI_COUNTRY_GROUPS.csv'),stringsAsFactors = FALSE)

# regions links the FAO_AREA_CODE to the Region Name and Ocean Name
regions<-read.csv(paste0(path,'/CL_FI_WATERAREA_GROUPS.csv'),stringsAsFactors = FALSE)
regions[which(regions$Ocean_Group==''),'Ocean_Group']<-'Inland Waters'

# iso<-read.csv(paste0(path,'iso.csv'),stringsAsFactors = FALSE)

for(i in 1:length(regions$Code)){
  country_df[which(country_df$FISHING_AREA==regions$Code[i]),'OCEAN']<-regions$Ocean_Group[i]
  country_df[which(country_df$FISHING_AREA==regions$Code[i]),'REGION']<-regions$Name_en[i]
}

for(i in 1:length(unique(countries$UN_Code))){
  country_df[which(country_df$COUNTRY==countries$UN_Code[i]),'Country_Name_EN']<-countries$Name_En[i]
  country_df[which(country_df$COUNTRY==countries$UN_Code[i]),'ISO3']<-countries$ISO3_Code[i]
  
}
colnames(country_df)<-c('UN_CODE','FAO_AREA_CODE','OCEAN','REGION','COUNTRY','ISO3')
country_names<-country_df[,c('COUNTRY','ISO3','UN_CODE','FAO_AREA_CODE','REGION','OCEAN')]
write.csv(country_names,file=paste0(dirData,'/country_codes.csv'),row.names = FALSE)
}

# df<-effort

rfmogrid_2_FAOMFA<-function(path=dirData,rfmo,df){
  library(rgdal)
  library(tidyr)
  library(sp)
  ## find overlap between FAO major fishing area shapefiles and rfmo coordinates/grids
  rfmo_shp=paste0(path,'/RFMO_SHP/',rfmo,'/',rfmo,'.shp')
  rshp <- readOGR(dsn = rfmo_shp, stringsAsFactors = F)

  fao_shp<-paste0(path,'/FAO_AREAS/FAO_AREAS.shp')
  fshp <- readOGR(dsn = fao_shp)  
  
  df<-df
  
  # separate grids into the components
  if(rfmo=='IOTC'){
    df2<-separate(df, Grid, c("size", "quadrant", "lat", "lon"), sep = c(1, 2, 4)) 
  }
  
  # if(grepl('bath',data)==FALSE){
    df2$lat<-as.numeric(df2$lat)
    df2$lon<-as.numeric(df2$lon)
    
    df2$lat[df2$quadrant==1] <- df2$lat[df2$quadrant==1]+2.5
    df2$lon[df2$quadrant==1] <- df2$lon[df2$quadrant==1]+2.5
    
    #Q2
    df2$lat[df2$quadrant==2] <- (df2$lat[df2$quadrant==2]+ 2.5)*-1
    df2$lon[df2$quadrant==2] <- df2$lon[df2$quadrant==2]+2.5
  # }  
  
  # convert to data.frame>spatialpointsdataframe
  coordinates(df2)<- c("lon","lat")
  # spdf <- as_Spatial(fshp)
  
  # assign same projection as IOTC shapefile
  proj4string(df2) <- proj4string(fshp)
  

  # find where gridded points lie within the FAO major fishing regions, assigns FAO area, region, and ocean to df
  
  mfa<-subset(fshp, F_LEVEL %in% 'MAJOR')
  
  if(rfmo=='IOTC'){possible_mfa<-c(57,51,58,71,47,81,48)}
  
  ## 
  for(i in 1:length(possible_mfa)){
    this_mfa<-subset(fshp,F_CODE == possible_mfa[i])
    
  inside.grid <- !is.na(over(df2, as(this_mfa, "SpatialPolygons")))
  df2[which(inside.grid==T),c('Region','Ocean','FAO_AREA_CODE')]<-this_mfa@data[,c('NAME_EN','OCEAN','F_CODE')]
  }
  
  return(df2)
    }
  
