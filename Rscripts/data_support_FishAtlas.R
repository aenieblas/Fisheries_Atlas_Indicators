# data_support_FishAtlas.R
# data_sets preparation fisheries_atlas
# datasets will require updating (current versions updated as of 9/20).
# datasets : FishStatJ (v 2019)
#            TunaAtlas (v 2018)
#            FIRMS/GRSF (v 2020)
#            other RFB/RFMO (v 2020)

fishstatj<-function(path=dirData){
  # FishStatJ
  # objective : data frame of catch, cols including : Ocean	Region	FAO_Area	Stock_species	Family	Order		Year	Month Units time_step	datasource		plot_type  	*management_authority*
  # path<-'/home/ae/Documents/PERSONAL/FAO_SDG1441/FAOFishStats/'
  # planned updates: FISHSTATJ
  
  # catch<-catch[-which(catch$UNIT!='t')] # only consider catch in tonnes# catch<-catch[-which(catch$UNIT!='t')] # only consider catch in tonnes
  regions<-read.csv(paste0(path,'CL_FI_WATERAREA_GROUPS.csv'),stringsAsFactors = FALSE) # add this data to the repos
  species_groups<-read.csv(paste0(path,'CL_FI_SPECIES_GROUPS.csv'),stringsAsFactors = FALSE) # add this data to the repos
  catch<-read.csv(paste0(path,'TS_FI_CAPTURE.csv'),stringsAsFactors = FALSE)
  
  
  # catch<-catch[-which(catch$UNIT!='t')] # only consider catch in tonnes
  
  for(i in 1:length(regions$Code)){
    catch[which(catch$FISHING_AREA==regions$Code[i]),'OCEAN']<-regions$Ocean_Group[i]
    catch[which(catch$FISHING_AREA==regions$Code[i]),'REGION']<-regions$Name_en[i]
  }
  
  # for(i in 1:length(unique(catch$SPECIES))){
  #   catch[which(catch$SPECIES==unique(catch$SPECIES)[i]),'ORDER']<-species_groups[which(species_groups$X3Alpha_Code==unique(catch$SPECIES)[i]),'Order']
  #   catch[which(catch$SPECIES==unique(catch$SPECIES)[i]),'FAMILY']<-species_groups[which(species_groups$X3Alpha_Code==unique(catch$SPECIES)[i]),'Family']
  #   
  #   catch[which(catch$SPECIES==unique(catch$SPECIES)[i]),'MAJOR_GROUP']<-species_groups[which(species_groups$X3Alpha_Code==unique(catch$SPECIES)[i]),'Major_Group']
  #   
  # }
  # # colnames(catch$FISHING_AREA)<-'FAO_AREA'
  # catch$mngnt_body<-NA
  # catch$MONTH<-NA
  # catch$plot_type<-'timeseries'
  # catch$data_source<-'FishStatJ2019'
  # catch$time_step<-'annual'
  
  
  CATCH_FISHSTATJ<-catch[,c('OCEAN','REGION','FISHING_AREA','SPECIES','YEAR','UNIT','QUANTITY')]
  colnames(CATCH_FISHSTATJ)<-c('OCEAN','REGION','FAO_AREA','SPECIES','YEAR','UNIT','QUANTITY')
  if(length(grep(' ',CATCH_FISHSTATJ$OCEAN)>0)){CATCH_FISHSTATJ$OCEAN<-gsub(' ','',CATCH_FISHSTATJ$OCEAN)}
  # fish_atlas_stats<-unique(CATCH_FISHSTATJ[,c('OCEAN','REGION','MAJOR_GROUP','ORDER','FAMILY','SPECIES')])
  
  write.csv(CATCH_FISHSTATJ,file=paste0(dirData,'/CATCH_FISHSTATJ.csv'),row.names = FALSE)
  # write.csv(fish_atlas_stats,file=paste0(dirData,'/fish_atlas_stats.csv'),row.names = FALSE)
  return(CATCH_FISHSTATJ)
}


fishstatj_old<-function(path=dirData){
  # FishStatJ
  # objective : data frame of catch, cols including : Ocean	Region	FAO_Area	Stock_species	Family	Order		Year	Month Units time_step	datasource		plot_type  	*management_authority*
  # path<-'/home/ae/Documents/PERSONAL/FAO_SDG1441/FAOFishStats/'
  # planned updates: ASFIS, FISHSTATJ
  
  # catch<-catch[-which(catch$UNIT!='t')] # only consider catch in tonnes# catch<-catch[-which(catch$UNIT!='t')] # only consider catch in tonnes
regions<-read.csv(paste0(path,'CL_FI_WATERAREA_GROUPS.csv'),stringsAsFactors = FALSE)
species_groups<-read.csv(paste0(path,'CL_FI_SPECIES_GROUPS.csv'),stringsAsFactors = FALSE)
# asfis<-read.csv(paste0(path,'ASFIS_sp_2019.csv'),sep=';',stringsAsFactors = FALSE)
catch<-read.csv(paste0(path,'TS_FI_CAPTURE.csv'),stringsAsFactors = FALSE)


# catch<-catch[-which(catch$UNIT!='t')] # only consider catch in tonnes

for(i in 1:length(regions$Code)){
  catch[which(catch$FISHING_AREA==regions$Code[i]),'OCEAN']<-regions$Ocean_Group[i]
  catch[which(catch$FISHING_AREA==regions$Code[i]),'REGION']<-regions$Name_en[i]
}

for(i in 1:length(unique(catch$SPECIES))){
catch[which(catch$SPECIES==unique(catch$SPECIES)[i]),'ORDER']<-species_groups[which(species_groups$X3Alpha_Code==unique(catch$SPECIES)[i]),'Order']
catch[which(catch$SPECIES==unique(catch$SPECIES)[i]),'FAMILY']<-species_groups[which(species_groups$X3Alpha_Code==unique(catch$SPECIES)[i]),'Family']

catch[which(catch$SPECIES==unique(catch$SPECIES)[i]),'MAJOR_GROUP']<-species_groups[which(species_groups$X3Alpha_Code==unique(catch$SPECIES)[i]),'Major_Group']

}
# colnames(catch$FISHING_AREA)<-'FAO_AREA'
catch$mngnt_body<-NA
catch$MONTH<-NA
catch$plot_type<-'timeseries'
catch$data_source<-'FishStatJ2019'
catch$time_step<-'annual'


CATCH_FISHSTATJ<-catch[,c('OCEAN','REGION','FISHING_AREA','SPECIES','FAMILY','ORDER','MAJOR_GROUP','YEAR','MONTH','UNIT','QUANTITY','time_step','data_source','plot_type','mngnt_body')]
colnames(CATCH_FISHSTATJ)<-c('OCEAN','REGION','FAO_AREA','SPECIES','FAMILY','ORDER','MAJOR_GROUP','YEAR','MONTH','UNIT','QUANTITY','time_step','data_source','plot_type','mngnt_body')
if(length(grep(' ',CATCH_FISHSTATJ$OCEAN)>0)){CATCH_FISHSTATJ$OCEAN<-gsub(' ','',CATCH_FISHSTATJ$OCEAN)}
fish_atlas_stats<-unique(CATCH_FISHSTATJ[,c('OCEAN','REGION','MAJOR_GROUP','ORDER','FAMILY','SPECIES')])

write.csv(CATCH_FISHSTATJ,file=paste0(dirData,'/CATCH_FISHSTATJ.csv'),row.names = FALSE)
write.csv(fish_atlas_stats,file=paste0(dirData,'/fish_atlas_stats.csv'),row.names = FALSE)
return(CATCH_FISHSTATJ)
}

capture_df_levels<-function(capture_df='data/CATCH_FISHSTATJ.csv',dirData=getwd()){
  catch<-read.csv(capture_df)
  
  catch$GLOBAL<-'GLOBAL'
  
  # aggregate by level : global, ocean, region, [species?]
  # catch_region = subset(catch, !is.na(REGION)) %>% select(c(SPECIES,YEAR,QUANTITY, REGION)) %>% group_by(SPECIES,REGION, YEAR) %>% across(funs(sum)) %>% data.frame()
  catch_region = subset(catch, !is.na(REGION)) %>% select(c(SPECIES,YEAR,QUANTITY, REGION)) %>% group_by(SPECIES,REGION, YEAR) %>% summarise_each(funs(sum)) %>% data.frame()
  catch_region<-catch_region[order(catch_region$REGION),]
  catch_region[which(catch_region$REGION==''),'REGION']<-'Inland Waters'
  
  
  # region_all <-subset(catch, !is.na(REGION)) %>% select(c(YEAR,QUANTITY, REGION)) %>% group_by(REGION, YEAR) %>% across(funs(sum)) %>% data.frame()
  region_all <-subset(catch, !is.na(REGION)) %>% select(c(YEAR,QUANTITY, REGION)) %>% group_by(REGION, YEAR) %>% summarise_each(funs(sum)) %>% data.frame()
  region_all$SPECIES <- 'All spp'
  region_all[which(region_all$REGION==''),'REGION']<-'Inland Waters'
  
  catch_region<-rbind(catch_region,region_all[,c('REGION','YEAR','SPECIES','QUANTITY')])
  catch_region$YEAR<-as.Date(paste0('01-01-',catch_region$YEAR),format='%d-%m-%Y')
  
  # catch_ocean = subset(catch, !is.na(OCEAN)) %>% select(c(YEAR,QUANTITY, OCEAN,SPECIES)) %>% group_by(OCEAN, YEAR,SPECIES) %>% across(funs(sum)) %>% data.frame()
  catch_ocean = subset(catch, !is.na(OCEAN)) %>% select(c(YEAR,QUANTITY, OCEAN,SPECIES)) %>% group_by(OCEAN, YEAR,SPECIES) %>% summarise_each(funs(sum)) %>% data.frame()
  catch_ocean$OCEAN <-as.character(catch_ocean$OCEAN)
  catch_ocean[which(catch_ocean$OCEAN==''),'OCEAN']<-'Inland Waters'
  catch_ocean<-catch_ocean[order(catch_ocean$OCEAN),]
  
  # ocean_all<-subset(catch, !is.na(OCEAN)) %>% select(c(YEAR,QUANTITY, OCEAN)) %>% group_by(OCEAN, YEAR) %>% across(funs(sum)) %>% data.frame()
  ocean_all<-subset(catch, !is.na(OCEAN)) %>% select(c(YEAR,QUANTITY, OCEAN)) %>% group_by(OCEAN, YEAR) %>% summarise_each(funs(sum)) %>% data.frame()
  ocean_all$OCEAN <-as.character(ocean_all$OCEAN)
  ocean_all[which(ocean_all$OCEAN==''),'OCEAN']<-'Inland Waters'
  ocean_all$SPECIES<-'All spp'
  
  catch_ocean<-rbind(catch_ocean,ocean_all[,c('OCEAN','YEAR','SPECIES','QUANTITY')])
  catch_ocean$YEAR<-as.Date(paste0('01-01-',catch_ocean$YEAR),format='%d-%m-%Y')
  
  catch_global = subset(catch, !is.na(GLOBAL)) %>% select(c(YEAR,QUANTITY, GLOBAL,SPECIES)) %>% group_by(GLOBAL, YEAR,SPECIES) %>% summarise_each(funs(sum)) %>% data.frame()
  catch_global$GLOBAL <-as.character(catch_global$GLOBAL)
  catch_global[which(catch_global$GLOBAL==''),'GLOBAL']<-'Inland Waters'
  catch_global<-catch_global[order(catch_global$GLOBAL),]
  
  # catch_global = subset(catch, !is.na(GLOBAL)) %>% select(c(YEAR,QUANTITY, GLOBAL)) %>% group_by(GLOBAL, YEAR) %>% across(funs(sum)) %>% data.frame()
  global_all = subset(catch, !is.na(GLOBAL)) %>% select(c(YEAR,QUANTITY, GLOBAL)) %>% group_by(GLOBAL, YEAR) %>% summarise_each(funs(sum)) %>% data.frame()
  global_all$SPECIES<-'All spp'
  
  catch_global<-rbind(catch_global,global_all[,c('GLOBAL','YEAR','SPECIES','QUANTITY')])
  catch_global$YEAR<-as.Date(paste0('01-01-',catch_global$YEAR),format='%d-%m-%Y')
  
  catch_out<-list()
  catch_out$catch_global<-catch_global
  catch_out$catch_ocean<-catch_ocean
  catch_out$catch_region<-catch_region
  
  write.csv(catch_global,file=paste0(dirData,'/data/catch_global_FISHSTATJ.csv'),row.names = FALSE)
  write.csv(catch_ocean,file=paste0(dirData,'/data/catch_ocean_FISHSTATJ.csv'),row.names = FALSE)
  write.csv(catch_region,file=paste0(dirData,'/data/catch_region_FISHSTATJ.csv'),row.names = FALSE)
  
  return(catch_out)
}
catch_levels<-capture_df_levels(capture_df='data/CATCH_FISHSTATJ.csv')

capture_df_manip<-function(capture_df='data/CATCH_FISHSTATJ.csv',dirData){
  catch<-read.csv(capture_df)
  # data processing - catch fishstatj
  catch$GLOBAL<-'GLOBAL'
  
  
  # aggregate by level : global, ocean, region, [species?]
  # catch_region = subset(catch, !is.na(REGION)) %>% select(c(SPECIES,YEAR,QUANTITY, REGION)) %>% group_by(SPECIES,REGION, YEAR) %>% across(funs(sum)) %>% data.frame()
  catch_region = subset(catch, !is.na(REGION)) %>% select(c(REGION,YEAR,SPECIES,QUANTITY)) %>% group_by(REGION, YEAR,SPECIES) %>% summarise_each(funs(sum)) %>% data.frame()
  catch_region<-catch_region[order(catch_region$REGION),]
  catch_region[which(catch_region$REGION==''),'REGION']<-'Inland Waters'
  
  
  # region_all <-subset(catch, !is.na(REGION)) %>% select(c(YEAR,QUANTITY, REGION)) %>% group_by(REGION, YEAR) %>% across(funs(sum)) %>% data.frame()
  region_all <-subset(catch, !is.na(REGION)) %>% select(c(REGION,YEAR,QUANTITY)) %>% group_by(REGION, YEAR) %>% summarise_each(funs(sum)) %>% data.frame()
  region_all$SPECIES <- 'All spp'
  region_all[which(region_all$REGION==''),'REGION']<-'Inland Waters'
  
  catch_region<-rbind(catch_region,region_all[,c('REGION','YEAR','SPECIES','QUANTITY')])
  
  sumspp_region<-ddply(catch_region,.(YEAR,SPECIES,REGION), function(x) data.frame(qsum=sum(x$QUANTITY)))
  catch_region<-catch_region[which(sumspp_region$qsum>0),]
  catch_region$LEVEL<-'Region'
  
  
  
  # catch_ocean = subset(catch, !is.na(OCEAN)) %>% select(c(YEAR,QUANTITY, OCEAN,SPECIES)) %>% group_by(OCEAN, YEAR,SPECIES) %>% across(funs(sum)) %>% data.frame()
  catch_ocean = subset(catch, !is.na(OCEAN)) %>% select(c(OCEAN,YEAR,SPECIES,QUANTITY)) %>% group_by(OCEAN, YEAR,SPECIES) %>% summarise_each(funs(sum)) %>% data.frame()
  catch_ocean$OCEAN <-as.character(catch_ocean$OCEAN)
  catch_ocean[which(catch_ocean$OCEAN==''),'OCEAN']<-'Inland Waters'
  catch_ocean<-catch_ocean[order(catch_ocean$OCEAN),]
  
  # ocean_all<-subset(catch, !is.na(OCEAN)) %>% select(c(YEAR,QUANTITY, OCEAN)) %>% group_by(OCEAN, YEAR) %>% across(funs(sum)) %>% data.frame()
  ocean_all<-subset(catch, !is.na(OCEAN)) %>% select(c(OCEAN,YEAR,QUANTITY )) %>% group_by(OCEAN, YEAR) %>% summarise_each(funs(sum)) %>% data.frame()
  ocean_all$OCEAN <-as.character(ocean_all$OCEAN)
  ocean_all[which(ocean_all$OCEAN==''),'OCEAN']<-'Inland Waters'
  ocean_all$SPECIES<-'All spp'
  
  catch_ocean<-rbind(catch_ocean,ocean_all[,c('OCEAN','YEAR','SPECIES','QUANTITY')])
  sumspp_ocean<-ddply(catch_ocean,.(YEAR,SPECIES,OCEAN), function(x) data.frame(qsum=sum(x$QUANTITY)))
  catch_ocean<-catch_ocean[which(sumspp_ocean$qsum>0),]
  colnames(catch_ocean)<-c('REGION',colnames(catch_ocean)[2:4])
  catch_ocean$LEVEL<-'Ocean'  
  
  catch_global = subset(catch, !is.na(GLOBAL)) %>% select(c(GLOBAL,YEAR,SPECIES,QUANTITY)) %>% group_by(GLOBAL, YEAR,SPECIES) %>% summarise_each(funs(sum)) %>% data.frame()
  catch_global$GLOBAL <-as.character(catch_global$GLOBAL)
  catch_global[which(catch_global$GLOBAL==''),'GLOBAL']<-'Inland Waters'
  catch_global<-catch_global[order(catch_global$GLOBAL),]
  
  # catch_global = subset(catch, !is.na(GLOBAL)) %>% select(c(YEAR,QUANTITY, GLOBAL)) %>% group_by(GLOBAL, YEAR) %>% across(funs(sum)) %>% data.frame()
  global_all = subset(catch, !is.na(GLOBAL)) %>% select(c(GLOBAL,YEAR,QUANTITY)) %>% group_by(GLOBAL, YEAR) %>% summarise_each(funs(sum)) %>% data.frame()
  global_all$SPECIES<-'All spp'
  
  catch_global<-rbind(catch_global,global_all[,c('GLOBAL','YEAR','SPECIES','QUANTITY')])
  
  sumspp_global<-ddply(catch_global,.(YEAR,SPECIES,GLOBAL), function(x) data.frame(qsum=sum(x$QUANTITY)))
  catch_global<-catch_global[which(sumspp_global$qsum>0),]
  colnames(catch_global)<-c('REGION',colnames(catch_global)[2:4])
  catch_global$LEVEL<-'Global'  
  
  catch_levels<-rbind(catch_global,catch_ocean,catch_region)
  
  write.csv(catch_levels,file=paste0(dirData,'/catch_levels.csv'),row.names = FALSE)
  return(catch_levels)
  }


nominal_global_catch<-function(dirData,dirCode){
  # source(paste0(dirCode,'/global_catch_5deg_1m_ird_level0_20210119074338.R')) # data request via sdi-lab not working
  annomcatch<-read.csv(paste0(dirData,'/global_nominal_catch_ird_level0 - global_nominal_catch_ird_level0.csv'),stringsAsFactors = FALSE)
  library(lubridate)
  annomcatch$year<-year(as.Date(annomcatch$time_start,format='%Y-%m-%d'))
  issgear<-read.csv(paste0(dirData,'/gear_type_isscfg_conversions.csv'))
  
  annomcatch$gear_code<-NULL
  for(i in 1:length(unique(issgear$gear_type))){
  annomcatch[which(annomcatch$gear==issgear$gear_type[i]),'gear_code']<-issgear$isscfg_code[i]
  }
  library(plyr)
  dfc<-ddply(annomcatch,.(species, year, gear_code), function(x) data.frame(quantity=sum(x$value,na.rm=T)))
  
  dfc_all = dfc %>% select(c(year,gear_code,quantity)) %>% group_by(year, gear_code) %>% summarise_each(funs(sum)) %>% data.frame()
  dfc_all$species<-'All spp'
  
  dfc<-rbind(dfc,dfc_all[,c('species','year','gear_code','quantity')])
  
  dfc$year<-as.numeric(dfc$year)
  # monthgiscatch<-read.csv(paste0(dirData,'/global_catch_5deg_1m_firms_level0_20210119144707.csv'),stringsAsFactors = FALSE)
write.csv(dfc,file=paste0(dirData,'/nominal_catch_by_gear_tunaatlas.csv'),row.names = FALSE)
  return(dfc)
}


monthly_global_catch_5deg<-function(dirData,dirCode){
  # df<- yearAttributeName="year", 
  # monthAttributeName="month", 
  # speciesAttributeName="species",
  # gearTypeAttributeName="gear",
  # valueAttributeName="value",
  # meanPrev5YearsAttributeName="mean_prev_5_years",
  # stddevPrev5YearsAttributeName="stddev_prev_5_years"
  
  # source(paste0(dirCode,'/global_catch_5deg_1m_ird_level0_20210119074338.R')) # data request via sdi-lab not working
  monthcatch<-read.csv(paste0(dirData,'/global_catch_5deg_1m_firms_level0.csv'),stringsAsFactors = FALSE)
  library(plyr)
  mdf<-ddply(monthcatch,.(year,month,species, gear), function(x) data.frame(quantity=sum(x$value,na.rm=T)))
  # mdf.prev5yrs<-ddply(mdf,.(year,month,species,gear),function(x) data.frame(mean_prev_5_years=mean(x[!is.na(which(x$year==x$year-5)),'value'],na.rm=T)))
  
  library(zoo)
 
  avg.last.60 <- function (x) if (length(x) < 60) rep(NA, length(x)) else rollmeanr(x, 60, fill = NA)  ## 1.
  stdev.last.60 <- function (x) if (length(x) < 60) rep(NA, length(x)) else rollapply(x, width = 60, FUN = sd, na.rm = TRUE,fill =NA)  ## 1.
  
  alb<-mdf.prev5yrs[which(mdf.prev5yrs$species=='ALB'),]
  num_gear<-length(unique(alb$gear))
  year_range<-range(alb$year)

  dat2 <- data.frame(year = rep(format(seq(as.Date(paste0(year_range[1],"/1/1")),
                                         as.Date(paste0(year_range[2],"/12/1")), by = "month"),
                                     format = "%Y"),n=num_gear),
                     month = rep(1:12,n=(year_range[2]-year_range[1]) ),
                     species=rep('ALB',n=((year_range[2]-year_range[1]+1)*12)),
                     gear=rep(unique(alb$gear)),
                     quantity=NA)
  dat2$year<-as.integer(dat2$year)
  # then, merge dat and dat2
  alb %>%
    select(year, month) %>%
    right_join(dat2, by = c("year","month")) %>%
    select(year, month)
  
  
  res <- alb %>% group_by(species,gear) %>% arrange(year,month) %>%     ## 2.
    mutate(mean_prev_5_years=avg.last.60(quantity)) %>%
    # mutate(stddev_prev_5_years=stdev.last.60(quantity)) %>% ## 3.
    ungroup() %>% arrange(species,gear,year,month)         ## 4.
  resdf<-as.data.frame(res)
  
  
  library(dplyr)
  res <- mdf %>% group_by(species,gear) %>% arrange(year,month) %>%     ## 2.
    mutate(mean_prev_5_years=avg.last.60(quantity)) %>%
    # mutate(stddev_prev_5_years=stdev.last.60(quantity)) %>% ## 3.
    ungroup() %>% arrange(species,gear,year,month)         ## 4.
  
  res2 <- res %>% group_by(species,gear) %>% arrange(year,month) %>%     ## 2.
    # mutate(mean_prev_5_years=avg.last.60(quantity)) %>%
    mutate(stddev_prev_5_years=stdev.last.60(quantity)) %>% ## 3.
    ungroup() %>% arrange(species,gear,year,month)         ## 4.
  
  mdf.prev5yrs<-as.data.frame(res2)
  
  issgear<-read.csv(paste0(dirData,'/gear_type_isscfg_conversions.csv'))
  

  for(i in 1:length(unique(issgear$gear_type))){
    mdf.prev5yrs[which(mdf.prev5yrs$gear==issgear$gear_type[i]),'gear_code']<-issgear$isscfg_code[i]
  }
  
 
  

  library(lubridate)
  # annom
  catch$year<-year(as.Date(annomcatch$time_start,format='%Y-%m-%d'))
    
  dfc<-ddply(annomcatch,.(species, year, gear_code), function(x) data.frame(quantity=sum(x$value,na.rm=T)))
  
  dfc_all = dfc %>% select(c(year,gear_code,quantity)) %>% group_by(year, gear_code) %>% summarise_each(funs(sum)) %>% data.frame()
  dfc_all$species<-'All spp'
  
  dfc<-rbind(dfc,dfc_all[,c('species','year','gear_code','quantity')])
  
  dfc$year<-as.numeric(dfc$year)
  # monthgiscatch<-read.csv(paste0(dirData,'/global_catch_5deg_1m_firms_level0_20210119144707.csv'),stringsAsFactors = FALSE)
  write.csv(mdf.prev5yrs,file=paste0(dirData,'/monthly_catch_by_gear_tunaatlas_5yearavg.csv'),row.names = FALSE)
  return(dfc)
}







setwd('/home/ae/Documents/PERSONAL/FAO/BlueCloud/Indicators/Fisheries_Atlas_Indicators_Shiny')
dirData<-paste0(getwd(),'/data')
dirCode<-paste0(getwd(),'/Rscripts')
code_path=dirCode
data_path=dirData
rfmo='IOTC'

effort<-function(
  code_path=dirCode,
  data_path=dirData,
  rfmo=rfmo){
  ## AE : IMPROVEMENTS - generalise for effort dictionary
  ## - add a section to merge other RFMO data
  ## - catch by gear
  
  
  source(paste0(code_path,'/country_codes.R'))
 effort_path<- paste0(data_path,'/effort')
 country_codes<-read.csv(paste0(data_path,'/country_codes.csv'),stringsAsFactors = FALSE)
 country_codes<-country_codes[-which(country_codes$OCEAN=='Inland Waters'),]
  ## must be converted to TunaAtlas standards
  library('plyr')
  library('readr')
  library('reshape2')
  library('purrr')
  # combine the effort files from RFMOs by metadata
  # if(rfmo=='IOTC'){
    # load different catch-and-effort datasets
    files<-list.files(effort_path)
    effort_files<-paste0(effort_path,'/',files[ grep(rfmo,files)])
    # load and merge the effort files for the rfmo of interest
    dat_csv = ldply(effort_files, read_csv)
    # exclude catch information (leaving it in requires further treatment for merging)
    
    if(rfmo=='IOTC'){
      effort <- dat_csv[,c('Fleet','Gear','Year','MonthStart','MonthEnd','Grid','Effort','EffortUnits',
                           'QualityCode','Source')]
      effort[grep('REU',effort$Fleet),'Fleet']<-'RUN'
      effort[grep('EU',effort$Fleet),'Fleet']<-unlist(purrr::map(strsplit(effort[grep('EU',effort$Fleet),'Fleet'],'EU'),2))
      effort[grep('RUN',effort$Fleet),'Fleet']<-'REU'
    }
    
    ## Time series data of effort
    # assign regions and oceans to gridded data
    effort<-rfmogrid_2_FAOMFA(path=data_path,rfmo=rfmo,df=effort)
    EFFORT<-as.data.frame(effort[,c('Year','Gear','Fleet','MonthStart','Effort','EffortUnits',
                                    'QualityCode','Source','Region','Ocean','FAO_AREA_CODE')])
 
    colnames(EFFORT)<-c('YEAR','GEAR','FLEET','MONTH','QUANTITY','UNIT','QUALITY','SOURCE','REGION','OCEAN','FAO_AREA','LON','LAT')
    EFFORT_TS_MONTHLY<-ddply(EFFORT,.(YEAR,MONTH,GEAR,FLEET,UNIT,QUALITY,SOURCE,REGION,OCEAN,FAO_AREA), function(x) data.frame(QUANTITY=sum(x$QUANTITY,na.rm=T)))
    EFFORT_TS_ANNUAL<-ddply(EFFORT,.(YEAR,GEAR,FLEET,UNIT,QUALITY,SOURCE,REGION,OCEAN,FAO_AREA), function(x) data.frame(QUANTITY=sum(x$QUANTITY,na.rm=T)))
     
## ADD A SECTION TO MERGE MULTIPLE RFMO DATASETS    
     write.csv(EFFORT_TS_ANNUAL,file=paste0(data_path,'/EFFORT_TS_ANNUAL.csv'),row.names=FALSE)
     write.csv(EFFORT_TS_MONTHLY,file=paste0(data_path,'/EFFORT_TS_MONTHLY.csv'),row.names=FALSE)
}

setwd('/home/ae/Documents/PERSONAL/FAO/BlueCloud/Indicators/Fisheries_Atlas_Indicators_Shiny')
dirData<-paste0(getwd(),'/data')
dirCode<-paste0(getwd(),'/Rscripts')
code_path=dirCode
data_path=dirData
rfmo='IOTC'

spatial_gear<-function(
  code_path=dirCode,
  data_path=dirData,
  rfmo=rfmo){
  
  
  source(paste0(code_path,'/country_codes.R'))
  effort_path<- paste0(data_path,'/effort')
  country_codes<-read.csv(paste0(data_path,'/country_codes.csv'),stringsAsFactors = FALSE)
  country_codes<-country_codes[-which(country_codes$OCEAN=='Inland Waters'),]
  ## must be converted to TunaAtlas standards
  library('plyr')
  library('readr')
  library('reshape2')
  library('purrr')
  
  # combine the effort files by metadata

  # load different catch-and-effort datasets
  files<-list.files(effort_path)
  effort_files<-paste0(effort_path,'/',files[ grep(rfmo,files)])
  
  # load and merge the effort files for the rfmo of interest
  dat_csv = ldply(effort_files, read_csv)
  
  ## check plot

  
     ## CATCH BY GEAR (T) - stratified by species, free/fad schools

       ## rename and reorganise catch data by species, catch units (select tonnes only), and school type
       # remove catch units of NO
       catch_by_gear<-dat_csv[-grep('NO',colnames(dat_csv))]
       catch_by_gear$CatchUnits<-'MT'
       
       chck<-ddply(catch_by_gear,.(Year),function(x) data.frame(catch=sum(as.numeric(x[,12:dim(x)[2]],na.rm=T))))

      # reassign catch columns catch unit type to just species for later merge with school type catch information
      species<-NULL
        for(i in 1:length(grep('MT',colnames(catch_by_gear)))){
         colnames(catch_by_gear)<-gsub('-','.',colnames(catch_by_gear))
         species[i]<-unlist(purrr::map(strsplit(colnames(catch_by_gear[grep('MT',colnames(catch_by_gear))]),'\\.'),1))[i]
         eval(parse(text=paste0('catch_by_gear$',species[i],'<-catch_by_gear$',species[i],'.MT')))
       }
       catch_by_gear<-catch_by_gear[,-grep('.MT',colnames(catch_by_gear))]
       
       ## separate out the specific school types
       school_types<-unique(unlist(purrr::map(strsplit(colnames(catch_by_gear),'\\.'),2)))
       stcol<-grep(paste0('\\.',school_types,collapse='|'),colnames(catch_by_gear))
       
       # add a column to specific school types by column
       catch_by_gear$SchoolType<-NA
       for (s in 1:length(stcol)){
         catch_by_gear[which(!is.na(catch_by_gear[stcol[s]])),'SchoolType'] <- unique(unlist(purrr::map(strsplit(colnames(catch_by_gear)[stcol[s]],'\\.'),2)))
       }

       # empty the NAs
       catch_by_gear[,1:(dim(catch_by_gear)[2]-1)] <- lapply(catch_by_gear[,1:(dim(catch_by_gear)[2]-1)], function(x){ x[is.na(x)] <- ""
       return(x) })
       catch_by_gear<-as.data.frame(catch_by_gear)
       
       # merge the columns with the same species

       for(p in 1:length(species)){
         eval(parse(text=paste0('catch_by_gear$',tolower(species[p]),' <-NA')))
         eval(parse(text=paste0('catch_by_gear$',tolower(species[p]),'<-as.numeric(apply(catch_by_gear[,grep(species[p],colnames(catch_by_gear)),drop=F], 1, function(x) paste(x,sep="",collapse="") ))')))
         
       }
       
       # select_df<-catch_by_gear[,tolower(species)]
       # df<- cbind(catch_by_gear[,c(1:4,7:12)],catch_by_gear[,c('SchoolType',tolower(species))])
       # 
       # GEAR<-melt(df)
       
       if(rfmo=='IOTC'){
         gear <- catch_by_gear[,c('Fleet','Gear','Year','MonthStart','Grid','Effort','EffortUnits',
                                     'QualityCode','Source','CatchUnits','SchoolType',tolower(species))]
         gear[grep('REU',gear$Fleet),'Fleet']<-'RUN'
         gear[grep('EU',gear$Fleet),'Fleet']<-unlist(purrr::map(strsplit(gear[grep('EU',gear$Fleet),'Fleet'],'EU'),2))
         gear[grep('RUN',gear$Fleet),'Fleet']<-'REU'
       }
      
       ## Time series data of effort
       # assign regions and oceans to gridded data
       gear<-rfmogrid_2_FAOMFA(path=data_path,rfmo=rfmo,df=gear)
       GEAR<-as.data.frame(gear[,c('Year','Gear','Fleet','MonthStart','Effort','EffortUnits',
                                       'QualityCode','Source','CatchUnits','SchoolType','Region','Ocean','FAO_AREA_CODE',tolower(species))])
       
       colnames(GEAR)<-c('YEAR','GEAR','FLEET','MONTH','EFFORTQUANTITY','EFFORTUNITS','QUALITY','SOURCE','CATCHUNITS','SCHOOLTYPE','REGION','OCEAN','FAO_AREA',toupper(species),'LON','LAT')
       df<-melt(GEAR,id.vars = c('YEAR','GEAR','FLEET','MONTH','EFFORTQUANTITY','EFFORTUNITS','QUALITY','SOURCE','CATCHUNITS','SCHOOLTYPE','REGION','OCEAN','FAO_AREA','LON','LAT'))
       colnames(df)<-c('YEAR','GEAR','FLEET','MONTH','QUANTITY','UNIT','QUALITY','SOURCE','CATCHUNITS','SCHOOLTYPE','REGION','OCEAN','FAO_AREA','LON','LAT','SPECIES','CATCH')
       GEAR_TS_MONTHLY<-ddply(df,.(YEAR,MONTH,GEAR,REGION,OCEAN,FAO_AREA,SPECIES), function(x) data.frame(QUANTITY=sum(x$CATCH,na.rm=T)))
       GEAR_TS_ANNUAL<-ddply(df,.(YEAR,GEAR,REGION,OCEAN,FAO_AREA,SPECIES), function(x) data.frame(QUANTITY=sum(x$CATCH,na.rm=T)))
       
       ## ADD A SECTION TO MERGE MULTIPLE RFMO DATASETS    
       write.csv(GEAR_TS_ANNUAL,file=paste0(data_path,'/GEAR_TS_ANNUAL.csv'),row.names=FALSE)
       write.csv(GEAR_TS_MONTHLY,file=paste0(data_path,'/GEAR_TS_MONTHLY.csv'),row.names=FALSE)
       
}

setwd('/home/ae/Documents/PERSONAL/FAO/BlueCloud/Indicators/Fisheries_Atlas_Indicators_Shiny')
dirData<-paste0(getwd(),'/data')
dirCode<-paste0(getwd(),'/Rscripts')
code_path=dirCode
data_path=dirData
rfmo='IOTC'

gear<-function(code_path=dirCode,
               data_path=dirData,
               rfmo='IOTC'
){
  
  
}

       
#        for(j in 1:length(school_types)){
#          unite(as.data.frame(catch_by_gear), newcol, c(B, C, D), sep="")
#          
#          for(i in 1:length(grep(school_types[j],colnames(catch_by_gear)))){
#            # colnames(catch_by_gear)<-gsub('-','.',colnames(catch_by_gear))
#            species<-unlist(purrr::map(strsplit(colnames(catch_by_gear[grep(school_types[j],colnames(catch_by_gear))]),'\\.'),1))[i]
#            eval(parse(text=paste0('catch_by_gear[,is.na(c("',species,'","',species,'.',school_types[j],'")]<-""')))
#            eval(parse(text=paste0('catch_by_gear$',species,'<-catch_by_gear$',species,'.',school_types[j])))
#          }
#          
#       }
#        cbgm<-melt(catch_by_gear,id.vars = c('Fleet','Gear','Year','MonthStart','Grid','Effort','EffortUnits',
#                                             'QualityCode','Source','CatchUnits'))
#        
#        
#        # x<-rbind(eval(parse(text=paste0('read.csv("',paste(path,effort_files,sep='/'),'")'))),by=col.names)
#        ll<-read.csv(paste0(effort_path,'/',rfmo,'-LL-CE.csv'))
#        llm<-melt(ll,id.vars = colnames(ll)[1:11])
#        if(rfmo=='IOTC'){
#        llm$CatchUnits[grep('MT',llm$variable)]<-'MT'
#        llm$CatchUnits[grep('NO',llm$variable)]<-'NO'
#        llm$Species<-unlist(purrr::map(strsplit(as.character(llm$variable),'\\.'),1))
# 
#        psbb<-read.csv(paste0(effort_path,'/',rfmo,'-PSBB-CE.csv'))
#        psbbm<-melt(psbb,id.vars = colnames(psbb[c('Fleet','Gear','Year','MonthStart','Grid','Effort','EffortUnits','QualityCode','Source','CatchUnits')]))
#        othr<-read.csv(paste0(effort_path,'/',rfmo,'-OTHR-CE.csv'))
# 
#      # merge
#        merge(ll,psbb,othr)
#        }
#      
#      
#      
#      return (EFFORT_TS_ANNUAL)
# }
    # mdat<-melt(dat_csv,id.vars = colnames(dat_csv)[1:11])
    # cd<-unlist(strsplit(colnames(dat_csv),'-'))
# dat_csv[grep('MT',colnames(dat_csv))]
  #   cd<-colnames(dat_csv)
  #   grep('MT',colnames(dat_csv))
  #   # x<-rbind(eval(parse(text=paste0('read.csv("',paste(path,effort_files,sep='/'),'")'))),by=col.names)
  #   ll<-read.csv(paste0(path,'/',rfmo,'-LL-CE.csv'))
  #   llm<-melt(ll,id.vars = colnames(ll)[1:11])
  #   llm$CatchUnits[grep('MT',llm$variable)]<-'MT'
  #   llm$CatchUnits[grep('NO',llm$variable)]<-'NO'
  #   llm$Species<-unlist(map(strsplit(as.character(llm$variable),'\\.'),1))
  #   
  #   psbb<-read.csv(paste0(path,'/',rfmo,'-PSBB-CE.csv'))
  #   psbbm<-melt(psbb,id.vars = colnames(psbb)[1:12])
  #   othr<-read.csv(paste0(path,'/',rfmo,'-OTHR-CE.csv'))
  #   
  # # merge
  #   merge(ll,psbb,othr)
  }
    
    
  }
  
  
  
}


## copernicus 
## set up an 'fisheries atlas' username...
env<-function(username='anieblas', 
              pwd='Torben6.',
              filepath=paste0(dirData,'/Environment'),
              filename='test.nc',
              lat.low=35, 
              lat.hi=38, 
              lon.low=80,
              lon.hi=85,
              depth.low=1,
              depth.hi=2,
              time.low='2020-08-16 12:00:00',
              time.hi='2020-08-16 12:00:00',
              variable
            )
xx<-do.call(paste0('system(python3 -m motuclient --motu http://nrt.cmems-du.eu/motu-web/Motu --service-id GLOBAL_ANALYSIS_FORECAST_PHY_001_024-TDS 
       --product-id global-analysis-forecast-phy-001-024-monthly --longitude-min -180 --longitude-max 179.9166717529297 
       --latitude-min -80 --latitude-max 90 --date-min "2020-08-16 12:00:00" --date-max "2020-08-16 12:00:00" 
       --depth-min 0.493 --depth-max 0.4942 --variable bottomT --variable mlotst --variable so --variable thetao 
       --variable uo --variable vo --variable zos --out-dir "',filepath,'" --out-name "',filename,'" --user "',username,'" --pwd "',pwd,'"')))

