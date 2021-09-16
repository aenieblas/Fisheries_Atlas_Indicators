## fisheries atlas

## AE improvements - # link to OpenFairViewer queryinfo to get data

# first tab : Catch by Region 
# 1) add a time selection slider, 2) allow download of dataset, 
# 3) make revert to 'all spp' when changing between levels

# second tab : Effort by Region
# 


# load required packages
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
# if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")

library("ows4R")
library("sp")
library('readr')
library("shiny")
library("dplyr")
library("plotly")
library("DT")
library("shinyWidgets")
# library("shinycssloaders")
library("jsonlite")

#load module functions
# source("https://raw.githubusercontent.com/eblondel/OpenFairViewer/master/src/resources/shinyModule/QueryInfo.R")



# here to read data already transformed. use queryinfo at transformation step (data_support_FishAtlas.R)
## catch data - fishstatj
# catch<-read.csv('data/CATCH_FISHSTATJ.csv')

#monthly - tunaatlas
source('~/Downloads/global_catch_1deg_1m_ps_bb_ird_level0_20201029131417.R')
catch<-

# plot functions

catchbyregion_plot = function(catchts){
  
  #aggregate values by years and ocean
  aggData <- aggregate(QUANTITY ~ region + YEAR, data=catchts, sum)
  
  #keep only common time extent
  # max_year <- min(unlist(lapply(unique(catch), function(o) {return(if(length(subset(catch, OCEAN==o)$YEAR) > 0) max(subset(catch, OCEAN==o)$YEAR) else NA)})), na.rm=TRUE)
  # min_year <- max(unlist(lapply(unique(catch), function(o) {return(if(length(subset(catch, OCEAN==o)$YEAR) > 0) min(subset(catch, OCEAN==o)$YEAR) else NA)})), na.rm=TRUE)
  # aggData <- subset(aggData, YEAR >= min_year & YEAR <= max_year)
  
  #convert values from tons to thousand tons
  aggData$QUANTITY <- aggData$QUANTITY / 1000
  
  nb.cols <- length(unique(aggData$region))
  my.colors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)
  names(my.colors) <- unique(aggData$region)
  
  print(
    
    #build the plot
    resultPlot <- ggplot(aggData, aes(x=YEAR, y=QUANTITY,group=region)) + 
      geom_area(aes(fill=region), position="stack") + 
      geom_line(position="stack", color="grey20") + 
      scale_fill_manual(name="Ocean/Region", values=my.colors) +
      xlab("Year") + ylab("Catches in thousand tons") + 
      theme(legend.position="bottom")
    
  )
  # ggplotly(resultPlot, tooltip = c("text", "size")) %>% layout(legend = list(font = list(size=11)))
}

# 
# effortbyregion_plot = function(effortts){
#   
#   #aggregate values by years and ocean
#   aggData <- aggregate(QUANTITY ~ region + YEAR, data=catchts, sum)
#   
#   #keep only common time extent
#   # max_year <- min(unlist(lapply(unique(catch), function(o) {return(if(length(subset(catch, OCEAN==o)$YEAR) > 0) max(subset(catch, OCEAN==o)$YEAR) else NA)})), na.rm=TRUE)
#   # min_year <- max(unlist(lapply(unique(catch), function(o) {return(if(length(subset(catch, OCEAN==o)$YEAR) > 0) min(subset(catch, OCEAN==o)$YEAR) else NA)})), na.rm=TRUE)
#   # aggData <- subset(aggData, YEAR >= min_year & YEAR <= max_year)
#   
#   #convert values from tons to thousand tons
#   aggData$QUANTITY <- aggData$QUANTITY / 1000
#   
#   nb.cols <- length(unique(aggData$region))
#   my.colors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)
#   names(my.colors) <- unique(aggData$region)
#   
#   print(
#     
#     #build the plot
#     resultPlot <- ggplot(aggData, aes(x=YEAR, y=QUANTITY,group=region)) + 
#       geom_area(aes(fill=region), position="stack") + 
#       geom_line(position="stack", color="grey20") + 
#       scale_fill_manual(name="Ocean/Region", values=my.colors) +
#       xlab("Year") + ylab("Catches in thousand tons") + 
#       theme(legend.position="bottom")
#     
#   )
#   # ggplotly(resultPlot, tooltip = c("text", "size")) %>% layout(legend = list(font = list(size=11)))
# }

catchbygear_plot = function(df=gearts,
                            yearAttributeName="YEAR", 
                            speciesAttributeName="species",
                            gearTypeAttributeName="gear",
                            valueAttributeName="QUANTITY",
                            withSparql=TRUE){
  if (! require(XML) | ! require(ggplot2) | ! require(RColorBrewer)) {
    stop("Missing library")
  }
  
  if (missing(df)) {
    stop("Input data frame not specified")
  }
  
  #check for input attributes
  if(sum(names(df) == yearAttributeName) == 0) {
    stop("Cannot found year attribute")
  }
  
  if(sum(names(df) == speciesAttributeName) == 0) {
    stop("Cannot found species attribute")
  }
  
  if(sum(names(df) == gearTypeAttributeName) == 0) {
    stop("Cannot found gear attribute")
  }
  
  if(sum(names(df) == valueAttributeName) == 0) {
    stop("Cannot found value attribute")
  }  
  
  #format columns  
  df[, yearAttributeName] <- as.numeric(df[, yearAttributeName])
  df[, speciesAttributeName] <- as.factor(df[, speciesAttributeName])
  df[, gearTypeAttributeName] <- as.factor(df[, gearTypeAttributeName])
  df[, valueAttributeName] <- as.numeric(df[, valueAttributeName])    
  
  #aggregate to cut other columns
  df <- aggregate(x=df[, valueAttributeName], 
                  by=list(df[, yearAttributeName], df[, speciesAttributeName], df[, gearTypeAttributeName]), 
                  FUN=sum)
  #rename columns
  names(df) <- c("year", "species", "gear_type", "value")
  
  # for (species.current in unique(df$species)) {
  current.df <- df[df$species == species.current,]
  
  #aggregate values by years and gear type
  aggData <- aggregate(value ~ gear_type + year, data=current.df, sum)
  
  #convert values from tons to thousand tons
  aggData$value <- aggData$value / 1000
  
  #order factors levels by value
  aggData$gear_type <- factor(aggData$gear_type, levels=rev(levels(reorder(aggData$gear_type, aggData$value))))
  
  species.label <- species.current
  species.URI <- species.current
  
  nb.cols <- length(unique(aggData$gear_type))
  my.colors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)
  names(my.colors) <- unique(aggData$gear_type)
  
  print(
    #build the plot
    resultPlot <- ggplot(aggData, aes(x=year, y=value, fill=gear_type, order=gear_type)) + 
      geom_bar(stat="identity", width=0.8) + 
      geom_bar(stat="identity", width=0.8, colour="grey20", show.legend=FALSE) + 
      scale_fill_manual(name="Gear type", values=my.colors) +
      xlab("Year") + ylab("Catches in thousand tons") + 
      # ggtitle(paste(species.label, "catches by gear type")) +
      theme(legend.position="bottom")
  )
  library(rCharts)
  plotRchartsHighcharts  <- hPlot(value ~ year, data = aggData, type = 'column', group = 'gear_type', radius = 6, title = "Catches per month per fishing gear",width = "100%")
  plotRchartsHighcharts$xAxis(labels = list(rotation = -45, align = 'right', style = list(fontSize = '13px', fontFamily = 'Verdana, sans-serif')), replace = F)
  plotRchartsHighcharts$plotOptions(column = list(stacking = "normal", dataLabels = list(enabled = T, rotation = -90, align = 'right', color = '#FFFFFF', x = 4, y = 10, style = list(fontSize = '13px', fontFamily = 'Verdana, sans-serif'))))
  plotRchartsHighcharts$legend(align = 'center', verticalAlign = 'top', y = 30, margin = 20)
  # plotRchartsHighcharts$chart(width = 800,height = 400, zoomType = "xy")
  plotRchartsHighcharts$chart(zoomType = "xy")
  plotRchartsHighcharts$exporting(enabled = T)
  plotRchartsHighcharts 
  
  
  ## {title: MultiBar Chart}
  plotRchartsNVD3 <- nPlot(value ~ year, group = 'gear_type', data = aggData, type = 'multiBarChart', width = 800, height = 400)
  plotRchartsNVD3$xAxis(axisLabel = 'Year')
  plotRchartsNVD3$yAxis(axisLabel = 'Catches')
  # plotRchartsNVD3$chart(width = 800, height = 400, useInteractiveGuideline=TRUE)
  plotRchartsNVD3
  
}




# data processing
catch$GLOBAL<-'GLOBAL'
effort$GLOBAL<-'GLOBAL'

# aggregate by level : global, ocean, region, [species?]
# catch_region = subset(catch, !is.na(REGION)) %>% select(c(SPECIES,YEAR,QUANTITY, REGION)) %>% group_by(SPECIES,REGION, YEAR) %>% across(funs(sum)) %>% data.frame()
catch_region = subset(catch, !is.na(REGION)) %>% select(c(SPECIES,YEAR,QUANTITY, REGION)) %>% group_by(SPECIES,REGION, YEAR) %>% summarise_each(funs(sum)) %>% data.frame()
catch_region<-catch_region[order(catch_region$REGION),]

effort_region = subset(effort, !is.na(REGION)) %>% select(c(UNIT,GEAR,YEAR,QUANTITY, REGION)) %>% group_by(UNIT,GEAR,REGION, YEAR) %>% summarise_each(funs(sum)) %>% data.frame()
effort_region<-effort_region[order(effort_region$REGION),]

# region_all <-subset(catch, !is.na(REGION)) %>% select(c(YEAR,QUANTITY, REGION)) %>% group_by(REGION, YEAR) %>% across(funs(sum)) %>% data.frame()
region_all <-subset(catch, !is.na(REGION)) %>% select(c(YEAR,QUANTITY, REGION)) %>% group_by(REGION, YEAR) %>% summarise_each(funs(sum)) %>% data.frame()
region_all$SPECIES <- 'All spp'

region_all_effort <-subset(effort, !is.na(REGION)) %>% select(c(YEAR,QUANTITY, REGION)) %>% group_by(REGION, YEAR) %>% summarise_each(funs(sum)) %>% data.frame()
region_all_effort$GEAR <- 'All gears'
region_all_effort$UNIT <- 'All effort units'


catch_region<-rbind(catch_region,region_all[,c('SPECIES','REGION','YEAR','QUANTITY')])
effort_region<-rbind(effort_region,region_all_effort[,c('GEAR','UNIT','REGION','YEAR','QUANTITY')])

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
# catch_global = subset(catch, !is.na(GLOBAL)) %>% select(c(YEAR,QUANTITY, GLOBAL)) %>% group_by(GLOBAL, YEAR) %>% across(funs(sum)) %>% data.frame()
catch_global = subset(catch, !is.na(GLOBAL)) %>% select(c(YEAR,QUANTITY, GLOBAL)) %>% group_by(GLOBAL, YEAR) %>% summarise_each(funs(sum)) %>% data.frame()
catch_global$SPECIES<-'All spp'


##  gear data frame manipulation
gear$GLOBAL<-'GLOBAL'

gear_region = subset(gear, !is.na(REGION)) %>% select(c(SPECIES,GEAR,YEAR,QUANTITY, REGION)) %>% group_by(SPECIES,GEAR,REGION, YEAR) %>% summarise_each(funs(sum)) %>% data.frame()
gear_region<-gear_region[order(gear_region$REGION),]

region_all_gear <-subset(gear, !is.na(REGION)) %>% select(c(YEAR,QUANTITY, REGION)) %>% group_by(REGION, YEAR) %>% summarise_each(funs(sum)) %>% data.frame()
region_all_gear$GEAR <- 'All gears'
region_all_gear$SPECIES <- 'All spp'

gear_region<-rbind(gear_region,region_all_gear[,c('SPECIES','GEAR','REGION','YEAR','QUANTITY')])

gear_ocean = subset(gear, !is.na(OCEAN)) %>% select(c(YEAR,QUANTITY, OCEAN,SPECIES,GEAR)) %>% group_by(OCEAN, YEAR,SPECIES,GEAR) %>% summarise_each(funs(sum)) %>% data.frame()
gear_ocean$OCEAN <-as.character(gear_ocean$OCEAN)
gear_ocean[which(gear_ocean$OCEAN==''),'OCEAN']<-'Inland Waters'
gear_ocean<-gear_ocean[order(gear_ocean$OCEAN),]

gear_ocean_all<-subset(gear, !is.na(OCEAN)) %>% select(c(YEAR,QUANTITY, OCEAN)) %>% group_by(OCEAN, YEAR) %>% summarise_each(funs(sum)) %>% data.frame()
gear_ocean_all$OCEAN <-as.character(gear_ocean_all$OCEAN)
gear_ocean_all[which(gear_ocean_all$OCEAN==''),'OCEAN']<-'Inland Waters'
gear_ocean_all$SPECIES<-'All spp'
gear_ocean_all$GEAR<-'All gears'

gear_ocean<-rbind(gear_ocean,gear_ocean_all[,c('OCEAN','YEAR','SPECIES','GEAR','QUANTITY')])

gear_global = subset(gear, !is.na(GLOBAL)) %>% select(c(YEAR,QUANTITY, GLOBAL)) %>% group_by(GLOBAL, YEAR) %>% summarise_each(funs(sum)) %>% data.frame()
gear_global$SPECIES<-'All spp'
gear_global$GEAR<-'All gears'



# ui
ui <- bootstrapPage(
  tags$head(includeHTML("/home/ae/Documents/PERSONAL/FAO/BlueCloud/Indicators/nCoV_tracker-master/gtag.html")),
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             "Fisheries Atlas indicators", id="nav",
             tabPanel("Catch time series",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          
                          pickerInput("level_select", "Level:",   
                                      choices = c("Global", "Ocean", "Region"), 
                                      selected = c("Ocean"),
                                      multiple = FALSE),
                          
                          pickerInput("region_select", "Ocean:",   
                                      choices = unique(as.character(catch_ocean$OCEAN)),
                                      options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                      selected = unique(as.character(catch_ocean$OCEAN))[3],
                                      multiple = TRUE), 
                          
                          pickerInput("species_select", "Species:",   
                                      choices = unique(as.character(catch_ocean$SPECIES)),
                                      options = list(`actions-box` = TRUE),
                                      selected = 'All spp',
                                      multiple = FALSE),
                          
                          "Select level, regions, and <= 20 species from drop-down menus to update plots. "
                        ),
                        
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Catch by Region", plotOutput("region_plot"))#,
                            # tabPanel("Catch by Species", plotOutput("species_plot"))
                          )
                        )
                      )
             ),
             
             
             tabPanel("Gear time series",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          
                          pickerInput("gear_level_select", "Level:",   
                                      choices = c("Global", "Ocean", "Region"), 
                                      selected = c("Ocean"),
                                      multiple = FALSE),
                          
                          pickerInput("gear_region_select", "Ocean:",   
                                      choices = unique(as.character(gear_ocean$OCEAN)),
                                      options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                      selected = unique(as.character(gear_ocean$OCEAN))[2],
                                      multiple = FALSE), 
                          
                          pickerInput("gear_select", "Gear:",   
                                      choices = unique(as.character(gear_ocean$GEAR)),
                                      options = list(`actions-box` = TRUE),
                                      selected = 'All gears',
                                      multiple = TRUE),
                          
                          pickerInput("gear_species_select", "Species:",   
                                      choices = unique(as.character(gear_ocean$SPECIES)),
                                      options = list(`actions-box` = TRUE),
                                      selected = 'All spp',
                                      multiple = FALSE),
                          
                          "Select level, region, gear(s) and one or 'All' species from drop-down menus to update plots. "
                        ),
                        
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Catch by Gear", plotOutput("gear_plot"))#,
                            # tabPanel("Catch by Species", plotOutput("species_plot"))
                          )
                        )
                      )
             )
             
  ))





# server
server = function(input, output, session) {
  # data<-callModule(module = QueryInfo, id = "id_1")
  # callModule(module = FlagName,id="name",reactive(data$data))
  # callModule(module = AnimCapt,id="id_2",reactive(data$data))
  # callModule(module = PieSp,id="id_3",reactive(data$data))
  # callModule(module = DataTableWide,id="id_4",reactive(data$data),reactive(data$dsd))
  
  # update region selections
  observeEvent(input$level_select, {
    if (input$level_select=="Global") {
      updatePickerInput(session = session, inputId = "region_select", 
                        choices = unique(as.character(catch_global$GLOBAL)), 
                        selected = unique(as.character(catch_global$GLOBAL))[1])
      updatePickerInput(session = session, inputId = "species_select", 
                        choices = unique(as.character(catch_global$SPECIES)), 
                        selected = 'All spp')
      
    }
    
    if (input$level_select=="Ocean") {
      updatePickerInput(session = session, inputId = "region_select", 
                        choices = unique(as.character(catch_ocean$OCEAN)), 
                        selected = c("Indian"))
      updatePickerInput(session = session, inputId = "species_select", 
                        choices = unique(as.character(catch_ocean$SPECIES[grep(paste0(input$region_select,collapse='|'),catch_ocean$OCEAN)])), 
                        selected = 'All spp')
    }
    
    if (input$level_select=="Region") {
      updatePickerInput(session = session, inputId = "region_select", 
                        choices = unique(as.character(catch_region$REGION)), 
                        selected = unique(as.character(catch_region$REGION))[17:18])
      updatePickerInput(session = session, inputId = "species_select", 
                        choices = unique(as.character(catch_region$SPECIES[grep(paste0(input$region_select,collapse='|'),catch_region$REGION)])), 
                        selected = 'All spp')
    }
  }, ignoreInit = TRUE)
  
  
  # update region selections
  observeEvent(input$gear_level_select, {
    if (input$gear_level_select=="Global") {
      updatePickerInput(session = session, inputId = "gear_region_select", 
                        choices = unique(as.character(gear_global$GLOBAL)), 
                        selected = unique(as.character(gear_global$GLOBAL))[1])
      updatePickerInput(session = session, inputId = "gear_species_select", 
                        choices = unique(as.character(gear_global$SPECIES)), 
                        selected = 'All spp')
      updatePickerInput(session = session, inputId = "gear_select", 
                        choices = unique(as.character(gear_global$GEAR)), 
                        selected = 'All gears')
      
    }
    
    if (input$gear_level_select=="Ocean") {
      updatePickerInput(session = session, inputId = "gear_region_select", 
                        choices = unique(as.character(gear_ocean$OCEAN)), 
                        selected = c("Indian"))
      updatePickerInput(session = session, inputId = "gear_species_select", 
                        choices = unique(as.character(gear_ocean$SPECIES[grep(paste0(input$gear_region_select,collapse='|'),gear_ocean$OCEAN)])), 
                        selected = 'All spp')
      updatePickerInput(session = session, inputId = "gear_select", 
                        choices = unique(as.character(gear_ocean$GEAR)), 
                        selected = 'All gears')
    }
    
    if (input$gear_level_select=="Region") {
      updatePickerInput(session = session, inputId = "gear_region_select", 
                        choices = unique(as.character(gear_region$REGION)), 
                        selected = unique(as.character(gear_region$REGION))[17:18])
      updatePickerInput(session = session, inputId = "gear_species_select", 
                        choices = unique(as.character(gear_region$SPECIES[grep(paste0(input$gear_region_select,collapse='|'),gear_region$REGION)])), 
                        selected = 'All spp')
      updatePickerInput(session = session, inputId = "gear_select", 
                        choices = unique(as.character(gear_region$GEAR)), 
                        selected = 'All gears')
    }
    
    
    
    # if (input$level_select=="Country") {
    #   updatePickerInput(session = session, inputId = "region_select", 
    #                     choices = as.character(cv_today_reduced[order(-cv_today_reduced$cases),]$country), 
    #                     selected = cv_today_reduced$country)
    # }
  }, ignoreInit = TRUE)
  
  # create dataframe with selected region
  region_reactive_db = reactive({
    if (input$level_select=="Global") { 
      db = catch_global
      db$region = db$GLOBAL
      db$species = db$SPECIES
    }
    if (input$level_select=="Ocean") { 
      db = catch_ocean 
      db$region = db$OCEAN
      db$species = db$SPECIES
    }
    if (input$level_select=="Region") { 
      db = catch_region
      db$region = db$REGION
      db$species = db$SPECIES
    }
    
    db %>% filter(region %in% input$region_select & species %in% input$species_select)
  })
  
  gear_reactive_db = reactive({
    if (input$gear_level_select=="Global") { 
      db = gear_global
      db$region = db$GLOBAL
      db$species = db$SPECIES
      db$gear = db$GEAR
    }
    if (input$gear_level_select=="Ocean") { 
      db = gear_ocean 
      db$region = db$OCEAN
      db$species = db$SPECIES
      db$gear = db$GEAR
    }
    if (input$gear_level_select=="Region") { 
      db = gear_region
      db$region = db$REGION
      db$species = db$SPECIES
      db$gear = db$GEAR
    }
    
    db %>% filter(region %in% input$gear_region_select & species %in% input$gear_species_select & gear %in% input$gear_select)
  })
  # region-specific plots
  output$region_plot <- renderPlot({
    catchbyregion_plot(region_reactive_db())
  })
  
  output$gear_plot <- renderPlot({
    catchbygear_plot(gear_reactive_db())
  })
  
  # country-specific plots
  # output$species_plot <- renderPlotly({
  #   catchbyspecies_plot(region_reactive_db())
  # })
  
}

shinyApp(ui, server)
