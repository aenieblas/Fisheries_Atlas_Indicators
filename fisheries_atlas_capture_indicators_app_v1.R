## fisheries atlas

## AE improvements - # link to OpenFairViewer queryinfo to get data

# first tab : Catch by Region 
# add plot description using metadata.

# second tab : Effort by Region

#  be sure to include : spawning stock biomass, total landings, fishing mortality, effort and incidental catch of vulnerable and non-target species
# 

if(!require(pacman)) install.packages("pacman", repos = "http://cran.us.r-project.org")
p_load('magrittr','rvest','readxl','dplyr','maps','ggplot2','reshape2','ggiraph','RColorBrewer','leaflet','plotly',
       'shiny','shinyWidgets','shinydashboard','shinythemes','ows4R','sp','readr','DT','jsonlite')

setwd('/home/ae/Documents/PERSONAL/FAO/BlueCloud/Indicators/Fisheries_Atlas_Indicators_Shiny')


######################### data load and manipulation #########################################
## source data vis ows4R - GLOBAL CAPTURES (FISHSTATJ)
# source('Rscripts/fao_capture_flag_dbquery_advanced_20210118121116.R')

## transform the data 
# source('Rscripts/data_support_FishAtlas.R')
# catch<-fishstatj('data/')

# here to read data already transformed. use queryinfo at transformation step (data_support_FishAtlas.R)
## catch data - FISHSTATJ
catch_global<-read.csv('data/catch_global_FISHSTATJ.csv',header=T)
catch_global$YEAR<-as.Date(paste0('01-01-',catch_global$YEAR),format='%d-%m-%Y')

catch_region<-read.csv('data/catch_region_FISHSTATJ.csv',header=T)
catch_region$YEAR<-as.Date(paste0('01-01-',catch_region$YEAR),format='%d-%m-%Y')

catch_ocean<-read.csv('data/catch_ocean_FISHSTATJ.csv',header=T)
catch_ocean$YEAR<-as.Date(paste0('01-01-',catch_ocean$YEAR),format='%d-%m-%Y')

## update to read metadata from openfairviewer
catch_metadata<-NULL
catch_metadata$title<-'FAO - Global capture production - Quantity (1950 - 2018)'
catch_metadata$description<-'This database contains capture production statistics by country or territory,species item,and FAO Major Fishing Area. This GIS product allows to browse sum of capture by countries and year,and allows to select one or more species,and one or more FAO major fishing area(s).'

plot_description<-'Annual capture in thousand tons as reported to the FAO (1950-2018). Captures can be browsed at the global, ocean, or regional level. Data that are displayed on the plot can be downloaded as a csv file.'

## source data vis ows4R - TUNA ATLAS
# source('Rscripts/global_nominal_catch_ird_level0_20210119030430.R')
## annual nominal catch
annomcatch<-read.csv('data/nominal_catch_by_gear_tunaatlas.csv',stringsAsFactors = FALSE)






# effort<-read.csv('data/EFFORT_TS_ANNUAL.csv')
# gear<- read.csv('data/GEAR_TS_ANNUAL.csv')


# plot functions
source('Rscripts/plot_functions_FA.R')



ui <- bootstrapPage(
  tags$head(includeHTML("/home/ae/Documents/PERSONAL/FAO/BlueCloud/Indicators/nCoV_tracker-master/gtag.html")),
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             "Fisheries Atlas capture indicators", id="nav",
             tabPanel("Annual captures",
                      
                      sidebarLayout(
                        sidebarPanel(
                          # "Select level, regions, and either 'All spp' or one species from drop-down menus to update plots. "
                          h5("To update the plot, select level, ocean or regions, and either 'All spp' or one species from drop-down menus. "),
                         selectInput("level_select", "Level:",   
                                      choices = c("Global", "Ocean", "Region"), 
                                      selected = c("Global"),
                                      multiple = FALSE),
                          
                         selectInput("region_select", "Ocean/Region:",   
                                      # choices = unique(as.character(catch_ocean$OCEAN)),
                                      choices=unique(as.character(catch_global$GLOBAL)),
                                      # options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                      # selected = unique(as.character(catch_ocean$OCEAN))[2],
                                      selected = 'GLOBAL',
                                      multiple = TRUE), 
                          
                         selectInput("species_select", "Species:",   
                                      # choices = unique(as.character(catch_ocean$SPECIES)),
                                      choices = unique(as.character(catch_global$SPECIES)),
                                      # options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                      # choices=update_choices(catch_ocean)
                                      # options = list(`actions-box` = TRUE),
                                      selected = 'All spp',
                                      multiple = FALSE),
                         
                         sliderInput("Date",
                                     "Date:",
                                     min = min(catch_global$YEAR,na.rm=T),
                                     max = max(catch_global$YEAR,na.rm=T),
                                     value= c(min(catch_global$YEAR, na.rm=T),max(catch_global$YEAR, na.rm=T)),
                                     timeFormat="%Y"),
                         
                         downloadButton("downloadData", "Download")
                          
                          
                         
                         
                        ),
                        
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Captures by Ocean or Region", plotOutput("region_plot")),
                            h5(plot_description)
                            
                            #,
                            
                            # h5('Annual capture data')
                            # tabPanel("Catch by Species", plotOutput("species_plot"))
                          )
                        )
                      )
             )
  ))





# server
server = function(input, output, session) {
  choice <- NULL
  
  # update region selections
  # observeEvent(input$level_select, {
  observe({
    if (input$level_select=="Global") {
      updateSelectInput(session = session, inputId = "region_select", 
                        choices = unique(as.character(catch_global$GLOBAL)), 
                        selected = unique(as.character(catch_global$GLOBAL))[1])
    }
    
    if (input$level_select=="Ocean") {
      updateSelectInput(session = session, inputId = "region_select", 
                        choices = unique(as.character(catch_ocean$OCEAN)), 
                        selected = c("Atlantic"))
    }
    
    if (input$level_select=="Region") {
      updateSelectInput(session = session, inputId = "region_select", 
                        choices = unique(as.character(catch_region$REGION)), 
                        selected = unique(as.character(catch_region$REGION))[17:18])
    }
  })
  
  
  observe({
    req(input$region_select)
    if (input$level_select=="Global") {

      catch_global2<-catch_global[grep(paste0(input$region_select,collapse='|'),catch_global$GLOBAL),]
      updateSelectInput(session = session, inputId = "species_select",
                        choices = unique(as.character(catch_global2$SPECIES)),
                        selected = 'All spp')
      
    }
    
    if (input$level_select=="Ocean") {
      catch_ocean2<-catch_ocean[grep(paste0(input$region_select,collapse='|'),catch_ocean$OCEAN),]
      updateSelectInput(session = session, inputId = "species_select", 
                        choices = unique(as.character(catch_ocean2$SPECIES)), 
                        selected = 'All spp')
    }
    
    if (input$level_select=="Region") {
      catch_region2<-catch_region[grep(paste0(input$region_select,collapse='|'),catch_region$REGION),]
      updateSelectInput(session = session, inputId = "species_select", 
                        choices = unique(as.character(catch_region2$SPECIES[grep(paste0(input$region_select,collapse='|'),catch_region2$REGION)])), 
                        selected = 'All spp')
    }
  # }, ignoreInit = TRUE)
  })
  
  # create dataframe with selected region
  region_reactive_db = reactive({
    if (input$level_select=="Global") { 
      db = catch_global
      db$region = db$GLOBAL
      db$species = db$SPECIES
      db$quantity = db$QUANTITY
      
    }
    if (input$level_select=="Ocean") { 
      db = catch_ocean 
      db$region = db$OCEAN
      db$species = db$SPECIES
      db$quantity = db$QUANTITY
      
      
    }
    if (input$level_select=="Region") { 
      db = catch_region
      db$region = db$REGION
      db$species = db$SPECIES
      db$quantity = db$QUANTITY
    }
    
    db %>% filter(region %in% input$region_select & species %in% input$species_select )
    
    # db<-db[,c('region','species','quantity')]
  })
  
  
  # region-specific plots
  output$region_plot <- renderPlot({
    selected <- region_reactive_db()[which(region_reactive_db()$YEAR>input$Date[1] & region_reactive_db()$YEAR<input$Date[2]), ]
    catchbyregion_plot(selected)
  })
  output$dto <- renderDataTable({region_reactive_db()})
  
  output$downloadData <- downloadHandler(
    filename = function() {"test.csv"},
    content = function(file) {
      write.csv(region_reactive_db(), file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui, server)
