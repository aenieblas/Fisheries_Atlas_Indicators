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
library("jsonlite")



catchbygear_plot = function(df=gearts#,
                            # yearAttributeName="YEAR", 
                            # speciesAttributeName="species",
                            # gearTypeAttributeName="gear",
                            # valueAttributeName="QUANTITY",
                            # withSparql=TRUE
                            ){
  # if (! require(XML) | ! require(ggplot2) | ! require(RColorBrewer)) {
  #   stop("Missing library")
  # }
  # 
  # if (missing(df)) {
  #   stop("Input data frame not specified")
  # }
  # 
  # #check for input attributes
  # if(sum(names(df) == yearAttributeName) == 0) {
  #   stop("Cannot found year attribute")
  # }
  # 
  # if(sum(names(df) == speciesAttributeName) == 0) {
  #   stop("Cannot found species attribute")
  # }
  # 
  # if(sum(names(df) == gearTypeAttributeName) == 0) {
  #   stop("Cannot found gear attribute")
  # }
  # 
  # if(sum(names(df) == valueAttributeName) == 0) {
  #   stop("Cannot found value attribute")
  # }  
  # 
  # #format columns  
  # df[, yearAttributeName] <- as.numeric(df[, yearAttributeName])
  # df[, speciesAttributeName] <- as.factor(df[, speciesAttributeName])
  # df[, gearTypeAttributeName] <- as.factor(df[, gearTypeAttributeName])
  # df[, valueAttributeName] <- as.numeric(df[, valueAttributeName])    
  
  #aggregate to cut other columns
  # df <- aggregate(x=df[, valueAttributeName], 
  #                 by=list(df[, yearAttributeName], df[, speciesAttributeName], df[, gearTypeAttributeName]), 
  #                 FUN=sum)
  
  df <- aggregate(x=df[, 'QUANTITY'], 
                  by=list(df[, 'YEAR'], df[, 'species'], df[, 'gear']), 
                  FUN=sum)
  #rename columns
  names(df) <- c("year", "species", "gear_type", "value")
  
  # for (species.current in unique(df$species)) {
  ## AE : probably don't need this subsetting with reaactive df
  species.current<-unique(df$species)
  # current.df <- df[df$species == species.current,]
  
  #aggregate values by years and gear type
  # aggData <- aggregate(value ~ gear_type + year, data=current.df, sum)
  
  aggData <- df
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


## load data
gear<- read.csv('data/GEAR_TS_ANNUAL.csv')

##  gear data frame manipulation

gear$GLOBAL<-'GLOBAL'

gear_region = subset(gear, !is.na(REGION)) %>% select(c(SPECIES,GEAR,YEAR,QUANTITY, REGION)) %>% group_by(SPECIES,GEAR,REGION, YEAR) %>% summarise_each(funs(sum)) %>% data.frame()
gear_region<-gear_region[order(gear_region$REGION),]

region_all_gear <-subset(gear, !is.na(REGION)) %>% select(c(YEAR,QUANTITY, REGION)) %>% group_by(REGION, YEAR) %>% summarise_each(funs(sum)) %>% data.frame()
region_all_gear$GEAR <- 'All gears'
region_all_gear$SPECIES <- 'All spp'

gear_region<-rbind(gear_region,region_all_gear[,c('SPECIES','GEAR','REGION','YEAR','QUANTITY')])

g<-ggplot(gear_region,aes(x=YEAR,y=QUANTITY))
g<-g+geom_line()+facet_wrap(~REGION)
g

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

# g<-ggplot(gear_ocean,aes(x=YEAR,y=QUANTITY))
# g<-g+geom_line()+facet_wrap(~OCEAN)
# g

gear_global = subset(gear, !is.na(GLOBAL)) %>% select(c(YEAR,QUANTITY, GLOBAL)) %>% group_by(GLOBAL, YEAR) %>% summarise_each(funs(sum)) %>% data.frame()
gear_global$SPECIES<-'All spp'
gear_global$GEAR<-'All gears'

# g<-ggplot(gear_global,aes(x=YEAR,y=QUANTITY))
# g<-g+geom_line()
# g



# ui
ui <- bootstrapPage(
  tags$head(includeHTML("/home/ae/Documents/PERSONAL/FAO/BlueCloud/Indicators/nCoV_tracker-master/gtag.html")),
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             "Fisheries Atlas indicators", id="nav",
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
  observeEvent(input$gear_level_select, {
    if (input$gear_level_select=="Global") {
      updatePickerInput(session = session, inputId = "gear_region_select", 
                        choices = unique(as.character(gear_global$GLOBAL)), 
                        selected = unique(as.character(gear_global$GLOBAL))[1])
      updatePickerInput(session = session, inputId = "gear_species_select", 
                        choices = unique(as.character(gear_global$SPECIES)), 
                        selected = unique(as.character(gear_global$SPECIES))[1])
      updatePickerInput(session = session, inputId = "gear_select", 
                        choices = unique(as.character(gear_global$GEAR)), 
                        selected = unique(as.character(gear_global$GEAR))[1])
    }
    
    if (input$gear_level_select=="Ocean") {
      updatePickerInput(session = session, inputId = "gear_region_select", 
                        choices = unique(as.character(gear_ocean$OCEAN)), 
                        selected = c("Indian"))
      updatePickerInput(session = session, inputId = "gear_species_select", 
                        choices = unique(as.character(gear_ocean$SPECIES[grep(paste0(input$gear_region_select,collapse='|'),gear_ocean$OCEAN)])), 
                        selected = 'All spp')
      updatePickerInput(session = session, inputId = "gear_select", 
                        choices = unique(as.character(gear_ocean$GEAR[grep(paste0(input$gear_region_select,collapse='|'),gear_ocean$OCEAN)])), 
                        selected = 'All gears')
    }
    
    if (input$gear_level_select=="Region") {
      updatePickerInput(session = session, inputId = "gear_region_select", 
                        choices = unique(as.character(gear_region$REGION)), 
                        selected = c("Indian Ocean, Western"))
      updatePickerInput(session = session, inputId = "gear_species_select", 
                        choices = unique(as.character(gear_region$SPECIES[grep(paste0(input$gear_region_select,collapse='|'),gear_region$REGION)])), 
                        selected = 'All spp')
      updatePickerInput(session = session, inputId = "gear_select", 
                        choices = unique(as.character(gear_region$GEAR[grep(paste0(input$gear_region_select,collapse='|'),gear_region$REGION)])), 
                        selected = 'All gears')
    }

  }, ignoreInit = TRUE)
  

  
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

  output$gear_plot <- renderPlot({
    catchbygear_plot(gear_reactive_db())
  })
  

}

shinyApp(ui, server)
