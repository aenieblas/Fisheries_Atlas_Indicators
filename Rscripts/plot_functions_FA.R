# Fisheries Atlas plot functions
# Functions derived from IRDTunaAtlas github : https://github.com/juldebar/IRDTunaAtlas
# Code written by Norbert IRD.


catchbyregion_plot = function(catchts){
  # plot function for Fisheries Atlas, derived from I1 of  IRDTunaAtlas
  # plots time on the x, captures on the y
  # intended for use with global capture production (FishStatJ)
  # df[,c('QUANTITY','YEAR','region','species')]
  
  
  #aggregate values by years and ocean
  aggData <- aggregate(QUANTITY ~ region + YEAR, data=catchts, sum)
  
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

catchbygear_plot<-function(df, 
                      yearAttributeName="YEAR", 
                      speciesAttributeName="SPECIES",
                      gearTypeAttributeName="GEAR",
                      valueAttributeName="QUANTITY",
                      withSparql=TRUE)
{
  if (! require(XML) | ! require(ggplot2) | ! require(RColorBrewer)) {
    stop("Missing library")
  }
  
  if (missing(df)) {
    stop("Input data frame not specified")
  }
  
  #check for input attributes
  if(sum(names(df) == yearAttributeName) == 0) {
    stop("Cannot find year attribute")
  }
  
  if(sum(names(df) == speciesAttributeName) == 0) {
    stop("Cannot find species attribute")
  }
  
  if(sum(names(df) == gearTypeAttributeName) == 0) {
    stop("Cannot find gear attribute")
  }
  
  if(sum(names(df) == valueAttributeName) == 0) {
    stop("Cannot find value attribute")
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
  
  #setup the palette
  nb.cols <- length(unique(df$gear_type))
  my.colors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)
  names(my.colors) <- unique(df$gear_type)
  
  library(RColorBrewer)
  nb.cols <- length(unique(df$gear_type))
  qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  my.colors = sample(unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))),nb.cols)
  names(my.colors) <- unique(df$gear_type)
  # pie(rep(1,n), col=sample(col_vector, n))
  
  #define the result
  result.df <- c()
  
  for (species.current in unique(df$species)) {
    current.df <- df[df$species == species.current,]
    
    #aggregate values by years and gear type
    aggData <- aggregate(value ~ gear_type + year, data=current.df, sum)
    
    #convert values from tons to thousand tons
    aggData$value <- aggData$value / 1000
    
    #order factors levels by value
    aggData$gear_type <- factor(aggData$gear_type, levels=rev(levels(reorder(aggData$gear_type, aggData$value))))
    
    species.label <- species.current
    species.URI <- species.current
    
    # dodge <- position_dodge(width = 0.8)
    print(
      # resultPlot <- ggplot(aggData, aes(x=year, y=value, fill=gear_type, order=gear_type)) +
      #   geom_col(width=1, colour="grey20", show.legend=FALSE,position=position_dodge(width=1)) +
      #   geom_col(width=1,position=position_dodge(width=1)) +
      #   scale_fill_manual(name="Gear type", values=my.colors) +
      #   xlab("Year") + ylab("Catches in thousand tons") +
      #   # ggtitle(paste(species.label, "catches by gear type")) +
      #   theme(legend.position="bottom",aspect.ratio = 1/8)
        # coord_fixed(ratio = 5)#,aspect.ratio = 1/4) #+
        # theme(aspect.ratio = 2/1)
      
      resultPlot <- ggplot(aggData, aes(x=year, y=value,group=gear_type)) +
        geom_area(aes(fill=gear_type), position="stack") +
        geom_line(position="stack", color="grey20") +
        scale_fill_manual(name="Gear", values=my.colors) +
        xlab("Year") + ylab("Catches in thousand tons") +
        theme(legend.position="bottom")
    )
  }
}


i3_gearmonth <- function(df, 
                                            yearAttributeName="year", 
                                            monthAttributeName="month", 
                                            speciesAttributeName="species",
                                            gearTypeAttributeName="gear_code",
                                            valueAttributeName="quantity",
                                            meanPrev5YearsAttributeName="mean_prev_5_years",
                                            stddevPrev5YearsAttributeName="stddev_prev_5_years",
                                            withSparql=TRUE)
{
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
  
  if(sum(names(df) == monthAttributeName) == 0) {
    stop("Cannot found month attribute")
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
  
  if(sum(names(df) == meanPrev5YearsAttributeName) == 0) {
    stop("Cannot found mean for previous years attribute")
  }
  
  if(sum(names(df) == stddevPrev5YearsAttributeName) == 0) {
    stop("Cannot found std_dev for previous years attribute")
  }
  
  #format columns  
  df[, yearAttributeName] <- as.numeric(df[, yearAttributeName])
  df[, monthAttributeName] <- as.numeric(df[, monthAttributeName])
  df[, speciesAttributeName] <- as.factor(df[, speciesAttributeName])
  df[, gearTypeAttributeName] <- as.factor(df[, gearTypeAttributeName])
  df[, valueAttributeName] <- as.numeric(df[, valueAttributeName])
  df[, meanPrev5YearsAttributeName] <- as.numeric(df[, meanPrev5YearsAttributeName])
  df[, stddevPrev5YearsAttributeName] <- as.numeric(df[, stddevPrev5YearsAttributeName])
  
  #rename columns
  names(df)[which(names(df) == yearAttributeName)] <- "year"
  names(df)[which(names(df) == monthAttributeName)] <- "month"  
  names(df)[which(names(df) == speciesAttributeName)] <- "species"
  names(df)[which(names(df) == gearTypeAttributeName)] <- "gear_type"
  names(df)[which(names(df) == valueAttributeName)] <- "value"
  names(df)[which(names(df) == meanPrev5YearsAttributeName)] <- "mean_prev_5_years"
  names(df)[which(names(df) == stddevPrev5YearsAttributeName)] <- "stddev_prev_5_years"
  
  #from std deviation to variance, and root square the sum of variances
  fct <- function(vec)
  {
    var <- vec * vec
    var <- sum(var)
    return(sqrt(var))
  }
  
  #test if FAO usual gear codes are used
  #if (length(intersect(levels(df$gear_type), c("BB", "GILL", "LL", "PS", "OTHER_I", "OTHER_A", "TROL", "TRAP"))) == length(levels(df$gear_type))) {
  #  df$gear_type <- factor(df$gear_type, levels=c("BB", "GILL", "LL", "PS", "OTHER_I", "OTHER_A", "TROL", "TRAP"), labels=c("Baitboat", "Gillnet", "Longline", "Purse seine", "Unclass. art. Indian O.", "Unclass. art. Atl. O.", "Trol.", "Trap"))
  #}
  
  #setup the palette
  # my.colors <- brewer.pal(length(levels(df$gear_type)), "Set1")
  # names(my.colors) <- levels(df$gear_type)
  library(RColorBrewer)
  nb.cols <- length(unique(df$gear_type))
  qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  my.colors = sample(unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))),nb.cols)
  names(my.colors) <- unique(df$gear_type)
  
  #define the result
  result.df <- c()
  
  #for each species
  for (species.current in unique(df$species)) {
    
    #     if (withSparql) {      
    #       #get species scientific name from ecoscope sparql
    #       sparqlResult <- getSpeciesFromEcoscope(as.character(species.current))
    #       if (length(sparqlResult) > 0) {
    #         species.label <- sparqlResult[1,"scientific_name"]
    #         species.URI <- sparqlResult[1,"uri"]
    #       } else {
    #         species.label <- species.current
    #         species.URI <- species.current
    #       } 
    #     } else {
    #       species.label <- species.current
    #       species.URI <- species.current
    #     }
    
    species.label <- species.current
    species.URI <- species.current
    #for each year
    for (year.current in unique(df[df$species == species.current,]$year)) {
      current.df <- df[df$species == species.current & df$year == year.current,]
      
      if (! all(table(current.df$month) == 1)) {
        if (all(is.na(current.df$stddev_prev_5_years))) {
          stddev.agg <- cbind(month=unique(current.df$month), stddev_prev_5_years=NA)
        } else {
          stddev.agg <- aggregate(stddev_prev_5_years ~ month, data=current.df, fct)
        }
        
        if (all(is.na(current.df$mean_prev_5_years))) {
          mean.agg <- cbind(month=unique(current.df$month), mean_prev_5_years=NA)
        } else {
          mean.agg <- aggregate(mean_prev_5_years ~ month, data=current.df, sum)
        }
        
        dfPrev5Years <- merge(mean.agg, stddev.agg)            
        
      } else {
        dfPrev5Years <- current.df
      }
      #order gear factor levels by value
      current.df$gear_type <- factor(current.df$gear_type, levels=rev(levels(reorder(current.df$gear_type, current.df$value))))
      #set proper month label
      current.df$month <- factor(month.abb[current.df$month], levels=levels(reorder(month.abb[current.df$month], current.df$month)))
      dfPrev5Years$month <- factor(month.abb[dfPrev5Years$month], levels=levels(reorder(month.abb[dfPrev5Years$month], dfPrev5Years$month)))
      #build the plot
      print(
      resultPlot <- ggplot() +
        geom_bar(data=current.df,mapping=aes(x=month, y=value, fill=gear_type, order=gear_type),
                       stat="identity")+
        # geom_bar(data=current.df,
        #       mapping=aes(x=month, y=value, fill=gear_type, order=gear_type),
        #       stat="identity") +
        geom_line(data=dfPrev5Years,
              mapping=aes(x=month, y=mean_prev_5_years, group=1),
              stat="identity") +
        # layer(data=dfPrev5Years,
        #       mapping=aes(x=month, ymax=mean_prev_5_years + stddev_prev_5_years, ymin=mean_prev_5_years - stddev_prev_5_years),
        #       width=0.25,
        #       color="dimgray",
        #       stat="identity",
        #       geom="errorbar") +
        scale_fill_manual(name="Gear type", values=my.colors) +
        xlab("Month") + ylab("Catches in tons") + 
        ggtitle(paste(species.label, "monthly catches by gear type on", year.current))
      )
      #draw the plot
      # tempfile.base <- tempfile(pattern=paste("I3_", gsub(" ", "_", species.label), "_", as.character(year.current), "_", sep=""))
      # plot.filepath <- paste(tempfile.base, ".png", sep="")
      # ggsave(filename=plot.filepath, plot=resultPlot, dpi=300)
      # 
      # #       
      # #       #create the RDF metadata
      # rdf.filepath <- paste(tempfile.base, ".rdf", sep="")
      # #       buildRdf(rdf_file_path=rdf.filepath,
      # #                #rdf_subject="http://ecoscope.org/indicatorI3", 
      # #                rdf_subject=paste("http://www.ecoscope.org/ontologies/resources", tempfile.base, sep=""), 
      # #                titles=c("IRD Tuna Atlas: indicator #3 - catches by species for a given year by gear type and by month", 
      # #                         "IRD Atlas thonier : indicateur #3 - captures par espèces pour une année donnée par mois et par type d'engin"),
      # #                descriptions=c(paste(species.label, "catches by gear type and by month on", as.character(year.current)), 
      # #                               paste("Captures de", species.label, "par mois et par type d'engin pour l'année", as.character(year.current))),
      # #                subjects=c(as.character(species.current), as.character(unique(current.df$gear_type))),
      # #                #processes="&localfile;/processI3",
      # #                processes="http://www.ecoscope.org/ontologies/resources/processI3",
      # #                data_output_identifier=plot.filepath,             
      # #                start=as.character(year.current),
      # #                end=as.character(year.current),
      # #                spatial="POLYGON((-180 -90,-180 90,180 90,180 -90,-180 -90))",
      # #                withSparql)
      # 
      # result.df <- rbind(result.df, c(plot.file.path=plot.filepath, rdf.file.path=rdf.filepath))
      
    }
    
    
  }
  
  return(result.df)
}
