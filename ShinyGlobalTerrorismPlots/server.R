library(shiny)
library(ggplot2)
library(dplyr)
library(plyr)
library(ggvis)
library(scales)

###############################################################################

CalcTotalByFacet <- function(RiverFacets = "none"){
  #Number of Incidents, Fatalities, and Wounded by Years
  FacetVariables <- "Year"
  if(RiverFacets != "none"){
    FacetVariables <- c(FacetVariables, RiverFacets)
  }
  
  IncidentsByYears <- ddply(GTDdata, FacetVariables, summarise, 
                              total=length(Success))
  FatalitiesByYears <- ddply(GTDdata, FacetVariables, summarise, 
                              total=sum(Fatalities, na.rm = TRUE))
  WoundedByYears <- ddply(GTDdata, FacetVariables, summarise, 
                              total=sum(Wounded, na.rm = TRUE))
  
  VariablesByYear <- list("Incidents" = IncidentsByYears,
                          "Fatalities" = FatalitiesByYears,
                          "Wounded" = WoundedByYears)
}

###############################################################################
#               DATA PROCESSING AND OUTPUT
# Uses the data from GTD and Gapminder and user input to 
#   output the correct graphics.
###############################################################################

# Define server logic 
shinyServer(function(input, output, clientData, session) {
  
  #############################################################################  
  #     SCATTERPLOT CODE
  #############################################################################
  
  # A function to filter the data depending on user's input
  filterDataReactive <- reactive({
    
    #Data to select from GTD
    fixedVariables <- c("Year", "NumCode", "Region")
    
    #Filtering the GTDdata
    filteredGTD <- GTDdata
    
    #Option: Colors
    if(input$ScatterColorby != "none"){
      fixedVariables <- c(fixedVariables, input$ScatterColorby)    
    }
    
    #Option: Facets
    if(input$ScatterFacetby != "none"){
      fixedVariables <- c(fixedVariables, input$ScatterFacetby)
    }
    
    #By years
    filteredGTD <- filteredGTD[filteredGTD$Year >= input$ScatterYear[1] & 
                                 filteredGTD$Year <= input$ScatterYear[2], ]
    
    #By number of incidents
    filteredGTD <- filteredGTD[filteredGTD$NumIncidents >= input$ScatterNincidents, ]
    
    #Option: By attack type
    if(input$ScatterAttack != "all"){
      filteredGTD <- filteredGTD[filteredGTD[["AttackType"]] == input$ScatterAttack, ]
    }
    
    #Option: By target type
    if(input$ScatterTarget != "all"){
      filteredGTD <- filteredGTD[filteredGTD[["TargetType"]] == input$ScatterTarget, ]
    }
    
    #Option: By weapon type
    if(input$ScatterWeapon != "all" ){
      filteredGTD <- filteredGTD[filteredGTD[["WeaponType"]] == input$ScatterWeapon, ]
    }
    
    #Option: By region
    if(input$ScatterRegion != "all"){
      filteredGTD <- filteredGTD[filteredGTD$Region == input$ScatterRegion, ]
    }
    
    #Option: By success
    if(input$ScatterSuccess){
      filteredGTD <- filteredGTD[filteredGTD$Success == "successful", ]
    }
    
    # Make the Global Terrorism Database by Country-Year depending on the
    # y variable
    GTDbyCY <- switch(input$ScatterYaxis,
                      "Incidents" = ddply(filteredGTD, fixedVariables, 
                                          summarise, YvarGTD=length(Success)),
                      "Fatalities" = ddply(filteredGTD, fixedVariables, 
                                           summarise, YvarGTD=sum(Fatalities, na.rm = TRUE)),
                      "Wounded" = ddply(filteredGTD, fixedVariables, 
                                        summarise, YvarGTD=sum(Wounded, na.rm = TRUE)) 
    )
    
    #updateNumericInput(session, "ScatterNumPoints", value=length(GTDbyCY$Year))

    # merge GTD and GM data together to get rid of extra data in GMdata
    merge(GTDbyCY, GMdata, by=c("Year", "NumCode"))
  })
  
  prepareCurrentData <- reactive({
    GTDandGM <- filterDataReactive()
    
    #Selects data relevant to user input and assigns uniform names to that
    # data, so the ggvis functions can all use the same names
    currentData <- data.frame("ID" = paste(GTDandGM$Country, GTDandGM$Year),
                              "Country" = GTDandGM$Country,
                              "Year" = GTDandGM$Year,  
                              "Xvar" = GTDandGM[[input$ScatterXaxis]], 
                              "Yvar" = GTDandGM$YvarGTD)
    
    numPoints <- sum(complete.cases(currentData))
    updateNumericInput(session, "ScatterNumPoints", value=numPoints)
    
    #Option: Colors
    if(input$ScatterColorby != "none"){
      currentData$Color <- GTDandGM[[input$ScatterColorby]]
    }
    
    #Option: Facets
    if(input$ScatterFacetby != "none"){
      currentData$currentFacet <- GTDandGM[[input$ScatterFacetby]]
    }
    
    #Removes missing values
    currentData = currentData[complete.cases(currentData), ]
    
    #Ensures that there is a Y-variable if there is no data, so that ggvis
    # doesn't throw an error
    if(length(currentData$Year) == 0) currentData$Yvar <- currentData$Yvar + 0
    
    #Option: Logarithmic scaling, plus one to avoid having zeros in the data
    if(input$Scatterlogx){
      currentData$Xvar <- log10(currentData$Xvar + 1)
    }
    if(input$Scatterlogy){
      currentData$Yvar <- log10(currentData$Yvar + 1)
    }
    
    currentData
  })

  output$ScatterTitle <- renderText({
    
    if(input$ScatterPlotType == "ggvis"){
      PlotType <- "ggvis"
    } else{
      PlotType <- "ggplot"
    }
    
    paste("<div style='color:#1369bf' align = 'center' >
          Global Terrorism Scatterplot -",
          PlotType,
          "</div>")
  })
  
  #############################################################################  
  #     GGVIS CODE
  #############################################################################
  #Function for showing country-year, x, and y information when hovering over 
  #data points
  make_data_tooltip <- reactive({
    data_tooltip <- function(point) {
      # Purpose: Generates tooltip text
      if (is.null(point)) return(NULL)
      if (is.null(point$ID)) return(NULL)
      
      #Adjusts for logs in displaying the X and Y values of the points
      if(input$Scatterlogx){
        Xval <- round(10 ^ point$Xvar - 1)
      } else {
        Xval <- point$Xvar
      }
      if(input$Scatterlogy){
        Yval <- round(10 ^ point$Yvar - 1)
      } else {
        Yval <- point$Yvar
      }
      
      message <- paste0("<b>", point$ID, "</b><br>",
                 input$ScatterXaxis, "=", Xval, "<br>",
                 input$ScatterYaxis, "=", Yval)
      
      #Option: Color
      if(input$ScatterColorby != "none"){
        message <- paste0(message, "<br>",
                          input$ScatterColorby, ":", point$Color)
      }
      
      message
    }
    data_tooltip
  })
  
  #A reactive expression with the ggvis plot
  vis <- reactive({
    currentData <- prepareCurrentData()
    
    #Preparing titles for axes
    XaxisTitle = input$ScatterXaxis
    if(input$Scatterlogx) XaxisTitle <- paste("Log of", XaxisTitle)
    
    YaxisTitle = input$ScatterYaxis
    if(input$Scatterlogy) YaxisTitle <- paste("Log of", YaxisTitle)
    
    #Creates plot
    currentVis <- currentData %>% 
      ggvis(x = ~Xvar, y = ~Yvar) %>%
      layer_points(fill := "black", size := 50, size.hover := 200,
                   fillOpacity := 1, fillOpacity.hover := 0.5,
                   stroke:= "black", key := ~ID)  %>%
      #Sets titles for axes
      add_axis("x", title = XaxisTitle, format='d', ticks = 3,
               properties = axis_props(title = list(fontSize = 18))) %>%
      add_axis("y", title = YaxisTitle, format='d', ticks = 3,
               properties = axis_props(title = list(fontSize = 18))) %>%  
      add_tooltip(make_data_tooltip(), "hover") %>%
      set_options(width = 400, height = 400) 
  
    #Option: color by
    if(input$ScatterColorby != "none"){
      currentVis <- currentVis %>% 
        scale_ordinal('fill', range = customColors) %>%
        layer_points(fill = ~Color, size := 50, size.hover := 200,
                     fillOpacity := 1, fillOpacity.hover := 0.5,
                     stroke = ~Color, key := ~ID) %>%
        add_legend("fill", title=input$ScatterColorby) %>%
        add_legend("stroke", title=input$ScatterColorby) 
    }

    currentVis
  })

  #Plots the visualization
  bind_shiny(vis, "ggvis1")
  
  #############################################################################  
  #     GGPLOT CODE
  #############################################################################
  
  #ggplot scatterplot
  output$ggplot1 <- renderPlot({   
    currentData <- prepareCurrentData()  
    
    #Preparing titles for axes
    XaxisTitle = input$ScatterXaxis
    if(input$Scatterlogx) XaxisTitle <- paste("Log of", XaxisTitle)
    
    YaxisTitle = input$ScatterYaxis
    if(input$Scatterlogy) YaxisTitle <- paste("Log of", YaxisTitle)
    
    currentPlot <- ggplot(data=currentData, aes(x=Xvar, y=Yvar)) + 
      geom_point(size=4) + theme_bw() +
      xlab(XaxisTitle) + ylab(YaxisTitle) +
      theme(axis.title=element_text(size=18)) +
      scale_x_continuous(labels=comma) + scale_y_continuous(labels=comma)  +
      scale_colour_manual(values=customColors)
    
    #Option: Facets
    if(input$ScatterFacetby != "none" && length(currentData$Year) != 0){
      currentPlot <- currentPlot + facet_wrap(~currentFacet, ncol=4) + 
        theme(legend.position="right", strip.text=element_text(size=18))
    }
    
    #Option: Add color by region
    if(input$ScatterColorby != "none"){
      currentPlot <- currentPlot + aes(colour=Color) +
        scale_fill_manual(values=customColors)
    }

    currentPlot
  })
  
  output$ScatterNotes <- renderText({
    Notes <- paste("<i>* Each point on the graph represents the total number of", 
                   input$ScatterYaxis, 
                   "that occurred in a particular country and year.</i>")
    if(input$ScatterNumPoints == 0){
      Notes <- paste(strong("**There are no data points for the selected"),
                     strong("combination of filters**"),
                     br(),
                     Notes)
    }
    
    Notes
  })
  
  output$ScatterNotes_left <- renderText({
      
    Notes <- paste("<ul>", 
                   tags$li(strong("Number of Points: "), input$ScatterNumPoints),
                   tags$li(strong("X-axis: "), input$ScatterXaxis),
                   tags$li(strong("Y-axis: "), input$ScatterYaxis), 
                   tags$li(strong("Year: "), input$ScatterYear[1], "to", 
                           input$ScatterYear[2]),
                   tags$li(strong("Color By: "), input$ScatterColorby),
                   tags$li(strong("Facet: "), input$ScatterFacetby),
                   "</ul>")
  })
  
  output$ScatterNotes_right <- renderText({
    Notes <- paste("<ul>",
                   tags$li(strong("Region: "), input$ScatterRegion),
                   tags$li(strong("Attack Type: "), input$ScatterAttack),
                   tags$li(strong("Target Type: "), input$ScatterTarget),
                   tags$li(strong("Weapon Type: "), input$ScatterWeapon),
                   tags$li(strong("Minimum # of Incidents: "), input$ScatterNincidents),
                   "</ul>")
  })
  
  #############################################################################  
  #     RIVERPLOT CODE
  #############################################################################
  
  output$RiverPlot <- renderPlot({
    
    #Variable to join GTDOverTime and VariablesByYear (3 dataframes with
    # total number of incidents/fatalities/wounded by year and facet, 
    # if applicable)
    if(input$RiverFacets != "none"){
      joinTotalBy <- c("Year", input$RiverFacets)
    } else {
      joinTotalBy <- "Year"   
    }
    
    #Variables is used in ddply to extract absolute number of incidents, etc.
    Variables = c(joinTotalBy, input$RiverColors)
    
    #GTDOverTime is a dataframe with rows as each possible combination of year, 
    # color variable, facet variable (if applicable), and the corresponding 
    # count of the appropriate variable (incidents, fatalities, wounded) 
    # "absolute" references the column which has the absolute counts of 
    # incidents, fatalities, or wounded
    GTDOverTime <- switch(input$RiverYaxis,
                          "Incidents" = ddply(GTDdata, Variables, 
                             summarise, absolute=length(Success)),
                          "Fatalities" = ddply(GTDdata, Variables, 
                             summarise, absolute=sum(Fatalities, na.rm = TRUE)),
                          "Wounded" = ddply(GTDdata, Variables, 
                             summarise, absolute=sum(Wounded, na.rm = TRUE)))

    #Adding the total number of incidents by year (and facet, if applicable)
    # and calculating the relative values from the total and absolute
    VariablesByYear <- CalcTotalByFacet(input$RiverFacets)
    GTDOverTime <- left_join(GTDOverTime, VariablesByYear[[input$RiverYaxis]], 
                               by=joinTotalBy) 
    GTDOverTime$relative <- GTDOverTime$absolute / GTDOverTime$total
    
    #Renaming columns 
    names(GTDOverTime)[names(GTDOverTime) == input$RiverColors] <- "Color"  
    
    if(input$RiverFacets != "none" &&
         input$RiverColors != input$RiverFacets){
      names(GTDOverTime)[names(GTDOverTime)==input$RiverFacets]<-"currentFacet"
    }
    
    #Filters by years
    GTDOverTime <- GTDOverTime[GTDOverTime$Year >= input$RiverYears[1] &
                               GTDOverTime$Year <= input$RiverYears[2],  ]
    
    #Option: Color By
    if(input$RiverCounts == "P"){
        RiverPlot <- ggplot(data=GTDOverTime, aes(x=Year, y=relative, fill=Color)) +
          geom_area(position = "stack") +
          scale_y_continuous(labels=percent)

      # fix the huge number of y-axis for log
    } else if (input$RiverCounts == "C"){
#       if(input$Riverlogy){
#         GTDOverTime$logabsolute <- log10(GTDOverTime$absolute + 1)
#         GTDOverTime$logcumabsolute <- cumsum()
#         RiverPlot <- ggplot(data=GTDOverTime, aes(x=Year, y=absolute, fill=Color)) +
#           geom_area(position = "dodge") 
#         
# #         RiverPlot <- qplot(Year, absolute, data=GTDOverTime, 
# #                            fill = Color, geom="area")
#       } else {
        RiverPlot <- ggplot(data=GTDOverTime, aes(x=Year, y=absolute, fill=Color)) +
          geom_area(position = "stack") 
#       }
    }

    
   #Option: Facet By
   if(input$RiverFacets != "none"){
      if(input$RiverFacets != input$RiverColors){
        RiverPlot <- RiverPlot + facet_wrap(~currentFacet, ncol=4) 
      } else {
        RiverPlot <- RiverPlot + facet_wrap(~Color, ncol=4)    
      }
   }
   
    RiverPlot <- RiverPlot + 
      theme(legend.position="right", strip.text=element_text(size=18),
            axis.title=element_text(size=18)) + 
      ylab(input$RiverYaxis) +
      scale_fill_manual(values=customColors)
   
#   print(sum(is.na(GTDOverTime)))
   
#    A problem: why there is some grey area? Color by Target Type, Facet by
#         Weapon Type
#    GTDOverTime <- arrange(GTDOverTime, currentFacet)
#    GTDOverTime <- arrange(GTDOverTime, Year)
#    
#    View(GTDOverTime)

    RiverPlot
    
  })

  #############################################################################  
  #     BARPLOT CODE
  #############################################################################

  output$BarPlot <- renderPlot({ 

    #Removing year 1993 as an option
    validate(
      if(input$BarYears == 1993){
        "Year 1993 doesn't have data. Please choose another year."
      }
    )
    
    #Making title for X-axis based on user input
    if(input$BarRangeType == "Below/Above"){
      XaxisTitle <- "Below/Above n"
    } else {
      XaxisTitle <- "Number of"
    }
    XaxisTitle <- paste(XaxisTitle, input$BarYaxis)
    
    #Option: Color by
    if(input$BarColor != "none"){
      GTDbyCountryYear$Color <- GTDbyCountryYear[[input$BarColor]]
    }

    #Setting X variable
    BarYvar <- paste("Num", input$BarYaxis, sep="")
    
    if(input$BarRangeType == "Below/Above"){      
      GTDbyCountryYear$Xvar <- paste(input$BarNValue, "or Fewer", input$BarYaxis)
      AboveX <- GTDbyCountryYear[[BarYvar]] > input$BarNValue
      GTDbyCountryYear$Xvar[AboveX] <- paste(input$BarNValue + 1, "or More", input$BarYaxis)
      
      currentBarPlot <- ggplot(GTDbyCountryYear[GTDbyCountryYear$Year == input$BarYears, ], 
                               aes(factor(Xvar), y = ..count.., fill="#a6cee3")) + 
        geom_bar()
        
    } else {
      GTDbyCountryYear$Xvar <- GTDbyCountryYear[[BarYvar]]
      currentBarPlot <- ggplot(GTDbyCountryYear[GTDbyCountryYear$Year == input$BarYears, ], 
                               aes(x=Xvar, fill="#a6cee3")) + 
           geom_histogram() + geom_rug()
      if(!input$BarCounts) currentBarPlot <- currentBarPlot + 
        coord_cartesian(ylim=c(0, input$BarYlim))
    }
   
    currentBarPlot <- currentBarPlot + xlab(input$BarRangeType) + ylab("Number of Countries") +
      scale_fill_manual(values=customColors) +
      theme(legend.position="none", axis.title=element_text(size=18)) + 
      xlab(XaxisTitle)
  
    #Option: Color by
    if(input$BarColor != "none"){
      currentBarPlot <- currentBarPlot + aes(fill=Color) +
        theme(legend.position="right")
    }
    
    #Option: Puts count and percentage at the top of bars for Below/Above
    # n incidents without color
    if(input$BarColor == "none" & input$BarRangeType == "Below/Above"){
      currentBarPlot <- currentBarPlot + 
        geom_text(aes(y=(..count..), 
                      label = ifelse((..count..)==0,
                                     "", 
                                     paste(..count.., " (",
                                           scales::percent((..count..)/sum(..count..)),
                                           ")", sep = ""))),
                  stat="bin",colour="#1f78b4")
    }
    
    #Option: Change Y-axis to percentage
    if(input$BarCounts){     
        if(input$BarRangeType == "Histogram"){
          currentBarPlot <- currentBarPlot + 
            aes(y = ..count../sum(..count..)) +
            scale_y_continuous(labels = percent_format())
        } else {
          currentBarPlot <- currentBarPlot + 
            aes(y = ..count../sum(..count..)) +
            scale_y_continuous(labels = percent_format())      
        }
        currentBarPlot <- currentBarPlot + ylab("Percentage of Countries")
    } 
    
    currentBarPlot 
  })

  #Self-updating labels
  observe({
    BarYaxisVar <- input$BarYaxis
    
    # Radio group ==============================================
    BarRangeType_options <- list()
    BarRangeType_options[[paste("Below/Above n", BarYaxisVar)]] <- "Below/Above"
    BarRangeType_options[["Histogram"]] <- "Histogram"
    
    # Update text for range options to accomodate for Y-axis variable
    # ex. Change "Incidents" to "Fatalities" in range options label when
    #   Y-axis variable is changed from "Incidents" to "Fatalities"
    updateRadioButtons(session, "BarRangeType", choices = BarRangeType_options,
                       selected=input$BarRangeType)
  })

})