###############################################################################
# Input IDs
# X: "Xaxis"
# Y: "Yaxis"
# Checkbox: 'logx', 'logy'
# Region: "region"
# Target: "target"
# Weapon: "weapon"
# Attack: "attack"
# Year: "year"
# Incidents: "nincidents"
# Success only Checkbox: 'success'
# Color: "colorby"
# Facets: "facetby"
# ggvis: "plot1"
###############################################################################

library(shiny)
library(ggvis)

shinyUI(navbarPage("Global Terrorism Plots",
                   
  #scatterplot
  tabPanel(span("Scatterplot" , style = "color:#1369bf"), fluidPage(
    titlePanel(htmlOutput("ScatterTitle")),
      #div("Global Terrorism Scatterplot - ", style = "color:#1369bf", align = "center")),    
    fluidRow(
      
      # Sidebar tabs
      column(4, tabsetPanel(
        tabPanel("Axes", wellPanel( #RENAME PANEL TAB
          
          #X-axis (GM variables, warnings, log option)
          fluidRow(column(9, selectInput("ScatterXaxis", "X-axis Variable", choices = GMOptions)),
                   column(3, br(), checkboxInput("Scatterlogx", "Log"))),
          conditionalPanel(condition="input.ScatterXaxis=='Labour Rate'",
                           p("Only has data for 1980-2007")),
          conditionalPanel(condition="input.ScatterXaxis=='Unemployment Rate (Female)'",
                           p("Only has data for 1991-2007")),
          conditionalPanel(condition="input.ScatterXaxis=='Electricity per Capita'",
                           p("No data for 2012, 2013")),
          
          #Y-axis (GTD variables, warnings, log option)
          fluidRow(column(9, selectInput("ScatterYaxis", "Y-axis Variable", choices = GTDOptions)),
                   column(3, br(), checkboxInput("Scatterlogy", "Log"))),
          p(span(strong("note:"), style = "color:#1369bf"),
                      "To account for values of 0, \"Log\" is actually log(x+1)"),
                  
          #Type of scatterplot and conditional faceting
          radioButtons("ScatterPlotType", label="Type of Plot", inline=TRUE,
                       choices=c("ggvis" = "ggvis", "ggplot" = "ggplot"),
                       selected="ggvis"),
          conditionalPanel(condition="input.ScatterPlotType == 'ggplot'",
            selectInput("ScatterFacetby", "Facets", 
                        choices = c("None"="none", facetOptions))),
          
          #Color by and Success
          fluidRow(column(6, selectInput("ScatterColorby", "Color By", choices = colorOptions)),
                   column(6, checkboxInput("ScatterSuccess", "Success Only"))),
                  
          #Sliders for year and minimum incidents
          sliderInput("ScatterYear", "Year of Incidents", 1970, 2013, 
                      value = c(2000, 2013), sep=""),
          
          HTML("Additional resources for instructors: <a href='http://web.grinnell.edu/individuals/kuipers/stat2labs/visualization.html'>Stats2Labs</a>")
          )),
        
        # Four Filters
        tabPanel("Filters", wellPanel(
          selectInput("ScatterRegion", "Filter by Region", choices = regionOptions),
          selectInput("ScatterAttack", "Filter by Attack Type", choices = attackOptions),
          selectInput("ScatterTarget", "Filter by Target Type", choices = targetOptions),
          selectInput("ScatterWeapon", "Filter by Weapon Type", choices = weaponOptions),
          sliderInput("ScatterNincidents", "Restrict data to Country-Years that have at
                                            least n incidents",
                      0, 200, 0, step = 5)
        ))
      )),
      
      #Output
      column(8, conditionalPanel(condition="input.ScatterPlotType == 'ggvis'",
                                 ggvisOutput("ggvis1")),
                conditionalPanel(condition="input.ScatterPlotType == 'ggplot'",
                                 plotOutput("ggplot1", height = "500px")),
              htmlOutput("ScatterNotes"), br(),
              wellPanel(
                fluidRow(
                  column(6, htmlOutput("ScatterNotes_left")),
                  column(6, htmlOutput("ScatterNotes_right")))  
              )
            )
    ),
    
    #Stores information on how many data points are being displayed
    conditionalPanel(condition="1==0", 
                     numericInput("ScatterNumPoints", NULL,
                                  min = 0, max = 10000, value=0)
                     )
  )),
  
  #==================
  #Stacked line plot
  #==================
  
  tabPanel(span("Stacked-line Plot", style = "color:#1369bf"), fluidPage(
    titlePanel(div("Global Terrorism Stacked-line Plot", style = "color:#1369bf", align = "center")),
    fluidRow(
      
      #Sidebar
      column(4, wellPanel(
        
        #Y-axis with log optiom (REMOVED)
#       fluidRow(column(9, selectInput("RiverYaxis", "Y-axis Variable", choices = GTDOptions)),
#               column(3, br(), checkboxInput("Riverlogy", "Log"))
#       ),
        
        #Y-axis and percentage/counts
        selectInput("RiverYaxis", "Y-axis Variable", choices = GTDOptions),
        radioButtons("RiverCounts", label=NULL, selected="P",
          choices=c("By Percentage (%)" = "P", "By Count (#)" = "C")),
        
        #Color By, Facet By, and year slider
        selectInput("RiverColors", "Color By", choices=facetOptions),
        selectInput("RiverFacets", "Facet By", 
                    choices= c("None"="none", facetOptions)),
        sliderInput("RiverYears", "Year of Incidents",
                     1970, 2014, value = c(2000, 2014), sep="",
                    animate=TRUE),
        HTML("Additional resources for instructors: <a href='http://web.grinnell.edu/individuals/kuipers/stat2labs/visualization.html'>Stats2Labs</a>")
      )),
      
      #Output
      column(8, plotOutput("RiverPlot", height = "500px"))
    )
  )),

  #Bar Chart
  tabPanel(span("Bar Chart", style = "color:#1369bf"), fluidPage(
    titlePanel(div("Global Terrorism Bar Chart", style = "color:#1369bf", align = "center")),
    fluidRow(
      
      #Sidebar
      column(4, wellPanel(
        
        #Y-variablee for bar chart]
        selectInput("BarYaxis", "Y-axis Variable", choices=GTDOptions),
        
        #Categories for bar chart and corresponding options
        radioButtons("BarRangeType", "Type of range",
                      choices=c("Below/Above n Incidents" = "Below/Above",
                                 "Histogram" = "Histogram")),
        conditionalPanel(condition = "input.BarRangeType == 'Below/Above'",
          sliderInput("BarNValue", "Value for n", 0, 100, value = 0)),
        conditionalPanel(condition = "input.BarRangeType == 'Histogram' &
                                      !input.BarCounts",
          sliderInput("BarYlim", "Set the maximum of y-axis", 5, 200, value = 200,
                      step = 5)),
        
        #Year slider
        sliderInput("BarYears", "Year of Incidents", 1970, 2014, 
                    value = 1970, animate=TRUE, sep=""),
        p(span(strong("note:"), style = "color:#1369bf"), 
            "No graph for Year 1993 due to missing data"),
        
        #Color selection and conditional y-axis percentage option
        selectInput("BarColor", "Color By", choices=colorOptions),
        conditionalPanel(condition="input.BarColor != 'none' |
                        input.BarRangeType == 'Histogram'",
          checkboxInput("BarCounts", "Y-axis as Percent")),
        HTML("Additional resources for instructors: <a href='http://web.grinnell.edu/individuals/kuipers/stat2labs/visualization.html'>Stats2Labs</a>")
      )),
      
      #Output
      column(8, plotOutput("BarPlot", height = "500px"))
    )
  ))
))

