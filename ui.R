library(shiny)

shinyUI(fluidPage(
  
  sidebarLayout(position = "left",
    sidebarPanel(
      fileInput("dataFile","Choose Delimitedd File*",accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
      radioButtons('sep', 'Separator', c(Comma=',', Semicolon=';',Tab='\t'),','),
      textInput("StationID", "USGS Site Number*"),
      textInput("pCode", "Parameter Code (Samples)*"),
      textInput("rpCode", "Parameter Code (Regression)"),
      selectInput("dateformat","Date Format*", c(
        "mm/dd/yyyy hh:mm" = "%m/%d/%Y %H:%M",
        "mm/dd/yyyy hh:mm:ss" = "%m/%d/%Y %H:%M:%S",
        "yyyy-mm-dd hh:mm" = "%Y-%m-%d %H:%M",
        "yyyymmdd hhmm" = "%Y%m%d %H%M",
        "mm/dd/yy hh:mm" = "%m/%d/%y %H:%M",
        "yyyymmddhhmm" = "%Y%m%d%H%M",
        "yyyymmddhhmmss" = "%Y%m%d%H%M%S")),
      verbatimTextOutput("date_correct"),
      helpText(a("Look up a code", href="http://nwis.waterdata.usgs.gov/usa/nwis/pmcodes",target="_blank")),
      checkboxInput("durCurve", "Include flow duration curve (must provide Q data)"),
      uiOutput("loadQ"),
      checkboxInput("incMeth", "Specify a method code"),
      uiOutput("methStuff"),
      checkboxInput("incLoad", "Include load information in XML"),
      uiOutput("loadStuff"),
      uiOutput("d_choices"),
      uiOutput("i_choices"),
      textInput("form_i","Independent Parameter Codes*",value="XXXXX;XXXXX;XXXXX"),
      helpText(a("Look up a code", href="http://nwis.waterdata.usgs.gov/usa/nwis/pmcodes",target="_blank")),
      radioButtons("trans","Dependent Transformation*", choices=c("Log10","ln","None"),inline=TRUE),
      radioButtons("regMethod", "Regression Method*", choices=c("OLS", "Robust"), inline=TRUE),
      helpText("Robust method is experimental"),
      radioButtons('format', 'Document format*', c('HTML', 'Word'), inline = TRUE),
      downloadButton('downloadReport', label="Download"),
      downloadButton('downloadXML', label="Download XML")
    ),
    
    mainPanel(
      fluidRow(
        column(10),
        column(2,
          helpText("Version 1.0")
        )
      ),
      tabsetPanel(
        tabPanel("Help", 
          includeHTML("include.html"),
          downloadButton("downloadExample", "Download sample data"),
          fluidRow(
            column(6,
              imageOutput("c1")
            ),
            column(6,
              imageOutput("c2")       
            )
          )
        ),
        tabPanel("Data",tableOutput("data")),
        tabPanel("Verify XML",
          helpText("Verify that a generated XML file fits the required schema"),
          fileInput("xmlFile","Choose XML File"),
          textOutput("xmlMessages")
        )

      )
    )
  )
))
#