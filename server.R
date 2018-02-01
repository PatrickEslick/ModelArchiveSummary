library(shiny)
library(rmarkdown)
library(scales)
library(labeling)
library(ggplot2)
library(car)
library(dataRetrieval)
library(lubridate)
library(smwrQW)
library(smwrStats)
library(XML)
library(DAAG)
library(MASS)
library(grid)
source("regReport_functions.R")

options(shiny.maxRequestSize=30*1024^2)

shinyServer(function(input,output) {
  
  #Display the data file selected
  output$data <- renderTable({
    
    inFile <- input$dataFile
    if(is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, sep=input$sep)
    
  })
  
  #Check if the dates formatted with the given date format are not NA
  output$date_correct <- renderText({
    
    inFile <- input$dataFile
    if(is.null(inFile)) {
      return(" ")
    }
    dates <- read.csv(inFile$datapath, sep=input$sep)
    dates <- dates$datetime
    if(is.na(as.POSIXct(dates[1], format=input$dateformat))) {
      paste("That date format doesn't look quite right")
    } else {
      paste("That date format looks right")
    }
  })
  
  #Display options for selecting dependent variable
  output$d_choices <- renderUI({
    file <- input$dataFile
    
    if(is.null(file)) {
      return(NULL)
    }
    inFile <- read.csv(file$datapath, sep=input$sep)
    variables <- names(inFile)
    radioButtons("dep","Dependent Variable*",variables)
    
  })
  
  #Display options for selecting independent variables
  output$i_choices <- renderUI({
    file <- input$dataFile
    if(is.null(file)) {
      return(NULL)
    }
    inFile <- read.csv(file$datapath, sep=input$sep)
    variables <- names(inFile)
    checkboxGroupInput("ind","Independent Variable*",variables)
    
  })
  
  #Load continuous Q data for the duration curves, if selected
  output$loadQ <- renderUI({
    if(input$durCurve) {
      list(
        fileInput("QFile","Choose Delimited Q File",accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
        selectInput("Qdateformat","Date Format*", c(
          "mm/dd/yyyy hh:mm" = "%m/%d/%Y %H:%M",
          "mm/dd/yyyy hh:mm:ss" = "%m/%d/%Y %H:%M:%S",
          "yyyy-mm-dd hh:mm" = "%Y-%m-%d %H:%M",
          "yyyymmdd hhmm" = "%Y%m%d %H%M",
          "mm/dd/yy hh:mm" = "%m/%d/%y %H:%M",
          "yyyymmddhhmm" = "%Y%m%d%H%M",
          "yyyymmddhhmmss" = "%Y%m%d%H%M%S"))
      )
    } else {
      return(NULL)
    }
  })
  
  #If load conversion factors and units are wanted for the XML output
  output$loadStuff <- renderUI({
    if(input$incLoad) {
      list(
        textInput("load_conversion_factor", "Load Conversion Factor*", "0.002697"),
        textInput("load_units", "Load Units*", "tons per day")
      )
    } else {
      return(NULL)
    }
  })
  
  #Specify a method code, if wanted for XML
  output$methStuff <- renderUI({
    if(input$incMeth) {
      textInput("meth_code", "Method Code:")
    } else {
      return(NULL)
    }
  })
  
  #Download the report using the datafile and options selected
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste(input$dep, "_", input$StationID, sep = "", switch(
        input$format, PDF = '.pdf', HTML = '.html', Word = '.docx'
      ))
    },
    
    content = function(file) {
        
      reportFile <- switch(input$regMethod,
                     OLS = 'olsreport.Rmd',
                     Robust = 'rlmreport.Rmd',
                     Tobit = 'tobitreport.Rmd')
      
      src <- normalizePath(reportFile)
      
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, reportFile)
      
      library(rmarkdown)

      out <- render(reportFile, switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      
      file.rename(out, file)
    }
  )
  
  #Download XML file - little bit of a mess, could use some work, but works
  output$downloadXML <- downloadHandler(
    filename = function() {
      paste("p", input$rpCode, ".xml", sep="")
    },
    content = function(file) {
      station_no <- input$StationID
      pcode_id <- input$pCode
      r_pcode_id <- input$rpCode
      if(input$trans=="Log10") {
        transform_cd <- "log10"
        logtrans <- TRUE
      } else if(input$trans=="ln") { 
        transform_cd <- "ln"
        logtrans <- TRUE
      } else {
        logtrans <- FALSE
        transform_cd <- "none"
      }
      form <- c(input$dep, input$ind)
      formPCodes <- c(input$pCode, strsplit(input$form_i, ";")[[1]])
      dateformat <- input$dateformat
      #DO ANALYSIS TASKS
      
      inFile <- input$dataFile
      Data <- read.csv(inFile$datapath, sep=input$sep)
      rownames(Data) <- NULL
      Data <- na.omit(Data)
      
      attach(Data)
      f <- paste(form[1], "~", form[2], sep = "")
      if(length(form)>2){
        for(i in 3:length(form)){ 
          f <- paste(f, form[i], sep = "+")
        }
      }
      Data <- Data[,!(names(Data) %in% "flag")] #Exclude a "flag" column if one exists
      #Make the model
      g <- lm(f, data=Data)
      lm.stats <- summary.lm(g)
      summ <- summary(g)
      Residuals <- resid(g)
      b.count <- nrow(Data)
      b.rsquared <- summ$r.squared
      if(logtrans==TRUE){
        b.bcf <- mean(10^Residuals)
      } else {
        b.bcf <- 1.0
      }
      b.sigma <- lm.stats$sigma
      
      E.vars <- Data[,form[2:length(form)]]
      dim <- length(E.vars) + 1
      cov.mat <- matrix(data=NA, nrow=dim, ncol=dim)
      
      cov.data<-cbind(1,E.vars)                 #set up data frame for cov matrix
      cov.data<-as.matrix(cov.data)             #convert to matrix data type
      tcov.data<-t(cov.data)                    #transpose matrix
      cov.mat<-tcov.data %*% cov.data           #multiply the matrix by its transpose
      cov.mat<-solve(cov.mat)                   #invert
      #This block is a modified version of that found in the original R analysis file
      #for Chloride at 07144100
      if(nrow(cov.mat)>=2){
        cov.mat[1,2]<-cov.mat[1,2]/((cov.mat[1,1]*cov.mat[2,2])^0.5)}
      if(nrow(cov.mat)>=3){
        cov.mat[1,3]<-cov.mat[1,3]/((cov.mat[1,1]*cov.mat[3,3])^0.5)}
      if(nrow(cov.mat)>=4){
        cov.mat[1,4]<-cov.mat[1,4]/((cov.mat[1,1]*cov.mat[4,4])^0.5)}
      if(nrow(cov.mat)>=5){
        cov.mat[1,5]<-cov.mat[1,5]/((cov.mat[1,1]*cov.mat[5,5])^0.5)}
      if(nrow(cov.mat)>=6){
        cov.mat[1,6]<-cov.mat[1,6]/((cov.mat[1,1]*cov.mat[6,6])^0.5)}
      if(ncol(cov.mat)>=2){
        cov.mat[2,1]<-cov.mat[2,1]/((cov.mat[2,2]*cov.mat[1,1])^0.5)
        if(nrow(cov.mat)>=3){
          cov.mat[2,3]<-cov.mat[2,3]/((cov.mat[2,2]*cov.mat[3,3])^0.5)}
        if(nrow(cov.mat)>=4){
          cov.mat[2,4]<-cov.mat[2,4]/((cov.mat[2,2]*cov.mat[4,4])^0.5)}
        if(nrow(cov.mat)>=5){
          cov.mat[2,5]<-cov.mat[2,5]/((cov.mat[2,2]*cov.mat[5,5])^0.5)}
        if(nrow(cov.mat)>=6){
          cov.mat[2,6]<-cov.mat[2,6]/((cov.mat[2,2]*cov.mat[6,6])^0.5)}
      }
      if(ncol(cov.mat)>=3){
        cov.mat[3,1]<-cov.mat[3,1]/((cov.mat[3,3]*cov.mat[1,1])^0.5)
        cov.mat[3,2]<-cov.mat[3,2]/((cov.mat[3,3]*cov.mat[2,2])^0.5)
        if(nrow(cov.mat)>=4){
          cov.mat[3,4]<-cov.mat[3,4]/((cov.mat[3,3]*cov.mat[4,4])^0.5)}
        if(nrow(cov.mat)>=5){
          cov.mat[3,5]<-cov.mat[3,5]/((cov.mat[3,3]*cov.mat[5,5])^0.5)}
        if(nrow(cov.mat)>=6){
          cov.mat[3,6]<-cov.mat[3,6]/((cov.mat[3,3]*cov.mat[6,6])^0.5)}
      }  
      if(ncol(cov.mat)>=4){
        cov.mat[4,1]<-cov.mat[4,1]/((cov.mat[4,4]*cov.mat[1,1])^0.5)
        cov.mat[4,2]<-cov.mat[4,2]/((cov.mat[4,4]*cov.mat[2,2])^0.5)
        cov.mat[4,3]<-cov.mat[4,3]/((cov.mat[4,4]*cov.mat[3,3])^0.5)
        if(nrow(cov.mat)>=5){
          cov.mat[4,5]<-cov.mat[4,5]/((cov.mat[4,4]*cov.mat[5,5])^0.5)}
        if(nrow(cov.mat)>=6){
          cov.mat[4,6]<-cov.mat[4,6]/((cov.mat[4,4]*cov.mat[6,6])^0.5)}
      }
      if(ncol(cov.mat)>=5){
        cov.mat[5,1]<-cov.mat[5,1]/((cov.mat[5,5]*cov.mat[1,1])^0.5)
        cov.mat[5,2]<-cov.mat[5,2]/((cov.mat[5,5]*cov.mat[2,2])^0.5)
        cov.mat[5,3]<-cov.mat[5,3]/((cov.mat[5,5]*cov.mat[3,3])^0.5)
        cov.mat[5,4]<-cov.mat[5,4]/((cov.mat[5,5]*cov.mat[4,4])^0.5)
        if(nrow(cov.mat)>=6){
          cov.mat[5,6]<-cov.mat[5,6]/((cov.mat[5,5]*cov.mat[6,6])^0.5)}
      }
      if(ncol(cov.mat)>=6){
        cov.mat[6,1]<-cov.mat[6,1]/((cov.mat[6,6]*cov.mat[1,1])^0.5)
        cov.mat[6,2]<-cov.mat[6,2]/((cov.mat[6,6]*cov.mat[2,2])^0.5)
        cov.mat[6,3]<-cov.mat[6,3]/((cov.mat[6,6]*cov.mat[3,3])^0.5)
        cov.mat[6,4]<-cov.mat[6,4]/((cov.mat[6,6]*cov.mat[4,4])^0.5)
        cov.mat[6,5]<-cov.mat[6,5]/((cov.mat[6,6]*cov.mat[5,5])^0.5)  
      }
      if(nrow(cov.mat)>=1){
        cov.mat[1,1]<-1}
      if(nrow(cov.mat)>=2){
        cov.mat[2,2]<-1}
      if(nrow(cov.mat)>=3){
        cov.mat[3,3]<-1}
      if(nrow(cov.mat)>=4){
        cov.mat[4,4]<-1}
      if(nrow(cov.mat)>=5){
        cov.mat[5,5]<-1}
      if(nrow(cov.mat)>=6){
        cov.mat[6,6]<-1}
      row.names(cov.mat)[1] <- "Intercept"
      colnames(cov.mat)[1] <- "Intercept"
      cov.mat <- round(cov.mat, digits=4)
      
      multiple_r2_va <- round(b.rsquared,4)
      bias_corr_factor_va <- round(b.bcf,4)
      num_meas_va <- b.count
      datetimes <- as.POSIXct(Data$datetime, format=dateformat)
      begin_dtRM <- strptime(datetimes[1], format="%Y-%m-%d")
      end_dtRM <- strptime(datetimes[length(datetimes)], format="%Y-%m-%d")
      #Set up the dataframe for explanatory variable information
      names <- c("Intercept", as.character(formPCodes[2:length(formPCodes)]))
      coefficients <- round(g$coefficients,4)
      stderr <- round(lm.stats$coefficients[,"Std. Error"],4)
      transforms <- vector()
      for(var in form[2:length(form)]) {
        if(substr(var,1,3) %in% c("Log", "LOG", "log")) {
          transforms[length(transforms)+1] <- "log10"
        } else if(substr(var,1,3)=="sin"||substr(var,1,3)=="cos") {
          if(substr(var,1,5) %in% c("sin.2", "sin2p")) {
            transforms[length(transforms)+1] <- "sin2pi/365"
          } else if(substr(var,1,5) %in% c("cos.2", "cos2p")) {
            transforms[length(transforms)+1] <- "cos2pi/365"
          } else if(substr(var,1,5) %in% c("sin.4", "sin4p")) {
            transforms[length(transforms)+1] <- "sin4pi/365"
          } else if(substr(var,1,5)%in% c("cos.4", "cos4p")) {
            transforms[length(transforms)+1] <- "cos4pi/365"
          }
        } else if(substr(var,1,2) %in% c("Ln", "LN", "ln")) {
          transforms[length(transforms)+1] <- "ln"
        } else {
          transforms[length(transforms)+1] <- "none"
        }
      }
      transforms <- c("none", transforms)
      is_intercept <- vector()
      for(i in 1:length(names)) {
        if(names[i] == "Intercept") {
          is_intercept[i] <- "Y"
        } else {
          is_intercept[i] <- "N"
        }
      }
      is_day_of_year <- vector()
      for(i in 1:length(names)) {
        if(names[i] == "Day") {
          is_day_of_year[i] <- "Y"
        } else {
          is_day_of_year[i] <- "N"
        }
      }
      term_order <- 1:length(names)
      expVariables <- data.frame(names, coefficients, transforms, stderr, is_intercept, is_day_of_year, term_order)
      line <- vector()
      line[1] <- "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
      line[length(line)+1] <- "<model>"
      line[length(line)+1] <- paste("    <station_no>",station_no,"</station_no>",sep="")
      line[length(line)+1] <- "    <pcode>"
      line[length(line)+1] <- paste("        <pcode_id>",r_pcode_id,"</pcode_id>",sep="")
      line[length(line)+1] <- "    </pcode>"
      line[length(line)+1] <- "    <dataset>"
      if(input$incMeth) {
        line[length(line)+1] <- paste("        <meth_cd>",input$meth_code,"</meth_cd>",sep="")
      }
      line[length(line)+1] <- paste("        <backend_pcode_id_qwdata>",pcode_id,"</backend_pcode_id_qwdata>",sep="")
      if(input$incLoad) {
        line[length(line)+1] <- paste("        <load_conversion_factor_va>",input$load_conversion_factor,"</load_conversion_factor_va>",sep="")
        line[length(line)+1] <- paste("        <load_units_nm>",input$load_units,"</load_units_nm>", sep="")
      }
      line[length(line)+1] <- "    </dataset>"
      line[length(line)+1] <- "    <datasetpart>"
      line[length(line)+1] <- paste("        <begin_dt>","2015-01-01","</begin_dt>",sep="")
      line[length(line)+1] <- "    </datasetpart>"
      line[length(line)+1] <- "    <regressionmodel>"
      line[length(line)+1] <- paste("        <transform_cd>",transform_cd,"</transform_cd>",sep="")
      line[length(line)+1] <- paste("        <multiple_r2_va>",signif(multiple_r2_va,3),"</multiple_r2_va>",sep="")
      line[length(line)+1] <- paste("        <bias_corr_factor_va>",signif(bias_corr_factor_va,3),"</bias_corr_factor_va>",sep="")
      line[length(line)+1] <- paste("        <num_meas_va>",num_meas_va,"</num_meas_va>",sep="")
      line[length(line)+1] <- paste("        <residual_stddev_va>",signif(b.sigma,3),"</residual_stddev_va>",sep="")
      line[length(line)+1] <- paste("        <begin_dt>","2015-01-01","</begin_dt>",sep="")
      line[length(line)+1] <- "    </regressionmodel>"
      for(i in 1:nrow(expVariables)){ #i is the variable number of the main variable
        line[length(line)+1] <- "    <explanatoryvariable>"
        line[length(line)+1] <- paste("        <variable>",as.character(expVariables[i,1]),"</variable>",sep="")
        line[length(line)+1] <- paste("        <explan_coef_va>",as.character(signif(expVariables[i,2],3)),"</explan_coef_va>",sep="")
        line[length(line)+1] <- paste("        <transform_cd>",as.character(expVariables[i,3]),"</transform_cd>",sep="")
        line[length(line)+1] <- paste("        <std_err_va>",as.character(signif(expVariables[i,4],3)),"</std_err_va>",sep="")
        line[length(line)+1] <- paste("        <is_intercept>",as.character(expVariables[i,5]),"</is_intercept>",sep="")
        line[length(line)+1] <- paste("        <is_day_of_year>",as.character(expVariables[i,6]),"</is_day_of_year>",sep="")
        line[length(line)+1] <- paste("        <term_order_va>",as.character(expVariables[i,7]),"</term_order_va>",sep="")
        for(j in i:nrow(expVariables)) {
          line[length(line)+1] <- "        <covariance>"
          line[length(line)+1] <- paste("            <term>",as.character(j),"</term>",sep="")
          line[length(line)+1] <- paste("            <covariance_va>",as.character(cov.mat[i,j]),"</covariance_va>",sep="")
          line[length(line)+1] <- "        </covariance>"
        }
        line[length(line)+1] <- "    </explanatoryvariable>"
      }
      line[length(line)+1] <- "</model>"
      
      #Convert lines to UTF-8
      line <- enc2utf8(line)
      writeLines(line, file, sep="\n", useBytes=TRUE)
    }
  )
  
  output$xmlMessages <- renderText({
    inFile <- input$xmlFile
    if(is.null(inFile))
      return(NULL)
    doc <- xmlParse(inFile$datapath)
    xsd <- xmlSchemaParse("modelloader.xsd")
    validation <- xmlSchemaValidate(xsd, doc)
    if(validation$status == 0) {
      output <- "No errors"
    } else {
      output <- "Some Errors"
    }
    return(output)
  })
  
  output$downloadExample <- downloadHandler(
    filename = function() { paste("ExampleData", ".csv", sep="") },
    content = function(file) {
      write.csv(read.csv("exampleData.csv"), row.names=FALSE, file)
    }
  )
  
  output$c1 <- renderImage({
    return(list(
      src="Images/c1.PNG",
      contentType="image/png",
      alt="c1"
    ))
  }, deleteFile=FALSE)
  
  output$c2 <- renderImage({
    return(list(
      src="Images/c2.PNG",
      contentType="image/png",
      alt="c2"
    ))
  }, deleteFile=FALSE)
  
})