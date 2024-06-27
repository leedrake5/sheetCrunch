library(pbapply)
library(reshape2)
#library(TTR)
library(dplyr)
library(data.table)
library(shiny)
library(ggplot2)
library(ggtern)
library(random)
library(rhandsontable)
library(random)
library(gghighlight)
library(scales)
library(openxlsx)
#library(gRbase)



options(shiny.maxRequestSize=9000000*1024^40)


shinyServer(function(input, output, session) {
    
    output$filegrab <- renderUI({
        
        if(input$filetype=="CSV") {
            fileInput('file1', 'Choose CSV', multiple=TRUE,
            accept=c(".csv"))
        } else if(input$filetype=="Net") {
            fileInput('file1', 'Choose Net Counts', multiple=TRUE,
            accept=c(".csv"))
        }  else if(input$filetype=="Excel Spreadsheet") {
            fileInput('file1', 'Choose Spreadsheet', multiple=TRUE,
            accept=c(".xlsx"))
        }  else if(input$filetype=="CSV Spreadsheet") {
            fileInput('file1', 'Choose Spreadsheet', multiple=TRUE,
            accept=c(".csv"))
        }  else if(input$filetype=="Artax Excel") {
            fileInput('file1', 'Choose Spreadsheet', multiple=TRUE,
            accept=c(".xlsx"))
        }  else if(input$filetype=="PDZ") {
            fileInput('file1', 'Choose PDZ File', multiple=TRUE,
            accept=c(".pdz"))
        }
        
    })
    
    
    
    output$filegrab2 <- renderUI({
        
        if(input$filetype=="CSV") {
            fileInput('file2', 'Choose CSV', multiple=TRUE,
            accept=c(".csv"))
        } else if(input$filetype=="Net") {
            fileInput('file2', 'Choose Net Counts', multiple=TRUE,
            accept=c(".csv"))
        }  else if(input$filetype=="Excel Spreadsheet") {
            fileInput('file2', 'Choose Spreadsheet', multiple=TRUE,
            accept=c(".xlsx"))
        }  else if(input$filetype=="CSV Spreadsheet") {
            fileInput('file2', 'Choose Spreadsheet', multiple=TRUE,
            accept=c(".csv"))
        }  else if(input$filetype=="Artax Excel") {
            fileInput('file2', 'Choose Spreadsheet', multiple=TRUE,
            accept=c(".xlsx"))
        }  else if(input$filetype=="PDZ") {
            fileInput('file2', 'Choose PDZ File', multiple=TRUE,
            accept=c(".pdz"))
        }
        
    })
    
    
    output$filegrab3 <- renderUI({
        
        if(input$filetype=="CSV") {
            fileInput('file3', 'Choose CSV', multiple=TRUE,
            accept=c(".csv"))
        } else if(input$filetype=="Net") {
            fileInput('file3', 'Choose Net Counts', multiple=TRUE,
            accept=c(".csv"))
        }  else if(input$filetype=="Excel Spreadsheet") {
            fileInput('file3', 'Choose Spreadsheet', multiple=TRUE,
            accept=c(".xlsx"))
        }  else if(input$filetype=="CSV Spreadsheet") {
            fileInput('file3', 'Choose Spreadsheet', multiple=TRUE,
            accept=c(".csv"))
        }  else if(input$filetype=="Artax Excel") {
            fileInput('file3', 'Choose Spreadsheet', multiple=TRUE,
            accept=c(".xlsx"))
        }  else if(input$filetype=="PDZ") {
            fileInput('file3', 'Choose PDZ File', multiple=TRUE,
            accept=c(".pdz"))
        }
        
    })
    
    output$file2gen <- renderUI({
        
        if(input$otherdata==TRUE){
            fileInput('file2', 'Choose Spectra', multiple=TRUE,
            accept=c('text/csv',
            'text/comma-separated-values,text/plain',
            '.csv'))
        } else {
            p()
        }
        
    })
    
    
    output$calfile2gen <- renderUI({
        
        if(input$otherdata==TRUE){
            fileInput('calfileinput2', 'Load Cal File', accept='.quant', multiple=FALSE)
        } else {
            p()
        }
        
    })
    
    
    output$space23gen <- renderUI({
        
        if(input$otherdata==TRUE){
            tags$hr()
        } else {
            p()
        }
        
    })
    
    
    output$file3gen <- renderUI({
        
        if(input$otherdata==TRUE){
            fileInput('file3', 'Choose Spectra', multiple=TRUE,
            accept=c('text/csv',
            'text/comma-separated-values,text/plain',
            '.csv'))
        } else {
            p()
        }
        
    })
    
    
    output$calfile3gen <- renderUI({
        
        if(input$otherdata==TRUE){
            fileInput('calfileinput3', 'Load Cal File', accept='.quant', multiple=FALSE)
        } else {
            p()
        }
        
    })
    
    
    
    output$gainshiftui <- renderUI({
        
        if(input$advanced==TRUE){
            numericInput('gainshift', "Gain Shift (keV)", min=-1, max=1, value=0)
        } else {
            p()
        }
        
    })
    
    
    output$binaryui <- renderUI({
        
        if(input$advanced==TRUE && input$filetype=="PDZ"){
            numericInput('binaryshift', "Binary Shift (bits)", min=0, max=1000, value=0)
        } else {
            p()
        }
        
    })
    
    
    binaryHold <- reactive({
        
        if(input$advanced==TRUE){
            input$binaryshift
        } else if(input$advanced==FALSE){
            500
        }
        
    })
    
    
    gainshiftHold <- reactive({
        
        if(input$advanced==TRUE){
            input$gainshift
        } else if(input$advanced==FALSE){
            0
        }
        
    })

    

dataType <- reactive({
    
    if(input$filetype=="PDZ"){
        "Spectra"
    } else if(input$filetype=="CSV"){
        "Spectra"
    } else if(input$filetype=="Net"){
        "Net"
    } else if(input$filetype=="Excel Spreadsheet"){
        "Spreadsheet"
    } else if(input$filetype=="CSV Spreadsheet"){
        "Spreadsheet"
    } else if (input$filetype=="Artax Excel"){
        "Artax Excel"
    }
    
})


    
    
    fullSpectra1csv <- reactive({
        
        
        withProgress(message = 'Processing Data', value = 0, {
            
            inFile <- input$file1
            if (is.null(inFile)) return(NULL)
            temp = inFile$name
            temp <- gsub(".csv", "", temp)
            id.seq <- seq(1, 2048,1)
            
            n <- length(temp)*id.seq
            
            n.seq <- seq(1, length(inFile$name), 1)
            
            
            data <- pblapply(n.seq, function(x) csvFrame(filepath=inFile$datapath[x], filename=inFile$name[x]))
            data <- as.data.frame(do.call("rbind", data))
            
            
            incProgress(1/n)
            Sys.sleep(0.1)
        })
        
        
        data
        
    })
    
    
    readPDZ1 <- reactive({
        
        withProgress(message = 'Processing Data', value = 0, {
            
            inFile <- input$file1
            if (is.null(inFile)) return(NULL)
            
            n <- length(inFile$datapath)
            names <- inFile$name
            
            if(input$advanced==FALSE){
                myfiles.frame <- as.data.frame(do.call(rbind, lapply(seq(1, n, 1), function(x) readPDZData(filepath=inFile$datapath[x], filename=inFile$name[x]))))
            } else if(input$advanced==TRUE){
                myfiles.frame <- as.data.frame(do.call(rbind, lapply(seq(1, n, 1), function(x) readPDZ25DataManual(filepath=inFile$datapath[x], filename=inFile$name[x], binaryshift=binaryHold()))))
                
            }
            
            incProgress(1/n)
            Sys.sleep(0.1)
        })
        
        #myfiles.frame$Energy <- myfiles.frame$Energy
        
        myfiles.frame
        
        
    })
    
    
    fullSpectra1 <- reactive({
        
        if(input$filetype=="PDZ"){
            readPDZ1()
        } else if(input$filetype=="CSV"){
            fullSpectra1csv()
        }
        
        
    })
    
    fullSpectra <- reactive({
        
        fullSpectra1()
        
    })
    
    
    
    netCounts1 <- reactive({
        
        withProgress(message = 'Processing Data', value = 0, {
            
            
            inFile <- input$file1
            if (is.null(inFile)) return(NULL)
            
            #inName <- inFile$name
            #inPath <- inFile$datapath
            
            #inList <- list(inName, inPath)
            #names(inList) <- c("inName", "inPath")
            
            
            n <- length(inFile$name)
            net.names <- gsub("\\@.*","",inFile$name)
            
            myfiles = pblapply(inFile$datapath,  read_csv_net)
            
            
            myfiles.frame.list <- pblapply(myfiles, data.frame, stringsAsFactors=FALSE)
            nms = unique(unlist(pblapply(myfiles.frame.list, names)))
            myfiles.frame <- as.data.frame(do.call(rbind, lapply(myfiles.frame.list, "[", nms)))
            myfiles.frame <- as.data.frame(sapply(myfiles.frame, as.numeric))
            
            
            #myfiles.frame$Spectrum <- net.names
            
            united.frame <- data.frame(net.names, myfiles.frame)
            colnames(united.frame) <- c("Spectrum", names(myfiles.frame))
            #united.frame$None <- rep(1, length(united.frame$Spectrum))
            
            
            incProgress(1/n)
            Sys.sleep(0.1)
        })
        
        united.frame <- as.data.frame(united.frame)
        united.frame
        
    })
    
    
    artaxExcelData <- reactive({
        
        inFile <- input$file1
        
        if (is.null(inFile)) {return(NULL)}
        
        
        
        proto.fish <- loadWorkbook(file=inFile$datapath)
        just.fish <- readWorkbook(proto.fish, sheet=2)
        colnames(just.fish)[4] <- "Spectrum"

        
        just.fish[,4:length(just.fish)]
        
        
        
    })
    
    ExcelData <- reactive({
        inFile <- input$file1
        if (is.null(inFile)) return(NULL)

        proto.fish <- loadWorkbook(file=inFile$datapath)
        just.fish <- readWorkbook(proto.fish, sheet=1)
        just.fish[,1] <- as.character(just.fish[,1])
        quant.fish <- as.data.frame(just.fish[, sapply(just.fish, is.numeric)])
        qual.fish <- as.data.frame(just.fish[, !sapply(just.fish, is.numeric)])
        
        data.frame(Spectrum=qual.fish[,1], quant.fish)

    })
    
    sheetCSVData <- reactive({
        inFile <- input$file1
        if (is.null(inFile)) return(NULL)

        just.fish <- read.csv(inFile$datapath)
        just.fish[,1] <- as.character(just.fish[,1])
        quant.fish <- as.data.frame(just.fish[, sapply(just.fish, is.numeric)])
        qual.fish <- as.data.frame(just.fish[, !sapply(just.fish, is.numeric)])
        
        data.frame(Spectrum=qual.fish[,1], quant.fish)

    })
    
    qualExcelDataPre <- reactive({
        inFile <- input$file1
        if (is.null(inFile)) return(NULL)

        proto.fish <- loadWorkbook(file=inFile$datapath)
        just.fish <- readWorkbook(proto.fish, sheet=1)
        just.fish[,1] <- as.character(just.fish[,1])
        quant.fish <- as.data.frame(just.fish[, sapply(just.fish, is.numeric)])
        qual.fish <- as.data.frame(just.fish[, !sapply(just.fish, is.numeric)])
        if(length(qual.fish)>1){
            data.frame(Spectrum=qual.fish[,1], qual.fish[,2:length(qual.fish)])
        } else if(length(qual.fish)==1){
            data.frame(Spectrum=qual.fish[,1], Qualitative1=qual.fish[,1])
        }
        
    })
    
    qualExcelData <- reactive({
        
        if(input$filetype=="Excel Spreadsheet"){
            qualExcelDataPre()
        } else if(input$filetype=="CSV Spreadsheet"){
            sheetCSVData()
        }
        
    })
    
    
    fullSpectra2csv <- reactive({
        
        
        withProgress(message = 'Processing Data', value = 0, {
            
            inFile <- input$file2
            if (is.null(inFile)) return(NULL)
            temp = inFile$name
            temp <- gsub(".csv", "", temp)
            id.seq <- seq(1, 2048,1)
            
            n <- length(temp)*id.seq
            
            n.seq <- seq(1, length(inFile$name), 1)
            
            
            data <- pblapply(n.seq, function(x) csvFrame(filepath=inFile$datapath[x], filename=inFile$name[x]))
            data <- do.call("rbind", data)
            
            
            incProgress(1/n)
            Sys.sleep(0.1)
        })
        
        
        data
    })
    
    
    readPDZ2 <- reactive({
        
        withProgress(message = 'Processing Data', value = 0, {
            
            inFile <- input$file2
            if (is.null(inFile)) return(NULL)
            
            n <- length(inFile$datapath)
            names <- inFile$name
            
            myfiles.frame <- as.data.frame(do.call(rbind, lapply(seq(1, n, 1), function(x) readPDZData(filepath=inFile$datapath[x], filename=inFile$name[x]))))
            
            
            incProgress(1/n)
            Sys.sleep(0.1)
        })
        
        myfiles.frame$Energy <- myfiles.frame$Energy
        
        myfiles.frame
        
        
    })
    
    
    fullSpectra2 <- reactive({
        
        if(input$filetype=="PDZ"){
            readPDZ2()
        } else if(input$filetype=="CSV"){
            fullSpectra2csv()
        }
        
    })
    
    netCounts2 <- reactive({
        
        withProgress(message = 'Processing Data', value = 0, {
            
            
            inFile <- input$file2
            if (is.null(inFile)) return(NULL)
            
            #inName <- inFile$name
            #inPath <- inFile$datapath
            
            #inList <- list(inName, inPath)
            #names(inList) <- c("inName", "inPath")
            
            
            n <- length(inFile$name)
            net.names <- gsub("\\@.*","",inFile$name)
            
            myfiles = pblapply(inFile$datapath,  read_csv_net)
            
            
            myfiles.frame.list <- pblapply(myfiles, data.frame, stringsAsFactors=FALSE)
            nms = unique(unlist(pblapply(myfiles.frame.list, names)))
            myfiles.frame <- as.data.frame(do.call(rbind, lapply(myfiles.frame.list, "[", nms)))
            myfiles.frame <- as.data.frame(sapply(myfiles.frame, as.numeric))
            
            
            #myfiles.frame$Spectrum <- net.names
            
            united.frame <- data.frame(net.names, myfiles.frame)
            colnames(united.frame) <- c("Spectrum", names(myfiles.frame))
            #united.frame$None <- rep(1, length(united.frame$Spectrum))
            
            
            incProgress(1/n)
            Sys.sleep(0.1)
        })
        
        united.frame <- as.data.frame(united.frame)
        united.frame
        
    })
    
    

    
    fullSpectra3csv <- reactive({
        
        
        withProgress(message = 'Processing Data', value = 0, {
            
            inFile <- input$file3
            if (is.null(inFile)) return(NULL)
            temp = inFile$name
            temp <- gsub(".csv", "", temp)
            id.seq <- seq(1, 2048,1)
            
            n <- length(temp)*id.seq
            
            n.seq <- seq(1, length(inFile$name), 1)
            
            
            data <- pblapply(n.seq, function(x) csvFrame(filepath=inFile$datapath[x], filename=inFile$name[x]))
            data <- do.call("rbind", data)
            
            
            incProgress(1/n)
            Sys.sleep(0.1)
        })
        
        
        data
    })
    
    readPDZ3 <- reactive({
        
        withProgress(message = 'Processing Data', value = 0, {
            
            inFile <- input$file3
            if (is.null(inFile)) return(NULL)
            
            n <- length(inFile$datapath)
            names <- inFile$name
            
            myfiles.frame <- as.data.frame(do.call(rbind, lapply(seq(1, n, 1), function(x) readPDZData(filepath=inFile$datapath[x], filename=inFile$name[x]))))
            
            
            incProgress(1/n)
            Sys.sleep(0.1)
        })
        
        myfiles.frame$Energy <- myfiles.frame$Energy
        
        myfiles.frame
        
        
    })
    
    fullSpectra3 <- reactive({
        
        if(input$filetype=="PDZ"){
            readPDZ3()
        } else if(input$filetype=="CSV"){
            fullSpectra3csv()
        }
    })
    
    
    
    netCounts3 <- reactive({
        
        withProgress(message = 'Processing Data', value = 0, {
            
            
            inFile <- input$file3
            if (is.null(inFile)) return(NULL)
            
            #inName <- inFile$name
            #inPath <- inFile$datapath
            
            #inList <- list(inName, inPath)
            #names(inList) <- c("inName", "inPath")
            
            
            n <- length(inFile$name)
            net.names <- gsub("\\@.*","",inFile$name)
            
            myfiles = pblapply(inFile$datapath,  read_csv_net)
            
            
            myfiles.frame.list <- pblapply(myfiles, data.frame, stringsAsFactors=FALSE)
            nms = unique(unlist(pblapply(myfiles.frame.list, names)))
            myfiles.frame <- as.data.frame(do.call(rbind, lapply(myfiles.frame.list, "[", nms)))
            myfiles.frame <- as.data.frame(sapply(myfiles.frame, as.numeric))
            
            
            #myfiles.frame$Spectrum <- net.names
            
            united.frame <- data.frame(net.names, myfiles.frame)
            colnames(united.frame) <- c("Spectrum", names(myfiles.frame))
            #united.frame$None <- rep(1, length(united.frame$Spectrum))
            
            
            incProgress(1/n)
            Sys.sleep(0.1)
        })
        
        united.frame <- as.data.frame(united.frame)
        united.frame
        
    })
    
    
    
    
    importSpreadsheet <- reactive({
        qualExcelData()
        
    })
    
    #input$otherdata <- FALSE
    
    #is.null(input$file1)==FALSE
    
    observeEvent(input$actionprocess, {
        
        
        myData1 <- reactive({
            
            data <- if(dataType()=="Spectra"){
                fullSpectra1()
            } else if(input$filetype=="Net"){
                netCounts1()
            } else if(input$filetype=="Excel Spreadsheet"){
                ExcelData()
            } else if(input$filetype=="CSV Spreadsheet"){
                sheetCSVData()
            } else if(input$filetype=="Artax Excel"){
                artaxExcelData()
            }
                
            data
            
            
        })
        
        myData2 <- reactive({
            
            data <- if(dataType()=="Spectra"){
                fullSpectra2()
            } else if(input$filetype=="Net"){
                netCounts2()
            }
            
            data
            
            
        })
        
        myData3 <- reactive({
            
            data <- if(dataType()=="Spectra"){
                fullSpectra3()
            } else if(input$filetype=="Net"){
                netCounts3()
            }
            
            data
            
            
        })
        
        
        

        
        
        calFileContents1 <- reactive({
            
            existingCalFile <- input$calfileinput1
            
            if (is.null(existingCalFile)) return(NULL)
            
            
            Calibration <- readRDS(existingCalFile$datapath)
            
            Calibration
            
        })
        
        
        
        dataCount1 <- reactive({
            inFile <- input$file1
            
            if(input$usecalfile==FALSE){
                length(inFile$datapath)
            }else if(input$usecalfile==TRUE){
                length(calFileContents()$Intensities)
            }
        })
        
        
        
        
        
        
        
        
        
        
        
        
        myValData1 <- reactive({
            
            data <- if(dataType()=="Spectra"){
                fullSpectra1()
            } else if(input$filetype=="Net"){
                netCounts1()
            }
            
            data
            
        })
        
        
        
        
        
        
        
        calValHold1 <- reactive({
            
            
            calFileContents1()[[6]]
            
            
            
            
            
        })
        
        calVariables1 <- reactive({
            
            
            calFileContents1()$Intensities
            
            
            
        })
        
        calValElements1 <- reactive({
            calList <- calValHold1()
            valelements <- ls(calList)
            valelements
        })
        
        calVariableElements1 <- reactive({
            variables <- calVariables1()
            variableelements <- ls(variables)
            variableelements
        })
        
        
        
        
        
        
        tableInputValCounts1 <- reactive({
            valelements <- calValElements1()
            variableelements <- calVariableElements1()
            val.data <- myValData1()
            
            if(dataType()=="Spectra"){spectra.line.list <- lapply(valelements, function(x) elementGrab(element.line=x, data=val.data))}
            if(dataType()=="Spectra"){element.count.list <- lapply(spectra.line.list, '[', 2)}
            
            
            
            if(dataType()=="Spectra"){spectra.line.vector <- as.numeric(unlist(element.count.list))}
            
            if(dataType()=="Spectra"){dim(spectra.line.vector) <- c(length(spectra.line.list[[1]]$Spectrum), length(valelements))}
            
            if(dataType()=="Spectra"){spectra.line.frame <- data.frame(spectra.line.list[[1]]$Spectrum, spectra.line.vector)}
            
            if(dataType()=="Spectra"){colnames(spectra.line.frame) <- c("Spectrum", valelements)}
            
            if(dataType()=="Spectra"){spectra.line.frame <- as.data.frame(spectra.line.frame)}
            
            if(dataType()=="Spectra"){spectra.line.frame}
            
            if(dataType()=="Spectra"){val.line.table <- data.table(spectra.line.frame[, c("Spectrum", valelements), drop = FALSE])}
            
            
            if(input$filetype=="Net"){val.line.table <- val.data[c("Spectrum", valelements), drop=FALSE]}
            
            
            val.line.table
            
            
        })
        
        
        
        fullInputValCounts1 <- reactive({
            valelements <- calValElements1()
            variableelements <- calVariableElements1()
            val.data <- myValData1()
            
            if(dataType()=="Spectra"){spectra.line.list <- lapply(variableelements, function(x) elementGrab(element.line=x, data=val.data))}
            if(dataType()=="Spectra"){element.count.list <- lapply(spectra.line.list, `[`, 2)}
            
            
            if(dataType()=="Spectra"){spectra.line.vector <- as.numeric(unlist(element.count.list))}
            
            if(dataType()=="Spectra"){dim(spectra.line.vector) <- c(length(spectra.line.list[[1]]$Spectrum), length(variableelements))}
            
            if(dataType()=="Spectra"){spectra.line.frame <- data.frame(spectra.line.list[[1]]$Spectrum, spectra.line.vector)}
            
            if(dataType()=="Spectra"){colnames(spectra.line.frame) <- c("Spectrum", variableelements)}
            
            if(dataType()=="Spectra"){spectra.line.frame <- as.data.frame(spectra.line.frame)}
            
            if(dataType()=="Spectra"){val.line.table <- spectra.line.frame[c("Spectrum", variableelements)]}
            
            if(input$filetype=="Net"){val.line.table <- val.data}
            
            val.line.table
        })
        
        
        
        
        
        
        
        tableInputValQuant1 <- reactive({
            
            count.table <- data.frame(fullInputValCounts1())
            the.cal <- calValHold1()
            elements <- calValElements1()
            variables <- calVariableElements1()
            valdata <- myValData1()
            
            
            
            
            
            predicted.list <- pblapply(elements, function (x)
            if(dataType()=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==1){
                predict(
                object=the.cal[[x]][[2]],
                newdata=general.prep(
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x)
                )
            } else if(dataType()=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==2) {
                predict(
                object=the.cal[[x]][[2]],
                newdata=simple.tc.prep(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x
                )
                )
            } else if(dataType()=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==3) {
                predict(
                object=the.cal[[x]][[2]],
                newdata=simple.comp.prep(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
                norm.max=the.cal[[x]][[1]][1]$CalTable$Max
                )
                )
            } else if(dataType()=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==1){
                predict(
                object=the.cal[[x]][[2]],
                newdata=lukas.simp.prep(
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                )
                )
            } else if(dataType()=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==2){
                predict(
                object=the.cal[[x]][[2]],
                newdata=lukas.tc.prep(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                )
                )
            } else if(dataType()=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==3){
                predict(
                object=the.cal[[x]][[2]],
                newdata=lukas.comp.prep(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
                norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
                norm.max=the.cal[[x]][[1]][1]$CalTable$Max
                )
                )
            }else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==1){
                predict(
                object=the.cal[[x]][[2]],
                newdata=general.prep.net(
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x)
                )
            } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==2) {
                predict(
                object=the.cal[[x]][[2]],
                newdata=simple.tc.prep.net(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x
                )
                )
            } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==3) {
                predict(
                object=the.cal[[x]][[2]],
                newdata=simple.comp.prep.net(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
                norm.max=the.cal[[x]][[1]][1]$CalTable$Max
                )
                )
            } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==1){
                predict(
                object=the.cal[[x]][[2]],
                newdata=lukas.simp.prep.net(
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                )
                )
            } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==2){
                predict(
                object=the.cal[[x]][[2]],
                newdata=lukas.tc.prep.net(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                )
                )
            } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==3){
                predict(
                object=the.cal[[x]][[2]],
                newdata=lukas.comp.prep.net(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
                norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
                norm.max=the.cal[[x]][[1]][1]$CalTable$Max
                )
                )
            }
            
            
            
            )
            
            predicted.vector <- unlist(predicted.list)
            
            dim(predicted.vector) <- c(length(count.table$Spectrum), length(elements))
            
            predicted.frame <- data.frame(count.table$Spectrum, predicted.vector)
            
            colnames(predicted.frame) <- c("Spectrum", elements)
            
            #predicted.data.table <- data.table(predicted.frame)
            #predicted.values <- t(predicted.values)
            #predicted.data.table
            predicted.frame
            
        })
        
        
        
        
        
        
        calFileContents2 <- reactive({
            
            existingCalFile <- input$calfileinput2
            
            if (is.null(existingCalFile)) return(NULL)
            
            
            Calibration <- readRDS(existingCalFile$datapath)
            
            Calibration
            
        })
        
        
        
        dataCount2 <- reactive({
            inFile <- input$file2
            
            if(input$usecalfile==FALSE){
                length(inFile$datapath)
            }else if(input$usecalfile==TRUE){
                length(calFileContents2()$Intensities)
            }
        })
        
        
        
        
        
        
        
        
        myValData2 <- reactive({
            
            data <- if(dataType()=="Spectra"){
                fullSpectra2()
            } else if(input$filetype=="Net"){
                netCounts2()
            }
            
            data
            
        })
        
        
        
        
        
        calValHold2 <- reactive({
            
            
            calFileContents2()[[6]]
            
            
            
        })
        
        calVariables2 <- reactive({
            
            
            calFileContents2()$Intensities
            
            
            
        })
        
        calValElements2 <- reactive({
            calList <- calValHold2()
            valelements <- ls(calList)
            valelements
        })
        
        calVariableElements2 <- reactive({
            variables <- calVariables2()
            variableelements <- ls(variables)
            variableelements
        })
        
        
        
        
        
        
        tableInputValCounts2 <- reactive({
            valelements <- calValElements2()
            variableelements <- calVariableElements2()
            val.data <- myValData2()
            
            if(dataType()=="Spectra"){spectra.line.list <- lapply(valelements, function(x) elementGrab(element.line=x, data=val.data))}
            if(dataType()=="Spectra"){element.count.list <- lapply(spectra.line.list, '[', 2)}
            
            
            
            if(dataType()=="Spectra"){spectra.line.vector <- as.numeric(unlist(element.count.list))}
            
            if(dataType()=="Spectra"){dim(spectra.line.vector) <- c(length(spectra.line.list[[1]]$Spectrum), length(valelements))}
            
            if(dataType()=="Spectra"){spectra.line.frame <- data.frame(spectra.line.list[[1]]$Spectrum, spectra.line.vector)}
            
            if(dataType()=="Spectra"){colnames(spectra.line.frame) <- c("Spectrum", valelements)}
            
            if(dataType()=="Spectra"){spectra.line.frame <- as.data.frame(spectra.line.frame)}
            
            if(dataType()=="Spectra"){spectra.line.frame}
            
            if(dataType()=="Spectra"){val.line.table <- data.table(spectra.line.frame[, c("Spectrum", valelements), drop = FALSE])}
            
            
            if(input$filetype=="Net"){val.line.table <- val.data[c("Spectrum", valelements), drop=FALSE]}
            
            
            val.line.table
            
            
        })
        
        
        
        fullInputValCounts2 <- reactive({
            valelements <- calValElements2()
            variableelements <- calVariableElements2()
            val.data <- myValData2()
            
            if(dataType()=="Spectra"){spectra.line.list <- lapply(variableelements, function(x) elementGrab(element.line=x, data=val.data))}
            if(dataType()=="Spectra"){element.count.list <- lapply(spectra.line.list, `[`, 2)}
            
            
            if(dataType()=="Spectra"){spectra.line.vector <- as.numeric(unlist(element.count.list))}
            
            if(dataType()=="Spectra"){dim(spectra.line.vector) <- c(length(spectra.line.list[[1]]$Spectrum), length(variableelements))}
            
            if(dataType()=="Spectra"){spectra.line.frame <- data.frame(spectra.line.list[[1]]$Spectrum, spectra.line.vector)}
            
            if(dataType()=="Spectra"){colnames(spectra.line.frame) <- c("Spectrum", variableelements)}
            
            if(dataType()=="Spectra"){spectra.line.frame <- as.data.frame(spectra.line.frame)}
            
            if(dataType()=="Spectra"){val.line.table <- spectra.line.frame[c("Spectrum", variableelements)]}
            
            if(input$filetype=="Net"){val.line.table <- val.data}
            
            val.line.table
        })
        
        
        
        
        
        
        
        tableInputValQuant2 <- reactive({
            
            count.table <- data.frame(fullInputValCounts2())
            the.cal <- calValHold2()
            elements <- calValElements2()
            variables <- calVariableElements2()
            valdata <- myValData2()
            
            
            
            
            
            predicted.list <- pblapply(elements, function (x)
            if(dataType()=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==1){
                predict(
                object=the.cal[[x]][[2]],
                newdata=general.prep(
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x)
                )
            } else if(dataType()=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==2) {
                predict(
                object=the.cal[[x]][[2]],
                newdata=simple.tc.prep(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x
                )
                )
            } else if(dataType()=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==3) {
                predict(
                object=the.cal[[x]][[2]],
                newdata=simple.comp.prep(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
                norm.max=the.cal[[x]][[1]][1]$CalTable$Max
                )
                )
            } else if(dataType()=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==1){
                predict(
                object=the.cal[[x]][[2]],
                newdata=lukas.simp.prep(
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                )
                )
            } else if(dataType()=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==2){
                predict(
                object=the.cal[[x]][[2]],
                newdata=lukas.tc.prep(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                )
                )
            } else if(dataType()=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==3){
                predict(
                object=the.cal[[x]][[2]],
                newdata=lukas.comp.prep(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
                norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
                norm.max=the.cal[[x]][[1]][1]$CalTable$Max
                )
                )
            }else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==1){
                predict(
                object=the.cal[[x]][[2]],
                newdata=general.prep.net(
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x)
                )
            } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==2) {
                predict(
                object=the.cal[[x]][[2]],
                newdata=simple.tc.prep.net(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x
                )
                )
            } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==3) {
                predict(
                object=the.cal[[x]][[2]],
                newdata=simple.comp.prep.net(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
                norm.max=the.cal[[x]][[1]][1]$CalTable$Max
                )
                )
            } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==1){
                predict(
                object=the.cal[[x]][[2]],
                newdata=lukas.simp.prep.net(
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                )
                )
            } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==2){
                predict(
                object=the.cal[[x]][[2]],
                newdata=lukas.tc.prep.net(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                )
                )
            } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==3){
                predict(
                object=the.cal[[x]][[2]],
                newdata=lukas.comp.prep.net(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
                norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
                norm.max=the.cal[[x]][[1]][1]$CalTable$Max
                )
                )
            }
            
            
            
            )
            
            predicted.vector <- unlist(predicted.list)
            
            dim(predicted.vector) <- c(length(count.table$Spectrum), length(elements))
            
            predicted.frame <- data.frame(count.table$Spectrum, predicted.vector)
            
            colnames(predicted.frame) <- c("Spectrum", elements)
            
            #predicted.data.table <- data.table(predicted.frame)
            #predicted.values <- t(predicted.values)
            #predicted.data.table
            predicted.frame
            
        })
        
        
        

        
        
        calFileContents3 <- reactive({
            
            existingCalFile <- input$calfileinput3
            
            if (is.null(existingCalFile)) return(NULL)
            
            
            Calibration <- readRDS(existingCalFile$datapath)
            
            Calibration
            
        })
        
        
        
        dataCount3 <- reactive({
            inFile <- input$file3
            
            if(input$usecalfile==FALSE){
                length(inFile$datapath)
            }else if(input$usecalfile==TRUE){
                length(calFileContents3()$Intensities)
            }
        })
        
        
        
        
        
        
        
        
        
        
        
        
        myValData3 <- reactive({
            
            data <- if(dataType()=="Spectra"){
                fullSpectra3()
            } else if(input$filetype=="Net"){
                netCounts3()
            }
            
            data
            
        })
        
        
        
        
        
        
        
        
        
        
        calValHold3 <- reactive({
            
            
            calFileContents3()[[6]]
            
            
            
            
            
        })
        
        calVariables3 <- reactive({
            
            
            calFileContents3()$Intensities
            
            
            
        })
        
        calValElements3 <- reactive({
            calList <- calValHold3()
            valelements <- ls(calList)
            valelements
        })
        
        calVariableElements3 <- reactive({
            variables <- calVariables3()
            variableelements <- ls(variables)
            variableelements
        })
        
        
        
        
        
        
        tableInputValCounts3 <- reactive({
            valelements <- calValElements3()
            variableelements <- calVariableElements3()
            val.data <- myValData3()
            
            if(dataType()=="Spectra"){spectra.line.list <- lapply(valelements, function(x) elementGrab(element.line=x, data=val.data))}
            if(dataType()=="Spectra"){element.count.list <- lapply(spectra.line.list, '[', 2)}
            
            
            
            if(dataType()=="Spectra"){spectra.line.vector <- as.numeric(unlist(element.count.list))}
            
            if(dataType()=="Spectra"){dim(spectra.line.vector) <- c(length(spectra.line.list[[1]]$Spectrum), length(valelements))}
            
            if(dataType()=="Spectra"){spectra.line.frame <- data.frame(spectra.line.list[[1]]$Spectrum, spectra.line.vector)}
            
            if(dataType()=="Spectra"){colnames(spectra.line.frame) <- c("Spectrum", valelements)}
            
            if(dataType()=="Spectra"){spectra.line.frame <- as.data.frame(spectra.line.frame)}
            
            if(dataType()=="Spectra"){spectra.line.frame}
            
            if(dataType()=="Spectra"){val.line.table <- data.table(spectra.line.frame[, c("Spectrum", valelements), drop = FALSE])}
            
            
            if(input$filetype=="Net"){val.line.table <- val.data[c("Spectrum", valelements), drop=FALSE]}
            
            
            val.line.table
            
            
        })
        
        
        
        fullInputValCounts3 <- reactive({
            valelements <- calValElements3()
            variableelements <- calVariableElements3()
            val.data <- myValData3()
            
            if(dataType()=="Spectra"){spectra.line.list <- lapply(variableelements, function(x) elementGrab(element.line=x, data=val.data))}
            if(dataType()=="Spectra"){element.count.list <- lapply(spectra.line.list, `[`, 2)}
            
            
            if(dataType()=="Spectra"){spectra.line.vector <- as.numeric(unlist(element.count.list))}
            
            if(dataType()=="Spectra"){dim(spectra.line.vector) <- c(length(spectra.line.list[[1]]$Spectrum), length(variableelements))}
            
            if(dataType()=="Spectra"){spectra.line.frame <- data.frame(spectra.line.list[[1]]$Spectrum, spectra.line.vector)}
            
            if(dataType()=="Spectra"){colnames(spectra.line.frame) <- c("Spectrum", variableelements)}
            
            if(dataType()=="Spectra"){spectra.line.frame <- as.data.frame(spectra.line.frame)}
            
            if(dataType()=="Spectra"){val.line.table <- spectra.line.frame[c("Spectrum", variableelements)]}
            
            if(input$filetype=="Net"){val.line.table <- val.data}
            
            val.line.table
        })
        
        
        
        
        
        
        
        tableInputValQuant3 <- reactive({
            
            count.table <- data.frame(fullInputValCounts3())
            the.cal <- calValHold3()
            elements <- calValElements3()
            variables <- calVariableElements3()
            valdata <- myValData3()
            
            
            
            
            
            predicted.list <- pblapply(elements, function (x)
            if(dataType()=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==1){
                predict(
                object=the.cal[[x]][[2]],
                newdata=general.prep(
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x)
                )
            } else if(dataType()=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==2) {
                predict(
                object=the.cal[[x]][[2]],
                newdata=simple.tc.prep(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x
                )
                )
            } else if(dataType()=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==3) {
                predict(
                object=the.cal[[x]][[2]],
                newdata=simple.comp.prep(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
                norm.max=the.cal[[x]][[1]][1]$CalTable$Max
                )
                )
            } else if(dataType()=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==1){
                predict(
                object=the.cal[[x]][[2]],
                newdata=lukas.simp.prep(
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                )
                )
            } else if(dataType()=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==2){
                predict(
                object=the.cal[[x]][[2]],
                newdata=lukas.tc.prep(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                )
                )
            } else if(dataType()=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==3){
                predict(
                object=the.cal[[x]][[2]],
                newdata=lukas.comp.prep(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
                norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
                norm.max=the.cal[[x]][[1]][1]$CalTable$Max
                )
                )
            }else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==1){
                predict(
                object=the.cal[[x]][[2]],
                newdata=general.prep.net(
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x)
                )
            } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==2) {
                predict(
                object=the.cal[[x]][[2]],
                newdata=simple.tc.prep.net(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x
                )
                )
            } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==3) {
                predict(
                object=the.cal[[x]][[2]],
                newdata=simple.comp.prep.net(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
                norm.max=the.cal[[x]][[1]][1]$CalTable$Max
                )
                )
            } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==1){
                predict(
                object=the.cal[[x]][[2]],
                newdata=lukas.simp.prep.net(
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                )
                )
            } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==2){
                predict(
                object=the.cal[[x]][[2]],
                newdata=lukas.tc.prep.net(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                )
                )
            } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==3){
                predict(
                object=the.cal[[x]][[2]],
                newdata=lukas.comp.prep.net(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
                norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
                norm.max=the.cal[[x]][[1]][1]$CalTable$Max
                )
                )
            }
            
            
            
            )
            
            predicted.vector <- unlist(predicted.list)
            
            dim(predicted.vector) <- c(length(count.table$Spectrum), length(elements))
            
            predicted.frame <- data.frame(count.table$Spectrum, predicted.vector)
            
            colnames(predicted.frame) <- c("Spectrum", elements)
            
            #predicted.data.table <- data.table(predicted.frame)
            #predicted.values <- t(predicted.values)
            #predicted.data.table
            predicted.frame
            
        })
    

    
    
    
    
        
        myData <- reactive({
            
            data <- if(dataType()=="Spectra"){
                fullSpectra()
            } else if(input$filetype=="Net"){
                netCounts()
            }else if(dataType()=="Spreadsheet"){
                importSpreadsheet()
            }
            
            data
            
            
        })
        
        
        calFileContents <- reactive({
            
            existingCalFile <- input$calfileinput
            
            if (is.null(existingCalFile)) return(NULL)
            
            
            Calibration <- readRDS(existingCalFile$datapath)
            
            Calibration
            
        })
        
        
        
        dataCount <- reactive({
            inFile <- input$file1
            
            if(input$usecalfile==FALSE){
                length(inFile$datapath)
            }else if(input$usefile==TRUE){
                length(calFileContents()$Intensities)
            }
        })
        
        
        
        
     





        
        # Return the requested dataset
        datasetInput <- reactive({
            switch(input$element,
            "H.table" = H.table,
            "He.table" = He.table,
            "Li.table" = Li.table,
            "Be.table" = Be.table,
            "B.table" = B.table,
            "C.table" = C.table,
            "N.table" = N.table,
            "O.table" = O.table,
            "F.table" = F.table,
            "Ne.table" = Ne.table,
            "Na.table" = Na.table,
            "Mg.table" = Mg.table,
            "Al.table" = Al.table,
            "Si.table" = Si.table,
            "P.table" = P.table,
            "S.table" = S.table,
            "Cl.table" = Cl.table,
            "Ar.table" = Ar.table,
            "K.table" = K.table,
            "Ca.table" = Ca.table,
            "Sc.table" = Sc.table,
            "Ti.table" = Ti.table,
            "V.table" = V.table,
            "Cr.table" = Cr.table,
            "Mn.table" = Mn.table,
            "Fe.table" = Fe.table,
            "Co.table" = Co.table,
            "Ni.table" = Ni.table,
            "Cu.table" = Cu.table,
            "Zn.table" = Zn.table,
            "Ga.table" = Ga.table,
            "Ge.table" = Ge.table,
            "As.table" = As.table,
            "Se.table" = Se.table,
            "Br.table" = Br.table,
            "Kr.table" = Kr.table,
            "Rb.table" = Rb.table,
            "Sr.table" = Sr.table,
            "Y.table" = Y.table,
            "Zr.table" = Zr.table,
            "Nb.table" = Nb.table,
            "Mo.table" = Mo.table,
            "Tc.table" = Tc.table,
            "Ru.table" = Ru.table,
            "Rh.table" = Rh.table,
            "Pd.table" = Pd.table,
            "Ag.table" = Ag.table,
            "Cd.table" = Cd.table,
            "In.table" = In.table,
            "Sn.table" = Sn.table,
            "Sb.table" = Sb.table,
            "Te.table" = Te.table,
            "I.table" = I.table,
            "Xe.table" = Xe.table,
            "Cs.table" = Cs.table,
            "Ba.table" = Ba.table,
            "La.table" = La.table,
            "Ce.table" = Ce.table,
            "Pr.table" = Pr.table,
            "Nd.table" = Nd.table,
            "Pm.table" = Pm.table,
            "Sm.table" = Sm.table,
            "Eu.table" = Eu.table,
            "Gd.table" = Gd.table,
            "Tb.table" = Tb.table,
            "Dy.table" = Dy.table,
            "Ho.table" = Ho.table,
            "Er.table" = Er.table,
            "Tm.table" = Tm.table,
            "Yb.table" = Yb.table,
            "Lu.table" = Lu.table,
            "Hf.table" = Hf.table,
            "Ta.table" = Ta.table,
            "W.table" = W.table,
            "Re.table" = Re.table,
            "Os.table" = Os.table,
            "Ir.table" = Ir.table,
            "Pt.table" = Pt.table,
            "Au.table" = Au.table,
            "Hg.table" = Hg.table,
            "Tl.table" = Tl.table,
            "Pb.table" = Pb.table,
            "Bi.table" = Bi.table,
            "Po.table" = Po.table,
            "At.table" = At.table,
            "Rn.table" = Rn.table,
            "Fr.table" = Fr.table,
            "Ra.table" = Ra.table,
            "Ac.table" = Ac.table,
            "Th.table" = Th.table,
            "Pa.table" = Pa.table,
            "U.table" = U.table)
        })
        
         observeEvent(input$actionplot, {
        
        # Expression that generates a histogram. The expression is
        # wrapped in a call to renderPlot to indicate that:
        #
        #  1) It is "reactive" and therefore should re-execute automatically
        #     when inputs change
        #  2) Its output type is a plot
        ranges <- reactiveValues(x = NULL, y = NULL)
        
        
        
         plotInput <- reactive({
             


             data <- myData()
             id.seq <- seq(1, 2048,1)
             
             n <- length(data$Energy)
             
             element <- datasetInput()
             intensity.norm <- (element$Intensity/max(element$Intensity))*max(data$CPS)
             intensity.base <- (element$Intensity/max(element$Intensity))
             
             
             
             spectral.plot <- qplot(data$Energy, data$CPS, xlab = "Energy (keV)", ylab = "Counts per Second", geom="line", colour=data$Spectrum) +
             theme_light()+
             theme(legend.position="bottom") +
             geom_segment(aes(x=element$Line, xend=element$Line, y = 0, yend=intensity.norm), colour="grey50", linetype=2)  +
             scale_colour_discrete("Spectrum") +
             coord_cartesian(xlim = ranges$x, ylim = ranges$y)
             
             
             ###Background Subtraction
             # data.n <- as.data.frame(dcast(data=data, formula=Energy~Spectrum, fun.aggregate = sum,value.var = "CPS"))
             
             # background.subtracted <- pbapply(data.n, 2, Hodder.v)
             #background.subtracted <- as.data.frame(background.subtracted)
             # background.subtracted$Energy <- data.n$Energy
             
             # background.melt <- melt(background.subtracted, id="Energy")
             # colnames(background.melt) <- c("Energy", "Spectrum", "CPS")
             
             
             #transformed.spectral.plot <-  qplot(background.melt$Energy+1, SMA(background.melt$CPS, 10), xlab = "Energy (keV)", ylab = "CPS", geom="line", colour=background.melt$Spectrum)+
             # theme_light()+
             # theme(legend.position="bottom") +
             # geom_segment(aes(x=element$Line, xend=element$Line, y = 0, yend=intensity.norm), colour="grey50", linetype=2)  +
             # scale_colour_discrete("Spectrum") +
             # coord_cartesian(xlim = ranges$x, ylim = ranges$y)


# if (input$backgroundsubtract == FALSE) {
#   spectral.plot

#} else if (input$backgroundsubtract == TRUE) {
#   transformed.spectral.plot
# }

spectral.plot
       

         })
         
         
        corrPlot <- reactive({
             
             #data_table <-  myData()[,sapply(myData(), is.numeric)]
             data_table <- select_if(dataMerge(), is.numeric)
             correlations <- cor(data_table, use="pairwise.complete.obs")
             corrplot::corrplot(correlations, method="circle")
             
         })



        output$distPlot <- renderPlot({

            print(corrPlot())


        })
        
        # When a double-click happens, check if there's a brush on the plot.
        # If so, zoom to the brush bounds; if not, reset the zoom.
        observeEvent(input$plot1_dblclick, {
            brush <- input$plot1_brush
            if (!is.null(brush)) {
                ranges$x <- c(brush$xmin*mean(data$Energy), brush$xmax*max(data$Energy))
                ranges$y <- c(brush$ymin*mean(data$CPS), brush$ymax*max(data$CPS))
                
            } else {
                ranges$x <- NULL
                ranges$y <- NULL
            }
            
            
            
        })
        
        output$downloadPlot <- downloadHandler(
        
        plotname <- "SpectraPlot",
        
    
        filename = function() { paste(paste(c(input$projectname, "-", plotname), collapse=''), '.tiff', sep='') },
        content = function(file) {
            ggsave(file,plotInput(), device="tiff", compression="lzw",  dpi=300, width=12, height=7)
        }
        )
        
        
        
         })
        
      
   
    
    
         
         dataMerge <- reactive({
             
             if(dataType()!="Spreadsheet"){
                 
                 first.instrument <- if(dataType()=="Spectra" && input$usecalfile==FALSE){
                     spectra.line.fn(myData1())
                 } else if(input$filetype=="Net" && input$usecalfile==FALSE){
                     myData1()
                 }  else if(dataType()=="Spectra" && input$usecalfile==TRUE) {
                     tableInputValQuant1()
                 } else if(input$filetype=="Net" && input$usecalfile==TRUE){
                     tableInputValQuant1()
                 }
                 
                 if(is.null(input$file2)==FALSE){
                     second.instrument <- if(dataType()=="Spectra" && input$usecalfile==FALSE){
                         spectra.line.fn(myData2())
                     } else if(input$filetype=="Net" && input$usecalfile==FALSE){
                         myData2()
                     } else if(dataType()=="Spectra" && input$usecalfile==TRUE) {
                         tableInputValQuant2()
                     } else if(input$filetype=="Net" && input$usecalfile==TRUE){
                         tableInputValQuant2()
                     }
                 }
                 
                 if(is.null(input$file3)==FALSE){
                     third.instrument <- if(dataType()=="Spectra" && input$usecalfile==FALSE){
                         spectra.line.fn(myData3())
                     } else if(input$filetype=="Net" && input$usecalfile==FALSE){
                         myData3()
                     } else if(dataType()=="Spectra" && input$usecalfile==TRUE) {
                         tableInputValQuant3()
                     } else if(input$filetype=="Net" && input$usecalfile==TRUE){
                         tableInputValQuant3()
                     }
                 }
                 
                 
                 if(is.null(input$file2)==TRUE && is.null(input$file3)==TRUE){
                     first.instrument
                 }else if(is.null(input$file2)==FALSE && is.null(input$file3)==TRUE){
                     merge(first.instrument, second.instrument, all=TRUE)
                 }else if(is.null(input$file2)==FALSE && is.null(input$file3)==FALSE){
                     merge(merge(first.instrument, second.instrument, all=TRUE), third.instrument, all=TRUE)
                 }
             }  else if(dataType()=="Spreadsheet" && input$usecalfile==FALSE){
                 myData1()
             } else if(input$filetype=="Artax Excel" && input$usecalfile==FALSE){
                 myData1()
             }
             
         })
         
         
         
         
         
         lineOptions <- reactive({
             
             spectra.line.table <- dataMerge()[ ,!(colnames(dataMerge()) == "Spectrum")]
             if(input$usecalfile==TRUE){
                 quant.frame <- dataMerge()[ ,!(colnames(dataMerge()) =="Spectrum")]
                 quantified <- colnames(quant.frame)
             }
             
             standard <- if(input$usecalfile==FALSE && dataType()=="Spectra"){
                 spectralLines
             } else if(input$usecalfile==FALSE && input$filetype=="Net"){
                 colnames(spectra.line.table)
             } else if(input$usecalfile==TRUE && dataType()=="Spectra"){
                 quantified
             }else if(input$usecalfile==TRUE && input$filetype=="Net"){
                 quantified
             } else if(dataType()=="Spreadsheet"){
                 colnames(spectra.line.table)
             } else if(input$filetype=="Artax Excel"){
                 colnames(spectra.line.table)
             }
             
         })
         
         defaultLines <- reactive({
             
             spectra.line.table <- dataMerge()
             if(input$usecalfile==TRUE){quantified <- colnames(dataMerge()[ ,!(colnames(dataMerge()) =="Spectrum")])
             }
             
             standard <- if(input$usecalfile==FALSE && dataType()=="Spectra"){
                 defaultVariables()
             } else if(input$usecalfile==FALSE && input$filetype=="Net"){
                 defaultVariables()
             } else if(input$usecalfile==TRUE && dataType()=="Spectra"){
                 quantified
             }else if(input$usecalfile==TRUE && input$filetype=="Net"){
                 quantified
             } else if(dataType()=="Spreadsheet"){
                 colnames(spectra.line.table[ ,!(colnames(spectra.line.table) == "Spectrum")])
             }
             
         })
         
         
         optionLines <- reactive({
             
             if(input$clusterlearn==FALSE){
                 defaultLines()
             } else if(input$clusterlearn==TRUE){
                 theFish()
             }
             
         })
         
         
         output$defaultlines <- renderUI({
             
             
             checkboxGroupInput('show_vars', 'Elemental lines to show:',
             choices=lineOptions(), selected = optionLines())
         })
         


 tableInput <- reactive({
     spectra.line.table <- dataMerge()
     select.line.table <- datatable(spectra.line.table[, c("Spectrum", input$show_vars), drop = FALSE])
     select.line.table
 })


  output$mytable1 <- renderDataTable({
   
  tableInput()

  })
  

  
  
  hotableInput <- reactive({
      
      spectra.line.table <- dataMerge()
      spectra.line.vector <- spectra.line.table$Spectrum
      
      spectra.line.vector <- if(is.null(spectra.line.vector)==FALSE){
          spectra.line.table$Spectrum
      } else if(is.null(spectra.line.vector)==TRUE){
          seq(1, 5, 1)
      }
      
       n <- length(spectra.line.vector)
      
      lin.vector <- seq(from = 1, to = n, by = 1)


      na.vector <- rep("HOLD", n)
     
      
      
      empty.line.table <- data.frame(spectra.line.vector, na.vector, na.vector, na.vector, na.vector, na.vector, na.vector)
      colnames(empty.line.table) <- c("Spectrum", "Qualitative1", "Qualitative2", "Qualitative3", "Qualitative4", "Qualitative5", "Qualitative6")
      empty.line.table$Quantitative <- lin.vector
      

      
      
      if(dataType()=="Spreadsheet" && length(qualExcelData())>=2){empty.line.table$Qualitative1 <- qualExcelData()[,2]}
           if(dataType()=="Spreadsheet" && length(qualExcelData())>=3){empty.line.table$Qualitative2 <- qualExcelData()[,3]}
                     if(dataType()=="Spreadsheet" && length(qualExcelData())>=4){empty.line.table$Qualitative3 <- qualExcelData()[,4]}
                        if(dataType()=="Spreadsheet" && length(qualExcelData())>=5){empty.line.table$Qualitative4 <- qualExcelData()[,5]}
                            if(dataType()=="Spreadsheet" && length(qualExcelData())>=6){empty.line.table$Qualitative5 <- qualExcelData()[,6]}
                                if(dataType()=="Spreadsheet" && length(qualExcelData())>=7){empty.line.table$Qualitative6 <- qualExcelData()[,7]}
     
     new.line.table <- if(dataType()=="Spreadsheet"){
         merge(qualExcelData(), empty.line.table, by="Spectrum")
     } else if(dataType()!="Spreadsheet"){
         empty.line.table
     }

      
      new.line.table
      
  })
  
  values <- reactiveValues()
  
  observe({
      if (!is.null(input$hot)) {
          DF = hot_to_r(input$hot)
      } else {
          if (is.null(values[["DF"]]))
          DF <- hotableInput()
          else
          DF <- values[["DF"]]
      }
      values[["DF"]] <- DF
  })

  
  ## Handsontable
  
  output$hot <- renderRHandsontable({
      DF <- values[["DF"]]
      if (!is.null(DF))
      rhandsontable(DF, useTypes = FALSE, stretchH = "all")
  })
  
  
  

  qualitativeSelect1 <- reactive({
      
      qualitative.data <- values[["DF"]]
      qual.options <- unique(qualitative.data$Qualitative1)
      qual.options
      
  })
  
  qualitativeSelect2 <- reactive({
      
      qualitative.data <- values[["DF"]]
      qual.options <- unique(qualitative.data$Qualitative2)
      qual.options
      
  })
  
  qualitativeSelect3 <- reactive({
      
      qualitative.data <- values[["DF"]]
      qual.options <- unique(qualitative.data$Qualitative3)
      qual.options
      
  })
  
  qualitativeSelect4 <- reactive({
      
      qualitative.data <- values[["DF"]]
      qual.options <- unique(qualitative.data$Qualitative4)
      qual.options
      
  })
  
  qualitativeSelect5 <- reactive({
      
      qualitative.data <- values[["DF"]]
      qual.options <- unique(qualitative.data$Qualitative5)
      qual.options
      
  })
  
  qualitativeSelect6 <- reactive({
      
      qualitative.data <- values[["DF"]]
      qual.options <- unique(qualitative.data$Qualitative6)
      qual.options
      
  })
  
  
  output$qualSelect1a <- renderUI({
      
      
      checkboxGroupInput('qual_select1a', 'Select',
      choices=qualitativeSelect1(), selected = qualitativeSelect1())
  })
  
  output$qualSelect2a <- renderUI({
      
      
      checkboxGroupInput('qual_select2a', 'Select',
      choices=qualitativeSelect2(), selected = qualitativeSelect2())
  })
  
  output$qualSelect3a <- renderUI({
      
      
      checkboxGroupInput('qual_select3a', 'Select',
      choices=qualitativeSelect3(), selected = qualitativeSelect3())
  })
  
  output$qualSelect4a <- renderUI({
      
      
      checkboxGroupInput('qual_select4a', 'Select',
      choices=qualitativeSelect4(), selected = qualitativeSelect4())
  })
  
  
  output$qualSelect5a <- renderUI({
      
      
      checkboxGroupInput('qual_select5a', 'Select',
      choices=qualitativeSelect5(), selected = qualitativeSelect5())
  })
  
  output$qualSelect6a <- renderUI({
      
      
      checkboxGroupInput('qual_select6a', 'Select',
      choices=qualitativeSelect6(), selected = qualitativeSelect6())
  })
  
  
  output$qualSelect1b <- renderUI({
      
      
      checkboxGroupInput('qual_select1b', 'Select',
      choices=qualitativeSelect1(), selected = NULL)
  })
  
  output$qualSelect2b <- renderUI({
      
      
      checkboxGroupInput('qual_select2b', 'Select',
      choices=qualitativeSelect2(), selected = NULL)
  })
  
  output$qualSelect3b <- renderUI({
      
      
      checkboxGroupInput('qual_select3b', 'Select',
      choices=qualitativeSelect3(), selected = NULL)
  })
  
  output$qualSelect4b <- renderUI({
      
      
      checkboxGroupInput('qual_select4b', 'Select',
      choices=qualitativeSelect4(), selected = NULL)
  })
  
  
  output$qualSelect5b <- renderUI({
      
      
      checkboxGroupInput('qual_select5b', 'Select',
      choices=qualitativeSelect5(), selected = NULL)
  })
  
  output$qualSelect6b <- renderUI({
      
      
      checkboxGroupInput('qual_select6b', 'Select',
      choices=qualitativeSelect6(), selected = NULL)
  })

  dataMerge0 <- reactive({
      
      spectra.line.table <- dataMerge()
      quality.table <- values[["DF"]]

      
      merge(spectra.line.table, quality.table, by="Spectrum")
  })


dataMerge1a <- reactive({
    
    spectra.line.table <- dataMerge0()
    
    
    filter(spectra.line.table,
    Qualitative1 %in% input$qual_select1a,
    Qualitative2 %in% input$qual_select2a,
    Qualitative3 %in% input$qual_select3a,
    Qualitative4 %in% input$qual_select4a,
    Qualitative5 %in% input$qual_select5a,
    Qualitative6 %in% input$qual_select6a
    )
    
})

dataMerge1b <- reactive({
    
        spectra.line.table <- dataMerge0()
    
    
    
    
    filter(spectra.line.table,
    Qualitative1 %in% input$qual_select1b,
    Qualitative2 %in% input$qual_select2b,
    Qualitative3 %in% input$qual_select3b,
    Qualitative4 %in% input$qual_select4b,
    Qualitative5 %in% input$qual_select5b,
    Qualitative6 %in% input$qual_select6b
    )
    
})

mergedHold <- reactive({
    
    #merged.table <- merge(dataMerge1a(), dataMerge1b(), all=TRUE)
        
    if(input$usefull==FALSE){
        c(as.vector(dataMerge1a()$Spectrum),  as.vector(dataMerge1b()$Spectrum))
    } else if(input$usefull==TRUE){
        dataMerge()$Spectrum
    }
    
})

output$clipsubsetfinal <- renderUI({
    
    checkboxGroupInput("show_rows", label="Choose Samples", choices=mergedHold(), selected=mergedHold())
    
})


dataMerge2 <- reactive({
    
    merged.table.first <-  dataMerge0()
    
    merged.table <- merged.table.first[merged.table.first$Spectrum %in% mergedHold(),]
        
    
    
    #merge(dataMerge1a(), dataMerge1b(), all=TRUE)
    
    filter(merged.table,
    Spectrum %in% input$show_rows
    )
    

    
})





dataMerge1aTableOutput <- reactive({
    spectra.line.table <- dataMerge1a()
    select.line.table <- datatable(spectra.line.table)
    select.line.table
})

output$mydatamerge1a <- renderDataTable({
    
    dataMerge1aTableOutput()
    
})

output$downloadsubseta <- downloadHandler(
filename = function() { paste(paste(c(input$projectname, "_", "SubsetA"), collapse=''), '.csv', sep=',') },
content = function(file
) {
    write.csv(dataMerge1a(), file)
}
)



dataMerge1bTableOutput <- reactive({
    spectra.line.table <- dataMerge1b()
    select.line.table <- datatable(spectra.line.table)
    select.line.table
})

output$mydatamerge1b <- renderDataTable({
    
    dataMerge1bTableOutput()
    
})

output$downloadsubsetb <- downloadHandler(
filename = function() { paste(paste(c(input$projectname, "_", "SubsetB"), collapse=''), '.csv', sep=',') },
content = function(file
) {
    write.csv(dataMerge1b(), file)
}
)



dataMerge2TableOutput <- reactive({
    spectra.line.table <- dataMerge2()
    select.line.table <- datatable(spectra.line.table)
    select.line.table
})

output$mydatamerge2 <- renderDataTable({
    
    dataMerge2TableOutput()
    
})

output$downloadsubsetfinal <- downloadHandler(
filename = function() { paste(paste(c(input$projectname, "_", "Subset"), collapse=''), '.csv', sep=',') },
content = function(file
) {
    write.csv(dataMerge2(), file)
}
)


dataMerge3 <- reactive({
    
    spectra.line.table <- if(input$usefull==FALSE){
        dataMerge2()
    } else if(input$usefull==TRUE){
        dataMerge()
    }
    
    quality.table <- values[["DF"]]

    spectra.line.table <- merge(spectra.line.table[,c("Spectrum", input$show_vars),], quality.table, by="Spectrum")
    
    #spectra.line.table <- spectra.line.table[complete.cases(spectra.line.table[,input$show_vars]),]
    
    
    
    #spectra.line.table <- spectra.line.table[complete.cases(spectra.line.table),]
    
    if(input$logtrans==TRUE){spectra.line.table[,input$show_vars] <- log(spectra.line.table[,input$show_vars]+10)}

    spectra.line.table
    
})



qualityTable <- reactive({
    spectra.line.table <- dataMerge2()
    quality.table <- values[["DF"]]
    
    partial.table <- spectra.line.table[,c("Spectrum", input$show_vars)]

    
    full.table <- merge(x=partial.table, y=quality.table, by.x="Spectrum", by.y="Spectrum")
    full.table <- full.table[complete.cases(full.table),]
    
    full.table[,ls(quality.table)]
    
})



choiceLines <- reactive({
    
    spectra.line.table <- dataMerge2()
    
    standard <- if(dataType()=="Spectra"){
        colnames(spectra.line.table[ ,!(colnames(spectra.line.table) == "Spectrum")])
    } else if(input$filetype=="Net"){
        colnames(spectra.line.table[ ,!(colnames(spectra.line.table) == "Spectrum")])
    } else if(dataType()=="Spreadsheet"){
        colnames(spectra.line.table[ ,!(colnames(spectra.line.table) == "Spectrum")])
    }
    
})
  

  
  output$downloadData <- downloadHandler(
  filename = function() { paste(c(input$projectname, "_", "CountTable", ".xlsx"), collapse='') },
  content = function(file
  ) {
      openxlsx::write.xlsx(dataMerge3(), file)
  }
  )
  
  

  #####PCA Analysis
  
  xrfKReactive <- reactive({
      
      spectra.line.table <- dataMerge3()
     
      xrf.pca.frame <- spectra.line.table[,input$show_vars]
      xrf.pca.frame <- xrf.pca.frame[complete.cases(xrf.pca.frame),]
      #xrf.pca.frame <- as.data.frame(sapply( xrf.pca.frame, as.numeric ))

      
      xrf.k <- kmeans(xrf.pca.frame, centers=input$knum, iter.max=1000, nstart=500, algorithm=c("Hartigan-Wong"), trace=TRUE)
      xrf.pca <- prcomp(xrf.pca.frame, scale.=FALSE)
      
      xrf.scores <- as.data.frame(xrf.pca$x)
      
      cluster.frame <- data.frame(spectra.line.table$Spectrum, xrf.k$cluster, xrf.scores)
      
      colnames(cluster.frame) <- c("Assay", "Cluster", names(xrf.scores))
      
      cluster.frame



  })
  
  
  findPeaks <- reactive({
      
      
      
      
  })
  
  
  ####Choose Clusters
  ####Identify best variables
  defaultVariables <- reactive({
      
      inFile <- input$file1
      
      elements <- if(input$filetype=="PDZ") {
          c("Ca.K.alpha", "Ti.K.alpha", "Fe.K.alpha")
      } else if(input$filetype=="CSV"){
              hold <- read.csv(inFile$datapath[[1]])
              voltage <- as.numeric(as.vector(hold[11,1]))
              if(voltage<25){
                  accepted.spec.light
              }else{
                  accepted.spec.trace
              }
          } else if(input$filetype=="Net"){
              hold <- read.csv(inFile$datapath[[1]])
              hold.k <- subset(hold, Line=="K12")
              hold.med <- median(hold.k$Energy.keV)
              if(hold.med<=5){
                  accepted.net.light
              } else if(!(hold.med < 5 | hold.med > 7)){
                  accepted.net.combined
              } else if(hold.med >= 7){
                  accepted.net.trace
              }
          } else if(input$filetype=="Artax Excel"){
              proto.fish <- loadWorkbook(file=inFile$datapath)
              just.fish <- readWorkbook(proto.fish, sheet=1)
              voltage <- as.numeric(just.fish[4,2])
              if(voltage<25){
                  accepted.net.light
              }else{
                  accepted.net.trace
              }
          } else if(dataType()=="Spreadsheet"){
              lineOptions()
          }

      
      elements
      
      
  })
  
  output$nvariablesui <- renderUI({
      
      if(input$clusterlearn==TRUE){
          numericInput("nvariables", label = "# Elements", min=2, max=length(defaultVariables()), value=2)
      } else if(input$clusterlearn==FALSE){
          p()
      }
      
  })
  
  
  output$usesubsetui <- renderUI({
      
      if(input$clusterlearn==TRUE){
          checkboxInput("usesubset", label="Use Subset", value=FALSE)
      } else if(input$clusterlearn==FALSE){
          p()
      }
      
  })
  
  # thanksForAllTheFish <- reactive({
  #
  #    spectra.line.table <- dataMerge()
  #
  #    if(input$filetype!="Spreadsheet"){
  #        elements <- as.vector(intersect(defaultVariables(), colnames(spectra.line.table[,-1])))
  #    } else if(dataType()=="Spreadsheet"){
  #        elements <- colnames(spectra.line.table[,-1])
  #    }
  #
      #    combos_mod <- function(a.vector){
      #
      #   so <- seq(from=2, to=input$nvariables, by=1)
      #
      #   long <- pblapply(so, function(x) combnPrim(x=a.vector, m=x))
      #   and <- pblapply(long, function(x) plyr::alply(x, 2))
      #   thanks.for.all.the.fish <- do.call(list, unlist(and, recursive=FALSE))
      #
      #   thanks.for.all.the.fish
      #
      #}
      
      #thanks.for.all.the.fish <- combos_mod(elements)
      
      #list.of.elbows <- pbapply::pblapply(thanks.for.all.the.fish, function(x) optimal_k_chain(spectra.line.table[,x]))
      #names(list.of.elbows) <- seq(1, length(list.of.elbows), 1)
      #frame.of.elbows <- do.call("rbind", list.of.elbows)
      #result <- frame.of.elbows[which.max(frame.of.elbows$percent),]
      #best.choice <- thanks.for.all.the.fish[[as.numeric(rownames(result))]]
      #
      #
      #})
  
  
  fishVector <- reactive({
      
      spectra.line.table <- if(input$usesubset==FALSE){
          dataMerge()
      } else if(input$usesubset==TRUE){
          as.data.frame(dataMerge2()[, sapply(dataMerge2(), is.numeric)])
      }
      

      if(input$filetype!="Spreadsheet"){
          elements <- as.vector(intersect(defaultVariables(), colnames(spectra.line.table)))
      } else if(dataType()=="Spreadsheet"){
          elements <- colnames(spectra.line.table)
      }
      
      combos_mod <- function(a.vector){
          
          so <- seq(from=2, to=input$nvariables, by=1)
          
          long <- pblapply(so, function(x) combnPrim(x=a.vector, m=x), cl=14L)
          and <- pblapply(long, function(x) plyr::alply(x, 2), cl=14L)
          thanks.for.all.the.fish <- do.call(list, unlist(and, recursive=FALSE))
          
          thanks.for.all.the.fish
          
      }
      
      combos_mod(elements)
      
  })
  
  thanksForAllTheFish <- reactive({
      
      spectra.line.table <- if(input$usesubset==FALSE){
          dataMerge()
      } else if(input$usesubset==TRUE){
          as.data.frame(dataMerge2()[, sapply(dataMerge2(), is.numeric)])
      }
      

      
      if(input$filetype!="Spreadsheet"){
          elements <- as.vector(intersect(defaultVariables(), colnames(spectra.line.table)))
      } else if(dataType()=="Spreadsheet"){
          elements <- colnames(spectra.line.table)
      }
      
      thanks.for.all.the.fish <- fishVector()
      
      
      thanks.for.all.the.length <- lapply(thanks.for.all.the.fish, `length<-`, max(lengths(thanks.for.all.the.fish)))
      names(thanks.for.all.the.length) <- seq(1, length(thanks.for.all.the.length), 1)
      
      frame.of.thanks <- as.data.frame(do.call("rbind", thanks.for.all.the.length))
      
      list.of.elbows <- pbapply::pblapply(thanks.for.all.the.fish, function(x) optimal_k_chain(spectra.line.table[,x]), cl=14L)
      names(list.of.elbows) <- seq(1, length(list.of.elbows), 1)
      frame.of.elbows <- as.data.frame(do.call("rbind", list.of.elbows))
      cbind(frame.of.elbows, frame.of.thanks)
      
      
  })
  
  output$thanksforallthefish <- renderDataTable({
      
      data.table(thanksForAllTheFish())
      
  })
  
  
  output$thanksforallthefishtable <- downloadHandler(
  filename = function() { paste(paste(c(input$projectname, "_", "MCLTable"), collapse=''), '.csv', sep='') },
  content = function(file
  ) {
      write.csv(thanksForAllTheFish(), file)
  }
  )
  
  theFish <- reactive({
      
      thanks.for.all.the.fish <- fishVector()
      
      frame.of.elbows <- thanksForAllTheFish()
      
      result <- frame.of.elbows[which.max(frame.of.elbows$percent),]
      best.choice <- thanks.for.all.the.fish[[as.numeric(rownames(result))]]
      best.choice
      
  })
  
  
  ###Optimal Clusters
  
  optimalK <- reactive({
      
      
      spectra.line.table <- dataMerge()
      
      n <- if(nrow(spectra.line.table)<30){
          nrow(spectra.line.table)-5
      } else {
          30
      }
      
      
      xrf.pca.frame <- spectra.line.table[,input$show_vars]
      xrf.pca.frame <- xrf.pca.frame[complete.cases(xrf.pca.frame),]
      
      wss <- (nrow(xrf.pca.frame)-1)*sum(apply(xrf.pca.frame,2,var))
      for (i in 2:n) wss[i] <- sum(kmeans(xrf.pca.frame,
      centers=i)$withinss)
      
      data.frame(
      clustercount=seq(1, n, 1),
      wss=wss)
      
  })
  
  output$wsstable <- downloadHandler(
  filename = function() { paste(paste(c(input$projectname, "_", "WSSTable"), collapse=''), '.csv', sep=',') },
  content = function(file
  ) {
      write.csv(optimalK(), file)
  }
  )
  
  
  
  
  screeCrunch <- reactive({
      
      wss.frame <- optimalK()
      
      best.choice <- scree_crunch(dataframe=wss.frame, dependent="wss", independent="clustercount")
      
      best.choice
      
  })
  
  output$knumui <- renderUI({
      
      numericInput("knum", label = "K-Means", value=2)
      
  })
  
  
  optimalKplot <- reactive({
      
      wss.frame <- optimalK()
      
      
      ggplot(wss.frame, aes(clustercount, wss)) +
      geom_line() +
      geom_point() +
      geom_point(data=wss.frame[screeCrunch(), ], aes(clustercount, wss), size=3) +
      geom_point(data=wss.frame[screeCrunch(), ], aes(clustercount, wss), pch=1, size=6) +
      scale_x_continuous("Number of Clusters") +
      scale_y_continuous("Within Groups Sum of Squares", labels=comma) +
      theme_light()
      
  })
  
  
  
  
  output$optimalkplot <- renderPlot({
      optimalKplot()
      
  })
  
  
  output$hover_infooptimalk <- renderUI({
      
      point.table <- optimalK()
      
      hover <- input$plot_hoveroptimalk
      point <- nearPoints(point.table,  coordinfo=hover,   threshold = 5, maxpoints = 1, addDist = TRUE)
      if (nrow(point) == 0) return(NULL)
      
      
      
      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      
      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      # actual tooltip created as wellPanel
      wellPanel(
      style = style,
      p(HTML(paste0("<b> Spectrum: </b>", point$Spectrum, "<br/>",
      "<b> Cluster Count: </b>", point$clustercount, "<br/>",
      "<b> WSS: </b>", point$wss, "<br/>",
      "<b> Percent: </b>", percent(round(1-point$wss/max(point.table$wss), 2)), "<br/>"
      
      )))
      )
  })
  

  
  xrfPCAReactive <- reactive({
      
      spectra.line.table <- dataMerge3()
      
     

      
      xrf.clusters <- xrfKReactive()
      
      element.counts <- spectra.line.table[,input$show_vars]

      
      
      
      xrf.pca.results <- data.frame(xrf.clusters, element.counts)
      
      
      xrf.pca.results
  })
  
  qualChoices <- reactive({
      c(names(values[["DF"]]), "Cluster")
  })
  
  output$pcacolourui <- renderUI({
      
      selectInput("pcacolour", "Colour", choices=c(qualChoices(), "Focus"), selected="Cluster")
      
  })
  

  
  
  output$pcaFocusVariable <- renderUI({
      
      if(input$pcacolour=="Focus"){selectInput('pcafocusvariable', "Choose Variable", choices=qualChoices(), selected="Qualitative1")} else {
          p()
      }
      
  })
  
  output$pcaFocusUI <- renderUI({
      
      if(input$pcacolour=="Focus"){selectInput('pcafocuschoice', "Choose Focus", choices=unique(values[["DF"]][input$pcafocusvariable]), selected="Qualitative1", multiple=TRUE)} else {
          p()
      }
      
  })
  
  output$pcaFocusLabel <- renderUI({
      if(input$pcacolour=="Focus"){selectInput('pcafocuslabel', "Choose Label", choices=c("None", names(values[["DF"]])), selected="None")} else {
          p()
      }
      
  })
  
  
  clusterFrame <- reactive({
      
          spectra.line.table <- dataMerge3()
          

          
      xrf.pca.results <- xrfKReactive()
      
      xrf.k <- xrfKReactive()
      
      quality.table <-qualityTable()
      
      colour.table <- data.frame(xrf.k$Cluster, spectra.line.table)
      colnames(colour.table) <- c("Cluster", names(spectra.line.table))
      
      
      
      
      unique.spec <- seq(1, length(colour.table$Spectrum), 1)
      null <- rep(1, length(unique.spec))
      
      spectra.line.table$Cluster <- xrf.k$Cluster
      spectra.line.table$PC1 <- xrf.k$PC1
      spectra.line.table$PC2 <- xrf.k$PC2
      
      spectra.line.table
      
  })
  
  
  plotInput2color <- reactive({
      
      spectra.line.table <- clusterFrame()
      
      color.plot <- if(input$elipseplot1==FALSE){
          ggplot(data= spectra.line.table) +
          geom_point(aes(PC1, PC2, colour=as.factor(spectra.line.table[,input$pcacolour]), shape=as.factor(spectra.line.table[,input$pcacolour])), size = input$spotsize+1) +
          geom_point(aes(PC1, PC2), colour="grey30", size=input$spotsize-2) +
          coord_cartesian(xlim = rangespca$x, ylim = rangespca$y, expand = TRUE) +
          scale_x_continuous("Principle Component 1") +
          scale_y_continuous("Principle Component 2") +
          theme_light() +
          theme(axis.text.x = element_text(size=15)) +
          theme(axis.text.y = element_text(size=15)) +
          theme(axis.title.x = element_text(size=15)) +
          theme(axis.title.y = element_text(size=15, angle=90)) +
          theme(plot.title=element_text(size=20)) +
          theme(legend.title=element_text(size=15)) +
          theme(legend.text=element_text(size=15)) +
          scale_shape_manual(input$pcacolour, values=1:nlevels(as.factor(spectra.line.table[,input$pcacolour]))) +
          scale_colour_discrete(input$pcacolour) +
          geom_point(aes(PC1, PC2), colour="grey30", size=input$spotsize-2)
      } else if(input$elipseplot1==TRUE){
          ggplot(data= spectra.line.table) +
          stat_ellipse(aes(PC1, PC2, colour=as.factor(spectra.line.table[,input$pcacolour]), linetype=as.factor(spectra.line.table[,input$pcacolour]))) +
          geom_point(aes(PC1, PC2, colour=as.factor(spectra.line.table[,input$pcacolour]), shape=as.factor(spectra.line.table[,input$pcacolour])), size = input$spotsize+1) +
          geom_point(aes(PC1, PC2), colour="grey30", size=input$spotsize-2) +
          coord_cartesian(xlim = rangespca$x, ylim = rangespca$y, expand = TRUE) +
          scale_x_continuous("Principle Component 1") +
          scale_y_continuous("Principle Component 2") +
          theme_light() +
          theme(axis.text.x = element_text(size=15)) +
          theme(axis.text.y = element_text(size=15)) +
          theme(axis.title.x = element_text(size=15)) +
          theme(axis.title.y = element_text(size=15, angle=90)) +
          theme(plot.title=element_text(size=20)) +
          theme(legend.title=element_text(size=15)) +
          theme(legend.text=element_text(size=15)) +
          scale_shape_manual(input$pcacolour, values=1:nlevels(as.factor(spectra.line.table[,input$pcacolour]))) +
          scale_colour_discrete(input$pcacolour) +
          scale_linetype_discrete(input$pcacolour) +
          geom_point(aes(PC1, PC2), colour="grey30", size=input$spotsize-2)
      }
      
      color.plot
      
  })
  
  plotInput2focus <- reactive({
      
      spectra.line.table <- clusterFrame()

      if (input$pcacolour == "Focus" && input$pcafocuslabel=="None") {new.spectra.line.table <- spectra.line.table[,c("Spectrum", "PC1", "PC2", input$pcafocusvariable)]}
      
      if (input$pcacolour == "Focus" && input$pcafocuslabel=="None") {colnames(new.spectra.line.table) <- c("Spectrum", "PC1", "PC2", "Selected")}
      
      if (input$elipseplot1 == FALSE && input$pcacolour == "Focus" && input$pcafocuslabel=="None") {select.plot <- gghighlight_point(new.spectra.line.table, aes(PC1, PC2, colour=Selected), Selected %in% c(input$pcafocuschoice), size=input$spotsize,  use_group_by=FALSE, use_direct_label=FALSE) + coord_cartesian(xlim = rangespca$x, ylim = rangespca$y, expand = TRUE) + scale_x_continuous("Principle Component 1") + scale_y_continuous("Principle Component 2") + theme(axis.text.x = element_text(size=15)) + theme(axis.text.y = element_text(size=15)) + theme(axis.title.x = element_text(size=15)) + theme(axis.title.y = element_text(size=15, angle=90)) + theme(plot.title=element_text(size=20)) + theme(legend.title=element_text(size=15)) + theme(legend.text=element_text(size=15)) + theme_light()}
      
      if (input$elipseplot1 == TRUE && input$pcacolour == "Focus"  && input$pcafocuslabel=="None") {select.plot <- gghighlight_point(new.spectra.line.table, aes(PC1, PC2, colour=Selected), Selected %in% c(input$pcafocuschoice), size=input$spotsize, use_group_by=FALSE, use_direct_label=FALSE) +  coord_cartesian(xlim = rangespca$x, ylim = rangespca$y, expand = TRUE) + scale_x_continuous("Principle Component 1") + scale_y_continuous("Principle Component 2") + theme(axis.text.x = element_text(size=15)) + theme(axis.text.y = element_text(size=15)) + theme(axis.title.x = element_text(size=15)) + theme(axis.title.y = element_text(size=15, angle=90)) + theme(plot.title=element_text(size=20)) + theme(legend.title=element_text(size=15)) + stat_ellipse() + theme_light()}
      
      if (input$pcacolour == "Focus" && input$pcafocuslabel!="None") {newer.spectra.line.table <- spectra.line.table[,c("Spectrum", "PC1", "PC2", input$pcafocusvariable, input$pcafocuslabel)]}
      
      if (input$pcacolour == "Focus" && input$pcafocuslabel!="None") {colnames(newer.spectra.line.table) <- c("Spectrum", "PC1", "PC2", "Selected", "Label")}
      
      if (input$elipseplot1 == FALSE && input$pcacolour == "Focus" && input$pcafocuslabel!="None") {select.plot <- gghighlight_point(newer.spectra.line.table, aes(PC1, PC2, colour=Selected), Selected %in% c(input$pcafocuschoice), size=input$spotsize, label_key=Label, use_group_by=FALSE, use_direct_label=TRUE) + coord_cartesian(xlim = rangespca$x, ylim = rangespca$y, expand = TRUE) + scale_x_continuous("Principle Component 1") + scale_y_continuous("Principle Component 2") + theme(axis.text.x = element_text(size=15)) + theme(axis.text.y = element_text(size=15)) + theme(axis.title.x = element_text(size=15)) + theme(axis.title.y = element_text(size=15, angle=90)) + theme(plot.title=element_text(size=20)) + theme(legend.title=element_text(size=15)) + theme_light()}
      
      if (input$elipseplot1 == TRUE && input$pcacolour == "Focus"  && input$pcafocuslabel!="None") {select.plot <- gghighlight_point(newer.spectra.line.table, aes(PC1, PC2, colour=Selected), Selected %in% c(input$pcafocuschoice), size=input$spotsize, label_key=Label, use_group_by=FALSE, use_direct_label=TRUE) + coord_cartesian(xlim = rangespca$x, ylim = rangespca$y, expand = TRUE) + scale_x_continuous("Principle Component 1") + scale_y_continuous("Principle Component 2") + theme(axis.text.x = element_text(size=15)) + theme(axis.text.y = element_text(size=15)) + theme(axis.title.x = element_text(size=15)) + theme(axis.title.y = element_text(size=15, angle=90)) + theme(plot.title=element_text(size=20)) + theme(legend.title=element_text(size=15)) + stat_ellipse() + theme_light()}
      
      select.plot
      
  })
  
  
  plotInput2 <- reactive({
      
      if(input$pcacolour!="Focus"){
          plotInput2color()
      } else if(input$pcacolour=="Focus"){
          plotInput2focus()
      }
      
  })
  
  
  
  output$xrfpcaplot <- renderPlot({
      plotInput2()
      
  })
  

  
  output$hover_infopca <- renderUI({
      
      point.table <- clusterFrame()
      
      hover <- input$plot_hoverpca
      point <- nearPoints(point.table,  coordinfo=hover,   threshold = 5, maxpoints = 1, addDist = TRUE)
      if (nrow(point) == 0) return(NULL)
      


      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      
      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      # actual tooltip created as wellPanel
      wellPanel(
      style = style,
      p(HTML(paste0("<b> Spectrum: </b>", point$Spectrum, "<br/>",
      "<b> PC1: </b>", round(point$PC1, 2), "<br/>",
      "<b> PC2: </b>", round(point$PC2, 2), "<br/>"

      )))
      )
  })
  
  rangespca <- reactiveValues(x = NULL, y = NULL)
  
  observeEvent(input$plot_pca_dblclick, {
      brush <- input$plot_pca_brush
      if (!is.null(brush)) {
          rangespca$x <- c(brush$xmin, brush$xmax)
          rangespca$y <- c(brush$ymin, brush$ymax)
          
      } else {
          rangespca$x <- NULL
          rangespca$y <- NULL
      }
  })

  
  
  output$downloadPlot2 <- downloadHandler(
  filename = function() { paste(paste(c(input$projectname, "_", "PCAPlot"), collapse=''), '.tiff',  sep='') },
  content = function(file) {
      ggsave(file,plotInput2(), device="tiff", compression="lzw",  dpi=300, width=12, height=7)
  }
  )
  
  
  
  pcaTableInputFull <- reactive({
      xrf.pca.results <- xrfPCAReactive()

      xrf.pca.results
 
      
  })
  
  
  
  
  output$xrfpcatable <- DT::renderDataTable({
      
    
      
      df <- xrfKReactive()
      
      

      DT::datatable(df)
      
  })




output$xrfpcatablefull <- DT::renderDataTable({
    
    df <- pcaTableInputFull()
    DT::datatable(df)
    
})




output$xrfpcatablefulldownload <- downloadHandler(
filename = function() { paste(paste(c(input$projectname, "_", "PCATable"), collapse=''), '.csv', sep=',') },
content = function(file
) {
    write.csv(pcaTableInputFull(), file)
}
)





outApp <- reactive({
    
    xrf.k <- xrfKReactive()
    
    quality.table <-qualityTable()
    
    colour.table <- data.frame(xrf.k$Cluster, spectra.line.table)
    colnames(colour.table) <- c("Cluster", names(spectra.line.table))
    
    names(colour.table)
    
    
})



###Fingerprinting Module


# Return the requested dataset
datasetInputMatch <- reactive({
    switch(input$elementfingerprint,
    "H.table" = H.table,
    "He.table" = He.table,
    "Li.table" = Li.table,
    "Be.table" = Be.table,
    "B.table" = B.table,
    "C.table" = C.table,
    "N.table" = N.table,
    "O.table" = O.table,
    "F.table" = F.table,
    "Ne.table" = Ne.table,
    "Na.table" = Na.table,
    "Mg.table" = Mg.table,
    "Al.table" = Al.table,
    "Si.table" = Si.table,
    "P.table" = P.table,
    "S.table" = S.table,
    "Cl.table" = Cl.table,
    "Ar.table" = Ar.table,
    "K.table" = K.table,
    "Ca.table" = Ca.table,
    "Sc.table" = Sc.table,
    "Ti.table" = Ti.table,
    "V.table" = V.table,
    "Cr.table" = Cr.table,
    "Mn.table" = Mn.table,
    "Fe.table" = Fe.table,
    "Co.table" = Co.table,
    "Ni.table" = Ni.table,
    "Cu.table" = Cu.table,
    "Zn.table" = Zn.table,
    "Ga.table" = Ga.table,
    "Ge.table" = Ge.table,
    "As.table" = As.table,
    "Se.table" = Se.table,
    "Br.table" = Br.table,
    "Kr.table" = Kr.table,
    "Rb.table" = Rb.table,
    "Sr.table" = Sr.table,
    "Y.table" = Y.table,
    "Zr.table" = Zr.table,
    "Nb.table" = Nb.table,
    "Mo.table" = Mo.table,
    "Tc.table" = Tc.table,
    "Ru.table" = Ru.table,
    "Rh.table" = Rh.table,
    "Pd.table" = Pd.table,
    "Ag.table" = Ag.table,
    "Cd.table" = Cd.table,
    "In.table" = In.table,
    "Sn.table" = Sn.table,
    "Sb.table" = Sb.table,
    "Te.table" = Te.table,
    "I.table" = I.table,
    "Xe.table" = Xe.table,
    "Cs.table" = Cs.table,
    "Ba.table" = Ba.table,
    "La.table" = La.table,
    "Ce.table" = Ce.table,
    "Pr.table" = Pr.table,
    "Nd.table" = Nd.table,
    "Pm.table" = Pm.table,
    "Sm.table" = Sm.table,
    "Eu.table" = Eu.table,
    "Gd.table" = Gd.table,
    "Tb.table" = Tb.table,
    "Dy.table" = Dy.table,
    "Ho.table" = Ho.table,
    "Er.table" = Er.table,
    "Tm.table" = Tm.table,
    "Yb.table" = Yb.table,
    "Lu.table" = Lu.table,
    "Hf.table" = Hf.table,
    "Ta.table" = Ta.table,
    "W.table" = W.table,
    "Re.table" = Re.table,
    "Os.table" = Os.table,
    "Ir.table" = Ir.table,
    "Pt.table" = Pt.table,
    "Au.table" = Au.table,
    "Hg.table" = Hg.table,
    "Tl.table" = Tl.table,
    "Pb.table" = Pb.table,
    "Bi.table" = Bi.table,
    "Po.table" = Po.table,
    "At.table" = At.table,
    "Rn.table" = Rn.table,
    "Fr.table" = Fr.table,
    "Ra.table" = Ra.table,
    "Ac.table" = Ac.table,
    "Th.table" = Th.table,
    "Pa.table" = Pa.table,
    "U.table" = U.table)
})





spectraMatchData <- reactive({
    data <- fullSpectra1()
    data$CPS <- as.numeric(data$CPS)
    data$Energy <- as.numeric(data$Energy)
    data$Spectrum <- as.vector(data$Spectrum)
    
    if(input$usesubsetmatch==TRUE){
        data <- filter(data,
        Spectrum %in% input$show_rows
        )
        
    }
    
    data <- data[complete.cases(data),]
    data <- na.omit(data)
    
    data <- plyr::arrange(data, Spectrum, Energy)

    data
})




spectraSplit <- reactive({
    
    data <- spectraMatchData()
    
    namez <- make.names(sort(as.vector(unique(data$Spectrum))))
    

    
    out <- split(data, f=data$Spectrum)
    names(out) <- namez
    out
    
})




optionsDatum <- reactive({
    
    na.omit(names(spectraSplit()))
    
})

output$choosespectraui <- renderUI({
    
    selectInput('selectedfingerprint', "Spectrum to Match", choices=optionsDatum(), selected=optionsDatum()[1])
    
})

choosenSpectrum <- reactive({
    
    spectraSplit()[[input$selectedfingerprint]]
    
})

spectraRest <- reactive({
    
    data <- spectraSplit()
    
    data[[input$selectedfingerprint]] <- NULL
    
    data
    
})

restNames <- reactive({
    
    names(spectraRest())
    
})

lmList <- reactive({

    lm.list <- if(input$matchtype=="Untransformed"){
        pblapply(restNames(), function(x) lm(choosenSpectrum()$CPS~spectraRest()[[x]]$CPS))
    } else if(input$matchtype=="Velocity"){
        pblapply(restNames(), function(x) lm(Hodder.v(choosenSpectrum()$CPS)~Hodder.v(spectraRest()[[x]]$CPS)))
    } else if(input$matchtype=="Log"){
        pblapply(restNames(), function(x) lm(log(choosenSpectrum()$CPS)~log(spectraRest()[[x]]$CPS)))
    } else if(input$matchtype=="Log-Velocity"){
        pblapply(restNames(), function(x) lm(log(Hodder.v(choosenSpectrum()$CPS))~log(Hodder.v(spectraRest()[[x]]$CPS))))
    }

    names(lm.list) <- restNames()

    
    lm.list
    
})


r2Frame <- reactive({
    
    
    
    r2.list <- pblapply(restNames(), function(x) summary(lmList())$r.squared)
    names(r2.list) <- restNames()
    
    r2.frame <- data.frame(Spectrum=restNames(), R2=unlist(r2.list))
    r2.frame <- r2.frame[order(-r2.frame$R2),]
    r2.frame
    
})

aicFrame <- reactive({
    
    aic.list <- pblapply(lmList(), function(x) extractAIC(x, k=2)[2])
    names(aic.list) <- restNames()
    
    aic.frame <- data.frame(Spectrum=restNames(), AIC=unlist(aic.list))
    aic.frame <- aic.frame[order(aic.frame$AIC),]
    aic.frame
    
})


bicFrame <- reactive({
    
    bic.list <- pblapply(lmList(), function(x) extractAIC(x, k=log(2))[2])
    names(bic.list) <- restNames()
    
    bic.frame <- data.frame(Spectrum=restNames(), BIC=unlist(bic.list))
    bic.frame <- bic.frame[order(bic.frame$BIC),]
    bic.frame

})

matchTable <- reactive({
    
    if(input$matchcriteria=="R2"){
        r2Frame()
    } else if(input$matchcriteria=="AIC"){
        aicFrame()
    } else if(input$matchcriteria=="BIC"){
        bicFrame()
    }
    
})

bestMatch <- reactive({
    
    make.names(matchTable()$Spectrum[1])
    
})

output$thebestmatchui <- renderUI({
    
    h4(paste0("The best match is ", bestMatch()))
    
})


matchResults <- reactive({
    
    rbind(choosenSpectrum(), spectraSplit()[[bestMatch()]])
    
})

ranges_match <- reactiveValues(x = NULL, y = NULL)


matchPlot <- reactive({
    
    data <- matchResults()
    
    element <- datasetInputMatch()
    intensity.norm <- (element$Intensity/max(element$Intensity))*max(data$CPS)
    intensity.base <- (element$Intensity/max(element$Intensity))
    
    
    
    qplot(data$Energy, data$CPS, xlab = "Energy (keV)", ylab = "Counts per Second", geom="line", colour=data$Spectrum) +
    theme_light()+
    theme(legend.position="bottom") +
    geom_segment(aes(x=element$Line, xend=element$Line, y = 0, yend=intensity.norm), colour="grey50", linetype=2)  +
    scale_colour_discrete("Spectrum") +
    coord_cartesian(xlim = ranges_match$x, ylim = ranges_match$y)



    })


observeEvent(input$plot_dblclickmatch, {
    data <- matchResults()
    brush <- input$plot_brushmatch
    if (!is.null(brush)) {
        ranges_match$x <- c(brush$xmin, brush$xmax)
        ranges_match$y <- c(brush$ymin, brush$ymax)
        
    } else {
        ranges_match$x <- NULL
        ranges_match$y <- NULL
    }
        })
    
    output$matchplot <- renderPlot({
        
        matchPlot()
        
        
    })
    
    output$hover_infomatch <- renderUI({
        
        point.table <- matchResults()
        
        
        hover <- input$plot_hovermatch
        point <- nearPoints(point.table,  coordinfo=hover,   threshold = 5, maxpoints = 1, addDist = TRUE)
        if (nrow(point) == 0) return(NULL)
        
        
        
        
        # calculate point position INSIDE the image as percent of total dimensions
        # from left (horizontal) and from top (vertical)
        left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
        top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
        
        # calculate distance from left and bottom side of the picture in pixels
        left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
        top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
        
        
        # create style property fot tooltip
        # background color is set so tooltip is a bit transparent
        # z-index is set so we are sure are tooltip will be on top
        style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
        "left:", left_px + 2, "px; top:", top_px + 2, "px;")
        
        # actual tooltip created as wellPanel
        wellPanel(
        style = style,
        p(HTML(paste0("<b> Spectrum: </b>", point$Spectrum, "<br/>",
        "<b> Energy: </b>", round(point$Energy, 2), "<br/>",
        "<b> Counts: </b>", round(point$CPS, 2), "<br/>"
        
        )))
        
        )
    })
    
    
    ranges_matchmetric <- reactiveValues(x = NULL, y = NULL)
    
    
    matchPlotMetric <- reactive({
        
        data <- matchResults()
        
        max.metric <- if(input$matchtype == "Untransformed"){
            max(data$CPS)
        } else if(input$matchtype == "Velocity"){
            max(Hodder.v(data$CPS))
        } else if(input$matchtype == "Log"){
            max(log(data$CPS))
        } else if(input$matchtype == "Log-Velocity"){
            max(log(Hodder.v(data$CPS)))
        }
        
        element <- datasetInputMatch()
        intensity.norm <- (element$Intensity/max(element$Intensity))*max.metric
        intensity.base <- (element$Intensity/max(element$Intensity))
        
        
        
        plot <- if(input$matchtype == "Untransformed"){
            qplot(data$Energy, data$CPS, xlab = "Energy (keV)", ylab = "Counts per Second", geom="line", colour=data$Spectrum) +
            theme_light()+
            theme(legend.position="bottom") +
            geom_segment(aes(x=element$Line, xend=element$Line, y = 0, yend=intensity.norm), colour="grey50", linetype=2)  +
            scale_colour_discrete("Spectrum") +
            coord_cartesian(xlim = ranges_matchmetric$x, ylim = ranges_matchmetric$y)
        } else if(input$matchtype == "Velocity"){
            qplot(data$Energy, Hodder.v(data$CPS), xlab = "Energy (keV)", ylab = "Counts per Second", geom="line", colour=data$Spectrum) +
            theme_light()+
            theme(legend.position="bottom") +
            geom_segment(aes(x=element$Line, xend=element$Line, y = 0, yend=intensity.norm), colour="grey50", linetype=2)  +
            scale_colour_discrete("Spectrum") +
            coord_cartesian(xlim = ranges_match$x, ylim = ranges_match$y)
        } else if(input$matchtype == "Log"){
            qplot(data$Energy, log(data$CPS), xlab = "Energy (keV)", ylab = "Counts per Second", geom="line", colour=data$Spectrum) +
            theme_light()+
            theme(legend.position="bottom") +
            geom_segment(aes(x=element$Line, xend=element$Line, y = 0, yend=intensity.norm), colour="grey50", linetype=2)  +
            scale_colour_discrete("Spectrum") +
            coord_cartesian(xlim = ranges_matchmetric$x, ylim = ranges_matchmetric$y)
        } else if(input$matchtype == "Log-Velocity"){
            qplot(data$Energy, log(Hodder.v(data$CPS)), xlab = "Energy (keV)", ylab = "Counts per Second", geom="line", colour=data$Spectrum) +
            theme_light()+
            theme(legend.position="bottom") +
            geom_segment(aes(x=element$Line, xend=element$Line, y = 0, yend=intensity.norm), colour="grey50", linetype=2)  +
            scale_colour_discrete("Spectrum") +
            coord_cartesian(xlim = ranges_matchmetric$x, ylim = ranges_matchmetric$y)
        }
        
        plot
        
    })
    
    
    observeEvent(input$plot_dblclickmatchmetric, {
        data <- matchResults()
        brush <- input$plot_brushmatchmetric
        if (!is.null(brush)) {
            ranges_matchmetric$x <- c(brush$xmin, brush$xmax)
            ranges_matchmetric$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges_matchmetric$x <- NULL
            ranges_matchmetric$y <- NULL
        }
    })
        
        
        output$matchplotmetric <- renderPlot({
            
            matchPlotMetric()
            
        })
        
        output$hover_infomatchmetric <- renderUI({
            
            point.table <- matchResults()
            
            point.table$CPS <- if(input$matchtype == "Untransformed"){
                point.table$CPS
            } else if(input$matchtype == "Velocity"){
                Hodder.v(point.table$CPS)
            } else if(input$matchtype == "Log"){
                log(point.table$CPS)
            } else if(input$matchtype == "Log-Velocity"){
                log(Hodder.v(point.table$CPS))
            }
            
            
            hover <- input$plot_hovermatchmetric
            point <- nearPoints(point.table,  coordinfo=hover,   threshold = 5, maxpoints = 1, addDist = TRUE)
            if (nrow(point) == 0) return(NULL)
            
            
            
            
            # calculate point position INSIDE the image as percent of total dimensions
            # from left (horizontal) and from top (vertical)
            left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
            top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
            
            # calculate distance from left and bottom side of the picture in pixels
            left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
            top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
            
            
            # create style property fot tooltip
            # background color is set so tooltip is a bit transparent
            # z-index is set so we are sure are tooltip will be on top
            style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
            "left:", left_px + 2, "px; top:", top_px + 2, "px;")
            
            # actual tooltip created as wellPanel
            wellPanel(
            style = style,
            p(HTML(paste0("<b> Spectrum: </b>", point$Spectrum, "<br/>",
            "<b> Energy: </b>", round(point$Energy, 2), "<br/>",
            "<b> Counts: </b>", round(point$CPS, 2), "<br/>"
            
            )))
            
            )
        })
    
    output$matchtable <- renderDataTable({
        
        data.table(matchTable())
        
    })





output$inApp <- renderUI({
    selectInput(inputId = "app", label = h4("Application"), choices =  outApp())
})


dataDefaultSelect <- reactive({
    
    data.options <-defaultLines()
    data.selected <- data.options[5]
    data.selected
    
})

secondDefaultSelect <- reactive({
    
    data.options <-defaultLines()
    data.selected <- data.options[6]
    data.selected
    
})


  
  choiceLinesRatio <- reactive({
      
      spectra.line.table <- dataMerge3()
      
      spectra.line.table$None <- rep(1, length(spectra.line.table$Spectrum))
      
      standard <- if(dataType()=="Spectra"){
          colnames(spectra.line.table[ ,!(colnames(spectra.line.table) == "Spectrum")])
      } else if(input$filetype=="Net"){
          colnames(spectra.line.table[ ,!(colnames(spectra.line.table) == "Spectrum")])
      } else if(dataType()=="Spreadsheet"){
          colnames(spectra.line.table[ ,!(colnames(spectra.line.table) == "Spectrum")])
      }
      
  })
  
  
  ratioChooseA <- reactive({
      spectra.line.table <- dataMerge3()
      spectra.line.names <- colnames(spectra.line.table)
      
      
      standard <- spectra.line.names[2]
      standard
      
  })
  
  
  
  ratioChooseB <- reactive({
      spectra.line.table <- dataMerge3()
      spectra.line.names <- colnames(spectra.line.table)
      
      
      standard <- "None"
      
      
      standard
      
  })
  
  
  ratioChooseC <- reactive({
      spectra.line.table <- dataMerge3()
      spectra.line.names <- colnames(spectra.line.table)
      
      
      standard <- spectra.line.names[4]
      
      
      standard
      
  })
  
  
  ratioChooseD <- reactive({
      spectra.line.table <- dataMerge3()
      spectra.line.names <- colnames(spectra.line.table)
      
      standard <- "None"
      
      
      standard
      
  })
  
  
  output$inelementratioa <- renderUI({
      selectInput("elementratioa", "Element A", choices=choiceLinesRatio(), selected=ratioChooseA())
  })
  
  output$inelementratiob <- renderUI({
      selectInput("elementratiob", "Element B", choices=choiceLinesRatio(), selected=ratioChooseB())
  })
  
  output$inelementratioc <- renderUI({
      selectInput("elementratioc", "Element C", choices=choiceLinesRatio(), selected=ratioChooseC())
  })
  
  output$inelementratiod <- renderUI({
      selectInput("elementratiod", "Element D", choices=choiceLinesRatio(), selected=ratioChooseD())
  })
  

  

  output$ratiocolourui <- renderUI({
      
      selectInput("ratiocolour", "Ratio Plot Type", choices=c("Black", qualChoices(), "Focus"), selected="Cluster")
      
  })
  

  
  
  output$ratioFocusVariable <- renderUI({
      
      if(input$ratiocolour=="Focus"){selectInput('ratiofocusvariable', "Choose Variable", choices=qualChoices(), selected="Qualitative1")} else {
          p()
      }
      
  })
  
  output$ratioFocusUI <- renderUI({
      
      if(input$ratiocolour=="Focus"){selectInput('ratiofocuschoice', "Choose Focus", choices=unique(values[["DF"]][input$ratiofocusvariable]), selected="Qualitative1", multiple=TRUE)} else {
          p()
      }
      
  })
  
  output$ratioFocusLabel <- renderUI({
      if(input$ratiocolour=="Focus"){selectInput('ratiofocuslabel', "Choose Label", choices=c("None", names(values[["DF"]])), selected="None")} else {
          p()
      }
      
  })
  
  output$ratioFocusShape <- renderUI({
      if(input$ratiocolour=="Focus"){selectInput('ratiofocusshape', "Choose Label", choices=c("None", names(values[["DF"]])), selected="None")} else {
          p()
      }
      
  })
  
  ratioFrame <- reactive({
      
      spectra.line.table <- dataMerge3()
      spectra.line.table$None <- rep(1, length(spectra.line.table$Spectrum))
      
      xrf.k <- xrfKReactive()
      
      colour.table <- data.frame(xrf.k$Cluster, spectra.line.table)
      colnames(colour.table) <- c("Cluster", names(spectra.line.table))
      
      unique.spec <- seq(1, length(colour.table$Spectrum), 1)
      null <- rep(1, length(unique.spec))
      
      spectra.line.table$Cluster <- xrf.k$Cluster
      
           first.ratio <-spectra.line.table[input$elementratioa]
           second.ratio <- spectra.line.table[input$elementratiob]
           third.ratio <- spectra.line.table[input$elementratioc]
           fourth.ratio <- spectra.line.table[input$elementratiod]
           
           first.ratio.norm <- first.ratio/sum(first.ratio)
           second.ratio.norm <- second.ratio/sum(second.ratio)
           third.ratio.norm <- third.ratio/sum(third.ratio)
           fourth.ratio.norm <- fourth.ratio/sum(fourth.ratio)
           
           ratio.frame <- data.frame(A=first.ratio, B=second.ratio, C=third.ratio, D=fourth.ratio, spectra.line.table, stringsAsFactors=FALSE)
           
           
           ratio.frame$X <- ratio.frame[,1]/ratio.frame[,2]
           ratio.frame$Y <- ratio.frame[,3]/ratio.frame[,4]
           
           
           
           ratio.frame
      
      
  })
  
  plotInput4black <- reactive({
      
      ratio.frame <- ratioFrame()
      
      if(input$elementratiob!="None"){ratio.names.x <- c(input$elementratioa, "/", input$elementratiob)}
      if(input$elementratiod!="None"){ratio.names.y <- c(input$elementratioc, "/", input$elementratiod)}
      
      if(input$elementratiob=="None"){ratio.names.x <- c(input$elementratioa)}
      if(input$elementratiod=="None"){ratio.names.y <- c(input$elementratioc)}
      
      ratio.names.x <- paste(ratio.names.x, sep=",", collapse="")
      ratio.names.y <- paste(ratio.names.y, sep=",", collapse="")
      
      black.ratio.plot <- qplot(X, Y, data=ratio.frame, xlab = ratio.names.x, ylab = ratio.names.y ) +
      geom_point(lwd=input$spotsize2) +
      coord_cartesian(xlim = rangesratio$x, ylim = rangesratio$y, expand = TRUE) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      geom_point(colour="grey30", size=input$spotsize2-2, alpha=0.01) +
      scale_color_hue(direction = input$colourdirectionratio)
      
      black.ratio.plot
      
  })
  
  plotInput4color <- reactive({
      
      ratio.frame <- ratioFrame()
      
      if(input$elementratiob!="None"){ratio.names.x <- c(input$elementratioa, "/", input$elementratiob)}
      if(input$elementratiod!="None"){ratio.names.y <- c(input$elementratioc, "/", input$elementratiod)}
      
      if(input$elementratiob=="None"){ratio.names.x <- c(input$elementratioa)}
      if(input$elementratiod=="None"){ratio.names.y <- c(input$elementratioc)}
      
      ratio.names.x <- paste(ratio.names.x, sep=",", collapse="")
      ratio.names.y <- paste(ratio.names.y, sep=",", collapse="")
      
      color.plot <- if(input$elipseplot2==FALSE){
          qplot(X, Y, data=ratio.frame, xlab = ratio.names.x, ylab = ratio.names.y ) +
          geom_point(aes(colour=as.factor(ratio.frame[,input$ratiocolour]), shape=as.factor(ratio.frame[,input$ratiocolour])), size=input$spotsize2+1) +
          geom_point(colour="grey30", size=input$spotsize2-2) +
          coord_cartesian(xlim = rangesratio$x, ylim = rangesratio$y, expand = TRUE) +
          scale_shape_manual(input$ratiocolour, values=1:nlevels(as.factor(as.factor(ratio.frame[,input$ratiocolour])))) +
          scale_colour_discrete(input$ratiocolour) +
          theme_light() +
          theme(axis.text.x = element_text(size=15)) +
          theme(axis.text.y = element_text(size=15)) +
          theme(axis.title.x = element_text(size=15)) +
          theme(axis.title.y = element_text(size=15, angle=90)) +
          theme(plot.title=element_text(size=20)) +
          theme(legend.title=element_text(size=15)) +
          theme(legend.text=element_text(size=15)) +
          geom_point(colour="grey30", size=input$spotsize2-2, alpha=0.01)
      } else if(input$elipseplot2==TRUE){
          ggplot(data=ratio.frame) +
          stat_ellipse(aes(X, Y, colour=as.factor(ratio.frame[,input$ratiocolour]), linetype=as.factor(ratio.frame[,input$ratiocolour]))) +
          geom_point(aes(X, Y, colour=as.factor(ratio.frame[,input$ratiocolour]), shape=as.factor(ratio.frame[,input$ratiocolour])), size = input$spotsize2+1) +
          geom_point(aes(X, Y), colour="grey30", size=input$spotsize2-2) +
          coord_cartesian(xlim = rangesratio$x, ylim = rangesratio$y, expand = TRUE) +
          scale_x_continuous(ratio.names.x) +
          scale_y_continuous(ratio.names.y) +
          theme_light() +
          theme(axis.text.x = element_text(size=15)) +
          theme(axis.text.y = element_text(size=15)) +
          theme(axis.title.x = element_text(size=15)) +
          theme(axis.title.y = element_text(size=15, angle=90)) +
          theme(plot.title=element_text(size=20)) +
          theme(legend.title=element_text(size=15)) +
          theme(legend.text=element_text(size=15)) +
          scale_shape_manual(input$ratiocolour, values=1:nlevels(as.factor(ratio.frame[,input$ratiocolour]))) +
          scale_colour_discrete(input$ratiocolour) +
          scale_linetype_discrete(input$ratiocolour) +
          geom_point(aes(X, Y), colour="grey30", size=input$spotsize2-2)
      }
      
      color.plot
      
  })
  
  plotInput4focus <- reactive({
      
      ratio.frame <- ratioFrame()
      
      if(input$elementratiob!="None"){ratio.names.x <- c(input$elementratioa, "/", input$elementratiob)}
      if(input$elementratiod!="None"){ratio.names.y <- c(input$elementratioc, "/", input$elementratiod)}
      
      if(input$elementratiob=="None"){ratio.names.x <- c(input$elementratioa)}
      if(input$elementratiod=="None"){ratio.names.y <- c(input$elementratioc)}
      
      ratio.names.x <- paste(ratio.names.x, sep=",", collapse="")
      ratio.names.y <- paste(ratio.names.y, sep=",", collapse="")
      
      if (input$ratiocolour == "Focus" && input$ratiofocuslabel=="None") {new.ratio.table <- ratio.frame[,c("Spectrum", "X", "Y", input$ratiofocusvariable)]}
      
      if (input$ratiocolour == "Focus" && input$ratiofocuslabel=="None") {colnames(new.ratio.table) <- c("Spectrum", "X", "Y", "Selected")}
      
      if(input$ratiofocusshape!="None"){
          new.ratio.table$Shape <- ratio.frame[,input$ratiofocusshape]
          colnames(new.ratio.table) <- c("Spectrum", "X", "Y", "Selected", input$ratiofocusshape)
      }
      
      if (input$elipseplot2 == FALSE && input$ratiocolour == "Focus" && input$ratiofocuslabel=="None") {select.plot <- gghighlight_point(new.ratio.table, aes_string("X", "Y", colour="Selected", shape=input$ratiofocusshape), Selected %in% c(input$ratiofocuschoice), size=input$spotsize2, use_group_by=FALSE, use_direct_label=FALSE) + coord_cartesian(xlim = rangesratio$x, ylim = rangesratio$y, expand = TRUE) +  scale_x_continuous(ratio.names.x) + scale_y_continuous(ratio.names.y) + theme(axis.text.x = element_text(size=15)) + theme(axis.text.y = element_text(size=15)) + theme(axis.title.x = element_text(size=15)) + theme(axis.title.y = element_text(size=15, angle=90)) + theme(plot.title=element_text(size=20)) + theme(legend.title=element_text(size=15)) + theme_light()}
      
      if (input$elipseplot2 == TRUE && input$ratiocolour == "Focus"  && input$ratiofocuslabel=="None") {select.plot <- gghighlight_point(new.ratio.table, aes(X, Y, colour=Selected, shape=input$ratiofocusshape), Selected %in% c(input$ratiofocuschoice), size=input$spotsize2, use_group_by=FALSE, use_direct_label=FALSE) + scale_x_continuous(ratio.names.x) + coord_cartesian(xlim = rangesratio$x, ylim = rangesratio$y, expand = TRUE) + scale_y_continuous(ratio.names.y) + theme(axis.text.x = element_text(size=15)) + theme(axis.text.y = element_text(size=15)) + theme(axis.title.x = element_text(size=15)) + theme(axis.title.y = element_text(size=15, angle=90)) + theme(plot.title=element_text(size=20)) + theme(legend.title=element_text(size=15)) + stat_ellipse() + theme_light()}
      
      if (input$ratiocolour == "Focus" && input$ratiofocuslabel!="None") {newer.ratio.table <- ratio.frame[,c("Spectrum", "X", "Y", input$ratiofocusvariable, input$ratiofocuslabel)]}
      
      if (input$ratiocolour == "Focus" && input$ratiofocuslabel!="None") {colnames(newer.ratio.table) <- c("Spectrum", "X", "Y", "Selected", "Label")}
      
      if (input$elipseplot2 == FALSE && input$ratiocolour == "Focus" && input$ratiofocuslabel!="None") {select.plot <- gghighlight_point(newer.ratio.table, aes(X, Y, colour=Selected, shape=input$ratiofocusshape), Selected %in% c(input$ratiofocuschoice), size=input$spotsize2, label_key=Label, use_group_by=FALSE, use_direct_label=TRUE) + coord_cartesian(xlim = rangesratio$x, ylim = rangesratio$y, expand = TRUE) +  scale_x_continuous(ratio.names.x) + scale_y_continuous(ratio.names.y) + theme(axis.text.x = element_text(size=15)) + theme(axis.text.y = element_text(size=15)) + theme(axis.title.x = element_text(size=15)) + theme(axis.title.y = element_text(size=15, angle=90)) + theme(plot.title=element_text(size=20)) + theme(legend.title=element_text(size=15)) + theme_light()}
      
      if (input$elipseplot2 == TRUE && input$ratiocolour == "Focus"  && input$ratiofocuslabel!="None") {select.plot <- gghighlight_point(newer.ratio.table, aes(X, Y, colour=Selected, shape=input$ratiofocusshape), Selected %in% c(input$ratiofocuschoice), size=input$spotsize2, label_key=Label, use_group_by=FALSE, use_direct_label=TRUE) + coord_cartesian(xlim = rangesratio$x, ylim = rangesratio$y, expand = TRUE) + scale_x_continuous(ratio.names.x) + scale_y_continuous(ratio.names.y) + theme(axis.text.x = element_text(size=15)) + theme(axis.text.y = element_text(size=15)) + theme(axis.title.x = element_text(size=15)) + theme(axis.title.y = element_text(size=15, angle=90)) + theme(plot.title=element_text(size=20)) + theme(legend.title=element_text(size=15)) + stat_ellipse() + theme_light()}
      
      select.plot
      
  })
  
  plotInput4 <- reactive({
      
      if(input$ratiocolour == "Black"){
          plotInput4black()
      } else if(input$ratiocolour != "Black" && input$ratiocolour != "Focus"){
          plotInput4color()
      } else if(input$ratiocolour == "Focus"){
          plotInput4focus()
      }
      
  })
  
  
  

   output$elementratiotimeseries <- renderPlot({
       plotInput4()
    })
   
   
   
   
   
   output$hover_inforatio <- renderUI({
       
       point.table <- ratioFrame()
       
       hover <- input$plot_hoverratio
       point <- nearPoints(point.table,  coordinfo=hover,   threshold = 5, maxpoints = 1, addDist = TRUE)
       if (nrow(point) == 0) return(NULL)
       
       
       # calculate point position INSIDE the image as percent of total dimensions
       # from left (horizontal) and from top (vertical)
       left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
       top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
       
       # calculate distance from left and bottom side of the picture in pixels
       left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
       top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
       
       
       
       
       
       # create style property fot tooltip
       # background color is set so tooltip is a bit transparent
       # z-index is set so we are sure are tooltip will be on top
       style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
       "left:", left_px + 2, "px; top:", top_px + 2, "px;")
       
       # actual tooltip created as wellPanel
       wellPanel(
       style = style,
       p(HTML(paste0("<b> Spectrum: </b>", point$Spectrum, "<br/>",
       "<b> Qual 1: </b>", point$Qualitative1, "<br/>",
     "<b> Qual 2: </b>", point$Qualitative2, "<br/>"

       
       
       )))
       )
   })
   
   rangesratio <- reactiveValues(x = NULL, y = NULL)
   
   observeEvent(input$plot_ratio_dblclick, {
       brush <- input$plot_ratio_brush
       if (!is.null(brush)) {
           rangesratio$x <- c(brush$xmin, brush$xmax)
           rangesratio$y <- c(brush$ymin, brush$ymax)
           
       } else {
           rangesratio$x <- NULL
           rangesratio$y <- NULL
       }
   })
   
   ratioTerm <- reactive({
       
          ratio.names <- paste(c(c(substr(input$elementratioa, 1,2), "-", substr(input$elementratiob, 1, 2)), "_", c(substr(input$elementratioc,1,2), "-", substr(input$elementratiod,1,2), "_RatioPlot")), collapse="")
          ratio.label <- paste(c(input$projectname, "_", ratio.names), collapse='')
          ratio.label
   })
   
   output$downloadPlot4 <- downloadHandler(

   
   filename = function() { paste(ratioTerm(), '.tiff', sep='') },
   content = function(file) {
       ggsave(file,plotInput4(), device="tiff", compression="lzw",  dpi=300, width=12, height=7)
   }
   )




ternaryChooseA <- reactive({
    spectra.line.table <- dataMerge3()
    spectra.line.names <- colnames(spectra.line.table)
    
    
    standard <- if(dataType()=="Spectra"){
        "Al.K.alpha"
    } else if(input$filetype=="Net"){
        spectra.line.names[2]
    } else if (dataType()=="Spreadsheet"){
        spectra.line.names[2]
    }
    
    standard
    
})

ternaryChooseB <- reactive({
    spectra.line.table <- dataMerge3()
    spectra.line.names <- colnames(spectra.line.table)
    
    
    standard <- if(dataType()=="Spectra"){
        "Si.K.alpha"
    } else if(input$filetype=="Net"){
        spectra.line.names[3]
    } else if (dataType()=="Spreadsheet"){
        spectra.line.names[3]
    }
    
    standard
    
})

ternaryChooseC <- reactive({
    spectra.line.table <- dataMerge3()
    spectra.line.names <- colnames(spectra.line.table)
    
    
    standard <- if(dataType()=="Spectra"){
        "Ca.K.alpha"
    } else if(input$filetype=="Net"){
        spectra.line.names[4]
    } else if (dataType()=="Spreadsheet"){
        spectra.line.names[4]
    }
    
    standard
    
})

output$inaxisa <- renderUI({
    selectInput("axisa", "Axis A", choices=choiceLines(), selected=ternaryChooseA())
})

output$inaxisb <- renderUI({
    selectInput("axisb", "Axis B", choices=choiceLines(), selected=ternaryChooseB())
})

output$inaxisc <- renderUI({
    selectInput("axisc", "Axis C", choices=choiceLines(), selected=ternaryChooseC())
})

output$ternarycolourui <- renderUI({
    
    selectInput("ternarycolour", "Ternary Plot Type", choices=c("Black", qualChoices()), selected="Cluster")
    
})

firstAxis <- reactive({
    
    spectra.line.table <- dataMerge3()
    
    first.axis.nonorm <- as.vector(spectra.line.table[,input$axisa])
    first.axis.norm <- first.axis.nonorm/sum(first.axis.nonorm)
    first.axis <- if(input$ternnormplot==FALSE){
        first.axis.nonorm
    } else if(input$ternnormplot==TRUE){
        first.axis.norm
    }
    first.axis
})

secondAxis <- reactive({
    
    spectra.line.table <- dataMerge3()
    
    second.axis.nonorm <- as.vector(spectra.line.table[,input$axisb])
    second.axis.norm <- second.axis.nonorm/sum(second.axis.nonorm)
    second.axis <- if(input$ternnormplot==FALSE){
        second.axis.nonorm
    } else if(input$ternnormplot==TRUE){
        second.axis.norm
    }
    second.axis
})

thirdAxis <- reactive({
    
    spectra.line.table <- dataMerge3()
    
    third.axis.nonorm <- as.vector(spectra.line.table[,input$axisc])
    third.axis.norm <- third.axis.nonorm/sum(third.axis.nonorm)
    third.axis <- if(input$ternnormplot==FALSE){
        third.axis.nonorm
    } else if(input$ternnormplot==TRUE){
        third.axis.norm
    }
    third.axis
})

tenaryFrame <- reactive({
    
    spectra.line.table <- dataMerge3()[,!colnames(dataMerge3()) %in%  c(input$axisa, input$axisb, input$axisc)]
    
      
      xrf.k <- xrfKReactive()
      
      quality.table <-qualityTable()
      
      
      colour.table <- data.frame(xrf.k$Cluster, spectra.line.table)
      colnames(colour.table) <- c("Cluster", names(spectra.line.table))
      
      
      
      
      unique.spec <- seq(1, length(colour.table$Spectrum), 1)
      null <- rep(1, length(unique.spec))
      
      first.axis <- firstAxis()
      second.axis <- secondAxis()
      third.axis <- thirdAxis()
      
      spectra.line.table$Cluster <- xrf.k$Cluster
      
      axis.frame <- data.frame(Hold1=first.axis, Hold2=second.axis, Hold3=third.axis, spectra.line.table, stringsAsFactors=FALSE)
      colnames(axis.frame)[1:3] <- c(input$axisa, input$axisb, input$axisc)
      
      axis.frame
    
    
})


plotInput5black <- reactive({
    
    axis.frame <- tenaryFrame()
    
    ternaryplot <- if(input$terndensityplot==FALSE){
            ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
            geom_point(size=input$ternpointsize) +
            theme_light() +
            theme(axis.text.x = element_text(size=15)) +
            theme(axis.text.y = element_text(size=15)) +
            theme(axis.title.x = element_text(size=15)) +
            theme(axis.title.y = element_text(size=15, angle=90)) +
            theme(plot.title=element_text(size=20)) +
            theme(legend.title=element_text(size=15)) +
            theme(legend.text=element_text(size=15))
        } else if(input$terndensityplot==TRUE){
            ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
            geom_density_tern() +
            geom_point(size=input$ternpointsize) +
            theme_light() +
            theme(axis.text.x = element_text(size=15)) +
            theme(axis.text.y = element_text(size=15)) +
            theme(axis.title.x = element_text(size=15)) +
            theme(axis.title.y = element_text(size=15, angle=90)) +
            theme(plot.title=element_text(size=20)) +
            theme(legend.title=element_text(size=15)) +
            theme(legend.text=element_text(size=15))
        }
        
        ternaryplot

})

plotInput5color <- reactive({
    
    axis.frame <- tenaryFrame()
    
    ternaryplot <- if(input$terndensityplot==FALSE){
            ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
            geom_point(aes(colour = as.factor(axis.frame[,input$ternarycolour]), shape=as.factor(axis.frame[,input$ternarycolour])), size=input$ternpointsize+1) +
            geom_point(colour="grey30", size=input$ternpointsize-2) +
            scale_shape_manual(input$ternarycolour, values=1:nlevels(as.factor(axis.frame[,input$ternarycolour]))) +
            scale_colour_discrete(input$ternarycolour) +
            theme_light() +
            theme(axis.text.x = element_text(size=15)) +
            theme(axis.text.y = element_text(size=15)) +
            theme(axis.title.x = element_text(size=15)) +
            theme(axis.title.y = element_text(size=15, angle=90)) +
            theme(plot.title=element_text(size=20)) +
            theme(legend.title=element_text(size=15)) +
            theme(legend.text=element_text(size=15))
        } else if(input$terndensityplot==TRUE){
            ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
            geom_density_tern() +
            geom_point(aes(colour = as.factor(axis.frame[,input$ternarycolour]), shape=as.factor(axis.frame[,input$ternarycolour])), size=input$ternpointsize) +
            geom_point(colour="grey30", size=input$ternpointsize-2) +
            scale_shape_manual("Cluster", values=1:nlevels(as.factor(axis.frame[,input$ternarycolour]))) +
            scale_colour_discrete("Cluster") +
            theme_light() +
            theme(axis.text.x = element_text(size=15)) +
            theme(axis.text.y = element_text(size=15)) +
            theme(axis.title.x = element_text(size=15)) +
            theme(axis.title.y = element_text(size=15, angle=90)) +
            theme(plot.title=element_text(size=20)) +
            theme(legend.title=element_text(size=15)) +
            theme(legend.text=element_text(size=15))
        }
        
        ternaryplot
    
})

plotInput5 <- reactive({
    
    if(input$ternarycolour=="Black"){
        print(plotInput5black())
    } else if(input$ternarycolour!="Black"){
        print(plotInput5color())
    }
    
})

output$ternaryplot <- renderPlot({
    plotInput5()
})




axisTerm <- reactive({
    axis.names.tern <- paste(gsub("[.]", "-", c(substr(input$axisa, 1, 2), substr(input$axisb, 1, 2), substr(input$axisc, 1, 2), "Ternary")), collapse='')
    axis.labels <- paste(c(input$projectname, "_", axis.names.tern), collapse='')
    axis.labels
})

output$downloadPlot5 <- downloadHandler(



filename = function() { paste(axisTerm(), '.tiff', sep='') },
content = function(file) {
    ggsave(file,plotInput5(), device="tiff", compression="lzw",  dpi=300, width=12, height=7)
}
)

####Machine Learning

allVars <- reactive({
    colnames(dataMerge3())
})

output$variableui <- renderUI({
    
    selectInput('variable', "Variable", choices=allVars(), selected="Qualitative1", multiple=FALSE)
    
})

output$predictorsui <- renderUI({
    req(input$variable)
    selectInput('predictors', "Predictors", choices=allVars()[!allVars() %in% input$variable], selected=input$show_vars, multiple=TRUE)
    
})


modelKind <- reactive({
    if(is.numeric(dataMerge3()[,input$variable])==TRUE){
        "Regression"
    } else if(is.numeric(dataMerge3()[,input$variable])==FALSE){
        "Classification"
    }
})


needTry <- reactive({
    if(input$modeltype==1){
        FALSE
    } else if(input$modeltype==2){
        FALSE
    } else if(input$modeltype==3){
        FALSE
    } else if(input$modeltype==4){
        TRUE
    }  else if(input$modeltype==5){
        TRUE
    } else if(input$modeltype==6 && input$neuralhiddenlayers == 1){
        FALSE
    } else if(input$modeltype==6 && input$neuralhiddenlayers > 1){
        TRUE
    } else if(input$modeltype==7 && input$neuralhiddenlayers == 1){
        FALSE
    } else if(input$modeltype==7 && input$neuralhiddenlayers > 1){
        TRUE
    } else if(input$modeltype==8){
        FALSE
    } else if(input$modeltype==9){
        FALSE
    }
})

needCaret <- reactive({
    if(input$modeltype==1){
        FALSE
    } else if(input$modeltype==2){
        FALSE
    } else if(input$modeltype==3){
        FALSE
    } else if(input$modeltype==4){
        TRUE
    }  else if(input$modeltype==5){
        TRUE
    } else if(input$modeltype==6){
        TRUE
    } else if(input$modeltype==7){
        TRUE
    } else if(input$modeltype==8){
        TRUE
    } else if(input$modeltype==9){
        TRUE
    }
})

needForest <- reactive({
    if(input$modeltype==1){
        FALSE
    } else if(input$modeltype==2){
        FALSE
    } else if(input$modeltype==3){
        FALSE
    } else if(input$modeltype==4){
        TRUE
    }  else if(input$modeltype==5){
        TRUE
    } else if(input$modeltype==6){
        FALSE
    } else if(input$modeltype==7){
        FALSE
    } else if(input$modeltype==8){
        TRUE
    } else if(input$modeltype==9){
        TRUE
    }
})

needNeuralNet <- reactive({
    if(input$modeltype==1){
        FALSE
    } else if(input$modeltype==2){
        FALSE
    } else if(input$modeltype==3){
        FALSE
    } else if(input$modeltype==4){
        FALSE
    }  else if(input$modeltype==5){
        FALSE
    } else if(input$modeltype==6){
        TRUE
    } else if(input$modeltype==7){
        TRUE
    } else if(input$modeltype==8){
        FALSE
    } else if(input$modeltype==9){
        FALSE
    }
})

needXGBoost <- reactive({
    if(input$modeltype==1){
        FALSE
    } else if(input$modeltype==2){
        FALSE
    } else if(input$modeltype==3){
        FALSE
    } else if(input$modeltype==4){
        FALSE
    }  else if(input$modeltype==5){
        FALSE
    } else if(input$modeltype==6){
        FALSE
    } else if(input$modeltype==7){
        FALSE
    } else if(input$modeltype==8){
        TRUE
    } else if(input$modeltype==9){
        TRUE
    }
})


modelType <- reactive({
    if(input$modeltype==4){
        "Forest"
    } else if(input$modeltype==8){
        req(input$xgbtype)
        if(input$xgbtype=="Linear"){
            "XGBLinear"
        } else if(input$xgbtype=="Tree"){
            "XGBTree"
        }
    }
})

mlSplit <- reactive({
    result <- input$split
            if(result==0){
                result <- NULL
            }
            result
})

minN <- reactive({
    result <- input$min_n
            if(result==0){
                result <- NULL
            }
            result
})

output$foresttryui <- renderUI({
    req(input$modeltype)
    if(needTry()==TRUE){
        forestTryUI(radiocal=input$modeltype, neuralhiddenlayers=2, selection=10, maxsample=100)
    } else if(needTry()==FALSE){
        NULL
    }
})

forestTry <- reactive({
    if(needTry()==TRUE){
        input$foresttry
    } else if(needTry()==FALSE){
        NULL
    }
})


output$forestmetricui <- renderUI({
    if(needCaret()==TRUE){
        if(modelKind()=="Regression"){
            forestMetricQuantUI(radiocal=input$modeltype, selection="RMSE")
        } else if(modelKind()=="Classification"){
            forestMetricQualUI(radiocal=input$modeltype, selection="Accuracy")
        }
        
    } else if(needCaret()==FALSE){
        NULL
    }
})

forestMetric <- reactive({
    if(needCaret()==TRUE){
        input$forestmetric
    } else if(needCaret()==FALSE){
        NULL
    }
})


output$foresttrainui <- renderUI({
    req(input$modeltype)
    if(needCaret()==TRUE){
        forestTrainUI(radiocal=input$modeltype, selection="repeatedcv")
    } else if(needCaret()==FALSE){
        NULL
    }
})

forestTrain <- reactive({
    if(needCaret()==TRUE){
        input$foresttrain
    } else if(needCaret()==FALSE){
        NULL
    }
})

output$forestnumberui <- renderUI({
    req(input$modeltype)
    if(needCaret()==TRUE){
        forestNumberUI(radiocal=input$modeltype, selection=30)
    } else if(needCaret()==FALSE){
        NULL
    }
})

forestNumber <- reactive({
    if(needCaret()==TRUE){
        input$forestnumber
    } else if(needCaret()==FALSE){
        NULL
    }
})

output$cvrepeatsui <- renderUI({
    req(input$modeltype, input$forestnumber)
    if(forestTrain()=="repeatedcv"){
        cvRepeatsUI(radiocal=input$modeltype, foresttrain=forestTrain(), selection=3)
    } else if(forestTrain()!="repeatedcv"){
        NULL
    }
})

cvRepeats <- reactive({
    if(forestTrain()=="repeatedcv"){
        input$cvrepeats
    } else if(forestTrain()!="repeatedcv"){
        NULL
    }
})


output$foresttreesui <- renderUI({
    req(input$modeltype)
    if(needForest()==TRUE){
        forestTreesUI(radiocal=input$modeltype, selection=1000)
    } else if(needXGBoost()==TRUE){
        forestTreesUI(radiocal=input$modeltype, selection=1000)
    } else {
        NULL
    }
})

forestTrees <- reactive({
    req(input$modeltype)
    if(needForest()==TRUE){
        input$foresttrees
    } else if(needXGBoost()==TRUE){
        input$foresttrees
    } else {
        NULL
    }
})


output$neuralhiddenlayersui <- renderUI({
    req(input$modeltype)
    if(needNeuralNet()==TRUE){
        neuralHiddenLayersUI(radiocal=input$modeltype, selection=1)
    } else if(needNeuralNet()==FALSE){
        NULL
    }
})

neuralHiddenLayer <- reactive({
    tryCatch(input$neuralhiddenlayer,
        error=function(e)
            1)
})


output$neuralhiddenunitsui <- renderUI({
    req(input$modeltype)
    if(needNeuralNet()==TRUE){
        neuralHiddenUnitsUi(radiocal=input$modeltype, selection=c(2, 4))
    } else if(needNeuralNet()==FALSE){
        NULL
    }
})

neuralHiddenUnits <- reactive({
    if(modelType()=="ShallowNeuralNet"){
        paste0(input$neuralhiddenunits[1], "-", input$neuralhiddenunits[2])
    } else if(modelType()=="DeepNeuralNet"){
        paste0(input$neuralhiddenunits[1], "-", input$neuralhiddenunits[2])
    } else {
        NULL
    }
})


output$neuralweightdecayui <- renderUI({
    req(input$modeltype)
    if(needNeuralNet()==TRUE){
        neuralWeightDecayUI(radiocal=input$modeltype, selection=c(0.3, 0.5), neuralhiddenlayers=input$neuralhiddenlayers)
    } else if(needNeuralNet()==FALSE){
        NULL
    }
})

neuralWeightDecay <- reactive({
    if(modelType()=="ShallowNeuralNet"){
        paste0(input$neuralweightdecay[1], "-", input$neuralweightdecay[2])
    } else if(modelType()!="ShallowNeuralNet"){
        NULL
    }
})

output$neuralmaxiterationsui <- renderUI({
    req(input$modeltype)
    if(needNeuralNet()==TRUE){
        neuralMaxIterationsUI(radiocal=input$modeltype, selection=2000, neuralhiddenlayers=input$neuralhiddenlayers)
    } else if(needNeuralNet()==FALSE){
        NULL
    }
    
})

neuralMaxIterations <- reactive({
    tryCatch(input$neuralmaxiterations,
        error=function(e)
            1000)
})


output$xgbtypeui <- renderUI({
    req(input$modeltype)
    if(needXGBoost()==TRUE){
        xgbTypeUI(radiocal=input$modeltype, selection="Linear")
    } else if(needXGBoost()==FALSE){
        NULL
    }
})

xgbType <- reactive({
        if(needXGBoost()==TRUE){
        input$xgbtype
    } else if(needXGBoost()==FALSE){
        NULL
    }
})


output$usebayesui <- renderUI({
    req(input$modeltype)
    if(needXGBoost()==TRUE){
        checkboxInput("usebayes", "Use Bayesian Parameter Estimation", value=FALSE)
    } else {
        NULL
    }
})

useBayes <- reactive({
    if(needXGBoost()==TRUE){
        input$usebayes
    } else {
        FALSE
    }
})

output$foldsui <- renderUI({
    req(input$modeltype, input$usebayes)
    if(useBayes()==TRUE){
        sliderInput("folds", "K-folds", min=2, max=50, value=5)
    } else if(useBayes()==FALSE){
        NULL
    }
})

output$testingroundsui <- renderUI({
    req(input$modeltype)
    if(needXGBoost()==TRUE){
        sliderInput("testrounds", "Testing Rounds", min=2, max=1000, value=200)
    } else if(needXGBoost()==FALSE){
        NULL
    }
})

testRounds <- reactive({
    req(input$modeltype)
    if(needXGBoost()==TRUE){
        input$testrounds
    } else if(needXGBoost()==FALSE){
        NULL
    }
})

bayesFolds <- reactive({
    if(needXGBoost()==TRUE){
        input$folds
    } else if(needXGBoost()==FALSE){
        NULL
    }
})

output$init_pointsui <- renderUI({
    req(input$modeltype, input$usebayes)
    if(useBayes()==TRUE){
        sliderInput("init_points", "Initial Runs", min=2, max=150, value=50)
    } else if(useBayes()==FALSE){
        50
    }
})

bayesInitPoints <- reactive({
    if(needXGBoost()==TRUE){
        input$init_points
    } else if(needXGBoost()==FALSE){
        NULL
    }
})

output$n_iterui <- renderUI({
    req(input$modeltype, input$usebayes)
    if(useBayes()==TRUE){
        sliderInput("n_iter", "Bayesian Iterations", min=2, max=50, value=10)
    } else if(useBayes()==FALSE){
        NULL
    }
})

bayesNIter <- reactive({
    if(needXGBoost()==TRUE){
        input$n_iter
    } else if(needXGBoost()==FALSE){
        NULL
    }
})

output$treedepthui <- renderUI({
    req(input$modeltype)
    if(needXGBoost()==TRUE){
        req(input$xgbtype)
        if(input$usebayes==TRUE){
            treeDepthUI(radiocal=input$modeltype, selection=c(2, 40), xgbtype=input$xgbtype)
        } else if(input$usebayes==FALSE){
            treeDepthUI(radiocal=input$modeltype, selection=c(2, 7), xgbtype=input$xgbtype)
        }
    } else if(needXGBoost()==FALSE){
        NULL
    }
})

treeDepth <- reactive({
    if(modelType()=="XGBTree"){
        paste0(input$treedepth[1], "-", input$treedepth[2])
    } else if(modelType()!="XGBTree"){
        NULL
    }
})


output$xgbalphaui <- renderUI({
    req(input$modeltype)
    if(needXGBoost()==TRUE){
        req(input$xgbtype)
        if(input$usebayes==TRUE){
            xgbAlphaUI(radiocal=input$modeltype, selection=c(0, 10), xgbtype=input$xgbtype)
        } else if(input$usebayes==FALSE){
            xgbAlphaUI(radiocal=input$modeltype, selection=c(0.05, 0.95), xgbtype=input$xgbtype)
        }
    } else if(needXGBoost()==FALSE){
        NULL
    }
})

xgbAlpha <- reactive({
    if(modelType()=="XGBLinear"){
        paste0(input$xgbalpha[1], "-", input$xgbalpha[2])
    } else if(modelType()!="XGBLinear"){
        NULL
    }
})


output$xgbgammaui <- renderUI({
    req(input$modeltype)
    if(needXGBoost()==TRUE){
        req(input$xgbtype)
        if(input$usebayes==TRUE){
            xgbGammaUI(radiocal=input$modeltype, selection=c(0.0, 1), xgbtype=input$xgbtype)
        } else if(input$usebayes==FALSE){
            xgbGammaUI(radiocal=input$modeltype, selection=c(0.0, 1), xgbtype=input$xgbtype)
        }
    } else if(needXGBoost()==FALSE){
        NULL
    }
})

xgbGamma <- reactive({
    if(modelType()=="XGBTree"){
        paste0(input$xgbgamma[1], "-", input$xgbgamma[2])
    } else if(modelType()!="XGBTree"){
        NULL
    }
})

output$xgbetaui <- renderUI({
    req(input$modeltype)
    if(needXGBoost()==TRUE){
        req(input$xgbtype)
        if(input$usebayes==TRUE){
            xgbEtaUI(radiocal=input$modeltype, selection=c(0.01, 0.999))
        } else if(input$usebayes==FALSE){
            xgbEtaUI(radiocal=input$modeltype, selection=c(0.01, 0.99))
        }
    } else if(needXGBoost()==FALSE){
        NULL
    }
})

xgbEta <- reactive({
    if(modelType()=="XGBTree"){
        paste0(input$xgbeta[1], "-", input$xgbeta[2])
    } else if(modelType()=="XGBLinear"){
        paste0(input$xgbeta[1], "-", input$xgbeta[2])
    } else {
        NULL
    }
})

output$xgblambdaui <- renderUI({
    req(input$modeltype)
    if(needXGBoost()==TRUE){
        req(input$xgbtype)
        if(input$usebayes==TRUE){
            xgbLambdaUI(radiocal=input$modeltype, selection=c(0, 10), xgbtype=input$xgbtype)
        } else if(input$usebayes==FALSE){
            xgbLambdaUI(radiocal=input$modeltype, selection=c(0.05, 0.95), xgbtype=input$xgbtype)
        }
    } else if(needXGBoost()==FALSE){
        NULL
    }
})

xgbLambda <- reactive({
    if(modelType()=="XGBLinear"){
        paste0(input$xgblambda[1], "-", input$xgblambda[2])
    } else if(modelType()!="XGBLinear"){
        NULL
    }
})


output$xgbsubsampleui <- renderUI({
    req(input$modeltype)
    if(needXGBoost()==TRUE){
        req(input$xgbtype)
        if(input$usebayes==TRUE){
            xgbSubSampleUI(radiocal=input$modeltype, selection=c(0.05, 0.95), xgbtype=input$xgbtype)
        } else if(input$usebayes==FALSE){
            xgbSubSampleUI(radiocal=input$modeltype, selection=c(0.05, 0.95), xgbtype=input$xgbtype)
        }
    } else if(needXGBoost()==FALSE){
        NULL
    }
})

xgbSubSample <- reactive({
    if(modelType()=="XGBTree"){
        paste0(input$xgbsubsample[1], "-", input$xgbsubsample[2])
    } else if(modelType()!="XGBTree"){
        NULL
    }
})


output$xgbcolsampleui <- renderUI({
    req(input$modeltype)
    if(needXGBoost()==TRUE){
        req(input$xgbtype)
        if(input$usebayes==TRUE){
            xgbColSampleUI(radiocal=input$modeltype, selection=c(0.05, 0.95), xgbtype=input$xgbtype)
        } else if(input$usebayes==FALSE){
            xgbColSampleUI(radiocal=input$modeltype, selection=c(0.05, 0.95), xgbtype=input$xgbtype)
        }
    } else if(needXGBoost()==FALSE){
        NULL
    }
})

xgbColSample <- reactive({
    if(modelType()=="XGBTree"){
        paste0(input$xgbcolsample[1], "-", input$xgbcolsample[2])
    } else if(modelType()!="XGBTree"){
        NULL
    }
})

output$xgbminchildui <- renderUI({
    req(input$modeltype)
    if(needXGBoost()==TRUE){
        req(input$xgbtype)
        xgbMinChildUI(radiocal=input$modeltype, selection=c(1, 3), xgbtype=input$xgbtype)
    } else if(needXGBoost()==FALSE){
        NULL
    }
})

xgbMinChild <- reactive({
    if(modelType()=="XGBTree"){
        paste0(input$xgbminchild[1], "-", input$xgbminchild[2])
    } else if(modelType()!="XGBTree"){
        NULL
    }
})

output$paralleltypeui <- renderUI({
    selectInput("paralleltype", "Parallel Processing Type", choices=c("windows", "osx", "linux"), selected=get_os())

})

Model <- reactive({
    
    
        withProgress(message = 'Running Model', value = 0, {

            model <- autoMLTable(data=dataMerge3(), variable=input$variable, predictors=input$predictors, min.n=minN(), split=mlSplit()/100, type=modelType(), treedepth=treeDepth(), xgbalpha=xgbAlpha(), xgbeta=xgbEta(), xgbgamma=xgbGamma(), xgblambda=xgbLambda(), xgbcolsample=xgbColSample(), xgbsubsample=xgbSubSample(), xgbminchild=xgbMinChild(), nrounds=forestTrees(), test_nrounds=testRounds(), try=forestTry(), trees=forestTrees(), metric=forestMetric(), train=forestTrain(), cvrepeats=cvRepeats(), number=forestNumber(), Bayes=useBayes(), folds=bayesFolds(), init_points=bayesInitPoints(), n_iter=bayesNIter(), parallelMethod=input$paralleltype)
            #model <- autoMLTable(data=dataMerge3(), variable=input$variable, predictors=input$predictors, min.n=minN(), split=mlSplit()/100, type=modelType(), treedepth=treeDepth(), xgbalpha=xgbAlpha(), xgbeta=xgbEta(), xgbgamma=xgbGamma(), xgblambda=xgbLambda(), xgbcolsample=xgbColSample(), xgbsubsample=xgbSubSample(), xgbminchild=xgbMinChild(), nrounds=forestTrees(), test_nrounds=testRounds(), try=forestTry(), trees=forestTrees(), metric=forestMetric(), train=forestTrain(), cvrepeats=cvRepeats(), number=forestNumber(), Bayes=TRUE, folds=30, init_points=10, n_iter=2, parallelMethod=input$paralleltype)

        incProgress(1/1)
        })
        
        model
})

model_run <- reactiveValues()
model_run$ran <- NULL

observeEvent(input$runmodel, {
    isolate(model_run$ran <- Model())
})

resultePlot <- reactive({
    model_run$ran$ResultPlot
    #Model()$ImportancePlot
})

output$resultplot <- renderPlot({
    print(resultePlot())
})

importancePlot <- reactive({
    model_run$ran$ImportancePlot
    #Model()$ImportancePlot
})

output$importanceplot <- renderPlot({
    print(importancePlot())
})

output$modeldownload <- downloadHandler(
filename <- function(){
    paste(input$qualname, "qual", sep=".")
},

content = function(file) {
    saveRDS(Model(), file = file, compress="xz")
}
)

output$downloadimportance <- downloadHandler(
filename = function() { paste(input$qualname, '_Importance.tiff',  sep='') },
content = function(file) {
    ggsave(file,Model()$ImportancePlot, device="tiff", compression="lzw",  dpi=300, width=input$importancewidth, height=input$importanceheight)
}
)

output$downloadresult <- downloadHandler(
filename = function() { paste(input$qualname, '_Result.tiff',  sep='') },
content = function(file) {
    ggsave(file,Model()$ResultPlot, device="tiff", compression="lzw",  dpi=300, width=input$resultwidth, height=input$resultheight)
}
)



})

})

