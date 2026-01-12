get_os <- function(){
    sysinf <- Sys.info()
    if (!is.null(sysinf)){
        os <- sysinf['sysname']
        if (os == 'Darwin')
        os <- "osx"
    } else { ## mystery machine
        os <- .Platform$OS.type
        if (grepl("^darwin", R.version$os))
        os <- "osx"
        if (grepl("linux-gnu", R.version$os))
        os <- "linux"
    }
    tolower(os)
}

#options(repos = BiocInstaller::biocinstallRepos())
#getOption("repos")
#options(download.file.method="libcurl", url.method="libcurl")
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
list.of.bioconductor <- c("graph", "RBGL", "Rgraphviz")
new.bioconductor <- list.of.bioconductor[!(list.of.bioconductor %in% installed.packages()[,"Package"])]
#if(length(new.bioconductor)) source("https://www.bioconductor.org/biocLite.R")
if(length(new.bioconductor)) BiocManager::install(new.bioconductor)


list.of.packages <- c("pbapply", "reshape2", "TTR", "dplyr", "ggplot2", "shiny", "rhandsontable", "random", "data.table", "DT", "shinythemes", "Cairo", "gghighlight", "scales", "openxlsx", "devtools", "digest", "tibble", "purrr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#if(packageVersion("ggplot2")!="2.2.1") devtools::install_version("ggplot2", version = "2.2.1", repos = "http://cran.us.r-project.org", checkBuilt=TRUE)
#if(packageVersion("gghighlight")!="0.0.1") devtools::install_version("gghighlight", version = "0.0.1", repos = "http://cran.us.r-project.org", checkBuilt=TRUE)
#if(packageVersion("ggtern")!="2.2.0") devtools::install_version("ggtern", version = "2.2.0", repos = "http://cran.us.r-project.org", checkBuilt=TRUE)



if("rPDZ" %in% installed.packages()[,"Package"]==FALSE && get_os()=="windows"){
    tryCatch(install.packages("http://www.xrf.guru/packages/rPDZ_1.0.zip", repos=NULL, type="win.binary"), error=function(e) NULL)
} else if ("rPDZ" %in% installed.packages()[,"Package"]==FALSE && get_os()=="osx"){
    tryCatch(install.packages("http://www.xrf.guru/packages/rPDZ_1.0.tgz", repos=NULL), error=function(e) NULL)
} else if ("rPDZ" %in% installed.packages()[,"Package"]==FALSE && get_os()=="linux"){
    tryCatch(install.packages("http://www.xrf.guru/packages/rPDZ_1.0.tar.gz", repos=NULL), error=function(e) NULL)
}

tryCatch(library(rPDZ), error=function(e) NULL)


library(pbapply)
library(reshape2)
#library(TTR)
library(dplyr)
#library(ggtern)
library(ggplot2)
library(shiny)
#library(gRbase)

# Element choices for selectInput dropdowns (used in multiple places in ui.R)
element_choices <- c(
    "(Ne) Neon" = "Ne.table",
    "(Na) Sodium" = "Na.table",
    "(Mg) Magnesium" = "Mg.table",
    "(Al) Aluminum" = "Al.table",
    "(Si) Silicon" = "Si.table",
    "(P)  Phosphorous" = "P.table",
    "(S)  Sulfur" = "S.table",
    "(Cl) Chlorine" = "Cl.table",
    "(Ar) Argon" = "Ar.table",
    "(K)  Potassium" = "K.table",
    "(Ca) Calcium" = "Ca.table",
    "(Sc) Scandium" = "Sc.table",
    "(Ti) Titanium" = "Ti.table",
    "(V)  Vanadium" = "V.table",
    "(Cr) Chromium" = "Cr.table",
    "(Mn) Manganese" = "Mn.table",
    "(Fe) Iron" = "Fe.table",
    "(Co) Cobalt" = "Co.table",
    "(Ni) Nickel" = "Ni.table",
    "(Cu) Copper" = "Cu.table",
    "(Zn) Zinc" = "Zn.table",
    "(Ga) Gallium" = "Ga.table",
    "(Ge) Germanium" = "Ge.table",
    "(As) Arsenic" = "As.table",
    "(Se) Selenium" = "Se.table",
    "(Br) Bromium" = "Br.table",
    "(Kr) Krypton" = "Kr.table",
    "(Rb) Rubidium" = "Rb.table",
    "(Sr) Strontium" = "Sr.table",
    "(Y)  Yttrium" = "Y.table",
    "(Zr) Zirconium" = "Zr.table",
    "(Nb) Niobium" = "Nb.table",
    "(Mo) Molybdenum" = "Mo.table",
    "(Tc) Technicium" = "Tc.table",
    "(Ru) Ruthenium" = "Ru.table",
    "(Rh) Rhodium" = "Rh.table",
    "(Pd) Paladium" = "Pd.table",
    "(Ag) Silver" = "Ag.table",
    "(Cd) Cadmium" = "Cd.table",
    "(In) Indium" = "In.table",
    "(Sn) Tin" = "Sn.table",
    "(Sb) Antimony" = "Sb.table",
    "(Te) Tellerium" = "Te.table",
    "(I) Iodine" = "I.table",
    "(Xe) Xenon" = "Xe.table",
    "(Cs) Cesium" = "Cs.table",
    "(Bs) Barium" = "Ba.table",
    "(Ce) Cerium" = "Ce.table",
    "(Pr) Praeseodymeum" = "Pr.table",
    "(Nd) Neodymeum" = "Nd.table",
    "(Pr) Promethium" = "Pr.table",
    "(Sm) Samarium" = "Sm.table",
    "(Eu) Europium" = "Eu.table",
    "(Gd) Gadolinium" = "Gd.table",
    "(Tb) Terbium" = "Tb.table",
    "(Dy) Dysprosium" = "Dy.table",
    "(Ho) Holmium" = "Ho.table",
    "(Er) Erbium" = "Er.table",
    "(Tm) Thullium" = "Tm.table",
    "(Yb) Ytterbium" = "Yb.table",
    "(Lu) Lutetium" = "Lu.table",
    "(Hf) Halfnium" = "Hf.table",
    "(Ta) Tantalum" = "Ta.table",
    "(W)  Tungsten" = "W.table",
    "(Re) Rhenium" = "Re.table",
    "(Os) Osmium" = "Os.table",
    "(Ir) Irridium" = "Ir.table",
    "(Pt) Platinum" = "Pt.table",
    "(Au) Gold" = "Au.table",
    "(Hg) Mercury" = "Hg.table",
    "(Tl) Thallium" = "Tl.table",
    "(Pb) Lead" = "Pb.table",
    "(Bi) Bismuth" = "Bi.table",
    "(Po) Polonium" = "Po.table",
    "(At) Astatine" = "At.table",
    "(Rn) Radon" = "Rn.table",
    "(Fr) Francium" = "Fr.table",
    "(Ra) Radium" = "Ra.table",
    "(Ac) Actinum" = "Ac.table",
    "(Th) Thorium" = "Th.table",
    "(Pa) Proactinum" = "Pa.table",
    "(U)  Uranium" = "U.table"
)

layOut = function(...) {
    
    require(grid)
    
    x <- list(...)
    n <- max(sapply(x, function(x) max(x[[2]])))
    p <- max(sapply(x, function(x) max(x[[3]])))
    pushViewport(viewport(layout = grid.layout(n, p)))
    
    for (i in seq_len(length(x))) {
        print(x[[i]][[1]], vp = viewport(layout.pos.row = x[[i]][[2]],
        layout.pos.col = x[[i]][[3]]))
    }
} 

Hodder.v <- function(y)
{
    
    n<-length(y)
    
    for(i in 1:(n-1)) {
        y[i] <- y[i+1] - y[i]
        y[1:(n-1)]
        y <- y
    }
    y <- c(0, y[1:(n-1)])
    
    return(y)
}



read_csv_filename_x <- function(filename){
    ret <- read.csv(file=filename, sep=",", header=FALSE)
    return.res <- as.numeric(as.vector(ret$V2[18]))/1000
    return.chan.counts <-as.numeric(as.vector(ret$V1[22:2069]))
    return.energy <- return.chan.counts*return.res
    return(return.energy)
}

read_csv_filename_y <- function(filename){
    ret <- read.csv(file=filename, sep=",", header=FALSE)
    return.live.time <- as.numeric(as.vector(ret$V2[10]))
    return.counts <- as.numeric(as.vector(ret$V2[22:2069]))
    return.cps <- return.counts/return.live.time
    return(return.cps)
}



read_csv_filename_x <- function(filename){
    ret <- read.csv(file=filename, sep=",", header=FALSE)
    return.res <- as.numeric(as.vector(ret$V2[18]))/1000
    return.chan.counts <-as.numeric(as.vector(ret$V1[22:2069]))
    return.energy <- return.chan.counts*return.res
    return(return.energy)
}

read_csv_filename_y <- function(filename){
    ret <- read.csv(file=filename, sep=",", header=FALSE)
    return.live.time <- as.numeric(as.vector(ret$V2[10]))
    return.counts <- as.numeric(as.vector(ret$V2[22:2069]))
    return.cps <- return.counts/return.live.time
    return(return.cps)
}

csvFrame <- function(filepath, filename){
    
    data.frame(Energy=read_csv_filename_x(filepath), CPS=read_csv_filename_y(filepath), Spectrum=rep(filename, length(read_csv_filename_x(filepath))))
    
}

read_csv_net <- function(filepath) {
    
    ret <- read.csv(file=filepath, sep=",", header=TRUE)
    element <- ret$Element
    line <- ret$Line
    net <- ret$Net
    background <- ret$Backgr.
    eline <- paste(element, line, sep="-")
    
    simple.table <- data.frame(net)
    colnames(simple.table) <- NULL
    simple.transpose <- as.data.frame(t(simple.table))
    colnames(simple.transpose) <- eline
    
    simple.transpose
    
}




readSPTData <- function(filepath, filename){
    filename <- gsub(".spt", "", filename)
    filename.vector <- rep(filename, 4096)
    
    meta <- paste0(readLines(filepath, n=16),collapse=" ")
    meta.split <- strsplit(meta, " ")
    chan.1 <- as.numeric(meta.split[[1]][32])
    energy.1 <- as.numeric(sub(",", ".", meta.split[[1]][33], fixed = TRUE))
    chan.2 <- as.numeric(meta.split[[1]][34])
    energy.2 <- as.numeric(sub(",", ".", meta.split[[1]][35], fixed = TRUE))
    
    channels <- c(chan.1, chan.2)
    energies <- c(energy.1, energy.2)
    
    energy.cal <- lm(energies~ channels)
    
    time <- as.numeric(meta.split[[1]][17])/1000
    
    raw <- read.table(filepath, skip=16)
    cps <- raw[,1]/time
    newdata <- as.data.frame(seq(1, 4096, 1))
    colnames(newdata) <- "channels"
    energy <- as.vector(predict.lm(energy.cal, newdata=newdata))
    energy2 <- newdata[,1]*summary(energy.cal)$coef[2]
    spectra.frame <- data.frame(energy, cps, filename.vector)
    colnames(spectra.frame) <- c("Energy", "CPS", "Spectrum")
    return(spectra.frame)
}


readMCAData <- function(filepath, filename){
    filename <- gsub(".mca", "", filename)
    filename.vector <- rep(filename, 4096)
    
    full <- read.csv(filepath, row.names=NULL)
    
    chan.1.a.pre <- as.numeric(unlist(strsplit(gsub("# Calibration1: ", "", full[13,1]), " ")))
    chan.1.b.pre <- as.numeric(full[13,2])
    chan.2.a.pre <- as.numeric(unlist(strsplit(gsub("# Calibration2: ", "", full[14,1]), " ")))
    chan.2.b.pre <- as.numeric(full[14,2])
    
    
    chan.1 <- chan.1.a.pre[1]
    energy.1 <- chan.1.a.pre[2] + chan.1.b.pre/(10^nchar(chan.1.b.pre))
    chan.2 <- chan.2.a.pre[1]
    energy.2 <- chan.2.a.pre[2] + chan.2.b.pre/(10^nchar(chan.2.b.pre))
    
    channels <- c(chan.1, chan.2)
    energies <- c(energy.1, energy.2)
    
    energy.cal <- lm(energies~channels)
    
    time.1 <- as.numeric(gsub("# Live time: ", "", full[10,1], " "))
    time.2 <- as.numeric(full[10,2])
    time <- time.1 + time.2/(10^nchar(time.2))
    
    cps <- as.numeric(full[17:4112, 1])/time
    newdata <- as.data.frame(seq(1, 4096, 1))
    colnames(newdata) <- "channels"
    energy <- as.vector(predict.lm(energy.cal, newdata=newdata))
    energy2 <- newdata[,1]*summary(energy.cal)$coef[2]
    spectra.frame <- data.frame(energy, cps, filename.vector)
    colnames(spectra.frame) <- c("Energy", "CPS", "Spectrum")
    return(spectra.frame)
}


readSPXData <- function(filepath, filename){
    
    filename <- gsub(".spx", "", filename)
    filename.vector <- rep(filename, 4096)
    
    xmlfile <- xmlTreeParse(filepath)
    xmllist <- xmlToList(xmlfile)
    channels.pre <- xmllist[["ClassInstance"]][["Channels"]][[1]]
    counts <- as.numeric(strsplit(channels.pre, ",", )[[1]])
    newdata <- as.data.frame(seq(1, 4096, 1))
    intercept <- as.numeric(xmllist[["ClassInstance"]][["ClassInstance"]][["CalibAbs"]])
    slope <- as.numeric(xmllist[["ClassInstance"]][["ClassInstance"]][["CalibLin"]])
    time <- as.numeric(xmllist[[2]][["TRTHeaderedClass"]][[3]][["LifeTime"]])/1000
    
    cps <- counts/time
    energy <- newdata[,1]*slope+intercept
    
    spectra.frame <- data.frame(energy, cps, filename.vector)
    colnames(spectra.frame) <- c("Energy", "CPS", "Spectrum")
    return(spectra.frame)
    
}


readPDZ25DataExpiremental <- function(filepath, filename){
    
    filename <- gsub(".pdz", "", filename)
    filename.vector <- rep(filename, 2048)
    
    nbrOfRecords <- 3000
    integers <- int_to_unit(readBin(con=filepath, what= "int", n=3000, endian="little"))
    floats <- readBin(con=filepath, what="float", size=4, n=nbrOfRecords, endian="little")
    integer.sub <- integers[124:2171]
    
    sequence <- seq(1, length(integer.sub), 1)
    
    time.est <- integers[144]/10
    
    channels <- sequence
    energy <- sequence*.02
    counts <- integer.sub/(integers[144]/10)
    
    unfold(data.frame(Energy=energy, CPS=counts, Spectrum=filename.vector))
    
}


readPDZ24DataExpiremental <- function(filepath, filename){
    
    filename <- gsub(".pdz", "", filename)
    filename.vector <- rep(filename, 2048)
    
    nbrOfRecords <- 3000
    integers <- int_to_unit(readBin(con=filepath, what= "int", n=3000, endian="little"))
    floats <- readBin(con=filepath, what="float", size=4, n=nbrOfRecords, endian="little")
    integer.sub <- integers[90:2137]
    sequence <- seq(1, length(integer.sub), 1)
    
    time.est <- integer.sub[21]
    
    channels <- sequence
    energy <- sequence*.02
    counts <- integer.sub/(integer.sub[21]/10)
    
    unfold(data.frame(Energy=energy, CPS=counts, Spectrum=filename.vector))
    
}


#Rcpp::sourceCpp("pdz.cpp")

readPDZ25Data <- function(filepath, filename){
    
    filename <- gsub(".pdz", "", filename)
    filename.vector <- rep(filename, 2020)
    
    nbrOfRecords <- 2020
    integers <- readPDZ25(filepath, start=481, size=nbrOfRecords)
    
    sequence <- seq(1, length(integers), 1)
    
    time.est <- integers[21]
    
    channels <- sequence
    energy <- sequence*.02
    counts <- integers/(integers[21]/10)
    
    data.frame(Energy=energy, CPS=counts, Spectrum=filename.vector)
    
}


readPDZ24Data<- function(filepath, filename){
    
    filename <- gsub(".pdz", "", filename)
    filename.vector <- rep(filename, 2020)
    
    nbrOfRecords <- 2020
    integers <- readPDZ24(filepath, start=357, size=nbrOfRecords)
    sequence <- seq(1, length(integers), 1)
    
    time.est <- integers[21]
    
    channels <- sequence
    energy <- sequence*.02
    counts <- integers/(integers[21]/10)
    
    data.frame(Energy=energy, CPS=counts, Spectrum=filename.vector)
    
}



readPDZ25DataManual <- function(filepath, filename, binaryshift){
    
    filename <- gsub(".pdz", "", filename)
    filename.vector <- rep(filename, 2020)
    
    nbrOfRecords <- 2020
    integers <- readPDZ25(filepath, start=binaryshift, size=nbrOfRecords)
    
    sequence <- seq(1, length(integers), 1)
    
    time.est <- integers[21]
    
    channels <- sequence
    energy <- sequence*.02
    counts <- integers/(integers[144]/10)
    
    data.frame(Energy=energy, CPS=counts, Spectrum=filename.vector)
    
}



readPDZData <- function(filepath, filename) {
    nbrOfRecords <- 10000
    
    
    floats <- readBin(con=filepath, what="float", size=4, n=nbrOfRecords, endian="little")
    
    if(floats[[9]]=="5"){
        readPDZ25Data(filepath, filename)
    }else {
        readPDZ24Data(filepath, filename)
    }
    
    
}

file.0 <- function(file) {
    if (length(file) > 0)
    {
    return(file)
    }else{
        return(levels(file))
    }
}

is.0 <- function(cps, file) {
    file.0 <- function(file) {
        if (length(file) > 0)
        {
            return(file)
        }else{
            return(levels(file))
        }
    }
    if (length(cps) > 0)
    {
        hope <-data.frame(cps, file.0(file))
        return(hope)
    } else {
        empty <- rep(0, length(file.0(file)))
        framed <- data.frame(empty, file.0(file))
        return(framed)
    }
}

dt_options <- reactive({
    # dynamically create options for `aoColumns` depending on how many columns are selected.
    toggles <- lapply(1:length(input$show_vars), function(x) list(bSearchable = F))
    # for `species` columns
    toggles[[length(toggles) + 1]] <- list(bSearchable = T)
    
    list(
    aoColumns = toggles,
    bFilter = 1, bSortClasses = 1,
    aLengthMenu = list(c(10,25,50, -1), list('10','25', '50', 'Todas')),
    iDisplayLength = 10
    )
})

ifrm <- function(obj, env = globalenv()) {
    obj <- deparse(substitute(obj))
    if(exists(obj, envir = env)) {
        rm(list = obj, envir = env)
    }
}

scree_crunch <- function(dataframe, dependent, independent){
    
    simple.frame <- data.frame(
    newY = dataframe[,dependent]/max(dataframe[,dependent]),
    newX = dataframe[,independent]/max(dataframe[,independent]))
    
    sims <-data.frame(
    sims1 = seq(from=1, to=nrow(dataframe)-1, by=1),
    sims2 = seq(from=2, to=nrow(dataframe), by=1)
    )
    
    n <- seq(from=1, to=nrow(sims), by=1)
    
    lm.sims <- pbapply::pblapply(n, function(x) summary(lm(newY~newX, data=simple.frame[sims[,1][x]:sims[,2][x],])))
    
    
    slopes <- unlist(pbapply::pblapply(n, function(x) as.vector(lm.sims[[x]]["coefficients"][[1]][2])))
    
    #rsquared <- unlist(pbapply::pblapply(n, function(x) as.vector(lm.sims[[x]]["r.squared"])))
    
    greater.1 <- which(abs(slopes) > 1)
    
    greater.1[length(greater.1)]+1
    
    
}

black.diamond <- read.csv("data/blackdiamond.csv", header=FALSE, sep=",")
black.diamond.melt <- read.csv(file="data/blackdiamondmelt.csv")


my.cores <- if(parallel::detectCores()>=3){
    paste0(parallel::detectCores()-2)
} else if(parallel::detectCores()<=2){
    "1"
}


spectralLines <- c("Ne.K.alpha", "Ne.K.beta", "Na.K.alpha", "Na.K.beta", "Mg.K.alpha", "Mg.K.beta", "Al.K.alpha", "Al.K.beta", "Si.K.alpha", "Si.K.beta", "P.K.alpha", "P.K.beta", "S.K.alpha", "S.K.beta", "Cl.K.alpha", "Cl.K.beta", "Ar.K.alpha", "Ar.K.beta", "K.K.alpha", "K.K.beta", "Ca.K.alpha", "Ca.K.beta", "Sc.K.alpha", "Sc.K.beta", "Ti.K.alpha", "Ti.K.beta", "V.K.alpha", "V.K.beta", "Cr.K.alpha", "Cr.K.beta", "Mn.K.alpha", "Mn.K.beta", "Fe.K.alpha", "Fe.K.beta", "Co.K.alpha", "Co.K.beta", "Ni.K.alpha", "Ni.K.beta", "Cu.K.alpha", "Cu.K.beta", "Zn.K.alpha", "Zn.K.beta", "Ga.K.alpha", "Ga.K.beta", "Ge.K.alpha", "Ge.K.beta", "As.K.alpha", "As.K.beta", "Se.K.alpha", "Se.K.beta", "Br.K.alpha", "Br.K.beta", "Kr.K.alpha", "Kr.K.beta", "Rb.K.alpha", "Rb.K.beta", "Sr.K.alpha", "Sr.K.beta", "Y.K.alpha", "Y.K.beta", "Zr.K.alpha", "Zr.K.beta", "Nb.K.alpha", "Nb.K.beta", "Mo.K.alpha", "Mo.K.beta", "Mo.L.alpha", "Mo.L.beta", "Ru.K.alpha", "Ru.K.beta", "Ru.L.alpha", "Ru.L.beta", "Rh.K.alpha", "Rh.K.beta", "Rh.L.alpha", "Rh.L.beta", "Pd.K.alpha", "Pd.K.beta", "Pd.L.alpha", "Pd.L.beta", "Ag.K.alpha", "Ag.K.beta", "Ag.L.alpha", "Ag.L.beta", "Cd.K.alpha", "Cd.K.beta", "Cd.L.alpha", "Cd.L.beta", "In.K.alpha", "In.K.beta", "In.L.alpha", "Sn.K.alpha", "Sn.K.beta", "Sn.L.alpha", "Sn.L.beta", "Sb.K.alpha", "Sb.K.beta", "Sb.L.alpha", "Sb.L.beta", "Te.K.alpha", "Te.K.beta", "Te.L.alpha", "Te.L.beta", "I.K.alpha", "I.K.beta", "I.L.alpha", "I.L.beta", "Xe.K.alpha", "Xe.K.beta", "Xe.L.alpha", "Xe.L.beta", "Cs.K.alpha", "Cs.K.beta", "Cs.L.alpha", "Cs.L.beta", "Ba.K.alpha", "Ba.K.beta", "Ba.L.alpha", "Ba.L.beta", "La.K.alpha", "La.K.beta", "La.L.alpha", "La.L.beta", "Ce.K.alpha", "Ce.K.beta", "Ce.L.alpha", "Ce.L.beta", "Pr.K.alpha", "Pr.K.beta", "Pr.L.alpha", "Pr.L.beta", "Nd.K.alpha", "Nd.K.beta", "Nd.L.alpha", "Nd.L.beta", "Pm.L.alpha", "Pm.L.beta", "Sm.L.alpha", "Sm.L.beta", "Eu.L.alpha", "Eu.L.beta", "Gd.L.alpha", "Gd.L.beta", "Tb.L.alpha", "Tb.L.beta", "Dy.L.alpha", "Dy.L.beta", "Ho.L.alpha", "Ho.L.beta", "Er.L.alpha", "Er.L.beta", "Tm.L.alpha", "Tm.L.beta", "Yb.L.alpha", "Yb.L.beta", "Lu.L.alpha", "Lu.L.beta", "Hf.L.alpha", "Hf.L.beta", "Ta.L.alpha", "Ta.L.beta", "W.L.alpha", "W.L.beta", "Re.L.alpha", "Re.L.beta", "Os.L.alpha", "Os.L.beta", "Ir.L.alpha", "Ir.L.beta", "Pt.L.alpha", "Pt.L.beta", "Au.L.alpha", "Au.L.beta", "Hg.L.alpha", "Hg.L.beta", "Tl.L.alpha", "Tl.L.beta", "Pb.L.alpha", "Pb.L.beta", "Bi.L.alpha", "Bi.L.beta", "Po.L.alpha", "Po.L.beta", "At.L.alpha", "At.L.beta", "Rn.L.alpha", "Rn.L.beta", "Fr.L.alpha", "Fr.L.beta", "Ra.L.alpha", "Ra.L.beta", "Ac.L.alpha", "Ac.L.beta", "Th.L.alpha", "Th.L.beta", "Pa.L.alpha", "Pa.L.beta", "U.L.alpha", "U.L.beta", "Pu.L.alpha", "Pu.L.beta", "Au.M.line", "Hg.M.line", "Pb.M.line", "U.M.line")

standard <- c("Spectrum", "Ca.K.alpha", "Ti.K.alpha", "Fe.K.alpha")

kalphaLines <- c("Na"="Na.K.alpha",  "Mg"="Mg.K.alpha", "Al"="Al.K.alpha", "Si"="Si.K.alpha", "P"="P.K.alpha", "S"="S.K.alpha", "Cl"="Cl.K.alpha", "Ar"="Ar.K.alpha", "K"="K.K.alpha", "Ca"="Ca.K.alpha", "Sc"="Sc.K.alpha", "Ti"="Ti.K.alpha", "V"="V.K.alpha", "Cr"="Cr.K.alpha", "Mn"="Mn.K.alpha", "Fe"="Fe.K.alpha", "Co"="Co.K.alpha", "Ni"="Ni.K.alpha", "Cu"="Cu.K.alpha", "Zn"="Zn.K.alpha", "Ga"="Ga.K.alpha", "Ge"="Ge.K.alpha", "As"="As.K.alpha", "Se"="Se.K.alpha", "Br"="Br.K.alpha", "Kr"="Kr.K.alpha", "Rb"="Rb.K.alpha", "Sr"="Sr.K.alpha", "Y"="Y.K.alpha", "Zr"="Zr.K.alpha", "Nb"="Nb.K.alpha", "Mo"="Mo.K.alpha", "Ru"="Ru.K.alpha", "Rh"="Rh.K.alpha", "Pd"="Pd.K.alpha", "Ag"="Ag.K.alpha", "Cd"="Cd.K.alpha", "In"="In.K.alpha", "Sn"="Sn.K.alpha", "Sb"="Sb.K.alpha", "Te"="Te.K.alpha", "I"="I.K.alpha", "Xe"="Xe.K.alpha", "Cs"="Cs.K.alpha", "Ba"="Ba.K.alpha", "La"="La.K.alpha", "Ce"="Ce.K.alpha", "Pr"="Pr.K.alpha", "Nd"="Nd.K.alpha")

kbetaLines <- c("Na"="Na.K.beta",  "Mg"="Mg.K.beta", "Al"="Al.K.beta", "Si"="Si.K.beta", "P"="P.K.beta", "S"="S.K.beta", "Cl"="Cl.K.beta", "Ar"="Ar.K.beta", "K"="K.K.beta", "Ca"="Ca.K.beta", "Sc"="Sc.K.beta", "Ti"="Ti.K.beta", "V"="V.K.beta", "Cr"="Cr.K.beta", "Mn"="Mn.K.beta", "Fe"="Fe.K.beta", "Co"="Co.K.beta", "Ni"="Ni.K.beta", "Cu"="Cu.K.beta", "Zn"="Zn.K.beta", "Ga"="Ga.K.beta", "Ge"="Ge.K.beta", "As"="As.K.beta", "Se"="Se.K.beta", "Br"="Br.K.beta", "Kr"="Kr.K.beta", "Rb"="Rb.K.beta", "Sr"="Sr.K.beta", "Y"="Y.K.beta", "Zr"="Zr.K.beta", "Nb"="Nb.K.beta", "Mo"="Mo.K.beta", "Ru"="Ru.K.beta", "Rh"="Rh.K.beta", "Pd"="Pd.K.beta", "Ag"="Ag.K.beta", "Cd"="Cd.K.beta", "In"="In.K.beta", "Sn"="Sn.K.beta", "Sb"="Sb.K.beta", "Te"="Te.K.beta", "I"="I.K.beta", "Xe"="Xe.K.beta", "Cs"="Cs.K.beta", "Ba"="Ba.K.beta", "La"="La.K.beta", "Ce"="Ce.K.beta", "Pr"="Pr.K.beta", "Nd"="Nd.K.beta")

lalphaLines <- c("Mo"="Mo.L.alpha", "Ru"="Ru.L.alpha", "Rh"="Rh.L.alpha", "Pd"="Pd.L.alpha", "Ag"="Ag.L.alpha", "Cd"="Cd.L.alpha", "In"="In.L.alpha", "Sn"="Sn.L.alpha", "Sb"="Sb.L.alpha", "Te"="Te.L.alpha", "I"="I.L.alpha", "Xe"="Xe.L.alpha", "Cs"="Cs.L.alpha", "Ba"="Ba.L.alpha", "La"="La.L.alpha", "Ce"="Ce.L.alpha", "Pr"="Pr.L.alpha", "Nd"="Nd.L.alpha", "Pm"="Pm.L.alpha", "Sm"="Sm.L.alpha", "Eu"="Eu.L.alpha", "Gd"="Gd.L.alpha", "Tb"="Tb.L.alpha", "Dy"="Dy.L.alpha", "Ho"="Ho.L.alpha", "Er"="Er.L.alpha", "Tm"="Tm.L.alpha", "Yb"="Yb.L.alpha", "Lu"="Lu.L.alpha", "Hf"="Hf.L.alpha", "Ta"="Ta.L.alpha", "W"="W.L.alpha", "Re"="Re.L.alpha", "Os"="Os.L.alpha", "Ir"="Ir.L.alpha", "Pt"="Pt.L.alpha", "Au"="Au.L.alpha", "Hg"="Hg.L.alpha", "Tl"="Tl.L.alpha", "Pb"="Pb.L.alpha", "Bi"="Bi.L.alpha", "Po"="Po.L.alpha", "At"="At.L.alpha", "Rn"="Rn.L.alpha", "Fr"="Fr.L.alpha", "Ra"="Ra.L.alpha", "Ac"="Ac.L.alpha", "Th"="Th.L.alpha", "Pa"="Pa.L.alpha", "U"="U.L.alpha")

lbetaLines <- c("Mo"="Mo.L.beta", "Ru"="Ru.L.beta", "Rh"="Rh.L.beta", "Pd"="Pd.L.beta", "Ag"="Ag.L.beta", "Cd"="Cd.L.beta", "In"="In.L.beta", "Sn"="Sn.L.beta", "Sb"="Sb.L.beta", "Te"="Te.L.beta", "I"="I.L.beta", "Xe"="Xe.L.beta", "Cs"="Cs.L.beta", "Ba"="Ba.L.beta", "La"="La.L.beta", "Ce"="Ce.L.beta", "Pr"="Pr.L.beta", "Nd"="Nd.L.beta", "Pm"="Pm.L.beta", "Sm"="Sm.L.beta", "Eu"="Eu.L.beta", "Gd"="Gd.L.beta", "Tb"="Tb.L.beta", "Dy"="Dy.L.beta", "Ho"="Ho.L.beta", "Er"="Er.L.beta", "Tm"="Tm.L.beta", "Yb"="Yb.L.beta", "Lu"="Lu.L.beta", "Hf"="Hf.L.beta", "Ta"="Ta.L.beta", "W"="W.L.beta", "Re"="Re.L.beta", "Os"="Os.L.beta", "Ir"="Ir.L.beta", "Pt"="Pt.L.beta", "Au"="Au.L.beta", "Hg"="Hg.L.beta", "Tl"="Tl.L.beta", "Pb"="Pb.L.beta", "Bi"="Bi.L.beta", "Po"="Po.L.beta", "At"="At.L.beta", "Rn"="Rn.L.beta", "Fr"="Fr.L.beta", "Ra"="Ra.L.beta", "Ac"="Ac.L.beta", "Th"="Th.L.beta", "Pa"="Pa.L.beta", "U"="U.L.beta")

mLines <- c("Au"="Au.M.line","Hg"="Hg.M.line", "Pb"="Pb.M.line", "U"="U.M.line")


######Load lines
lineLibrary <- readRDS("data/LineDefinitions.rdata")
fluorescence.lines <- lineLibrary$FluorescenceeLines
Wide <- lineLibrary$Wide
attach(lineLibrary$Tables)
attach(lineLibrary$Narrow)

line_strip <- function(elements){
    elements <- gsub(".K.alpha", "", elements)
    elements <- gsub(".K.beta", "", elements)
    elements <- gsub(".L.alpha", "", elements)
    elements <- gsub(".L.beta", "", elements)
    elements <- gsub(".M.line", "", elements)
    elements <- gsub(".K12", "", elements)
    elements <- gsub(".L1", "", elements)
    elements
}

atomic_order <- function(element){
    subset(fluorescence.lines, Symbol==line_strip(element))$AtomicNumber
}


atomic_order_vector <- function(elements){
    unlist(lapply(elements, atomic_order))
}

order_elements <- function(elements){
    not.elements <- elements[!elements %in% spectralLines]
    elements <- elements[elements %in% spectralLines]
    
    
    elements.simp <- mgsub::mgsub(pattern=c(".K.alpha", ".K.beta", ".L.alpha", ".L.beta", ".M.line"), replacement=c("", "", "", "", ""), string=elements)
    
    elements <- as.vector(as.character(elements[match(as.character(fluorescence.lines$Symbol), elements.simp)]))
    
    return(c(elements[complete.cases(elements)], not.elements))
}

element_line_pull <- function(element.line){
    element <- strsplit(x=element.line, split="\\.")[[1]][1]
    destination <- strsplit(x=element.line, split="\\.")[[1]][2]
    distance <- strsplit(x=element.line, split="\\.")[[1]][3]
    data.frame(ElementLine=element.line, Element=element, Orbital=destination, Line=distance, stringsAsFactors=FALSE)
}


elementGrabKalpha <- function(element, data) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    hold.frame <- data[!(data$Energy < elementLine[6][1,]-0.02 | data$Energy > elementLine[5][1,]+0.02), c("CPS", "Spectrum")]
    hold.ag <- aggregate(list(hold.frame$CPS), by=list(hold.frame$Spectrum), FUN="sum")
    colnames(hold.ag) <- c("Spectrum", paste(element, "K-alpha", sep=" "))
    
    hold.ag
    
}


elementGrabKbeta <- function(element, data) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    hold.cps <- if(elementLine[8][1,]!=0){
        subset(data$CPS, !(data$Energy < elementLine[7][1,]-0.02 | data$Energy > elementLine[8][1,]+0.02))
    } else if(elementLine[8][1,]==0){
        subset(data$CPS, !(data$Energy < elementLine[7][1,]-0.02 | data$Energy > elementLine[7][1,]+0.02))
    }
    
    
    hold.file <- if(elementLine[8][1,]!=0){
        subset(data$Spectrum, !(data$Energy < elementLine[7][1,]-0.02 | data$Energy > elementLine[8][1,]+0.02))
    } else if(elementLine[8][1,]==0){
            subset(data$Spectrum, !(data$Energy < elementLine[7][1,]-0.02 | data$Energy > elementLine[7][1,]+0.02))
    }
    hold.frame <- data.frame(is.0(hold.cps, hold.file), stringsAsFactors=FALSE)
    colnames(hold.frame) <- c("CPS", "Spectrum")
    hold.ag <- aggregate(list(hold.frame$CPS), by=list(hold.frame$Spectrum), FUN="sum")
    colnames(hold.ag) <- c("Spectrum", paste(element, "K-beta", sep=" "))
    
    hold.ag
    
}


elementGrabLalpha <- function(element, data) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    hold.frame <- data[!(data$Energy < elementLine[11][1,]-0.02 | data$Energy > elementLine[10][1,]+0.02), c("CPS", "Spectrum")]
    hold.ag <- aggregate(list(hold.frame$CPS), by=list(hold.frame$Spectrum), FUN="sum")
    colnames(hold.ag) <- c("Spectrum", paste(element, "L-alpha", sep=" "))
    
    hold.ag
    
}


elementGrabLbeta <- function(element, data) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    hold.frame <- data[!(data$Energy < elementLine[12][1,]-0.02 | data$Energy > elementLine[14][1,]+0.02), c("CPS", "Spectrum")]
    hold.ag <- aggregate(list(hold.frame$CPS), by=list(hold.frame$Spectrum), FUN="sum")
    colnames(hold.ag) <- c("Spectrum", paste(element, "L-beta", sep=" "))
    
    hold.ag
    
}

elementGrabMalpha <- function(element, data) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    hold.frame <- data[!(data$Energy < elementLine[20][1,]-0.02 | data$Energy > elementLine[22][1,]+0.02), c("CPS", "Spectrum")]
    hold.ag <- aggregate(list(hold.frame$CPS), by=list(hold.frame$Spectrum), FUN="sum")
    colnames(hold.ag) <- c("Spectrum", paste(element, "M-line", sep=" "))
    
    hold.ag
    
}


elementGrabpre <- function(element.line, data) {
    
    element.line <- make.names(element.line)
    
    element <- strsplit(x=element.line, split="\\.")[[1]][1]
    destination <- strsplit(x=element.line, split="\\.")[[1]][2]
    distance <- strsplit(x=element.line, split="\\.")[[1]][3]
    
    if(destination=="K" && distance=="alpha"){
        elementGrabKalpha(element=element, data=data)
    } else if(destination=="K" && distance=="beta"){
        elementGrabKbeta(element=element, data=data)
    } else if(destination=="L" && distance=="alpha"){
        elementGrabLalpha(element=element, data=data)
    } else if (destination=="L" && distance=="beta"){
        elementGrabLbeta(element=element, data=data)
    } else if (destination=="M" && distance=="line"){
        elementGrabMalpha(element=element, data=data)
    }
        
}




range_subset_xrf <- function(range.frame, data){
    
    new.data <- subset(data, Energy >= range.frame$EnergyMin & Energy <= range.frame$EnergyMax, drop=TRUE)
    newer.data <- aggregate(new.data, by=list(new.data$Spectrum), FUN=mean, na.rm=TRUE)[,c("Group.1", "CPS")]
    colnames(newer.data) <- c("Spectrum", as.character(range.frame$Name))
    newer.data
}


xrf_parse <- function(range.table, data){
    
    choice.lines <- range.table[complete.cases(range.table),]
    
    choice.list <- split(choice.lines, f=choice.lines$Name)
    names(choice.list) <- choice.lines[,"Name"]
    
    index <- choice.lines[,"Name"]
    
    selected.list <- lapply(index, function(x) range_subset_xrf(range.frame=choice.list[[x]], data=data))
    
    Reduce(function(...) merge(..., all=T), selected.list)
}

xrf_parse_single <- function(range.table, data, element){
    
    choice.lines <- range.table[range.table$Name %in% element,]
    
    choice.list <- split(choice.lines, f=choice.lines$Name)
    names(choice.list) <- choice.lines[,"Name"]
    
    index <- choice.lines[,"Name"]
    
    selected.list <- lapply(index, function(x) range_subset_xrf(range.frame=choice.list[[x]], data=data))
    
    Reduce(function(...) merge(..., all=T), selected.list)
}



elementGrab <- function(element.line, data, range.table=NULL){
    
    is.element <- element.line %in% spectralLines
    
    if(is.element==TRUE){
        elementGrabpre(element.line, data)
    } else if(is.element==FALSE){
        xrf_parse_single(range.table, data, element.line)
    }

    
}


elementFrame <- function(data, elements){
    
    spectra.line.list <- if(get_os()=="windows"){
        lapply(elements, function(x) elementGrab(element.line=x, data=data))
    } else if(get_os()!="windows"){
        core.mod <- if(length(elements)>=as.numeric(my.cores)){
            as.numeric(my.cores)
        } else if(length(elements)<as.numeric(my.cores)){
            length(elements)
        }
        pblapply(cl=core.mod, X=elements, function(x) elementGrab(element.line=x, data=data))
    }
    
    element.count.list <- lapply(spectra.line.list, '[', 2)
    
    spectra.line.vector <- as.numeric(unlist(element.count.list))
    
    dim(spectra.line.vector) <- c(length(spectra.line.list[[1]]$Spectrum), length(elements))
    
    spectra.line.frame <- data.frame(spectra.line.list[[1]]$Spectrum, spectra.line.vector, stringsAsFactors=FALSE)
    
    colnames(spectra.line.frame) <- c("Spectrum", elements)
    
    spectra.line.frame <- as.data.frame(spectra.line.frame, stringsAsFactors=FALSE)
    
    spectra.line.frame <- spectra.line.frame[order(as.character(spectra.line.frame$Spectrum)),]
    
    spectra.line.frame$Spectrum <- gsub(".pdz", "", spectra.line.frame$Spectrum)
    spectra.line.frame$Spectrum <- gsub(".csv", "", spectra.line.frame$Spectrum)
    spectra.line.frame$Spectrum <- gsub(".CSV", "", spectra.line.frame$Spectrum)
    spectra.line.frame$Spectrum <- gsub(".spt", "", spectra.line.frame$Spectrum)
    spectra.line.frame$Spectrum <- gsub(".mca", "", spectra.line.frame$Spectrum)
    spectra.line.frame$Spectrum <- gsub(".spx", "", spectra.line.frame$Spectrum)
    
    
    spectra.line.frame
    
}


wideElementGrabLine <- function(element.line, data) {
    
    element <- strsplit(x=element.line, split="\\.")[[1]][1]
    destination <- strsplit(x=element.line, split="\\.")[[1]][2]
    distance <- strsplit(x=element.line, split="\\.")[[1]][3]
    
    line <- paste0(destination, "-", distance)
    
    elementLine <- Wide[[element]]
    
    emission <- if(line=="K-alpha"){
        "Ka1"
    } else if(line=="K-beta"){
        "Kb1"
    } else if(line=="L-alpha"){
        "La1"
    } else if(line=="L-beta"){
        "Lb1"
    } else if(line=="M-line"){
        "Ma1"
    }
    
    #hold.frame <- data[data$Energy < elementLine[2, emission] && data$Energy > elementLine[1, emission], c("CPS", "Spectrum")]
    hold.frame <- data[!(data$Energy < elementLine[1, emission] | data$Energy > elementLine[2, emission]), c("CPS", "Spectrum")]
    
    hold.ag <- aggregate(list(hold.frame$CPS), by=list(hold.frame$Spectrum), FUN="sum")
    colnames(hold.ag) <- c("Spectrum", paste(element, line, sep=" "))
    
    hold.ag
    
}

wideElementGrab <- function(element.line, data, range.table=NULL){
    
    is.element <- element.line %in% spectralLines
    
    if(is.element==TRUE){
        wideElementGrabLine(element.line, data)
    } else if(is.element==FALSE){
        xrf_parse_single(range.table, data, element.line)
    }

    
}

wideElementFrame <- function(data, elements){
    
    spectra.line.list <- if(get_os()=="windows"){
        lapply(elements, function(x) wideElementGrab(element.line=x, data=data))
    }else if(get_os()!="windows"){
        core.mod <- if(length(elements)>=as.numeric(my.cores)){
            as.numeric(my.cores)
        } else if(length(elements)<as.numeric(my.cores)){
            length(elements)
        }
        #pblapply(cl=core.mod, X=elements, function(x) wideElementGrab(element.line=x, data=data))
        lapply(elements, function(x) wideElementGrab(element.line=x, data=data))
    }
    
    element.count.list <- lapply(spectra.line.list, '[', 2)
    
    spectra.line.vector <- as.numeric(unlist(element.count.list))
    
    dim(spectra.line.vector) <- c(length(spectra.line.list[[1]]$Spectrum), length(elements))
    
    spectra.line.frame <- data.frame(spectra.line.list[[1]]$Spectrum, spectra.line.vector, stringsAsFactors=FALSE)
    
    colnames(spectra.line.frame) <- c("Spectrum", elements)
    
    spectra.line.frame <- as.data.frame(spectra.line.frame, stringsAsFactors=FALSE)
    
    spectra.line.frame <- spectra.line.frame[order(as.character(spectra.line.frame$Spectrum)),]
    
    spectra.line.frame$Spectrum <- gsub(".pdz", "", spectra.line.frame$Spectrum)
    spectra.line.frame$Spectrum <- gsub(".csv", "", spectra.line.frame$Spectrum)
    spectra.line.frame$Spectrum <- gsub(".CSV", "", spectra.line.frame$Spectrum)
    spectra.line.frame$Spectrum <- gsub(".spt", "", spectra.line.frame$Spectrum)
    spectra.line.frame$Spectrum <- gsub(".mca", "", spectra.line.frame$Spectrum)
    spectra.line.frame$Spectrum <- gsub(".spx", "", spectra.line.frame$Spectrum)
    
    
    spectra.line.frame
    
}



###############
###Prep Data###
###############


###############
###Raw Spectra##
###############


general.prep <- function(spectra.line.table, element.line) {
    
    intensity <- spectra.line.table[,element.line]
    
    
    predict.frame <- data.frame(intensity)
    colnames(predict.frame) <- c("Intensity")
    
    
    
    predict.intensity <- data.frame(predict.frame$Intensity)
    colnames(predict.intensity) <- c("Intensity")
    
    predict.intensity
}

simple.tc.prep <- function(data,spectra.line.table, element.line) {
    
    intensity <- spectra.line.table[,element.line]
    
    
    total.counts <- aggregate(CPS~Spectrum, data=data, sum)
    colnames(total.counts) <- c("Spectrum", "CPS")
    
    
    
    predict.frame.tc <- data.frame(intensity/total.counts$CPS)
    colnames(predict.frame.tc) <- c("Intensity")
    
    
    
    predict.intensity.tc <- data.frame(predict.frame.tc$Intensity)
    colnames(predict.intensity.tc) <- c("Intensity")
    
    predict.intensity.tc
}


simple.comp.prep <- function(data, spectra.line.table, element.line, norm.min, norm.max) {
    
    
    
    intensity <- spectra.line.table[,element.line]
    
    
    compton.norm <- subset(data$CPS, !(data$Energy < norm.min | data$Energy > norm.max))
    compton.file <- subset(data$Spectrum, !(data$Energy < norm.min | data$Energy > norm.max))
    compton.frame <- data.frame(is.0(compton.norm, compton.file))
    colnames(compton.frame) <- c("Compton", "Spectrum")
    compton.frame.ag <- aggregate(list(compton.frame$Compton), by=list(compton.frame$Spectrum), FUN="sum")
    colnames(compton.frame.ag) <- c("Spectrum", "Compton")
    
    predict.frame.comp <- data.frame( intensity/compton.frame.ag$Compton)
    colnames(predict.frame.comp) <- c("Intensity")
    
    
    
    predict.intensity.comp <- data.frame(predict.frame.comp$Intensity)
    colnames(predict.intensity.comp) <- c("Intensity")
    
    predict.intensity.comp
    
}



###Prep Data



lukas.simp.prep <- function(spectra.line.table, element.line, slope.element.lines, intercept.element.lines) {
    
    
    intensity <- spectra.line.table[,element.line]
    
    
    intercept.none <- rep(0, length(spectra.line.table[,1]))
    lukas.intercept.table.x <- data.frame(spectra.line.table, intercept.none, intercept.none)
    colnames(lukas.intercept.table.x) <- c(names(spectra.line.table), "None", "NoneNull")
    
    
    
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lukas.slope.table <- data.frame(spectra.line.table, slope.none)
    colnames(lukas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    lukas.intercept.table <- data.frame(rowSums(lukas.intercept.table.x[,c(intercept.element.lines, "None", "NoneNull")], na.rm = TRUE))
    colnames(lukas.intercept.table) <- c("first")
    
    
    
    lukas.intercept <- lukas.intercept.table$first
    lukas.slope <- data.frame(lukas.slope.table[,slope.element.lines])
    colnames(lukas.slope) <- slope.element.lines
    
    
    
    predict.frame.luk <- data.frame(((1+intensity/(intensity+lukas.intercept))-lukas.intercept/(intensity+lukas.intercept)),lukas.slope)
    colnames(predict.frame.luk) <- c("Intensity", names(lukas.slope))
    
    predict.frame.luk <- data.frame(((1+intensity/(intensity+lukas.intercept)-lukas.intercept/(intensity+lukas.intercept))),lukas.slope)
    colnames(predict.frame.luk) <- c("Intensity", names(lukas.slope))
    
    
    
    predict.intensity.luk <- data.frame(predict.frame.luk$Intensity, lukas.slope)
    colnames(predict.intensity.luk) <- c("Intensity", names(lukas.slope))
    
    predict.intensity.luk
    
    
}



lukas.tc.prep <- function(data, spectra.line.table, element.line, slope.element.lines, intercept.element.lines) {
    
    
    intensity <- spectra.line.table[,element.line]
    
    
    total.counts <- aggregate(CPS~Spectrum, data=data, sum)
    colnames(total.counts) <- c("Spectrum", "CPS")
    
    
    intercept.none <- rep(0, length(spectra.line.table[,1]))
    lukas.intercept.table.x <- data.frame(spectra.line.table, intercept.none, intercept.none)
    colnames(lukas.intercept.table.x) <- c(names(spectra.line.table), "None", "NoneNull")
    
    
    
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lukas.slope.table <- data.frame(spectra.line.table, slope.none)
    colnames(lukas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    lukas.intercept.table.tc <- data.frame(rowSums(lukas.intercept.table.x[,c(intercept.element.lines, "None", "NoneNull")], na.rm = TRUE))/total.counts$CPS
    colnames(lukas.intercept.table.tc) <- c("first")
    
    
    
    lukas.intercept.tc <- lukas.intercept.table.tc$first
    lukas.slope.tc <- data.frame(lukas.slope.table[,slope.element.lines])/total.counts$CPS
    colnames(lukas.slope.tc) <- slope.element.lines
    
    
    
    predict.intensity.luk.tc <- data.frame(((1+intensity/(intensity+lukas.intercept.tc)-lukas.intercept.tc/(intensity+lukas.intercept.tc))),lukas.slope.tc)
    colnames(predict.intensity.luk.tc) <- c("Intensity", names(lukas.slope.tc))
    
    
    predict.intensity.luk.tc
}





lukas.comp.prep <- function(data, spectra.line.table, element.line, slope.element.lines, intercept.element.lines, norm.min, norm.max) {
    
    
    intensity <- spectra.line.table[,element.line]
    
    
    
    compton.norm <- subset(data$CPS, !(data$Energy < norm.min | data$Energy > norm.max))
    compton.file <- subset(data$Spectrum, !(data$Energy < norm.min | data$Energy > norm.max))
    compton.frame <- data.frame(is.0(compton.norm, compton.file))
    colnames(compton.frame) <- c("Compton", "Spectrum")
    compton.frame.ag <- aggregate(list(compton.frame$Compton), by=list(compton.frame$Spectrum), FUN="sum")
    colnames(compton.frame.ag) <- c("Spectrum", "Compton")
    
    
    intercept.none <- rep(0, length(spectra.line.table[,1]))
    lukas.intercept.table.x <- data.frame(spectra.line.table, intercept.none, intercept.none)
    colnames(lukas.intercept.table.x) <- c(names(spectra.line.table), "None", "NoneNull")
    
    
    
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lukas.slope.table <- data.frame(spectra.line.table, slope.none)
    colnames(lukas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    
    lukas.intercept.table.comp <- data.frame(rowSums(lukas.intercept.table.x[,c(intercept.element.lines, "None", "NoneNull")], na.rm = TRUE)/compton.frame.ag$Compton)
    colnames(lukas.intercept.table.comp) <- c("first")
    
    
    
    lukas.intercept.comp <- lukas.intercept.table.comp$first
    lukas.slope.comp <- data.frame(lukas.slope.table[,slope.element.lines]/compton.frame.ag$Compton)
    colnames(lukas.slope.comp) <- slope.element.lines
    
    
    predict.frame.luk.comp <- data.frame(((1+intensity/compton.frame.ag$Compton)/(intensity/compton.frame.ag$Compton+lukas.intercept.comp)-lukas.intercept.comp/(intensity/compton.frame.ag$Compton+lukas.intercept.comp)),lukas.slope.comp)
    colnames(predict.frame.luk.comp) <- c("Intensity", names(lukas.slope.comp))
    
    
    
    predict.intensity.luk.comp <- data.frame(predict.frame.luk.comp$Intensity, lukas.slope.comp)
    colnames(predict.intensity.luk.comp) <- c("Intensity", names(lukas.slope.comp))
    
    
    predict.intensity.luk.comp
}




###############
###Prep Data###
###############


###############
###Net Counts##
###############


general.prep.net <- function(spectra.line.table, element.line) {
    
    intensity <- spectra.line.table[,element.line]
    
    
    predict.frame <- data.frame(intensity)
    colnames(predict.frame) <- c("Intensity")
    
    
    
    predict.intensity <- data.frame(predict.frame$Intensity)
    colnames(predict.intensity) <- c("Intensity")
    
    predict.intensity
}

simple.tc.prep.net <- function(data,spectra.line.table, element.line) {
    
    intensity <- spectra.line.table[,element.line]
    
    total.counts.net <- rowSums(spectra.line.table[length(spectra.line.table)])
    total.counts <- data.frame(data$Spectrum, total.counts.net)
    colnames(total.counts) <- c("Spectrum", "CPS")
    
    
    
    predict.frame.tc <- data.frame(intensity/total.counts$CPS)
    colnames(predict.frame.tc) <- c("Intensity")
    
    
    
    predict.intensity.tc <- data.frame(predict.frame.tc$Intensity)
    colnames(predict.intensity.tc) <- c("Intensity")
    
    predict.intensity.tc
}


simple.comp.prep.net <- function(data, spectra.line.table, element.line, norm.min, norm.max) {
    
    
    
    intensity <- spectra.line.table[,element.line]
    
    
    compton.ag.fake.Spectrum <- data$Spectrum
    compton.ag.fake.Compton <- rep(1, length(data$Spectrum))
    compton.ag.fake <- data.frame(compton.ag.fake.Spectrum,compton.ag.fake.Compton)
    colnames(compton.ag.fake) <- c("Spectrum", "Compton")
    
    predict.frame.comp <- data.frame( intensity/compton.ag.fake$Compton)
    colnames(predict.frame.comp) <- c("Intensity")
    
    
    
    predict.intensity.comp <- data.frame(predict.frame.comp$Intensity)
    colnames(predict.intensity.comp) <- c("Intensity")
    
    predict.intensity.comp
    
}



###Prep Data



lukas.simp.prep.net <- function(spectra.line.table, element.line, slope.element.lines, intercept.element.lines) {
    
    
    intensity <- spectra.line.table[,element.line]
    
    intercept.none <- rep(0, length(spectra.line.table[,1]))
    lukas.intercept.table.x <- data.frame(spectra.line.table, intercept.none, intercept.none)
    colnames(lukas.intercept.table.x) <- c(names(spectra.line.table), "None", "NoneNull")
    
    
    
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lukas.slope.table <- data.frame(spectra.line.table, slope.none)
    colnames(lukas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    lukas.intercept.table <- data.frame(rowSums(lukas.intercept.table.x[,c(intercept.element.lines, "None", "NoneNull")], na.rm = TRUE))
    colnames(lukas.intercept.table) <- c("first")
    
    
    
    lukas.intercept <- lukas.intercept.table$first
    lukas.slope <- data.frame(lukas.slope.table[,slope.element.lines])
    colnames(lukas.slope) <- slope.element.lines
    
    
    
    predict.frame.luk <- data.frame(((1+intensity/(intensity+lukas.intercept))-lukas.intercept/(intensity+lukas.intercept)),lukas.slope)
    colnames(predict.frame.luk) <- c("Intensity", names(lukas.slope))
    
    
    
    predict.intensity.luk <- data.frame(predict.frame.luk$Intensity, lukas.slope)
    colnames(predict.intensity.luk) <- c("Intensity", names(lukas.slope))
    
    predict.intensity.luk
    
    
}



lukas.tc.prep.net <- function(data, spectra.line.table, element.line, slope.element.lines, intercept.element.lines) {
    
    
    intensity <- spectra.line.table[,element.line]
    
    
    total.counts.net <- rowSums(spectra.line.table[length(spectra.line.table)])
    total.counts <- data.frame(data$Spectrum, total.counts.net)
    colnames(total.counts) <- c("Spectrum", "CPS")
    
    
    
    
    intercept.none <- rep(0, length(spectra.line.table[,1]))
    lukas.intercept.table.x <- data.frame(spectra.line.table, intercept.none, intercept.none)
    colnames(lukas.intercept.table.x) <- c(names(spectra.line.table), "None", "NoneNull")
    
    
    
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lukas.slope.table <- data.frame(spectra.line.table, slope.none)
    colnames(lukas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    
    lukas.intercept.table.tc <- data.frame(rowSums(lukas.intercept.table.x[,c(intercept.element.lines, "None", "NoneNull")], na.rm = TRUE))/total.counts$CPS
    colnames(lukas.intercept.table.tc) <- c("first")
    
    
    
    
    lukas.intercept.tc <- lukas.intercept.table.tc$first
    lukas.slope.tc <- data.frame(lukas.slope.table[,slope.element.lines])/total.counts$CPS
    colnames(lukas.slope.tc) <- slope.element.lines
    
    
    predict.intensity.luk.tc <- data.frame(((1+intensity/(intensity+lukas.intercept.tc)-lukas.intercept.tc/(intensity+lukas.intercept.tc))),lukas.slope.tc)
    colnames(predict.intensity.luk.tc) <- c("Intensity", names(lukas.slope.tc))
    
    
    predict.intensity.luk.tc
}


lukas.comp.prep.net <- function(data, spectra.line.table, element.line, slope.element.lines, intercept.element.lines, norm.min, norm.max) {
    
    
    intensity <- spectra.line.table[,element.line]
    
    
    
    
    compton.ag.fake.Spectrum <- data$Spectrum
    compton.ag.fake.Compton <- rep(1, length(data$Spectrum))
    compton.ag.fake <- data.frame(compton.ag.fake.Spectrum,compton.ag.fake.Compton)
    colnames(compton.ag.fake) <- c("Spectrum", "Compton")
    
    
    intercept.none <- rep(0, length(spectra.line.table[,1]))
    lukas.intercept.table.x <- data.frame(spectra.line.table, intercept.none, intercept.none)
    colnames(lukas.intercept.table.x) <- c(names(spectra.line.table), "None", "NoneNull")
    
    
    
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lukas.slope.table <- data.frame(spectra.line.table, slope.none)
    colnames(lukas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    
    lukas.intercept.table.comp <- data.frame(rowSums(lukas.intercept.table.x[,c(intercept.element.lines, "None", "NoneNull")], na.rm = TRUE))/compton.ag.fake$Compton
    colnames(lukas.intercept.table.comp) <- c("first")
    
    
    
    
    lukas.intercept.comp <- lukas.intercept.table.comp$first
    lukas.slope.comp <- data.frame(lukas.slope.table[,slope.element.lines])/compton.ag.fake$Compton
    colnames(lukas.slope.comp) <- slope.element.lines
    
    
    
    predict.frame.luk.comp <- data.frame(((1+predict.frame.comp$Intensity/(predict.frame.comp$Intensity+lukas.intercept.comp)-lukas.intercept.comp/(predict.frame.comp$Intensity+lukas.intercept.comp))),lukas.slope.comp)
    colnames(predict.frame.luk.comp) <- c("Intensity", names(lukas.slope.comp))
    
    
    
    predict.intensity.luk.comp <- data.frame(predict.frame.luk.comp$Intensity, lukas.slope.comp)
    colnames(predict.intensity.luk.comp) <- c("Intensity", names(lukas.slope.comp))
    
    
    predict.intensity.luk.comp
}



scree_crunch <- function(dataframe, dependent, independent){
    
    simple.frame <- data.frame(
    newY = dataframe[,dependent]/max(dataframe[,dependent]),
    newX = dataframe[,independent]/max(dataframe[,independent]))
    
    sims <-data.frame(
    sims1 = seq(from=1, to=nrow(dataframe)-1, by=1),
    sims2 = seq(from=2, to=nrow(dataframe), by=1)
    )
    
    n <- seq(from=1, to=nrow(sims), by=1)
    
    lm.sims <- lapply(n, function(x) summary(lm(newY~newX, data=simple.frame[sims[,1][x]:sims[,2][x],])))
    
    
    slopes <- unlist(lapply(n, function(x) as.vector(lm.sims[[x]]["coefficients"][[1]][2])))
    
    #rsquared <- unlist(pbapply::pblapply(n, function(x) as.vector(lm.sims[[x]]["r.squared"])))
    
    greater.1 <- which(abs(slopes) > 1)
    
    greater.1[length(greater.1)]+1
    
    
}

combos <- function(a.vector){
    
    so <- seq(from=2, to=length(a.vector), by=1)
    
    long <- pblapply(so, function(x) combnPrim(x=a.vector, m=x))
    and <- pblapply(long, function(x) plyr::alply(x, 2))
    thanks.for.all.the.fish <- do.call(list, unlist(and, recursive=FALSE))
    
    thanks.for.all.the.fish
    
}

optimal_k_chain <- function(a.data.frame){
    
    #n <- if(nrow(a.data.frame)<30){
    #nrow(a.data.frame)-5
    #} else {
    #30
    #}
    
    n <- 20
    
    xrf.pca.frame <- a.data.frame[complete.cases(a.data.frame),]
    
    wss <- (nrow(xrf.pca.frame)-1)*sum(apply(xrf.pca.frame,2,var))
    for (i in 2:n) wss[i] <- sum(kmeans(xrf.pca.frame,
    centers=i)$withinss)
    
    result <- data.frame(
    clustercount=seq(1, n, 1),
    wss=wss,
    percent=1-wss/max(wss))
    
    best.choice <- scree_crunch(dataframe=result, dependent="percent", independent="clustercount")
    
    result[best.choice,]
    
}


accepted.spec.light <- c("Na.K.alpha", "Mg.K.alpha", "Al.K.alpha", "Si.K.alpha", "P.K.alpha", "S.K.alpha", "Cl.K.alpha", "K.K.alpha", "Ca.K.alpha", "Ti.K.alpha", "Ba.L.alpha", "Cr.K.alpha", "Mn.K.alpha", "Fe.K.alpha")
accepted.spec.trace <- c("K.K.alpha", "Ca.K.alpha", "Ti.K.alpha", "V.K.alpha", "Ba.L.alpha", "Cr.K.alpha", "Mn.K.alpha", "Fe.K.alpha", "Co.K.alpha", "Ni.K.alpha", "Cu.K.alpha", "Zn.K.alpha", "Ga.K.alpha", "Br.K.alpha", "Se.K.alpha", "Rb.K.alpha", "Sr.K.alpha", "Y.K.alpha", "Zr.K.alpha", "Nb.K.alpha", "Mo.K.alpha", "Rh.K.alpha", "Ag.K.alpha", "Cd.K.alpha", "Sn.K.alpha", "Sb.K.alpha", "Ba.K.alpha", "Au.L.alpha", "Hg.L.alpha", "Pb.L.beta", "Th.L.alpha", "U.L.alpha", "Rayleigh", "Compton")
accepted.spec.combined <- c("Na.K.alpha", "Mg.K.alpha", "Al.K.alpha", "Si.K.alpha", "P.K.alpha", "S.K.alpha", "Cl.K.alpha", "K.K.alpha", "Ca.K.alpha", "Ti.K.alpha", "V.K.alpha", "Ba.L.alpha", "Cr.K.alpha", "Mn.K.alpha", "Fe.K.alpha", "Co.K.alpha", "Ni.K.alpha", "Cu.K.alpha", "Zn.K.alpha", "Ga.K.alpha", "Br.K.alpha", "Se.K.alpha", "Rb.K.alpha", "Sr.K.alpha", "Y.K.alpha", "Zr.K.alpha", "Nb.K.alpha", "Mo.K.alpha", "Rh.K.alpha", "Ag.K.alpha", "Cd.K.alpha", "Sn.K.alpha", "Sb.K.alpha", "Ba.K.alpha", "Au.L.alpha", "Hg.L.alpha", "Pb.L.beta", "Th.L.alpha", "U.L.alpha", "Rayleigh", "Compton")

accepted.net.light <- c("Na.K12", "Mg.K12", "Al.K12", "Si.K12", "P.K12", "S.K12", "Cl.K12", "K.K12", "Ca.K12", "Ti.K12", "Ba.L1", "Cr.K12", "Mn.K12", "Fe.K12", "Rh.L1")
accepted.net.trace <- c("K.K12", "Ca.K12", "Ti.K12", "V.K12", "Ba.L1", "Cr.K12", "Mn.K12", "Fe.K12", "Co.K12", "Ni.K12", "Cu.K12", "Zn.K12", "Ga.K12", "Br.K12", "Se.K12", "Rb.K12", "Sr.K12", "Y.K12", "Zr.K12", "Nb.K12", "Mo.K12", "Ag.K12", "Cd.K12", "Sn.K12", "Sb.K12", "Ba.K12", "Au.L1", "Hg.L1", "Pb.L1", "Th.L1", "U.L1", "Rh.K12")
accepted.net.combined <- c("Na.K12", "Mg.K12", "Al.K12", "Si.K12", "P.K12", "S.K12", "Cl.K12", "K.K12", "Ca.K12", "Ti.K12", "V.K12", "Ba.L1", "Cr.K12", "Mn.K12", "Fe.K12", "Co.K12", "Ni.K12", "Cu.K12", "Zn.K12", "Ga.K12", "Br.K12", "Se.K12", "Rb.K12", "Sr.K12", "Y.K12", "Zr.K12", "Nb.K12", "Mo.K12", "Ag.K12", "Cd.K12", "Sn.K12", "Sb.K12", "Ba.K12", "Au.L1", "Hg.L1", "Pb.L1", "Th.L1", "U.L1", "Rh.K12")


preference.light <- c("Na.K12", "Mg.K12", "Al.K12", "Si.K12", "P.K12", "S.K12", "Cl.K12", "K.K12", "Ca.K12", "Rh.L1", "Na.K.alpha", "Mg.K.alpha", "Al.K.alpha", "Si.K.alpha", "P.K.alpha", "S.K.alpha", "Cl.K.alpha", "K.K.alpha", "Ca.K.alpha", "Rh.L.alpha")
preference.trace <- c("Ti.K12", "V.K12", "Ba.L1", "Cr.K12", "Mn.K12", "Fe.K12", "Co.K12", "Ni.K12", "Cu.K12", "Zn.K12", "Ga.K12", "Br.K12", "Se.K12", "Rb.K12", "Sr.K12", "Y.K12", "Zr.K12", "Nb.K12", "Mo.K12", "Ag.K12", "Cd.K12", "Sn.K12", "Sb.K12", "Ba.K12", "Au.L1", "Hg.L1", "Pb.L1", "Th.L1", "U.L1", "Rh.K12", "Ti.K.alpha", "V.K.alpha", "Ba.L.alpha", "Cr.K.alpha", "Mn.K.alpha", "Fe.K.alpha", "Co.K.alpha", "Ni.K.alpha", "Cu.K.alpha", "Zn.K.alpha", "Ga.K.alpha", "Br.K.alpha", "Se.K.alpha", "Rb.K.alpha", "Sr.K.alpha", "Y.K.alpha", "Zr.K.alpha", "Nb.K.alpha", "Mo.K.alpha", "Ag.K.alpha", "Cd.K.alpha", "Sn.K.alpha", "Sb.K.alpha", "Ba.K.alpha", "Au.L.alpha", "Hg.L.alpha", "Pb.L.beta", "Th.L.alpha", "U.L.alpha", "Rh.K.alpha", "Compton", "Rayleigh")

spectra.line.fn <- function(data) {
    
    
    Na.Ka.cps <- subset(data$CPS, !(data$Energy < Na$Line[2]-0.02 | data$Energy > Na$Line[1]+0.02))
    Na.file <- subset(data$Spectrum, !(data$Energy < Na$Line[2]-0.02 | data$Energy > Na$Line[1]+0.02))
    Na.Ka.frame <- data.frame(is.0(Na.Ka.cps, Na.file))
    colnames(Na.Ka.frame) <- c("Counts", "Spectrum")
    Na.Ka.ag <- aggregate(list(Na.Ka.frame$Counts), by=list(Na.Ka.frame$Spectrum), FUN="sum")
    colnames(Na.Ka.ag) <- c("Spectrum", "Na K-alpha")
    
    Na.Kb.cps <- subset(data$CPS, !(data$Energy < Na$Line[3]-0.02 | data$Energy > Na$Line[3]+0.02))
    Na.file <- subset(data$Spectrum, !(data$Energy < Na$Line[3]-0.02 | data$Energy > Na$Line[3]+0.02))
    Na.Kb.frame <- data.frame(is.0(Na.Kb.cps, Na.file))
    colnames(Na.Kb.frame) <- c("Counts", "Spectrum")
    Na.Kb.ag <- aggregate(list(Na.Kb.frame$Counts), by=list(Na.Kb.frame$Spectrum), FUN="sum")
    colnames(Na.Kb.ag) <- c("Spectrum", "Na K-beta")
    
    
    Mg.Ka.cps <- subset(data$CPS, !(data$Energy < Mg$Line[2]-0.02 | data$Energy > Mg$Line[1]+0.02))
    Mg.file <- subset(data$Spectrum, !(data$Energy < Mg$Line[2]-0.02 | data$Energy > Mg$Line[1]+0.02))
    Mg.Ka.frame <- data.frame(is.0(Mg.Ka.cps, Mg.file))
    colnames(Mg.Ka.frame) <- c("Counts", "Spectrum")
    Mg.Ka.ag <- aggregate(list(Mg.Ka.frame$Counts), by=list(Mg.Ka.frame$Spectrum), FUN="sum")
    colnames(Mg.Ka.ag) <- c("Spectrum", "Mg K-alpha")
    
    
    Mg.Kb.cps <- subset(data$CPS, !(data$Energy < Mg$Line[3]-0.02 | data$Energy > Mg$Line[3]+0.02))
    Mg.file <- subset(data$Spectrum, !(data$Energy < Mg$Line[3]-0.02 | data$Energy > Mg$Line[3]+0.02))
    Mg.Kb.frame <- data.frame(is.0(Mg.Kb.cps, Mg.file))
    colnames(Mg.Kb.frame) <- c("Counts", "Spectrum")
    Mg.Kb.ag <- aggregate(list(Mg.Kb.frame$Counts), by=list(Mg.Kb.frame$Spectrum), FUN="sum")
    colnames(Mg.Kb.ag) <- c("Spectrum", "Mg K-beta")
    
    
    Al.Ka.cps <- subset(data$CPS, !(data$Energy < Al$Line[2]-0.02 | data$Energy > Al$Line[1]+0.02))
    Al.file <- subset(data$Spectrum, !(data$Energy < Al$Line[2]-0.02 | data$Energy > Al$Line[1]+0.02))
    Al.Ka.frame <- data.frame(is.0(Al.Ka.cps, Al.file))
    colnames(Al.Ka.frame) <- c("Counts", "Spectrum")
    Al.Ka.ag <- aggregate(list(Al.Ka.frame$Counts), by=list(Al.Ka.frame$Spectrum), FUN="sum")
    colnames(Al.Ka.ag) <- c("Spectrum", "Al K-alpha")
    
    
    Al.Kb.cps <- subset(data$CPS, !(data$Energy < Al$Line[3]-0.02 | data$Energy > Al$Line[3]+0.02))
    Al.file <- subset(data$Spectrum, !(data$Energy < Al$Line[3]-0.02 | data$Energy > Al$Line[3]+0.02))
    Al.Kb.frame <- data.frame(is.0(Al.Kb.cps, Al.file))
    colnames(Al.Kb.frame) <- c("Counts", "Spectrum")
    Al.Kb.ag <- aggregate(list(Al.Kb.frame$Counts), by=list(Al.Kb.frame$Spectrum), FUN="sum")
    colnames(Al.Kb.ag) <- c("Spectrum", "Al K-beta")
    
    
    Si.Ka.cps <- subset(data$CPS, !(data$Energy < Si$Line[2]-0.02 | data$Energy > Si$Line[1]+0.02))
    Si.file <- subset(data$Spectrum, !(data$Energy < Si$Line[2]-0.02 | data$Energy > Si$Line[1]+0.02))
    Si.Ka.frame <- data.frame(is.0(Si.Ka.cps, Si.file))
    colnames(Si.Ka.frame) <- c("Counts", "Spectrum")
    Si.Ka.ag <- aggregate(list(Si.Ka.frame$Counts), by=list(Si.Ka.frame$Spectrum), FUN="sum")
    colnames(Si.Ka.ag) <- c("Spectrum", "Si K-alpha")
    
    
    Si.Kb.cps <- subset(data$CPS, !(data$Energy < Si$Line[3]-0.02 | data$Energy > Si$Line[3]+0.02))
    Si.file <- subset(data$Spectrum, !(data$Energy < Si$Line[3]-0.02 | data$Energy > Si$Line[3]+0.02))
    Si.Kb.frame <- data.frame(is.0(Si.Kb.cps, Si.file))
    colnames(Si.Kb.frame) <- c("Counts", "Spectrum")
    Si.Kb.ag <- aggregate(list(Si.Kb.frame$Counts), by=list(Si.Kb.frame$Spectrum), FUN="sum")
    colnames(Si.Kb.ag) <- c("Spectrum", "Si K-beta")
    
    
    P.Ka.cps <- subset(data$CPS, !(data$Energy < P$Line[2]-0.02 | data$Energy > P$Line[1]+0.02))
    P.file <- subset(data$Spectrum, !(data$Energy < P$Line[2]-0.02 | data$Energy > P$Line[1]+0.02))
    P.Ka.frame <- data.frame(is.0(P.Ka.cps, P.file))
    colnames(P.Ka.frame) <- c("Counts", "Spectrum")
    P.Ka.ag <- aggregate(list(P.Ka.frame$Counts), by=list(P.Ka.frame$Spectrum), FUN="sum")
    colnames(P.Ka.ag) <- c("Spectrum", "P K-alpha")
    
    P.Kb.cps <- subset(data$CPS, !(data$Energy < P$Line[3]-0.02 | data$Energy > P$Line[3]+0.02))
    P.file <- subset(data$Spectrum, !(data$Energy < P$Line[3]-0.02 | data$Energy > P$Line[3]+0.02))
    P.Kb.frame <- data.frame(is.0(P.Kb.cps, P.file))
    colnames(P.Kb.frame) <- c("Counts", "Spectrum")
    P.Kb.ag <- aggregate(list(P.Kb.frame$Counts), by=list(P.Kb.frame$Spectrum), FUN="sum")
    colnames(P.Kb.ag) <- c("Spectrum", "P K-beta")
    
    S.Ka.cps <- subset(data$CPS, !(data$Energy < S$Line[2]-0.02 | data$Energy > S$Line[1]+0.02))
    S.file <- subset(data$Spectrum, !(data$Energy < S$Line[2]-0.02 | data$Energy > S$Line[1]+0.02))
    S.Ka.frame <- data.frame(is.0(S.Ka.cps, S.file))
    colnames(S.Ka.frame) <- c("Counts", "Spectrum")
    S.Ka.ag <- aggregate(list(S.Ka.frame$Counts), by=list(S.Ka.frame$Spectrum), FUN="sum")
    colnames(S.Ka.ag) <- c("Spectrum", "S K-alpha")
    
    
    S.Kb.cps <- subset(data$CPS, !(data$Energy < S$Line[3]-0.02 | data$Energy > S$Line[3]+0.02))
    S.file <- subset(data$Spectrum, !(data$Energy < S$Line[3]-0.02 | data$Energy > S$Line[3]+0.02))
    S.Kb.frame <- data.frame(is.0(S.Kb.cps, S.file))
    colnames(S.Kb.frame) <- c("Counts", "Spectrum")
    S.Kb.ag <- aggregate(list(S.Kb.frame$Counts), by=list(S.Kb.frame$Spectrum), FUN="sum")
    colnames(S.Kb.ag) <- c("Spectrum", "S K-beta")
    
    
    Cl.Ka.cps <- subset(data$CPS, !(data$Energy < Cl$Line[2]-0.02 | data$Energy > Cl$Line[1]+0.02))
    Cl.file <- subset(data$Spectrum, !(data$Energy < Cl$Line[2]-0.02 | data$Energy > Cl$Line[1]+0.02))
    Cl.Ka.frame <- data.frame(is.0(Cl.Ka.cps, Cl.file))
    colnames(Cl.Ka.frame) <- c("Counts", "Spectrum")
    Cl.Ka.ag <- aggregate(list(Cl.Ka.frame$Counts), by=list(Cl.Ka.frame$Spectrum), FUN="sum")
    colnames(Cl.Ka.ag) <- c("Spectrum", "Cl K-alpha")
    
    Cl.Kb.cps <- subset(data$CPS, !(data$Energy < Cl$Line[3]-0.02 | data$Energy > Cl$Line[3]+0.02))
    Cl.file <- subset(data$Spectrum, !(data$Energy < Cl$Line[3]-0.02 | data$Energy > Cl$Line[3]+0.02))
    Cl.Kb.frame <- data.frame(is.0(Cl.Kb.cps, Cl.file))
    colnames(Cl.Kb.frame) <- c("Counts", "Spectrum")
    Cl.Kb.ag <- aggregate(list(Cl.Kb.frame$Counts), by=list(Cl.Kb.frame$Spectrum), FUN="sum")
    colnames(Cl.Kb.ag) <- c("Spectrum", "Cl K-beta")
    
    Ar.Ka.cps <- subset(data$CPS, !(data$Energy < Ar$Line[2]-0.02 | data$Energy > Ar$Line[1]+0.02))
    Ar.file <- subset(data$Spectrum, !(data$Energy < Ar$Line[2]-0.02 | data$Energy > Ar$Line[1]+0.02))
    Ar.Ka.frame <- data.frame(is.0(Ar.Ka.cps, Ar.file))
    colnames(Ar.Ka.frame) <- c("Counts", "Spectrum")
    Ar.Ka.ag <- aggregate(list(Ar.Ka.frame$Counts), by=list(Ar.Ka.frame$Spectrum), FUN="sum")
    colnames(Ar.Ka.ag) <- c("Spectrum", "Ar K-alpha")
    
    
    Ar.Kb.cps <- subset(data$CPS, !(data$Energy < Ar$Line[3]-0.02 | data$Energy > Ar$Line[3]+0.02))
    Ar.file <- subset(data$Spectrum, !(data$Energy < Ar$Line[3]-0.02 | data$Energy > Ar$Line[3]+0.02))
    Ar.Kb.frame <- data.frame(is.0(Ar.Kb.cps, Ar.file))
    colnames(Ar.Kb.frame) <- c("Counts", "Spectrum")
    Ar.Kb.ag <- aggregate(list(Ar.Kb.frame$Counts), by=list(Ar.Kb.frame$Spectrum), FUN="sum")
    colnames(Ar.Kb.ag) <- c("Spectrum", "Ar K-beta")
    
    
    K.Ka.cps <- subset(data$CPS, !(data$Energy < K$Line[2]-0.02 | data$Energy > K$Line[1]+0.02))
    K.file <- subset(data$Spectrum, !(data$Energy < K$Line[2]-0.02 | data$Energy > K$Line[1]+0.02))
    K.Ka.frame <- data.frame(is.0(K.Ka.cps, K.file))
    colnames(K.Ka.frame) <- c("Counts", "Spectrum")
    K.Ka.ag <- aggregate(list(K.Ka.frame$Counts), by=list(K.Ka.frame$Spectrum), FUN="sum")
    colnames(K.Ka.ag) <- c("Spectrum", "K K-alpha")
    
    K.Kb.cps <- subset(data$CPS, !(data$Energy < K$Line[3]-0.02 | data$Energy > K$Line[3]+0.02))
    K.file <- subset(data$Spectrum, !(data$Energy < K$Line[3]-0.02 | data$Energy > K$Line[3]+0.02))
    K.Kb.frame <- data.frame(is.0(K.Kb.cps, K.file))
    colnames(K.Kb.frame) <- c("Counts", "Spectrum")
    K.Kb.ag <- aggregate(list(K.Kb.frame$Counts), by=list(K.Kb.frame$Spectrum), FUN="sum")
    colnames(K.Kb.ag) <- c("Spectrum", "K K-beta")
    
    Ca.Ka.cps <- subset(data$CPS, !(data$Energy < Ca$Line[2]-0.02 | data$Energy > Ca$Line[1]+0.02))
    Ca.file <- subset(data$Spectrum, !(data$Energy < Ca$Line[2]-0.02 | data$Energy > Ca$Line[1]+0.02))
    Ca.Ka.frame <- data.frame(is.0(Ca.Ka.cps, Ca.file))
    colnames(Ca.Ka.frame) <- c("Counts", "Spectrum")
    Ca.Ka.ag <- aggregate(list(Ca.Ka.frame$Counts), by=list(Ca.Ka.frame$Spectrum), FUN="sum")
    colnames(Ca.Ka.ag) <- c("Spectrum", "Ca K-alpha")
    
    
    Ca.Kb.cps <- subset(data$CPS, !(data$Energy < Ca$Line[3]-0.02 | data$Energy > Ca$Line[3]+0.02))
    Ca.file <- subset(data$Spectrum, !(data$Energy < Ca$Line[3]-0.02 | data$Energy > Ca$Line[3]+0.02))
    Ca.Kb.frame <- data.frame(is.0(Ca.Kb.cps, Ca.file))
    colnames(Ca.Kb.frame) <- c("Counts", "Spectrum")
    Ca.Kb.ag <- aggregate(list(Ca.Kb.frame$Counts), by=list(Ca.Kb.frame$Spectrum), FUN="sum")
    colnames(Ca.Kb.ag) <- c("Spectrum", "Ca K-beta")
    
    Sc.Ka.cps <- subset(data$CPS, !(data$Energy < Sc$Line[2]-0.02 | data$Energy > Sc$Line[1]+0.02))
    Sc.file <- subset(data$Spectrum, !(data$Energy < Sc$Line[2]-0.02 | data$Energy > Sc$Line[1]+0.02))
    Sc.Ka.frame <- data.frame(is.0(Sc.Ka.cps, Sc.file))
    colnames(Sc.Ka.frame) <- c("Counts", "Spectrum")
    Sc.Ka.ag <- aggregate(list(Sc.Ka.frame$Counts), by=list(Sc.Ka.frame$Spectrum), FUN="sum")
    colnames(Sc.Ka.ag) <- c("Spectrum", "Sc K-alpha")
    
    Sc.Kb.cps <- subset(data$CPS, !(data$Energy < Sc$Line[3]-0.02 | data$Energy > Sc$Line[3]+0.02))
    Sc.file <- subset(data$Spectrum, !(data$Energy < Sc$Line[3]-0.02 | data$Energy > Sc$Line[3]+0.02))
    Sc.Kb.frame <- data.frame(is.0(Sc.Kb.cps, Sc.file))
    colnames(Sc.Kb.frame) <- c("Counts", "Spectrum")
    Sc.Kb.ag <- aggregate(list(Sc.Kb.frame$Counts), by=list(Sc.Kb.frame$Spectrum), FUN="sum")
    colnames(Sc.Kb.ag) <- c("Spectrum", "Sc K-beta")
    
    Ti.Ka.cps <- subset(data$CPS, !(data$Energy < Ti$Line[2]-0.02 | data$Energy > Ti$Line[1]+0.02))
    Ti.file <- subset(data$Spectrum, !(data$Energy < Ti$Line[2]-0.02 | data$Energy > Ti$Line[1]+0.02))
    Ti.Ka.frame <- data.frame(is.0(Ti.Ka.cps, Ti.file))
    colnames(Ti.Ka.frame) <- c("Counts", "Spectrum")
    Ti.Ka.ag <- aggregate(list(Ti.Ka.frame$Counts), by=list(Ti.Ka.frame$Spectrum), FUN="sum")
    colnames(Ti.Ka.ag) <- c("Spectrum", "Ti K-alpha")
    
    Ti.Kb.cps <- subset(data$CPS, !(data$Energy < Ti$Line[3]-0.02 | data$Energy > Ti$Line[3]+0.02))
    Ti.file <- subset(data$Spectrum, !(data$Energy < Ti$Line[3]-0.02 | data$Energy > Ti$Line[3]+0.02))
    Ti.Kb.frame <- data.frame(is.0(Ti.Kb.cps, Ti.file))
    colnames(Ti.Kb.frame) <- c("Counts", "Spectrum")
    Ti.Kb.ag <- aggregate(list(Ti.Kb.frame$Counts), by=list(Ti.Kb.frame$Spectrum), FUN="sum")
    colnames(Ti.Kb.ag) <- c("Spectrum", "Ti K-beta")
    
    V.Ka.cps <- subset(data$CPS, !(data$Energy < V$Line[2]-0.02 | data$Energy > V$Line[1]+0.02))
    V.file <- subset(data$Spectrum, !(data$Energy < V$Line[2]-0.02 | data$Energy > V$Line[1]+0.02))
    V.Ka.frame <- data.frame(is.0(V.Ka.cps, V.file))
    colnames(V.Ka.frame) <- c("Counts", "Spectrum")
    V.Ka.ag <- aggregate(list(V.Ka.frame$Counts), by=list(V.Ka.frame$Spectrum), FUN="sum")
    colnames(V.Ka.ag) <- c("Spectrum", "V K-alpha")
    
    
    V.Kb.cps <- subset(data$CPS, !(data$Energy < V$Line[3]-0.02 | data$Energy > V$Line[3]+0.02))
    V.file <- subset(data$Spectrum, !(data$Energy < V$Line[3]-0.02 | data$Energy > V$Line[3]+0.02))
    V.Kb.frame <- data.frame(is.0(V.Kb.cps, V.file))
    colnames(V.Kb.frame) <- c("Counts", "Spectrum")
    V.Kb.ag <- aggregate(list(V.Kb.frame$Counts), by=list(V.Kb.frame$Spectrum), FUN="sum")
    colnames(V.Kb.ag) <- c("Spectrum", "V K-beta")
    
    Cr.Ka.cps <- subset(data$CPS, !(data$Energy < Cr$Line[2]-0.02 | data$Energy > Cr$Line[1]+0.02))
    Cr.file <- subset(data$Spectrum, !(data$Energy < Cr$Line[2]-0.02 | data$Energy > Cr$Line[1]+0.02))
    Cr.Ka.frame <- data.frame(is.0(Cr.Ka.cps, Cr.file))
    colnames(Cr.Ka.frame) <- c("Counts", "Spectrum")
    Cr.Ka.ag <- aggregate(list(Cr.Ka.frame$Counts), by=list(Cr.Ka.frame$Spectrum), FUN="sum")
    colnames(Cr.Ka.ag) <- c("Spectrum", "Cr K-alpha")
    
    Cr.Kb.cps <- subset(data$CPS, !(data$Energy < Cr$Line[3]-0.02 | data$Energy > Cr$Line[3]+0.02))
    Cr.file <- subset(data$Spectrum, !(data$Energy < Cr$Line[3]-0.02 | data$Energy > Cr$Line[3]+0.02))
    Cr.Kb.frame <- data.frame(is.0(Cr.Kb.cps, Cr.file))
    colnames(Cr.Kb.frame) <- c("Counts", "Spectrum")
    Cr.Kb.ag <- aggregate(list(Cr.Kb.frame$Counts), by=list(Cr.Kb.frame$Spectrum), FUN="sum")
    colnames(Cr.Kb.ag) <- c("Spectrum", "Cr K-beta")
    
    Mn.Ka.cps <- subset(data$CPS, !(data$Energy < Mn$Line[2]-0.02 | data$Energy > Mn$Line[1]+0.02))
    Mn.file <- subset(data$Spectrum, !(data$Energy < Mn$Line[2]-0.02 | data$Energy > Mn$Line[1]+0.02))
    Mn.Ka.frame <- data.frame(is.0(Mn.Ka.cps, Mn.file))
    colnames(Mn.Ka.frame) <- c("Counts", "Spectrum")
    Mn.Ka.ag <- aggregate(list(Mn.Ka.frame$Counts), by=list(Mn.Ka.frame$Spectrum), FUN="sum")
    colnames(Mn.Ka.ag) <- c("Spectrum", "Mn K-alpha")
    
    Mn.Kb.cps <- subset(data$CPS, !(data$Energy < Mn$Line[3]-0.02 | data$Energy > Mn$Line[3]+0.02))
    Mn.file <- subset(data$Spectrum, !(data$Energy < Mn$Line[3]-0.02 | data$Energy > Mn$Line[3]+0.02))
    Mn.Kb.frame <- data.frame(is.0(Mn.Kb.cps, Mn.file))
    colnames(Mn.Kb.frame) <- c("Counts", "Spectrum")
    Mn.Kb.ag <- aggregate(list(Mn.Kb.frame$Counts), by=list(Mn.Kb.frame$Spectrum), FUN="sum")
    colnames(Mn.Kb.ag) <- c("Spectrum", "Mn K-beta")
    
    Fe.Ka.cps <- subset(data$CPS, !(data$Energy < Fe$Line[2]-0.02 | data$Energy > Fe$Line[1]+0.02))
    Fe.file <- subset(data$Spectrum, !(data$Energy < Fe$Line[2]-0.02 | data$Energy > Fe$Line[1]+0.02))
    Fe.Ka.frame <- data.frame(is.0(Fe.Ka.cps, Fe.file))
    colnames(Fe.Ka.frame) <- c("Counts", "Spectrum")
    Fe.Ka.ag <- aggregate(list(Fe.Ka.frame$Counts), by=list(Fe.Ka.frame$Spectrum), FUN="sum")
    colnames(Fe.Ka.ag) <- c("Spectrum", "Fe K-alpha")
    
    Fe.Kb.cps <- subset(data$CPS, !(data$Energy < Fe$Line[3]-0.02 | data$Energy > Fe$Line[3]+0.02))
    Fe.file <- subset(data$Spectrum, !(data$Energy < Fe$Line[3]-0.02 | data$Energy > Fe$Line[3]+0.02))
    Fe.Kb.frame <- data.frame(is.0(Fe.Kb.cps, Fe.file))
    colnames(Fe.Kb.frame) <- c("Counts", "Spectrum")
    Fe.Kb.ag <- aggregate(list(Fe.Kb.frame$Counts), by=list(Fe.Kb.frame$Spectrum), FUN="sum")
    colnames(Fe.Kb.ag) <- c("Spectrum", "Fe K-beta")
    
    Co.Ka.cps <- subset(data$CPS, !(data$Energy < Co$Line[2]-0.02 | data$Energy > Co$Line[1]+0.02))
    Co.file <- subset(data$Spectrum, !(data$Energy < Co$Line[2]-0.02 | data$Energy > Co$Line[1]+0.02))
    Co.Ka.frame <- data.frame(is.0(Co.Ka.cps, Co.file))
    colnames(Co.Ka.frame) <- c("Counts", "Spectrum")
    Co.Ka.ag <- aggregate(list(Co.Ka.frame$Counts), by=list(Co.Ka.frame$Spectrum), FUN="sum")
    colnames(Co.Ka.ag) <- c("Spectrum", "Co K-alpha")
    
    Co.Kb.cps <- subset(data$CPS, !(data$Energy < Co$Line[3]-0.02 | data$Energy > Co$Line[3]+0.02))
    Co.file <- subset(data$Spectrum, !(data$Energy < Co$Line[3]-0.02 | data$Energy > Co$Line[3]+0.02))
    Co.Kb.frame <- data.frame(is.0(Co.Kb.cps, Co.file))
    colnames(Co.Kb.frame) <- c("Counts", "Spectrum")
    Co.Kb.ag <- aggregate(list(Co.Kb.frame$Counts), by=list(Co.Kb.frame$Spectrum), FUN="sum")
    colnames(Co.Kb.ag) <- c("Spectrum", "Co K-beta")
    
    Ni.Ka.cps <- subset(data$CPS, !(data$Energy < Ni$Line[2]-0.02 | data$Energy > Ni$Line[1]+0.02))
    Ni.file <- subset(data$Spectrum, !(data$Energy < Ni$Line[2]-0.02 | data$Energy > Ni$Line[1]+0.02))
    Ni.Ka.frame <- data.frame(is.0(Ni.Ka.cps, Ni.file))
    colnames(Ni.Ka.frame) <- c("Counts", "Spectrum")
    Ni.Ka.ag <- aggregate(list(Ni.Ka.frame$Counts), by=list(Ni.Ka.frame$Spectrum), FUN="sum")
    colnames(Ni.Ka.ag) <- c("Spectrum", "Ni K-alpha")
    
    Ni.Kb.cps <- subset(data$CPS, !(data$Energy < Ni$Line[3]-0.02 | data$Energy > Ni$Line[3]+0.02))
    Ni.file <- subset(data$Spectrum, !(data$Energy < Ni$Line[3]-0.02 | data$Energy > Ni$Line[3]+0.02))
    Ni.Kb.frame <- data.frame(is.0(Ni.Kb.cps, Ni.file))
    colnames(Ni.Kb.frame) <- c("Counts", "Spectrum")
    Ni.Kb.ag <- aggregate(list(Ni.Kb.frame$Counts), by=list(Ni.Kb.frame$Spectrum), FUN="sum")
    colnames(Ni.Kb.ag) <- c("Spectrum", "Ni K-beta")
    
    Cu.Ka.cps <- subset(data$CPS, !(data$Energy < Cu$Line[2]-0.02 | data$Energy > Cu$Line[1]+0.02))
    Cu.file <- subset(data$Spectrum, !(data$Energy < Cu$Line[2]-0.02 | data$Energy > Cu$Line[1]+0.02))
    Cu.Ka.frame <- data.frame(is.0(Cu.Ka.cps, Cu.file))
    colnames(Cu.Ka.frame) <- c("Counts", "Spectrum")
    Cu.Ka.ag <- aggregate(list(Cu.Ka.frame$Counts), by=list(Cu.Ka.frame$Spectrum), FUN="sum")
    colnames(Cu.Ka.ag) <- c("Spectrum", "Cu K-alpha")
    
    Cu.Kb.cps <- subset(data$CPS, !(data$Energy < Cu$Line[3]-0.02 | data$Energy > Cu$Line[3]+0.02))
    Cu.file <- subset(data$Spectrum, !(data$Energy < Cu$Line[3]-0.02 | data$Energy > Cu$Line[3]+0.02))
    Cu.Kb.frame <- data.frame(is.0(Cu.Kb.cps, Cu.file))
    colnames(Cu.Kb.frame) <- c("Counts", "Spectrum")
    Cu.Kb.ag <- aggregate(list(Cu.Kb.frame$Counts), by=list(Cu.Kb.frame$Spectrum), FUN="sum")
    colnames(Cu.Kb.ag) <- c("Spectrum", "Cu K-beta")
    
    Zn.Ka.cps <- subset(data$CPS, !(data$Energy < Zn$Line[2]-0.02 | data$Energy > Zn$Line[1]+0.02))
    Zn.file <- subset(data$Spectrum, !(data$Energy < Zn$Line[2]-0.02 | data$Energy > Zn$Line[1]+0.02))
    Zn.Ka.frame <- data.frame(is.0(Zn.Ka.cps, Zn.file))
    colnames(Zn.Ka.frame) <- c("Counts", "Spectrum")
    Zn.Ka.ag <- aggregate(list(Zn.Ka.frame$Counts), by=list(Zn.Ka.frame$Spectrum), FUN="sum")
    colnames(Zn.Ka.ag) <- c("Spectrum", "Zn K-alpha")
    
    Zn.Kb.cps <- subset(data$CPS, !(data$Energy < Zn$Line[3]-0.02 | data$Energy > Zn$Line[3]+0.02))
    Zn.file <- subset(data$Spectrum, !(data$Energy < Zn$Line[3]-0.02 | data$Energy > Zn$Line[3]+0.02))
    Zn.Kb.frame <- data.frame(is.0(Zn.Kb.cps, Zn.file))
    colnames(Zn.Kb.frame) <- c("Counts", "Spectrum")
    Zn.Kb.ag <- aggregate(list(Zn.Kb.frame$Counts), by=list(Zn.Kb.frame$Spectrum), FUN="sum")
    colnames(Zn.Kb.ag) <- c("Spectrum", "Zn K-beta")
    
    Ga.Ka.cps <- subset(data$CPS, !(data$Energy < Ga$Line[2]-0.02 | data$Energy > Ga$Line[1]+0.02))
    Ga.file <- subset(data$Spectrum, !(data$Energy < Ga$Line[2]-0.02 | data$Energy > Ga$Line[1]+0.02))
    Ga.Ka.frame <- data.frame(is.0(Ga.Ka.cps, Ga.file))
    colnames(Ga.Ka.frame) <- c("Counts", "Spectrum")
    Ga.Ka.ag <- aggregate(list(Ga.Ka.frame$Counts), by=list(Ga.Ka.frame$Spectrum), FUN="sum")
    colnames(Ga.Ka.ag) <- c("Spectrum", "Ga K-alpha")
    
    Ga.Kb.cps <- subset(data$CPS, !(data$Energy < Ga$Line[3]-0.02 | data$Energy > Ga$Line[3]+0.02))
    Ga.file <- subset(data$Spectrum, !(data$Energy < Ga$Line[3]-0.02 | data$Energy > Ga$Line[3]+0.02))
    Ga.Kb.frame <- data.frame(is.0(Ga.Kb.cps, Ga.file))
    colnames(Ga.Kb.frame) <- c("Counts", "Spectrum")
    Ga.Kb.ag <- aggregate(list(Ga.Kb.frame$Counts), by=list(Ga.Kb.frame$Spectrum), FUN="sum")
    colnames(Ga.Kb.ag) <- c("Spectrum", "Ga K-beta")
    
    Ge.Ka.cps <- subset(data$CPS, !(data$Energy < Ge$Line[2]-0.02 | data$Energy > Ge$Line[1]+0.02))
    Ge.file <- subset(data$Spectrum, !(data$Energy < Ge$Line[2]-0.02 | data$Energy > Ge$Line[1]+0.02))
    Ge.Ka.frame <- data.frame(is.0(Ge.Ka.cps, Ge.file))
    colnames(Ge.Ka.frame) <- c("Counts", "Spectrum")
    Ge.Ka.ag <- aggregate(list(Ge.Ka.frame$Counts), by=list(Ge.Ka.frame$Spectrum), FUN="sum")
    colnames(Ge.Ka.ag) <- c("Spectrum", "Ge K-alpha")
    
    Ge.Kb.cps <- subset(data$CPS, !(data$Energy < Ge$Line[3]-0.02 | data$Energy > Ge$Line[3]+0.02))
    Ge.file <- subset(data$Spectrum, !(data$Energy < Ge$Line[3]-0.02 | data$Energy > Ge$Line[3]+0.02))
    Ge.Kb.frame <- data.frame(is.0(Ge.Kb.cps, Ge.file))
    colnames(Ge.Kb.frame) <- c("Counts", "Spectrum")
    Ge.Kb.ag <- aggregate(list(Ge.Kb.frame$Counts), by=list(Ge.Kb.frame$Spectrum), FUN="sum")
    colnames(Ge.Kb.ag) <- c("Spectrum", "Ge K-beta")
    
    As.Ka.cps <- subset(data$CPS, !(data$Energy < As$Line[2]-0.02 | data$Energy > As$Line[1]+0.02))
    As.file <- subset(data$Spectrum, !(data$Energy < As$Line[2]-0.02 | data$Energy > As$Line[1]+0.02))
    As.Ka.frame <- data.frame(is.0(As.Ka.cps, As.file))
    colnames(As.Ka.frame) <- c("Counts", "Spectrum")
    As.Ka.ag <- aggregate(list(As.Ka.frame$Counts), by=list(As.Ka.frame$Spectrum), FUN="sum")
    colnames(As.Ka.ag) <- c("Spectrum", "As K-alpha")
    
    As.Kb.cps <- subset(data$CPS, !(data$Energy < As$Line[3]-0.02 | data$Energy > As$Line[3]+0.02))
    As.file <- subset(data$Spectrum, !(data$Energy < As$Line[3]-0.02 | data$Energy > As$Line[3]+0.02))
    As.Kb.frame <- data.frame(is.0(As.Kb.cps, As.file))
    colnames(As.Kb.frame) <- c("Counts", "Spectrum")
    As.Kb.ag <- aggregate(list(As.Kb.frame$Counts), by=list(As.Kb.frame$Spectrum), FUN="sum")
    colnames(As.Kb.ag) <- c("Spectrum", "As K-beta")
    
    Se.Ka.cps <- subset(data$CPS, !(data$Energy < Se$Line[2]-0.02 | data$Energy > Se$Line[1]+0.02))
    Se.file <- subset(data$Spectrum, !(data$Energy < Se$Line[2]-0.02 | data$Energy > Se$Line[1]+0.02))
    Se.Ka.frame <- data.frame(is.0(Se.Ka.cps, Se.file))
    colnames(Se.Ka.frame) <- c("Counts", "Spectrum")
    Se.Ka.ag <- aggregate(list(Se.Ka.frame$Counts), by=list(Se.Ka.frame$Spectrum), FUN="sum")
    colnames(Se.Ka.ag) <- c("Spectrum", "Se K-alpha")
    
    Se.Kb.cps <- subset(data$CPS, !(data$Energy < Se$Line[3]-0.02 | data$Energy > Se$Line[3]+0.02))
    Se.file <- subset(data$Spectrum, !(data$Energy < Se$Line[3]-0.02 | data$Energy > Se$Line[3]+0.02))
    Se.Kb.frame <- data.frame(is.0(Se.Kb.cps, Se.file))
    colnames(Se.Kb.frame) <- c("Counts", "Spectrum")
    Se.Kb.ag <- aggregate(list(Se.Kb.frame$Counts), by=list(Se.Kb.frame$Spectrum), FUN="sum")
    colnames(Se.Kb.ag) <- c("Spectrum", "Se K-beta")
    
    Br.Ka.cps <- subset(data$CPS, !(data$Energy < Br$Line[2]-0.02 | data$Energy > Br$Line[1]+0.02))
    Br.file <- subset(data$Spectrum, !(data$Energy < Br$Line[2]-0.02 | data$Energy > Br$Line[1]+0.02))
    Br.Ka.frame <- data.frame(is.0(Br.Ka.cps, Br.file))
    colnames(Br.Ka.frame) <- c("Counts", "Spectrum")
    Br.Ka.ag <- aggregate(list(Br.Ka.frame$Counts), by=list(Br.Ka.frame$Spectrum), FUN="sum")
    colnames(Br.Ka.ag) <- c("Spectrum", "Br K-alpha")
    
    Br.Kb.cps <- subset(data$CPS, !(data$Energy < Br$Line[3]-0.02 | data$Energy > Br$Line[3]+0.02))
    Br.file <- subset(data$Spectrum, !(data$Energy < Br$Line[3]-0.02 | data$Energy > Br$Line[3]+0.02))
    Br.Kb.frame <- data.frame(is.0(Br.Kb.cps, Br.file))
    colnames(Br.Kb.frame) <- c("Counts", "Spectrum")
    Br.Kb.ag <- aggregate(list(Br.Kb.frame$Counts), by=list(Br.Kb.frame$Spectrum), FUN="sum")
    colnames(Br.Kb.ag) <- c("Spectrum", "Br K-beta")
    
    Kr.Ka.cps <- subset(data$CPS, !(data$Energy < Kr$Line[2]-0.02 | data$Energy > Kr$Line[1]+0.02))
    Kr.file <- subset(data$Spectrum, !(data$Energy < Kr$Line[2]-0.02 | data$Energy > Kr$Line[1]+0.02))
    Kr.Ka.frame <- data.frame(is.0(Kr.Ka.cps, Kr.file))
    colnames(Kr.Ka.frame) <- c("Counts", "Spectrum")
    Kr.Ka.ag <- aggregate(list(Kr.Ka.frame$Counts), by=list(Kr.Ka.frame$Spectrum), FUN="sum")
    colnames(Kr.Ka.ag) <- c("Spectrum", "Kr K-alpha")
    
    Kr.Kb.cps <- subset(data$CPS, !(data$Energy < Kr$Line[3]-0.02 | data$Energy > Kr$Line[3]+0.02))
    Kr.file <- subset(data$Spectrum, !(data$Energy < Kr$Line[3]-0.02 | data$Energy > Kr$Line[3]+0.02))
    Kr.Kb.frame <- data.frame(is.0(Kr.Kb.cps, Kr.file))
    colnames(Kr.Kb.frame) <- c("Counts", "Spectrum")
    Kr.Kb.ag <- aggregate(list(Kr.Kb.frame$Counts), by=list(Kr.Kb.frame$Spectrum), FUN="sum")
    colnames(Kr.Kb.ag) <- c("Spectrum", "Kr K-beta")
    
    Rb.Ka.cps <- subset(data$CPS, !(data$Energy < Rb$Line[2]-0.02 | data$Energy > Rb$Line[1]+0.02))
    Rb.file <- subset(data$Spectrum, !(data$Energy < Rb$Line[2]-0.02 | data$Energy > Rb$Line[1]+0.02))
    Rb.Ka.frame <- data.frame(is.0(Rb.Ka.cps, Rb.file))
    colnames(Rb.Ka.frame) <- c("Counts", "Spectrum")
    Rb.Ka.ag <- aggregate(list(Rb.Ka.frame$Counts), by=list(Rb.Ka.frame$Spectrum), FUN="sum")
    colnames(Rb.Ka.ag) <- c("Spectrum", "Rb K-alpha")
    
    Rb.Kb.cps <- subset(data$CPS, !(data$Energy < Rb$Line[3]-0.02 | data$Energy > Rb$Line[3]+0.02))
    Rb.file <- subset(data$Spectrum, !(data$Energy < Rb$Line[3]-0.02 | data$Energy > Rb$Line[3]+0.02))
    Rb.Kb.frame <- data.frame(is.0(Rb.Kb.cps, Rb.file))
    colnames(Rb.Kb.frame) <- c("Counts", "Spectrum")
    Rb.Kb.ag <- aggregate(list(Rb.Kb.frame$Counts), by=list(Rb.Kb.frame$Spectrum), FUN="sum")
    colnames(Rb.Kb.ag) <- c("Spectrum", "Rb K-beta")
    
    Sr.Ka.cps <- subset(data$CPS, !(data$Energy < Sr$Line[2]-0.02 | data$Energy > Sr$Line[1]+0.02))
    Sr.file <- subset(data$Spectrum, !(data$Energy < Sr$Line[2]-0.02 | data$Energy > Sr$Line[1]+0.02))
    Sr.Ka.frame <- data.frame(is.0(Sr.Ka.cps, Sr.file))
    colnames(Sr.Ka.frame) <- c("Counts", "Spectrum")
    Sr.Ka.ag <- aggregate(list(Sr.Ka.frame$Counts), by=list(Sr.Ka.frame$Spectrum), FUN="sum")
    colnames(Sr.Ka.ag) <- c("Spectrum", "Sr K-alpha")
    
    Sr.Kb.cps <- subset(data$CPS, !(data$Energy < Sr$Line[3]-0.02 | data$Energy > Sr$Line[3]+0.02))
    Sr.file <- subset(data$Spectrum, !(data$Energy < Sr$Line[3]-0.02 | data$Energy > Sr$Line[3]+0.02))
    Sr.Kb.frame <- data.frame(is.0(Sr.Kb.cps, Sr.file))
    colnames(Sr.Kb.frame) <- c("Counts", "Spectrum")
    Sr.Kb.ag <- aggregate(list(Sr.Kb.frame$Counts), by=list(Sr.Kb.frame$Spectrum), FUN="sum")
    colnames(Sr.Kb.ag) <- c("Spectrum", "Sr K-beta")
    
    Y.Ka.cps <- subset(data$CPS, !(data$Energy < Y$Line[2]-0.02 | data$Energy > Y$Line[1]+0.02))
    Y.file <- subset(data$Spectrum, !(data$Energy < Y$Line[2]-0.02 | data$Energy > Y$Line[1]+0.02))
    Y.Ka.frame <- data.frame(is.0(Y.Ka.cps, Y.file))
    colnames(Y.Ka.frame) <- c("Counts", "Spectrum")
    Y.Ka.ag <- aggregate(list(Y.Ka.frame$Counts), by=list(Y.Ka.frame$Spectrum), FUN="sum")
    colnames(Y.Ka.ag) <- c("Spectrum", "Y K-alpha")
    
    Y.Kb.cps <- subset(data$CPS, !(data$Energy < Y$Line[3]-0.02 | data$Energy > Y$Line[3]+0.02))
    Y.file <- subset(data$Spectrum, !(data$Energy < Y$Line[3]-0.02 | data$Energy > Y$Line[3]+0.02))
    Y.Kb.frame <- data.frame(is.0(Y.Kb.cps, Y.file))
    colnames(Y.Kb.frame) <- c("Counts", "Spectrum")
    Y.Kb.ag <- aggregate(list(Y.Kb.frame$Counts), by=list(Y.Kb.frame$Spectrum), FUN="sum")
    colnames(Y.Kb.ag) <- c("Spectrum", "Y K-beta")
    
    Zr.Ka.cps <- subset(data$CPS, !(data$Energy < Zr$Line[2]-0.02 | data$Energy > Zr$Line[1]+0.02))
    Zr.file <- subset(data$Spectrum, !(data$Energy < Zr$Line[2]-0.02 | data$Energy > Zr$Line[1]+0.02))
    Zr.Ka.frame <- data.frame(is.0(Zr.Ka.cps, Zr.file))
    colnames(Zr.Ka.frame) <- c("Counts", "Spectrum")
    Zr.Ka.ag <- aggregate(list(Zr.Ka.frame$Counts), by=list(Zr.Ka.frame$Spectrum), FUN="sum")
    colnames(Zr.Ka.ag) <- c("Spectrum", "Zr K-alpha")
    
    Zr.Kb.cps <- subset(data$CPS, !(data$Energy < Zr$Line[3]-0.02 | data$Energy > Zr$Line[3]+0.02))
    Zr.file <- subset(data$Spectrum, !(data$Energy < Zr$Line[3]-0.02 | data$Energy > Zr$Line[3]+0.02))
    Zr.Kb.frame <- data.frame(is.0(Zr.Kb.cps, Zr.file))
    colnames(Zr.Kb.frame) <- c("Counts", "Spectrum")
    Zr.Kb.ag <- aggregate(list(Zr.Kb.frame$Counts), by=list(Zr.Kb.frame$Spectrum), FUN="sum")
    colnames(Zr.Kb.ag) <- c("Spectrum", "Zr K-beta")
    
    Nb.Ka.cps <- subset(data$CPS, !(data$Energy < Nb$Line[2]-0.02 | data$Energy > Nb$Line[1]+0.02))
    Nb.file <- subset(data$Spectrum, !(data$Energy < Nb$Line[2]-0.02 | data$Energy > Nb$Line[1]+0.02))
    Nb.Ka.frame <- data.frame(is.0(Nb.Ka.cps, Nb.file))
    colnames(Nb.Ka.frame) <- c("Counts", "Spectrum")
    Nb.Ka.ag <- aggregate(list(Nb.Ka.frame$Counts), by=list(Nb.Ka.frame$Spectrum), FUN="sum")
    colnames(Nb.Ka.ag) <- c("Spectrum", "Nb K-alpha")
    
    Nb.Kb.cps <- subset(data$CPS, !(data$Energy < Nb$Line[3]-0.02 | data$Energy > Nb$Line[3]+0.02))
    Nb.file <- subset(data$Spectrum, !(data$Energy < Nb$Line[3]-0.02 | data$Energy > Nb$Line[3]+0.02))
    Nb.Kb.frame <- data.frame(is.0(Nb.Kb.cps, Nb.file))
    colnames(Nb.Kb.frame) <- c("Counts", "Spectrum")
    Nb.Kb.ag <- aggregate(list(Nb.Kb.frame$Counts), by=list(Nb.Kb.frame$Spectrum), FUN="sum")
    colnames(Nb.Kb.ag) <- c("Spectrum", "Nb K-beta")
    
    Mo.Ka.cps <- subset(data$CPS, !(data$Energy < Mo$Line[2]-0.02 | data$Energy > Mo$Line[1]+0.02))
    Mo.file <- subset(data$Spectrum, !(data$Energy < Mo$Line[2]-0.02 | data$Energy > Mo$Line[1]+0.02))
    Mo.Ka.frame <- data.frame(is.0(Mo.Ka.cps, Mo.file))
    colnames(Mo.Ka.frame) <- c("Counts", "Spectrum")
    Mo.Ka.ag <- aggregate(list(Mo.Ka.frame$Counts), by=list(Mo.Ka.frame$Spectrum), FUN="sum")
    colnames(Mo.Ka.ag) <- c("Spectrum", "Mo K-alpha")
    
    Mo.Kb.cps <- subset(data$CPS, !(data$Energy < Mo$Line[3]-0.02 | data$Energy > Mo$Line[3]+0.02))
    Mo.file <- subset(data$Spectrum, !(data$Energy < Mo$Line[3]-0.02 | data$Energy > Mo$Line[3]+0.02))
    Mo.Kb.frame <- data.frame(is.0(Mo.Kb.cps, Mo.file))
    colnames(Mo.Kb.frame) <- c("Counts", "Spectrum")
    Mo.Kb.ag <- aggregate(list(Mo.Kb.frame$Counts), by=list(Mo.Kb.frame$Spectrum), FUN="sum")
    colnames(Mo.Kb.ag) <- c("Spectrum", "Mo K-beta")
    
    Tc.Ka.cps <- subset(data$CPS, !(data$Energy < Tc$Line[2]-0.02 | data$Energy > Tc$Line[1]+0.02))
    Tc.file <- subset(data$Spectrum, !(data$Energy < Tc$Line[2]-0.02 | data$Energy > Tc$Line[1]+0.02))
    Tc.Ka.frame <- data.frame(is.0(Tc.Ka.cps, Tc.file))
    colnames(Tc.Ka.frame) <- c("Counts", "Spectrum")
    Tc.Ka.ag <- aggregate(list(Tc.Ka.frame$Counts), by=list(Tc.Ka.frame$Spectrum), FUN="sum")
    colnames(Tc.Ka.ag) <- c("Spectrum", "Tc K-alpha")
    
    Tc.Kb.cps <- subset(data$CPS, !(data$Energy < Tc$Line[3]-0.02 | data$Energy > Tc$Line[3]+0.02))
    Tc.file <- subset(data$Spectrum, !(data$Energy < Tc$Line[3]-0.02 | data$Energy > Tc$Line[3]+0.02))
    Tc.Kb.frame <- data.frame(is.0(Tc.Kb.cps, Tc.file))
    colnames(Tc.Kb.frame) <- c("Counts", "Spectrum")
    Tc.Kb.ag <- aggregate(list(Tc.Kb.frame$Counts), by=list(Tc.Kb.frame$Spectrum), FUN="sum")
    colnames(Tc.Kb.ag) <- c("Spectrum", "Tc K-beta")
    
    Ru.Ka.cps <- subset(data$CPS, !(data$Energy < Ru$Line[2]-0.02 | data$Energy > Ru$Line[1]+0.02))
    Ru.file <- subset(data$Spectrum, !(data$Energy < Ru$Line[2]-0.02 | data$Energy > Ru$Line[1]+0.02))
    Ru.Ka.frame <- data.frame(is.0(Ru.Ka.cps, Ru.file))
    colnames(Ru.Ka.frame) <- c("Counts", "Spectrum")
    Ru.Ka.ag <- aggregate(list(Ru.Ka.frame$Counts), by=list(Ru.Ka.frame$Spectrum), FUN="sum")
    colnames(Ru.Ka.ag) <- c("Spectrum", "Ru K-alpha")
    
    Ru.Kb.cps <- subset(data$CPS, !(data$Energy < Ru$Line[3]-0.02 | data$Energy > Ru$Line[3]+0.02))
    Ru.file <- subset(data$Spectrum, !(data$Energy < Ru$Line[3]-0.02 | data$Energy > Ru$Line[3]+0.02))
    Ru.Kb.frame <- data.frame(is.0(Ru.Kb.cps, Ru.file))
    colnames(Ru.Kb.frame) <- c("Counts", "Spectrum")
    Ru.Kb.ag <- aggregate(list(Ru.Kb.frame$Counts), by=list(Ru.Kb.frame$Spectrum), FUN="sum")
    colnames(Ru.Kb.ag) <- c("Spectrum", "Ru K-beta")
    
    Rh.Ka.cps <- subset(data$CPS, !(data$Energy < Rh$Line[2]-0.02 | data$Energy > Rh$Line[1]+0.02))
    Rh.file <- subset(data$Spectrum, !(data$Energy < Rh$Line[2]-0.02 | data$Energy > Rh$Line[1]+0.02))
    Rh.Ka.frame <- data.frame(is.0(Rh.Ka.cps, Rh.file))
    colnames(Rh.Ka.frame) <- c("Counts", "Spectrum")
    Rh.Ka.ag <- aggregate(list(Rh.Ka.frame$Counts), by=list(Rh.Ka.frame$Spectrum), FUN="sum")
    colnames(Rh.Ka.ag) <- c("Spectrum", "Rh K-alpha")
    
    Rh.Kb.cps <- subset(data$CPS, !(data$Energy < Rh$Line[3]-0.02 | data$Energy > Rh$Line[3]+0.02))
    Rh.file <- subset(data$Spectrum, !(data$Energy < Rh$Line[3]-0.02 | data$Energy > Rh$Line[3]+0.02))
    Rh.Kb.frame <- data.frame(is.0(Rh.Kb.cps, Rh.file))
    colnames(Rh.Kb.frame) <- c("Counts", "Spectrum")
    Rh.Kb.ag <- aggregate(list(Rh.Kb.frame$Counts), by=list(Rh.Kb.frame$Spectrum), FUN="sum")
    colnames(Rh.Kb.ag) <- c("Spectrum", "Rh K-beta")
    
    Pd.Ka.cps <- subset(data$CPS, !(data$Energy < Pd$Line[2]-0.02 | data$Energy > Pd$Line[1]+0.02))
    Pd.file <- subset(data$Spectrum, !(data$Energy < Pd$Line[2]-0.02 | data$Energy > Pd$Line[1]+0.02))
    Pd.Ka.frame <- data.frame(is.0(Pd.Ka.cps, Pd.file))
    colnames(Pd.Ka.frame) <- c("Counts", "Spectrum")
    Pd.Ka.ag <- aggregate(list(Pd.Ka.frame$Counts), by=list(Pd.Ka.frame$Spectrum), FUN="sum")
    colnames(Pd.Ka.ag) <- c("Spectrum", "Pd K-alpha")
    
    Pd.Kb.cps <- subset(data$CPS, !(data$Energy < Pd$Line[3]-0.02 | data$Energy > Pd$Line[3]+0.02))
    Pd.file <- subset(data$Spectrum, !(data$Energy < Pd$Line[3]-0.02 | data$Energy > Pd$Line[3]+0.02))
    Pd.Kb.frame <- data.frame(is.0(Pd.Kb.cps, Pd.file))
    colnames(Pd.Kb.frame) <- c("Counts", "Spectrum")
    Pd.Kb.ag <- aggregate(list(Pd.Kb.frame$Counts), by=list(Pd.Kb.frame$Spectrum), FUN="sum")
    colnames(Pd.Kb.ag) <- c("Spectrum", "Pd K-beta")
    
    Ag.Ka.cps <- subset(data$CPS, !(data$Energy < Ag$Line[2]-0.02 | data$Energy > Ag$Line[1]+0.02))
    Ag.file <- subset(data$Spectrum, !(data$Energy < Ag$Line[2]-0.02 | data$Energy > Ag$Line[1]+0.02))
    Ag.Ka.frame <- data.frame(is.0(Ag.Ka.cps, Ag.file))
    colnames(Ag.Ka.frame) <- c("Counts", "Spectrum")
    Ag.Ka.ag <- aggregate(list(Ag.Ka.frame$Counts), by=list(Ag.Ka.frame$Spectrum), FUN="sum")
    colnames(Ag.Ka.ag) <- c("Spectrum", "Ag K-alpha")
    
    Ag.Kb.cps <- subset(data$CPS, !(data$Energy < Ag$Line[3]-0.02 | data$Energy > Ag$Line[3]+0.02))
    Ag.file <- subset(data$Spectrum, !(data$Energy < Ag$Line[3]-0.02 | data$Energy > Ag$Line[3]+0.02))
    Ag.Kb.frame <- data.frame(is.0(Ag.Kb.cps, Ag.file))
    colnames(Ag.Kb.frame) <- c("Counts", "Spectrum")
    Ag.Kb.ag <- aggregate(list(Ag.Kb.frame$Counts), by=list(Ag.Kb.frame$Spectrum), FUN="sum")
    colnames(Ag.Kb.ag) <- c("Spectrum", "Ag K-beta")
    
    Cd.Ka.cps <- subset(data$CPS, !(data$Energy < Cd$Line[2]-0.02 | data$Energy > Cd$Line[1]+0.02))
    Cd.file <- subset(data$Spectrum, !(data$Energy < Cd$Line[2]-0.02 | data$Energy > Cd$Line[1]+0.02))
    Cd.Ka.frame <- data.frame(is.0(Cd.Ka.cps, Cd.file))
    colnames(Cd.Ka.frame) <- c("Counts", "Spectrum")
    Cd.Ka.ag <- aggregate(list(Cd.Ka.frame$Counts), by=list(Cd.Ka.frame$Spectrum), FUN="sum")
    colnames(Cd.Ka.ag) <- c("Spectrum", "Cd K-alpha")
    
    Cd.Kb.cps <- subset(data$CPS, !(data$Energy < Cd$Line[3]-0.02 | data$Energy > Cd$Line[3]+0.02))
    Cd.file <- subset(data$Spectrum, !(data$Energy < Cd$Line[3]-0.02 | data$Energy > Cd$Line[3]+0.02))
    Cd.Kb.frame <- data.frame(is.0(Cd.Kb.cps, Cd.file))
    colnames(Cd.Kb.frame) <- c("Counts", "Spectrum")
    Cd.Kb.ag <- aggregate(list(Cd.Kb.frame$Counts), by=list(Cd.Kb.frame$Spectrum), FUN="sum")
    colnames(Cd.Kb.ag) <- c("Spectrum", "Cd K-beta")
    
    In.Ka.cps <- subset(data$CPS, !(data$Energy < In$Line[2]-0.02 | data$Energy > In$Line[1]+0.02))
    In.file <- subset(data$Spectrum, !(data$Energy < In$Line[2]-0.02 | data$Energy > In$Line[1]+0.02))
    In.Ka.frame <- data.frame(is.0(In.Ka.cps, In.file))
    colnames(In.Ka.frame) <- c("Counts", "Spectrum")
    In.Ka.ag <- aggregate(list(In.Ka.frame$Counts), by=list(In.Ka.frame$Spectrum), FUN="sum")
    colnames(In.Ka.ag) <- c("Spectrum", "In K-alpha")
    
    In.Kb.cps <- subset(data$CPS, !(data$Energy < In$Line[3]-0.02 | data$Energy > In$Line[3]+0.02))
    In.file <- subset(data$Spectrum, !(data$Energy < In$Line[3]-0.02 | data$Energy > In$Line[3]+0.02))
    In.Kb.frame <- data.frame(is.0(In.Kb.cps, In.file))
    colnames(In.Kb.frame) <- c("Counts", "Spectrum")
    In.Kb.ag <- aggregate(list(In.Kb.frame$Counts), by=list(In.Kb.frame$Spectrum), FUN="sum")
    colnames(In.Kb.ag) <- c("Spectrum", "In K-beta")
    
    Sn.Ka.cps <- subset(data$CPS, !(data$Energy < Sn$Line[2]-0.02 | data$Energy > Sn$Line[1]+0.02))
    Sn.file <- subset(data$Spectrum, !(data$Energy < Sn$Line[2]-0.02 | data$Energy > Sn$Line[1]+0.02))
    Sn.Ka.frame <- data.frame(is.0(Sn.Ka.cps, Sn.file))
    colnames(Sn.Ka.frame) <- c("Counts", "Spectrum")
    Sn.Ka.ag <- aggregate(list(Sn.Ka.frame$Counts), by=list(Sn.Ka.frame$Spectrum), FUN="sum")
    colnames(Sn.Ka.ag) <- c("Spectrum", "Sn K-alpha")
    
    Sn.Kb.cps <- subset(data$CPS, !(data$Energy < Sn$Line[3]-0.02 | data$Energy > Sn$Line[3]+0.02))
    Sn.file <- subset(data$Spectrum, !(data$Energy < Sn$Line[3]-0.02 | data$Energy > Sn$Line[3]+0.02))
    Sn.Kb.frame <- data.frame(is.0(Sn.Kb.cps, Sn.file))
    colnames(Sn.Kb.frame) <- c("Counts", "Spectrum")
    Sn.Kb.ag <- aggregate(list(Sn.Kb.frame$Counts), by=list(Sn.Kb.frame$Spectrum), FUN="sum")
    colnames(Sn.Kb.ag) <- c("Spectrum", "Sn K-beta")
    
    Sb.Ka.cps <- subset(data$CPS, !(data$Energy < Sb$Line[2]-0.02 | data$Energy > Sb$Line[1]+0.02))
    Sb.file <- subset(data$Spectrum, !(data$Energy < Sb$Line[2]-0.02 | data$Energy > Sb$Line[1]+0.02))
    Sb.Ka.frame <- data.frame(is.0(Sb.Ka.cps, Sb.file))
    colnames(Sb.Ka.frame) <- c("Counts", "Spectrum")
    Sb.Ka.ag <- aggregate(list(Sb.Ka.frame$Counts), by=list(Sb.Ka.frame$Spectrum), FUN="sum")
    colnames(Sb.Ka.ag) <- c("Spectrum", "Sb K-alpha")
    
    Sb.Kb.cps <- subset(data$CPS, !(data$Energy < Sb$Line[3]-0.02 | data$Energy > Sb$Line[3]+0.02))
    Sb.file <- subset(data$Spectrum, !(data$Energy < Sb$Line[3]-0.02 | data$Energy > Sb$Line[3]+0.02))
    Sb.Kb.frame <- data.frame(is.0(Sb.Kb.cps, Sb.file))
    colnames(Sb.Kb.frame) <- c("Counts", "Spectrum")
    Sb.Kb.ag <- aggregate(list(Sb.Kb.frame$Counts), by=list(Sb.Kb.frame$Spectrum), FUN="sum")
    colnames(Sb.Kb.ag) <- c("Spectrum", "Sb K-beta")
    
    Te.Ka.cps <- subset(data$CPS, !(data$Energy < Te$Line[2]-0.02 | data$Energy > Te$Line[1]+0.02))
    Te.file <- subset(data$Spectrum, !(data$Energy < Te$Line[2]-0.02 | data$Energy > Te$Line[1]+0.02))
    Te.Ka.frame <- data.frame(is.0(Te.Ka.cps, Te.file))
    colnames(Te.Ka.frame) <- c("Counts", "Spectrum")
    Te.Ka.ag <- aggregate(list(Te.Ka.frame$Counts), by=list(Te.Ka.frame$Spectrum), FUN="sum")
    colnames(Te.Ka.ag) <- c("Spectrum", "Te K-alpha")
    
    Te.Kb.cps <- subset(data$CPS, !(data$Energy < Te$Line[3]-0.02 | data$Energy > Te$Line[3]+0.02))
    Te.file <- subset(data$Spectrum, !(data$Energy < Te$Line[3]-0.02 | data$Energy > Te$Line[3]+0.02))
    Te.Kb.frame <- data.frame(is.0(Te.Kb.cps, Te.file))
    colnames(Te.Kb.frame) <- c("Counts", "Spectrum")
    Te.Kb.ag <- aggregate(list(Te.Kb.frame$Counts), by=list(Te.Kb.frame$Spectrum), FUN="sum")
    colnames(Te.Kb.ag) <- c("Spectrum", "Te K-beta")
    
    I.Ka.cps <- subset(data$CPS, !(data$Energy < I$Line[2]-0.02 | data$Energy > I$Line[1]+0.02))
    I.file <- subset(data$Spectrum, !(data$Energy < I$Line[2]-0.02 | data$Energy > I$Line[1]+0.02))
    I.Ka.frame <- data.frame(is.0(I.Ka.cps, I.file))
    colnames(I.Ka.frame) <- c("Counts", "Spectrum")
    I.Ka.ag <- aggregate(list(I.Ka.frame$Counts), by=list(I.Ka.frame$Spectrum), FUN="sum")
    colnames(I.Ka.ag) <- c("Spectrum", "I K-alpha")
    
    I.Kb.cps <- subset(data$CPS, !(data$Energy < I$Line[3]-0.02 | data$Energy > I$Line[3]+0.02))
    I.file <- subset(data$Spectrum, !(data$Energy < I$Line[3]-0.02 | data$Energy > I$Line[3]+0.02))
    I.Kb.frame <- data.frame(is.0(I.Kb.cps, I.file))
    colnames(I.Kb.frame) <- c("Counts", "Spectrum")
    I.Kb.ag <- aggregate(list(I.Kb.frame$Counts), by=list(I.Kb.frame$Spectrum), FUN="sum")
    colnames(I.Kb.ag) <- c("Spectrum", "I K-beta")
    
    Xe.Ka.cps <- subset(data$CPS, !(data$Energy < Xe$Line[2]-0.02 | data$Energy > Xe$Line[1]+0.02))
    Xe.file <- subset(data$Spectrum, !(data$Energy < Xe$Line[2]-0.02 | data$Energy > Xe$Line[1]+0.02))
    Xe.Ka.frame <- data.frame(is.0(Xe.Ka.cps, Xe.file))
    colnames(Xe.Ka.frame) <- c("Counts", "Spectrum")
    Xe.Ka.ag <- aggregate(list(Xe.Ka.frame$Counts), by=list(Xe.Ka.frame$Spectrum), FUN="sum")
    colnames(Xe.Ka.ag) <- c("Spectrum", "Xe K-alpha")
    
    Xe.Kb.cps <- subset(data$CPS, !(data$Energy < Xe$Line[3]-0.02 | data$Energy > Xe$Line[3]+0.02))
    Xe.file <- subset(data$Spectrum, !(data$Energy < Xe$Line[3]-0.02 | data$Energy > Xe$Line[3]+0.02))
    Xe.Kb.frame <- data.frame(is.0(Xe.Kb.cps, Xe.file))
    colnames(Xe.Kb.frame) <- c("Counts", "Spectrum")
    Xe.Kb.ag <- aggregate(list(Xe.Kb.frame$Counts), by=list(Xe.Kb.frame$Spectrum), FUN="sum")
    colnames(Xe.Kb.ag) <- c("Spectrum", "Xe K-beta")
    
    Cs.Ka.cps <- subset(data$CPS, !(data$Energy < Cs$Line[2]-0.02 | data$Energy > Cs$Line[1]+0.02))
    Cs.file <- subset(data$Spectrum, !(data$Energy < Cs$Line[2]-0.02 | data$Energy > Cs$Line[1]+0.02))
    Cs.Ka.frame <- data.frame(is.0(Cs.Ka.cps, Cs.file))
    colnames(Cs.Ka.frame) <- c("Counts", "Spectrum")
    Cs.Ka.ag <- aggregate(list(Cs.Ka.frame$Counts), by=list(Cs.Ka.frame$Spectrum), FUN="sum")
    colnames(Cs.Ka.ag) <- c("Spectrum", "Cs K-alpha")
    
    Cs.Kb.cps <- subset(data$CPS, !(data$Energy < Cs$Line[3]-0.02 | data$Energy > Cs$Line[3]+0.02))
    Cs.file <- subset(data$Spectrum, !(data$Energy < Cs$Line[3]-0.02 | data$Energy > Cs$Line[3]+0.02))
    Cs.Kb.frame <- data.frame(is.0(Cs.Kb.cps, Cs.file))
    colnames(Cs.Kb.frame) <- c("Counts", "Spectrum")
    Cs.Kb.ag <- aggregate(list(Cs.Kb.frame$Counts), by=list(Cs.Kb.frame$Spectrum), FUN="sum")
    colnames(Cs.Kb.ag) <- c("Spectrum", "Cs K-beta")
    
    Ba.Ka.cps <- subset(data$CPS, !(data$Energy < Ba$Line[2]-0.02 | data$Energy > Ba$Line[1]+0.02))
    Ba.file <- subset(data$Spectrum, !(data$Energy < Ba$Line[2]-0.02 | data$Energy > Ba$Line[1]+0.02))
    Ba.Ka.frame <- data.frame(is.0(Ba.Ka.cps, Ba.file))
    colnames(Ba.Ka.frame) <- c("Counts", "Spectrum")
    Ba.Ka.ag <- aggregate(list(Ba.Ka.frame$Counts), by=list(Ba.Ka.frame$Spectrum), FUN="sum")
    colnames(Ba.Ka.ag) <- c("Spectrum", "Ba K-alpha")
    
    Ba.Kb.cps <- subset(data$CPS, !(data$Energy < Ba$Line[3]-0.02 | data$Energy > Ba$Line[3]+0.02))
    Ba.file <- subset(data$Spectrum, !(data$Energy < Ba$Line[3]-0.02 | data$Energy > Ba$Line[3]+0.02))
    Ba.Kb.frame <- data.frame(is.0(Ba.Kb.cps, Ba.file))
    colnames(Ba.Kb.frame) <- c("Counts", "Spectrum")
    Ba.Kb.ag <- aggregate(list(Ba.Kb.frame$Counts), by=list(Ba.Kb.frame$Spectrum), FUN="sum")
    colnames(Ba.Kb.ag) <- c("Spectrum", "Ba K-beta")
    
    La.Ka.cps <- subset(data$CPS, !(data$Energy < La$Line[2]-0.02 | data$Energy > La$Line[1]+0.02))
    La.file <- subset(data$Spectrum, !(data$Energy < La$Line[2]-0.02 | data$Energy > La$Line[1]+0.02))
    La.Ka.frame <- data.frame(is.0(La.Ka.cps, La.file))
    colnames(La.Ka.frame) <- c("Counts", "Spectrum")
    La.Ka.ag <- aggregate(list(La.Ka.frame$Counts), by=list(La.Ka.frame$Spectrum), FUN="sum")
    colnames(La.Ka.ag) <- c("Spectrum", "La K-alpha")
    
    La.Kb.cps <- subset(data$CPS, !(data$Energy < La$Line[3]-0.02 | data$Energy > La$Line[3]+0.02))
    La.file <- subset(data$Spectrum, !(data$Energy < La$Line[3]-0.02 | data$Energy > La$Line[3]+0.02))
    La.Kb.frame <- data.frame(is.0(La.Kb.cps, La.file))
    colnames(La.Kb.frame) <- c("Counts", "Spectrum")
    La.Kb.ag <- aggregate(list(La.Kb.frame$Counts), by=list(La.Kb.frame$Spectrum), FUN="sum")
    colnames(La.Kb.ag) <- c("Spectrum", "La K-beta")
    
    Ce.Ka.cps <- subset(data$CPS, !(data$Energy < Ce$Line[2]-0.02 | data$Energy > Ce$Line[1]+0.02))
    Ce.file <- subset(data$Spectrum, !(data$Energy < Ce$Line[2]-0.02 | data$Energy > Ce$Line[1]+0.02))
    Ce.Ka.frame <- data.frame(is.0(Ce.Ka.cps, Ce.file))
    colnames(Ce.Ka.frame) <- c("Counts", "Spectrum")
    Ce.Ka.ag <- aggregate(list(Ce.Ka.frame$Counts), by=list(Ce.Ka.frame$Spectrum), FUN="sum")
    colnames(Ce.Ka.ag) <- c("Spectrum", "Ce K-alpha")
    
    Ce.Kb.cps <- subset(data$CPS, !(data$Energy < Ce$Line[3]-0.02 | data$Energy > Ce$Line[3]+0.02))
    Ce.file <- subset(data$Spectrum, !(data$Energy < Ce$Line[3]-0.02 | data$Energy > Ce$Line[3]+0.02))
    Ce.Kb.frame <- data.frame(is.0(Ce.Kb.cps, Ce.file))
    colnames(Ce.Kb.frame) <- c("Counts", "Spectrum")
    Ce.Kb.ag <- aggregate(list(Ce.Kb.frame$Counts), by=list(Ce.Kb.frame$Spectrum), FUN="sum")
    colnames(Ce.Kb.ag) <- c("Spectrum", "Ce K-beta")
    
    Pr.Ka.cps <- subset(data$CPS, !(data$Energy < Pr$Line[2]-0.02 | data$Energy > Pr$Line[1]+0.02))
    Pr.file <- subset(data$Spectrum, !(data$Energy < Pr$Line[2]-0.02 | data$Energy > Pr$Line[1]+0.02))
    Pr.Ka.frame <- data.frame(is.0(Pr.Ka.cps, Pr.file))
    colnames(Pr.Ka.frame) <- c("Counts", "Spectrum")
    Pr.Ka.ag <- aggregate(list(Pr.Ka.frame$Counts), by=list(Pr.Ka.frame$Spectrum), FUN="sum")
    colnames(Pr.Ka.ag) <- c("Spectrum", "Pr K-alpha")
    
    Pr.Kb.cps <- subset(data$CPS, !(data$Energy < Pr$Line[3]-0.02 | data$Energy > Pr$Line[3]+0.02))
    Pr.file <- subset(data$Spectrum, !(data$Energy < Pr$Line[3]-0.02 | data$Energy > Pr$Line[3]+0.02))
    Pr.Kb.frame <- data.frame(is.0(Pr.Kb.cps, Pr.file))
    colnames(Pr.Kb.frame) <- c("Counts", "Spectrum")
    Pr.Kb.ag <- aggregate(list(Pr.Kb.frame$Counts), by=list(Pr.Kb.frame$Spectrum), FUN="sum")
    colnames(Pr.Kb.ag) <- c("Spectrum", "Pr K-beta")
    
    Nd.Ka.cps <- subset(data$CPS, !(data$Energy < Nd$Line[2]-0.02 | data$Energy > Nd$Line[1]+0.02))
    Nd.file <- subset(data$Spectrum, !(data$Energy < Nd$Line[2]-0.02 | data$Energy > Nd$Line[1]+0.02))
    Nd.Ka.frame <- data.frame(is.0(Nd.Ka.cps, Nd.file))
    colnames(Nd.Ka.frame) <- c("Counts", "Spectrum")
    Nd.Ka.ag <- aggregate(list(Nd.Ka.frame$Counts), by=list(Nd.Ka.frame$Spectrum), FUN="sum")
    colnames(Nd.Ka.ag) <- c("Spectrum", "Nd K-alpha")
    
    Nd.Kb.cps <- subset(data$CPS, !(data$Energy < Nd$Line[3]-0.02 | data$Energy > Nd$Line[3]+0.02))
    Nd.file <- subset(data$Spectrum, !(data$Energy < Nd$Line[3]-0.02 | data$Energy > Nd$Line[3]+0.02))
    Nd.Kb.frame <- data.frame(is.0(Nd.Kb.cps, Nd.file))
    colnames(Nd.Kb.frame) <- c("Counts", "Spectrum")
    Nd.Kb.ag <- aggregate(list(Nd.Kb.frame$Counts), by=list(Nd.Kb.frame$Spectrum), FUN="sum")
    colnames(Nd.Kb.ag) <- c("Spectrum", "Nd K-beta")
    
    Mo.La.cps <- subset(data$CPS, !(data$Energy < Mo$Line[7]-0.02 | data$Energy > Mo$Line[6]+0.02))
    Mo.file <- subset(data$Spectrum, !(data$Energy < Mo$Line[7]-0.02 | data$Energy > Mo$Line[6]+0.02))
    Mo.La.frame <- is.0(Mo.La.cps,Mo.file)
    colnames(Mo.La.frame) <- c("Counts", "Spectrum")
    Mo.La.ag <- aggregate(list(Mo.La.frame$Counts), by=list(Mo.La.frame$Spectrum), FUN="sum")
    colnames(Mo.La.ag) <- c("Spectrum", "Mo L-alpha")
    
    Tc.La.cps <- subset(data$CPS, !(data$Energy < Tc$Line[7]-0.02 | data$Energy > Tc$Line[6]+0.02))
    Tc.file <- subset(data$Spectrum, !(data$Energy < Tc$Line[7]-0.02 | data$Energy > Tc$Line[6]+0.02))
    Tc.La.frame <- is.0(Tc.La.cps,Tc.file)
    colnames(Tc.La.frame) <- c("Counts", "Spectrum")
    Tc.La.ag <- aggregate(list(Tc.La.frame$Counts), by=list(Tc.La.frame$Spectrum), FUN="sum")
    colnames(Tc.La.ag) <- c("Spectrum", "Tc L-alpha")
    
    Ru.La.cps <- subset(data$CPS, !(data$Energy < Ru$Line[7]-0.02 | data$Energy > Ru$Line[6]+0.02))
    Ru.file <- subset(data$Spectrum, !(data$Energy < Ru$Line[7]-0.02 | data$Energy > Ru$Line[6]+0.02))
    Ru.La.frame <- is.0(Ru.La.cps,Ru.file)
    colnames(Ru.La.frame) <- c("Counts", "Spectrum")
    Ru.La.ag <- aggregate(list(Ru.La.frame$Counts), by=list(Ru.La.frame$Spectrum), FUN="sum")
    colnames(Ru.La.ag) <- c("Spectrum", "Ru L-alpha")
    
    Rh.La.cps <- subset(data$CPS, !(data$Energy < Rh$Line[7]-0.02 | data$Energy > Rh$Line[6]+0.02))
    Rh.file <- subset(data$Spectrum, !(data$Energy < Rh$Line[7]-0.02 | data$Energy > Rh$Line[6]+0.02))
    Rh.La.frame <- data.frame(is.0(Rh.La.cps, Rh.file))
    colnames(Rh.La.frame) <- c("Counts", "Spectrum")
    Rh.La.ag <- aggregate(list(Rh.La.frame$Counts), by=list(Rh.La.frame$Spectrum), FUN="sum")
    colnames(Rh.La.ag) <- c("Spectrum", "Rh L-alpha")
    
    Pd.La.cps <- subset(data$CPS, !(data$Energy < Pd$Line[7]-0.02 | data$Energy > Pd$Line[6]+0.02))
    Pd.file <- subset(data$Spectrum, !(data$Energy < Pd$Line[7]-0.02 | data$Energy > Pd$Line[6]+0.02))
    Pd.La.frame <- data.frame(is.0(Pd.La.cps, Pd.file))
    colnames(Pd.La.frame) <- c("Counts", "Spectrum")
    Pd.La.ag <- aggregate(list(Pd.La.frame$Counts), by=list(Pd.La.frame$Spectrum), FUN="sum")
    colnames(Pd.La.ag) <- c("Spectrum", "Pd L-alpha")
    
    Ag.La.cps <- subset(data$CPS, !(data$Energy < Ag$Line[7]-0.02 | data$Energy > Ag$Line[6]+0.02))
    Ag.file <- subset(data$Spectrum, !(data$Energy < Ag$Line[7]-0.02 | data$Energy > Ag$Line[6]+0.02))
    Ag.La.frame <- data.frame(is.0(Ag.La.cps, Ag.file))
    colnames(Ag.La.frame) <- c("Counts", "Spectrum")
    Ag.La.ag <- aggregate(list(Ag.La.frame$Counts), by=list(Ag.La.frame$Spectrum), FUN="sum")
    colnames(Ag.La.ag) <- c("Spectrum", "Ag L-alpha")
    
    Cd.La.cps <- subset(data$CPS, !(data$Energy < Cd$Line[7]-0.02 | data$Energy > Cd$Line[6]+0.02))
    Cd.file <- subset(data$Spectrum, !(data$Energy < Cd$Line[7]-0.02 | data$Energy > Cd$Line[6]+0.02))
    Cd.La.frame <- data.frame(is.0(Cd.La.cps, Cd.file))
    colnames(Cd.La.frame) <- c("Counts", "Spectrum")
    Cd.La.ag <- aggregate(list(Cd.La.frame$Counts), by=list(Cd.La.frame$Spectrum), FUN="sum")
    colnames(Cd.La.ag) <- c("Spectrum", "Cd L-alpha")
    
    In.La.cps <- subset(data$CPS, !(data$Energy < In$Line[7]-0.02 | data$Energy > In$Line[6]+0.02))
    In.file <- subset(data$Spectrum, !(data$Energy < In$Line[7]-0.02 | data$Energy > In$Line[6]+0.02))
    In.La.frame <- data.frame(is.0(In.La.cps, In.file))
    colnames(In.La.frame) <- c("Counts", "Spectrum")
    In.La.ag <- aggregate(list(In.La.frame$Counts), by=list(In.La.frame$Spectrum), FUN="sum")
    colnames(In.La.ag) <- c("Spectrum", "In L-alpha")
    
    Sn.La.cps <- subset(data$CPS, !(data$Energy < Sn$Line[7]-0.02 | data$Energy > Sn$Line[6]+0.02))
    Sn.file <- subset(data$Spectrum, !(data$Energy < Sn$Line[7]-0.02 | data$Energy > Sn$Line[6]+0.02))
    Sn.La.frame <- data.frame(is.0(Sn.La.cps, Sn.file))
    colnames(Sn.La.frame) <- c("Counts", "Spectrum")
    Sn.La.ag <- aggregate(list(Sn.La.frame$Counts), by=list(Sn.La.frame$Spectrum), FUN="sum")
    colnames(Sn.La.ag) <- c("Spectrum", "Sn L-alpha")
    
    Sb.La.cps <- subset(data$CPS, !(data$Energy < Sb$Line[7]-0.02 | data$Energy > Sb$Line[6]+0.02))
    Sb.file <- subset(data$Spectrum, !(data$Energy < Sb$Line[7]-0.02 | data$Energy > Sb$Line[6]+0.02))
    Sb.La.frame <- data.frame(is.0(Sb.La.cps, Sb.file))
    colnames(Sb.La.frame) <- c("Counts", "Spectrum")
    Sb.La.ag <- aggregate(list(Sb.La.frame$Counts), by=list(Sb.La.frame$Spectrum), FUN="sum")
    colnames(Sb.La.ag) <- c("Spectrum", "Sb L-alpha")
    
    Te.La.cps <- subset(data$CPS, !(data$Energy < Te$Line[7]-0.02 | data$Energy > Te$Line[6]+0.02))
    Te.file <- subset(data$Spectrum, !(data$Energy < Te$Line[7]-0.02 | data$Energy > Te$Line[6]+0.02))
    Te.La.frame <- data.frame(is.0(Te.La.cps, Te.file))
    colnames(Te.La.frame) <- c("Counts", "Spectrum")
    Te.La.ag <- aggregate(list(Te.La.frame$Counts), by=list(Te.La.frame$Spectrum), FUN="sum")
    colnames(Te.La.ag) <- c("Spectrum", "Te L-alpha")
    
    I.La.cps <- subset(data$CPS, !(data$Energy < I$Line[7]-0.02 | data$Energy > I$Line[6]+0.02))
    I.file <- subset(data$Spectrum, !(data$Energy < I$Line[7]-0.02 | data$Energy > I$Line[6]+0.02))
    I.La.frame <- data.frame(is.0(I.La.cps, I.file))
    colnames(I.La.frame) <- c("Counts", "Spectrum")
    I.La.ag <- aggregate(list(I.La.frame$Counts), by=list(I.La.frame$Spectrum), FUN="sum")
    colnames(I.La.ag) <- c("Spectrum", "I L-alpha")
    
    Xe.La.cps <- subset(data$CPS, !(data$Energy < Xe$Line[7]-0.02 | data$Energy > Xe$Line[6]+0.02))
    Xe.file <- subset(data$Spectrum, !(data$Energy < Xe$Line[7]-0.02 | data$Energy > Xe$Line[6]+0.02))
    Xe.La.frame <- data.frame(is.0(Xe.La.cps, Xe.file))
    colnames(Xe.La.frame) <- c("Counts", "Spectrum")
    Xe.La.ag <- aggregate(list(Xe.La.frame$Counts), by=list(Xe.La.frame$Spectrum), FUN="sum")
    colnames(Xe.La.ag) <- c("Spectrum", "Xe L-alpha")
    
    Cs.La.cps <- subset(data$CPS, !(data$Energy < Cs$Line[7]-0.02 | data$Energy > Cs$Line[6]+0.02))
    Cs.file <- subset(data$Spectrum, !(data$Energy < Cs$Line[7]-0.02 | data$Energy > Cs$Line[6]+0.02))
    Cs.La.frame <- data.frame(is.0(Cs.La.cps, Cs.file))
    colnames(Cs.La.frame) <- c("Counts", "Spectrum")
    Cs.La.ag <- aggregate(list(Cs.La.frame$Counts), by=list(Cs.La.frame$Spectrum), FUN="sum")
    colnames(Cs.La.ag) <- c("Spectrum", "Cs L-alpha")
    
    Ba.La.cps <- subset(data$CPS, !(data$Energy < Ba$Line[7]-0.02 | data$Energy > Ba$Line[6]+0.02))
    Ba.file <- subset(data$Spectrum, !(data$Energy < Ba$Line[7]-0.02 | data$Energy > Ba$Line[6]+0.02))
    Ba.La.frame <- data.frame(is.0(Ba.La.cps, Ba.file))
    colnames(Ba.La.frame) <- c("Counts", "Spectrum")
    Ba.La.ag <- aggregate(list(Ba.La.frame$Counts), by=list(Ba.La.frame$Spectrum), FUN="sum")
    colnames(Ba.La.ag) <- c("Spectrum", "Ba L-alpha")
    
    La.La.cps <- subset(data$CPS, !(data$Energy < La$Line[7]-0.02 | data$Energy > La$Line[6]+0.02))
    La.file <- subset(data$Spectrum, !(data$Energy < La$Line[7]-0.02 | data$Energy > La$Line[6]+0.02))
    La.La.frame <- data.frame(is.0(La.La.cps, La.file))
    colnames(La.La.frame) <- c("Counts", "Spectrum")
    La.La.ag <- aggregate(list(La.La.frame$Counts), by=list(La.La.frame$Spectrum), FUN="sum")
    colnames(La.La.ag) <- c("Spectrum", "La L-alpha")
    
    Ce.La.cps <- subset(data$CPS, !(data$Energy < Ce$Line[7]-0.02 | data$Energy > Ce$Line[6]+0.02))
    Ce.file <- subset(data$Spectrum, !(data$Energy < Ce$Line[7]-0.02 | data$Energy > Ce$Line[6]+0.02))
    Ce.La.frame <- data.frame(is.0(Ce.La.cps, Ce.file))
    colnames(Ce.La.frame) <- c("Counts", "Spectrum")
    Ce.La.ag <- aggregate(list(Ce.La.frame$Counts), by=list(Ce.La.frame$Spectrum), FUN="sum")
    colnames(Ce.La.ag) <- c("Spectrum", "Ce L-alpha")
    
    Pr.La.cps <- subset(data$CPS, !(data$Energy < Pr$Line[7]-0.02 | data$Energy > Pr$Line[6]+0.02))
    Pr.file <- subset(data$Spectrum, !(data$Energy < Pr$Line[7]-0.02 | data$Energy > Pr$Line[6]+0.02))
    Pr.La.frame <- data.frame(is.0(Pr.La.cps, Pr.file))
    colnames(Pr.La.frame) <- c("Counts", "Spectrum")
    Pr.La.ag <- aggregate(list(Pr.La.frame$Counts), by=list(Pr.La.frame$Spectrum), FUN="sum")
    colnames(Pr.La.ag) <- c("Spectrum", "Pr L-alpha")
    
    Nd.La.cps <- subset(data$CPS, !(data$Energy < Nd$Line[7]-0.02 | data$Energy > Nd$Line[6]+0.02))
    Nd.file <- subset(data$Spectrum, !(data$Energy < Nd$Line[7]-0.02 | data$Energy > Nd$Line[6]+0.02))
    Nd.La.frame <- data.frame(is.0(Nd.La.cps, Nd.file))
    colnames(Nd.La.frame) <- c("Counts", "Spectrum")
    Nd.La.ag <- aggregate(list(Nd.La.frame$Counts), by=list(Nd.La.frame$Spectrum), FUN="sum")
    colnames(Nd.La.ag) <- c("Spectrum", "Nd L-alpha")
    
    Pm.La.cps <- subset(data$CPS, !(data$Energy < Pm$Line[7]-0.02 | data$Energy > Pm$Line[6]+0.02))
    Pm.file <- subset(data$Spectrum, !(data$Energy < Pm$Line[7]-0.02 | data$Energy > Pm$Line[6]+0.02))
    Pm.La.frame <- data.frame(is.0(Pm.La.cps, Pm.file))
    colnames(Pm.La.frame) <- c("Counts", "Spectrum")
    Pm.La.ag <- aggregate(list(Pm.La.frame$Counts), by=list(Pm.La.frame$Spectrum), FUN="sum")
    colnames(Pm.La.ag) <- c("Spectrum", "Pm L-alpha")
    
    Sm.La.cps <- subset(data$CPS, !(data$Energy < Sm$Line[7]-0.02 | data$Energy > Sm$Line[6]+0.02))
    Sm.file <- subset(data$Spectrum, !(data$Energy < Sm$Line[7]-0.02 | data$Energy > Sm$Line[6]+0.02))
    Sm.La.frame <- data.frame(is.0(Sm.La.cps, Sm.file))
    colnames(Sm.La.frame) <- c("Counts", "Spectrum")
    Sm.La.ag <- aggregate(list(Sm.La.frame$Counts), by=list(Sm.La.frame$Spectrum), FUN="sum")
    colnames(Sm.La.ag) <- c("Spectrum", "Sm L-alpha")
    
    Eu.La.cps <- subset(data$CPS, !(data$Energy < Eu$Line[7]-0.02 | data$Energy > Eu$Line[6]+0.02))
    Eu.file <- subset(data$Spectrum, !(data$Energy < Eu$Line[7]-0.02 | data$Energy > Eu$Line[6]+0.02))
    Eu.La.frame <- data.frame(is.0(Eu.La.cps, Eu.file))
    colnames(Eu.La.frame) <- c("Counts", "Spectrum")
    Eu.La.ag <- aggregate(list(Eu.La.frame$Counts), by=list(Eu.La.frame$Spectrum), FUN="sum")
    colnames(Eu.La.ag) <- c("Spectrum", "Eu L-alpha")
    
    Gd.La.cps <- subset(data$CPS, !(data$Energy < Gd$Line[7]-0.02 | data$Energy > Gd$Line[6]+0.02))
    Gd.file <- subset(data$Spectrum, !(data$Energy < Gd$Line[7]-0.02 | data$Energy > Gd$Line[6]+0.02))
    Gd.La.frame <- data.frame(is.0(Gd.La.cps, Gd.file))
    colnames(Gd.La.frame) <- c("Counts", "Spectrum")
    Gd.La.ag <- aggregate(list(Gd.La.frame$Counts), by=list(Gd.La.frame$Spectrum), FUN="sum")
    colnames(Gd.La.ag) <- c("Spectrum", "Gd L-alpha")
    
    Tb.La.cps <- subset(data$CPS, !(data$Energy < Tb$Line[7]-0.02 | data$Energy > Tb$Line[6]+0.02))
    Tb.file <- subset(data$Spectrum, !(data$Energy < Tb$Line[7]-0.02 | data$Energy > Tb$Line[6]+0.02))
    Tb.La.frame <- data.frame(is.0(Tb.La.cps, Tb.file))
    colnames(Tb.La.frame) <- c("Counts", "Spectrum")
    Tb.La.ag <- aggregate(list(Tb.La.frame$Counts), by=list(Tb.La.frame$Spectrum), FUN="sum")
    colnames(Tb.La.ag) <- c("Spectrum", "Tb L-alpha")
    
    Dy.La.cps <- subset(data$CPS, !(data$Energy < Dy$Line[7]-0.02 | data$Energy > Dy$Line[6]+0.02))
    Dy.file <- subset(data$Spectrum, !(data$Energy < Dy$Line[7]-0.02 | data$Energy > Dy$Line[6]+0.02))
    Dy.La.frame <- data.frame(is.0(Dy.La.cps, Dy.file))
    colnames(Dy.La.frame) <- c("Counts", "Spectrum")
    Dy.La.ag <- aggregate(list(Dy.La.frame$Counts), by=list(Dy.La.frame$Spectrum), FUN="sum")
    colnames(Dy.La.ag) <- c("Spectrum", "Dy L-alpha")
    
    Ho.La.cps <- subset(data$CPS, !(data$Energy < Ho$Line[7]-0.02 | data$Energy > Ho$Line[6]+0.02))
    Ho.file <- subset(data$Spectrum, !(data$Energy < Ho$Line[7]-0.02 | data$Energy > Ho$Line[6]+0.02))
    Ho.La.frame <- data.frame(is.0(Ho.La.cps, Ho.file))
    colnames(Ho.La.frame) <- c("Counts", "Spectrum")
    Ho.La.ag <- aggregate(list(Ho.La.frame$Counts), by=list(Ho.La.frame$Spectrum), FUN="sum")
    colnames(Ho.La.ag) <- c("Spectrum", "Ho L-alpha")
    
    Er.La.cps <- subset(data$CPS, !(data$Energy < Er$Line[7]-0.02 | data$Energy > Er$Line[6]+0.02))
    Er.file <- subset(data$Spectrum, !(data$Energy < Er$Line[7]-0.02 | data$Energy > Er$Line[6]+0.02))
    Er.La.frame <- data.frame(is.0(Er.La.cps, Er.file))
    colnames(Er.La.frame) <- c("Counts", "Spectrum")
    Er.La.ag <- aggregate(list(Er.La.frame$Counts), by=list(Er.La.frame$Spectrum), FUN="sum")
    colnames(Er.La.ag) <- c("Spectrum", "Er L-alpha")
    
    Tm.La.cps <- subset(data$CPS, !(data$Energy < Tm$Line[7]-0.02 | data$Energy > Tm$Line[6]+0.02))
    Tm.file <- subset(data$Spectrum, !(data$Energy < Tm$Line[7]-0.02 | data$Energy > Tm$Line[6]+0.02))
    Tm.La.frame <- data.frame(is.0(Tm.La.cps, Tm.file))
    colnames(Tm.La.frame) <- c("Counts", "Spectrum")
    Tm.La.ag <- aggregate(list(Tm.La.frame$Counts), by=list(Tm.La.frame$Spectrum), FUN="sum")
    colnames(Tm.La.ag) <- c("Spectrum", "Tm L-alpha")
    
    Yb.La.cps <- subset(data$CPS, !(data$Energy < Yb$Line[7]-0.02 | data$Energy > Yb$Line[6]+0.02))
    Yb.file <- subset(data$Spectrum, !(data$Energy < Yb$Line[7]-0.02 | data$Energy > Yb$Line[6]+0.02))
    Yb.La.frame <- data.frame(is.0(Yb.La.cps, Yb.file))
    colnames(Yb.La.frame) <- c("Counts", "Spectrum")
    Yb.La.ag <- aggregate(list(Yb.La.frame$Counts), by=list(Yb.La.frame$Spectrum), FUN="sum")
    colnames(Yb.La.ag) <- c("Spectrum", "Yb L-alpha")
    
    Lu.La.cps <- subset(data$CPS, !(data$Energy < Lu$Line[7]-0.02 | data$Energy > Lu$Line[6]+0.02))
    Lu.file <- subset(data$Spectrum, !(data$Energy < Lu$Line[7]-0.02 | data$Energy > Lu$Line[6]+0.02))
    Lu.La.frame <- data.frame(is.0(Lu.La.cps, Lu.file))
    colnames(Lu.La.frame) <- c("Counts", "Spectrum")
    Lu.La.ag <- aggregate(list(Lu.La.frame$Counts), by=list(Lu.La.frame$Spectrum), FUN="sum")
    colnames(Lu.La.ag) <- c("Spectrum", "Lu L-alpha")
    
    Hf.La.cps <- subset(data$CPS, !(data$Energy < Hf$Line[7]-0.02 | data$Energy > Hf$Line[6]+0.02))
    Hf.file <- subset(data$Spectrum, !(data$Energy < Hf$Line[7]-0.02 | data$Energy > Hf$Line[6]+0.02))
    Hf.La.frame <- data.frame(is.0(Hf.La.cps, Hf.file))
    colnames(Hf.La.frame) <- c("Counts", "Spectrum")
    Hf.La.ag <- aggregate(list(Hf.La.frame$Counts), by=list(Hf.La.frame$Spectrum), FUN="sum")
    colnames(Hf.La.ag) <- c("Spectrum", "Hf L-alpha")
    
    Ta.La.cps <- subset(data$CPS, !(data$Energy < Ta$Line[7]-0.02 | data$Energy > Ta$Line[6]+0.02))
    Ta.file <- subset(data$Spectrum, !(data$Energy < Ta$Line[7]-0.02 | data$Energy > Ta$Line[6]+0.02))
    Ta.La.frame <- data.frame(is.0(Ta.La.cps, Ta.file))
    colnames(Ta.La.frame) <- c("Counts", "Spectrum")
    Ta.La.ag <- aggregate(list(Ta.La.frame$Counts), by=list(Ta.La.frame$Spectrum), FUN="sum")
    colnames(Ta.La.ag) <- c("Spectrum", "Ta L-alpha")
    
    W.La.cps <- subset(data$CPS, !(data$Energy < W$Line[7]-0.02 | data$Energy > W$Line[6]+0.02))
    W.file <- subset(data$Spectrum, !(data$Energy < W$Line[7]-0.02 | data$Energy > W$Line[6]+0.02))
    W.La.frame <- data.frame(is.0(W.La.cps, W.file))
    colnames(W.La.frame) <- c("Counts", "Spectrum")
    W.La.ag <- aggregate(list(W.La.frame$Counts), by=list(W.La.frame$Spectrum), FUN="sum")
    colnames(W.La.ag) <- c("Spectrum", "W L-alpha")
    
    Re.La.cps <- subset(data$CPS, !(data$Energy < Re$Line[7]-0.02 | data$Energy > Re$Line[6]+0.02))
    Re.file <- subset(data$Spectrum, !(data$Energy < Re$Line[7]-0.02 | data$Energy > Re$Line[6]+0.02))
    Re.La.frame <- data.frame(is.0(Re.La.cps, Re.file))
    colnames(Re.La.frame) <- c("Counts", "Spectrum")
    Re.La.ag <- aggregate(list(Re.La.frame$Counts), by=list(Re.La.frame$Spectrum), FUN="sum")
    colnames(Re.La.ag) <- c("Spectrum", "Re L-alpha")
    
    Os.La.cps <- subset(data$CPS, !(data$Energy < Os$Line[7]-0.02 | data$Energy > Os$Line[6]+0.02))
    Os.file <- subset(data$Spectrum, !(data$Energy < Os$Line[7]-0.02 | data$Energy > Os$Line[6]+0.02))
    Os.La.frame <- data.frame(is.0(Os.La.cps, Os.file))
    colnames(Os.La.frame) <- c("Counts", "Spectrum")
    Os.La.ag <- aggregate(list(Os.La.frame$Counts), by=list(Os.La.frame$Spectrum), FUN="sum")
    colnames(Os.La.ag) <- c("Spectrum", "Os L-alpha")
    
    Ir.La.cps <- subset(data$CPS, !(data$Energy < Ir$Line[7]-0.02 | data$Energy > Ir$Line[6]+0.02))
    Ir.file <- subset(data$Spectrum, !(data$Energy < Ir$Line[7]-0.02 | data$Energy > Ir$Line[6]+0.02))
    Ir.La.frame <- data.frame(is.0(Ir.La.cps, Ir.file))
    colnames(Ir.La.frame) <- c("Counts", "Spectrum")
    Ir.La.ag <- aggregate(list(Ir.La.frame$Counts), by=list(Ir.La.frame$Spectrum), FUN="sum")
    colnames(Ir.La.ag) <- c("Spectrum", "Ir L-alpha")
    
    Pt.La.cps <- subset(data$CPS, !(data$Energy < Pt$Line[7]-0.02 | data$Energy > Pt$Line[6]+0.02))
    Pt.file <- subset(data$Spectrum, !(data$Energy < Pt$Line[7]-0.02 | data$Energy > Pt$Line[6]+0.02))
    Pt.La.frame <- data.frame(is.0(Pt.La.cps, Pt.file))
    colnames(Pt.La.frame) <- c("Counts", "Spectrum")
    Pt.La.ag <- aggregate(list(Pt.La.frame$Counts), by=list(Pt.La.frame$Spectrum), FUN="sum")
    colnames(Pt.La.ag) <- c("Spectrum", "Pt L-alpha")
    
    Au.La.cps <- subset(data$CPS, !(data$Energy < Au$Line[7]-0.02 | data$Energy > Au$Line[6]+0.02))
    Au.file <- subset(data$Spectrum, !(data$Energy < Au$Line[7]-0.02 | data$Energy > Au$Line[6]+0.02))
    Au.La.frame <- data.frame(is.0(Au.La.cps, Au.file))
    colnames(Au.La.frame) <- c("Counts", "Spectrum")
    Au.La.ag <- aggregate(list(Au.La.frame$Counts), by=list(Au.La.frame$Spectrum), FUN="sum")
    colnames(Au.La.ag) <- c("Spectrum", "Au L-alpha")
    
    Hg.La.cps <- subset(data$CPS, !(data$Energy < Hg$Line[7]-0.02 | data$Energy > Hg$Line[6]+0.02))
    Hg.file <- subset(data$Spectrum, !(data$Energy < Hg$Line[7]-0.02 | data$Energy > Hg$Line[6]+0.02))
    Hg.La.frame <- data.frame(is.0(Hg.La.cps, Hg.file))
    colnames(Hg.La.frame) <- c("Counts", "Spectrum")
    Hg.La.ag <- aggregate(list(Hg.La.frame$Counts), by=list(Hg.La.frame$Spectrum), FUN="sum")
    colnames(Hg.La.ag) <- c("Spectrum", "Hg L-alpha")
    
    Tl.La.cps <- subset(data$CPS, !(data$Energy < Tl$Line[7]-0.02 | data$Energy > Tl$Line[6]+0.02))
    Tl.file <- subset(data$Spectrum, !(data$Energy < Tl$Line[7]-0.02 | data$Energy > Tl$Line[6]+0.02))
    Tl.La.frame <- data.frame(is.0(Tl.La.cps, Tl.file))
    colnames(Tl.La.frame) <- c("Counts", "Spectrum")
    Tl.La.ag <- aggregate(list(Tl.La.frame$Counts), by=list(Tl.La.frame$Spectrum), FUN="sum")
    colnames(Tl.La.ag) <- c("Spectrum", "Tl L-alpha")
    
    Pb.La.cps <- subset(data$CPS, !(data$Energy < Pb$Line[7]-0.02 | data$Energy > Pb$Line[6]+0.02))
    Pb.file <- subset(data$Spectrum, !(data$Energy < Pb$Line[7]-0.02 | data$Energy > Pb$Line[6]+0.02))
    Pb.La.frame <- data.frame(is.0(Pb.La.cps, Pb.file))
    colnames(Pb.La.frame) <- c("Counts", "Spectrum")
    Pb.La.ag <- aggregate(list(Pb.La.frame$Counts), by=list(Pb.La.frame$Spectrum), FUN="sum")
    colnames(Pb.La.ag) <- c("Spectrum", "Pb L-alpha")
    
    Bi.La.cps <- subset(data$CPS, !(data$Energy < Bi$Line[7]-0.02 | data$Energy > Bi$Line[6]+0.02))
    Bi.file <- subset(data$Spectrum, !(data$Energy < Bi$Line[7]-0.02 | data$Energy > Bi$Line[6]+0.02))
    Bi.La.frame <- data.frame(is.0(Bi.La.cps, Bi.file))
    colnames(Bi.La.frame) <- c("Counts", "Spectrum")
    Bi.La.ag <- aggregate(list(Bi.La.frame$Counts), by=list(Bi.La.frame$Spectrum), FUN="sum")
    colnames(Bi.La.ag) <- c("Spectrum", "Bi L-alpha")
    
    Po.La.cps <- subset(data$CPS, !(data$Energy < Po$Line[7]-0.02 | data$Energy > Po$Line[6]+0.02))
    Po.file <- subset(data$Spectrum, !(data$Energy < Po$Line[7]-0.02 | data$Energy > Po$Line[6]+0.02))
    Po.La.frame <- data.frame(is.0(Po.La.cps, Po.file))
    colnames(Po.La.frame) <- c("Counts", "Spectrum")
    Po.La.ag <- aggregate(list(Po.La.frame$Counts), by=list(Po.La.frame$Spectrum), FUN="sum")
    colnames(Po.La.ag) <- c("Spectrum", "Po L-alpha")
    
    At.La.cps <- subset(data$CPS, !(data$Energy < At$Line[7]-0.02 | data$Energy > At$Line[6]+0.02))
    At.file <- subset(data$Spectrum, !(data$Energy < At$Line[7]-0.02 | data$Energy > At$Line[6]+0.02))
    At.La.frame <- data.frame(is.0(At.La.cps, At.file))
    colnames(At.La.frame) <- c("Counts", "Spectrum")
    At.La.ag <- aggregate(list(At.La.frame$Counts), by=list(At.La.frame$Spectrum), FUN="sum")
    colnames(At.La.ag) <- c("Spectrum", "At L-alpha")
    
    Rn.La.cps <- subset(data$CPS, !(data$Energy < Rn$Line[7]-0.02 | data$Energy > Rn$Line[6]+0.02))
    Rn.file <- subset(data$Spectrum, !(data$Energy < Rn$Line[7]-0.02 | data$Energy > Rn$Line[6]+0.02))
    Rn.La.frame <- data.frame(is.0(Rn.La.cps, Rn.file))
    colnames(Rn.La.frame) <- c("Counts", "Spectrum")
    Rn.La.ag <- aggregate(list(Rn.La.frame$Counts), by=list(Rn.La.frame$Spectrum), FUN="sum")
    colnames(Rn.La.ag) <- c("Spectrum", "Rn L-alpha")
    
    Fr.La.cps <- subset(data$CPS, !(data$Energy < Fr$Line[7]-0.02 | data$Energy > Fr$Line[6]+0.02))
    Fr.file <- subset(data$Spectrum, !(data$Energy < Fr$Line[7]-0.02 | data$Energy > Fr$Line[6]+0.02))
    Fr.La.frame <- data.frame(is.0(Fr.La.cps, Fr.file))
    colnames(Fr.La.frame) <- c("Counts", "Spectrum")
    Fr.La.ag <- aggregate(list(Fr.La.frame$Counts), by=list(Fr.La.frame$Spectrum), FUN="sum")
    colnames(Fr.La.ag) <- c("Spectrum", "Fr L-alpha")
    
    Ra.La.cps <- subset(data$CPS, !(data$Energy < Ra$Line[7]-0.02 | data$Energy > Ra$Line[6]+0.02))
    Ra.file <- subset(data$Spectrum, !(data$Energy < Ra$Line[7]-0.02 | data$Energy > Ra$Line[6]+0.02))
    Ra.La.frame <- data.frame(is.0(Ra.La.cps, Ra.file))
    colnames(Ra.La.frame) <- c("Counts", "Spectrum")
    Ra.La.ag <- aggregate(list(Ra.La.frame$Counts), by=list(Ra.La.frame$Spectrum), FUN="sum")
    colnames(Ra.La.ag) <- c("Spectrum", "Ra L-alpha")
    
    Ac.La.cps <- subset(data$CPS, !(data$Energy < Ac$Line[7]-0.02 | data$Energy > Ac$Line[6]+0.02))
    Ac.file <- subset(data$Spectrum, !(data$Energy < Ac$Line[7]-0.02 | data$Energy > Ac$Line[6]+0.02))
    Ac.La.frame <- data.frame(is.0(Ac.La.cps, Ac.file))
    colnames(Ac.La.frame) <- c("Counts", "Spectrum")
    Ac.La.ag <- aggregate(list(Ac.La.frame$Counts), by=list(Ac.La.frame$Spectrum), FUN="sum")
    colnames(Ac.La.ag) <- c("Spectrum", "Ac L-alpha")
    
    Th.La.cps <- subset(data$CPS, !(data$Energy < Th$Line[7]-0.02 | data$Energy > Th$Line[6]+0.02))
    Th.file <- subset(data$Spectrum, !(data$Energy < Th$Line[7]-0.02 | data$Energy > Th$Line[6]+0.02))
    Th.La.frame <- data.frame(is.0(Th.La.cps, Th.file))
    colnames(Th.La.frame) <- c("Counts", "Spectrum")
    Th.La.ag <- aggregate(list(Th.La.frame$Counts), by=list(Th.La.frame$Spectrum), FUN="sum")
    colnames(Th.La.ag) <- c("Spectrum", "Th L-alpha")
    
    Pa.La.cps <- subset(data$CPS, !(data$Energy < Pa$Line[7]-0.02 | data$Energy > Pa$Line[6]+0.02))
    Pa.file <- subset(data$Spectrum, !(data$Energy < Pa$Line[7]-0.02 | data$Energy > Pa$Line[6]+0.02))
    Pa.La.frame <- data.frame(is.0(Pa.La.cps, Pa.file))
    colnames(Pa.La.frame) <- c("Counts", "Spectrum")
    Pa.La.ag <- aggregate(list(Pa.La.frame$Counts), by=list(Pa.La.frame$Spectrum), FUN="sum")
    colnames(Pa.La.ag) <- c("Spectrum", "Pa L-alpha")
    
    U.La.cps <- subset(data$CPS, !(data$Energy < U$Line[7]-0.02 | data$Energy > U$Line[6]+0.02))
    U.file <- subset(data$Spectrum, !(data$Energy < U$Line[7]-0.02 | data$Energy > U$Line[6]+0.02))
    U.La.frame <- data.frame(is.0(U.La.cps, U.file))
    colnames(U.La.frame) <- c("Counts", "Spectrum")
    U.La.ag <- aggregate(list(U.La.frame$Counts), by=list(U.La.frame$Spectrum), FUN="sum")
    colnames(U.La.ag) <- c("Spectrum", "U L-alpha")
    
    Pu.La.cps <- subset(data$CPS, !(data$Energy < Pu$Line[7]-0.02 | data$Energy > Pu$Line[6]+0.02))
    Pu.file <- subset(data$Spectrum, !(data$Energy < Pu$Line[7]-0.02 | data$Energy > Pu$Line[6]+0.02))
    Pu.La.frame <- data.frame(is.0(Pu.La.cps, Pu.file))
    colnames(Pu.La.frame) <- c("Counts", "Spectrum")
    Pu.La.ag <- aggregate(list(Pu.La.frame$Counts), by=list(Pu.La.frame$Spectrum), FUN="sum")
    colnames(Pu.La.ag) <- c("Spectrum", "Pu L-alpha")
    
    Mo.Lb.cps <- subset(data$CPS, !(data$Energy < Mo$Line[8]-0.02 | data$Energy > Mo$Line[10]+0.02))
    Mo.file <- subset(data$Spectrum, !(data$Energy < Mo$Line[8]-0.02 | data$Energy > Mo$Line[10]+0.02))
    Mo.Lb.frame <- is.0(Mo.Lb.cps,Mo.file)
    colnames(Mo.Lb.frame) <- c("Counts", "Spectrum")
    Mo.Lb.ag <- aggregate(list(Mo.Lb.frame$Counts), by=list(Mo.Lb.frame$Spectrum), FUN="sum")
    colnames(Mo.Lb.ag) <- c("Spectrum", "Mo L-beta")
    
    Tc.Lb.cps <- subset(data$CPS, !(data$Energy < Tc$Line[8]-0.02 | data$Energy > Tc$Line[10]+0.02))
    Tc.file <- subset(data$Spectrum, !(data$Energy < Tc$Line[8]-0.02 | data$Energy > Tc$Line[10]+0.02))
    Tc.Lb.frame <- is.0(Tc.Lb.cps,Tc.file)
    colnames(Tc.Lb.frame) <- c("Counts", "Spectrum")
    Tc.Lb.ag <- aggregate(list(Tc.Lb.frame$Counts), by=list(Tc.Lb.frame$Spectrum), FUN="sum")
    colnames(Tc.Lb.ag) <- c("Spectrum", "Tc L-beta")
    
    Ru.Lb.cps <- subset(data$CPS, !(data$Energy < Ru$Line[8]-0.02 | data$Energy > Ru$Line[10]+0.02))
    Ru.file <- subset(data$Spectrum, !(data$Energy < Ru$Line[8]-0.02 | data$Energy > Ru$Line[10]+0.02))
    Ru.Lb.frame <- is.0(Ru.Lb.cps,Ru.file)
    colnames(Ru.Lb.frame) <- c("Counts", "Spectrum")
    Ru.Lb.ag <- aggregate(list(Ru.Lb.frame$Counts), by=list(Ru.Lb.frame$Spectrum), FUN="sum")
    colnames(Ru.Lb.ag) <- c("Spectrum", "Ru L-beta")
    
    Rh.Lb.cps <- subset(data$CPS, !(data$Energy < Rh$Line[8]-0.02 | data$Energy > Rh$Line[10]+0.02))
    Rh.file <- subset(data$Spectrum, !(data$Energy < Rh$Line[8]-0.02 | data$Energy > Rh$Line[10]+0.02))
    Rh.Lb.frame <- is.0(Rh.Lb.cps,Rh.file)
    colnames(Rh.Lb.frame) <- c("Counts", "Spectrum")
    Rh.Lb.ag <- aggregate(list(Rh.Lb.frame$Counts), by=list(Rh.Lb.frame$Spectrum), FUN="sum")
    colnames(Rh.Lb.ag) <- c("Spectrum", "Rh L-beta")
    
    Rh.Lb.cps <- subset(data$CPS, !(data$Energy < Rh$Line[8]-0.02 | data$Energy > Rh$Line[10]+0.02))
    Rh.file <- subset(data$Spectrum, !(data$Energy < Rh$Line[8]-0.02 | data$Energy > Rh$Line[10]+0.02))
    Rh.Lb.frame <- data.frame(is.0(Rh.Lb.cps, Rh.file))
    colnames(Rh.Lb.frame) <- c("Counts", "Spectrum")
    Rh.Lb.ag <- aggregate(list(Rh.Lb.frame$Counts), by=list(Rh.Lb.frame$Spectrum), FUN="sum")
    colnames(Rh.Lb.ag) <- c("Spectrum", "Rh L-beta")
    
    Pd.Lb.cps <- subset(data$CPS, !(data$Energy < Pd$Line[8]-0.02 | data$Energy > Pd$Line[10]+0.02))
    Pd.file <- subset(data$Spectrum, !(data$Energy < Pd$Line[8]-0.02 | data$Energy > Pd$Line[10]+0.02))
    Pd.Lb.frame <- data.frame(is.0(Pd.Lb.cps, Pd.file))
    colnames(Pd.Lb.frame) <- c("Counts", "Spectrum")
    Pd.Lb.ag <- aggregate(list(Pd.Lb.frame$Counts), by=list(Pd.Lb.frame$Spectrum), FUN="sum")
    colnames(Pd.Lb.ag) <- c("Spectrum", "Pd L-beta")
    
    Ag.Lb.cps <- subset(data$CPS, !(data$Energy < Ag$Line[8]-0.02 | data$Energy > Ag$Line[10]+0.02))
    Ag.file <- subset(data$Spectrum, !(data$Energy < Ag$Line[8]-0.02 | data$Energy > Ag$Line[10]+0.02))
    Ag.Lb.frame <- data.frame(is.0(Ag.Lb.cps, Ag.file))
    colnames(Ag.Lb.frame) <- c("Counts", "Spectrum")
    Ag.Lb.ag <- aggregate(list(Ag.Lb.frame$Counts), by=list(Ag.Lb.frame$Spectrum), FUN="sum")
    colnames(Ag.Lb.ag) <- c("Spectrum", "Ag L-beta")
    
    Cd.Lb.cps <- subset(data$CPS, !(data$Energy < Cd$Line[8]-0.02 | data$Energy > Cd$Line[10]+0.02))
    Cd.file <- subset(data$Spectrum, !(data$Energy < Cd$Line[8]-0.02 | data$Energy > Cd$Line[10]+0.02))
    Cd.Lb.frame <- data.frame(is.0(Cd.Lb.cps, Cd.file))
    colnames(Cd.Lb.frame) <- c("Counts", "Spectrum")
    Cd.Lb.ag <- aggregate(list(Cd.Lb.frame$Counts), by=list(Cd.Lb.frame$Spectrum), FUN="sum")
    colnames(Cd.Lb.ag) <- c("Spectrum", "Cd L-beta")
    
    In.Lb.cps <- subset(data$CPS, !(data$Energy < In$Line[8]-0.02 | data$Energy > In$Line[10]+0.02))
    In.file <- subset(data$Spectrum, !(data$Energy < In$Line[8]-0.02 | data$Energy > In$Line[10]+0.02))
    In.Lb.frame <- data.frame(is.0(In.Lb.cps, In.file))
    colnames(In.Lb.frame) <- c("Counts", "Spectrum")
    In.Lb.ag <- aggregate(list(In.Lb.frame$Counts), by=list(In.Lb.frame$Spectrum), FUN="sum")
    colnames(In.Lb.ag) <- c("Spectrum", "In L-beta")
    
    Sn.Lb.cps <- subset(data$CPS, !(data$Energy < Sn$Line[8]-0.02 | data$Energy > Sn$Line[10]+0.02))
    Sn.file <- subset(data$Spectrum, !(data$Energy < Sn$Line[8]-0.02 | data$Energy > Sn$Line[10]+0.02))
    Sn.Lb.frame <- data.frame(is.0(Sn.Lb.cps, Sn.file))
    colnames(Sn.Lb.frame) <- c("Counts", "Spectrum")
    Sn.Lb.ag <- aggregate(list(Sn.Lb.frame$Counts), by=list(Sn.Lb.frame$Spectrum), FUN="sum")
    colnames(Sn.Lb.ag) <- c("Spectrum", "Sn L-beta")
    
    Sb.Lb.cps <- subset(data$CPS, !(data$Energy < Sb$Line[8]-0.02 | data$Energy > Sb$Line[10]+0.02))
    Sb.file <- subset(data$Spectrum, !(data$Energy < Sb$Line[8]-0.02 | data$Energy > Sb$Line[10]+0.02))
    Sb.Lb.frame <- data.frame(is.0(Sb.Lb.cps, Sb.file))
    colnames(Sb.Lb.frame) <- c("Counts", "Spectrum")
    Sb.Lb.ag <- aggregate(list(Sb.Lb.frame$Counts), by=list(Sb.Lb.frame$Spectrum), FUN="sum")
    colnames(Sb.Lb.ag) <- c("Spectrum", "Sb L-beta")
    
    Te.Lb.cps <- subset(data$CPS, !(data$Energy < Te$Line[8]-0.02 | data$Energy > Te$Line[10]+0.02))
    Te.file <- subset(data$Spectrum, !(data$Energy < Te$Line[8]-0.02 | data$Energy > Te$Line[10]+0.02))
    Te.Lb.frame <- data.frame(is.0(Te.Lb.cps, Te.file))
    colnames(Te.Lb.frame) <- c("Counts", "Spectrum")
    Te.Lb.ag <- aggregate(list(Te.Lb.frame$Counts), by=list(Te.Lb.frame$Spectrum), FUN="sum")
    colnames(Te.Lb.ag) <- c("Spectrum", "Te L-beta")
    
    I.Lb.cps <- subset(data$CPS, !(data$Energy < I$Line[8]-0.02 | data$Energy > I$Line[10]+0.02))
    I.file <- subset(data$Spectrum, !(data$Energy < I$Line[8]-0.02 | data$Energy > I$Line[10]+0.02))
    I.Lb.frame <- data.frame(is.0(I.Lb.cps, I.file))
    colnames(I.Lb.frame) <- c("Counts", "Spectrum")
    I.Lb.ag <- aggregate(list(I.Lb.frame$Counts), by=list(I.Lb.frame$Spectrum), FUN="sum")
    colnames(I.Lb.ag) <- c("Spectrum", "I L-beta")
    
    Xe.Lb.cps <- subset(data$CPS, !(data$Energy < Xe$Line[8]-0.02 | data$Energy > Xe$Line[10]+0.02))
    Xe.file <- subset(data$Spectrum, !(data$Energy < Xe$Line[8]-0.02 | data$Energy > Xe$Line[10]+0.02))
    Xe.Lb.frame <- data.frame(is.0(Xe.Lb.cps, Xe.file))
    colnames(Xe.Lb.frame) <- c("Counts", "Spectrum")
    Xe.Lb.ag <- aggregate(list(Xe.Lb.frame$Counts), by=list(Xe.Lb.frame$Spectrum), FUN="sum")
    colnames(Xe.Lb.ag) <- c("Spectrum", "Xe L-beta")
    
    Cs.Lb.cps <- subset(data$CPS, !(data$Energy < Cs$Line[8]-0.02 | data$Energy > Cs$Line[10]+0.02))
    Cs.file <- subset(data$Spectrum, !(data$Energy < Cs$Line[8]-0.02 | data$Energy > Cs$Line[10]+0.02))
    Cs.Lb.frame <- data.frame(is.0(Cs.Lb.cps, Cs.file))
    colnames(Cs.Lb.frame) <- c("Counts", "Spectrum")
    Cs.Lb.ag <- aggregate(list(Cs.Lb.frame$Counts), by=list(Cs.Lb.frame$Spectrum), FUN="sum")
    colnames(Cs.Lb.ag) <- c("Spectrum", "Cs L-beta")
    
    Ba.Lb.cps <- subset(data$CPS, !(data$Energy < Ba$Line[8]-0.02 | data$Energy > Ba$Line[10]+0.02))
    Ba.file <- subset(data$Spectrum, !(data$Energy < Ba$Line[8]-0.02 | data$Energy > Ba$Line[10]+0.02))
    Ba.Lb.frame <- data.frame(is.0(Ba.Lb.cps, Ba.file))
    colnames(Ba.Lb.frame) <- c("Counts", "Spectrum")
    Ba.Lb.ag <- aggregate(list(Ba.Lb.frame$Counts), by=list(Ba.Lb.frame$Spectrum), FUN="sum")
    colnames(Ba.Lb.ag) <- c("Spectrum", "Ba L-beta")
    
    La.Lb.cps <- subset(data$CPS, !(data$Energy < La$Line[8]-0.02 | data$Energy > La$Line[10]+0.02))
    La.file <- subset(data$Spectrum, !(data$Energy < La$Line[8]-0.02 | data$Energy > La$Line[10]+0.02))
    La.Lb.frame <- data.frame(is.0(La.Lb.cps, La.file))
    colnames(La.Lb.frame) <- c("Counts", "Spectrum")
    La.Lb.ag <- aggregate(list(La.Lb.frame$Counts), by=list(La.Lb.frame$Spectrum), FUN="sum")
    colnames(La.Lb.ag) <- c("Spectrum", "La L-beta")
    
    Ce.Lb.cps <- subset(data$CPS, !(data$Energy < Ce$Line[8]-0.02 | data$Energy > Ce$Line[10]+0.02))
    Ce.file <- subset(data$Spectrum, !(data$Energy < Ce$Line[8]-0.02 | data$Energy > Ce$Line[10]+0.02))
    Ce.Lb.frame <- data.frame(is.0(Ce.Lb.cps, Ce.file))
    colnames(Ce.Lb.frame) <- c("Counts", "Spectrum")
    Ce.Lb.ag <- aggregate(list(Ce.Lb.frame$Counts), by=list(Ce.Lb.frame$Spectrum), FUN="sum")
    colnames(Ce.Lb.ag) <- c("Spectrum", "Ce L-beta")
    
    Pr.Lb.cps <- subset(data$CPS, !(data$Energy < Pr$Line[8]-0.02 | data$Energy > Pr$Line[10]+0.02))
    Pr.file <- subset(data$Spectrum, !(data$Energy < Pr$Line[8]-0.02 | data$Energy > Pr$Line[10]+0.02))
    Pr.Lb.frame <- data.frame(is.0(Pr.Lb.cps, Pr.file))
    colnames(Pr.Lb.frame) <- c("Counts", "Spectrum")
    Pr.Lb.ag <- aggregate(list(Pr.Lb.frame$Counts), by=list(Pr.Lb.frame$Spectrum), FUN="sum")
    colnames(Pr.Lb.ag) <- c("Spectrum", "Pr L-beta")
    
    Nd.Lb.cps <- subset(data$CPS, !(data$Energy < Nd$Line[8]-0.02 | data$Energy > Nd$Line[10]+0.02))
    Nd.file <- subset(data$Spectrum, !(data$Energy < Nd$Line[8]-0.02 | data$Energy > Nd$Line[10]+0.02))
    Nd.Lb.frame <- data.frame(is.0(Nd.Lb.cps, Nd.file))
    colnames(Nd.Lb.frame) <- c("Counts", "Spectrum")
    Nd.Lb.ag <- aggregate(list(Nd.Lb.frame$Counts), by=list(Nd.Lb.frame$Spectrum), FUN="sum")
    colnames(Nd.Lb.ag) <- c("Spectrum", "Nd L-beta")
    
    Pm.Lb.cps <- subset(data$CPS, !(data$Energy < Pm$Line[8]-0.02 | data$Energy > Pm$Line[10]+0.02))
    Pm.file <- subset(data$Spectrum, !(data$Energy < Pm$Line[8]-0.02 | data$Energy > Pm$Line[10]+0.02))
    Pm.Lb.frame <- data.frame(is.0(Pm.Lb.cps, Pm.file))
    colnames(Pm.Lb.frame) <- c("Counts", "Spectrum")
    Pm.Lb.ag <- aggregate(list(Pm.Lb.frame$Counts), by=list(Pm.Lb.frame$Spectrum), FUN="sum")
    colnames(Pm.Lb.ag) <- c("Spectrum", "Pm L-beta")
    
    Sm.Lb.cps <- subset(data$CPS, !(data$Energy < Sm$Line[8]-0.02 | data$Energy > Sm$Line[10]+0.02))
    Sm.file <- subset(data$Spectrum, !(data$Energy < Sm$Line[8]-0.02 | data$Energy > Sm$Line[10]+0.02))
    Sm.Lb.frame <- data.frame(is.0(Sm.Lb.cps, Sm.file))
    colnames(Sm.Lb.frame) <- c("Counts", "Spectrum")
    Sm.Lb.ag <- aggregate(list(Sm.Lb.frame$Counts), by=list(Sm.Lb.frame$Spectrum), FUN="sum")
    colnames(Sm.Lb.ag) <- c("Spectrum", "Sm L-beta")
    
    Eu.Lb.cps <- subset(data$CPS, !(data$Energy < Eu$Line[8]-0.02 | data$Energy > Eu$Line[10]+0.02))
    Eu.file <- subset(data$Spectrum, !(data$Energy < Eu$Line[8]-0.02 | data$Energy > Eu$Line[10]+0.02))
    Eu.Lb.frame <- data.frame(is.0(Eu.Lb.cps, Eu.file))
    colnames(Eu.Lb.frame) <- c("Counts", "Spectrum")
    Eu.Lb.ag <- aggregate(list(Eu.Lb.frame$Counts), by=list(Eu.Lb.frame$Spectrum), FUN="sum")
    colnames(Eu.Lb.ag) <- c("Spectrum", "Eu L-beta")
    
    Gd.Lb.cps <- subset(data$CPS, !(data$Energy < Gd$Line[8]-0.02 | data$Energy > Gd$Line[10]+0.02))
    Gd.file <- subset(data$Spectrum, !(data$Energy < Gd$Line[8]-0.02 | data$Energy > Gd$Line[10]+0.02))
    Gd.Lb.frame <- data.frame(is.0(Gd.Lb.cps, Gd.file))
    colnames(Gd.Lb.frame) <- c("Counts", "Spectrum")
    Gd.Lb.ag <- aggregate(list(Gd.Lb.frame$Counts), by=list(Gd.Lb.frame$Spectrum), FUN="sum")
    colnames(Gd.Lb.ag) <- c("Spectrum", "Gd L-beta")
    
    Tb.Lb.cps <- subset(data$CPS, !(data$Energy < Tb$Line[8]-0.02 | data$Energy > Tb$Line[10]+0.02))
    Tb.file <- subset(data$Spectrum, !(data$Energy < Tb$Line[8]-0.02 | data$Energy > Tb$Line[10]+0.02))
    Tb.Lb.frame <- data.frame(is.0(Tb.Lb.cps, Tb.file))
    colnames(Tb.Lb.frame) <- c("Counts", "Spectrum")
    Tb.Lb.ag <- aggregate(list(Tb.Lb.frame$Counts), by=list(Tb.Lb.frame$Spectrum), FUN="sum")
    colnames(Tb.Lb.ag) <- c("Spectrum", "Tb L-beta")
    
    Dy.Lb.cps <- subset(data$CPS, !(data$Energy < Dy$Line[8]-0.02 | data$Energy > Dy$Line[10]+0.02))
    Dy.file <- subset(data$Spectrum, !(data$Energy < Dy$Line[8]-0.02 | data$Energy > Dy$Line[10]+0.02))
    Dy.Lb.frame <- data.frame(is.0(Dy.Lb.cps, Dy.file))
    colnames(Dy.Lb.frame) <- c("Counts", "Spectrum")
    Dy.Lb.ag <- aggregate(list(Dy.Lb.frame$Counts), by=list(Dy.Lb.frame$Spectrum), FUN="sum")
    colnames(Dy.Lb.ag) <- c("Spectrum", "Dy L-beta")
    
    Ho.Lb.cps <- subset(data$CPS, !(data$Energy < Ho$Line[8]-0.02 | data$Energy > Ho$Line[10]+0.02))
    Ho.file <- subset(data$Spectrum, !(data$Energy < Ho$Line[8]-0.02 | data$Energy > Ho$Line[10]+0.02))
    Ho.Lb.frame <- data.frame(is.0(Ho.Lb.cps, Ho.file))
    colnames(Ho.Lb.frame) <- c("Counts", "Spectrum")
    Ho.Lb.ag <- aggregate(list(Ho.Lb.frame$Counts), by=list(Ho.Lb.frame$Spectrum), FUN="sum")
    colnames(Ho.Lb.ag) <- c("Spectrum", "Ho L-beta")
    
    Er.Lb.cps <- subset(data$CPS, !(data$Energy < Er$Line[8]-0.02 | data$Energy > Er$Line[10]+0.02))
    Er.file <- subset(data$Spectrum, !(data$Energy < Er$Line[8]-0.02 | data$Energy > Er$Line[10]+0.02))
    Er.Lb.frame <- data.frame(is.0(Er.Lb.cps, Er.file))
    colnames(Er.Lb.frame) <- c("Counts", "Spectrum")
    Er.Lb.ag <- aggregate(list(Er.Lb.frame$Counts), by=list(Er.Lb.frame$Spectrum), FUN="sum")
    colnames(Er.Lb.ag) <- c("Spectrum", "Er L-beta")
    
    Tm.Lb.cps <- subset(data$CPS, !(data$Energy < Tm$Line[8]-0.02 | data$Energy > Tm$Line[10]+0.02))
    Tm.file <- subset(data$Spectrum, !(data$Energy < Tm$Line[8]-0.02 | data$Energy > Tm$Line[10]+0.02))
    Tm.Lb.frame <- data.frame(is.0(Tm.Lb.cps, Tm.file))
    colnames(Tm.Lb.frame) <- c("Counts", "Spectrum")
    Tm.Lb.ag <- aggregate(list(Tm.Lb.frame$Counts), by=list(Tm.Lb.frame$Spectrum), FUN="sum")
    colnames(Tm.Lb.ag) <- c("Spectrum", "Tm L-beta")
    
    Yb.Lb.cps <- subset(data$CPS, !(data$Energy < Yb$Line[8]-0.02 | data$Energy > Yb$Line[10]+0.02))
    Yb.file <- subset(data$Spectrum, !(data$Energy < Yb$Line[8]-0.02 | data$Energy > Yb$Line[10]+0.02))
    Yb.Lb.frame <- data.frame(is.0(Yb.Lb.cps, Yb.file))
    colnames(Yb.Lb.frame) <- c("Counts", "Spectrum")
    Yb.Lb.ag <- aggregate(list(Yb.Lb.frame$Counts), by=list(Yb.Lb.frame$Spectrum), FUN="sum")
    colnames(Yb.Lb.ag) <- c("Spectrum", "Yb L-beta")
    
    Lu.Lb.cps <- subset(data$CPS, !(data$Energy < Lu$Line[8]-0.02 | data$Energy > Lu$Line[10]+0.02))
    Lu.file <- subset(data$Spectrum, !(data$Energy < Lu$Line[8]-0.02 | data$Energy > Lu$Line[10]+0.02))
    Lu.Lb.frame <- data.frame(is.0(Lu.Lb.cps, Lu.file))
    colnames(Lu.Lb.frame) <- c("Counts", "Spectrum")
    Lu.Lb.ag <- aggregate(list(Lu.Lb.frame$Counts), by=list(Lu.Lb.frame$Spectrum), FUN="sum")
    colnames(Lu.Lb.ag) <- c("Spectrum", "Lu L-beta")
    
    Hf.Lb.cps <- subset(data$CPS, !(data$Energy < Hf$Line[8]-0.02 | data$Energy > Hf$Line[10]+0.02))
    Hf.file <- subset(data$Spectrum, !(data$Energy < Hf$Line[8]-0.02 | data$Energy > Hf$Line[10]+0.02))
    Hf.Lb.frame <- data.frame(is.0(Hf.Lb.cps, Hf.file))
    colnames(Hf.Lb.frame) <- c("Counts", "Spectrum")
    Hf.Lb.ag <- aggregate(list(Hf.Lb.frame$Counts), by=list(Hf.Lb.frame$Spectrum), FUN="sum")
    colnames(Hf.Lb.ag) <- c("Spectrum", "Hf L-beta")
    
    Ta.Lb.cps <- subset(data$CPS, !(data$Energy < Ta$Line[8]-0.02 | data$Energy > Ta$Line[10]+0.02))
    Ta.file <- subset(data$Spectrum, !(data$Energy < Ta$Line[8]-0.02 | data$Energy > Ta$Line[10]+0.02))
    Ta.Lb.frame <- data.frame(is.0(Ta.Lb.cps, Ta.file))
    colnames(Ta.Lb.frame) <- c("Counts", "Spectrum")
    Ta.Lb.ag <- aggregate(list(Ta.Lb.frame$Counts), by=list(Ta.Lb.frame$Spectrum), FUN="sum")
    colnames(Ta.Lb.ag) <- c("Spectrum", "Ta L-beta")
    
    W.Lb.cps <- subset(data$CPS, !(data$Energy < W$Line[8]-0.02 | data$Energy > W$Line[10]+0.02))
    W.file <- subset(data$Spectrum, !(data$Energy < W$Line[8]-0.02 | data$Energy > W$Line[10]+0.02))
    W.Lb.frame <- data.frame(is.0(W.Lb.cps, W.file))
    colnames(W.Lb.frame) <- c("Counts", "Spectrum")
    W.Lb.ag <- aggregate(list(W.Lb.frame$Counts), by=list(W.Lb.frame$Spectrum), FUN="sum")
    colnames(W.Lb.ag) <- c("Spectrum", "W L-beta")
    
    Re.Lb.cps <- subset(data$CPS, !(data$Energy < Re$Line[8]-0.02 | data$Energy > Re$Line[10]+0.02))
    Re.file <- subset(data$Spectrum, !(data$Energy < Re$Line[8]-0.02 | data$Energy > Re$Line[10]+0.02))
    Re.Lb.frame <- data.frame(is.0(Re.Lb.cps, Re.file))
    colnames(Re.Lb.frame) <- c("Counts", "Spectrum")
    Re.Lb.ag <- aggregate(list(Re.Lb.frame$Counts), by=list(Re.Lb.frame$Spectrum), FUN="sum")
    colnames(Re.Lb.ag) <- c("Spectrum", "Re L-beta")
    
    Os.Lb.cps <- subset(data$CPS, !(data$Energy < Os$Line[8]-0.02 | data$Energy > Os$Line[10]+0.02))
    Os.file <- subset(data$Spectrum, !(data$Energy < Os$Line[8]-0.02 | data$Energy > Os$Line[10]+0.02))
    Os.Lb.frame <- data.frame(is.0(Os.Lb.cps, Os.file))
    colnames(Os.Lb.frame) <- c("Counts", "Spectrum")
    Os.Lb.ag <- aggregate(list(Os.Lb.frame$Counts), by=list(Os.Lb.frame$Spectrum), FUN="sum")
    colnames(Os.Lb.ag) <- c("Spectrum", "Os L-beta")
    
    Ir.Lb.cps <- subset(data$CPS, !(data$Energy < Ir$Line[8]-0.02 | data$Energy > Ir$Line[10]+0.02))
    Ir.file <- subset(data$Spectrum, !(data$Energy < Ir$Line[8]-0.02 | data$Energy > Ir$Line[10]+0.02))
    Ir.Lb.frame <- data.frame(is.0(Ir.Lb.cps, Ir.file))
    colnames(Ir.Lb.frame) <- c("Counts", "Spectrum")
    Ir.Lb.ag <- aggregate(list(Ir.Lb.frame$Counts), by=list(Ir.Lb.frame$Spectrum), FUN="sum")
    colnames(Ir.Lb.ag) <- c("Spectrum", "Ir L-beta")
    
    Pt.Lb.cps <- subset(data$CPS, !(data$Energy < Pt$Line[8]-0.02 | data$Energy > Pt$Line[10]+0.02))
    Pt.file <- subset(data$Spectrum, !(data$Energy < Pt$Line[8]-0.02 | data$Energy > Pt$Line[10]+0.02))
    Pt.Lb.frame <- data.frame(is.0(Pt.Lb.cps, Pt.file))
    colnames(Pt.Lb.frame) <- c("Counts", "Spectrum")
    Pt.Lb.ag <- aggregate(list(Pt.Lb.frame$Counts), by=list(Pt.Lb.frame$Spectrum), FUN="sum")
    colnames(Pt.Lb.ag) <- c("Spectrum", "Pt L-beta")
    
    Au.Lb.cps <- subset(data$CPS, !(data$Energy < Au$Line[8]-0.02 | data$Energy > Au$Line[10]+0.02))
    Au.file <- subset(data$Spectrum, !(data$Energy < Au$Line[8]-0.02 | data$Energy > Au$Line[10]+0.02))
    Au.Lb.frame <- data.frame(is.0(Au.Lb.cps, Au.file))
    colnames(Au.Lb.frame) <- c("Counts", "Spectrum")
    Au.Lb.ag <- aggregate(list(Au.Lb.frame$Counts), by=list(Au.Lb.frame$Spectrum), FUN="sum")
    colnames(Au.Lb.ag) <- c("Spectrum", "Au L-beta")
    
    Hg.Lb.cps <- subset(data$CPS, !(data$Energy < Hg$Line[8]-0.02 | data$Energy > Hg$Line[10]+0.02))
    Hg.file <- subset(data$Spectrum, !(data$Energy < Hg$Line[8]-0.02 | data$Energy > Hg$Line[10]+0.02))
    Hg.Lb.frame <- data.frame(is.0(Hg.Lb.cps, Hg.file))
    colnames(Hg.Lb.frame) <- c("Counts", "Spectrum")
    Hg.Lb.ag <- aggregate(list(Hg.Lb.frame$Counts), by=list(Hg.Lb.frame$Spectrum), FUN="sum")
    colnames(Hg.Lb.ag) <- c("Spectrum", "Hg L-beta")
    
    Tl.Lb.cps <- subset(data$CPS, !(data$Energy < Tl$Line[8]-0.02 | data$Energy > Tl$Line[10]+0.02))
    Tl.file <- subset(data$Spectrum, !(data$Energy < Tl$Line[8]-0.02 | data$Energy > Tl$Line[10]+0.02))
    Tl.Lb.frame <- data.frame(is.0(Tl.Lb.cps, Tl.file))
    colnames(Tl.Lb.frame) <- c("Counts", "Spectrum")
    Tl.Lb.ag <- aggregate(list(Tl.Lb.frame$Counts), by=list(Tl.Lb.frame$Spectrum), FUN="sum")
    colnames(Tl.Lb.ag) <- c("Spectrum", "Tl L-beta")
    
    Pb.Lb.cps <- subset(data$CPS, !(data$Energy < Pb$Line[8]-0.02 | data$Energy > Pb$Line[10]+0.02))
    Pb.file <- subset(data$Spectrum, !(data$Energy < Pb$Line[8]-0.02 | data$Energy > Pb$Line[10]+0.02))
    Pb.Lb.frame <- data.frame(is.0(Pb.Lb.cps, Pb.file))
    colnames(Pb.Lb.frame) <- c("Counts", "Spectrum")
    Pb.Lb.ag <- aggregate(list(Pb.Lb.frame$Counts), by=list(Pb.Lb.frame$Spectrum), FUN="sum")
    colnames(Pb.Lb.ag) <- c("Spectrum", "Pb L-beta")
    
    Bi.Lb.cps <- subset(data$CPS, !(data$Energy < Bi$Line[8]-0.02 | data$Energy > Bi$Line[10]+0.02))
    Bi.file <- subset(data$Spectrum, !(data$Energy < Bi$Line[8]-0.02 | data$Energy > Bi$Line[10]+0.02))
    Bi.Lb.frame <- data.frame(is.0(Bi.Lb.cps, Bi.file))
    colnames(Bi.Lb.frame) <- c("Counts", "Spectrum")
    Bi.Lb.ag <- aggregate(list(Bi.Lb.frame$Counts), by=list(Bi.Lb.frame$Spectrum), FUN="sum")
    colnames(Bi.Lb.ag) <- c("Spectrum", "Bi L-beta")
    
    Po.Lb.cps <- subset(data$CPS, !(data$Energy < Po$Line[8]-0.02 | data$Energy > Po$Line[10]+0.02))
    Po.file <- subset(data$Spectrum, !(data$Energy < Po$Line[8]-0.02 | data$Energy > Po$Line[10]+0.02))
    Po.Lb.frame <- data.frame(is.0(Po.Lb.cps, Po.file))
    colnames(Po.Lb.frame) <- c("Counts", "Spectrum")
    Po.Lb.ag <- aggregate(list(Po.Lb.frame$Counts), by=list(Po.Lb.frame$Spectrum), FUN="sum")
    colnames(Po.Lb.ag) <- c("Spectrum", "Po L-beta")
    
    At.Lb.cps <- subset(data$CPS, !(data$Energy < At$Line[8]-0.02 | data$Energy > At$Line[10]+0.02))
    At.file <- subset(data$Spectrum, !(data$Energy < At$Line[8]-0.02 | data$Energy > At$Line[10]+0.02))
    At.Lb.frame <- data.frame(is.0(At.Lb.cps, At.file))
    colnames(At.Lb.frame) <- c("Counts", "Spectrum")
    At.Lb.ag <- aggregate(list(At.Lb.frame$Counts), by=list(At.Lb.frame$Spectrum), FUN="sum")
    colnames(At.Lb.ag) <- c("Spectrum", "At L-beta")
    
    Rn.Lb.cps <- subset(data$CPS, !(data$Energy < Rn$Line[8]-0.02 | data$Energy > Rn$Line[10]+0.02))
    Rn.file <- subset(data$Spectrum, !(data$Energy < Rn$Line[8]-0.02 | data$Energy > Rn$Line[10]+0.02))
    Rn.Lb.frame <- data.frame(is.0(Rn.Lb.cps, Rn.file))
    colnames(Rn.Lb.frame) <- c("Counts", "Spectrum")
    Rn.Lb.ag <- aggregate(list(Rn.Lb.frame$Counts), by=list(Rn.Lb.frame$Spectrum), FUN="sum")
    colnames(Rn.Lb.ag) <- c("Spectrum", "Rn L-beta")
    
    Fr.Lb.cps <- subset(data$CPS, !(data$Energy < Fr$Line[8]-0.02 | data$Energy > Fr$Line[10]+0.02))
    Fr.file <- subset(data$Spectrum, !(data$Energy < Fr$Line[8]-0.02 | data$Energy > Fr$Line[10]+0.02))
    Fr.Lb.frame <- data.frame(is.0(Fr.Lb.cps, Fr.file))
    colnames(Fr.Lb.frame) <- c("Counts", "Spectrum")
    Fr.Lb.ag <- aggregate(list(Fr.Lb.frame$Counts), by=list(Fr.Lb.frame$Spectrum), FUN="sum")
    colnames(Fr.Lb.ag) <- c("Spectrum", "Fr L-beta")
    
    Ra.Lb.cps <- subset(data$CPS, !(data$Energy < Ra$Line[8]-0.02 | data$Energy > Ra$Line[10]+0.02))
    Ra.file <- subset(data$Spectrum, !(data$Energy < Ra$Line[8]-0.02 | data$Energy > Ra$Line[10]+0.02))
    Ra.Lb.frame <- data.frame(is.0(Ra.Lb.cps, Ra.file))
    colnames(Ra.Lb.frame) <- c("Counts", "Spectrum")
    Ra.Lb.ag <- aggregate(list(Ra.Lb.frame$Counts), by=list(Ra.Lb.frame$Spectrum), FUN="sum")
    colnames(Ra.Lb.ag) <- c("Spectrum", "Ra L-beta")
    
    Ac.Lb.cps <- subset(data$CPS, !(data$Energy < Ac$Line[8]-0.02 | data$Energy > Ac$Line[10]+0.02))
    Ac.file <- subset(data$Spectrum, !(data$Energy < Ac$Line[8]-0.02 | data$Energy > Ac$Line[10]+0.02))
    Ac.Lb.frame <- data.frame(is.0(Ac.Lb.cps, Ac.file))
    colnames(Ac.Lb.frame) <- c("Counts", "Spectrum")
    Ac.Lb.ag <- aggregate(list(Ac.Lb.frame$Counts), by=list(Ac.Lb.frame$Spectrum), FUN="sum")
    colnames(Ac.Lb.ag) <- c("Spectrum", "Ac L-beta")
    
    Th.Lb.cps <- subset(data$CPS, !(data$Energy < Th$Line[8]-0.02 | data$Energy > Th$Line[10]+0.02))
    Th.file <- subset(data$Spectrum, !(data$Energy < Th$Line[8]-0.02 | data$Energy > Th$Line[10]+0.02))
    Th.Lb.frame <- data.frame(is.0(Th.Lb.cps, Th.file))
    colnames(Th.Lb.frame) <- c("Counts", "Spectrum")
    Th.Lb.ag <- aggregate(list(Th.Lb.frame$Counts), by=list(Th.Lb.frame$Spectrum), FUN="sum")
    colnames(Th.Lb.ag) <- c("Spectrum", "Th L-beta")
    
    Pa.Lb.cps <- subset(data$CPS, !(data$Energy < Pa$Line[8]-0.02 | data$Energy > Pa$Line[10]+0.02))
    Pa.file <- subset(data$Spectrum, !(data$Energy < Pa$Line[8]-0.02 | data$Energy > Pa$Line[10]+0.02))
    Pa.Lb.frame <- data.frame(is.0(Pa.Lb.cps, Pa.file))
    colnames(Pa.Lb.frame) <- c("Counts", "Spectrum")
    Pa.Lb.ag <- aggregate(list(Pa.Lb.frame$Counts), by=list(Pa.Lb.frame$Spectrum), FUN="sum")
    colnames(Pa.Lb.ag) <- c("Spectrum", "Pa L-beta")
    
    U.Lb.cps <- subset(data$CPS, !(data$Energy < U$Line[8]-0.02 | data$Energy > U$Line[10]+0.02))
    U.file <- subset(data$Spectrum, !(data$Energy < U$Line[8]-0.02 | data$Energy > U$Line[10]+0.02))
    U.Lb.frame <- data.frame(is.0(U.Lb.cps, U.file))
    colnames(U.Lb.frame) <- c("Counts", "Spectrum")
    U.Lb.ag <- aggregate(list(U.Lb.frame$Counts), by=list(U.Lb.frame$Spectrum), FUN="sum")
    colnames(U.Lb.ag) <- c("Spectrum", "U L-beta")
    
    Pu.Lb.cps <- subset(data$CPS, !(data$Energy < Pu$Line[8]-0.02 | data$Energy > Pu$Line[10]+0.02))
    Pu.file <- subset(data$Spectrum, !(data$Energy < Pu$Line[8]-0.02 | data$Energy > Pu$Line[10]+0.02))
    Pu.Lb.frame <- data.frame(is.0(Pu.Lb.cps, Pu.file))
    colnames(Pu.Lb.frame) <- c("Counts", "Spectrum")
    Pu.Lb.ag <- aggregate(list(Pu.Lb.frame$Counts), by=list(Pu.Lb.frame$Spectrum), FUN="sum")
    colnames(Pu.Lb.ag) <- c("Spectrum", "Pu L-beta")
    
    compton.norm <- subset(data$CPS, !(data$Energy < 18.4 | data$Energy > 19.4))
    compton.file <- subset(data$Spectrum, !(data$Energy < 18.4 | data$Energy > 19.4))
    compton.frame <- data.frame(is.0(compton.norm, compton.file))
    colnames(compton.frame) <- c("Compton", "Spectrum")
    compton.frame.ag <- aggregate(list(compton.frame$Compton), by=list(compton.frame$Spectrum), FUN="sum")
    colnames(compton.frame.ag) <- c("Spectrum", "Compton")
    
    Rayleigh <- Rh.Ka.ag
    colnames(Rayleigh) <- c("Spectrum", "Rayleigh")
    Compton <- compton.frame.ag
    
    spectra.lines <- Reduce(function(x, y) merge(x, y, all=TRUE), list(Na.Ka.ag, Na.Kb.ag, Mg.Ka.ag, Mg.Kb.ag, Al.Ka.ag, Al.Kb.ag, Si.Ka.ag, Si.Kb.ag, P.Ka.ag, P.Kb.ag, S.Ka.ag, S.Kb.ag, Cl.Ka.ag, Cl.Kb.ag, Ar.Ka.ag, Ar.Kb.ag, K.Ka.ag, K.Kb.ag, Ca.Ka.ag, Ca.Kb.ag, Sc.Ka.ag, Sc.Kb.ag, Ti.Ka.ag, Ti.Kb.ag, V.Ka.ag, V.Kb.ag, Cr.Ka.ag, Cr.Kb.ag, Mn.Ka.ag, Mn.Kb.ag, Fe.Ka.ag, Fe.Kb.ag, Co.Ka.ag, Co.Kb.ag, Ni.Ka.ag, Ni.Kb.ag, Cu.Ka.ag, Cu.Kb.ag, Zn.Ka.ag, Zn.Kb.ag, Ga.Ka.ag, Ga.Kb.ag, Ge.Ka.ag, Ge.Kb.ag, As.Ka.ag, As.Kb.ag, Se.Ka.ag, Se.Kb.ag, Br.Ka.ag, Br.Kb.ag, Kr.Ka.ag, Kr.Kb.ag, Rb.Ka.ag, Rb.Kb.ag, Sr.Ka.ag, Sr.Kb.ag, Y.Kb.ag, Y.Ka.ag, Zr.Ka.ag, Zr.Kb.ag, Nb.Ka.ag, Nb.Kb.ag, Mo.Ka.ag, Mo.Kb.ag, Mo.La.ag, Mo.Lb.ag, Ru.Ka.ag, Ru.Kb.ag, Ru.La.ag, Ru.Lb.ag, Rh.Ka.ag, Rh.Kb.ag, Rh.La.ag, Rh.Lb.ag, Pd.Ka.ag, Pd.Kb.ag, Pd.La.ag, Pd.Lb.ag, Ag.Ka.ag, Ag.Kb.ag, Ag.La.ag, Ag.Lb.ag, Cd.Ka.ag, Cd.Kb.ag, Cd.La.ag, Cd.Lb.ag,  In.Ka.ag, In.Kb.ag, In.La.ag, Sn.Ka.ag, Sn.Kb.ag, Sn.La.ag, Sn.Lb.ag, Sb.Ka.ag, Sb.Kb.ag, Sb.La.ag, Sb.Lb.ag, Te.Ka.ag, Te.Kb.ag, Te.La.ag, Te.Lb.ag, I.Ka.ag, I.Kb.ag, I.La.ag, I.Lb.ag, Xe.Ka.ag, Xe.Kb.ag, Xe.La.ag, Xe.Lb.ag, Cs.Ka.ag, Cs.Kb.ag, Cs.La.ag, Cs.Lb.ag, Ba.Ka.ag, Ba.Kb.ag, Ba.La.ag, Ba.Lb.ag, La.Ka.ag, La.Kb.ag, La.La.ag, La.Lb.ag, Ce.Ka.ag, Ce.Kb.ag, Ce.La.ag, Ce.Lb.ag, Pr.Ka.ag, Pr.Kb.ag, Pr.La.ag, Pr.Lb.ag, Nd.Ka.ag, Nd.Kb.ag, Nd.La.ag, Nd.Lb.ag, Pm.La.ag, Pm.Lb.ag, Sm.La.ag, Sm.Lb.ag, Eu.La.ag, Eu.Lb.ag, Gd.La.ag, Gd.Lb.ag, Tb.La.ag, Tb.Lb.ag, Dy.La.ag, Dy.Lb.ag, Ho.La.ag, Ho.Lb.ag, Er.La.ag, Er.Lb.ag, Tm.La.ag, Tm.Lb.ag, Yb.La.ag, Yb.Lb.ag, Lu.La.ag, Lu.Lb.ag, Hf.La.ag, Hf.Lb.ag, Ta.La.ag, Ta.Lb.ag, W.La.ag, W.Lb.ag, Re.La.ag, Re.Lb.ag, Os.La.ag, Os.Lb.ag, Ir.La.ag, Ir.Lb.ag, Pt.La.ag, Pt.Lb.ag, Au.La.ag, Au.Lb.ag, Hg.La.ag, Hg.Lb.ag, Tl.La.ag, Tl.Lb.ag, Pb.La.ag, Pb.Lb.ag, Bi.La.ag, Bi.Lb.ag, Po.La.ag, Po.Lb.ag, At.La.ag, At.Lb.ag, Rn.La.ag, Rn.Lb.ag, Fr.La.ag, Fr.Lb.ag, Ra.La.ag, Ra.Lb.ag, Ac.La.ag, Ac.Lb.ag, Th.La.ag, Th.Lb.ag, Pa.La.ag, Pa.Lb.ag, U.La.ag, U.Lb.ag, Pu.La.ag, Pu.Lb.ag, Compton, Rayleigh))
    
    spectra.lines <- data.frame(spectra.lines)
    return(spectra.lines)
    
}



forestTryUI <- function(radiocal=3, neuralhiddenlayers=NULL, selection=NULL, maxsample=NULL){
    
    neuralhiddenlayers <- if(is.null(neuralhiddenlayers)){
        1
    } else if(!is.null(neuralhiddenlayers)){
        neuralhiddenlayers
    }
    
    selection <- if(is.null(selection)){
        5
    } else if(!is.null(selection)){
        selection
    }
    
    maxsample <- if(is.null(maxsample)){
        15
    } else if(!is.null(maxsample)){
        maxsample
    }
    
    if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        sliderInput("foresttry", label="Sampling", min=2, max=maxsample-2, value=selection)
    }  else if(radiocal==5){
        sliderInput("foresttry", label="Sampling", min=2, max=maxsample-2, value=selection)
    } else if(radiocal==6 && neuralhiddenlayers == 1){
        NULL
    } else if(radiocal==6 && neuralhiddenlayers > 1){
        sliderInput("foresttry", label="Sampling", min=2, max=maxsample-2, value=selection)
    } else if(radiocal==7 && neuralhiddenlayers == 1){
        NULL
    } else if(radiocal==7 && neuralhiddenlayers > 1){
        sliderInput("foresttry", label="Sampling", min=2, max=maxsample-2, value=selection)
    } else if(radiocal==8){
        NULL
    } else if(radiocal==9){
        NULL
    }
}

maeSummary <- function (data,
lev = NULL,
model = NULL) {
    out <- Metrics::mae(data$obs, data$pred)
    names(out) <- "MAE"
    out
}

logmaeSummary <- function (data,
lev = NULL,
model = NULL) {
    out <- Metrics::mae(log10(data$obs), log10(data$pred))
    names(out) <- "logMAE"
    out
}

smapeSummary <- function (data,
lev = NULL,
model = NULL) {
    out <- Metrics::smape(data$obs, data$pred)
    names(out) <- "SMAPE"
    out
}

forestMetricQuantUI <- function(radiocal, selection){
    if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        selectInput("forestmetric", label="Metric", choices=c("Root Mean Square Error"="RMSE", "R2"="Rsquared", "Mean Absolute Error"="MAE", "Log Absolute Error"="logMAE", "Symmetric Mean Absolute Percentage Error"="SMAPE"), selected=selection)
    } else if(radiocal==5){
        selectInput("forestmetric", label="Metric", choices=c("Root Mean Square Error"="RMSE", "R2"="Rsquared", "Mean Absolute Error"="MAE", "Log Absolute Error"="logMAE", "Symmetric Mean Absolute Percentage Error"="SMAPE"), selected=selection)
    } else if(radiocal==6){
        selectInput("forestmetric", label="Metric", choices=c("Root Mean Square Error"="RMSE", "R2"="Rsquared", "Mean Absolute Error"="MAE", "Log Absolute Error"="logMAE", "Symmetric Mean Absolute Percentage Error"="SMAPE"), selected=selection)
    } else if(radiocal==7){
        selectInput("forestmetric", label="Metric", choices=c("Root Mean Square Error"="RMSE", "R2"="Rsquared", "Mean Absolute Error"="MAE", "Log Absolute Error"="logMAE", "Symmetric Mean Absolute Percentage Error"="SMAPE"), selected=selection)
    } else if(radiocal==8){
        selectInput("forestmetric", label="Metric", choices=c("Root Mean Square Error"="RMSE", "R2"="Rsquared", "Mean Absolute Error"="MAE", "Log Absolute Error"="logMAE", "Symmetric Mean Absolute Percentage Error"="SMAPE"), selected=selection)
    } else if(radiocal==9){
        selectInput("forestmetric", label="Metric", choices=c("Root Mean Square Error"="RMSE", "R2"="Rsquared", "Mean Absolute Error"="MAE", "Log Absolute Error"="logMAE", "Symmetric Mean Absolute Percentage Error"="SMAPE"), selected=selection)
    }
}

forestMetricQualUI <- function(radiocal, selection){
    if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        selectInput("forestmetric", label="Metric", choices=c("Accuracy"="Accuracy", "Kappa"="Kappa", "Log Loss"="logLoss", "Region Under Curve"="ROC"), selected=selection)
    } else if(radiocal==5){
        selectInput("forestmetric", label="Metric", choices=c("Accuracy"="Accuracy", "Kappa"="Kappa", "Log Loss"="logLoss", "Region Under Curve"="ROC"), selected=selection)
    } else if(radiocal==6){
        selectInput("forestmetric", label="Metric", choices=c("Accuracy"="Accuracy", "Kappa"="Kappa", "Log Loss"="logLoss", "Region Under Curve"="ROC"), selected=selection)
    } else if(radiocal==7){
        selectInput("forestmetric", label="Metric", choices=c("Accuracy"="Accuracy", "Kappa"="Kappa", "Log Loss"="logLoss", "Region Under Curve"="ROC"), selected=selection)
    } else if(radiocal==8){
        selectInput("forestmetric", label="Metric", choices=c("Accuracy"="Accuracy", "Kappa"="Kappa", "Log Loss"="logLoss", "Region Under Curve"="ROC"), selected=selection)
    } else if(radiocal==9){
        selectInput("forestmetric", label="Metric", choices=c("Accuracy"="Accuracy", "Kappa"="Kappa", "Log Loss"="logLoss", "Region Under Curve"="ROC"), selected=selection)
    }
}

forestTrainUI <- function(radiocal, selection){
    if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        selectInput("foresttrain", label="Train Control", choices=c("k-fold Cross Validation"="cv", "Bootstrap"="boot", "0.632 Bootstrap"="boot632", "Optimism Bootstrap"="optimism_boot", "Repeated k-fold Cross Validation"="repeatedcv", "Leave One Out Cross Validation"="LOOCV", "Out of Bag Estimation"="oob"), selected=selection)
    }  else if(radiocal==5){
        selectInput("foresttrain", label="Train Control", choices=c("k-fold Cross Validation"="cv", "Bootstrap"="boot", "0.632 Bootstrap"="boot632", "Optimism Bootstrap"="optimism_boot", "Repeated k-fold Cross Validation"="repeatedcv", "Leave One Out Cross Validation"="LOOCV", "Out of Bag Estimation"="oob"), selected=selection)
    } else if(radiocal==6){
        selectInput("foresttrain", label="Train Control", choices=c("k-fold Cross Validation"="cv", "Bootstrap"="boot", "0.632 Bootstrap"="boot632", "Optimism Bootstrap"="optimism_boot", "Repeated k-fold Cross Validation"="repeatedcv", "Leave One Out Cross Validation"="LOOCV"), selected=selection)
    } else if(radiocal==7){
        selectInput("foresttrain", label="Train Control", choices=c("k-fold Cross Validation"="cv", "Bootstrap"="boot", "0.632 Bootstrap"="boot632", "Optimism Bootstrap"="optimism_boot", "Repeated k-fold Cross Validation"="repeatedcv", "Leave One Out Cross Validation"="LOOCV"), selected=selection)
    } else if(radiocal==8){
        selectInput("foresttrain", label="Train Control", choices=c("k-fold Cross Validation"="cv", "Bootstrap"="boot", "0.632 Bootstrap"="boot632", "Optimism Bootstrap"="optimism_boot", "Repeated k-fold Cross Validation"="repeatedcv", "Leave One Out Cross Validation"="LOOCV"), selected=selection)
    } else if(radiocal==9){
        selectInput("foresttrain", label="Train Control", choices=c("k-fold Cross Validation"="cv", "Bootstrap"="boot", "0.632 Bootstrap"="boot632", "Optimism Bootstrap"="optimism_boot", "Repeated k-fold Cross Validation"="repeatedcv", "Leave One Out Cross Validation"="LOOCV"), selected=selection)
    }
}

forestNumberUI <- function(radiocal, selection){
    if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        sliderInput("forestnumber", label="Iterations", min=5, max=2000, value=selection)
    }  else if(radiocal==5){
        sliderInput("forestnumber", label="Iterations", min=5, max=2000, value=selection)
    } else if(radiocal==6){
        sliderInput("forestnumber", label="Iterations", min=5, max=2000, value=selection)
    } else if(radiocal==7){
        sliderInput("forestnumber", label="Iterations", min=5, max=2000, value=selection)
    } else if(radiocal==8){
        sliderInput("forestnumber", label="Iterations", min=5, max=2000, value=selection)
    } else if(radiocal==9){
        sliderInput("forestnumber", label="Iterations", min=5, max=2000, value=selection)
    }
}

cvRepeatsUI <- function(radiocal, foresttrain, selection){
    if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4 && foresttrain=="repeatedcv"){
        sliderInput("cvrepeats", label="Repeats", min=5, max=500, value=selection)
    } else if(radiocal==4 && foresttrain!="repeatedcv"){
        NULL
    } else if(radiocal==5 && foresttrain=="repeatedcv"){
        sliderInput("cvrepeats", label="Repeats", min=5, max=500, value=selection)
    } else if(radiocal==5 && foresttrain!="repeatedcv"){
        NULL
    } else if(radiocal==6 && foresttrain=="repeatedcv"){
        sliderInput("cvrepeats", label="Repeats", min=5, max=500, value=selection)
    } else if(radiocal==6 && foresttrain!="repeatedcv"){
        NULL
    } else if(radiocal==7 && foresttrain=="repeatedcv"){
        sliderInput("cvrepeats", label="Repeats", min=5, max=500, value=selection)
    } else if(radiocal==7 && foresttrain!="repeatedcv"){
        NULL
    } else if(radiocal==8 && foresttrain=="repeatedcv"){
        sliderInput("cvrepeats", label="Repeats", min=5, max=500, value=selection)
    } else if(radiocal==8 && foresttrain!="repeatedcv"){
        NULL
    } else if(radiocal==9 && foresttrain=="repeatedcv"){
        sliderInput("cvrepeats", label="Repeats", min=5, max=500, value=selection)
    } else if(radiocal==9 && foresttrain!="repeatedcv"){
        NULL
    }
}

forestTreesUI <- function(radiocal, selection, xgbtype="Tree"){
    if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        sliderInput("foresttrees", label="Trees", min=50, max=2000, value=selection)
    } else if(radiocal==5){
        sliderInput("foresttrees", label="Trees", min=50, max=2000, value=selection)
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        NULL
    } else if(radiocal==8 && xgbtype=="Tree"){
        sliderInput("foresttrees", label="Number of Rounds", min=50, max=2000, value=selection)
    } else if(radiocal==8 && xgbtype=="Linear"){
        sliderInput("foresttrees", label="Number of Rounds", min=50, max=2000, value=selection)
    } else if(radiocal==9 && xgbtype=="Tree"){
        sliderInput("foresttrees", label="Number of Rounds", min=50, max=2000, value=selection)
    } else if(radiocal==9 && xgbtype=="Linear"){
        sliderInput("foresttrees", label="Number of Rounds", min=50, max=2000, value=selection)
    }
}

neuralHiddenLayersUI <- function(radiocal, selection){
    if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        sliderInput("neuralhiddenlayers", label="Hidden Layers", min=1, max=3, value=selection)
    } else if(radiocal==7){
        sliderInput("neuralhiddenlayers", label="Hidden Layers", min=1, max=3, value=selection)
    } else if(radiocal==8){
        NULL
    } else if(radiocal==9){
        NULL
    }
}

neuralHiddenUnitsUi <- function(radiocal, selection){
    if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        sliderInput("neuralhiddenunits", label="Hidden Units", min=1, max=10, value=selection)
    } else if(radiocal==7){
        sliderInput("neuralhiddenunits", label="Hidden Units", min=1, max=10, value=selection)
    } else if(radiocal==8){
        NULL
    } else if(radiocal==9){
        NULL
    }
}

neuralWeightDecayUI <- function(radiocal, selection, neuralhiddenlayers){
    if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6 && neuralhiddenlayers == 1){
        sliderInput("neuralweightdecay", label="Weight Decay", min=0.1, max=0.7, step=0.1, value=selection)
    } else if(radiocal==6 && neuralhiddenlayers > 1){
        NULL
    } else if(radiocal==7 && neuralhiddenlayers == 1){
        sliderInput("neuralweightdecay", label="Weight Decay", min=0.1, max=0.7, step=0.1, value=selection)
    } else if(radiocal==7 && neuralhiddenlayers > 1){
        NULL
    } else if(radiocal==8){
        NULL
    } else if(radiocal==9){
        NULL
    }
}

neuralMaxIterationsUI <- function(radiocal, selection, neuralhiddenlayers){
    if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6 && neuralhiddenlayers == 1){
        sliderInput("neuralmaxiterations", label="Max Iterations", min=50, max=2000, value=selection)
    } else if(radiocal==6 && neuralhiddenlayers > 1){
        NULL
    } else if(radiocal==7 && neuralhiddenlayers == 1){
        sliderInput("neuralmaxiterations", label="Max Iterations", min=50, max=2000, value=selection)
    } else if(radiocal==7 && neuralhiddenlayers > 1){
        NULL
    } else if(radiocal==8){
        NULL
    } else if(radiocal==9){
        NULL
    }
}

treeDepthUI <- function(radiocal, selection, xgbtype="Tree"){
    if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        NULL
    } else if(radiocal==8 && xgbtype=="Tree"){
        sliderInput("treedepth", label="Tree Depth", min=2, max=50, step=1, value=selection)
    } else if(radiocal==8 && xgbtype=="Linear"){
        NULL
    } else if(radiocal==9 && xgbtype=="Tree"){
        sliderInput("treedepth", label="Tree Depth", min=2, max=50, step=1, value=selection)
    } else if(radiocal==9 && xgbtype=="Linear"){
        NULL
    }
}

xgbTypeUI <- function(radiocal, selection){
    if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        NULL
    } else if(radiocal==8){
        selectInput("xgbtype", label="XGBoost Type", choices=c("Tree", "Linear"), selected="Tree")
    } else if(radiocal==9){
        selectInput("xgbtype", label="XGBoost Type", choices=c("Tree", "Linear"), selected="Tree")
    }
}

xgbAlphaUI <- function(radiocal, selection, xgbtype="Tree"){
    if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        NULL
    } else if(radiocal==8 && xgbtype=="Tree"){
        NULL
    } else if(radiocal==8 && xgbtype=="Linear"){
        sliderInput("xgbalpha", label="Alpha", min=0, max=10, step=0.05, value=selection)
    } else if(radiocal==9 && xgbtype=="Tree"){
        NULL
    } else if(radiocal==9 && xgbtype=="Linear"){
        sliderInput("xgbalpha", label="Alpha", min=0, max=10, step=0.05, value=selection)
    }
}

xgbGammaUI <- function(radiocal, selection, xgbtype="Tree"){
    if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        NULL
    } else if(radiocal==8 && xgbtype=="Tree"){
        sliderInput("xgbgamma", label="Gamma", min=0, max=10, step=0.05, value=selection)
    } else if(radiocal==8 && xgbtype=="Linear"){
        NULL
    } else if(radiocal==9 && xgbtype=="Tree"){
        sliderInput("xgbgamma", label="Gamma", min=0, max=10, step=0.05, value=selection)
    } else if(radiocal==9 && xgbtype=="Linear"){
        NULL
    }
}

xgbEtaUI <- function(radiocal, selection){
    if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        NULL
    } else if(radiocal==8){
        sliderInput("xgbeta", label="Eta", min=0.05, max=0.95, step=0.05, value=selection)
    } else if(radiocal==9){
        sliderInput("xgbeta", label="Eta", min=0.05, max=0.95, step=0.05, value=selection)
        
    }
}

xgbLambdaUI <- function(radiocal, selection, xgbtype="Tree"){
    if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        NULL
    } else if(radiocal==8 && xgbtype=="Tree"){
        NULL
    } else if(radiocal==8 && xgbtype=="Linear"){
        sliderInput("xgblambda", label="Lambda", min=0, max=10, step=0.05, value=selection)
    } else if(radiocal==9 && xgbtype=="Tree"){
        NULL
    } else if(radiocal==9 && xgbtype=="Linear"){
        sliderInput("xgblambda", label="Lambda", min=0, max=10, step=0.05, value=selection)
    }
}

xgbSubSampleUI <- function(radiocal, selection, xgbtype="Tree"){
    if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        NULL
    } else if(radiocal==8 && xgbtype=="Tree"){
        sliderInput("xgbsubsample", label="Sub Sample", min=0.05, max=0.95, step=0.05, value=selection)
    } else if(radiocal==8 && xgbtype=="Linear"){
        NULL
    } else if(radiocal==9 && xgbtype=="Tree"){
        sliderInput("xgbsubsample", label="Sub Sample", min=0.05, max=0.95, step=0.05, value=selection)
    } else if(radiocal==9 && xgbtype=="Linear"){
        NULL
    }
}

xgbColSampleUI <- function(radiocal, selection, xgbtype="Tree"){
    if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        NULL
    } else if(radiocal==8 && xgbtype=="Tree"){
        sliderInput("xgbcolsample", label="Col Sample", min=0.05, max=0.95, step=0.05, value=selection)
    } else if(radiocal==8 && xgbtype=="Linear"){
        NULL
    } else if(radiocal==9 && xgbtype=="Tree"){
        sliderInput("xgbcolsample", label="Col Sample", min=0.05, max=0.95, step=0.05, value=selection)
    } else if(radiocal==9 && xgbtype=="Linear"){
        NULL
    }
}

xgbMinChildUI <- function(radiocal, selection, xgbtype="Tree"){
    if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        NULL
    } else if(radiocal==8 && xgbtype=="Tree"){
        sliderInput("xgbminchild", label="Min Child", min=0, max=15, step=1, value=selection)
    } else if(radiocal==8 && xgbtype=="Linear"){
        NULL
    } else if(radiocal==9 && xgbtype=="Tree"){
        sliderInput("xgbminchild", label="Min Child", min=0, max=15, step=1, value=selection)
    } else if(radiocal==9 && xgbtype=="Linear"){
        NULL
    }
}

source("MachineLearning.R")

