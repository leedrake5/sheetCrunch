
#CloudSpectra

This app is designed for qualitative and semi-quantitive analysis of XRF spectra. The software is optimized for the Bruker Tracer IIISD, but you can email me at b.lee.drake@gmail if you would like to help adapt it for other platforms. 


You can run this software from the R console with:

```
if (!require(devtools))
  install.packages("devtools")
devtools::install_github("leedrake5/CloudSpectra”)
shiny::runGitHub("leedrake5/CloudSpectra")
```

This software requires external processing packages in R. If you are running this for the first time, copy and paste the following:

```
install.packages(c(“DT”, “dplyr”, “shinythemes”, “shiny”, “pblapply”, “reshape2”, “TTR”, “data.table”, “ggplot2”, “ggtern”, “devtools”))
install_github(“rstudio/shiny-incubator”)
```

…and go grab a cup of coffee, because it will be a bit of a wait as things download. 

