library(DT)
library(dplyr)
library(shinythemes)
library(shiny)
library(rhandsontable)



shinyUI(navbarPage("Analyzer", id="nav", theme = shinytheme("paper"),
tabPanel("Data",
div(class="outer",
sidebarLayout(
sidebarPanel(



actionButton("actionprocess", label = "Process Data"),
actionButton("actionplot", label = "Plot Covariance"),
downloadButton('downloadPlot', "Plot"),


tags$hr(),

uiOutput('filegrab'),


selectInput("filetype", label=NULL, c("PDZ","CSV", "Net", "Excel Spreadsheet", "CSV Spreadsheet", "Artax Excel"), selected="Excel Spreadsheet"),

tags$hr(),

textInput('projectname', label="Project Name", value=""),

#checkboxInput('advanced', "Advanced", value=FALSE),
#uiOutput('gainshiftui'),
#uiOutput('binaryui'),

tags$hr(),


element <- selectInput(
"element", "Element:",
c("(Ne) Neon" = "Ne.table",
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
"(Zn) Zinc"= "Zn.table",
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
"(U)  Uranium" = "U.table"),
selected="Fe.table"),


#checkboxInput('backgroundsubtract', "Background Subtract"),



tags$hr(),

fileInput('calfileinput1', 'Load Cal File', accept='.quant', multiple=FALSE),

checkboxInput('usecalfile', "Use Cal File"),




tags$hr(),

checkboxInput('otherdata', "Import Other Data", value=FALSE),

uiOutput('file2gen'),
uiOutput('calfile2gen'),
uiOutput('space23gen'),
uiOutput('file3gen'),
uiOutput('calfile3gen')



),




mainPanel(
fluidRow(
column(width = 11, class = "well",
plotOutput("distPlot", height = 650,
dblclick = "plot1_dblclick",
brush = brushOpts(
id = "plot1_brush",
resetOnNew = TRUE
)))))
))
),

tabPanel("Counts",
div(class="outer",

fluidRow(
sidebarLayout(
sidebarPanel(



actionButton('hotableprocess', "Enter Values"),
downloadButton('downloadData', "Table"),
downloadButton('thanksforallthefishtable', "MCL"),

tags$hr(),

conditionalPanel(
condition='input.dataset === spectra.line.table',
checkboxInput('clusterlearn', "Machine Learn Cluster", value=FALSE),
uiOutput('nvariablesui'),
uiOutput('usesubsetui'),
uiOutput('defaultlines')

)),




mainPanel(
tabsetPanel(
id = 'dataset',
tabPanel('All Data', dataTableOutput('mytable1')),
tabPanel('Add Categories', rHandsontableOutput('hot')),
tabPanel('Machine Determined Clusers', DT::dataTableOutput('thanksforallthefish'))

))
)

)



)),

tabPanel("Subset A",
fluidRow(
sidebarLayout(

sidebarPanel(
downloadButton("downloadsubseta", label = "Download")
),

mainPanel(
tabsetPanel(
id = 'dataset',
tabPanel('Subset Data A', splitLayout(cellWidths=c("10%", "10%", "10%", "10%", "10%", "10%"),
uiOutput('qualSelect1a'),
uiOutput('qualSelect2a'),
uiOutput('qualSelect3a'),
uiOutput('qualSelect4a'),
uiOutput('qualSelect5a'),
uiOutput('qualSelect6a')
)
),
tabPanel('Subsetted Table A', dataTableOutput('mydatamerge1a'))
)
)))),


tabPanel("Subset B",
fluidRow(
sidebarLayout(

sidebarPanel(
downloadButton("downloadsubsetb", label = "Download")
),

mainPanel(
tabsetPanel(
id = 'dataset',
tabPanel('Subset Data B', splitLayout(cellWidths=c("10%", "10%", "10%", "10%", "10%", "10%"),
uiOutput('qualSelect1b'),
uiOutput('qualSelect2b'),
uiOutput('qualSelect3b'),
uiOutput('qualSelect4b'),
uiOutput('qualSelect5b'),
uiOutput('qualSelect6b')
)
),
tabPanel('Subsetted Table B', dataTableOutput('mydatamerge1b'))
)
)))),

tabPanel("Subset Table",
fluidRow(
sidebarLayout(

sidebarPanel(
downloadButton("downloadsubsetfinal", label = "Download"),
checkboxInput("usefull", "Use full table", value=T),
uiOutput('clipsubsetfinal')
),

mainPanel(
tabsetPanel(
id = 'dataset',
tabPanel('Subsetted Table', dataTableOutput('mydatamerge2'))
))))),

tabPanel("PCA",
div(class="outer",


fluidRow(
sidebarLayout(

sidebarPanel(
uiOutput('knumui'),

uiOutput('pcacolourui'),

uiOutput('pcaFocusVariable'),
uiOutput('pcaFocusUI'),
uiOutput('pcaFocusLabel'),


sliderInput("spotsize", label = "Point Size", value=2, min=2, max=15),

checkboxInput('elipseplot1', "Elipse"),
checkboxInput('logtrans', "Log Transform"),

checkboxInput('usesubset', "Subset Data", value=FALSE),


uiOutput('inxlimrangepca'),
uiOutput('inylimrangepca'),


tags$hr(),


downloadButton('downloadPlot2', "Plot"),
downloadButton('xrfpcatablefulldownload', "Results")

),



mainPanel(
tabsetPanel(
id = 'dataset',
tabPanel('PCA Plot',

# this is an extra div used ONLY to create positioned ancestor for tooltip
# we don't change its position
div(
style = "position:relative",
plotOutput("xrfpcaplot", height = 650,
hover = hoverOpts("plot_hoverpca", delay = 100, delayType = "debounce"),
dblclick = "plot_pca_dblclick",
brush = brushOpts(id = "plot_pca_brush", resetOnNew = TRUE)),
uiOutput("hover_infopca")
)


),
tabPanel("Optimal Clusters",
div(
style = "position:relative",
plotOutput('optimalkplot',
hover = hoverOpts("plot_hoveroptimalk", delay = 100, delayType = "debounce")),
uiOutput("hover_infooptimalk"))
),
tabPanel("Table", DT::dataTableOutput('xrfpcatable'))


))

))

)),


tabPanel("Match",
div(class="outer",

fluidRow(
sidebarLayout(

sidebarPanel(
checkboxInput('usesubsetmatch', label="Use Subset", value=FALSE),
uiOutput('choosespectraui'),
selectInput('matchtype', "Match Type", choices=c("Untransformed", "Velocity", "Log", "Log-Velocity"), selected="Velocity"),
selectInput('matchcriteria', "Match Criteria", choices=c("R2", "AIC", "BIC"), selected="BIC"),
tags$hr(),
uiOutput('thebestmatchui'),

tags$hr(),

selectInput('elementfingerprint', "Element:",
c("(Ne) Neon" = "Ne.table",
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
"(Zn) Zinc"= "Zn.table",
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
"(U)  Uranium" = "U.table"),
selected="Fe.table")

),

mainPanel(
tabsetPanel(
tabPanel('Match',
div(
style = "position:relative",
plotOutput('matchplotmetric', height = 650,
dblclick = "plot_dblclickmatchmetric",
brush = brushOpts(id = "plot_brushmatchmetric", resetOnNew = TRUE),
hover = hoverOpts("plot_hovermatchmetric", delay = 100, delayType = "debounce")),
uiOutput("hover_infomatchmetric")
)),
tabPanel('Spectra',
div(
style = "position:relative",
plotOutput('matchplot', height = 650,
dblclick = "plot_dblclickmatch",
brush = brushOpts(id = "plot_brushmatch", resetOnNew = TRUE),
hover = hoverOpts("plot_hovermatch", delay = 100, delayType = "debounce")),
uiOutput("hover_infomatch")
)),
tabPanel("All Matches", DT::dataTableOutput('matchtable'))
))



)
)

)
),


tabPanel("Ternary Diagram",
div(class="outer",


fluidRow(
sidebarLayout(

sidebarPanel(


uiOutput('ternarycolourui'),


tags$hr(),


uiOutput('inaxisa'),
uiOutput('inaxisb'),
uiOutput('inaxisc'),
checkboxInput('terndensityplot', "Density Contour"),
checkboxInput('ternnormplot', "Normalize"),


tags$hr(),

sliderInput("ternpointsize", label = "Point Size", value=5, min=2, max=15),


tags$hr(),

downloadButton('downloadPlot5', "Plot")

),

mainPanel(
tabPanel('Ternary Plot', plotOutput('ternaryplot',
dblclick = "plot1_dblclick", height = 800,
brush = brushOpts(
id = "plot1_brush",
resetOnNew = TRUE
))))


))

)),

tabPanel("Elemental Ratios",
div(class="outer",
fluidRow(
sidebarLayout(
sidebarPanel(
uiOutput('ratiocolourui'),
uiOutput('ratioFocusVariable'),
uiOutput('ratioFocusUI'),
uiOutput('ratioFocusLabel'),
uiOutput('ratioFocusShape'),
tags$hr(),
uiOutput('inelementratioa'),
uiOutput('inelementratiob'),
uiOutput('inelementratioc'),
uiOutput('inelementratiod'),
tags$hr(),
sliderInput("spotsize2", label = "Point Size", value=5, min=2, max=15),
uiOutput('inxlimrangeratio'),
uiOutput('inylimrangeratio'),
checkboxInput('elipseplot2', "Elipse"),
tags$hr(),
downloadButton('downloadPlot4', "Plot")
),
mainPanel(
div(
style = "position:relative",
plotOutput("elementratiotimeseries", height = 650,
hover = hoverOpts("plot_hoverratio", delay = 100, delayType = "debounce"),
dblclick = "plot_ratio_dblclick",
brush = brushOpts(id = "plot_ratio_brush", resetOnNew = TRUE)),
uiOutput("hover_inforatio")
)
))
))


),

tabPanel("Machine Learning",
sidebarLayout(
sidebarPanel(
actionButton('runmodel', "Run"),
tags$hr(),
textInput("qualname", label = "Model Name", value="myModel"),
uiOutput('variableui'),
uiOutput('predictorsui'),
tags$hr(),
selectInput('modeltype', "Model Type", choices=list("Forest"=4, "XGBoost"=8), selected=4),
sliderInput('split', "Cross-Validation Split", min=0, max=99, value=20),
sliderInput('min_n', "Minimum Class Category", min=0, max=25, value=5),
uiOutput('xgbtypeui'),
uiOutput('usebayesui'),
uiOutput('foldsui'),
uiOutput('init_pointsui'),
uiOutput('n_iterui'),
uiOutput('forestmetricui'),
uiOutput('foresttrainui'),
uiOutput('cvrepeatsui'),
uiOutput('forestnumberui'),
uiOutput('foresttryui'),
uiOutput('testingroundsui'),
uiOutput('foresttreesui'),
uiOutput('neuralhiddenlayersui'),
uiOutput('neuralhiddenunitsui'),
uiOutput('neuralweightdecayui'),
uiOutput('neuralmaxiterationsui'),
uiOutput('treedepthui'),
uiOutput('xgbalphaui'),
uiOutput('xgbgammaui'),
uiOutput('xgbetaui'),
uiOutput('xgblambdaui'),
uiOutput('xgbsubsampleui'),
uiOutput('xgbcolsampleui'),
uiOutput('xgbminchildui'),
tags$hr(),
uiOutput('paralleltypeui'),
tags$hr(),
downloadButton('modeldownload', "Model")
),

mainPanel(
tabsetPanel(
tabPanel('Result',
plotOutput('resultplot', height="600px"),
numericInput('resultwidth', "Width", min=1, max=100, value=5),
numericInput('resultheight', "Height", min=1, max=100, value=7),
downloadButton('downloadresult', "Result Plot")),
tabPanel('Importance',
plotOutput('importanceplot', height="600px"),
downloadButton('downloadimportance', "Importance Plot"),
numericInput('importancewidth', "Width", min=1, max=100, value=7),
numericInput('importanceheight', "Height", min=1, max=100, value=10))
)
)

)

)


))










