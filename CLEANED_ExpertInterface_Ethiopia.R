####################################################################################################
#
# Shiny App for CLEANED System ILRI  August 2018 by Victor N. Mose and Caroline Mburu
####################################################################################################
## app.R ##


rm(list=ls())
options(repos=c(CRAN="https://cran.rstudio.com"))
library(rsconnect)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(graphics)
library(rgeos)
library(sp)
library(raster)
library(maptools)
library(rgdal)
library(gridExtra)
library(grid)
library(RColorBrewer)
path<<-'D:/Dropbox/Cleaned Ethiopia'
setwd(path)
source('2-load_data.R')

body <- dashboardBody(
  
  
  
  useShinyjs(),
  
  
  
  actionButton("hideSidebar", "Hide sidebar menu:",
               icon("angle-double-left"), 
               style="color: #fff; background-color: #8B2323; border-color: #2e6da4"),
  
  actionButton("showSidebar", "Show sidebar menu:",
               icon("angle-double-right"), 
               style="color: #fff; background-color: #8B2323; border-color: #2e6da4"),
  
  
  tags$head(tags$style(HTML(".grViz { width:100%!important;}"))),
  
  
  mainPanel(tabsetPanel(id = "inTabset",
                        tabPanel(title = "Productivity module", value = "panel1", ""),
                        tabPanel(title = "Water Impact", value = "panel2",
                                 fluidRow(box(width=12, background ="blue",img(src="waterimpact.jpg", width=650))),
                                 fluidRow(box(width=12, title = "", height=700,
                                              actionButton("gowt", "Run the water Impact"),
                                              
                                              plotOutput("plot12"))),
                                 
                                 fluidRow(box(width=12, title = "Summary table water Impact",
                                             
                                              plotOutput("plotable"))
                                 )
                        ),
                        tabPanel(title = "Greenhouse gas Impact", value = "panel3", 
                                 fluidRow(box(width=12, background ="green",img(src="greenhouseimpact.jpg", width=650))),
                                 fluidRow(box(width=12, title = " ",
                                              actionButton("gogh", "Run Greenhouse Impact "),
                                              # numericInput("n", "n", 50),
                                              plotOutput("plot123"))),
                                 fluidRow(box(width=12, title = "Summary table greenhouse Impact",
                                              #actionButton("gowt", " "),
                                              # numericInput("n", "n", 50),
                                              plotOutput("plotableghg"))
                                 )),
                        # tabPanel(title = "Biodiversity Impact", value = "panel4", 
                        #          fluidRow(box(width=12, background ="light-blue",img(src="biodiversityimpact.JPG", width=650))),
                        #          fluidRow(box(width=12, title = " ",
                        #                       actionButton("gobdiv", "Run biodivesity Impact"),
                        #                       # numericInput("n", "n", 50),
                        #                       plotOutput("plot12bid"))
                        #                   
                        #          ),
                        #          
                        #          fluidRow(box(width=12, title = "Summary table biodiversity Impact",
                        #                       #actionButton("gowt", " "),
                        #                       # numericInput("n", "n", 50),
                        #                       plotOutput("plotablediv"))
                        #          )),
                        tabPanel(title = "Soil Impact", value = "panel5", 
                                 fluidRow(box(width=12, background ="red",img(src="soilimpact.JPG", width=650))),
                                 fluidRow(box(width=12, title = " ",height=1000,
                                              actionButton("gosol", "Run soil Impact",icon = NULL),
                                              # numericInput("n", "n", 50),
                                              plotOutput("plotsoil"))
                                          
                                          
                                 ),
                                 
                                 fluidRow(box(width=12, title = "Summary table soil Impact",
                                              #actionButton("gowt", " "),
                                              # numericInput("n", "n", 50),
                                              plotOutput("plotablesoil"))))
  )),
  
  
  
  tabItems(
    tabItem(tabName = "dashboard1",
            #h2("ADVANCED USER INTERFACE"),
            
  tags$head(tags$style(HTML("div.col-sm-6 {padding:1px}"))),
  fluidRow(
    box(status = "warning", title="Notes for Advanced/Expert Users", width = 12, "", background ="red", img(src="IntroAdvanced1.jpg", width=650))),
  
  fluidRow(
    box(status = "warning", title="Click here to update baserun!", width = 12, "", background ="red",
        div(style="height: 35px;", actionButton("go", "Base run")))),
  
  fluidRow(box(width=12,
               #actionButton("go", " "),
               # numericInput("n", "n", 50),
               verbatimTextOutput("plot",  placeholder = TRUE)
               
               
  )),
  
  
  
  fluidRow(
  box(status = "primary",width = 12, "PRODUCTION CATEGORIES :-----------:-----------:------------:-------------:------:-----:-----------: DATA ALREADY LOADED",background = "navy")
),

fluidRow(
    box(status = "warning",width = 2, "",background ="red", textInput("name","Enter your name here:","My own")),
    box(status = "warning", width = 2, "LOCAL DAIRY CATTLE",background ="maroon"),
    box(status = "warning", width = 2,"LOCAL FATTENING/REARING CATTLE",background ="maroon"),
    box(status = "warning", width = 2, "DRAFT CATTLE",background ="maroon"),
    box(status = "warning", width = 2, "CROSS-BREED DAIRY CATTLE",background ="maroon"),
    box(status = "warning", width = 2, "SHEEP",background ="maroon")),

fluidRow(
    box(status = "warning", width = 2, "", background ="green",
        div(style="height: 35px;",textInput("3a", "",value="Number of Animals")),
        div(style="height: 35px;",textInput("4a", "", value="Alive weight(kg)")),
        div(style="height: 35px;",textInput("5a", "", value="Milk production(kg/cow/year)")),
        textInput("6a", "", value="Dressing percentage")),
    
    box(status = "warning", width = 2, "",background ="green",
        div(style="height: 35px;",numericInput("numcow_es", "", 22000, min = 0, max = 1000000)),
        div(style="height: 35px;",numericInput("lwes", "",150 , min = 100, max = 300)),
        div(style="height: 35px;",numericInput("myes", "", 1100, min = 0, max = 4000)),
        numericInput("des", "",  0.3, min = 0, max = 1)), 
    
    
    box(status = "warning", width = 2, " ",background ="green",
        # div(style="height: 35px;",textInput("mpkk", "", value="N/A")),
        # div(style="height: 35px;",textInput("mpkk1q", "", value="N/A")),
        div(style="height: 35px;",numericInput("numcow_sis", "", 19000, min = 0, max = 1000000)),
        div(style="height: 35px;",numericInput("lwsis", "", 150, min = 10, max =1000 )),
        div(style="height: 35px;",numericInput("mysis", "", 0, min = 0, max = 5000)),
        numericInput("dsis", "",  0.5, min = 0, max = 1)), 
    
    
    box(status = "warning", width = 2, " ",background ="green",
        # div(style="height: 35px;",textInput("mpkk", "", value="N/A")),
        # div(style="height: 35px;",textInput("mpkk1q", "", value="N/A")),
        div(style="height: 35px;",numericInput("numcow_da", "", 10000, min = 0, max = 1000000)),
        div(style="height: 35px;",numericInput("lwda", "", 150, min = 10, max =1000 )),
        div(style="height: 35px;",numericInput("myda", "", 0, min = 0, max = 5000)),
        numericInput("dda", "",  0.4, min = 0, max = 1)), 
    
    
    box(status = "warning", width = 2, " ",background ="green",
        # div(style="height: 35px;",textInput("mpkk", "", value="N/A")),
        # div(style="height: 35px;",textInput("mpkk1q", "", value="N/A")),
        div(style="height: 35px;",numericInput("numcow_is", "", 500, min = 0, max = 1000000)),
        div(style="height: 35px;",numericInput("lwis", "", 210, min = 10, max =1000 )),
        div(style="height: 35px;",numericInput("myis", "", 2200, min = 0, max = 5000)),
        numericInput("dis", "",  0, min = 0, max = 1)), 
    
    
    box(status = "warning", width = 2, " ",background ="green",
        # div(style="height: 35px;",textInput("mpkk", "", value="N/A")),
        # div(style="height: 35px;",textInput("mpkk1q", "", value="N/A")),
        div(style="height: 35px;",numericInput("numsheep", "", 100000, min = 0, max = 1000000)),
        div(style="height: 35px;",numericInput("lwsh", "", 25, min = 10, max =1000 )),
        div(style="height: 35px;",numericInput("mysh", "", 0, min = 0, max = 5000)),
        numericInput("dsh", "",  0.4, min = 0, max = 1))
), 

fluidRow(
  box(status = "primary",width = 12, "Feed basket dry season",background = "blue")
  # box(status = "warning", width = 2, "",background = "blue"),
  # box(status = "warning", width = 2,"",background = "blue"),
  # box(status = "warning", width = 2, "",background = "blue"),
  # box(status = "warning", width = 2, "",background = "blue")
),
fluidRow(
    box(status = "warning", width = 2, "", background ="blue",
        div(style="height: 35px;",textInput("1Fb", "", value="Natural grass")),
        div(style="height: 35px;",textInput("2aFb", "",value="Cereal crop residue")), 
        div(style="height: 35px;",textInput("3aFb", "", value="Rice crop residue")),  
        div(style="height: 35px;",textInput("4aFb", "", value="Legume crop residue")),
        div(style="height: 35px;",textInput("5afb", "", value="Planted fodder")),
        div(style="height: 35px;",textInput("6afb", "", value="Concentrate - bran")),
        div(style="height: 35px;",textInput("7afb", "", value="Concentrate - oil seed cake")),
        div(style="height: 35px;",textInput("8afb", "", value="Hay")),
        textInput("9aFb", "", value="Silage")),
  box(status = "warning", width = 2, "",background ="blue",
      div(style="height: 35px;",numericInput("efng1", "", 40, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("efrc1", "", 5, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("efrr1", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("efrl1", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("efpf1", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("efconc1", "", 10, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("efconos1", "", 5, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("efhay1", "", 40, min = 0, max = 100)),
      numericInput("efsil1", "", 0, min = 0, max = 100)),
  
  box(status = "warning", width = 2, "",background ="blue",
      div(style="height: 35px;",numericInput("sfng1", "", 40, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sfrc1", "", 5, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sfrr1", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sfrl1", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sfpf1", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sfconc1", "", 10, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sfconos1", "", 5, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sfhay1", "", 40, min = 0, max = 100)),
      numericInput("sfsil1", "", 0, min = 0, max = 100)),
  
  box(status = "warning", width = 2, "",background ="blue",
      div(style="height: 35px;",numericInput("dafng1", "", 40, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("dafrc1", "", 10, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("dafrr1", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("dafrl1", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("dafpf1", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("dafconc1", "", 5, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("dafconos1", "", 5, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("dafhay1", "", 40, min = 0, max = 100)),
      numericInput("dafsil1", "", 0, min = 0, max = 100)),

  box(status = "warning", width = 2, "",background ="blue",
      div(style="height: 35px;",numericInput("ifng1", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("ifrc1", "", 10, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("ifrr1", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("ifrl1", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("ifpf1", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("ifconc1", "", 25, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("ifconos1", "", 25, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("ifhay1", "", 40, min = 0, max = 100)),
      numericInput("ifsil1", "", 0, min = 0, max = 100)), 

  box(status = "warning", width = 2, "",background ="blue",
      div(style="height: 35px;",numericInput("shfng1", "", 45, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("shfrc1", "", 10, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("shfrr1", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("shfrl1", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("shfpf1", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("shfconc1", "", 5, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("shfconos1", "", 5, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("shfhay1", "", 35, min = 0, max = 100)),
      numericInput("shfsil1", "", 0, min = 0, max = 100))
  ),
fluidRow(
  box(status = "primary",width = 12, "Feed basket wet season",background = "orange")
  # box(status = "warning", width = 2, "",background = "orange"),
  # box(status = "warning", width = 2,"",background = "orange"),
  # box(status = "warning", width = 2, "",background = "orange"),
  # box(status = "warning", width = 2, "",background = "orange")
),
fluidRow(
  box(status = "warning", width = 2, "", background ="orange",
      div(style="height: 35px;",textInput("1Fb", "", value="Natural grass")),
      div(style="height: 35px;",textInput("2aFb", "",value="Cereal crop residue")), 
      div(style="height: 35px;",textInput("3aFb", "", value="Rice crop residue")),  
      div(style="height: 35px;",textInput("4aFb", "", value="Legume crop residue")),
      div(style="height: 35px;",textInput("5afb", "", value="Planted fodder")),
      div(style="height: 35px;",textInput("6afb", "", value="Concentrate - bran")),
      div(style="height: 35px;",textInput("7afb", "", value="Concentrate - oil seed cake")),
      div(style="height: 35px;",textInput("8afb", "", value="Hay")),
      textInput("9aFb", "", value="Silage")),
  
  box(status = "warning", width = 2, "", background ="orange",
      div(style="height: 35px;",numericInput("efng2", "", 95, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("efrc2", "", 5, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("efrr2", "", 0, min = 0, max = 0)),
      div(style="height: 35px;",numericInput("efrl2", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("efpf2", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("efconc2", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("efconos2", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("efhay2", "", 0, min = 0, max = 100)),
      numericInput("efsil2", "", 0, min = 0, max = 100)),
  

  box(status = "warning", width = 2, "", background ="orange",
      div(style="height: 35px;",numericInput("sfng2", "", 95, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sfrc2", "", 5, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sfrr2", "", 0, min = 0, max = 0)),
      div(style="height: 35px;",numericInput("sfrl2", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sfpf2", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sfconc2", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sfconos2", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sfhay2", "", 0, min = 0, max = 100)),
      numericInput("sfsil2", "", 0, min = 0, max = 100)),
  
  box(status = "warning", width = 2, "", background ="orange",
      div(style="height: 35px;",numericInput("dafng2", "", 95, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("dafrc2", "", 5, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("dafrr2", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("dafrl2", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("dafpf2", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("dafconc2", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("dafconos2", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("dafhay2", "", 0, min = 0, max = 100)),
      numericInput("dafsil2", "", 0, min = 0, max = 100)),
  
  
  box(status = "warning", width = 2, "", background ="orange",
      div(style="height: 35px;",numericInput("ifng2", "", 20, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("ifrc2", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("ifrr2", "", 0, min = 0, max = 0)),
      div(style="height: 35px;",numericInput("ifrl2", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("ifpf2", "", 50, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("ifconc2", "", 20, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("ifconos2", "", 10, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("ifhay2", "", 0, min = 0, max = 100)),
      numericInput("ifsil2", "", 0, min = 0, max = 100)), 
  
 
  box(status = "warning", width = 2, "", background ="orange",
      div(style="height: 35px;",numericInput("shfng2", "", 95, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("shfrc2", "", 5, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("shfrr2", "", 0, min = 0, max = 0)),
      div(style="height: 35px;",numericInput("shfrl2", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("shfpf2", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("shfconc2", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("shfconos2", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("shfhay2", "", 0, min = 0, max = 100)),
      numericInput("shfsil2", "", 0, min = 0, max = 100))
  
  
  ),
fluidRow(
  box(status = "primary",width = 12, "Manure Management",background = "teal")
  # box(status = "warning", width = 2, "",background = "teal"),
  # box(status = "warning", width = 2,"",background = "teal"),
  # box(status = "warning", width = 2, "",background = "teal"),
  # box(status = "warning", width = 2, "",background = "teal")
),
fluidRow(
  box(status = "warning", width = 2, "", background ="teal",
      div(style="height: 35px;",textInput("1Fbm", "", value="% in lagoon")),
      div(style="height: 35px;",textInput("2aFbm", "",value="% as liquid slurry")), 
      div(style="height: 35px;",textInput("3aFbm", "", value="% as solid storage ")),  
      div(style="height: 35px;",textInput("4aFbm", "", value="% as drylot")),
      div(style="height: 35px;",textInput("5afm", "", value="% left on pasture")),
      div(style="height: 35px;",textInput("6afbm", "", value="% daily spread")),
      div(style="height: 35px;",textInput("7afbm", "", value="% in digester ")),
      div(style="height: 35px;",textInput("8afbm", "", value="% used as fuel")),
      textInput("9aFbm", "", value="% other management")),
  box(status = "warning", width = 2, "", background ="teal",
      div(style="height: 35px;",numericInput("es_lagoon_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("es_liquidslurry_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("es_solidstorage_perc", "", 45, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("es_drylot_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("es_pasture_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("es_dailyspread_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("es_digester_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("es_fuel_perc", "", 55, min = 0, max = 100)),
      numericInput("es_other_perc", "", 0, min = 0, max = 100)),
  box(status = "warning", width = 2, "", background ="teal",
      div(style="height: 35px;",numericInput("sis_lagoon_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sis_liquidslurry_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sis_solidstorage_perc", "", 70, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sis_drylot_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sis_pasture_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sis_dailyspread_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sis_digester_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sis_fuel_perc", "", 30, min = 0, max = 100)),
      numericInput("sis_other_perc", "", 0, min = 0, max = 100)),
  box(status = "warning", width = 2, "", background ="teal",
      div(style="height: 35px;",numericInput("da_lagoon_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("da_liquidslurry_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("da_solidstorage_perc", "", 45, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("da_drylot_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("da_pasture_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("da_dailyspread_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("da_digester_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("da_fuel_perc", "", 55, min = 0, max = 100)),
      numericInput("da_other_perc", "", 0, min = 0, max = 100)),
  box(status = "warning", width = 2, "", background ="teal",
      div(style="height: 35px;",numericInput("is_lagoon_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("is_liquidslurry_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("is_solidstorage_perc", "", 75, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("is_drylot_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("is_pasture_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("is_dailyspread_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("is_digester_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("is_fuel_perc", "", 25, min = 0, max = 100)),
      numericInput("is_other_perc", "", 0, min = 0, max = 100)),
  box(status = "warning", width = 2, "", background ="teal",
      div(style="height: 35px;",numericInput("sh_lagoon_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sh_liquidslurry_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sh_solidstorage_perc", "", 40, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sh_drylot_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sh_pasture_perc", "", 40, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sh_dailyspread_perc", "", 18, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sh_digester_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sh_fuel_perc", "", 2, min = 0, max = 100)),
      numericInput("sh_other_perc", "", 0, min = 0, max = 100))  
  
  
),

fluidRow(
  box(status = "primary",width = 12, "SEASONALITY DEFINITION",background = "navy")
),

# fluidRow(
#   box(status = "primary",width = 10, "Crop seasonality",background = "green"),
#   box(status = "primary",width = 2, "Variable",background = "green")
# ),

fluidRow(
  box(status = "primary", width = 10, "", background ="green",
      div(style="height: 35px;",textInput("1q", "", value="number of dry season months")),
      textInput("2q", "", value="number of wet season months ")),
  box(status = "primary", width = 2, "", background ="green",
      div(style="height: 35px;",numericInput("ds", "", 8, min = 1, max = 11)),
      numericInput("ws", "", 4, min = 1, max = 11))
),

# fluidRow(
#   box(status = "primary",width = 10, "Combination of seasonality ",background = "green"),
#   box(status = "primary",width = 2, "Variable",background = "green")
# ),
# 
# fluidRow(
#   box(status = "primary", width = 10, "", background ="green",
#       div(style="height: 35px;",textInput("1m", "", value="number of month the transhumant troupeau is in area during the wet season")),
#       textInput("2m", "", value="number of month the transhumant troupeau is in area during the dry season ")),
#   box(status = "primary", width = 2, "", background ="green",
#       div(style="height: 35px;",numericInput("ws_st1", "", 3, min = 1, max = 12)),
#       numericInput("ds_st1", "", 2, min = 1, max = 12))
# ),

fluidRow(
  box(status = "primary",width = 12, "CROP AND SOIL CHARACTERISTIC DEFINITION",background = "navy")
),

fluidRow(
  box(status = "primary",width = 12, "Manure production ",background = "blue")
),

fluidRow(
  box(status = "warning", width = 10, "", background ="blue",
      div(style="height: 35px;",textInput("1n", "", value="manure production local dairy cattle per day")),
      div(style="height: 35px;",textInput("2n", "", value="manure production from a local fattening/rearing cattle per day")),
      div(style="height: 35px;",textInput("3n", "", value="manure production from a draft cattle per day")),
      div(style="height: 35px;",textInput("3n", "", value="manure production from a cross-breed dairy cattle per day")),
      textInput("4n", "", value="manure production from a sheep per day")),
  
  box(status = "warning", width = 2, "", background ="blue",
      div(style="height: 35px;",numericInput("mprod_es", "", 2, min = 0, max = 20)),
      div(style="height: 35px;",numericInput("mprod_sis", "", 3, min = 0, max = 20)),
      div(style="height: 35px;",numericInput("mprod_da", "", 2, min = 0, max = 20)),
      div(style="height: 35px;",numericInput("mprod_is", "", 4, min = 0, max = 20)),
      
      numericInput("mprod_sh", "", 0.1, min = 0, max = 20))
),

fluidRow(
  box(status = "primary",width = 12, "Stored manure, slurry and fertilizer application to different crop",background = "teal")
),
fluidRow(
  box(status = "warning", width = 10, "", background ="teal",
      div(style="height: 35px;",textInput("1ssf", "", value="percent of stored manure applied to cereals")),
      div(style="height: 35px;",textInput("2ssf", "", value="percent of stored manure applied to legume ")),
      div(style="height: 35px;",textInput("3ssf", "", value="percent of stored manure applied to planted fodder")),
      div(style="height: 35px;",textInput("4ssf", "", value="percent of stored manure applied to rice ")),
      div(style="height: 35px;",textInput("5ssf", "", value="percent of stored manure applied to grazing land  ")),
      div(style="height: 35px;",textInput("6ssf", "", value="percent of stored slurry applied to cereals")),
      div(style="height: 35px;",textInput("7ssf", "", value="percent of stored slurry applied to legume ")),
      div(style="height: 35px;",textInput("8ssf", "", value="percent of stored slurry applied to planted fodder")),
      div(style="height: 35px;",textInput("9ssf", "", value="percent of stored slurry applied to rice ")),
      div(style="height: 35px;",textInput("10ssf", "", value="percent of stored slurry applied to grazing land ")),
      div(style="height: 35px;",textInput("11ssf", "", value="Fertilizer application kg/ha applied to cereals ")),
      div(style="height: 35px;",textInput("12ssf", "", value="Fertilizer application kg/ha applied to legume ")),
      div(style="height: 35px;",textInput("13ssf", "", value="Fertilizer application kg/ha applied to planted fodder")),
      div(style="height: 35px;",textInput("14ssf", "", value="Fertilizer application kg/ha applied to rice ")),
      textInput("15ssf", "", value="Fertilizer application kg/ha applied to grazing land")),
  
  box(status = "warning", width = 2, "", background ="teal",
      div(style="height: 35px;",numericInput("manc", "", 0.6, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("manl", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("manpf", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("manr", "",0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("mangraz", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sluc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("slul", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("slupf", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("slur", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("slugraz", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("fertc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("fertl", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("fertpf", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("fertr", "", 0, min = 0, max = 100)),
      numericInput("fertgraz", "", 0, min = 0, max = 100))
),

fluidRow(
  box(status = "primary",width = 12, "Crop yield and tillage ",background = "orange")
),
fluidRow(
  box(status = "warning", width = 10, "", background ="orange",
      div(style="height: 35px;",textInput("1cyt", "", value="exogenous yield productivity gain in percentage of cereal  yield ")),
      div(style="height: 35px;",textInput("2cyt", "", value="exogenous yield productivity gain in percentage of legume yield")),
      div(style="height: 35px;",textInput("3cyt", "", value="exogenous yield productivity gain in percentage of planted  yield")),
      div(style="height: 35px;",textInput("4cyt", "", value="exogenous yield productivity gain in percentage of grassland yield ")),
      div(style="height: 35px;",textInput("5cyt", "", value="exogenous yield productivity gain in percentage of rice yield")),
      div(style="height: 35px;",textInput("6cyt", "", value="percentage of cropland that is tilled ")),
      div(style="height: 35px;",textInput("7cyt", "", value="percentage of cropland that is on reduced till ")),
      div(style="height: 35px;",textInput("8cyt", "", value="percentage of cropland that is on no till ")),
      div(style="height: 35px;",textInput("9cyt", "", value="percentage of land with low input")),
      div(style="height: 35px;",textInput("10cyt", "", value="percentage of land with medium input")),
      div(style="height: 35px;",textInput("11cyt", "", value="percentage of land with high input no manure")),
      textInput("12cyt", "", value="percentage of land with high input with manure")),
  
  box(status = "warning", width = 2, "", background ="orange",
      div(style="height: 35px;",numericInput("pgc", "", 0.2, min = 0, max = 1)),
      div(style="height: 35px;",numericInput("pgl", "", 0.0, min = 0, max = 1)),
      div(style="height: 35px;",numericInput("pgpf", "", 0.0, min = 0, max = 1)),
      div(style="height: 35px;",numericInput("pgg", "", 0, min = 0, max = 1)),
      div(style="height: 35px;",numericInput("pgr", "", 0.0, min = 0, max = 1)),
      div(style="height: 35px;",numericInput("perc_til", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("perc_redtil", "", 100, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("perc_notil", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("perc_inlow", "", 100, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("perc_inmedium", "",0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("perc_inhighnoman", "", 0, min = 0, max = 100)),
      numericInput("perc_inhighman", "", 0, min = 0, max = 100))
),

# fluidRow(
#   box(status = "primary",width = 12, "Switches ",background = "light-blue")
# 
# ),
# fluidRow(
#   box(status = "warning",width = 4, "Objective",background ="red"),
#   box(status = "warning", width = 4, "Name",background ="maroon"),
#   box(status = "warning", width = 4,"Activates a loop that",background ="maroon")
#   
# ),


# fluidRow(
#   box(status = "warning", title=" Objective",width = 4, "", background ="orange",
#       textInput("tx2", "", value="Activates land use change")),
#      
#   
#   box(status = "warning", title=" Name", width = 4, "", background ="orange",
#       selectInput("lucc", "", choices=c("No.Change","LUC1","LUC2","LUC3","LUC4"))
#    
#   ),
#   box(status = "warning", title="Activates a loop that",width = 4, "", background ="orange",
# textInput("tx2", "", value="Re-computes land cover using the raster converted to cropland ")
#       
#   ) 
#   
# ),

fluidRow(
  box(width=12, " Baseline Productivity",plotOutput("baseplot")))

)




)

)



# We'll save it in a variable `ui` so that we can preview it in the console
ui <- dashboardPage(
  # dashboardHeader(title = "SAIRLA PROJECT: CLEANED APPLICATION SYSTEM-ADVANCED USERS ",titleWidth=750,
  #                 
  #                 tags$li(class = "dropdown",
  #                         tags$a(href="https://www.ilri.org", target="_blank", 
  #                                tags$img(height = "30px", alt="SNAP Logo", src="https://www.ilri.org/sites/default/files/styles/card_image_thumb/public/default_images/defautl_img.jpg")
  #                         ))),     
  
  
  dashboardHeader(title = "SAIRLA PROJECT : CLEANED INTERFACE SYSTEM FOR ATSBI ETHIOPIA-ADVANCED USERS",titleWidth=1000,
                  
                  # tags$li(class = "dropdown",
                  #         tags$a(href="https://www.ilri.org", target="_blank", 
                  #                tags$img(height = "30px", alt="SNAP Logo", src="https://www.ilri.org/sites/default/files/styles/card_image_thumb/public/default_images/defautl_img.jpg")
                  #        ))),     
                  
                  
                  tags$li(class = "dropdown",
                          tags$a(href="www", target="_blank", 
                                 tags$img(height = "60px", alt="SNAP Logo", src="crpaaseia.jpg")
                          ))
  ),
  
  
  
  dashboardSidebar(width=150,
                   sidebarMenu(
                     menuItem("ADVANCED USERS", tabName = "dashboard1", icon = icon("dashboard"))
                     #menuItem("SIMPLE USERS", tabName = "widgets1", icon = icon("th"))
                     # menuItem("Vegetation mapping", tabName = "Veg", icon = icon("th"))
                   )),
  
  
  body, skin='yellow'
  
  
  
)






# Preview the UI in the console
#source("D:/MOSEVICTOR_PC/MOSE_E/ILRI_SHINY_APP2017/CLEANED_BURKINAFASO2017/CLEANED-Burkina/3-cleaned/Update_user_interface.R", local=TRUE)

server = function(input, output, session) {
 
  observeEvent(input$showSidebar, {
    shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
  })
  observeEvent(input$hideSidebar, {
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  })
  
  
  
  
  
  
  #eventReactive(input$go,{})
  # observeEvent(input$st1, {
  # 
  #   x <- input$st1
  #   
  #   updateNumericInput(session, "st2", value = 12-x)})
  # 
  # observeEvent(input$st2, {
  #   
  #   x3 <- input$st2
  #   
  #   updateNumericInput(session, "st1", value = 12-x3)})
  # 
  # 
  # 
  # observeEvent(input$ds, {
  # 
  #   x1 <- input$ds
  #   
  #   updateNumericInput(session, "ws", value = 12-x1)})
  # 
  # 
  # observeEvent(input$ws, {
  #   
  #   x1d <- input$ws
  #   
  #   updateNumericInput(session, "ds", value = 12-x1d)})
  # 
  # 
  # 
   pok<- eventReactive(input$go, {

     if(input$go==0)
       return()


     
     
     #clearing all memory 
         #set path to cleaned tool
     path<<-'D:/Dropbox/Cleaned Ethiopia'
     ###################################This sheet defines all user defined variables#################
     
     
     
     
     
     setwd(path)
     
     # enter the number of animals per system 
     #the dual purpose dairy cow 
     numcow_es<<-input$numcow_es # see parameterization excel file 
     # the dual puropse fattening and rearing 
     numcow_sis <<- input$numcow_sis
     # dual system : draft animals 
     numcow_da <<- input$numcow_da
     
     # the specialized dairy with improved breeds 
     numcow_is <<-input$numcow_is
     
     # sheep 
     numsheep <<- input$numsheep
     
     
     preset <<-0 #if 1 the interface uses the presets if 0 or other it uses the manual input below. 
     scenario<-read.csv('1-input/parameter/preset.csv',  skip=2)
     
     # select from the preset scenario
     
     DD<<- 'DD0'  #options are (DD0, DD1,DD2)
     DF<<- 'DF0'  #options are (DF0, DF1, DF2)
     DA<<- 'DA0'  #options are (DA0, DA1, DA2)
     SD <<- 'SD0'  #options are (SD0, SD1, SD2)
     SH<<- "SH0" # options are (SH0, SH1 and SH2)
     
     Cr<<- 'Cr0' #options are (Cr0,Cr1)
     
     #give your scanario a name 
     name<<-input$name
     
     #############################manual defintion of the parameters#####################
     
     
     #seasonality 
     # climate seasonality # based on usaid livelihood zones 
     ds<<- input$ds # dry season
     ws<<- input$ws # wet season  
     
     #DEFINE THE DIFFERENT SYSTEM 
     #################################
     
     # the dual system dairy
     #define liveweigtht in kg for the the breed in the extensive system (200)
     lwes<<-input$lwes
     #define milk yield (kg/cow/year) for the breed in the extensive system (400) #500 for health
     myes<<-input$myes # 5 litre a day for 220 days 
     
     des<<-input$des
     dsis<<-input$dsis
     dis<<-input$dis
     dda<<-input$dda
     dsh<<-input$dsh
     
     #dry season 
     #natural grass in percent (51)
     efng1<<- input$efng1
     #crop residues cerals (49)
     efrc1<<- input$efrc1
     #crop residue legumes (0) 
     efrl1<<-input$efrl1
     #crop residue rice (0) 
     efrr1<<-input$efrr1
     #planted fodder (0)
     efpf1<<-input$efpf1
     # concentrates cereal (maize bran) (0)
     efconc1<<- input$efconc1
     # concentrates oilseed cake (0)
     efconos1<<- input$efconos1
     #hey()
     efhay1<<-input$efhay1
     # silage made from grass
     efsil1<<-input$efsil1
     
     #wet season  
     #natural grass in percent (98)
     efng2<<- input$efng2  
     #crop residues cerals (2)
     efrc2<<- input$efrc2 
     #crop residues rice (2)
     efrr2<<- input$efrr2 
     #crop residue legumes (0) 
     efrl2<<-input$efrl2
     #planted fodder (0)
     efpf2<<-input$efpf2  #
     # concentrates cereal (maize bran) (0)
     efconc2<<- input$efconc2
     # concentrates oilseed cake (0)
     efconos2<<- input$efconos2
     #hey()
     efhay2<<-input$efhay2
     # silage made from grass
     efsil2<<-input$efsil2
     #check if a 100%
     
     
     # manure management in the extensive system  system in percent (100%)
     #(if ipcc=1, then no need to adjust this )
     es_lagoon_perc<<- input$es_lagoon_perc
     es_liquidslurry_perc<<-input$es_liquidslurry_perc
     es_solidstorage_perc<<-input$es_solidstorage_perc
     es_drylot_perc<<-input$es_drylot_perc
     es_pasture_perc<<-input$es_pasture_perc
     es_dailyspread_perc<<-input$es_dailyspread_perc
     es_digester_perc<<-input$es_digester_perc
     es_fuel_perc<<-input$es_fuel_perc
     es_other_perc<<-input$es_other_perc
     
     # dual system meat 
     
     #define liveweigtht in kg for the the breed in the dual purpose system  (200)
     lwsis<<-input$lwsis  
     #define milk yield (kg/cow/year) for the breed in the semi intensive system (2000) 
     mysis<<-input$mysis 
     
     #feed basket  for dual systeme breed
     #dressing percentage 
     dsis <<-input$dsis 
     
     #feed basket  semi-intensive  system season dry
     #natural grass in percent (33)
     sfng1<<-input$sfng1 
     #crop residues cereals (35)
     sfrc1<<-input$sfrc1 
     #crop residue from rice
     sfrr1<<- input$sfrr1 
     #crop residue legumes (12)
     sfrl1<<-input$sfrl1 
     #planted fodder (10)
     sfpf1<<-input$sfpf1 
     # concentrates cereal (maize bran) (5)
     sfconc1<<- input$sfconc1 
     # concentrates oilseed cake (5)
     sfconos1<<- input$sfconos1 
     #hey()
     sfhay1<<-input$sfhay1 
     # silage made from grass
     sfsil1<<- input$sfsil1 
     
     
     
     # wet season 
     #natural grass in percent (57)
     sfng2<<-input$sfng2 
     #crop residues cereals (10)
     sfrc2<<-input$sfrc2 
     #crop residue from rice (0)
     sfrr2<<-input$sfrr2 
     #crop residue legumes (5) 
     sfrl2<<-input$sfrl2 
     #planted fodder (14)
     sfpf2<<-input$sfpf2 
     # concentrates cereal (maize bran) (5)
     sfconc2<<- input$sfconc2 
     # concentrates oilseed cake (9)
     sfconos2<<- input$sfconos2 
     #hey()
     sfhay2<<-input$sfhay2 
     # silage made from grass
     sfsil2<<-input$sfsil2 
     
     # manure management in the semi-intensive system in percent (100%)
     #(if ipcc=1, then no need to adjust this )
     sis_lagoon_perc<<- input$sis_lagoon_perc 
     sis_liquidslurry_perc<<-input$sis_liquidslurry_perc 
     sis_solidstorage_perc<<-input$sis_solidstorage_perc 
     sis_drylot_perc<<-input$sis_drylot_perc 
     sis_pasture_perc<<-input$sis_pasture_perc 
     sis_dailyspread_perc<<-input$sis_dailyspread_perc 
     sis_digester_perc<<-input$sis_digester_perc 
     sis_fuel_perc<<-input$sis_fuel_perc 
     sis_other_perc<<-input$sis_other_perc 
     

     
     ## draft system 
     
     #define liveweigtht in kg for the the breed as draft animals (220)
     lwda<<-input$lwda
     
     #define milk yield (kg/cow/year) fro the draft animal (1000)
     myda<<-input$myda 
     # dressing percenatge
     dda<<- input$dda 
     
     #feed basket for dairy wet season
     #natural grass in percent (15)
     dafng1<<-input$dafng1
     #crop residues cerals (40)
     dafrc1<<-input$dafrc1
     #crop residue from rice
     dafrr1<<-input$dafrr1
     #crop residue legumes (30)
     dafrl1<<-input$dafrl1
     #planted fodder ()
     dafpf1<<-input$dafpf1
     # concentrates cereal (maize bran) (5)
     dafconc1<<- input$dafconc1
     # concentrates oilseed cake (10)
     dafconos1<<- input$dafconos1
     #hey()
     dafhay1<<-input$dafhay1
     # silage made from grass
     dafsil1<<-input$dafsil1
     
     
     
     
     #feed basket for dairy dry season
     #natural grass in percent (60)
     dafng2<<-input$dafng2
     #crop residues cerals (0)
     dafrc2<<-input$dafrc2
     #crop residue from rice
     dafrr2<<- input$dafrr2 
     #crop residue legumes (10)
     dafrl2<<-input$dafrl2
     #planted fodder (10)
     dafpf2<<-input$dafpf2
     # concentrates cereal (maize bran) (20)
     dafconc2<<- input$dafconc2
     # concentrates oilseed cake (10)
     dafconos2<<- input$dafconos2
     #hey()
     dafhay2<<-input$dafhay2
     # silage made 
     dafsil2<<-input$dafsil2
     
     # manure management in the semi-intensive system in percent (100%)
     #(if ipcc=1, then no need to adjust this )
     da_lagoon_perc<<- input$da_lagoon_perc
     da_liquidslurry_perc<<-input$da_liquidslurry_perc
     da_solidstorage_perc<<-input$da_solidstorage_perc
     da_drylot_perc<<-input$da_drylot_perc
     da_pasture_perc<<-input$da_pasture_perc
     da_dailyspread_perc<<-input$da_dailyspread_perc
     da_digester_perc<<-input$da_digester_perc
     da_fuel_perc<<-input$da_fuel_perc
     da_other_perc<<-input$da_other_perc
     
     
     
     ################################################### specialized dairy system 
     #define liveweigtht in kg for the the breed in the intensive system (300)
     lwis<<-input$lwis
     
     #define milk yield (kg/cow/year) for the breed in the intensive system (7500)
     myis<<-input$myis
     
     #feed basket for intensive system
     #dry season
     #natural grass in percent (20)
     ifng1<<-input$ifng1
     #crop residues cerals (30)
     ifrc1<<-input$ifrc1
     #crop residue from rice (0)
     ifrr1<<-input$ifrr1
     #crop residue legumes (10)
     ifrl1<<-input$ifrl1
     #planted fodder (30)
     ifpf1<<-input$ifpf1
     # concentrates cereal (maize bran) (10)
     ifconc1<<- input$ifconc1
     # concentrates oilseed cake (10)
     ifconos1<<- input$ifconos1
     #hey()
     ifhay1<<-input$ifhay1
     # silage made from grass
     ifsil1<<-input$ifsil1
     
     
     
     #feed basket for dairy wet season
     #natural grass in percent (30)
     ifng2<<-input$ifng2
     #crop residues cerals (10)
     ifrc2<<-input$ifrc2
     #crop residue from rice (0)
     ifrr2<<-  input$ifrc2
     #crop residue legumes (5)
     ifrl2<<-input$ifrl2
     #planted fodder (35)
     ifpf2<<-input$ifpf2
     # concentrates cereal (maize bran) (10)
     ifconc2<<- input$ifconc2
     # concentrates oilseed cake (10)
     ifconos2<<- input$ifconos2
     #hey()
     ifhay2<<-input$ifhay2
     # silage made from grass
     ifsil2<<-input$ifsil2
     
     # manure management in the semi-intensive system in percent (100%)
     #(if ipcc=1, then no need to adjust this )
     is_lagoon_perc<<- input$is_lagoon_perc
     is_liquidslurry_perc<<-input$is_liquidslurry_perc
     is_solidstorage_perc<<-input$is_solidstorage_perc
     is_drylot_perc<<-input$is_drylot_perc
     is_pasture_perc<<-input$is_pasture_perc
     is_dailyspread_perc<<-input$is_dailyspread_perc
     is_digester_perc<<-input$is_digester_perc
     is_fuel_perc<<-input$is_fuel_perc
     is_other_perc<<-input$is_other_perc
     
     
     ####### sheep 
     #define liveweigtht in kg for the the breed as draft animals (20)
     lwsh<<-input$lwsh
     
     #define dressing persentage
     dsh <<-input$dsh
     
     #feed basket for dairy wet season
     #natural grass in percent (15)
     shfng1<<-input$shfng1
     #crop residues cerals (40)
     shfrc1<<-input$shfrc1
     #crop residue from rice
     shfrr1<<-input$shfrr1
     #crop residue legumes (30)
     shfrl1<<-input$shfrl1
     #planted fodder ()
     shfpf1<<-input$shfpf1
     # concentrates cereal (maize bran) (5)
     shfconc1<<- input$shfconc1
     # concentrates oilseed cake (10)
     shfconos1<<- input$shfconos1
     #hey()
     shfhay1<<-input$shfhay1
     # silage made from grass
     shfsil1<<-input$shfsil1
     
     
     #feed basket for sheep dry season
     #natural grass in percent (60)
     shfng2<<-input$shfng2
     #crop residues cerals (0)
     shfrc2<<-input$shfrc2
     #crop residue from rice
     shfrr2<<- input$shfrr2
     #crop residue legumes (10)
     shfrl2<<-input$shfrl2
     #planted fodder (10)
     shfpf2<<-input$shfpf2
     # concentrates cereal (maize bran) (20)
     shfconc2<<- input$shfconc2
     # concentrates oilseed cake (10)
     shfconos2<<- input$shfconos2
     #hey()
     shfhay2<<-input$shfhay2
     # silage made 
     shfsil2<<-input$shfsil2
     
     sh_pasture_perc<<-input$sh_pasture_perc
     
     
     
     #######################################################################################
     #global variable definition
     
     #ipcc= 1 the code will use ipcc tier 2 standards for manure storage in stead of the user defined one
     ipcc<<-0
     
     
     #exogenous yield productivity gain in percentage of yield
     #crop
     pgc<<- input$pgc
     #legumes
     pgl<<-input$pgl
     #planted fodder
     pgpf<<-input$pgpf
     #grassland
     pgg<<- input$pgg
     
     
     
     #linking the manure availability to the production system 
     mprod_es<<- input$mprod_es #manure production from a cow in the dual system - rearing/fattening and draf per day
     mprod_da<<-input$mprod_da #manure production from a cow in the dual system - rearing/fattening and draf per day
     mprod_sis<<-input$mprod_sis #manure production from a cow from in the dual system lactating animals per day
     mprod_is<<-input$mprod_is  #manure production from a cow in the specialized dairy per day
     mprod_sh<<-input$mprod_sh
     
     #percent of stored manure applied to the different crop
     #cereal (mprod_c *% to this crop for linking with production )
     manc<<-input$manc
     # legumes  (mprod_c *% to this crop for linking with production )
     manl<<-input$manl
     #planted fodder  (mprod_c *% to this crop for linking with production )
     manpf<<-input$manpf
     #rice  (mprod_r *% to this crop for linking with production )
     manr<<-input$manr 
     #grazing land  (mprod_r *% to this crop for linking with production )
     mangraz<<-input$mangraz 
     
     # application of slurry kg/ha
     #cereal ()
     sluc<<-input$sluc
     # legumes (0)
     slul<<-input$slul
     #planted fodder ()
     slupf<<-input$slupf
     #grazing land
     slugraz<<-input$slugraz
     #rice land
     slur<<-input$slur
     
     
     slurryconv<<-input$slurryconv #conversion rate between slurry (NPK) and Nitrogen
     #we need a source here What about compost and other manure. 
     
     #inorganic fertilizer application in kg per hectare
     
     #cereal (50 is recommended)
     fertc<<-input$fertc
     #rice (50 is recommended)
     fertr<<-input$fertr
     # legumes (0)
     fertl<<-input$fertl
     #planted fodder 
     fertpf<<-input$fertpf
     #grazing land
     fertgraz<<-input$fertgraz
     
     Fertconv<<-input$Fertconv #conversion rate between fertilizer (NPK) and Nitrogen, depends on the locally available ferilizer, +/- 20%
     # from impact lit we know that DAP is most commonly used - Joanne is looking for conversion rates
     
     
     #exogenous yield productivity gain in percentage of yield
     #crop
     pgc<<-input$pgc
     #legumes
     pgl<<-input$pgl
     #planted fodder
     pgpf<<-input$pgpf
     #grassland
     pgg<<-input$pgg
     
     # rice 
     pgr<<-input$pgr
     
     #############soil management option on cropland (ghg)
     perc_til<<-input$perc_til #percentage of cropland that is tilled 
     perc_redtil<<-input$perc_redtil #percentage of cropland that is on reduced till
     perc_notil<<-input$perc_notil #percentage of cropland that is on no till
     
     perc_inlow<<-input$perc_inlow  #percentage of land with low input 
     perc_inmedium<<-input$perc_inmedium  #percentage of land with medium input 
     perc_inhighnoman<<-input$perc_inhighnoman  #percentage of land with high input no manure 
     perc_inhighman<<-input$perc_inhighman   #percentage of land with high input with manure
     
     
     #####reading some r info
     setwd(path)
     pixel<-read.csv('1-input/parameter/pixel.csv')
     pixel<-pixel[2]
     
     
     source('2-feedbasket_nonlinear2.r')

   
     paste("Productivity parameters updated for this session.")
   })
    

output$plot <- renderText(
    {     withProgress(pok(), message="Please wait: Updating productivity module.")
      
      pok()
      
    
      })
  
  pok1<- eventReactive(input$gowt, {
      setwd(path)
      
    if(input$gowt==0)
      return()
    
    source('1-water.R')})
  
  output$plot12 <- renderPlot(
    { 
      withProgress(pok1(), message="Please wait: Running Water Impact.")
      
      pok1()
      
      
    })
  
  pok1t<- eventReactive(input$gowt, {
    
    if(input$gowt==0)
      return()
    
    grid.table(water_ind2)
    #data.frame(rbind(water_ind,water_ind_diff))
  
     })
    
  output$plotable<- renderPlot(
  
   {  if(input$gowt==0)
     return()
     
     pok1t()
    
  
  })
  
  
  pok1gh<- eventReactive(input$gogh, {
    
    if(input$gogh==0)
      return()
      setwd(path)
    source('1-ghg.r')})
  
  output$plot123 <- renderPlot(
    { 
      
      withProgress(pok1gh(), message="Please wait: Running Greenhouse Gas Impact." )
      
      pok1gh()
      
      
    })
  
  pok1tgh<- eventReactive(input$gogh, {
    
    if(input$gogh==0)
      return()
    grid.table(Coe_ind2)
    #data.frame(rbind(Coe_ind,Coe_ind_diff))
  })
  
  output$plotableghg <- renderPlot(
    { pok1tgh()
      
      
    })
  
  # pok1bdiv<- eventReactive(input$gobdiv, {
  #   
  #   if(input$gobdiv==0)
  #     return()
  #     setwd(path)
  #     
  #   source('1-biodiv.r')})
  # 
  # output$plot12bid <- renderPlot(
  #   { 
  #     withProgress(pok1bdiv(), message="Please wait: Running Biodiversity Impact" )
  #     
  #     pok1bdiv()
  #     
  #     
  #   })
  # 
  # pok1tbiv<- eventReactive(input$gobdiv, {
  #   
  #   if(input$gobdiv==0)
  #     return()
  #   
  #   grid.table(bio_ind2)
  #   
  #   #data.frame(rbind(bio_ind,bio_ind_diff))
  # })
  # 
  # output$plotablediv <- renderPlot(
  #   { pok1tbiv()
  #     
  #     
  #   })
  
  psoil<- eventReactive(input$gosol, {
    
    if(input$gosol==0)
      return()
      setwd(path)
      
    source('1-soil.r')})
  

  
  
  output$plotsoil <- renderPlot(
    { 
      
      withProgress(psoil(), message="Please wait: Running Soil Impact.")
      
      psoil()
      
    } ,height = 800, width = 800)  
  
  
  
  
  
  
    # output$plotsoil <- renderPlot(
    # { psoil()
    #   
    #   
    # })
  
  ptsoil<- eventReactive(input$gosol, {
    
    if(input$gosol==0)
      return()
    grid.table(soil_ind2)
    #data.frame(rbind(soil_ind,soil_ind_diff))
  })
  
  output$plotablesoil <- renderPlot(
    { ptsoil()
      
      
    })
  ptbaseprod<- eventReactive(input$go, {
    
    if(input$go==0)
      return()
    
    grid.table(prod_ind2)
    
  })
  
  output$baseplot <- renderPlot(
    { ptbaseprod()
      
      
    })
  
  
}
shinyApp(ui = ui,server)

