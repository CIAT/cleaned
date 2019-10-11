####################################################################################################
#
# Shiny App for CLEANED System ILRI  August 2018 by Victor N. Mose and Caroline Mburu\
# Adjusted user interface for Tanzania, Lushoto, ReSLess by Catherine Pfeifer August 2019
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
path<<-'D:/Dropbox/Cleaned Tanzania'
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
                        tabPanel(title = "Productivity", value = "panel1", ""),
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
                                 fluidRow(box(width=12, title = "Summary table green house Impact",
                                              #actionButton("gowt", " "),
                                              # numericInput("n", "n", 50),
                                              plotOutput("plotableghg"))
                                 )),
                        tabPanel(title = "Biodiversity Impact", value = "panel4", 
                                 fluidRow(box(width=12, background ="light-blue",img(src="biodiversityimpact.JPG", width=650))),
                                 fluidRow(box(width=12, title = " ",
                                              actionButton("gobdiv", "Run biodivesity Impact"),
                                              # numericInput("n", "n", 50),
                                              plotOutput("plot12bid"))
                                          
                                 ),
                                 
                                 fluidRow(box(width=12, title = "Summary table biodiversity Impact",
                                              #actionButton("gowt", " "),
                                              # numericInput("n", "n", 50),
                                              plotOutput("plotablediv"))
                                 )),
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
    box(status = "warning", title="Click here to update !", width = 12, "", background ="red",
        div(style="height: 35px;", actionButton("go", "run")))),
  
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
  # box(status = "warning", width = 2, "PASTORAL TRANSHUMANT HERD",background ="maroon"),
  # box(status = "warning", width = 2,"PASTORAL DAIRY HERD",background ="maroon"),
  box(status = "warning", width = 2, "LOCAL BREED ANIMALS",background ="maroon"),
  box(status = "warning", width = 2, "CROSSBREED ANIMAL",background ="maroon"),
  box(status = "warning", width = 2, "ALMOST PURE BREED ANIMALS",background ="maroon")),
  


fluidRow(
  box(status = "warning", width = 2, "", background ="green",
      # div(style="height: 35px;",textInput("1a", "", value="Number of troupeaux(herd)")),
      # div(style="height: 35px;",textInput("2a", "",value="Number of animals per troup")),
      div(style="height: 35px;",textInput("3a", "",value="Number of Animals")),
      div(style="height: 35px;",textInput("4a", "", value="Alive weight(kg)")),
      div(style="height: 35px;",textInput("5a", "", value="Milk production(kg/cow/year)")),
      textInput("6a", "", value="Dressing percentage")),
  
  # box(status = "warning", width = 1, "",background ="green",
  #     div(style="height: 35px;",numericInput("tt", "LongT", 100, min = 60, max =1000000)),
  #     div(style="height: 35px;",numericInput("att", "", 100, min = 60, max = 1000)),
  #     div(style="height: 35px;",textInput("**", "", value="**")),
  #     div(style="height: 35px;",numericInput("lwes", "", 200, min = 100, max = 1000)),
  #     div(style="height: 35px;",textInput("mp", "", value="N/A")),
  #     #textInput("**r", "", value="**")),
  #     numericInput("des", "", 0.38, min = 0, max = 100)),
  
  
  
  
  # box(status = "warning", width = 1, "",background ="green",
  #     div(style="height: 35px;",numericInput("tpt", "ShortT", 238, min = 60, max =1000000)),
  #     div(style="height: 35px;",numericInput("atpt", "", 120, min = 60, max = 1000)),
  #     div(style="height: 35px;",textInput("**", "", value="**")),
  #     div(style="height: 35px;",textInput("**1", "", value="**")),
  #     div(style="height: 35px;",textInput("mpa", "", value="N/A")),
  #     textInput("**r1", "", value="**")),
  # numericInput("des", "", 0.38, min = 0, max = 100)),
  
  
  
  
  
  
  # box(status = "warning", width = 2, "",background ="green",
  #     div(style="height: 35px;",numericInput("tt", "", 100, min = 60, max =1000000)),
  #     div(style="height: 35px;",numericInput("att", "", 100, min = 60, max = 1000)),
  #     div(style="height: 35px;",textInput("**", "", value="**")),
  #     div(style="height: 35px;",numericInput("lwes", "", 200, min = 100, max = 1000)),
  #     div(style="height: 35px;",textInput("mp", "", value="N/A")),
       #numericInput("des", "", 0.38, min = 0, max = 100)),
  
  box(status = "warning", width = 2,"",background ="green",
      
      #div(style="height: 35px;",numericInput("tl", "", 200, min = 60, max = 1000)),
      #div(style="height: 35px;",numericInput("atl", "", 20, min = 60, max = 1000)),
      div(style="height: 35px;",numericInput("numcow_es", "", 25000, min = 0, max = 1000000)),
      div(style="height: 35px;",numericInput("lwes", "",180 , min = 100, max = 300)),
      div(style="height: 35px;",numericInput("myes", "", 500, min = 0, max = 4000)),
      textInput("mp143A", "",  value="N/A")), 
  
 
  
  box(status = "warning", width = 2, " ",background ="green",
      # div(style="height: 35px;",textInput("mpkk", "", value="N/A")),
      # div(style="height: 35px;",textInput("mpkk1q", "", value="N/A")),
      div(style="height: 35px;",numericInput("numcow_sis", "", 15000, min = 0, max = 1000000)),
      div(style="height: 35px;",numericInput("lwsis", "", 220, min = 10, max =1000 )),
      div(style="height: 35px;",numericInput("mysis", "", 2000, min = 0, max = 5000)),
      textInput("mp143B", "", value="N/A")),
  
  
  
  box(status = "warning", width = 2, "",background ="green",
      
      # div(style="height: 35px;",textInput("mp11", "", value="N/A")),
      # div(style="height: 35px;",textInput("mp12", "", value="N/A")),
      div(style="height: 35px;",numericInput("numcow_is", "", 0, min = 0, max = 1000000)),
      div(style="height: 35px;",numericInput("lwis", "", 250, min = 150, max = 500)),
      div(style="height: 35px;",numericInput("myis", "", 2300, min = 0, max = 5000)),
      textInput("mp143", "", value="N/A"))# ,
  
  
  # box(status = "warning", width = 2, " ",background ="green",
  #     div(style="height: 35px;",textInput("mpkk2", "", value="N/A")),
  #     div(style="height: 35px;",textInput("mpkk3", "", value="N/A")),
  #     div(style="height: 35px;",numericInput("numcow_da", "", 22500, min = 0, max = 10000)),
  #     div(style="height: 35px;",numericInput("lwda", "", 250, min = 10, max =1000 )),
  #     div(style="height: 35px;",textInput("mpkkr4", "", value="N/A")),
       #textInput("mpkk5", "", value="N/A"))
  
),

# fluidRow(box(status = "warning", width = 2, " ",background ="green",
#              numericInput("numcow_c", "Number of animals crossing area:", 200000, min = 10, max =1000000 ))),




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
      textInput("9aFb", "", value="Silage")  
      
  ),
  box(status = "warning", width = 2, "",background ="blue",
      div(style="height: 35px;",numericInput("efng1", "", 30, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("efrc1", "", 70, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("efrr1", "", 0, min = 0, max = 0)),
      div(style="height: 35px;",numericInput("efrl1", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("efpf1", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("efconc1", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("efconos1", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("efhay1", "", 0, min = 0, max = 100)),
      numericInput("efsil1", "", 0, min = 0, max = 100)),
  # box(status = "warning", width = 2, "",background ="blue",
  #     div(style="height: 35px;",numericInput("lefng1", "", 95, min = 0, max = 100)),
  #     div(style="height: 35px;",numericInput("lefrc1", "", 0, min = 0, max = 100)),
  #     div(style="height: 35px;",numericInput("lefrr1", "", 0, min = 0, max = 100)),
  #     div(style="height: 35px;",numericInput("lefrl1", "", 5, min = 0, max = 100)),
  #     div(style="height: 35px;",numericInput("lefpf1", "", 0, min = 0, max = 100)),
  #     div(style="height: 35px;",numericInput("lefconc1", "", 0, min = 0, max = 100)),
  #     numericInput("lefconos1", "", 0, min = 0, max = 100)),
 
  box(status = "warning", width = 2, "",background ="blue",
      div(style="height: 35px;",numericInput("sfng1", "", 20, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sfrc1", "", 45, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sfrr1", "", 0, min = 0, max = 0)),
      div(style="height: 35px;",numericInput("sfrl1", "", 12, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sfpf1", "", 5, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sfconc1", "", 5, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sfconos1", "", 5, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sfhay1", "", 8, min = 0, max = 100)),
      numericInput("sfsil1", "", 0, min = 0, max = 100)),
  box(status = "warning", width = 2, "",background ="blue",
      div(style="height: 35px;",numericInput("ifng1", "", 5, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("ifrc1", "", 15, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("ifrr1", "", 0, min = 0, max = 0)),
      div(style="height: 35px;",numericInput("ifrl1", "", 10, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("ifpf1", "", 35, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("ifconc1", "", 15, min = 0, max = 100)),

        div(style="height: 35px;",numericInput("ifconos1", "", 5, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("ifhay1", "", 10, min = 0, max = 100)),
      numericInput("ifsil1", "", 5, min = 0, max = 100))#,
  # box(status = "warning", width = 2, "",background ="blue",
  #     div(style="height: 35px;",numericInput("dafng1", "", 80, min = 0, max = 100)),
  #     div(style="height: 35px;",numericInput("dafrc1", "", 0, min = 0, max = 100)),
  #     div(style="height: 35px;",numericInput("dafrr1", "", 0, min = 0, max = 100)),
  #     div(style="height: 35px;",numericInput("dafrl1", "", 0, min = 0, max = 100)),
  #     div(style="height: 35px;",numericInput("dafpf1", "", 0, min = 0, max = 100)),
  #     div(style="height: 35px;",numericInput("dafconc1", "", 10, min = 0, max = 100)),
  #     numericInput("dafconos1", "", 10, min = 0, max = 100))
  
  
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
      textInput("9aFb", "", value="Silage") 
  ), 
  box(status = "warning", width = 2, "", background ="orange",
      div(style="height: 35px;",numericInput("efng2", "", 98, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("efrc2", "", 2, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("efrr2", "", 0, min = 0, max = 0)),
      div(style="height: 35px;",numericInput("efrl2", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("efpf2", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("efconc2", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("efconos2", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("efhay2", "", 0, min = 0, max = 100)),
      numericInput("efsil2", "", 0, min = 0, max = 100)),
  # box(status = "warning", width = 2, "", background ="orange",
  #     div(style="height: 35px;",numericInput("lefng2", "", 50, min = 0, max = 100)),
  #     div(style="height: 35px;",numericInput("lefrc2", "", 45, min = 0, max = 100)),
  #     div(style="height: 35px;",numericInput("lefrr2", "", 5, min = 0, max = 100)),
  #     div(style="height: 35px;",numericInput("lefrl2", "", 0, min = 0, max = 100)),
  #     div(style="height: 35px;",numericInput("lefpf2", "", 0, min = 0, max = 100)),
  #     div(style="height: 35px;",numericInput("lefconc2", "", 0, min = 0, max = 100)),
  #     numericInput("lefconos2", "", 0, min = 0, max = 100)),
 
  box(status = "warning", width = 2, "", background ="orange",
      div(style="height: 35px;",numericInput("sfng2", "", 62, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sfrc2", "", 5, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sfrr2", "", 0, min = 0, max = 0)),
      div(style="height: 35px;",numericInput("sfrl2", "", 5, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sfpf2", "", 14, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sfconc2", "", 9, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sfconos2", "", 5, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sfhay2", "", 0, min = 0, max = 100)),
      numericInput("sfsil2", "", 0, min = 0, max = 100)),
  
  box(status = "warning", width = 2, "", background ="orange",
      div(style="height: 35px;",numericInput("ifng2", "", 25, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("ifrc2", "", 5, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("ifrr2", "", 0, min = 0, max = 0)),
      div(style="height: 35px;",numericInput("ifrl2", "", 5, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("ifpf2", "", 45, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("ifconc2", "", 15, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("ifconos2", "", 5, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("ifhay2", "", 0, min = 0, max = 100)),
      numericInput("ifsil2", "", 0, min = 0, max = 100))#,
  
  # box(status = "warning", width = 2, "", background ="orange",
  #     div(style="height: 35px;",numericInput("dafng2", "", 20, min = 0, max = 100)),
  #     div(style="height: 35px;",numericInput("dafrc2", "", 40, min = 0, max = 100)),
  #     div(style="height: 35px;",numericInput("dafrr2", "", 10, min = 0, max = 100)),
  #     div(style="height: 35px;",numericInput("dafrl2", "", 15, min = 0, max = 100)),
  #     div(style="height: 35px;",numericInput("dafpf2", "", 0, min = 0, max = 100)),
  #     div(style="height: 35px;",numericInput("dafconc2", "", 10, min = 0, max = 100)),
  #     numericInput("dafconos2", "", 5, min = 0, max = 100)) 
  
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
      div(style="height: 35px;",numericInput("es_solidstorage_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("es_drylot_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("es_pasture_perc", "", 80, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("es_dailyspread_perc", "", 10, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("es_digester_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("es_fuel_perc", "", 3, min = 0, max = 100)),
      numericInput("es_other_perc", "", 7, min = 0, max = 100)),
  # box(status = "warning", width = 2, "", background ="teal",
  #     div(style="height: 35px;",numericInput("les_lagoon_perc", "", 0, min = 0, max = 100)),
  #     div(style="height: 35px;",numericInput("les_liquidslurry_perc", "", 0, min = 0, max = 100)),
  #     div(style="height: 35px;",numericInput("les_solidstorage_perc", "", 20, min = 0, max = 100)),
  #     div(style="height: 35px;",numericInput("les_drylot_perc", "", 0, min = 0, max = 100)),
  #     div(style="height: 35px;",numericInput("les_pasture_perc", "", 80, min = 0, max = 100)),
  #     div(style="height: 35px;",numericInput("les_dailyspread_perc", "", 100, min = 0, max = 100)),
  #     div(style="height: 35px;",numericInput("les_digester_perc", "", 0, min = 0, max = 100)),
  #     div(style="height: 35px;",numericInput("les_fuel_perc", "", 0, min = 0, max = 100)),
  #     numericInput("les_other_perc", "", 0, min = 0, max = 100)),
  
  box(status = "warning", width = 2, "", background ="teal",
      div(style="height: 35px;",numericInput("sis_lagoon_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sis_liquidslurry_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sis_solidstorage_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sis_drylot_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sis_pasture_perc", "", 10, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sis_dailyspread_perc", "", 80, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sis_digester_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("sis_fuel_perc", "", 3, min = 0, max = 100)),
      numericInput("sis_other_perc", "", 7, min = 0, max = 100)), 
  box(status = "warning", width = 2, "", background ="teal",
      div(style="height: 35px;",numericInput("is_lagoon_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("is_liquidslurry_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("is_solidstorage_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("is_drylot_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("is_pasture_perc", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("is_dailyspread_perc", "", 90, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("is_digester_perc", "", 5, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("is_fuel_perc", "", 0, min = 0, max = 100)),
      numericInput("is_other_perc", "", 5, min = 0, max = 100))#,

  # box(status = "warning", width = 2, "", background ="teal",
  #     div(style="height: 35px;",numericInput("da_lagoon_perc", "", 0, min = 0, max = 100)),
  #     div(style="height: 35px;",numericInput("da_liquidslurry_perc", "", 0, min = 0, max = 100)),
  #     div(style="height: 35px;",numericInput("da_solidstorage_perc", "", 100, min = 0, max = 100)),
  #     div(style="height: 35px;",numericInput("da_drylot_perc", "", 0, min = 0, max = 100)),
  #     div(style="height: 35px;",numericInput("da_pasture_perc", "", 0, min = 0, max = 100)),
  #     div(style="height: 35px;",numericInput("da_dailyspread_perc", "", 0, min = 0, max = 100)),
  #     div(style="height: 35px;",numericInput("da_digester_perc", "", 0, min = 0, max = 100)),
  #     div(style="height: 35px;",numericInput("da_fuel_perc", "", 0, min = 0, max = 100)),
  #     numericInput("da_other_perc", "", 0, min = 0, max = 100))  
  
  
),

fluidRow(
  box(status = "primary",width = 12, "SEASONALITY DEFINITION",background = "navy")
),

# fluidRow(
#   box(status = "primary",width = 10, "Transhumance season",background = "green"),
#   box(status = "primary",width = 2, "Variable ",background = "green")
# ),
# fluidRow(
#   box(status = "warning", width = 10, "", background ="green",
#       div(style="height: 35px;",textInput("1p", "", value="season when transhumant animals are paturage in Bama and small transhumance in the study area (juillet_novembre)")),
#       textInput("2p", "", value="season when transhumant animals are gone (decembre_june)")),
#   box(status = "warning", width = 2, "", background ="green",
#       div(style="height: 35px;",numericInput("st1", "", 5, min = 1, max = 11)),
#       numericInput("st2", "", 7, min = 1, max = 11))
# ),

# fluidRow(
#   box(status = "primary",width = 10, "Crop seasonality",background = "green"),
#   box(status = "primary",width = 2, "Variable",background = "green")
# ),

fluidRow(
  box(status = "primary", width = 10, "", background ="green",
      div(style="height: 35px;",textInput("1q", "", value="dry season in months")),
      textInput("2q", "", value="wet season in month")),
  box(status = "primary", width = 2, "", background ="green",
      div(style="height: 35px;",numericInput("ds", "", 4, min = 1, max = 11)),
      numericInput("ws", "", 8, min = 1, max = 11))
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
      div(style="height: 35px;",textInput("1n", "", value="manure production from a local breed cow per day")),
      div(style="height: 35px;",textInput("2n", "", value="manure production from a cross breed cow per day")),
      textInput("4n", "", value="manure production from a an almost pure breed cow per day")),
  
  box(status = "warning", width = 2, "", background ="blue",
      div(style="height: 35px;",numericInput("mprod_es", "", 2, min = 0, max = 20)),
      div(style="height: 35px;",numericInput("mprod_sis", "", 3, min = 0, max = 20)),
      numericInput("mprod_is", "", 3, min = 0, max = 20))
),

fluidRow(
  box(status = "primary",width = 12, "Stored manure, slurry and fertilizer application to different crop",background = "teal")
),
fluidRow(
  box(status = "warning", width = 10, "", background ="teal",
      div(style="height: 35px;",textInput("1ssf", "", value="proportion of stored manure applied to cereals")),
      div(style="height: 35px;",textInput("2ssf", "", value="proportion of stored manure applied to legume ")),
      div(style="height: 35px;",textInput("3ssf", "", value="proportion of stored manure applied to planted fodder")),
      div(style="height: 35px;",textInput("4ssf", "", value="proportion of stored manure applied to rice ")),
      div(style="height: 35px;",textInput("5ssf", "", value="proportion of stored manure applied to grazing land  ")),
      div(style="height: 35px;",textInput("6ssf", "", value="proportion of stored slurry applied to cereals")),
      div(style="height: 35px;",textInput("7ssf", "", value="proportion of stored slurry applied to legume ")),
      div(style="height: 35px;",textInput("8ssf", "", value="proportion of stored slurry applied to planted fodder")),
      div(style="height: 35px;",textInput("9ssf", "", value="proportion of stored slurry applied to rice ")),
      div(style="height: 35px;",textInput("10ssf", "", value="proportion of stored slurry applied to grazing land ")),
      div(style="height: 35px;",textInput("11ssf", "", value="Fertilizer application kg/ha applied to cereals ")),
      div(style="height: 35px;",textInput("12ssf", "", value="Fertilizer application kg/ha applied to legume ")),
      div(style="height: 35px;",textInput("13ssf", "", value="Fertilizer application kg/ha applied to planted fodder")),
      div(style="height: 35px;",textInput("14ssf", "", value="Fertilizer application kg/ha applied to rice ")),
      div(style="height: 35px;",textInput("15ssf", "", value="Fertilizer application kg/ha applied to grazing land ")),
      textInput("16ssf", "", value="Conversion rate between fertilizer (NPK) and Nitrogen")),
  
  box(status = "warning", width = 2, "", background ="teal",
      div(style="height: 35px;",numericInput("manc", "", 0.6, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("manl", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("manpf", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("manr", "", 0.3, min = 0, max = 100)),
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
      div(style="height: 35px;",numericInput("fertgraz", "", 0, min = 0, max = 100)),
      numericInput("Fertconv", "", 0.2, min = 0, max = 1))
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
      div(style="height: 35px;",numericInput("pgc", "", 0.02, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("pgl", "", 0.0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("pgpf", "", 0.0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("pgg", "", 0.0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("pgr", "", 0.0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("perc_til", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("perc_redtil", "", 100, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("perc_notil", "", 0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("perc_inlow", "", 100, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("perc_inmedium", "",0, min = 0, max = 100)),
      div(style="height: 35px;",numericInput("perc_inhighnoman", "", 0, min = 0, max = 100)),
      numericInput("perc_inhighman", "", 0, min = 0, max = 100))
),

fluidRow(
  box(status = "primary",width = 12, "land use change ",background = "light-blue")
  
),
# fluidRow(
#   box(status = "warning",width = 4, "Objective",background ="red"),
#   box(status = "warning", width = 4, "Name",background ="maroon"),
#   box(status = "warning", width = 4,"Activates a loop that",background ="maroon")
#   
# ),


fluidRow(
    box(status = "warning", width = 10, "add crop land in percent of baseline of biomass ",background ="aqua",
        # div(style="height: 35px;",textInput("mpkk", "", value="N/A")),
        # div(style="height: 35px;",textInput("mpkk1q", "", value="N/A")),
        numericInput("add", "", 0, min = 0, max = 100))
     
),

fluidRow(
  box(width=6, " Baseline Productivity",plotOutput("baseplot")))

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
  
  
  dashboardHeader(title = "SAIRLA PROJECT : CLEANED INTERFACE SYSTEM FOR LUSHOTO TANZANIA-ADVANCED USERS",titleWidth=1000,
                  
                  # tags$li(class = "dropdown",
                  #         tags$a(href="https://www.ilri.org", target="_blank", 
                  #                tags$img(height = "30px", alt="SNAP Logo", src="https://www.ilri.org/sites/default/files/styles/card_image_thumb/public/default_images/defautl_img.jpg")
                  #        ))),     
                  
                  
                  tags$li(class = "dropdown",
                          tags$a(href="www", target="_blank", 
                                 tags$img(height = "60px", alt="SNAP Logo", src="crpaaseiatz.jpg")
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
  observeEvent(input$st1, {
  
    x <- input$st1
    
    updateNumericInput(session, "st2", value = 12-x)})

  observeEvent(input$st2, {
    
    x3 <- input$st2
    
    updateNumericInput(session, "st1", value = 12-x3)})
  

  
  observeEvent(input$ds, {

    x1 <- input$ds
    
    updateNumericInput(session, "ws", value = 12-x1)})
  
  
  observeEvent(input$ws, {
    
    x1d <- input$ws
    
    updateNumericInput(session, "ds", value = 12-x1d)})
  
  
  
   pok<- eventReactive(input$go, {
     
     if(input$go==0)
       return()
     
     
     
     
     #clearing all memory 
         #set path to cleaned tool
     path<<-'D:/Dropbox/Cleaned Tanzania' 
     ###################################This sheet defines all user defined variables#################
     
     #put here the name of the run 
     preset <<-0 #if 1 the interface uses the presets if 0 or other it uses the manual input below. 
     
     name<<-input$name
     ##########livestock systems 
     #seasonality 
     # #transhumance seasonality 
     # st1<<- input$st1 # season when transhumant animals are paturage in Bama and small transhumance in the study area (juillet-novembre)
     # st2<<- 12-st1 # season when transhumant animals are gone (decembre - june) 
     #crop seasonality 
     ds<<- input$ds #dry season (October- April)
     ws<<- 12-ds  # wet season (May-September )
     
     # ws_st1<<-input$ws_st1 # number of month the transhumant troupeau is in area during the wet season
     # ds_st1<<-input$ds_st1 # number of month the transhumant troupeau is in area during the dry season
     # 
     
     #DEFINE THE DIFFERENT SYSTEM 
     #################################
     
     # the extensive agro pastoral system
     
     # definition des troupeaux
     #nombre de troupeau qui font la longue transhumance (70 in Bama)
     #tt<<-input$tt
     #nombre de troupeau laitier (150 in Bama)
     #tl<-200
     #tl<<-input$tl
     #nombre de troupeau qui ne font que la petite transhumance (180 in Bama)
     #tpt<<-input$tpt
     # tpt<<-input$tpt
     #nombre moyen ou modal de tete dans un troupeau pour la petite ou la grande transhumance
     #att<<-input$att
      #atpt<<-input$atpt
     # atpt<<-input$atpt
     # nombre moyen ou modal de tete d'un troupeau laitier
     #atl<-20
     #atl<<-input$atl
     # definition de l'animal extensif transhumant
     #define liveweigtht in kg for the the breed in the transhumant system (200)
     #lwes=200 
     numcow_es<<-input$numcow_es
     
     lwes<<-input$lwes
     # definition de l'animal extensif laitier
     #define liveweigtht in kg for the the breed in the transhumant system (200)
     #lwles<<-input$lwles 
     #define milk yield (kg/cow/year) for the breed in the extensive system (400) 
    myes<<-input$myes # http://www.abcburkina.net/fr/nos-dossiers/la-filiere-lait/586-18-performances-laitieres-des-vaches-de-races-locales
     #dressing percentage http://www.dpi.nsw.gov.au/__data/assets/pdf_file/0006/103992/dressing-percentages-for-cattle.pdf
     des <<- input$des
     #numcow_c<<- input$numcow_c #200000
     #feed basket for transhumant troupeau in the extensive systems
     # season wet season 
     #natural grass in percent (95)
     efng1<<- input$efng1
     #crop residues cerals (0)
     efrc1<<-input$efrc1
     #crop residue from rice
     efrr1<<-input$efrr1
     #crop residue legumes (10) 
     efrl1<<-input$efrl1
     #planted fodder (0)
     efpf1<<-input$efpf1
     # concentrates cereal (maize bran) (0)
     efconc1<<- input$efconc1
     # concentrates oilseed cake (0)
     efconos1<<- input$efconos1
     #check if a 100%
     efhay1<<- input$efhay1
     efsil1<<- input$efsil1
     #season dry season  
     #natural grass in percent (100)
     efng2<<- input$efng2  
     #crop residues cerals (0)
     efrc2<<- input$efrc2
     #crop residue from rice
     efrr2<<-input$efrr2
     #crop residue legumes (0) 
     efrl2<<-input$efrl2
     #planted fodder (0)
     efpf2<<-input$efpf2  #
     # concentrates cereal (maize bran) (0)
     efconc2<<-input$efconc2
     # concentrates oilseed cake (0)
     efconos2<<-input$efconos2
     #check if a 100%
     efhay2<<- input$efhay2
     efsil2<<- input$efsil2
     #feed basket for milking troupeau in the extensive systems
     # season wet

     
     # manure management in the extensive system  system in percent (100%)
     #(if ipcc=1, then no need to adjust this )
     # es_lagoon_perc<<- 00
     es_lagoon_perc<<-input$es_lagoon_perc
     # es_liquidslurry_perc<<-00
     es_liquidslurry_perc<<-input$es_liquidslurry_perc
     # es_solidstorage_perc<<-00
     es_solidstorage_perc<<-input$es_solidstorage_perc
     # es_drylot_perc<<-00
     es_drylot_perc<<-input$es_drylot_perc
     # es_pasture_perc<<-100
     es_pasture_perc<<-input$es_pasture_perc
     # es_dailyspread_perc<<-00
     es_dailyspread_perc<<-input$es_dailyspread_perc
     # es_digester_perc<<-00
     es_digester_perc<<-input$es_digester_perc
     # es_fuel_perc<<-00
     es_fuel_perc<<-input$es_fuel_perc
     # es_other_perc<<-00
     es_other_perc<<-input$es_other_perc
     #do by season 
     
     
    
     
     
     ####################################### the fattening system###################
     
     # numcow_f<<- 60000
     numcow_sis<<-input$numcow_sis
     
     #define liveweigtht in kg for the the breed in the semi intensive system (250)
     # lwsis<<-250 
     lwsis<<-input$lwsis
     #define milk yield (kg/cow/year) for the breed in the semi intensive system (0)
     # mysis<<-0
     mysis<<-input$mysis
     
     #dressing percentage http://www.dpi.nsw.gov.au/__data/assets/pdf_file/0006/103992/dressing-percentages-for-cattle.pdf
     # dsis <<- 0.48
     dsis <<- input$dsis
     #feed basket  for fattening  system season wet
     
     #natural grass in percent (15)
     # sfng1<<-80
     sfng1<<-input$sfng1
     #crop residues cereals (40)
     # sfrc1<<-0
     sfrc1<<-input$sfrc1
     #crop residue from rice
     # sfrr1<<- 0
     sfrr1<<-input$sfrr1
     #crop residue legumes (30)
     # sfrl1<<-0
     sfrl1<<-input$sfrl1
     #planted fodder ()
     # sfpf1<<-0 
     sfpf1<<-input$sfpf1
     # concentrates cereal (maize bran) (5)
     # sfconc1<<- 10
     sfconc1<<- input$sfconc1
     # concentrates oilseed cake (10)
     # sfconos1<<- 10
     sfconos1<<-input$sfconos1
     sfhay1<<- input$sfhay1
     sfsil1<<- input$sfsil1
     # dry season 
     #natural grass in percent (50)
     # sfng2<<-25
     sfng2<<-input$sfng2
     #crop residues cereals (0)
     # sfrc2<<-40
     sfrc2<<-input$sfrc2
     #crop residue from rice
     # sfrr2<<-10 
     sfrr2<<-input$sfrr2
     #crop residue legumes (5) 
     # sfrl2<<-0
     sfrl2<<-input$sfrl2
     #planted fodder (12)
     # sfpf2<<-5 
     sfpf2<<-input$sfpf2
     # concentrates cereal (maize bran) (20)
     # sfconc2<<- 10
     sfconc2<<- input$sfconc2
     # concentrates oilseed cake (10)
     # sfconos2<<- 10  
     sfconos2<<-input$sfconos2
     sfhay2<<- input$sfhay2
     sfsil2<<- input$sfsil2
     
     # manure management in the fattening system in percent (100%)
     #(if ipcc=1, then no need to adjust this )
     # sis_lagoon_perc<<- 00
     sis_lagoon_perc<<-input$sis_lagoon_perc
     # sis_liquidslurry_perc<<-00
     sis_liquidslurry_perc<<-input$sis_liquidslurry_perc
     # sis_solidstorage_perc<<-100
     sis_solidstorage_perc<<-input$sis_solidstorage_perc
     # sis_drylot_perc<<-00
     sis_drylot_perc<<-input$sis_drylot_perc
     # sis_pasture_perc<<-00
     sis_pasture_perc<<-input$sis_pasture_perc
     # sis_dailyspread_perc<<-00
     sis_dailyspread_perc<<-input$sis_dailyspread_perc
     # sis_digester_perc<<-00
     sis_digester_perc<<-input$sis_digester_perc
     # sis_fuel_perc<<-00
     sis_fuel_perc<<-input$sis_fuel_perc
     # sis_other_perc<<-00
     sis_other_perc<<-input$sis_other_perc
     
     ################################# dairy system non pastoral ############
     
     # numcow_d <<- 1400
     numcow_is <<-input$numcow_is
     #define liveweigtht in kg for the the breed in dairy system (250)
     # lwis<<-250
     lwis<<-input$lwis
     #define milk yield (kg/cow/year) for the breed in the dairy system (1000)
     # myis<<-1000 #http://www.abcburkina.net/fr/nos-dossiers/la-filiere-lait/586-18-performances-laitieres-des-vaches-de-races-locales
     myis<<-input$myis
     
     
     #feed basket for dairy wet season
     #natural grass in percent (15)
     # ifng1<<-80
     ifng1<<-input$ifng1
     #crop residues cerals (40)
     # ifrc1<<-0
     ifrc1<<-input$ifrc1
     #crop residue from rice
     # ifrr1<<-0
     ifrr1<<-input$ifrr1
     #crop residue legumes (30)
     # ifrl1<<-0
     ifrl1<<-input$ifrl1
     #planted fodder ()
     # ifpf1<<-0
     ifpf1<<-input$ifpf1
     # concentrates cereal (maize bran) (5)
     # ifconc1<<- 10
     ifconc1<<-input$ifconc1
     # concentrates oilseed cake (10)
     # ifconos1<<- 10
     ifconos1<<-input$ifconos1
     ifhay1<<- input$ifhay1
     ifsil1<<- input$ifsil1
     
     #feed basket for dairy dry season
     #natural grass in percent (60)
     # ifng2<<-20
     ifng2<<-input$ifng2
     #crop residues cerals (0)
     #ifrc2<<-25
     ifrc2<<-input$ifrc2
     #crop residue from rice
     # ifrr2<<- 0 
     ifrr2<<- input$ifrr2
     #crop residue legumes (10)
     # ifrl2<<-15
     ifrl2<<-input$ifrl2
     #planted fodder (10)
     # ifpf2<<-0
     ifpf2<<-input$ifpf2
     # concentrates cereal (maize bran) (20)
     # ifconc2<<- 20
     ifconc2<<- input$ifconc2
     # concentrates oilseed cake (10)
     # ifconos2<<- 20
     ifconos2<<-input$ifconos2
     ifhay2<<- input$ifhay2
     ifsil2<<- input$ifsil2
     
     # manure management in the semi-intensive system in percent (100%)
     #(if ipcc=1, then no need to adjust this )
     # is_lagoon_perc<<- 00
     is_lagoon_perc<<-input$is_lagoon_perc
     # is_liquidslurry_perc<<-00
     is_liquidslurry_perc<<-input$is_liquidslurry_perc
     # is_solidstorage_perc<<-100
     is_solidstorage_perc<<-input$is_solidstorage_perc
     # is_drylot_perc<<-00
     is_drylot_perc<<-input$is_drylot_perc
     # is_pasture_perc<<-00
     is_pasture_perc<<-input$is_pasture_perc
     # is_dailyspread_perc<<-00
     is_dailyspread_perc<<-input$is_dailyspread_perc
     # is_digester_perc<<-00
     is_digester_perc<<-input$is_digester_perc
     # is_fuel_perc<<-00
     is_fuel_perc<<-input$is_fuel_perc
     # is_other_perc<<-00
     is_other_perc<<-input$is_other_perc
     ################################# darft animals ############
     
     # # numcow_da <<- 22500
     # numcow_da <<-input$numcow_da
     # #define liveweigtht in kg for the the breed in dairy system (250)
     # # lwda<<-250
     # lwda<<-input$lwda
     # 
     # #define milk yield (kg/cow/year) for the breed in the dairy system (1000)
     # # myda<<-0 #http://www.abcburkina.net/fr/nos-dossiers/la-filiere-lait/586-18-performances-laitieres-des-vaches-de-races-locales
     # myda<<-input$myda
     # 
     # #feed basket for dairy wet season
     # #natural grass in percent (15)
     # # dafng1<<-80
     # dafng1<<-input$dafng1
     # #crop residues cerals (40)
     # # dafrc1<<-0
     # dafrc1<<-input$dafrc1
     # #crop residue from rice
     # # dafrr1<<-0
     # dafrr1<<-input$dafrr1
     # #crop residue legumes (30)
     # # dafrl1<<-0
     # dafrl1<<-input$dafrl1
     # #planted fodder ()
     # # dafpf1<<-0
     # dafpf1<<-input$dafpf1
     # # concentrates cereal (maize bran) (5)
     # # dafconc1<<- 10
     # dafconc1<<- input$dafconc1
     # # concentrates oilseed cake (10)
     # # dafconos1<<- 10
     # dafconos1<<- input$dafconos1
     # #feed basket for dairy dry season
     # #natural grass in percent (60)
     # # dafng2<<-20
     # dafng2<<-input$dafng2
     # #crop residues cerals (0)
     # # dafrc2<<-40
     # dafrc2<<-input$dafrc2
     # #crop residue from rice
     # # dafrr2<<- 10 
     # dafrr2<<- input$dafrr2
     # #crop residue legumes (10)
     # # dafrl2<<-15
     # dafrl2<<-input$dafrl2
     # #planted fodder (10)
     # # dafpf2<<-0
     # dafpf2<<-input$dafpf2
     # # concentrates cereal (maize bran) (20)
     # # dafconc2<<- 10
     # dafconc2<<-input$dafconc2
     # # concentrates oilseed cake (10)
     # # dafconos2<<- 5
     # dafconos2<<-input$dafconos2
     # 
     # # manure management in the semi-intensive system in percent (100%)
     # #(if ipcc=1, then no need to adjust this )
     # # da_lagoon_perc<<- 00
     # da_lagoon_perc<<-input$da_lagoon_perc
     # # da_liquidslurry_perc<<-00
     # da_liquidslurry_perc<<-input$da_liquidslurry_perc
     # # da_solidstorage_perc<<-100
     # da_solidstorage_perc<<-input$da_solidstorage_perc
     # # da_drylot_perc<<-00
     # da_drylot_perc<<-input$da_drylot_perc
     # # da_pasture_perc<<-00
     # da_pasture_perc<<-input$da_pasture_perc
     # # da_dailyspread_perc<<-00
     # da_dailyspread_perc<<-input$da_dailyspread_perc
     # # da_digester_perc<<-00
     # da_digester_perc<<-input$da_digester_perc
     # # da_fuel_perc<<-00
     # da_fuel_perc<<-input$da_fuel_perc
     # # da_other_perc<<-00
     # da_other_perc<<-input$ da_other_perc
     # #######transhumants animals
     # # numcow_trans<<- 100000
     # numcow_trans<<-input$numcow_trans
     # 
     # ta<<-1 #time in area
     # #ta<<-input$ta
     
     #######################################################################################
     #global variable definition
     #ipcc= 1 the code will use ipcc tier 2 standards for manure storage in stead of the user defined one
     ipcc<<-0 
     #ipcc<<-input$ipcc
     
     #########################parmeters specific to the soil Impact      ##################
     
     
     #linking the manure availability to the production system 
   
     #mprod_f<<- 3 #manure production from a cow in the fattening system per day
     mprod_sis<<- input$mprod_sis
     #mprod_d<<- 3 #manure production from a cow in the dairy system per day
     mprod_is<<- input$mprod_is
     #mprod_da<<- 3 #manure production from a cow in the dairy system per day
     mprod_es<<- input$mprod_es
     
     #percent of stored manure applied to the different crop
     #cereal (mprod_c *% to this crop for linking with production )
     #manc<<-0.5
     manc<<-input$manc
     # legumes  (mprod_c *% to this crop for linking with production )
     #manl<<-0
     manl<<-input$manl
     #planted fodder  (mprod_c *% to this crop for linking with production )
     #manpf<<- 0
     manpf<<- input$manpf
     #rice  (mprod_r *% to this crop for linking with production )
     #manr<<- 0.3 
     manr<<-input$manr
     #grazing land  (mprod_r *% to this crop for linking with production )
     #mangraz<<-0 
     mangraz<<-input$mangraz
     # application of slurry kg/ha
     #cereal ()
     #sluc<<-0
     sluc<<-input$sluc
     # legumes (0)
     #slul<<-0
     slul<<-input$slul
     #planted fodder ()
     #slupf<<-0
     slupf<<-input$slupf
     #grazing land
     #slugraz<<-0
     slugraz<<-input$slugraz
     #rice land
     #slur<<-0
     slur<<-input$slur
     
     slurryconv<<- 0.001 #conversion rate between slurry (NPK) and Nitrogen
     #slurryconv<<-input$slurryconv                  #we need a source here What about compost and other manure. 
     
     #inorganic fertilizer application in kg per hectare
     
     #cereal (50 is recommended)
     # fertc<<-0
     fertc<<-input$fertc
     #rice (50 is recommended)
     # fertr<<-0
     fertr<<-input$fertr
     # legumes (0)
     # fertl<<-0
     fertl<<-input$fertl
     #planted fodder 
     # fertpf<<-0
     fertpf<<-input$fertpf
     #grazing land
     #fertgraz<<-0
     fertgraz<<-input$fertgraz
     
     Fertconv<<- 0.2 #conversion rate between fertilizer (NPK) and Nitrogen, depends on the locally available ferilizer, +/- 20%
     #Fertconv<<-input$Fertconv              # from impact lit we know that DAP is most commonly used - Joanne is looking for conversion rates
     
     
     #exogenous yield productivity gain in percentage of yield
     #crop
     # pgc<<- 0.0
     pgc<<-input$pgc
     #legumes
     # pgl<<-0.0
     pgl<<-input$pgl
     #planted fodder
     # pgpf<<-0.0
     pgpf<<-input$pgpf
     #grassland
     # pgg<<-0.0
     pgg<<-input$pgg
     # rice 
     # pgr<<- 0.0
     pgr<<-input$pgr
     #############soil management option on cropland (ghg)
     # perc_til<<- 0 #percentage of cropland that is tilled 
    perc_til<<-input$perc_til
    # perc_redtil <<-100 #percentage of cropland that is on reduced till
    perc_redtil <<-input$perc_redtil  
    # perc_notil <<- 0 #percentage of cropland that is on no till
    perc_notil <<- input$perc_notil
     # perc_inlow <<-100  #percentage of land with low input 
     perc_inlow <<-input$perc_inlow
     # perc_inmedium <<- 0  #percentage of land with medium input 
     perc_inmedium <<- input$perc_inmedium
     # perc_inhighnoman<<-0  #percentage of land with high input no manure 
     perc_inhighnoman<<-input$perc_inhighnoman
     # perc_inhighman <<-0   #percentage of land with high input with manure
     perc_inhighman <<-input$perc_inhighman
     
     #####reading some r info
     #setwd(path)
     pixel<-read.csv('1-input/parameter/pixel.csv')
     pixel<-as.numeric(pixel[2])
     
     ##############################land use driven scenarios
     library(raster)
     
     #do we run a land use change driven scenario? then we need to run the land use change module first 
     # and read here the file indicating the pixels that have changed
     
     add<<-input$add
     #add the path to the changes in land use rasters
     #path to changing cropland i.e. cropland change layer
     
     #cpath<-'1-input/spatial/landuse_scenario/addcrop.tif'
     
     #testing the switch in land use using availlable tif file... example
     # cpath1<<-'1-input/spatial/landuse_scenario/addcrop20.tif'
     # cpath2<<-'1-input/spatial/landuse_scenario/addcrop50.tif'
     # cpath3<<-'1-input/spatial/landuse_scenario/addcrop20p.tif'
     # cpath4<<-'1-input/spatial/landuse_scenario/addcrop50p.tif'
     # 
     # if (lucc=='LUC1'){
     #   addcrop<<- raster(cpath1)
     #   
     # } else {if(lucc=='LUC2'){
     #   addcrop<<-  raster(cpath2)
     # } else {if(lucc=='LUC3'){
     #   addcrop<<-  raster(cpath3)
     # } else{if(lucc=='LUC4') {
     #   addcrop<<-  raster(cpath4)
     # } else {lucs<<-0}
     # }}}
     # 
     # 
     
     # if (lucs==1){
     #   addcrop<<- raster(cpath)
     # }
     
     library(raster)
     
     #do we run a land use change driven scenario? then we need to run the land use change module first 
     # and read here the file indicating the pixels that have changed
     
     #add the path to the changes in land use rasters
     #path to changing cropland i.e. cropland change layer
     
     #library(raster)
     
     #do we run a land use change driven scenario? then we need to run the land use change module first 
     # and read here the file indicating the pixels that have changed
     
     #add the path to the changes in land use rasters
     #path to changing cropland i.e. cropland change layer
     # cpath<<-'1-input/spatial/landuse_scenario'
     # lucs<<-1
     # if (LUC=='LUC1'){
     #   addcrop<<- raster(paste(cpath,as.character(scenario[37,'LUC1']),sep="/"))
     #   
     # } else {if(LUC=='LUC2'){
     #   addcrop<<- raster(paste(cpath,as.character(scenario[37,'LUC2']),sep="/"))
     # } else {if(LUC=='LUC3'){
     #   addcrop<<- raster(paste(cpath,as.character(scenario[37,'LUC3']),sep="/"))
     # } else{if(LUC=='LUC4') {
     #   addcrop<<- raster(paste(cpath,as.character(scenario[37,'LUC4']),sep="/"))
     # } else {lucs<<-0}
     # }}}
     
    
     #setwd(path)
     
     #source('3-cleaned/2-load_data.r')
     #setwd(path)
     
     source('2-feedbasket_nonlinear2Simple.r')

   
     paste("Productivity parameters updated for this session.")
   })
    

output$plot <- renderText(
    {     withProgress(pok(), message="Please wait: Updating productivity module.")
      
      pok()
      
    
      })
  
  pok1<- eventReactive(input$gowt, {
    
    if(input$gowt==0)
      return()
    
    source('1-waterSimple.R')})
  
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
    
    source('1-ghgSimple.r')})
  
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
  
  pok1bdiv<- eventReactive(input$gobdiv, {
    
    if(input$gobdiv==0)
      return()
    
    source('1-biodivSimple.r')})
  
  output$plot12bid <- renderPlot(
    { 
      withProgress(pok1bdiv(), message="Please wait: Running Biodiversity Impact." )
      
      pok1bdiv()
      
      
    })
  
  pok1tbiv<- eventReactive(input$gobdiv, {
    
    if(input$gobdiv==0)
      return()
    
    grid.table(bio_ind2)
    
    #data.frame(rbind(bio_ind,bio_ind_diff))
  })
  
  output$plotablediv <- renderPlot(
    { pok1tbiv()
      
      
    })
  
  psoil<- eventReactive(input$gosol, {
    
    if(input$gosol==0)
      return()
    
    source('1-soilSimple.r')})
  

  
  
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

