####################################################################################################
#
# Shiny App for CLEANED Ethiopia System ILRI  

# October 2018 by Catherine Pfeifer
# base on base code of May 2017 by Victor N. Mose and Caroline Mburu, LocateIT
####################################################################################################
## app.R ##

rm(list=ls())
library(shinydashboard)
library(shiny)
library(raster)
library(maptools)
library(rgdal)
library(gridExtra)
library(grid)

path<<-'D:/Dropbox/Cleaned Ethiopia'

setwd(path)


source('2-load_data.r')

sidebar1<-dashboardSidebar(width=250,
                           sidebarMenu(
                               menuItem(""),
                               menuItem("ABOUT CLEANED TOOL", tabName = "widgets1", icon = icon("th")),
                               menuItem("SIMPLE USERS", tabName = "dashboard1", icon = icon("dashboard")),
                               #menuItem("SIMPLE USERS: FRENCH", tabName = "widgets1a", icon = icon("th"))
                               menuItem("ADVANCED USERS", tabName = "widadv", icon = icon("dashboard"))
                               # menuItem("Vegetation mapping", tabName = "Veg", icon = icon("th"))
                           ))


#source('3-cleaned/2-load_data.r')

body <- dashboardBody(

  tags$head(tags$style(HTML(".grViz { width:100%!important;}"))),
 
   tabItems(
      
      #first tab item (OK)
      
      tabItem(tabName ="widgets1",fluidRow (
          
          
          box(width=12,
              
              column(width=1,offset=2, h4("", style="padding:100px;")),
              img(src="Buks1.jpg" , width=650),                                         
              
              background ="blue"),
          
          
          
          box(width=12,background ="green", 
              column(width=1,offset=2, h4("", style="padding:100px;")),
              # div(img(...), style="text-align: center;").
              
              div(img(src="paramterization.jpg",style="text-align: center;", width=650))
              #img(src="m1.jpg", width=476)
              
          )
      ),
      fluidRow(box(width=12, background ="blue",title="Study area: Atsbi Tigray, Ethiopia",
                   column(width=1,offset=2, h4("", style="padding:10px;")),
                   img(src="studyarea.jpg", width=650)),
               box(width=12,background ="green", 
                   column(width=1,offset=1, h4("", style="padding:10px;")),
                   img(src="vignettes.jpg", width=1000)
               )
      ),
      
      
      fluidRow (box(width=12,background ="light-blue", 
                    column(width=1,offset=2, h4("", style="padding:10px;")),
                    img(src="disc.jpg", width=650)
                    #img(src="m1.jpg", width=476) 
                    
      ),
      box(width=12,background ="green", 
          column(width=1,offset=2, h4("", style="padding:10px;")),
          img(src="Refs.jpg", width=650)
          # #     #img(src="m1.jpg", width=476) 
          
      )
      ),
      
      fluidRow (box(width=12,background ="light-blue", 
                    column(width=1,offset=2, h4("", style="padding:10px;")),
                    img(src="otherp.jpg", width=650)
                    #img(src="m1.jpg", width=476) 
                    
      ),
      
      tags$head(tags$style("#text1{color: red;
                           font-size: 20px;
                           font-style: italic;
                           }"
      )),
      
      box(width=12,background ="black", title="Links",
          fluidRow(column(width=1,offset=3, h4("", style="padding:10px;")),
                   url <- a("Click here for ILRI website", href="https://www.ilri.org/")),
          fluidRow(column(width=1,offset=3, h4("", style="padding:10px;")),
                   url <- a("Click here for SEI website", href="https://www.sei.org")),
          fluidRow(column(width=1,offset=3, h4("", style="padding:10px;")),
                   url <- a("Click here for LocateIT website", href="http://www.locateit.co.ke")),
          fluidRow(column(width=1,offset=3, h4("", style="padding:10px;")),
                   url <- a("Click here for GITHUB link", href="https://github.com/ilri/CLEANED-R")),
          fluidRow(column(width=1,offset=3, h4("", style="padding:10px;")),
                   url <- a("Click here for CLEANED tool documentation", href="https://cgspace.cgiar.org/handle/10568/33745"))
          #onclick=sprintf("window.open('%s')", url)
          #tagList("URL link:", url)
      )
      
      
    
      
      
    
      
      )
      
      ),
      #Second tab item
      # tabItem(tabName="widgets1a",
      #         
      #         url <- a("Click here for the French Version of CLEANED", href="https://jrstimeseriesforecast.shinyapps.io/versionfrench2018/")
      #         
      # ),
      
      #Third tab item
      tabItem(tabName="widadv",

      url <- a("Click here for Advanced User Interface of CLEANED", href="https://ilri.shinyapps.io/cleaned-r-resless-atsbi_eth_ex/")#, 
      # onclick=sprintf("window.open('%s')", url) ,
      #tagList("URL link:", url)
      ),
      #Fourth tab item
      tabItem(tabName = "dashboard1",
              
  
  mainPanel(
    
    tabsetPanel(id = "inTabset",
                        tabPanel(title = "Productivity computation", value = "panel1", ""),
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
                             
                                 fluidRow(box(width=12, title = " ",height=700,
                                              actionButton("gogh", "Run Greenhouse Impact "),
                                              # numericInput("n", "n", 50),
                                              plotOutput("plot123"))),
                                 fluidRow(box(width=12, title = "Summary table green house Impact",
                                              #actionButton("gowt", " "),
                                              # numericInput("n", "n", 50),
                                              plotOutput("plotableghg"))
                                 )),
                        # tabPanel(title = "Biodiversity Impact", value = "panel3", 
                        #          
                        #          fluidRow(box(width=12, title = " ",height=700,
                        #                       actionButton("gobdiv", "Run biodivesity Impact"),
                        #                       # numericInput("n", "n", 50),
                        #                       plotOutput("plot12bid"))
                        #                   
                        #          ),
                        #          
                        #          fluidRow(box(width=4, title = "Summary table biodiversity Impact",
                        #                       #actionButton("gowt", " "),
                        #                       # numericInput("n", "n", 50),
                        #                       plotOutput("plotablediv"))
                        #          )),
                        tabPanel(title = "Soil Impact", value = "panel3", 
                                 fluidRow(box(width=12, background ="red",img(src="soilimpact.JPG", width=650))),
                                 fluidRow(box(actionButton("gosol", "Run soil Impact",icon = NULL), title = " ",height=1000, width=12,
                                              
                                              # numericInput("n", "n", 50),
                                              plotOutput("plotsoil"),align = "centre")
                                          
                                          # box(width=4, title = "Summary table soil Impact",
                                          #       #actionButton("gowt", " "),
                                          #      # numericInput("n", "n", 50),
                                          #       plotOutput("plotablesoil"))   
                                          
                                          
                                 ),
                                 
                                  fluidRow(box(width=12, title = "Summary table soil Impact",
                                              #actionButton("gowt", " "),
                                               # numericInput("n", "n", 50),
                                               plotOutput("plotablesoil")))
                                 )
  )),
  
  
  

            
  tags$head(tags$style(HTML("div.col-sm-6 {padding:1px}"))),

  
  
#   fluidRow(
#   box(status = "primary",width = 12, "PRODUCTION CATEGORIES :-----------:-----------:------------:-------------:------:-----:-----------: DATA ALREADY LOADED",background = "navy")
# ),
fluidRow(
  box(#status = "warning", title=" ", width = 12, "", background ="red",
    HTML('<script type="text/javascript">
        $(document).ready(function() {
         $("#DownloadButton").click(function() {
         $("#Download").text("Loading...");
         });
         });
         </script>
         '),
      div(style="height: 35px;", actionButton("go", "Click here to update feed basket")) ,width = 12, background ="red" )
  
 
  ),

fluidRow(box(width=12,
             #actionButton("go", " "),
             # numericInput("n", "n", 50),
             verbatimTextOutput("plot",  placeholder = TRUE)
             
             
)),

# fluidRow(box(width=4, background="red",
#          selectInput("preset", "Select [ 1 ] for the interface to use preset values or [ 0 ] for manual input", choices =c(0,1), selected = 0)
#              
# ),
# box(width=8,background = "light-blue",".",
#  
#     verbatimTextOutput("pretext",  placeholder = TRUE)
# )),



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
      # div(style="height: 35px;",textInput("1a", "", value="Number of troupeaux(herd)")),
      # div(style="height: 35px;",textInput("2a", "",value="Number of animals per troup")),
      textInput("3a", "",value="Number of Animals")
  ), 
  
  box(status = "warning", width = 2, "",background ="green",
      # div(style="height: 35px;",numericInput("tt", " Remain", 100, min = 60, max =1000000)),
      # div(style="height: 35px;",numericInput("att", "", 450, min = 60, max = 1000000)),
      numericInput("numcow_es", "", 22000, min = 0, max = 1000000)),

  
  box(status = "warning", width = 2, "",background ="green",
      # div(style="height: 35px;",numericInput("tpt", " Left", 500, min = 60, max =1000000)),
      # div(style="height: 35px;",numericInput("atpt", "", 100, min = 60, max = 1000000)),
      numericInput("numcow_sis", "", 19000, min = 0, max = 1000000)),
  
  
  box(status = "warning", width = 2,"",background ="green",
      
      # div(style="height: 35px;",numericInput("tl", "", 200, min = 60, max = 1000000)),
      # div(style="height: 35px;",numericInput("atl", "", 20, min = 60, max = 1000000)),
      numericInput("numcow_da", "", 10000, min = 0, max = 1000000)),
  
      
  box(status = "warning", width = 2, "",background ="green",
      
      # div(style="height: 35px;",textInput("mp11", "", value="N/A")),
      # div(style="height: 35px;",textInput("mp12", "", value="N/A")),
      numericInput("numcow_is", "", 500, min = 10, max = 1000000)),
   
  box(status = "warning", width = 2, " ",background ="green",
      # div(style="height: 35px;",textInput("mpkk", "", value="N/A")),
      # div(style="height: 35px;",textInput("mpkk1q", "", value="N/A")),
      numericInput("numsheep", "", 100000, min = 0, max = 1000000))#,

  
  # box(status = "warning", width = 2, " ",background ="green",
  #     # div(style="height: 35px;",textInput("mpkk2", "", value="N/A")),
  #     # div(style="height: 35px;",textInput("mpkk3", "", value="N/A")),
  #     numericInput("numcow_da", "", 22500, min = 0, max = 1000000))
   
  
),
# fluidRow(box(status = "warning", width = 2, " ",background ="green",
# numericInput("numcow_c", "Number of animals crossing area:", 200000, min = 10, max =1000000 ))),

fluidRow(
  box(status = "primary",width = 24, " SELECT PRESET SCENARIOS :-----------:-----------:------------:-------------:------:-----:-----------: ",background = "navy"),
 sidebarPanel(
    tabsetPanel(id = "tabset",
                tabPanel("LOCAL DAIRY CATTLE",
                         radioButtons("DD", "", choices =c("DD0", "DD1","DD2"), selected = "DD0")
                        
                ),
                tabPanel("FATTENING/REARING LOCAL CATTLE",
                         radioButtons("DF", "", choices=c('DF0','DF1','DF2'),selected='DF0')
                ),
                tabPanel("DRAFT CATTLE",
                         radioButtons("DA", "", choices=c('DA0','DA1'),selected='DA0')
                        
                ),
                tabPanel("SPECIALISED DAIRY CATTLE",
                         radioButtons("SD", "", choices=c('SD0','SD1','SD2') ,selected='SD0')
                        
                ),
                
                
                tabPanel("SHEEP",
                         radioButtons("SH", "", choices=c('SH0','SH1','SH2'),selected='SH0')
                        
                ),
                tabPanel("Crop productivity",
                         radioButtons("Ca", "", choices=c('Cr0','Cr1'),selected='Cr0')

                )#,
                
                # tabPanel("LUC",
                #          radioButtons("LUC", "", choices=c('LUC0','LUC1','LUC2','LUC3','LUC4'),selected='LUC0')
                #          
                # )
                
    )
    )
 
  ),
  
fluidRow(
  box(width=12, " Baseline Productivity",plotOutput("baseplot")))

)



)

)


# We'll save it in a variable `ui` so that we can preview it in the console
ui <- dashboardPage(
  dashboardHeader(title = "SAIRLA PROJECT : CLEANED INTERFACE SYSTEM",titleWidth=750,
                  
                   # tags$li(class = "dropdown",
                   #         tags$a(href="https://www.ilri.org", target="_blank", 
                   #                tags$img(height = "30px", alt="SNAP Logo", src="https://www.ilri.org/sites/default/files/styles/card_image_thumb/public/default_images/defautl_img.jpg")
                   #        ))),     
  
  
   tags$li(class = "dropdown",
           tags$a(href="www", target="_blank", 
                  tags$img(height = "60px", alt="SNAP Logo", src="crpa.jpg")
           ))
   ), 
  
  
  
  # dashboardSidebar(width=250,
  #                  sidebarMenu(
  #                    menuItem("SIMPLE USERS", tabName = "dashboard1", icon = icon("dashboard"))
  #                    #menuItem("ADVANCED USERS!", tabName = "widgets1", icon = icon("th"))
  #                    # menuItem("Vegetation mapping", tabName = "Veg", icon = icon("th"))
  #                  )),
  
 sidebar1,
 body
  
  
  
)






# Preview the UI in the console
#source("D:/MOSEVICTOR_PC/MOSE_E/ILRI_SHINY_APP2017/CLEANED_BURKINAFASO2017/CLEANED-Burkina/3-cleaned/Update_user_interface.R", local=TRUE)

server = function(input, output, session) {
  
  # observeEvent(input$go, {
  #   showModal(modalDialog(
  #     title = "Updating feed basket",
  #     "Dismiss this message when feed basket is updated and continue!"
  #   ))
  #   removeModal()
  # })
  # observeEvent(input$go){
  #   paste("hello")}
 
  
   pok<- eventReactive(input$go, {

     if(input$go==0)
       return()
    
     
     ####################################CLEANED version 2 for Burkina Faso ################################################

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
       
       
       preset <<-1 #if 1 the interface uses the presets if 0 or other it uses the manual input below. 
       setwd(path)
       scenario<<-read.csv('1-input/parameter/preset.csv',  skip=2)
       
       # select from the preset scenario
       
       DD <<- input$DD   #options are (DD0, DD1,DD2)
       DF <<- input$DF   #options are (DF0, DF1, DF2)
       DA <<- input$DA   #options are (DA0, DA1, DA2)
       SD <<- input$SD   #options are (SD0, SD1, SD2)
       SH <<- input$SH  # options are (SH0, SH1 and SH2)
       
       Cr<<- input$Ca  #options are (Cr0,Cr1)
       
       #give your scanario a name 
       name<<-input$name
       
       #############################manual defintion of the parameters#####################
       
       
       #seasonality 
       # climate seasonality # based on usaid livelihood zones 
       ds<<- 8 # dry season
       ws<<- 12-ds # wet season  
       
       #DEFINE THE DIFFERENT SYSTEM 
       #################################
       
       # the dual system dairy
       #define liveweigtht in kg for the the breed in the extensive system (200)
       lwes<<-0 
       #define milk yield (kg/cow/year) for the breed in the extensive system (400) #500 for health
       myes<<-0 # 5 litre a day for 220 days 
       
       des<<-0
       
       #dry season 
       #natural grass in percent (51)
       efng1<<- 0
       #crop residues cerals (49)
       efrc1<<- 0
       #crop residue legumes (0) 
       efrl1<<-0
       #planted fodder (0)
       efpf1<<-0
       # concentrates cereal (maize bran) (0)
       efconc1<<- 0
       # concentrates oilseed cake (0)
       efconos1<<- 0
       #hey()
       efhay1<<-0
       # silage made from grass
       efsil1<<-0
       
       #wet season  
       #natural grass in percent (98)
       efng2<<- 0  
       #crop residues cerals (2)
       efrc2<<- 0 
       #crop residue legumes (0) 
       efrl2<<-0
       #planted fodder (0)
       efpf2<<-0  #
       # concentrates cereal (maize bran) (0)
       efconc2<<- 0
       # concentrates oilseed cake (0)
       efconos2<<- 0
       #hey()
       efhay2<<-0
       # silage made from grass
       efsil2<<-0
       #check if a 100%
       
       
       # manure management in the extensive system  system in percent (100%)
       #(if ipcc=1, then no need to adjust this )
       es_lagoon_perc<<- 00
       es_liquidslurry_perc<<-00
       es_solidstorage_perc<<-00
       es_drylot_perc<<-00
       es_pasture_perc<<-100
       es_dailyspread_perc<<-00
       es_digester_perc<<-00
       es_fuel_perc<<-00
       es_other_perc<<-00
       
       # dual system meat 
       
       #define liveweigtht in kg for the the breed in the dual purpose system  (200)
       lwsis<<-0  
       #define milk yield (kg/cow/year) for the breed in the semi intensive system (2000) 
       mysis<<-0
       
       #feed basket  for dual systeme meed
       #dressing percentage 
       dsis <<- 0
       
       #feed basket  semi-intensive  system season dry
       #natural grass in percent (33)
       sfng1<<-0
       #crop residues cereals (35)
       sfrc1<<-0
       #crop residue from rice
       sfrr1<<- 0
       #crop residue legumes (12)
       sfrl1<<-0
       #planted fodder (10)
       sfpf1<<-0
       # concentrates cereal (maize bran) (5)
       sfconc1<<- 0
       # concentrates oilseed cake (5)
       sfconos1<<- 0
       #hey()
       sfhay1<<-0
       # silage made from grass
       sfsil1<<-0
       
       
       
       # wet season 
       #natural grass in percent (57)
       sfng2<<-0
       #crop residues cereals (10)
       sfrc2<<-0
       #crop residue from rice (0)
       sfrr2<<-0
       #crop residue legumes (5) 
       sfrl2<<-0
       #planted fodder (14)
       sfpf2<<-0
       # concentrates cereal (maize bran) (5)
       sfconc2<<- 0
       # concentrates oilseed cake (9)
       sfconos2<<- 0
       #hey()
       sfhay2<<-0
       # silage made from grass
       sfsil2<<-0
       
       # manure management in the semi-intensive system in percent (100%)
       #(if ipcc=1, then no need to adjust this )
       sis_lagoon_perc<<- 00
       sis_liquidslurry_perc<<-00
       sis_solidstorage_perc<<-00
       sis_drylot_perc<<-00
       sis_pasture_perc<<-00
       sis_dailyspread_perc<<-80
       sis_digester_perc<<-00
       sis_fuel_perc<<-00
       sis_other_perc<<-20
       
       
       
       ## draft system 
       
       #define liveweigtht in kg for the the breed as draft animals (220)
       lwda<<-0
       
       #define milk yield (kg/cow/year) fro the draft animal (1000)
       myda<<-0 
       # dressing percenatge
       dda<<- 0 
       
       #feed basket for dairy wet season
       #natural grass in percent (15)
       dafng1<<-0
       #crop residues cerals (40)
       dafrc1<<-0
       #crop residue from rice
       dafrr1<<-0
       #crop residue legumes (30)
       dafrl1<<-0
       #planted fodder ()
       dafpf1<<-0
       # concentrates cereal (maize bran) (5)
       dafconc1<<- 0
       # concentrates oilseed cake (10)
       dafconos1<<- 0
       #hey()
       dafhay1<<-0
       # silage made from grass
       dafsil1<<-0
       
       
       
       
       #feed basket for dairy dry season
       #natural grass in percent (60)
       dafng2<<-0
       #crop residues cerals (0)
       dafrc2<<-0
       #crop residue from rice
       dafrr2<<- 0 
       #crop residue legumes (10)
       dafrl2<<-0
       #planted fodder (10)
       dafpf2<<-0
       # concentrates cereal (maize bran) (20)
       dafconc2<<- 0
       # concentrates oilseed cake (10)
       dafconos2<<- 0
       #hey()
       dafhay2<<-0
       # silage made 
       dafsil2<<-0
       
       # manure management in the semi-intensive system in percent (100%)
       #(if ipcc=1, then no need to adjust this )
       da_lagoon_perc<<- 00
       da_liquidslurry_perc<<-00
       da_solidstorage_perc<<-100
       da_drylot_perc<<-00
       da_pasture_perc<<-00
       da_dailyspread_perc<<-00
       da_digester_perc<<-00
       da_fuel_perc<<-00
       da_other_perc<<-00
       
       
       
       ################################################### specialized dairy system 
       #define liveweigtht in kg for the the breed in the intensive system (300)
       lwis<<-0
       
       #define milk yield (kg/cow/year) for the breed in the intensive system (7500)
       myis<<-0
       
       #feed basket for intensive system
       #dry season
       #natural grass in percent (20)
       ifng1<<-0
       #crop residues cerals (30)
       ifrc1<<-0
       #crop residue from rice (0)
       ifrr1<<-0
       #crop residue legumes (10)
       ifrl1<<-0
       #planted fodder (30)
       ifpf1<<-0
       # concentrates cereal (maize bran) (10)
       ifconc1<<- 0
       # concentrates oilseed cake (10)
       ifconos1<<- 0
       #hey()
       ifhay1<<-0
       # silage made from grass
       ifsil1<<-0
       
       
       
       #feed basket for dairy wet season
       #natural grass in percent (30)
       ifng2<<-0
       #crop residues cerals (10)
       ifrc2<<-0
       #crop residue from rice (0)
       ifrr2<<-  0
       #crop residue legumes (5)
       ifrl2<<-0
       #planted fodder (35)
       ifpf2<<-0
       # concentrates cereal (maize bran) (10)
       ifconc2<<- 0
       # concentrates oilseed cake (10)
       ifconos2<<- 0
       #hey()
       ifhay2<<-0
       # silage made from grass
       ifsil2<<-0
       
       # manure management in the semi-intensive system in percent (100%)
       #(if ipcc=1, then no need to adjust this )
       is_lagoon_perc<<- 00
       is_liquidslurry_perc<<-00
       is_solidstorage_perc<<-00
       is_drylot_perc<<-00
       is_pasture_perc<<-00
       is_dailyspread_perc<<-90
       is_digester_perc<<-5
       is_fuel_perc<<-00
       is_other_perc<<-5
       
       
       ####### sheep 
       #define liveweigtht in kg for the the breed as draft animals (20)
       lwsh<<-0
       
       #define dressing persentage
       dsh <<-0
       
       #feed basket for dairy wet season
       #natural grass in percent (15)
       shfng1<<-0
       #crop residues cerals (40)
       shfrc1<<-0
       #crop residue from rice
       shfrr1<<-0
       #crop residue legumes (30)
       shfrl1<<-0
       #planted fodder ()
       shfpf1<<-0
       # concentrates cereal (maize bran) (5)
       shfconc1<<- 0
       # concentrates oilseed cake (10)
       shfconos1<<- 0
       #hey()
       shfhay1<<-0
       # silage made from grass
       shfsil1<<-0
       
       
       #feed basket for sheep dry season
       #natural grass in percent (60)
       shfng2<<-0
       #crop residues cerals (0)
       shfrc2<<-0
       #crop residue from rice
       shfrr2<<- 0 
       #crop residue legumes (10)
       shfrl2<<-0
       #planted fodder (10)
       shfpf2<<-0
       # concentrates cereal (maize bran) (20)
       shfconc2<<- 0
       # concentrates oilseed cake (10)
       shfconos2<<- 0
       #hey()
       shfhay2<<-0
       # silage made 
       shfsil2<<-0
       
       sh_pasture_perc<<-50
       
       
       
       #######################################################################################
       #global variable definition
       
       #ipcc= 1 the code will use ipcc tier 2 standards for manure storage in stead of the user defined one
       ipcc<<-0
       
       
       #exogenous yield productivity gain in percentage of yield
       #crop
       pgc<<- 0.0
       #legumes
       pgl<<- 0.0
       #planted fodder
       pgpf<<- 0.0
       #grassland
       pgg<<- 0.0
       
       
       
       #linking the manure availability to the production system 
       mprod_es<<- 2 #manure production from a cow in the dual system - rearing/fattening and draf per day
       mprod_da<<- 2 #manure production from a cow in the dual system - rearing/fattening and draf per day
       mprod_sis<<- 3 #manure production from a cow from in the dual system lactating animals per day
       mprod_is<<- 4  #manure production from a cow in the specialized dairy per day
       mprod_sh<<- 0.1
       
       #percent of stored manure applied to the different crop
       #cereal (mprod_c *% to this crop for linking with production )
       manc<<- 0.8
       # legumes  (mprod_c *% to this crop for linking with production )
       manl<<- 0
       #planted fodder  (mprod_c *% to this crop for linking with production )
       manpf<<- 0
       #rice  (mprod_r *% to this crop for linking with production )
       manr<<- 0 
       #grazing land  (mprod_r *% to this crop for linking with production )
       mangraz<<- 0 
       
       # application of slurry kg/ha
       #cereal ()
       sluc<<- 0
       # legumes (0)
       slul<<- 0
       #planted fodder ()
       slupf<<- 0
       #grazing land
       slugraz<<- 0
       #rice land
       slur<<- 0
       
       
       slurryconv<<- 0.001 #conversion rate between slurry (NPK) and Nitrogen
       #we need a source here What about compost and other manure. 
       
       #inorganic fertilizer application in kg per hectare
       
       #cereal (50 is recommended)
       fertc<<- 0
       #rice (50 is recommended)
       fertr<<- 0
       # legumes (0)
       fertl<<- 0
       #planted fodder 
       fertpf<<- 0
       #grazing land
       fertgraz<<- 0
       
       Fertconv<<- 0.2 #conversion rate between fertilizer (NPK) and Nitrogen, depends on the locally available ferilizer, +/- 20%
       # from impact lit we know that DAP is most commonly used - Joanne is looking for conversion rates
       
       
       #exogenous yield productivity gain in percentage of yield
       #crop
       pgc<<- 0.0
       #legumes
       pgl<<- 0.0
       #planted fodder
       pgpf<<- 0.0
       #grassland
       pgg<<-  0.0
       
       # rice 
       pgr<<-  0.0
       
       #############soil management option on cropland (ghg)
       perc_til<<-  0 #percentage of cropland that is tilled 
       perc_redtil <<-  100 #percentage of cropland that is on reduced till
       perc_notil <<-  0 #percentage of cropland that is on no till
       
       perc_inlow <<-  100  #percentage of land with low input 
       perc_inmedium <<-  0  #percentage of land with medium input 
       perc_inhighnoman <<- 0  #percentage of land with high input no manure 
       perc_inhighman <<- 0   #percentage of land with high input with manure
       
       
       
       
       
       
       
       
       
       
       
       
     
     setwd(path)
     

     setwd(path)
     
     
     source('2-feedbasket_nonlinear2.r')
   
     
     paste("Feed basket parameters updated for this session.")
     
     
   })

  
   
output$plot <- renderText(
  
 
    { 
      withProgress(pok(), message="Please wait: Updating feed basket.")
      
      pok()


      })

  pok1<- eventReactive(input$gowt, {

    if(input$gowt==0)
       return()
setwd(path)

   source('1-water.r')
})

  
 
  output$plot12 <- renderPlot(
    { 
      withProgress(pok1(), message="Please wait: Running Water Impact." )
      
      pok1()
      
    })
  
  

  pok1t<- eventReactive(input$gowt, {

    if(input$gowt==0)
      return()
    grid.table(water_ind2)

    })

  
 
 
  
  
  
  
  output$plotable <- renderPlot(
   { pok1t()


  })
  
  
  
  

  pok1gh<- eventReactive(input$gogh, {

    if(input$gogh==0)
      return()
    setwd(path)
    source('1-ghg.r')
    })

  output$plot123 <- renderPlot(
    { 
      
      withProgress(pok1gh(), message="Please wait: Running Greenhouse Gas Impact." )
      pok1gh()


    })

  pok1tgh<- eventReactive(input$gogh, {

    if(input$gogh==0)
      return()
    grid.table(Coe_ind2)
  
  })


  
  
  
  
  output$plotableghg <- renderPlot(
    { pok1tgh()


    })

  # pok1bdiv<- eventReactive(input$gobdiv, {
  # 
  #   if(input$gobdiv==0)
  #     return()
  #   setwd(path)
  #   source('1-biodiv.r')
  #   })

  # output$plot12bid <- renderPlot(
  #   { 
  #     
  #     withProgress(pok1bdiv(), message="Please wait: Running Biodiversity Impact." )
  #     
  #     pok1bdiv()
  # 
  # 
  #   },height = 450, width = 800)
  # 
  # pok1tbiv<- eventReactive(input$gobdiv, {
  # 
  #   if(input$gobdiv==0)
  #     return()
  #   grid.table(bio_ind2)
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
   source('1-soil.r')
    })

  output$plotsoil <- renderPlot(
    { 
      
      withProgress(psoil(), message="Please wait: Running Soil Impact." )
      
      psoil()
      
    } ,height = 800, width = 800)

  ptsoil<- eventReactive(input$gosol, {

    if(input$gosol==0)
      return()
      
    grid.table(soil_ind2)
   
  })

  output$plotablesoil <- renderPlot(
    { 
      ptsoil()


    })


  ptbaseprod<- eventReactive(input$go, {
    
    if(input$go==0)
      return()
    
    grid.table(prod_ind2)

  })
  
  output$baseplot <- renderPlot(
    { ptbaseprod()
      
      
    })
   
  observeEvent(input$preset, {
    
    output$pretext<-renderText({
      
      if(input$preset==0){
      paste("0 is selected : The interface is using manually entered inputs. ")
      }
      else{if(input$preset==1){paste("1 is selected : The interface is using preset values: SELECT PRESET SCENARIOS BELOW.")}
      
                }})
})
#)

}

shinyApp(ui = ui,server)

