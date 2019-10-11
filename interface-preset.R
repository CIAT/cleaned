####################################################################################################
#
# Shiny App for CLEANED Tanzania January 2018 by Catherine Pfeifer

#based on CLEANED System ILRI of  May 2017 by Victor N. Mose and Caroline Mburu
####################################################################################################
## app.R ##

rm(list=ls())
options(repos=c(CRAN="https://cran.rstudio.com"))
library(rsconnect)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(graphics)
library(rgdal)
library(rgeos)
library(sp)
library(raster)
library(maptools)
library(rgdal)
library(gridExtra)
library(grid)
library(RColorBrewer)


#path<<-'D:/MOSEVICTOR_PC/MOSE_E/ILRI_SHINY_APP2017/Cleaned Tanzania MASTER/Cleaned TanzaniaNOV2018'
path<<-'D:/Dropbox/Cleaned Tanzania' 

setwd(path)

source('2-load_data.r')


sidebar1<-dashboardSidebar(width=180,
                           sidebarMenu(
                             menuItem(""),
                             menuItem("ABOUT CLEANED TOOL", tabName = "widgets1", icon = icon("th")),
                             menuItem("SIMPLE USERS", tabName = "dashboard1", icon = icon("dashboard")),
                             menuItem("ADVANCED USERS", tabName = "dashboard2", icon = icon("dashboard"))
                             #menuItem("ADVANCED USERS!", tabName = "widgets1", icon = icon("th"))
                             # menuItem("Vegetation mapping", tabName = "Veg", icon = icon("th"))
                           ))

body <- dashboardBody( 
  
  useShinyjs(),
  
  actionButton("hideSidebar", "Hide sidebar menu:",
               icon("angle-double-left"), 
               style="color: #fff; background-color: #8B2323; border-color: #2e6da4"),
  
  actionButton("showSidebar", "Show sidebar menu:",
               icon("angle-double-right"), 
               style="color: #fff; background-color: #8B2323; border-color: #2e6da4"),
  
  
  tags$head(tags$style(HTML(".grViz { width:100%!important; font-size:10px}"))),
  
  tabItems(
    
    tabItem(tabName ="widgets1",
            
            fluidRow (
              
              
              box(width=12,
                  
                  column(width=1,offset=2, h4("", style="padding:100px;")),
                  img(src="Buks1.jpg" , width=650),                                         
                  
                  background ="blue"),
              
              
              
              box(width=12,background ="green", 
                  column(width=1,offset=2, h4("", style="padding:100px;")),
                  # div(img(...), style="text-align: center;").
                  
                  div(img(src="paramTZ.jpg",style="text-align: center;", width=650))
                  #img(src="m1.jpg", width=476)
                  
              )
            ),
            
            fluidRow(box(width=12, background ="blue",title="Study area: Lushoto District, Tanga in the Highlands of Northern Tanzania. ",
                         column(width=1,offset=2, h4("", style="padding:10px;")),
                         img(src="Tanzania_studyarea.jpg", width=650))
                     # box(width=12,background ="green",
                     #     column(width=1,offset=2, h4("", style="padding:10px;")),
                     #     img(src="Buks2.jpg", width=650)
                     # )
            ),
            
            
            fluidRow (box(width=12,background ="light-blue",
                          column(width=1,offset=2, h4("", style="padding:10px;")),
                          img(src="disc.jpg", width=650)
                          #img(src="m1.jpg", width=476)
                          
            ),
            box(width=12,background ="green", 
                column(width=1,offset=2, h4("", style="padding:10px;")),
                img(src="otherp.jpg", width=650)
                # #     #img(src="m1.jpg", width=476) 
                
            )
            ),
            
            fluidRow (box(width=12,background ="light-blue", 
                          column(width=1,offset=2, h4("", style="padding:10px;")),
                          img(src="Refs.jpg", width=650)
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
                # fluidRow(column(width=1,offset=3, h4("", style="padding:10px;")),
                #          url <- a("Click here for related publications", href="https://cgspace.cgiar.org/handle/10568/33745"))
                #onclick=sprintf("window.open('%s')", url)
                #tagList("URL link:", url)
            )
            
            )
            
            
            
            
            
            
            
            
            
            ),
    
    
    
    
    
    
    
    
    #First tabItem
    tabItem(tabName = "dashboard1",
            
            mainPanel(
              
              
              
              
              
              
              tabsetPanel(id = "inTabset",
                          tabPanel(title = "Productivity", value = "panel1", ""),
                          tabPanel(title = "Water impact", value = "panel2",
                                   fluidRow(box(width=12, background ="blue",img(src="waterimpact.jpg", width=650))),
                                   fluidRow(box(width=12, title = "", height=700,
                                                actionButton("gowt", "Run the water impact"),
                                                
                                                plotOutput("plot12"))),
                                   
                                   fluidRow(box(width=12, title = "Summary table water impact",
                                                
                                                
                                                plotOutput("plotable"))
                                   )
                          ),
                          tabPanel(title = "Greenhouse gas impact", value = "panel3", 
                                   
                                   
                                   fluidRow(box(width=12, background ="green",img(src="greenhouseimpact.jpg", width=650))),
                                   fluidRow(box(width=12, title = " ",height=700,
                                                actionButton("gogh", "Run Greenhouse impact "),
                                                # numericInput("n", "n", 50),
                                                plotOutput("plot123"))),
                                   
                                   fluidRow(box(width=12, title = "Summary table Greenhouse impact",
                                                #actionButton("gowt", " "),
                                                # numericInput("n", "n", 50),
                                                plotOutput("plotableghg"))
                                   )),
                          tabPanel(title = "Biodiversity impact", value = "panel3", 
                                   
                                   fluidRow(box(width=12, background ="light-blue",img(src="biodiversityimpact.jpg", width=650))),
                                   fluidRow(box(width=12, title = " ",height=700,
                                                actionButton("gobdiv", "Run biodivesity impact"),
                                                # numericInput("n", "n", 50),
                                                plotOutput("plot12bid"))),    
                                   
                                   
                                   
                                   fluidRow(box(width=12, title = "Summary table biodiversity impact",
                                                #actionButton("gowt", " "),
                                                # numericInput("n", "n", 50),
                                                plotOutput("plotablediv"))
                                   )),
                          tabPanel(title = "Soil impact", value = "panel3", 
                                   
                                   fluidRow(box(width=12, background ="red",img(src="soilimpact.jpg", width=650))),
                                   fluidRow(box(actionButton("gosol", "Run soil impact",icon = NULL), title = " ",height=1000, width=12,
                                                
                                                # numericInput("n", "n", 50),
                                                plotOutput("plotsoil"),align = "centre")),      
                                   
                                   
                                   # fluidRow(box(actionButton("gosol", "Run soil impact",icon = NULL), title = " ",height=1000, width=12,
                                   #              
                                   #           
                                   #              plotOutput("plotsoil"),align = "centre")
                                   #         
                                   #                                       ),
                                   
                                   fluidRow(box(width=12, title = "Summary table soil impact",
                                                #actionButton("gowt", " "),
                                                # numericInput("n", "n", 50),
                                                plotOutput("plotablesoil"))))
              )),
            
            tags$head(tags$style(HTML("div.col-sm-6 {padding:1px}"))),
            
            #tabItems(
            #tabItem(tabName = "dashboard1",
            #h2("ADVANCED USER INTERFACE"),
            
            #tags$head(tags$style(HTML("div.col-sm-6 {padding:1px}"))),
            
            #   fluidRow(
            #   box(status = "primary",width = 12, "PRODUCTION CATEGORIES :-----------:-----------:------------:-------------:------:-----:-----------: DATA ALREADY LOADED",background = "navy")
            # ),
            fluidRow(
              box(#status = "warning", title=" ", width = 12, "", background ="red",
                div(style="height: 35px;", actionButton("go", "Click here to update productivity")) ,width = 12, background ="red" )),
            
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
              box(status = "warning", width = 2, "Extensive-local breed",background ="maroon"),
              box(status = "warning", width = 2,"Semi-intensive - crossbreed",background ="maroon"),
              box(status = "warning", width = 2, "Intensive - pure breed",background ="maroon")),
            
            
            
            fluidRow(
              box(status = "warning", width = 2, "", background ="green",
                  # div(style="height: 35px;",textInput("1a", "", value="Number of troupeaux(herd)")),
                  # div(style="height: 35px;",textInput("2a", "",value="Number of animals per troup")),
                  textInput("3a", "",value="Number of Animals")
                  
                  
              ), 
              
              
              box(status = "warning", width = 2, "",background ="green",
                  
                  # div(style="height: 35px;",textInput("mp11", "", value="N/A")),
                  # div(style="height: 35px;",textInput("mp12", "", value="N/A")),
                  numericInput("numcow_es", "", 25000, min = 0, max = 1000000)),
              
              box(status = "warning", width = 2, " ",background ="green",
                  # div(style="height: 35px;",textInput("mpkk", "", value="N/A")),
                  # div(style="height: 35px;",textInput("mpkk1q", "", value="N/A")),
                  numericInput("numcow_sis", "", 15000, min = 0, max = 1000000)),
              
              
              box(status = "warning", width = 2, " ",background ="green",
                  # div(style="height: 35px;",textInput("mpkk2", "", value="N/A")),
                  # div(style="height: 35px;",textInput("mpkk3", "", value="N/A")),
                  numericInput("numcow_is", "", 0, min = 0, max = 1000000))
              
              
            ),
            
            fluidRow(
              box(status = "primary",width = 12, " SELECT PRESET SCENARIOS :-----------:-----------:------------:-------------:------:-----:-----------: ",background = "navy"),
              sidebarPanel(
                tabsetPanel(id = "tabset",
                            tabPanel("Local breed",
                                     radioButtons("L", "", choices =c("L0", "L1","L2"), selected = "L0")
                                     
                            ),
                            tabPanel("Cossbreed",
                                     radioButtons("Cb", "", choices=c('Cb0','Cb1','Cb2'),selected='Cb0')
                            ),
                            tabPanel("Mostly exotic breed",
                                     radioButtons("Eb", "", choices=c('E0','E1','E2'),selected='E0')
                                     
                            ),
                            # tabPanel("Fattening",
                            #          radioButtons("Fa", "", choices=c('FBR','F1','F2') ,selected='FBR')
                            #         
                            # ),
                            # 
                            # 
                            # tabPanel("Draft",
                            #          radioButtons("Tr", "", choices=c('TBR','T1'),selected='TBR')
                            #         
                            # ),
                            tabPanel("Crop",
                                     radioButtons("Cr", "", choices=c('Cr0','Cr1'),selected='Cr0')
                                     
                            ),
                            
                            
                            
                            tabPanel("land use change",
                                     fluidRow( box(status = "warning", width = 10, "add crop land in percent of baseline of biomass ",background ="aqua",
                                                   # div(style="height: 35px;",textInput("mpkk", "", value="N/A")),
                                                   # div(style="height: 35px;",textInput("mpkk1q", "", value="N/A")),
                                                   numericInput("add", "", 0, min = 0, max = 100))
                                               
                                     ))
                            
                )
              )
              
            ),
            
            fluidRow(
              box(width=12, "Production and area needs",plotOutput("baseplot")))
            
    ),
    
    tabItem(tabName = "dashboard2",
            url <- a("Click here for the Advanced/Expert version of the CLEANED tool", href="https://ilri.shinyapps.io/cleaned-r-resless-lushoto_tza_ex/")
          
            )
    
            )
  
  )



# We'll save it in a variable `ui` so that we can preview it in the console
ui <- dashboardPage(
  dashboardHeader(title = "SAIRLA PROJECT : CLEANED INTERFACE FOR TANZANIA ",titleWidth=750,
                  
                  # tags$li(class = "dropdown",
                  #         tags$a(href="https://www.ilri.org", target="_blank", 
                  #                tags$img(height = "30px", alt="SNAP Logo", src="https://www.ilri.org/sites/default/files/styles/card_image_thumb/public/default_images/defautl_img.jpg")
                  #         ))
                  #),     
                  
                  tags$li(class = "dropdown",
                          tags$a(href="www", target="_blank", 
                                 tags$img(height = "60px", alt="SNAP Logo", src="crpaaseiatz.jpg")
                          ))),
  
  
  
  
  sidebar1,
  
  
  body
  
  
  
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
  
  
  
  
  
  
  
  pok<- eventReactive(input$go, {
    
    if(input$go==0)
      return()
    
    
    ####################################CLEANED version 2 for Tanzania################################################
    # cleaned version 2
    # code by Catherine Pfeifer, ILRI, c.pfeifer@cgiar.org
    #code created on 20.6.2016
    #modified 25.1.2018
    #clearing all memory 
    rm(list=ls(all=TRUE))
    #set path to cleaned tool
    
    
    #path<<-'D:/MOSEVICTOR_PC/MOSE_E/ILRI_SHINY_APP2017/BURKINA_FASO_CLEANED_7.11.2017/Cleaned - Burkina'
    #path<<-'D:/MOSEVICTOR_PC/MOSE_E/ILRI_SHINY_APP2017/UPDATED2017NOV/CLEANED-Burkina6.11.2017/Cleaned - Burkina'
    setwd(path)
    
    ###################################This sheet defines all user defined variables#################
    #defining the number of animal in each system 
    
    
    # in the extenive system 
    numcow_es <<-input$numcow_es
    
    
    # in the semi-intenive system
    numcow_sis<<-input$numcow_sis 
    
    # in the intensive system 
    numcow_is <<-input$numcow_is 
    
    
    # presets 
    preset <<-1 #if 1 the interface uses the presets if 0 or other it uses the manual input below. 
    scenario<<-read.csv('1-input/parameter/preset.csv',  skip=2)
    
    # select from the preset scenario
    
    L<<-input$L  #options are (L0, L1,L2)
    Cb<<-input$Cb  #options are (Cb0, Cb1, Cb2)
    Eb<<-input$Eb  #options are (E0, E1, E2)
    Cr<<-input$Cr #options are (Cr0,Cr1)
    
    
    #Ca<<-input$Ca # 'CBR' #options are (CBR, C1)
    add<<-input$add # should be at 0 or user defined > 0
    
    #give your scanario a name 
    # name<<-"My Own"
    # if(name==""){ name<<-"My Own"} else {}
    name<<-input$name
    
    #############################manual defintion of the parameters#####################
    
    
    #seasonality 
    #seasonality 
    # climate seasonality 
    ds<<- 4 # dry season
    ws<<- 12-ds # wet season  
    
    #DEFINE THE DIFFERENT SYSTEM 
    #################################
    
    # the extensive agro pastoral system
    
    
    # definition de l'animal extensif transhumant
    #define liveweigtht in kg for the the breed in the transhumant system (200)
    lwes<<-200 
    # definition de l'animal extensif laitier
    
    #define milk yield (kg/cow/year) for the breed in the extensive system (400) 
    myes<<-1000 
    
    #feed basket for the extensive system
    # dry season 
    #natural grass in percent (95)
    efng1<<- 0  
    #crop residues cerals (0)
    efrc1<<- 0
    #crop residue from rice
    efrr1<<-100  
    #crop residue legumes (10) 
    efrl1<<-0
    #planted fodder (0)
    efpf1<<-0  
    # concentrates cereal (maize bran) (0)
    efconc1<<- 0
    # concentrates oilseed cak
    #natural grass in percent (100)
    efng2<<- 50  
    #crop residues cerals (0)
    efrc2<<- 45 
    #crop residue from rice
    efrr2<<-5 
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
    #do by season 
    
    
    
    efconc1<<- 0
    efconos1<<- 0
    #hey()
    efhay1<<-0
    # silage made from grass
    efsil1<<-0
    
    #season wet season  
    ####################################### the semi-intensive system ###################
    
    
    
    #define liveweigtht in kg for the the breed in the semi intensive system (250)
    lwsis<<-220 
    #define milk yield (kg/cow/year) for the breed in the semi intensive system (0)
    mysis<<-2000
    
    #feed basket  for semi-intensive system 
    #dry season 
    #natural grass in percent (15)
    sfng1<<-80
    #crop residues cereals (40)
    sfrc1<<-0
    #crop residue from rice
    sfrr1<<- 0
    #crop residue legumes (30)
    sfrl1<<-0
    #planted fodder ()
    sfpf1<<-0 
    # concentrates cereal (maize bran) (5)
    sfconc1<<- 10
    # concentrates oilseed cake (10)
    sfconos1<<- 10  
    #hey()
    sfhay1<<-13
    # silage made from grass
    sfsil1<<-0
    
    # dry season 
    #natural grass in percent (50)
    sfng2<<-25
    #crop residues cereals (0)
    sfrc2<<-40
    #crop residue from rice
    sfrr2<<-10 
    #crop residue legumes (5) 
    sfrl2<<-0
    #planted fodder (12)
    sfpf2<<-5 
    # concentrates cereal (maize bran) (20)
    sfconc2<<- 10
    # concentrates oilseed cake (10)
    sfconos2<<- 10  
    #hey()
    sfhay2<<-0
    # silage made from grass
    sfsil2<<-0
    
    
    
    # manure management in the fattening system in percent (100%)
    #(if ipcc=1, then no need to adjust this )
    sis_lagoon_perc<<- 00
    sis_liquidslurry_perc<<-00
    sis_solidstorage_perc<<-100
    sis_drylot_perc<<-00
    sis_pasture_perc<<-00
    sis_dailyspread_perc<<-00
    sis_digester_perc<<-00
    sis_fuel_perc<<-00
    sis_other_perc<<-00
    
    
    ################################# intensive system ############
    
    
    #define liveweigtht in kg for the the breed in dairy system (250)
    lwis<<-250
    
    #define milk yield (kg/cow/year) for the breed in the dairy system (1000)
    myis<<-2500 
    
    #feed basket for dairy wet season
    #natural grass in percent (15)
    ifng1<<-80
    #crop residues cerals (40)
    ifrc1<<-0
    #crop residue from rice
    ifrr1<<-0
    #crop residue legumes (30)
    ifrl1<<-0
    #planted fodder ()
    ifpf1<<-0
    # concentrates cereal (maize bran) (5)
    ifconc1<<- 10
    # concentrates oilseed cake (10)
    ifconos1<<- 10
    #hey()
    ifhay1<<-15
    # silage made from grass
    ifsil1<<-5
    
    #feed basket for dairy dry season
    #natural grass in percent (60)
    ifng2<<-20
    #crop residues cerals (0)
    ifrc2<<-25
    #crop residue from rice
    ifrr2<<- 0 
    #crop residue legumes (10)
    ifrl2<<-15
    #planted fodder (10)
    ifpf2<<-0
    # concentrates cereal (maize bran) (20)
    ifconc2<<- 20
    # concentrates oilseed cake (10)
    ifconos2<<- 20
    #hey()
    ifhay2<<-0
    # silage made from grass
    ifsil2<<-0
    
    
    # manure management in the semi-intensive system in percent (100%)
    #(if ipcc=1, then no need to adjust this )
    is_lagoon_perc<<- 00
    is_liquidslurry_perc<<-00
    is_solidstorage_perc<<-100
    is_drylot_perc<<-00
    is_pasture_perc<<-00
    is_dailyspread_perc<<-00
    is_digester_perc<<-00
    is_fuel_perc<<-00
    is_other_perc<<-00
    
    #######################################################################################
    #global variable definition
    #ipcc= 1 the code will use ipcc tier 2 standards for manure storage in stead of the user defined one
    ipcc<<-0 
    
    #########################parmeters specific to the soil pathway##################
    
    
    #linking the manure availability to the production system 
    mprod_es<<- 2 #manure production from a cow in the traditional system per day
    mprod_sis<<- 3 #manure production from a cow from semi-intensive cross breed system
    mprod_is<<- 3 #manure production from a cow in the fattening system per day
    
    
    
    #percent of stored manure applied to the different crop
    #cereal (mprod_c *% to this crop for linking with production )
    manc<<-0.4
    # legumes  (mprod_c *% to this crop for linking with production )
    manl<<-0
    #planted fodder  (mprod_c *% to this crop for linking with production )
    manpf<<- 0
    #rice  (mprod_r *% to this crop for linking with production )
    manr<<- 0.4 
    #grazing land  (mprod_r *% to this crop for linking with production )
    mangraz<<-0 
    
    # application of slurry kg/ha
    #cereal ()
    sluc<<-0
    # legumes (0)
    slul<<-0
    #planted fodder ()
    slupf<<-0
    #grazing land
    slugraz<<-0
    #rice land
    slur<<-0
    
    
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
    pgl<<-0.0
    #planted fodder
    pgpf<<-0.0
    #grassland
    pgg<<- 0.0
    
    # rice 
    pgr<<- 0.0
    
    #############soil management option on cropland (ghg)
    perc_til<<- 0 #percentage of cropland that is tilled 
    perc_redtil <<- 100 #percentage of cropland that is on reduced till
    perc_notil <<- 0 #percentage of cropland that is on no till
    
    perc_inlow <<- 100  #percentage of land with low input 
    perc_inmedium <<- 0  #percentage of land with medium input 
    perc_inhighnoman <<-0  #percentage of land with high input no manure 
    perc_inhighman <<-0   #percentage of land with high input with manure
    
    
    #####reading some r info
    setwd(path)
    pixel<<-read.csv('1-input/parameter/pixel.csv')
    pixel<<-as.numeric(pixel[2])
    
    ##############################land use driven scenarios
    library(raster)
    
    setwd(path)
    
    #source('3-cleaned/2-load_data.r')
    setwd(path)
    
    source('2-feedbasket_nonlinear2.r')
    
    paste("Productivity parameters updated for this session.")
  })
  
  
  output$plot <- renderText(
    { 
      withProgress(pok(), message="Please wait: Updating Productivity.")
      pok()
      
      
    })
  
  pok1<- eventReactive(input$gowt, {
    
    if(input$gowt==0)
      return()
    setwd(path)
    source('1-water.r')})
  
  output$plot12 <- renderPlot(
    {  withProgress(pok1(), message="Please wait: Running Water Impact" )
      
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
      withProgress(pok1gh(), message="Please wait: Running Greenhouse gas Impact" )
      
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
  
  pok1bdiv<- eventReactive(input$gobdiv, {
    
    if(input$gobdiv==0)
      return()
    setwd(path)
    source('1-biodiv.r')
  })
  
  output$plot12bid <- renderPlot(
    { 
      
      withProgress(pok1bdiv(), message="Please wait: Running Biodiversity Impact" )
      
      pok1bdiv()
      
      
    },height = 450, width = 800)
  
  pok1tbiv<- eventReactive(input$gobdiv, {
    
    if(input$gobdiv==0)
      return()
    grid.table(bio_ind2)
  })
  
  output$plotablediv <- renderPlot(
    { pok1tbiv()
      
      
    })
  
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