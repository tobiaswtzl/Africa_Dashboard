################################################################################
#BMZ Africa Dashboard
################################################################################

#checks if pacman package exists, if not install
if (!require("pacman")) install.packages("pacman")

#install/load packages
pacman::p_load("shiny",
               "dplyr",
               "here",
               "leaflet",
               "shinyWidgets",
               "plotly",
               "shinydashboard"
)

#load data
load(here("data", "data_memphis.rdata"))
load(here("data", "data_boxes.rdata"))
load(here("data", "data_referate.rdata"))
load(here("data", "map_data.rdata"))
load(here("data", "ipc_glossar.rdata"))

################################################################################
###Shiny App
################################################################################

#UI
ui <- dashboardPage(
  
  #define header
  dashboardHeader(title = "Afrika Dashboard"),
  
  #sidebar
  dashboardSidebar(
    
    #menu item, depended on renderMenu in Server
    sidebarMenuOutput("menu"),
    
    #Sidebar for first  menuitem (aktuelle Lage)
    conditionalPanel(
      condition = "input.tabs == 'aktuelle_daten'",
      awesomeRadio(
        inputId = "food_radio",
        label = "Kartenindikator auswählen", 
        choices = c("Ernährungssicherheit", "Veränderung Food Basket Preis"),
        selected = "Ernährungssicherheit",
        status = "warning"
      )
    ),
    
    #Sidebar for second menuitem (Bilaterale EZ (BMZ))
    conditionalPanel(
      condition = "input.tabs == 'ez_daten'",
      
      #choose date range
      sliderInput("date_range",
                  "Gewünschter Zeitraum",
                  value = c(2005, max(data_memphis$year)), 
                  min = min(data_memphis$year),
                  max = max(data_memphis$year),
                  step = 1,
                  sep = ""
      ),
      
      #Select DO
      pickerInput(
        inputId = "select_do", 
        label = "Do auswählen", 
        choices = c(
          "KfW" = "Kreditanstalt für Wiederaufbau (KFW)",
          "GIZ" = "Deutsche Gesellschaft für Internationale Zusammenarbeit (GIZ) GmbH",
          "PTB" = "Physikalisch-Technische Bundesanstalt (PTB)",
          "BGR" = "Bundesanstalt für Geowissenschaften und Rohstoffe (BGR)"
        ), 
        options = list(
          `actions-box` = TRUE, 
          size = 10,
          `selected-text-format` = "count > 3"
        ), 
        selected = c(
          "Kreditanstalt für Wiederaufbau (KFW)",
          "Deutsche Gesellschaft für Internationale Zusammenarbeit (GIZ) GmbH",
          "Physikalisch-Technische Bundesanstalt (PTB)",
          "Bundesanstalt für Geowissenschaften und Rohstoffe (BGR)"
        ),
        multiple = TRUE
      ),
      
      #siwtch gebucht / belegte gelder
      switchInput("switch_money", label = "Gelder", onLabel = "Belegt", offLabel = "Gesamt"),
      
      #Ez per capita
      switchInput("per_capita", label = "Gelder", onLabel = "Pro Kopf", offLabel = "Absolut"),
      
      #Reset Button
      actionButton("reset_all", "Reset auf Voreinstellungen")
    )
  ),
  
  ################################################################################
  #Main body
  ################################################################################
  
  dashboardBody(
    
    #avoid white flashing when refreshing data
    tags$style(".recalculating { opacity: inherit !important; }"),
    
    #change position of icon in value box
    tags$head(tags$style(HTML('.small-box .icon-large {top: 5px;}'))),
    
    #add space between sidebar items
    tags$style(".sidebar-menu li { margin-bottom: 20; }"),
    
    #define size of valueboxes
    tags$head(tags$style(HTML(".small-box {height: 200px}"))),
    
    #increase font size of sidebar
    tags$style(HTML(".main-sidebar { font-size: 20px; }")),
    
    tabItems(
      
      #Current situation
      tabItem(tabName = "aktuelle_daten",
              #Main page: left
              column(width = 6,
                     
                     #absolute values
                     box(
                       title = "Aktuelle Lage",
                       width = NULL,
                       status = "primary",
                       leafletOutput("africa_map_food", height = "67vh"), height = "72vh"
                     ),
                     
                     
                     #Additional information
                     box(
                       fluidRow(
                         
                         #info Box food basket price change
                         valueBoxOutput("hungerBox"),
                         
                         #info box demonstrations
                         valueBoxOutput("foodbasketBox"),
                         
                         #info box violence
                         valueBoxOutput("disasterBox")
                       ),
                       width = NULL,
                       status = "primary",
                       
                     )
                     
              ),
              #Main page: right
              column(width = 6.5,
                     
                     #zusätzliche Informationen
                     tabBox(title = "Zusätzliche Informationen",
                            id = "right_top",
                            
                            #Plot of chosen data
                            tabPanel(
                              title = "Grafische Darstellung",
                              width = NULL,
                              status = "primary",
                              div(
                                plotlyOutput("plot_aktuelle_lage", height = "67vh"),
                                
                                #fix height
                                style = 'height:790px;'
                              )
                            ),
                            
                            #Einschaetzung Regionalreferat
                            tabPanel(
                              title = "Einschätzung Regionalreferat",
                              width = NULL,
                              status = "primary",
                              div(
                                htmlOutput("einschaetzung_referate"),
                                
                                #fix height, enable scrollbar
                                style = 'overflow-y:scroll;height:790px;'
                              )
                            ),
                            
                            #Glossar
                            tabPanel(
                              title = "Glossar",
                              width = NULL,
                              status = "primary",
                              div(
                                tableOutput("ipc_glossar"),
                                
                                #fix height, enable scrollbar
                                height = "72vh", style = "overflow-y:scroll;font-size:120%; height:790px"
                              )
                            ),
                            
                            #Quellen
                            tabPanel(
                              title = "Quellen", width = NULL, status = "primary",
                              div(
                                tableOutput("quellen"),
                                
                                #fix height
                                height = "72vh", style = "overflow-y:scroll; font-size:120%; height:790px"
                              )
                            )
                            
                            
                            #Additional information
                     ),
                     box(
                       fluidRow(
                         
                         #info Box food basket price change
                         valueBoxOutput("displacementBox"),
                         
                         #info box demonstrations
                         valueBoxOutput("violenceBox"),
                         
                         #info box violence
                         valueBoxOutput("demoBox")
                       )
                     )
              )
              
      ),
      
      #Memphis Data
      tabItem(tabName = "ez_daten",
              
              #Main page: left
              column(width = 6,
                     
                     #Africa map
                     box(
                       title = "Bilaterale EZ des BMZ",
                       width = NULL,
                       status = "primary",
                       leafletOutput("africa_map", height = "67vh"), height = "72vh"
                     ),
                     
                     #progress bar Belegung Mittel
                     box(
                       title = "Belegung der Haushaltmittel", width = NULL, status = "primary",
                       plotlyOutput("gebucht_bar", height = "10vh")
                     )
              ),
              
              #Main page: right
              column(width = 6.5,
                     
                     #absolute values
                     tabBox(title = "Gesamtüberblick",
                            id = "right_top",
                            
                            #Kernthemen
                            tabPanel(
                              title = "Kernthemen",
                              width = NULL,
                              status = "primary",
                              plotlyOutput("kernthema_pie", height = "40vh")
                            ),
                            
                            #Referate
                            tabPanel(
                              title = "Referate",
                              width = NULL,
                              status = "primary",
                              plotlyOutput("referate_pie", height = "40vh")
                            ),
                            
                            #Fem EZ
                            tabPanel(
                              title = "Fem. EZ",
                              width = NULL,
                              status = "primary",
                              plotlyOutput("fem_ez_pie", height = "40vh")
                            ),
                            
                            #height
                            height = "50vh"
                     ),
                     
                     #values over time
                     tabBox(title = "Zeitverlauf",
                            id = "right_bottom",
                            
                            #Kernthemen
                            tabPanel(
                              title = "Kernthemen",
                              width = NULL,
                              plotlyOutput("kernthema_area")
                            ),
                            
                            #Referate
                            tabPanel(
                              title = "Referate",
                              width = NULL,
                              plotlyOutput("referat_area")
                            ),
                            
                            #Fem EZ
                            tabPanel(
                              title = "Fem. EZ",
                              width = NULL,
                              plotlyOutput("fem_ez_area")
                            ),
                            
                            #height
                            height = "40vh"
                     )
                     
              )
      )
    )
  )
)