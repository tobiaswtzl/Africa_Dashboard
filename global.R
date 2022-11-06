###############################################################################
##Global File
##############################################################################

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