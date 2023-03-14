library(shiny)
library(leaflet)
library(DBI)
library(RPostgreSQL)
library(RPostgres)
library(leaflet)
library(shinyWidgets)
library(DT)
library(glue)
library(shinycssloaders)
library(shinyalert)
library(shinyjs)
library(shinydashboard)
library(pool)
library(RColorBrewer)
library(sf)
library(tidyr)
library(dplyr)

# setwd("C:/Users/Romain/Downloads/PNA_Fadet_prospection")


region_NA <- st_read("departement.geojson")



js <- '
$(document).on("shiny:connected", function(){
  $("#map").css({
    width: window.innerWidth, 
    height: window.innerHeight
  });
  $(window).on("resize", function(e){
    if(e.target instanceof Window){
      $("#map").css({width: window.innerWidth, height: window.innerHeight});
    }
  });
})'

