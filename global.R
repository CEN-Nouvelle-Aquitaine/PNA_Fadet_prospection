library(shiny)
library(leaflet)
library(shinyWidgets)
library(DT)
library(shinycssloaders)
library(shinyalert)
library(shinyjs)
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

