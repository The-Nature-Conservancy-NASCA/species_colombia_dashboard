
## Define required packages
pckList <- c('rgdal', 'raster', 'sp', 'rJava',  'xlsx', 'xlsxjars',
             'shinydashboard', 'leaflet', 'leaflet.extras', 'shiny', 
                   'highcharter', 'shinycssloaders', 'fs')

## Install packages if not available
invisible(sapply(pckList, function(x){
  if (!x %in% rownames(installed.packages())){
    install.packages(x)
  }
}))


## Load libraries
library(highcharter)
library(leaflet)
library(leaflet.extras)
library(raster)
library(rgdal)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(sp)
library(xlsx)
library(fs)

## Here can be "warnings" but no "errors"
