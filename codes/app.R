library(fs)
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
#   setwd('C:/GoogleDrive/TNC/Products/h_Dashboard/')
print(paste('WD: ', getwd()))

#fs:file_show(path(getwd(), "Manual_Dashboard.pdf"))

load('3410spTraits_step6.RData') #  spColDf; str(spColDf)
load('metricsCompiled.RData') #  metricsCompiled; str(metricsCompiled)
load('MatrizCovariables.RData')
aoiR <<- raster('id1km_1335403cell.tif')

mets <- metricsCompiled
mets$Route[is.na(mets$Route)] <- ''
spColDf$redlistCategory[is.na(spColDf$redlistCategory)] <- ''

noEmptyCells <- which(!is.na(aoiR[]))

isShpLoad <<- FALSE
shp <<- NULL

xlsTable <<- data.frame(sp = c('Empty', 'Empty'))
origMapIndex <<- newMapIndex <<- raster(matrix(0))

gcs <- "+proj=longlat +ellps=GRS80 +no_defs"

# UI  ---------------
ui <- dashboardPage(
  skin = 'purple',
  dashboardHeader( 
    ## ------------------------------
    tags$li(a(href = 'https://www.nature.org/es-us/sobre-tnc/donde-trabajamos/tnc-en-latinoamerica/colombia/',
              img(src = 'TNClogoPrimary_OU_RGB_Colombia.png',
                  title = "TNC Colombia", height = "30px"),
              style = "padding-top:10px; padding-bottom:10px;"),
            class = "dropdown"),
    title = "Análisis de especies para las rutas NCS",  titleWidth = 600
    ## ------------------------------
  ), ## End class
  dashboardSidebar(disable = TRUE),
  dashboardBody( 
    tags$head(tags$style(
      HTML('.skin-blue .main-header .logo {
          background-color: #3c8dbc;
        }.skin-blue .main-header .logo:hover {
          background-color: #3c8dbc;
        }.nav>li>a {
        padding: 1vh 8vw;
        }'))),
    tabBox( 
      width = NULL,
      tabsetPanel( 
        type = 'pills',
        ####### UI Maps ------------
        tabPanel(
          id = 'maps', 
          title = 'Mapear índices', ## Tomar índices calculados y ponerlos en mapas
          fluidRow(h3(' ')),
          h5(''),
          fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
            column(width = 4,
                   selectInput(inputId = "maps_met", label = "Tipo índice espacial: ", 
                               choices = c(unique(metricsCompiled$Metric)[c(1, 2, 5, 3, 4)]), selected = 'Region'),
                   selectInput(inputId = "maps_ref", label = "Refinamiento coberturas: ", choices =  c('refined', 'nonrefined'), selected = 'refined'),
                   selectInput(inputId = "maps_rich", label = "Tipo especies: ", 
                               choices = c(unique(metricsCompiled$Index)), selected = 'ind0All'),
                   selectInput(inputId = "maps_class", label = "Grupo especies: ", 
                               choices = c(unique(metricsCompiled$Group)), selected = 'ALL'),
                   selectInput(inputId = "maps_route", label = "Ruta NCS: (no aplica para 'Region')", 
                               choices = na.omit(unique(metricsCompiled$Route))),
                   # selectInput(inputId = "maps_route", label = "Ruta NCS: ", 
                   #             choices = c(regionsTxt),
                   
                   actionButton("go_maps", HTML('<b>RUN!</b>')),
                   br(),
                   h6(HTML('--')),
                   downloadButton("maps_downtif", HTML('<b>Download TIF</b>')),
                   br(),
                   h6(HTML('--')),
                   downloadButton('maps_downloadDf', 'Descargar tabla de índices original'),
                   h6(HTML('--')),
                   actionButton("maps_manual", HTML('Abrir manual PDF'))
                   
                   
            ),
            
            column(width = 8, leafletOutput("maps_map", height = "600px") %>% withSpinner(color="#0dc5c1"))
          )
        ),
        
        ####### UI Species ------------
        
        tabPanel(
          id = 'species', 
          title = 'Consultar especies', ## Consultar especies basado en un mapa
          fluidRow(h3(' ')),
          h5(''),
          fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
            column(
              width = 5,
              h5('Se aceptan capas polígonos con una geometría sencilla que pueden estar en formatos ESRI Shapefile comprimidos en ZIP o individuales (mínimo .shp, .shx, .dbf), 
               GeoJSON, SQLite y Geopackage'),
              fluidRow(
                
                column(width = 10,
                       shiny::fileInput(
                         'shapefile', 'Polígono/AOI',
                         accept=c('.shp','.dbf','.sbn','.sbx',
                                  '.shx',".prj", '.zip', '.gpkg', '.SQLite', '.GeoJSON'),
                         multiple=TRUE)
                ),
                column( width = 2, h6(''), br(), actionButton("loadShp", "Cargar!") )
              ),
              # selectInput(inputId = "maps_red", label = "Lista roja: ", 
              #             choices = c(unique(spColDf$redlistCategory), 'ALL'), selected = 'ALL'),
              #
              
              leafletOutput("species_map", height = "400px")%>% withSpinner(color="#0dc5c1"),
              
              fluidRow(
                column(width = 8, 
                       selectInput(inputId = "species_ref", label = "Refinamiento coberturas: ", 
                                   choices =  c('refined', 'nonrefined'), selected = 'refined')),
                column(width = 4,  h5(''), br(), actionButton("go_species", HTML('<b>Run</b>')))
                
              ),
              h6(HTML('--')),
              actionButton("species_manual", HTML('Abrir manual PDF'))              
            ),
            column(width = 7, dataTableOutput('species_table') %>% withSpinner(color="#0dc5c1"), 
                   downloadButton('species_downloadDf', 'Descargar tabla'))
          )
        ),
        
        ####### UI Index ------------
        tabPanel(
          id = 'indexes',
          title = 'Obtener cifras',
          fluidRow(h3(' ')),
          h5(''),
          fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
            column(width = 6,
                   h6(HTML('--')),
                   selectInput(inputId = "indexes_ref", label = "Refinamiento coberturas: ", 
                               choices =  c('refined', 'nonrefined'), selected = 'refined'),
                   strong('R query:'),
                   textInput(width = '100%', inputId = "indexes_sql", label = "", value = "", 
                             placeholder = "Query here: className == 'AVES' & end == TRUE | redlistCategory != 'Least Concern'"),
                   # fluidRow(
                   #   column(width = 1, strong('SQL:')),
                   #   column(width = 11, textInput(width = '100%', inputId = "species_sql", label = "", value = "", 
                   #                                placeholder = "SQL here: className == 'AVES' & end == TRUE | redlistCategory != 'Least Concern'"))
                   # ),
                   fluidRow(
                     column(width = 4, actionButton("validatesql", HTML('<b>Validate query</b>'))),
                     column(width = 8, textOutput("validtxt"))
                   ),
                   h6(HTML('')),
                   downloadButton('downloadSpDf', 'Descargar tabla de especies original'),
                   h6(HTML('--')),
                   fluidRow(
                     column(width = 2, actionButton("go_indexes", HTML('<b>Run</b>'))),
                     column(width = 4, downloadButton("indexes_downtif", HTML('<b>Download TIF</b>')))
                     
                   ),
                   h6(HTML('--')),
                   actionButton("index_manual", HTML('Abrir manual PDF'))
            ),
            column(width = 6, leafletOutput("indexes_map", height = "650px") %>% withSpinner(color="#0dc5c1"))
          ),
        ) # close tab
        ####### close tabs ------------
        
      ) # tabsetPanel
    ) # tabbox
  ) # dashboardbody
)




##### SERVER ----------------------
server <- function(input, output, session) {
  
  ##### Default widgets ----------------------
  
  output$species_table <- renderDataTable(NULL)
  
  ##### Leaflets ----------------------

  output$maps_map <- output$species_map <- output$indexes_map <- renderLeaflet({
    reactShp$leaf0  %>%
      leaflet.extras::addDrawToolbar(targetGroup='draw', polylineOptions = FALSE,
                                     rectangleOptions = FALSE, circleOptions = FALSE,
                                     markerOptions = FALSE, circleMarkerOptions = FALSE,
                                     editOptions = leaflet.extras::editToolbarOptions())
    })

  reactShp <- reactiveValues(shp = FALSE,
                             leaf0 = leaflet()%>% addTiles() %>%
                               setView(lng = -73.7, lat = 4.28, zoom = 6))

  
  observeEvent(input$maps_manual, {fs::file_show(path(getwd(), "Manual_Dashboard.pdf"))})
  observeEvent(input$species_manual, {fs::file_show(path(getwd(), "Manual_Dashboard.pdf"))})
  observeEvent(input$index_manual, {fs::file_show(path(getwd(), "Manual_Dashboard.pdf"))})
  
    ##### Load shp  ----------------------
  
  observeEvent(input$loadShp, {
    inFiles <- input$shapefile
    if ( class(inFiles) != "NULL" ){
      if ( nrow(inFiles) == 1 & grepl('*\\.zip', inFiles$name)){ ## zip files
        outDir <- paste0(tempdir(), '2'); dir.create(outDir)
        outZip <- tempfile(tmpdir = outDir); dir.create(outZip)
        unzip(zipfile = inFiles$datapath, exdir = outZip)
        uZ <- list.files(outZip)
        shp <<- tryCatch(readOGR(verbose = FALSE, 
                                 outZip, layer = tools::file_path_sans_ext(uZ[1])), error = function (e) NULL)
        if(class(shp) == 'SpatialPolygonsDataFrame'){
          # if( min(shp@bbox['x', ])>-180 & max(shp@bbox['x', ])<180 &
          #     min(shp@bbox['y', ])>-90 & max(shp@bbox['y', ])<90 )  #><
          # {
            print("loaded!")
            isShpLoad <<- TRUE
            reactShp$shp <- TRUE
          # }
        }
        unlink(outDir,recursive = TRUE, force = TRUE)
      } else if ( nrow(inFiles) == 1 & grepl('\\.SQLite|\\.gpkg|\\.GeoJSON', inFiles$name)){ ## single
        shp <<- tryCatch(readOGR(inFiles$datapath[1], verbose = FALSE), error = function (e) NULL)
        if(class(shp) == 'SpatialPolygonsDataFrame'){
          # if( min(shp@bbox['x', ])>-180 & max(shp@bbox['x', ])<180 &
          #     min(shp@bbox['y', ])>-90 & max(shp@bbox['y', ])<90 )  #><
          # {
            print("loaded!")
            isShpLoad <<- TRUE
          # }
        }
      } else if ( nrow(inFiles) >= 3  & all(sapply(c('\\.shp', '\\.shx', '\\.dbf'), grep, inFiles$name))){ ## shp several
        inFiles$datapath2 <- gsub('\\/[0-9]\\.', '/1.', inFiles$datapath)
        sapply(inFiles$datapath,USE.NAMES = F, function(x){
          file.rename(x, 
                      gsub('\\/[0-9]\\.', '/1.', x)
          )
        })
        
        shp <<- tryCatch(readOGR(verbose = FALSE, dirname(inFiles$datapath2[1]),
                                 basename(tools::file_path_sans_ext(inFiles$datapath2[1]))), 
                         error = function (e) NULL)
        
        if(class(shp) == 'SpatialPolygonsDataFrame'){
          # if( min(shp@bbox['x', ])>-180 & max(shp@bbox['x', ])<180 &
          #     min(shp@bbox['y', ])>-90 & max(shp@bbox['y', ])<90 )  #><
          # {
            print("loaded!")
            isShpLoad <<- TRUE
          # }
        }
      }
      
      if (isShpLoad){
        
        if(is.na(shp@proj4string@projargs)){
          shp@proj4string@projargs <- gcs
        } else if(shp@proj4string@projargs != gcs){
          shp <<- spTransform(shp, CRSobj = CRS(gcs))
        }
        
        reactShp$shp <- TRUE
        reactShp$leaf0 <- leaflet()%>% addTiles() %>% addPolygons(data = shp)
        output$loadMap <- renderLeaflet({reactShp$leaf0})
        
       }
    }
  })
  
  ##### Go buttons maps  ----------------------  

  isolate(observeEvent(input$go_maps,{

  # input <- list(maps_ref = 'nonrefined',
  #               maps_met = 'RoutesVals',
  #               maps_class = 'ALL',
  #               maps_rich = 'ind0All',
  #               maps_route = 'ata_acm',
  #               maps_region = 'Amazonas')
    
      isolate(output$maps_map <- isolate(renderLeaflet({
        
        print('Calc - maps')
        # print(ls(all.names = TRUE))
        # if('varsMat' %in% ls(all.names = TRUE)){
        #   print('Load varsMat')
        #   load('MatrizCovariables.RData')
        #   varsMat <<- varsMat
        # }
        
        indR <-  aoiR * 0
        selVar0 <- isolate(subset(mets, 
                          mets$spGroup == input$maps_ref &
                            mets$Index == input$maps_rich & 
                            mets$Group == input$maps_class))
        
        isolate(if ( input$maps_met %in% c('Region') ){
          isolate(selVar <- subset(selVar0, selVar0$Metric == input$maps_met))
          for(i in 1:nrow(selVar)){
            isolate(pos <- which(varsMat$region == selVar$Region[i]))
            isolate(indR[varsMat$ID[pos]] <- selVar$spCount[i])
          }
        } else if ( input$maps_met %in% c('Routes') & !is.na(input$maps_route) ){
          isolate(selVar <- subset(selVar0, 
                           selVar0$Metric == input$maps_met &
                             selVar0$Route == input$maps_route))
          for(i in 1:nrow(selVar)){
            isolate(pos <- which(is.na(varsMat[, input$maps_route])))
            isolate(indR[varsMat$ID[pos]] <- selVar$spCount[i])
          }
        } else if(input$maps_met %in% c('RoutesByReg')) {
          
          isolate(selVar <- subset(selVar0, selVar0$Metric == 'RoutesByReg' & 
                             selVar0$Route == input$maps_route))
          for(i in 1:nrow(selVar)){
            isolate(pos <- which(is.na(varsMat[, input$maps_route]) & varsMat$region == selVar$Region[i]))
            isolate(indR[varsMat$ID[pos]] <- selVar$spCount[i])
          }
        } else if(input$maps_met %in% c('RoutesVals')) {
          isolate(selVar <- subset(selVar0, selVar0$Metric == 'RoutesVals' & 
                             selVar0$Route == input$maps_route))
          for(i in 1:nrow(selVar)){
            isolate(pos <- which( varsMat[, input$maps_route]  == selVar$RouteVal[i]))
            isolate(indR[varsMat$ID[pos]] <- selVar$spCount[i])
          }
          
        } else if(input$maps_met %in% c('RoutesValByReg')) {
          isolate(selVar <- subset(selVar0, selVar0$Metric == 'RoutesValByReg' & 
                             selVar0$Route == input$maps_route))
          for(i in 1:nrow(selVar)){
            isolate(pos <- which( varsMat[, input$maps_route]  == selVar$RouteVal[i] & 
                            varsMat$region == selVar$Region[i]))
            isolate(indR[varsMat$ID[pos]] <- selVar$spCount[i])
          }
        })
        
        rastVals <-  unique(values(indR))
        pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), rastVals, na.color = "transparent")
        origMapIndex <<- indR
        print('Done - maps')
        
        reactShp$leaf0 %>% addRasterImage(indR, colors = pal, opacity = 0.8) %>% 
          addLegend(pal = pal, values = rastVals, title = "Legend")
        
      })))
  }))
  
  
  ##### Go buttons validate taxonomy  ----------------------  
  isolate(observeEvent(input$validatesql,{
    if(input$indexes_sql != ''){
      sql <- tryCatch(eval(parse(text = paste0("subset(spColDf, ",input$indexes_sql, ")"))), error = function (e) NULL)
      if(class(sql) == 'data.frame'){
        sqlRes <- (paste0('Valid query for species. ', nrow(sql), ' species'))
      } else {
        sqlRes <- (paste0('No valid query for species. Using all the species'))
      }
    } else{
      sqlRes <- (paste0('Using all the species'))
      }
      output$validtxt <- renderText({ sqlRes })
  }))
  
  
  
  ##### Go buttons species  ----------------------  
  
  isolate(observeEvent(input$go_species,{

    output$species_table <- renderDataTable({
      isolate(polDraw <- input$species_map_draw_new_feature)
    if( !is.null(polDraw) | !is.null(shp) ) {
      print('sp_map')
      if (!is.null(shp) ){ 
        polAoi <- shp
      } else if(!is.null(polDraw)){
        coordMat <- (do.call(rbind, polDraw$geometry$coordinates[[1]]))
        coordMat <- (data.frame(x = unlist(coordMat[, 1]),
                       y = unlist(coordMat[, 2])))
        
        polAoi <- SpatialPolygonsDataFrame(SpatialPolygons(list(Polygons(list(Polygon(coordMat)),1))), 
                                           data = data.frame(id = 1))
      }
      
      
      posIdAoiC <- tryCatch(crop(aoiR, polAoi), error = function(e) NULL)
      isolate(if( is.null(posIdAoiC) ){ # out of area of interest
        sql <- data.frame(sortID = NA, species = 'No data available in this region', className = NA, presence = NA)
      } else {
        posIdAoi <- mask(posIdAoiC, polAoi) # plot(posIdAoi)
        
        if ( ncell(posIdAoi) == 1 & is.na(posIdAoi[1]) ){
          sql <- data.frame(sortID = NA, species = 'Define a bigger region', className = NA, presence = NA)
          
        } else {
          posAoi <- na.omit(posIdAoi[])
          if(all(is.na(posAoi))){
            sql <- data.frame(sortID = NA, species = 'No data available in this region', className = NA, presence = NA)
          } else {
            sql <- spColDf
            sql$presence <- 0
            if( input$species_ref == 'refined'){
              rdataPath <- 'species_vectors_refined/'
              for(m in 1:nrow(sql)){ # m = 1
                spTry <- tryCatch(load(paste0(rdataPath, gsub('\\.tif$', '.RData', sql$tifName[m]))), error = function(e) NULL) # spVec
                sql$presence[m] <- max(spVec[posAoi], na.rm = TRUE)
              }
            } else {
              rdataPath <- 'species_vectors_nonrefined/'
              for(m in 1:nrow(sql)){ # m = 1
                spTry <- tryCatch(load(paste0(rdataPath, gsub('\\.tif$', '.RData', sql$tifName[m]))), error = function(e) NULL) # spVec
                sql$presence[m] <- max(spVecnr[posAoi], na.rm = TRUE)
              }
            }
          }
        }
      })
      
      print('sp_map done')
      xlsTable <<- sql
      
    }
    xlsTable[, c('sortID', 'species', 'className',  'presence')]
  })
    
  })) ## ending go button
  
  
  ##### Go buttons indexes  ----------------------  
  isolate(observeEvent(input$go_indexes, {
    
    output$indexes_map <- renderLeaflet({
    if(input$indexes_sql != ''){
      sql <- tryCatch(eval(parse(text = paste0("subset(spColDf, ",input$indexes_sql, ")"))), error = function (e) NULL)
      if(class(sql) == 'data.frame'){
        print(paste0('Valid SQL query for species. ', nrow(sql), ' species'))
      } else {
        print(paste0('No valid SQL query for species. Using all the species'))
        sql <- spColDf
      }
    } else {
      sql <- spColDf
    }
    
    indexTemp <- aoiR * 0
    if( input$indexes_ref == 'refined'){
      rdataPath <- 'species_vectors_refined/'
      for(m in 1:nrow(sql)){ # m = 1
        spTry <- tryCatch(load(paste0(rdataPath, gsub('\\.tif$', '.RData', sql$tifName[m]))), error = function(e) NULL) # spVec
        indexTemp[noEmptyCells] <-  indexTemp[noEmptyCells] + spVec
      }
      
      
    } else {
      rdataPath <- 'species_vectors_nonrefined/'
      for(m in 1:nrow(sql)){ # m = 1
        spTry <- tryCatch(load(paste0(rdataPath, gsub('\\.tif$', '.RData', sql$tifName[m]))), error = function(e) NULL) # spVec
        indexTemp[noEmptyCells] <-  indexTemp[noEmptyCells] + spVecnr
      }
    }
    
    rastVals <-  unique(values(indexTemp))
    pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), rastVals, na.color = "transparent")
    newMapIndex <<- indexTemp
    print('Done - species')
    
    reactShp$leaf0 %>% addRasterImage(indexTemp, colors = pal, 
                                      opacity = 0.8, layerId = "rastvalues") %>% 
      addLegend(pal = pal, values = rastVals, title = "Species")#  %>% 
      #addMouseCoordinates() %>%
      #addImageQuery(indexTemp, type="mousemove", layerId = "values")
    })
  }))
      
  output$maps_downloadDf <- downloadHandler(
    filename = 'indicesOriginal.xlsx', 
    content = function(file) {
      xlsx::write.xlsx(metricsCompiled, file)
    })
  
  
  output$species_downloadDf <- downloadHandler(
    filename ='spListQuery.xlsx', 
    content = function(file) {
      print('Export spp list query')
      xlsx::write.xlsx(xlsTable, file)
    })
  
  output$downloadSpDf <- downloadHandler(
    filename = 'spListOriginal.xlsx', 
    content = function(file) {
      xlsx::write.xlsx(spColDf, file)
    })
  
  
  output$maps_downtif <- downloadHandler(
    filename = 'origIndexMap.tif', 
    content = function(file) {
      #print('Export tif')
      writeRaster(origMapIndex, file, format = 'GTiff')
      #resx <- writeRaster(indRexport , filename = paste0(getwd(), '/spAnsRaster', gsub('[a-zA-Z]|[:punct::]| ', '', Sys.time()), '.tif'), format = 'GTiff', overwrite=TRUE)
      #print(paste(resx@file@name))
    })
  
  output$indexes_downtif <- downloadHandler(
    filename = 'newIndexMap.tif', 
    content = function(file) {
      writeRaster(newMapIndex, file, format = 'GTiff')
    })
} # End server

shinyApp(ui, server)
