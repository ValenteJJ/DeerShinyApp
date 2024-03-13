
rm(list=ls())

library(shiny)
library(raster)
library(sf)
library(terra)
library(FedData)
library(tidyverse)
library(tigris)
library(shinydashboard)
library(data.table)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("getascii"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("stateName", "Select US State", choices = c("AK", "AL", "AR", "AZ", "CA", "CO", 
                                                                 "CT", "DC", "DE", "FL", "GA", "GU", "HI", "IA", "ID", "IL", "IN", 
                                                                 "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", 
                                                                 "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", 
                                                                 "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VI", "VT", 
                                                                 "WA", "WI", "WV", "WY"), selected = "AL"),
          selectInput("yearNum","Select Year for National Land Cover Database", choices = c ("2021", "2019", "2016", "2013", "2011", "2008", "2006", "2004","2001"), selected = "2021"),
          actionButton("btnSave", "Save State & Year"),
          selectInput("countyName", "Select county", choices = verbatimTextOutput("countyList")),
          actionButton("btnSave1", "Plot County"),
          actionButton("btnSave2", "Landcover Data"),
          actionButton("btnSave3", "Forest Cover"),
          actionButton("btnSave4", "Write ascii file")
          #downloadButton("downloadData", "Download file")
        ),

        mainPanel(
          fluidRow(
            verbatimTextOutput("countyList")
          ),
          fluidRow(
          splitLayout(cellWidths = c("33%", "33%", "34%"),
                      plotOutput("plot1"), plotOutput("plot2"), plotOutput("plot3"))
            )
          
        )
    )
)

server <- function(input, output, session) {
  
  nameList <- reactiveVal()
  observeEvent(input$btnSave, {
    countyShape = counties(state = input$stateName, cb = FALSE, year = input$yearNum)
    nameList(NULL)
    nameList(append(nameList(), countyShape$NAME))
    updateSelectInput(session, "countyName", choices = nameList())
  })
  output$countyList <- renderPrint(nameList())
  
  observeEvent(input$btnSave1, {
    output$plot1 <- renderPlot({
      countyShape = counties(state = input$stateName, cb = FALSE, year = input$yearNum)
      ##countyOfInterest = countyShape[which(countyShape$NAME==input$countyName),]
      countyOfInterest = countyShape %>% filter(NAME==input$countyName)
      p1 <- ggplot() +
        geom_sf(data = countyOfInterest, color="black",
                fill="white", size=0.25)
      p1
    })
  })
  
  observeEvent(input$btnSave2, {
    output$plot2 <- renderPlot({
      countyShape = counties(state = input$stateName, cb = FALSE, year = input$yearNum)
      ##countyOfInterest = countyShape[which(countyShape$NAME==input$countyName),]
      countyOfInterest = countyShape %>% filter(NAME==input$countyName)
      nlcdYear = 2019
      landMass = 'L48'
      directoryForStorage = getwd()
      ##
      countyBuffer = st_buffer(countyOfInterest, dist=2000)
      ##below replaced countyOfInterest with countyBuffer
      croppedNLCD = get_nlcd(countyBuffer,
                             label=input$countyName,
                             year = nlcdYear,
                             dataset = "landcover",
                             landmass = landMass,
                             extraction.dir = file.path(directoryForStorage, "FedData", "extractions", "nlcd"),
                             raster.options = c("COMPRESS=DEFLATE", "ZLEVEL=9"),
                             force.redo = TRUE
                             )
      #countyShape = as_Spatial(countyShape)
      countyShape = st_transform(countyOfInterest, crs(croppedNLCD))
      countyBuffer = st_transform(countyBuffer, crs(croppedNLCD))
      #countyShape = spTransform(countyShape, crs(croppedNLCD))
      p2 <- plot(croppedNLCD)
      p3 <- plot(st_geometry(countyShape), add=T, lwd=5)
      
      p3
      })
  })
  
  observeEvent(input$btnSave3, {
    output$plot3 <- renderPlot ({
      countyShape = counties(state = input$stateName, cb = FALSE, year = input$yearNum)
      ##countyOfInterest = countyShape[which(countyShape$NAME==input$countyName),]
      countyOfInterest = countyShape %>% filter(NAME==input$countyName)
      nlcdYear = 2019
      landMass = 'L48'
      directoryForStorage = getwd()
      ##
      countyBuffer = st_buffer(countyOfInterest, dist=2000)
      ##below replaced countyOfInterest with countyBuffer
      croppedNLCD = get_nlcd(countyBuffer,
                             label=input$countyName,
                             year = nlcdYear,
                             dataset = "landcover",
                             landmass = landMass,
                             extraction.dir = file.path(directoryForStorage, "FedData", "extractions", "nlcd"),
                             raster.options = c("COMPRESS=DEFLATE", "ZLEVEL=9"),
                             force.redo = TRUE
      )
      #countyShape = as_Spatial(countyShape)
      #countyShape = spTransform(countyShape, crs(croppedNLCD))
      countyShape = st_transform(countyOfInterest, crs(croppedNLCD))
      countyBuffer = st_transform(countyBuffer, crs(croppedNLCD))
      #Now we can reclassify the NLCD raster so that all forest has a value
      #of 1 and everything else has a value of 0
      reclassifyValues = matrix(c(1, 39, 0,
                                  40, 49, 1,
                                  50, 100, 0), nrow=3, ncol=3, byrow=T)
      forestNon = classify(croppedNLCD, rcl=reclassifyValues)
      forestNon = mask(forestNon, countyShape)
      
      #Now we can look at the distribution of forest (1 values) in the county
      #p3 <- plot(forestNon)
      #p3
      #increase the grain and calculate the mean
      aggForest <- aggregate(forestNon, fact=53, fun='mean')
      
      #Is there a specific projection that you need this in?
      crs(aggForest, proj=T)
      aggForest <- mask(aggForest, countyShape)
      p4.1 <- plot(aggForest)
      p4 <- plot(st_geometry(countyShape), add=T)
      p4
    })
  })
  
  observeEvent(input$btnSave4, {
    countyShape = counties(state = input$stateName, cb = FALSE, year = input$yearNum)
    ##countyOfInterest = countyShape[which(countyShape$NAME==input$countyName),]
    countyOfInterest = countyShape %>% filter(NAME==input$countyName)
    nlcdYear = 2019
    landMass = 'L48'
    directoryForStorage = getwd()
    
    ##
    countyBuffer = st_buffer(countyOfInterest, dist=2000)
    ##below replaced countyOfInterest with countyBuffer
    croppedNLCD = get_nlcd(countyBuffer,
                           label=input$countyName,
                           year = nlcdYear,
                           dataset = "landcover",
                           landmass = landMass,
                           extraction.dir = file.path(directoryForStorage, "FedData", "extractions", "nlcd"),
                           raster.options = c("COMPRESS=DEFLATE", "ZLEVEL=9"),
                           force.redo = TRUE
    )
    ##countyShape = as_Spatial(countyShape)
    ##countyShape = spTransform(countyShape, crs(croppedNLCD))
    countyShape = st_transform(countyOfInterest, crs(croppedNLCD))
    countyBuffer = st_transform(countyBuffer, crs(croppedNLCD))
    
    
    
    #Now we can reclassify the NLCD raster so that all forest has a value
    #of 1 and everything else has a value of 0
    reclassifyValues = matrix(c(1, 39, 0,
                                40, 49, 1,
                                50, 100, 0), nrow=3, ncol=3, byrow=T)
    forestNon = classify(croppedNLCD, rcl=reclassifyValues)
    forestNon = mask(forestNon, countyBuffer)
    #Now we can look at the distribution of forest (1 values) in the county
    #p3 <- plot(forestNon)
    #p3
    #increase the grain and calculate the mean
    aggForest <- aggregate(forestNon, fact=53, fun='mean')
    
    #Is there a specific projection that you need this in?
    crs(aggForest, proj=T)
    aggForest <- mask(aggForest, countyShape)
    
    #z <- raster(aggForest)
    z = as(aggForest, "Raster")
    writeRaster(z, filename=paste(input$countyName, ".asc"), format='ascii', overwrite=TRUE)
    #writeRaster(z, filename=paste(input$countyName, ".asc", sep=""), format='ascii', overwrite=TRUE)
    ##https://search.r-project.org/CRAN/refmans/RSAGA/html/read.ascii.grid.html
    #output$downloadData <- downloadHandler(
      #filename = function() {
        #paste("input$countyName-", Sys.Date(), ".asc", sep="")
      #},
      #content = function(file) {
        #z <- raster(aggForest)
        #writeRaster(z, filename="input$countyName.asc", format='ascii', overwrite=TRUE)
      #}
    #)
})
  
}

# Run the application 
shinyApp(ui = ui, server = server)
