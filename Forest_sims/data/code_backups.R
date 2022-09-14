###########################################################################################
pack<-list("shiny", "dplyr", "forcats", "leaflet", "opencage", 
           "highcharter","raster","ggplot2", "viridis", "RColorBrewer",
           "rgdal", "shapefile")
lapply(pack, "require", character.only = TRUE)

###Data
eco<-readRDS("./data/change_raster.rds")
clim<-readRDS("./data/temp_pr_change.rds")
county<-readOGR("./data/KE_Admin1_pro.shp")

ui<-fluidPage(theme = "style.css",
              div(style = "padding: 1px 0px; width: '100%'",
                  titlePanel(
                    title = "",
                    windowTitle = "Climate Change and ecosystem services in Kenya"
                  )
              ),
              navbarPage(
                
                # Application title.
                title = div(span(img(src = ""),
                                 "Climate change in Kenya",
                                 style = "position: relative; top: 50%; transform: translateY(-50%);")),
                # Demographics.
                tabPanel(
                  
                  "Future Climate",
                  
                  # One tab for each plot/table.
                  tabsetPanel(
                    
                    type = "tabs",
                    
                    # Circle-packing plot of ethnicity and gender.
                    tabPanel(
                      
                      "Temperature and Precipitation",
                      
                      # Sidebar panel for controls.
                      sidebarPanel(
                        selectInput(
                          inputId="RCP_scenarios", label="Pick a RCP scenario:",
                          choices = c("RCP 4.5", "RCP 8.5"), multiple = F
                        ),
                        selectInput(
                          inputId="Variable", label="Pick a climate variable:",
                          choices = c("Min Temperature", "Max Temperature", "Precipitation"), multiple = F
                        ),
                        tags$p(span("Large maps may take a few seconds to render.", style = "color:red")),
                        tags$p(span("Plots show % change in variable relative to mean values netween 1980 and 2014."), style = "color:black"),
                        tags$p(HTML("<b>Hover</b> to see the county name.")),
                      ),
                      
                      # Main panel with plot.
                      mainPanel(
                        plotOutput("climatechange", hover = "plot_hover"),
                        uiOutput("dynamic")
                      )
                      
                    ),
                    
                    # Bar plot of ethnic diversity and % female.
                    tabPanel(
                      
                      "Ecosystem services",
                      
                      # Sidebar panel for controls.
                      # Sidebar panel for controls.
                      sidebarPanel(
                        selectInput(
                          inputId="RCP scenarios",label= "Pick a RCP scenario:",
                          choices = c("RCP 4.5", "RCP 8.5"), multiple = F
                        ),
                        tags$p(span("Large graphs (e.g., of the full dataset) may take a few seconds to render.  The first graph may take up to two minutes if the app is retrieving new data from Rebrickable.", style = "color:red")),
                        tags$p(HTML("<b>Hover</b> to see the part name.")),
                        tags$p(HTML("Each circle represents a <b>unique minifigure or minidoll head</b>.")),
                        tags$p(HTML("Area is proportional to the <b>number of pieces</b> across all sets.")),
                        tags$p(HTML("<b>\"Ethnicity\"</b> is the color of the piece.  Yes, it's silly.")),
                        tags$p(HTML("<b>Gender</b> is inferred from keywords in the part name (\"Male\", \"Female\", etc., plus references to facial hair).")),
                        tags$p("Some heads are not labeled male/female but contain the name of a character of known gender (e.g., \"Han Solo\").  Incorporating this information would require a hand-maintained list of character names and their genders; I haven't done this.")
                      ),
                      
                      # Main panel with plot.
                      mainPanel(
                        plotOutput("ecosystem_change",
                                   width = "700px", height = "2000px")
                        
                      )
                      
                    )
                    
                  )
                )
              )
)

server<-function(input, output) {
  
  pack2<-list("raster", "ggplot2", "RColorBrewer", "rasterVis", "maptools", "maps")
  # lapply(pack, "install.packages", character.only=TRUE)
  lapply(pack, "require", character.only=TRUE)
  
  climresults<-reactive({
    temp_scen<- input$RCP_scenarios
    temp_var<-input$Variable
    
    if (temp_scen == "RCP 4.5"){
      vars45<-list(clim[[2]], clim[[4]], clim[[6]], clim[[8]], clim[[10]], clim[[12]])
      
      if (temp_var == "Min Temperature"){
        rast<-brick(list(vars45[[2]], vars45[[1]]))
        names(rast)<-c("Time period 2020 to 2050", "Time period 2050 to 2100")
      }
      
      if (temp_var == "Max Temperature"){
        rast<-brick(list(vars45[[4]], vars45[[3]]))
        names(rast)<-c("Time period 2020 to 2050", "Time period 2050 to 2100")
      }
      
      if (temp_var == "Precipitation"){
        rast<-brick(list(vars45[[6]], vars45[[5]]))
        names(rast)<-c("Time period 2020 to 2050", "Time period 2050 to 2100")
      }
      
    }  
    
    if (temp_scen == "RCP 8.5"){
      vars85<-list(clim[[1]], clim[[3]], clim[[5]], clim[[7]], clim[[9]], clim[[11]])
      
      if (temp_var == "Min Temperature"){
        rast<-brick(list(vars85[[2]], vars85[[1]]))
        names(rast)<-c("Time period 2020 to 2050", "Time period 2050 to 2100")
      }
      
      if (temp_var == "Max Temperature"){
        rast<-brick(list(vars85[[4]], vars85[[3]]))
        names(rast)<-c("Time period 2020 to 2050", "Time period 2050 to 2100")
      }
      
      if (temp_var == "Precipitation"){
        rast<-brick(list(vars85[[6]], vars85[[5]]))
        names(rast)<-c("Time period 2020 to 2050", "Time period 2050 to 2100")
      }
      
    }
    return (rast)
    
  })
  
  output$climatechange<-renderPlot({
    par(mfrow = c(1:2))
    
    for (i in 1:2){
      plot(subset(climresults(),i), main = names(climresults())[i])
      plot(county, add=TRUE)}
    
  })
  
  output$dynamic <- renderUI({
    req(input$plot_hover) 
    verbatimTextOutput("vals")
  })
  
  output$vals <- renderPrint({
    hover <- input$plot_hover 
    print(str(hover)) # list
    # y <- county$ADMIN1
    #paste0("County:", hover)
  })
  
  
  ###############################################################################  
  
  output$ecosystem_change<-renderPlot({
    plot(seq(1:100), seq(1:100), type = "l")
    
  })
  
}  

shinyApp(ui= ui, server = server)                   