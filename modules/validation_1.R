### Maps for climate change in Kenya selection 


validation_UI <- function(id){
  ns <- NS(id)
  tabPanel(title = "Simulation Details",
           h2("Simulation Details"),
           br(),
           p("A layer representing possible expanded forest cover in 2030 
                       (representing successful afforestation to achieve 10 % cover) 
                       was created by adding tree cover to agricultural and savanna 
                       areas in the proximity of existing evergreen and deciduous forests. 
                       Starting with this expanded forest cover scenario in 2030, 
                       changes in cover were simulated till the end of the century under 
                       each of the four climate scenarios (SSP1, SSP2, SSP3, SSP5) while 
                       incorporating the effects of three possible management actions 
                       (Do nothing: Current conditions of fire and grazing persist, 
                       Stop fires: Forest fires are fully controlled, Stop grazing: 
                       Livestock grazing is fully controlled).  A total of 12 scenarios 
                       were explored, each representing the combined effects of a unique 
                       climate future and a single management action. For each scenario, 
                       retaining forest cover in 2030 as a baseline, simulated changes in 
                       tree cover within evergreen and deciduous forest areas were calculated 
                       for the years 2050, 2070 and 2100."),
           br(),
           hr(),
           tabsetPanel(
             tabPanel("Model Validation", class = "validation-tab",
                      br(),
                      br(),
                      tabsetPanel(type = "pills", id = "model-eval",
                                  tabPanel("Net Primary Productivity",
                                           sidebarLayout(
                                             sidebarPanel(width = 5,
                                                          h5("Methods"),
                                                          tags$ul(
                                                            tags$li("L-Range simulations were conducted using 
                                                                   historical climate data for the years 1995-2014."), 
                                                            tags$li("Fire was represented using ESA CCI historical 
                                                                   fire data summarized to represent mean annual frequency of fire."), 
                                                            tags$li("Estimates of Annual Net Primary Productivity (NPP) for the years 
                                                                   2000-2014 from L-Range were compared against estimates of observed 
                                                                   Annual NPP from MODIS for the same period."),
                                                            tags$li("Estimates are shown only for areas dominated 
                                                                   by natural vegetation (trees, shrubs, grasses).")
                                                          ),
                                                          hr(),
                                                          h5("Map layer descriptions:"),
                                                          tags$ul(
                                                            tags$li(tags$strong("Mean Difference"), HTML(paste0("= The mean difference in ", "<b>", " annual NPP ",
                                                                "</b>", "between L-Range estimates (simulated) and MODIS (observed) for the 2000-2014 period. 
                                                                The units are KgC/sq. (Kilogram carbon per square meter)"))), 
                                                            tags$li(tags$strong("Reference"), HTML(paste0(" = the mean", "<b>", " annual NPP ", "</b>", "for the 2000 -2014 period 
                                                                based on MODIS (i.e. the observed dataset against which change was calculated).")))
                                                          )),
                                             mainPanel(width = 7,
                                                       leafletOutput(ns("npp_map"), width="100%",height="500px"),
                                                       
                                             ))),
                                  tabPanel("Above Ground Live Carbon", 
                                           sidebarLayout(
                                             sidebarPanel(width = 5,
                                                          h5("Methods"),
                                                          tags$ul(
                                                          ),
                                                          hr(),
                                                          h5("Map layer descriptions:"),
                                                             tags$ul(
                                                               tags$li(tags$strong("Mean Difference"), HTML(paste0("= The mean difference in ", "<b>", " above ground live carbon ",
                                                                                                                   "</b>", "for the year 2010 between L-Range estimates and a validation dataset (Spawn et al., 2020). 
                                                                The units are Mg/ha. (Megagrams per hectare)"))), 
                                                               tags$li(tags$strong("Reference"), HTML(paste0(" = the mean", "<b>", " above ground live carbon ", "</b>", "dataset based on Spawn et al 2020. 
                                                                                                             (i.e., the observed dataset against which change was calculated)")))
                                                             ),
                                                          ),
                                             mainPanel(width = 7,
                                                       leafletOutput(ns("above_map"), width="100%",height="500px")
                                                       ),
                                              )
                                           ),
                                  tabPanel("Below Ground Live Carbon", 
                                           sidebarLayout(
                                             sidebarPanel(width = 5,
                                                          h5("Methods"),
                                                          tags$ul(
                                                          ),
                                                          hr(),
                                                          h5("Map layer descriptions:"),
                                                             tags$ul(
                                                               tags$li(tags$strong("Mean Difference"), HTML(paste0("= The mean difference in ", "<b>", " below ground live carbon ",
                                                                                                                   "</b>", "for the year 2010 between L-Range estimates and a validation dataset (Spawn et al., 2020). 
                                                                The units are Mg/ha. (Megagrams per hectare)"))), 
                                                               tags$li(tags$strong("Reference"), HTML(paste0(" = the mean", "<b>", " below ground live carbon ", "</b>", "dataset based on Spawn et al 2020. 
                                                                                                             (i.e., the observed dataset against which change was calculated)")))
                                                             ),
                                             ),
                                             mainPanel(width = 7,
                                                       leafletOutput(ns("below_map"), width="100%",height="500px")
                                             ),
                                           )))),
             tabPanel("Model FAQ", class = "simulations-tab",
                      h2("FAQ"),
                      br(),
                      p(tags$strong("Question 1: "), "This is a place to add some questions."),
                      p("This is a pace to add some answers."),
                      #add button for download report 
                      br(),
                      tags$strong("For further technical details download the full report"),
                      downloadButton(outputId = ns("report2"),
                                     label = "Download Technical Report"),
                      br(),
                      em("Report download may take a few seconds"),
                      hr(),
                      
           )
          )
  )
}


# define server  ----------------------------------------------------------
validation_server <- function(id, npp_val, carbon_val, county){
  moduleServer(id,function(input,output,session){
    ## npp maps ------------------------------------------------------------
    output$npp_map <- renderLeaflet({
      leaflet(options = leafletOptions(minZoom = 4)) %>%
        #set zoom levels
        setView(lng = 37.826119933082545,
                lat = 0.3347526538983459,
                zoom = 6) %>%
        # tile providers
        addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
        # add county features
        addPolygons(
          data = county,
          fillColor = "",
          fillOpacity = 0,
          color = "black",
          layerId = ~ ADMIN1,
          weight = 1.5,
          group = "Counties",
          # add hover over lables
          label = ~ ADMIN1,
          labelOptions = labelOptions(
            noHide = F,
            style = list("font-weight" = "bold"),
            textsize = "15px"
          ),
          # add highlight options to make labels a bit more intuitive
          highlightOptions = highlightOptions(
            color = "yellow",
            opacity = 1,
            weight = 3,
            bringToFront = TRUE
          )
        ) %>%
        # add Reference layer and legend
        addRasterImage(npp_val$rasters$npp_ref,
                       colors = npp_val$npp_ref_pal,
                       group = "Reference") %>%
        addLegend_decreasing(
          "topright",
          pal = npp_val$npp_ref_pal,
          values = npp_val$npp_ref_values,
          title = "Reference",
          group = "Reference",
          decreasing = TRUE
        ) %>%
        # add difference layer and legend
        addRasterImage(npp_val$rasters$npp_dif,
                       colors = npp_val$npp_dif_pal,
                       group = "Mean Difference") %>%
        addLegend_decreasing(
          "topright",
          pal = npp_val$npp_dif_pal,
          values = npp_val$npp_dif_values,
          title = "Mean Difference",
          group = "Mean Difference",
          decreasing = TRUE
        ) %>%
        
        # add control groups
        addLayersControl(
          overlayGroups = c("Mean Difference",
                            "Reference",
                            "Counties"),
          position = "topleft",
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        hideGroup("Reference")
      
    })
    ## above ground carbon maps ------------------------------------------------------------
    above <- carbon_val$above
    
    
    output$above_map <- renderLeaflet({
      leaflet(options = leafletOptions(minZoom = 4)) %>%
        #set zoom levels
        setView(lng = 37.826119933082545,
                lat = 0.3347526538983459,
                zoom = 6) %>%
        # tile providers
        addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
        # add county features
        addPolygons(
          data = county,
          fillColor = "",
          fillOpacity = 0,
          color = "black",
          layerId = ~ ADMIN1,
          weight = 1.5,
          group = "Counties",
          # add hover over lables
          label = ~ ADMIN1,
          labelOptions = labelOptions(
            noHide = F,
            style = list("font-weight" = "bold"),
            textsize = "15px"
          ),
          # add highlight options to make labels a bit more intuitive
          highlightOptions = highlightOptions(
            color = "yellow",
            opacity = 1,
            weight = 3,
            bringToFront = TRUE
          )
        ) %>%
        ## above ground
        # add Reference layer and legend
        addRasterImage(
          above$cAbove_rasters$cAbove_ref,
          colors = above$cAbove_ref_pal,
          group = "Above Ground Reference"
        ) %>%
        addLegend_decreasing(
          "topright",
          pal = above$cAbove_ref_pal,
          values = above$cAbove_ref_values,
          title = "Above Ground Reference",
          group = "Above Ground Reference",
          decreasing = TRUE
        ) %>%
        # add difference layer and legend
        addRasterImage(
          above$cAbove_rasters$cAbove_dif,
          colors = above$cAbove_dif_pal,
          group = "Above Ground Mean Difference"
        ) %>%
        addLegend_decreasing(
          "topright",
          pal = above$cAbove_dif_pal,
          values = above$cAbove_dif_values,
          title = "Above Ground Mean Difference",
          group = "Above Ground Mean Difference",
          decreasing = TRUE
        ) %>%
        # add control groups
        addLayersControl(
          overlayGroups = c(
            "Above Ground Mean Difference",
            "Above Ground Reference",
            "Counties"
          ),
          position = "topleft",
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        hideGroup(c("Above Ground Reference"))
    })
    
    ## below ground carbon maps ----------------------------------------------------------------------
    below <- carbon_val$below
    
    output$below_map <- renderLeaflet({
      leaflet(options = leafletOptions(minZoom = 4)) %>%
        #set zoom levels
        setView(lng = 37.826119933082545,
                lat = 0.3347526538983459,
                zoom = 6) %>%
        # tile providers
        addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
        # add county features
        addPolygons(
          data = county,
          fillColor = "",
          fillOpacity = 0,
          color = "black",
          layerId = ~ ADMIN1,
          weight = 1.5,
          group = "Counties",
          # add hover over lables
          label = ~ ADMIN1,
          labelOptions = labelOptions(
            noHide = F,
            style = list("font-weight" = "bold"),
            textsize = "15px"
          ),
          # add highlight options to make labels a bit more intuitive
          highlightOptions = highlightOptions(
            color = "yellow",
            opacity = 1,
            weight = 3,
            bringToFront = TRUE
          )
        ) %>%
        ## Below ground
        # add Reference layer and legend
        addRasterImage(
          below$cBelow_rasters$cBelow_ref,
          colors = below$cBelow_ref_pal,
          group = "Below Ground Reference"
        ) %>%
        addLegend_decreasing(
          "topright",
          pal = below$cBelow_ref_pal,
          values = below$cBelow_ref_values,
          title = "Below Ground Reference",
          group = "Below Ground Reference",
          decreasing = TRUE
        ) %>%
        # add difference layer and legend
        addRasterImage(
          below$cBelow_rasters$cBelow_dif,
          colors = below$cBelow_dif_pal,
          group = "Below Ground Mean Difference"
        ) %>%
        addLegend_decreasing(
          "topright",
          pal = above$cAbove_dif_pal,
          values = above$cAbove_dif_values,
          title = "Below Ground Mean Difference",
          group = "Below Ground Mean Difference",
          decreasing = TRUE
        ) %>%
        # add control groups
        addLayersControl(
          overlayGroups = c(
            "Below Ground Mean Difference",
            "Below Ground Reference",
            "Counties"
          ),
          position = "topleft",
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        hideGroup(c("Below Ground Reference"))
    })

  # Technical Report Download -----------------------------------------------
  #add button for download report 
  output$report2 <- downloadHandler(
    filename = function(){
      paste0("ka_dst_technical_report", Sys.Date(),".pdf")
    },
    content = function(file){
      message("Report is being generated")

      report <- paste0("ka_dst_technical_report", Sys.Date(),".pdf")
      if(file.exists(report)){
        shinyalert(title = "Success",
                   text = "The report will download to
                     the download location used by your broswer.",
                   type = "success")
      }else{
        shinyalert(title = "Missing Data",
                   text = c(
                     "Files are missing and the report can not be download at this time."
                   ),
                   type = "error"
        )
      }
    }

  )
  })
}
