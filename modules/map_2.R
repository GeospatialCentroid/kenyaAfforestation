### Maps for climate change management 

# define ui ---------------------------------------------------------------
map2_UI <- function(id, panelName, county_names){
  ns <- NS(id)
  tagList( # ensures that correct html returns. Functions as a list. 
    h3(panelName),
    sidebarLayout(
      sidebarPanel(width = 3,
                   radioButtons(
                     inputId=ns("Timeline"),
                     label= tags$strong("Pick a future time period:"),
                     choices = list("2041-2060" = "50",
                                    "2061-2080" = "70",
                                    "2081-2100" = "100"),
                     selected = "50"
                   ),
                   # select mapped variable 
                   selectInput(
                     inputId=ns("Layer"),
                     label=tags$strong("Pick a disturbance scenario to visualize predicted change in tree cover:"),
                     choices = list("Historic Fire Severity" = "nothing",  ## this will need to change to match the new dataset convention
                                    "Decreased Fire Severity" = "less_fire",
                                    "Increased Fire Severity" = "more_fire"),
                     selected = "nothing"
                   ),
                   hr(),
                   # select County features 
                   selectInput(
                     inputId=ns("County23"), label=tags$strong("Pick a county to visualize change in tree cover:"),
                     choices = county_names, multiple = F,selected = NULL
                   ),
                   #add button to zoom to County
                   actionButton(inputId = ns("Zoom"), label = "Zoom to County"),
                   hr(),
                   #add button for download report 
                   downloadButton(outputId = ns("report"), "Generate Report for Selected County"),
                   br(),
                   em("Report generation may take a few seconds"),
                   hr(),
                   # visualize user click
                   tags$p(tags$strong("Click"), "on a pixel within Kenya to see value:"),
                   h6(htmlOutput(ns("pixelVal2"))),
                   em("You can view historic (before afforestation) and 2030 baseline tree cover layers via the map controls")
                   
      ),
      mainPanel(width = 9,
        leafletOutput(ns("map2")),
        fluidRow( ### even through this is still within the 10 unit wide main panel, width operates out of 12.
          align = "center",
          column(width = 6, height = "100%",
                 h5(strong("Kenya")),
                 plotlyOutput(ns("percentChangeCountry"))
                 #p("This plot summarizes the total change in tree cover throughout the country.")
          ),
          column(width = 6, 
                 h5(strong(textOutput(ns("countyText")))),
                 plotlyOutput(ns("percentChangeCounty"))
                 #p("This plot summarizes the total change in tree cover throughout the county")
            
          )
        ),
        fluidRow(
          align = "center",
          em("Boxplots summarize change in tree cover relative to the 2030 baseline from repeating simulation scenarios using 13 climate change models,"),
          hr(),
          h5("Category Descriptions:"),
          tags$p(tags$strong("All"), "= Tree cover change across all areas with trees."), 
          tags$p(tags$strong("New"), "= Tree cover change across all areas with newly planted trees (to achieve 2030 baseline)."),
          tags$p(tags$strong("Evergreen/Deciduous"), "= Tree cover change in areas with > 30% evergreen or decidious trees.")
        )
      )
    )
  )
}


# define server  ---------------------------------------------------------- 
map2_server <- function(id, histRaster, futureRaster, managementRasters, 
                        countryDF,countyDF, pal1, countyFeat, ssp,
                        decid_report, ever_report, proj_report,
                        population, ecosystem_data
                        # ssp, will need to add once all data is present. 
                        ){
  moduleServer(id,function(input,output,session){
   
  # suppress warnings (lots of non-important plotly warnings) 
  options(warn = -1)

  # define raster features  -------------------------------------------------
   hist1 <- histRaster
   base1 <- futureRaster
   
   # rename original countyDF for report generation (keep all scenarios)
   countyDF_all <- countyDF
  

  # filter datasets to specific spp -----------------------------------------
  managementRasters <- managementRasters[grepl(pattern = ssp, x = names(managementRasters))]
  countryDF <- countryDF[grepl(pattern = ssp, x = names(countryDF))]
  countyDF <- countyDF[grepl(pattern = ssp, x = names(countyDF))] 
   
   
  # bind features to single list  
   r1 <- list(
     nothing = managementRasters[[paste0("ssp",ssp,"_DoNothing")]],
     less_fire = managementRasters[[paste0("ssp",ssp,"_NoFires")]],
     more_fire = managementRasters[[paste0("ssp",ssp,"_DoubleFires")]]
   )

   
   # select object based on set name, then drop list to get raster brick -- input$layer : fire, nothing
  r2 <-  reactive({r1[grep(pattern = input$Layer, x = names(r1))][[1]]})
  # select raster layer based on the selected timeline --- double brackets on raster bricks 
  r3 <-  reactive({r2()[[grep(pattern = paste0("_", input$Timeline), x = names(r2()))]]})

  pal2 <- reactive(pal1[[input$Layer]]$palette)
  vals2 <- reactive(pal1[[input$Layer]]$values)
  title2 <- reactive(pal1[[input$Layer]]$title)

  
   #  generarte the map object 
    output$map2 <- leaflet::renderLeaflet({
      #map <-reactive({ 
      leaflet(options = leafletOptions(minZoom = 4)) %>%
        setView( lng = 37.826119933082545
                 , lat = 0.3347526538983459
                 , zoom = 6 )%>%
        # tile providers ----------------------------------------------------------
      # addProviderTiles("Stamen.Toner", group = "Light") %>%
        addProviderTiles("OpenStreetMap", group = "OpenStreetMap")%>%
        #leaflet.extras::addResetMapButton() %>%
        # add z levels ------------------------------------------------------------
      # addMapPane("Historic Data", zIndex = 406) %>%
      #   addMapPane("Baseline Data", zIndex = 407) %>%
      #   addMapPane("Projected Data", zIndex = 408) %>%
         addMapPane("Counties", zIndex = 409) %>%
        # add county features -----------------------------------------------------
      addPolygons(
        data = countyFeat,
        fillColor = "",
        fillOpacity = 0,
        color = "black",
        layerId = ~ ADMIN1,
        weight = 1.5,
        group = "Counties",
        options = pathOptions(pane = "Counties"),
        # add hover over lables 
        label= ~ ADMIN1,
        labelOptions = labelOptions(noHide = F,
                                    style = list("font-weight" = "bold"),
                                    textsize = "15px"),
        # add highlight options to make labels a bit more intuitive 
        highlightOptions = highlightOptions(
          color = "yellow",
          opacity = 1,
          weight = 3,
          bringToFront = TRUE
        )
      ) %>%
      # add control groups ------------------------------------------------------
      addLayersControl(
        # baseGroups = c("OpenStreetMap", "Light"),
        overlayGroups = c(
          "Historic Tree Cover",
          "2030 Baseline Tree Cover",
          #"Forest Cover Data",
          "Projected Change",
          "Counties"
        ),
        position = "topleft",
        options = layersControlOptions(collapsed = TRUE)
      ) %>% 
        hideGroup(
          group = c(
            #"Forest Cover Data",
            "Historic Tree Cover",
            "2030 Baseline Tree Cover"))

    })
    
    # this makes it so the proxy map is rendered in the background, otherwise the map is empty when you first navigate to this page
    outputOptions(output, "map2", suspendWhenHidden=FALSE)
    
    
    # add rasters to proxy map
    observe({

      leafletProxy("map2") %>% 
        #removeImage(layerId = "change") %>% 
        # add historic raster -----------------------------------------------------
      addRasterImage(hist1,
                     colors = pal1$hf$palette,
                     group = "Historic Tree Cover",
                     opacity = 1,
                     project = FALSE) %>%
        # add baseline raster features -----------------------------------------------------
      addRasterImage(base1,
                     colors = pal1$ef$palette,
                     group = "2030 Baseline Tree Cover",
                     opacity = 1,
                     project = FALSE) %>%
        # add percent change layer ------------------------------------------------
      addRasterImage(
        r3(),
        layerId = "change",
        colors = pal2(),
        group = "Projected Change",
        opacity = 1,
        project = FALSE
      ) %>%
        # add legend for percent change
        addLegend_decreasing(
          "bottomright",
          pal = pal2(),
          values = vals2(),
          title = title2(),
          #   labels = c("Low Change", "", "", "", "High Change"),
          opacity = 1,
          layerId = "changeLegend",
          group = "Projected Change",
          decreasing = TRUE
        ) 
        
        
    })
    
    # add forest % legend  ------------------------------------------------------------
    observeEvent(input$map2_groups,{

      leafletProxy("map2") %>%
        removeControl(layerId = "sharedLegend")

      if ('Historic Tree Cover' %in% input$map2_groups | '2030 Baseline Tree Cover' %in% input$map2_groups){
        leafletProxy("map2") %>%
          addLegend_decreasing(
            "topright",
            pal = pal1$ef$palette,
            values = pal1$ef$values,
            title = "Tree Cover(%)",
            #   labels = c("Low Change", "", "", "", "High Change"),
            opacity = 0.8,
            layerId = "sharedLegend",
            decreasing = TRUE,
            bins = 5
          )

      }



    })
    # set map zoom based on county --------------------------------------------
    
    observeEvent(input$Zoom,{
      c1 <- reactive({
        countyFeat %>%
          filter(ADMIN1  == input$County23) %>%
          st_set_agr("constant") %>% # attributes constant over geometries (suppresses warning message)
          sf::st_centroid() %>%
          st_coordinates()
      })
      # not sure about the zoom level 
      leafletProxy('map2') %>% 
        setView(lng =  c1()[1], lat = c1()[2], zoom = 8)
    })
    
    
    
    #map click ---------------------------------------------------------
    observeEvent(input$map2_click, {
      click <- input$map2_click
      clat <- click$lat
      clon <- click$lng
      # filter data
      #point <- as(st_point(x = c(clon, clat)), "Spatial")
      point <- st_sfc(st_point(x = c(clon,clat)),crs = 4326) %>% 
        st_transform(3857) %>%
        as("Spatial")
      # Get need baseline and percent change values
      # baseline
      extractVal1 <- raster::extract(base1, point)%>%
        round(digits = 2)
      #percent change
      extractVal2 <- raster::extract(r3(), point)%>%
        round(digits = 2)
      
      
      output$pixelVal2 <- renderText(paste0("Baseline Tree Cover:",
                                      "<b>", as.character(extractVal1), "</b>","%", "<br>",
                                      "Projected Change in Tree Cover:",
                                      "<b>", as.character(extractVal2),"</b>","%"))
    })
  


    

  # generate the forest change plots  -----------------------------------------------
   
  
    
    # get reactive dataframes
    
    ## COUNTRY
    df1 <- list(
      nothing = countryDF[[paste0("ssp",ssp,"_DoNothing")]],
      less_fire = countryDF[[paste0("ssp",ssp,"_NoFires")]],
      more_fire = countryDF[[paste0("ssp",ssp,"_DoubleFires")]]
    )
    # select df based on management action
    df2 <-  reactive({df1[grep(pattern = input$Layer, x = names(df1))][[1]]})
    
    
    ## COUNTY
    df1_a <- list(
      nothing = countyDF[[paste0("ssp",ssp,"_DoNothing")]],
      less_fire = countyDF[[paste0("ssp",ssp,"_NoFires")]],
      more_fire = countyDF[[paste0("ssp",ssp,"_DoubleFires")]]
    )
    # select df based on management action
    df2_a <-  reactive({df1_a[grep(pattern = input$Layer, x = names(df1_a))][[1]]})
    
    # select county based on inputs 
    df3_a <- reactive({df2_a() %>% filter(County == input$County23)})
    
    
    #set plotly parameters
    forest_pal <-c("#2d4221","#32a850", "#87c7cd",  "#d8cb39")
    forest_pal <- setNames(forest_pal, c("All", "Evergreen", "Deciduous", "New"))
    
    # set axis range based on county max and min
   # range <- reactive({list(min(df2_a()$Change, na.rm = TRUE), max(df2_a()$Change, na.rm = TRUE))})
    range <- reactive({list(min(df2_a()$value, na.rm = TRUE), max(df2_a()$value, na.rm = TRUE))})
    
    ## plotly for Country --------------------------------------------------------

    p1 <-  reactive({plot_ly(data = df2(), y = ~value, x = ~Year, type = "box",
                   color = ~Areas, name = ~Areas, colors = forest_pal) %>% 
        layout(yaxis = list(title = "<b>Relative Change in Tree Cover (%)</b>"
                            #range = range()
                            ),
               xaxis = list(title = "", tickfont = list(size = 16), side = "top"),
               boxmode = "group")
      })


    ## plotly data for County ----------------------------------------------
    p2 <- reactive({
        plot_ly(data = df3_a(), y = ~value, x = ~Year, type = "box",
                color = ~Areas, name = ~Areas, colors = forest_pal) %>% 
          layout(yaxis = list(title = "<b>Relative Change in Tree Cover (%)</b>" 
                              #range = range()
                              ),
                 xaxis = list(title = "", tickfont = list(size = 16), side = "top"),
                 boxmode = "group")
      })

    
    
    #output$varchange1 <- renderLeaflet({map()})
    output$percentChangeCountry <- renderPlotly({p1()})
    output$percentChangeCounty <- renderPlotly({
      if (req(!all(is.na(df3_a()[, 4])))) {
        p2()
      } else {
        return(NULL)

      }
    })
    # output$cnty3 <- renderText("")
    output$countyText <- renderText(paste(input$County23, "County"))
      
    # renderQMD <- reactive({
    #   quarto::quarto_render("reports/reportTemplate.qmd", 
    #                       execute_params = list(
    #                         username = input$County23,
    #                         County = input$County23, 
    #                         Year = input$Timeline ,
    #                         Management = input$layer,
    #                         Scenario =  "temp",
    #                         #Map =  leaflet() %>% addTiles(),
    #                         plot1 = p1
    #                         #plot2 = p2()
    #                       ))
    # })
    
    
  # render qmd report  ------------------------------------------------------
    # output$report <- downloadHandler(
    #   filename = paste0("KA_",input$County23,"_",Sys.Date(),".html"),
    #   content = function(file) {
    #     renderQMD()
    # 
    #     file.copy("reports/reportTemplate.html", file)
    # 
    #   }
    # )
    
  # render rmd report ------------------------------------------------------
    output$report <- downloadHandler(
      filename = function() {
        paste0(input$County23, "_Report_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        # render file in temp directory so .knit files don't go in app directory
        tempReport <- file.path(tempdir(), "report_example.Rmd")
        file.copy("reports/report_example.Rmd", tempReport, overwrite = TRUE)
        rmarkdown::render(
          tempReport,
          output_format = "pdf_document",
          output_file = file,
          params = list(
            county_shape = countyFeat,
            county_name = input$County23,
            time = input$Timeline,
            county_vals = countyDF_all,
            table = df3_a(),
            projection = proj_report,
            decid = decid_report,
            ever = ever_report,
            historic = histRaster,
            baseline = futureRaster,
            population = population,
            ecosystem_data = ecosystem_data
          ),
          envir = new.env(parent = globalenv()),
          clean = F,
          encoding = "utf-8"
        )
      }
    )
    
    } # end of moduleserver
  )
}


