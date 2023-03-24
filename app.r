### current testing the kenya afforestation revamp
### carverd@colostate.edu
### 20221014

library(shiny)
library(shinyWidgets)
library(leaflet)
library(bslib)
library(terra)
library(sf)
library(purrr)
library(raster)
library(rgdal)
library(leaflet.extras)
library(plotly)
### raster option within leaflet... old so we might loose other functionality 
#remotes::install_github("rstudio/leaflet", ref="joe/feature/raster-options")

# source modules --------------------------------------------------------
lapply(list.files(
  path = "modules/",
  pattern = ".R",
  full.names = TRUE
  ),
  source)

# source UI or Server only functions ------------------------------------
lapply(list.files(
  path = "appFunctions/",
  pattern = ".R",
  full.names = TRUE
  ),
  source)

# global variables -----------------------------------------
## define names for the climate features----
panelNames <-
  c("Optimistic Climate Future",
    "Middle of the Road Climate Future",
    "Pessimistic Climate Future",
    "Extreme Climate Future")


# Climate Change Page -----------------------------------------------------
## read in inputs ----
climateChangeInputs <- readRDS("appData/climateChangeInputs.RDS")
## define specific raster sets ----
clim_abs <- climateChangeInputs$abs_rasters
clim_change <- climateChangeInputs$change_rasters
## county names----
county_names <- climateChangeInputs$countyNames
## county shp----
county <- climateChangeInputs$county

## process raster data into spp groups----
allRasters_abs <- prepClim(rasters = clim_abs, ssps = c("hist","126","245","370", "585"))
allRasters_change <- prepClim(rasters = clim_change, ssps = c("126","245","370", "585"))


# Climate Management Inputs -----------------------------------------------
## read in climate management data ----
climateManagementInputs <- readRDS("appData/climateManagementInputs.RDS")

# set palette definitions  --------------------------------------------------
## read in palette object----
paletteList <- readRDS("appData/palettes.RDS")
## define specific palette object based on visualization set ----
pal_abs <- paletteList$pal_abs
pal_change <- paletteList$pal_change
pal_management <- paletteList$pal_management


# Validation Inputs -----------------------------------------------------------
npp_val <- readRDS("appData/validationInputs.RDS")

# UI section --------------------------------------------------------------
ui <- navbarPage(
    # required as reference for the button selection process. 
    id = "pages",
    theme = bs_theme(version = 5, bootswatch = "minty",
                   primary = "#2F4F4F",
                   secondary = "#2F4F4F") %>% 
    bslib::bs_add_rules(sass::sass_file("www/style.scss")),

  # the text that appears next to the pages
  title =  "Kenya Afforestation Decision Support Tool",
  # the text in the browser tab
  windowTitle = "Kenya Afforestation Application",
  # means of applying data to all pages -- could be useful in footer section
  # header = h5("This content appears at the top of every page "),
  # footer = "This content appears at the bottom of every page",

  ## Home page --------------------------------------------------------------- 
  tabPanel(title = "Home",
           htmlTemplate("www/homepage.html",
                        button_opt = pageButtonUi(id = "optimistic"),
                        button_status = pageButtonUi(id = "middle"),
                        button_pess = pageButtonUi(id = "pessimistic"),
                        button_ex = pageButtonUi(id = "extreme"),
                        button_ssp_link = actionButton("ssp-link", "Click here to read more about Shared Socio-Economic Pathways",
                                                       onClick = "window.open('https://www.carbonbrief.org/explainer-how-shared-socioeconomic-pathways-explore-future-climate-change/')")
           )
  ),
  ## About Page -------------------------------------------------------------------
  tabPanel(title = "About", class = "about-tab",
           p("This website was designed to let you visually explore the future of
          climate change and how it may impact Kenya’s forests. The purpose is to
          provide a useful source of information (using maps, graphs, and tables)
          about the potential impacts of climate change, and possible outcomes 
          that could arise for different types of forests, across Kenya." 
           ),
          p("On this page, you can find information regarding the origin of the
            project, the team of people that were involved, publications, and other
            relevant information." ),
          hr(),
          h2("Project Background"),
          p("This project was led by Colorado State University in collaboration with ",
            tags$a(href = "https://www.sei.org/centres/africa/", "SEI-Africa", target = "_blank"),
            " and the ",
            tags$a(href = "https://www.kefri.org/home.html", "Kenya Forestry Research Institute", target = "_blank"),
            " (KEFRI). The project was primarily funded by the ",
            tags$a(href = "https://www.nasa.gov/", "National Aeronautics and Space Administration", target = "_blank"),
            " (NASA) ",
            tags$a(href = "https://cce.nasa.gov/biodiversity/index.html", "Biological Diversity and Ecological Conservation program", target = "_blank"),
            " (Project# 80NSSC19K0182)."
          ),
          hr(),
          h2("Who We Are"),
          p("This website is the result of a collaboration among the following individuals and institutions."),
          strong("SEI-Africa"), 
          p("Dr. Anderson Kehbila"),
          strong("KEFRI"),
          p("Dr. Vincent Oeba"),
          strong("Colorado State University"),
          p("Professor Patrick Keys", br(),"Dr. Rekha Warrier", br(),
            "Professor Randall Boone", br(),"Professor Kathleen Galvin"),
          strong("Geospatial Centroid"),
          p("Dan Carver", br(), "Dr. Caitlin Mothes"),
          hr(),
          h2("Publications (In-progress)"),
          p("A variety of published work stemmed from this project, including reports, guidance documents, and academic journal articles"),
          tags$ol(
            tags$li("Report on forest cover change scenarios"), 
            tags$li("Model documentation for SPIRALL L-Range"), 
            tags$li("Ecology & Society article"),
            tags$li("Earth Interactions")
          )
  ),

  # combine scenarios into navbar menu
  navbarMenu(
    title = "Climate Scenarios",
  ## Optimistic --------------------------------------------------------------
  tabPanel(title = "Optimistic",
           tabsetPanel(
             type = "pills",
             tabPanel(
               "Climate change in Kenya",
               map_UI(id = "ssp126", panelName = panelNames[1])
             ),
             tabPanel(
               "Climate and management effects on forests",
               map2_UI(
                 id = "ssp126_2",
                 panelName = panelNames[1],
                 county_names = county_names
               )
             )
           )),
  ## Middle of the road ------------------------------------------------------
  tabPanel(title = "Middle of the Road",
           tabsetPanel(
             type = "pills",
             tabPanel(
               "Climate change in Kenya",
               map_UI(id = "ssp245", panelName = panelNames[2])
             ),
             tabPanel(
               "Climate and management effects on forests",
               map2_UI(
                 id = "ssp245_2",
                 panelName = panelNames[2],
                 county_names = county_names
               )
             )
           )),
  ## Pessimistic -------------------------------------------------------------
  tabPanel(title = "Pessimistic",
           tabsetPanel(
             type = "pills",
             tabPanel(
               "Climate change in Kenya",
               map_UI(id = "ssp370", panelName = panelNames[3])
             ),
             tabPanel(
               "Climate and management effects on forests",
               map2_UI(
                 id = "ssp370_2",
                 panelName = panelNames[3],
                 county_names = county_names
               )
             )
           )),
  ## Extreme Heat ------------------------------------------------------------
  tabPanel(title = "Extreme",
           tabsetPanel(
             type = "pills",
             tabPanel(
               "Climate change in Kenya",
                 map_UI(id = "ssp585", panelName = panelNames[4])
             ),
             tabPanel(
               "Climate and management effects on forests",
               map2_UI(
                 id = "ssp585_2",
                 panelName = panelNames[4],
                 county_names = county_names
               )
             )
           )
          )
  ),
  
  ## Model Information ----------------------------------------------
  tabPanel(title = "Model Information",
          h2("Model Summary"),
          p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Ut non felis 
            non nunc porta volutpat vulputate nec ligula. Donec dictum at tortor nec 
            imperdiet. Maecenas varius, sapien sit amet tincidunt hendrerit, nunc purus 
            elementum nisl, ut aliquet ex risus vitae sem. Praesent condimentum lacinia 
            elit eu egestas. Praesent ligula sem, porttitor ut lectus id, euismod 
            tristique mi. Sed cursus dignissim dolor, sit amet lobortis libero feugiat eu."),
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
                                                           tags$li(tags$strong("Mean Difference"), "= The mean difference in annual net primary productivity
                                                                between L-Range estimates (simulated) and MODIS (observed) for the 2000-2014 period. 
                                                                The units are KgC/sq. (Kilogram carbon per square meter)"), 
                                                           tags$li(tags$strong("Reference"), " = the mean annual NPP for the 2000 -2014 period 
                                                                based on MODIS (i.e. the observed dataset against which change was calculated).")
                                                         )),
                                            mainPanel(width = 7,
                                                      leafletOutput("npp_map", width="100%",height="500px"),
                                                      
                                            ))),
                                 tabPanel("Above and Below Ground Live Carbon"),
                                 tabPanel("Leaf Area Index"))),
            tabPanel("Simulation Details", class = "simulations-tab",
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
                       for the years 2050, 2070 and 2100."))
          )
  )

)



# Server ------------------------------------------------------------------


server <- function(input, output, session) {
  # set button functionality ------------------------------------------------
  pageButtonServer("optimistic", parentSession = session,pageName = "Optimistic" )
  pageButtonServer("middle", parentSession = session,pageName = "Middle of the Road" )
  pageButtonServer("pessimistic", parentSession = session,pageName = "Pessimistic" )
  pageButtonServer("extreme", parentSession = session,pageName = "Extreme" )


  

  # ssp126 data -------------------------------------------------------------
  map_server(id = "ssp126", histRasters = allRasters_abs$hist, 
             sspRasters =  allRasters_abs$`126`,
             changeRasters = allRasters_change$`126`,
             ssp = "126",
             #histPal = palettes[[1]],
             #sspPal = palettes[[2]],
             pals1 = pal_abs,
             pals2 = pal_change,
             countyFeat = county)
  map2_server(id = "ssp126_2",
              histRaster = climateManagementInputs$existingForest,
              futureRaster = climateManagementInputs$expandedForest,
              managementRasters = climateManagementInputs$forestChangeRasters,
              countryDF = climateManagementInputs$areaCountry,
              countyDF = climateManagementInputs$areaCounty,
              pal1 = pal_management,
              ssp = "126",
              countyFeat = county)
  # ssp245 data -------------------------------------------------------------
  map_server(id = "ssp245",
             histRasters = allRasters_abs$hist,
             sspRasters =  allRasters_abs$`245`,
             changeRasters = allRasters_change$`245`,
             ssp = "245",
             pals1 = pal_abs,
             pals2 = pal_change,
             countyFeat = county)
  map2_server(id = "ssp245_2",
              histRaster = climateManagementInputs$existingForest,
              futureRaster = climateManagementInputs$expandedForest,
              managementRasters = climateManagementInputs$forestChangeRasters,
              countryDF = climateManagementInputs$areaCountry,
              countyDF = climateManagementInputs$areaCounty,
              pal1 = pal_management,
              ssp = "245",
              countyFeat = county)  
  # ssp370 data -------------------------------------------------------------
  map_server(id = "ssp370",
             histRasters = allRasters_abs$hist,
             sspRasters =  allRasters_abs$`370`,
             changeRasters = allRasters_change$`370`,
             ssp = "370",
             # histPal = palettes[[1]],
             # sspPal = palettes[[4]],
             pals1 = pal_abs,
             pals2 = pal_change,
             countyFeat = county)
  map2_server(id = "ssp370_2",
              histRaster = climateManagementInputs$existingForest,
              futureRaster = climateManagementInputs$expandedForest,
              managementRasters = climateManagementInputs$forestChangeRasters,
              countryDF = climateManagementInputs$areaCountry,
              countyDF = climateManagementInputs$areaCounty,
              pal1 = pal_management,
              ssp = "370",
              countyFeat = county)  
  
  # ssp585 data ------------------------------------------------------------
  map_server(id = "ssp585",
             histRasters = allRasters_abs$hist,
             sspRasters =  allRasters_abs$`585`,
             changeRasters = allRasters_change$`585`,
             ssp = "585",
             # histPal = palettes[[1]],
             # sspPal = palettes[[5]],
             pals1 = pal_abs,
             pals2 = pal_change,
             countyFeat = county)
  map2_server(id = "ssp585_2",
              histRaster = climateManagementInputs$existingForest,
              futureRaster = climateManagementInputs$expandedForest,
              managementRasters = climateManagementInputs$forestChangeRasters,
              countryDF = climateManagementInputs$areaCountry,
              countyDF = climateManagementInputs$areaCounty,
              pal1 = pal_management,
              ssp = "585",
              countyFeat = county) 
  
  # validation maps ------------------------------------------------------
  
  ## npp maps ------------------------------------------------------------
  output$npp_map <- renderLeaflet({
    
    leaflet(options = leafletOptions(minZoom = 4)) %>%
      #set zoom levels
      setView(lng = 37.826119933082545,
              lat = 0.3347526538983459,
              zoom = 6) %>%
      # tile providers 
    addProviderTiles("OpenStreetMap", group = "OpenStreetMap")%>%
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
      overlayGroups = c(
        "Mean Difference",
        "Reference",
        "Counties"
      ),
      position = "topleft",
      options = layersControlOptions(collapsed = FALSE)
    ) %>% 
      hideGroup("Reference")
    
  })
}

shinyApp(ui, server)
