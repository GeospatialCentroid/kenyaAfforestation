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

# source modules --------------------------------------------------------
lapply(list.files(
  path = "modules/",
  pattern = ".R",
  full.names = TRUE
  ),
  source)

# source UI or Server only functions ------------------------------------
lapply(list.files(
  path = "functions/",
  pattern = ".R",
  full.names = TRUE
  ),
  source)

# global variables -----------------------------------------
## define names for the climate features
panelNames <-
  c(
    "Optimistic Climate Future",
    "Middle of the Road Climate Future",
    "Pessimistic Climate Future",
    "Extreme Climate Future"
  )


# Climate Change Page -----------------------------------------------------
climateChangeInputs <- readRDS("data/climateChangeInputs.RDS")
## raster inputs
clim_abs <- climateChangeInputs$abs_rasters
clim_change <- climateChangeInputs$change_rasters
## county names
county_names <- climateChangeInputs$countyNames
## county shp
county <- climateChangeInputs$county

### process into groups
allRasters_abs <- prepClim(rasters = clim_abs, ssps = c("hist","126","245","370", "585"))
allRasters_change <- prepClim(rasters = clim_change, ssps = c("126","245","370", "585"))

### preprocess all palette objects 
pal_abs <- generatePalettes(rasters = clim_abs, type = "abs")
pal_change <- generatePalettes(clim_change, type = "change")


# Climate Management Inputs -----------------------------------------------
climateManagementInputs <- readRDS("data/climateManagementInputs.RDS")

pal_management <- genPalettes_forestCover(climateManagementInputs)


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
           ))
  ),
  
  ## Additional Nav bar objects ----------------------------------------------
  
  ### this is backburner for the time being... might be something to work on after the inperson event. 
  # tabPanel(title = "Downloads",
  #          h2("Under construction")),
  tabPanel(title = "Model Validation",
           h2("Under construction")),
  tabPanel(title = "Simulation Details",
           h2("Under construction"))
  
)



# Server ------------------------------------------------------------------


server <- function(input, output, session) {
  # page transfer for buttons 
  pageButtonServer("optimistic", parentSession = session,pageName = "Optimistic" )
  pageButtonServer("middle", parentSession = session,pageName = "Middle of the Road" )
  pageButtonServer("pessimistic", parentSession = session,pageName = "Pessimistic" )
  pageButtonServer("extreme", parentSession = session,pageName = "Extreme" )
  
  # ssp126 data 
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
  
  # ssp245 data
  map_server(id = "ssp245",
             histRasters = allRasters_abs$hist,
             sspRasters =  allRasters_abs$`245`,
             changeRasters = allRasters_change$`245`,
             ssp = "245",
             # histPal = palettes[[1]],
             # sspPal = palettes[[3]],
             pals1 = pal_abs,
             pals2 = pal_change,
             countyFeat = county)
  #map2_server("ssp245_2")
  
  # ssp370 data
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
  #map2_server("ssp370_2")
  
  # ssp585 data
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
  #map2_server("ssp585_2")
  
}

shinyApp(ui, server)
