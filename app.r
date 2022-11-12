### current testing the kenya afforestation revamp
### carverd@colostate.edu
### 20221014

library(shiny)
library(leaflet)
library(bslib)
library(terra)
library(sf)
library(purrr)
library(raster)
library(rgdal)
library(leaflet.extras)

###
# we are using renv for package management of this application.
# use renv::snapshot() occasionally to update the package dependencies "Think save"
# renv will automatically load require packages and version with initalization
# of the .Rproj file. renv::restore() can be load the renv.lock file that store
# save from snapshot()
# *note: it is not clear how this will work within the shiny deployment.
###
#remove this for now, not working on some computers
#renv::restore()

###
# details on page change buttons in shiny modules 
# https://stackoverflow.com/questions/69831156/how-to-use-a-button-to-change-pages-in-an-r-shiny-app
### 

# source modules
lapply(list.files(
  path = "modules/",
  pattern = ".R",
  full.names = TRUE
),
source)
# source UI or Server only functions
lapply(list.files(
  path = "functions/",
  pattern = ".R",
  full.names = TRUE
),
source)

### define names for the climate features
panelNames <-
  c(
    "Optimistic Climate Future",
    "Middle of the Road Climate Future",
    "Pessimistic Climate Future",
    "Extreme Climate Future"
  )


# input dataset -----------------------------------------------------------
###
# this section will become a preprocessing step, but it is needed for the mark up phase of the project.
###

### projection raster template  -- required for maintaining the cell size of the input features.
pro_template <- rast("data/wgs_ext_res_temp.asc")
### test to see if this is needed.
crs(pro_template) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
# mask features
mask <- rast("data/ken_mask.tif") %>%
  terra::project(pro_template) # reprotion assing some decimal values
# replace 0 qith NA
mask[which(mask[] == 0)] <- NA
# replace all non NA with 1
mask[which(!is.na(mask[]))] <- 1

# county spatial feature
county <- sf::st_read("./data/KE_Admin1_pro.shp", stringsAsFactors = F)
county <- st_transform(county, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# vector of county names
county_names <- county$ADMIN1

# new climate data 
clim <- readRDS("data/climData_112022.rds")
raster::crs(clim) <- sf::st_crs(county)
r1 <- clim[[1]]
r2 <- crop(r1, sf::st_boundary(county))



# primary dataset for  the clim
clim2 <- readRDS("data/temp_pr_change.rds") %>%
  map(rast)%>%
  map(terra::project, pro_template)%>%
  map(terra::mask, mask) %>% 
  #map(terra::crop, county) %>%
  rast() # reduce to a layered raster that makes indexing easier 
  ### not sure if map applying is faster or slow. could test position of the rast call

# new input datat
# clim <- readRDS("data/climData_112022.rds")
# raster::crs(clim) <- sf::st_crs(county)
# 
# clim <- clim %>%
#   #map(rast)%>%
#   map(terra::project, pro_template)%>%
#   map(terra::crop, county) %>%
#   rast() # reduce to a layered raster that makes indexing easier 
### 

# parse out climate data into subsets for each module -- indepent blocks to feed 
# as inputs. 
allRasters <- prepClim(rasters = clim2, ssps = c("126","245","370"))

### buffer the clip process so it encludes all the country feature 
### redo labels (2030,50,70,90) - split into 20 year chunks of times 
### input new datasets 
### evaluate the implementation of the second map page 


###
# this content will present in application
###

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

  # Home page --------------------------------------------------------------- 
  tabPanel(title = "Home",
           htmlTemplate("www/homepage.html",
                        button_opt = pageButtonUi(id = "optimistic"),
                        button_status = pageButtonUi(id = "middle"),
                        button_pess = pageButtonUi(id = "pessimistic"),
                        button_ex = pageButtonUi(id = "extreme")
                        # button_opt = actionButton("button-opt", "View Scenario"),
                        # button_status = actionButton("button-status", "View Scenario"),
                        # button_pess = actionButton("button-pess", "View Scenario"),
                        #button_ex = actionButton("button-ex", "View Scenario")
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
  
  tabPanel(title = "Downloads",
           h2("content will be added to Model Downloads")),
  tabPanel(title = "Model Validation",
           h2("content will be added to Model Validation")),
  tabPanel(title = "Simulation Details",
           h2("content will be added to Simulation Details"))
  
)



# Server ------------------------------------------------------------------


server <- function(input, output, session) {
  # page transfer for buttons 
  pageButtonServer("optimistic", parentSession = session,pageName = "Optimistic" )
  pageButtonServer("middle", parentSession = session,pageName = "Middle of the Road" )
  pageButtonServer("pessimistic", parentSession = session,pageName = "Pessimistic" )
  pageButtonServer("extreme", parentSession = session,pageName = "Extreme" )
  
  # ssp126 data 
  map_server(id = "ssp126", rasters = allRasters$`126`,countyFeat = county)
  map2_server("ssp126_2")
  # # ssp245 data
  # map_server("ssp245", rasters = allRasters$`245`,countyFeat = county)
  # map2_server("ssp245_2")
  # # ssp370 data
  # map_server("ssp370", rasters = allRasters$`370`,countyFeat = county)
  # map2_server("ssp370_2")
  # # ssp585 data
  # map_server("ssp585", rasters = allRasters$`585`,countyFeat = county)
  # map2_server("ssp585_2")
  
  
  # passing reactive elements to module functions  --------------------------
  # from  https://www.youtube.com/watch?v=oOYaHsPXLvs&ab_channel=RConsortium 40min
  # test <- reactiveVal(1)
  # morevals <- reactiveValues(test = 1)
  #
  # module_server(id = "id_1", results = test )# this works
  # module_server(id = "id_2", results = test() )# this will only return the initial value
  #
  # module_server(id - "id_3", results = morevals) # this works
  # module_server(id = "id_4", results = morevals$test) # this does not work
  # ## passing a object defined in the ui
  # module_server(id - "id_5", results = reactive({input$test})) # this works
  # module_server(id - "id_5", results = input$test) # this does not work.
  
}

shinyApp(ui, server)
