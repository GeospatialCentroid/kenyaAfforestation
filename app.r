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


# input dataset -----------------------------------------------------------
## returns a named list with specific inputs 
# inputs <- renderInputOld()

# global variables -----------------------------------------
## define names for the climate features
panelNames <-
  c(
    "Optimistic Climate Future",
    "Middle of the Road Climate Future",
    "Pessimistic Climate Future",
    "Extreme Climate Future"
  )

# new input datasets  -----------------------------------------------------
inputs_new <- renderInputs()
## raster inputs
clim_new <- inputs_new$rasters
## county names
county_names <- inputs_new$countyNames
## county shp
county <- inputs_new$county

### process into groups
allRasters_new <- prepClim(rasters = clim_new, ssps = c("hist","126","245","370", "585"))
### preprocess all palette objects 
palettes <- getPallette(allRasters_new)
palTest <- generatePalettes(clim_new)

###** note: content to be added based on previous feedback 
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
  
  tabPanel(title = "Downloads",
           h2("Under construction")),
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
  map_server(id = "ssp126", histRasters = allRasters_new$hist, 
             sspRasters =  allRasters_new$`126`,
             ssp = "126",
             histPal = palettes[[1]],
             sspPal = palettes[[2]],
             pals = palTest,
             countyFeat = county)
  map2_server("ssp126_2")
  # ssp245 data
  map_server(id = "ssp245", 
             histRasters = allRasters_new$hist, 
             sspRasters =  allRasters_new$`245`,
             ssp = "245",
             histPal = palettes[[1]],
             sspPal = palettes[[3]],
             countyFeat = county)  
  map2_server("ssp245_2")
  # ssp370 data
  map_server(id = "ssp370", 
             histRasters = allRasters_new$hist, 
             sspRasters =  allRasters_new$`370`,
             ssp = "370",
             histPal = palettes[[1]],
             sspPal = palettes[[4]],
             countyFeat = county)  
  map2_server("ssp370_2")
  # ssp585 data
  map_server(id = "ssp585", 
             histRasters = allRasters_new$hist, 
             sspRasters =  allRasters_new$`585`,
             ssp = "585",
             histPal = palettes[[1]],
             sspPal = palettes[[5]],
             countyFeat = county)    # map2_server("ssp585_2")
  
  
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
