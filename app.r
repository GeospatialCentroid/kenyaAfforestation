### current testing the kenya afforestation revamp 
### carverd@colostate.edu 
### 20221014

library(shiny)
library(leaflet)
library(bslib)
# source modules
lapply(list.files(path = "modules/", pattern = ".R", full.names = TRUE), source)
# source UI or Server only functions 
lapply(list.files(path = "functions/", pattern = ".R", full.names = TRUE), source)

### define names for the climate features 
panelNames <- c("Optimistic Climate Future", "Middle of the Road Climate Future", 
                "Pessimistic Climate Future", "Extreme Heat Climate Future")

### define county_names
county_names<- c("one","two","three")
# UI section --------------------------------------------------------------
ui <- navbarPage(
  theme = bs_theme(version = 5, bootswatch = "minty") %>% 
    bslib::bs_add_rules(sass::sass_file("www/style.scss")),
  # the text that appears next to the pages 
  title =  HTML("Kenya Afforestation </br> Decision Support Tool"),
  # the text in the browser tab 
  windowTitle = "Kenya Afforestation Application",
  # means of applying data to all pages -- could be useful in footer section
  # header = h5("This content appears at the top of every page "),
  # footer = "This content appears at the bottom of every page",
  
# Home page --------------------------------------------------------------- 
  tabPanel(title = "Home",
           htmlTemplate("www/homepage.html",
                        button = actionButton("action", "Action"))
            
            
  ),

# combine scenarios into navbar menu
navbarMenu(
  title = "Climate Scenarios",
  # Optimistic --------------------------------------------------------------
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
  # Middle of the road ------------------------------------------------------
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
  # Pessimistic -------------------------------------------------------------
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
  # Extreme Heat ------------------------------------------------------------
  tabPanel(title = "Extreme Heat",
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

# Additional Nav bar objects ----------------------------------------------

tabPanel(title = "Downloads",
         h2("content will be added to Model Downloads")),
tabPanel(title = "Model Validation",
         h2("content will be added to Model Validation")),
tabPanel(title = "Simulation Details",
         h2("content will be added to Simulation Details"))

)

server <- function(input,output, session){
  # ssp126 data
  map_server("ssp126")
  map2_server("ssp126_2")
  # ssp245 data 
  map_server("ssp245")
  map2_server("ssp245_2")
  # ssp370 data 
  map_server("ssp370")
  map2_server("ssp370_2")
  # ssp585 data
  map_server("ssp585")
  map2_server("ssp585_2")

  
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

shinyApp(ui,server)
