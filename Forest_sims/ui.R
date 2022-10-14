library(shiny)
library(shinyWidgets)
library(dplyr)
library(forcats)
library(leaflet)
library(rgdal)
library(Rcpp)
library(sp)
library(raster)
library(ggplot2)
library(viridis)
library(RColorBrewer)
library(sf)
county<- sf::st_read("./data/KE_Admin1_pro.shp",stringsAsFactors = F)
county_names<-county$ADMIN1
print(county_names)

ui<-fluidPage(theme = "style.css",
              div(style = "padding: 1px 0px; width: '100%'",
                  titlePanel(
                    title = "",
                    windowTitle = ""
                  )),
              #navbarPage(

                # Application title.
                title = div(span(img(src = ""),
                                 "Kenya Afforestation Decision Support Tool",
                                 style = "position: relative; top: 50%; transform: translateY(-50%);")),
                tabsetPanel(
  ###########################################################################################################
  ###########################################################################################################
                  tabPanel("Optmistic" ,
                           tabsetPanel(
                             ###########################################################################################################
                             tabPanel("Climate change in Kenya",
                                      sidebarPanel(
                                        radioButtons(
                                          inputId="Timeline",label= "Pick a future timeperiod:",
                                          choices = c("Near term (2030)", "Medium term (2050)", "Long term (2100)"),
                                        ),

                                        selectInput(

                                          inputId="Layer", label="Pick a variable that you would like to visualize:",
                                          choices = c("Min Temperature", "Max Temperature", "Precipitation", "Net primary productivity"), multiple = F
                                        ),
                                        tags$p(span("Large maps may take a few seconds to render.", style = "color:red")),
                                        tags$p(HTML("<b>Click</b> on a pixel within Kenya to see the county name and pixel value.")),
                                        tags$p(HTML("<b> Click here </b> to see how these data were generated and to learn more about caveats (under construction)"))
                                      ),

                                      mainPanel(
                                        leafletOutput("varchange"),
                                        textOutput("cnty"),
                                        textOutput("facdat"),
                                        textOutput("explain")
                                      )
                             ),
                             ##########################################################################################################
                             tabPanel("Climate and management effects on existing forests",
                                      sidebarPanel(
                                        radioButtons(
                                          inputId="Timeline_1",
                                          label= "Pick a future timeperiod:",
                                          choices = c("Near term (2030)", "Medium term (2050)", "Long term (2100)"),
                                        ),

                                        selectInput(
                                          inputId="Management",
                                           label="Pick a management scenario:",
                                          choices = c( "Do nothing", "Stop grazing", "Stop fires", "Reduce water stress"),
                                           multiple = F
                                        ),
                                        tags$p(span("Large maps may take a few seconds to render.", style = "color:red")),
                                        tags$p(HTML("<b>Click</b> on a pixel within Kenya to see the county name and pixel value.")),
                                        tags$p(HTML("<b> Click here </b> to see how these data were generated and to learn more about caveats (under construction)"))
                                        ),
                                      
                                      mainPanel(
                                        leafletOutput("varchange1"),
                                        textOutput("cnty1"),
                                        textOutput("facdat1"),
                                        textOutput("explain1")
                                      )
                             ),
###############################################################################################################
                        tabPanel("Climate and management effects on expanded forests",
                            sidebarPanel(
                           radioButtons(
                           inputId="Visualize",
                           label= "Visualize an expanded forest cover scenario",
                           choices = c("Forest cover in 2030")
                              ),
                         h4("How does this cover change over time?"),
                         selectInput(
                         inputId="Management12",
                         label="Pick a management scenario:",
                         choices = c("Do nothing", "Stop fires"), multiple = F
                             ),
                         selectInput(
                           inputId="County13",
                           label="Pick a county to visualize forest cover changes:",
                           choices = c("All", county_names), multiple = F
                         ),
                          tags$p(span("Large maps may take a few seconds to render.", style = "color:red")),
                          tags$p(HTML("<b>Click</b> on a pixel within Kenya to see the county name and pixel value.")),
                          tags$p(HTML("<b> Click here </b> to see how these data were generated and to learn more about caveats (under construction)"))
                          ),

                         mainPanel(
                                 leafletOutput("varchange131"),
                                 textOutput('cnty13'),
                                 textOutput('facdat13'),
                                 textOutput("explain13"),
                                 tableOutput("tab13")
                        )
                  ))),
  ###########################################################################################################
  ###########################################################################################################
                  tabPanel ("Middle of the road ",
                            tabsetPanel(
                              tabPanel("Climate change in Kenya",
                                       sidebarPanel(
                                         radioButtons(
                                           inputId="Timeline2",label= "Pick a future timeperiod:",
                                           choices = c("Near term (2030)", "Medium term (2050)", "Long term (2100)"),
                                         ),

                                         selectInput(
                                           inputId="Layer2", label="Pick a variable that you would like to visualize:",
                                           choices = c("Min Temperature", "Max Temperature", "Precipitation", "Net primary productivity"), multiple = F
                                         ),
                                         tags$p(span("Large maps may take a few seconds to render.", style = "color:red")),
                                         tags$p(HTML("<b>Click</b> on a pixel within Kenya to see the county name and pixel value.")),
                                         tags$p(HTML("<b> Click here </b> to see how these data were generated and to learn more about caveats (under construction)"))
                                         ),

                                       mainPanel(
                                         leafletOutput("varchange2"),
                                         textOutput("cnty2"),
                                         textOutput("facdat2"),
                                         textOutput("explain2")
                                       )
                              ),
  ###############################################################################################################################################################
                              tabPanel("Climate and management effects on existing forests",
                                       sidebarPanel(
                                         radioButtons(
                                           inputId="Timeline2_1",label= "Pick a future timeperiod:",
                                           choices = c("Near term (2030)", "Medium term (2050)", "Long term (2100)"),
                                         ),

                                         selectInput(
                                          inputId="Management2", label="Pick a management scenario:",
                                           choices = c("Do nothing", "Stop grazing", "Stop fires", "Reduce water stress"), multiple = F),
                                         tags$p(span("Large maps may take a few seconds to render.", style = "color:red")),
                                         tags$p(HTML("<b>Click</b> on a pixel within Kenya to see the county name and pixel value.")),
                                         tags$p(HTML("<b> Click here </b> to see how these data were generated and to learn more about caveats (under construction)"))
                                       ),

                                       mainPanel(
                                         leafletOutput("varchange3"),
                                         textOutput("cnty3"),
                                         textOutput("facdat3"),
                                         textOutput("explain3")
                                       )
                                     ),

  ###################################################################################################
                    tabPanel("Climate and management effects on expanded forests",
                    sidebarPanel(
                      radioButtons(
                        inputId="Visualize",label= "Visualize an expanded forest cover scenario",
                        choices = c("Forest cover in 2030")
                      ),
                      h4("How does this cover change over time?"),
                      selectInput(
                        inputId="Management22", label="Pick a management scenario:",
                        choices = c( "Do nothing", "Stop fires"), multiple = F
                      ),
                      selectInput(
                        inputId="County23", label="Pick a county to visualize forest cover changes:",
                        choices = c("All", county_names), multiple = F
                      ),
                   tags$p(span("Large maps may take a few seconds to render.", style = "color:red")),
                   tags$p(HTML("<b>Click</b> on a pixel within Kenya to see the county name and pixel value.")),
                   tags$p(HTML("<b> Click here </b> to see how these data were generated and to learn more about caveats (under construction)"))
                    ),

                   mainPanel(
                     leafletOutput("varchange231"), textOutput('cnty23'), textOutput('facdat23'),
                     textOutput("explain23"), tableOutput("tab23")
                   )
             ))),
  ################################################################################################################
  #################################################################################################################
                      tabPanel ("Pessimistic",
                        tabsetPanel(
                           tabPanel("Climate change in Kenya",
                           sidebarPanel(
                           radioButtons(
                           inputId="Timeline3",label= "Pick a future timeperiod:",
                           choices = c("Near term (2030)", "Medium term (2050)", "Long term (2100)"),
                            ),

                         selectInput(
                           inputId="Layer3", label="Pick a variable that you would like to visualize:",
                           choices = c("Min Temperature", "Max Temperature", "Precipitation", "Net primary productivity"), multiple = F
                         ),
                         tags$p(span("Large maps may take a few seconds to render.", style = "color:red")),
                         tags$p(HTML("<b>Click</b> on a pixel within Kenya to see the county name and pixel value.")),
                         tags$p(HTML("<b> Click here </b> to see how these data were generated and to learn more about caveats (under construction)"))
                       ),

                       mainPanel(
                         leafletOutput("varchange4"),
                         textOutput("cnty4"),
                         textOutput("facdat4"),
                         textOutput("explain4")
                       )
              ),
              ###############################################################################################################################################################
              tabPanel("Climate and management effects on existing forests",
                       sidebarPanel(
                         radioButtons(
                           inputId="Timeline3_1",label= "Pick a future timeperiod:",
                           choices = c("Near term (2030)", "Medium term (2050)", "Long term (2100)"),
                         ),

                         selectInput(
                           inputId="Management3", label="Pick a variable that you would like to visualize:",
                           choices = c("Do nothing", "Stop grazing", "Stop fires", "Reduce water stress"), multiple = F),
                         tags$p(span("Large maps may take a few seconds to render.", style = "color:red")),
                         tags$p(HTML("<b>Click</b> on a pixel within Kenya to see the county name and pixel value.")),
                         tags$p(HTML("<b> Click here </b> to see how these data were generated and to learn more about caveats (under construction)"))
                        ),

                       mainPanel(
                         leafletOutput("varchange5"),
                         textOutput("cnty5"),
                         textOutput("facdat5"),
                         textOutput("explain5")
                       )
              ),

     ######################################################################################################
              tabPanel("Climate and management effects on expanded forests",
                          sidebarPanel(
                            radioButtons(
                              inputId="Visualize",label= "Visualize an expanded forest cover scenario",
                              choices = c("Forest cover in 2030")
                            ),
                            h4("How does this cover change over time?"),
                            selectInput(
                              inputId="Management32", label="Pick a management scenario:",
                              choices = c( "Do nothing", "Stop fires"), multiple = F
                            ),
                            selectInput(
                              inputId="County33", label="Pick a county to visualize forest cover changes:",
                              choices = c("All", county_names), multiple = F
                            ),
                            tags$p(span("Large maps may take a few seconds to render.", style = "color:red")),
                            tags$p(HTML("<b>Click</b> on a pixel within Kenya to see the county name and pixel value.")),
                            tags$p(HTML("<b> Click here </b> to see how these data were generated and to learn more about caveats (under construction)"))
                          ),

                       mainPanel(
                         leafletOutput("varchange331"), textOutput('cnty33'), textOutput('facdat33'),
                         textOutput("explain33"), tableOutput("tab33"))

                  ))),

    ###########################################################################################################
  ###########################################################################################################
  ###########################################################################################################
                         tabPanel("Downloads",
                           sidebarPanel(

                              radioButtons(
                              inputId="Climate",label= "Pick a future climate scenario:",
                              choices = c("Optimistic", "Middle of the road", "Pessimistic"),
                              ),
                              radioButtons(
                              inputId="Timeline4",label= "Pick a future timeperiod:",
                              choices = c("Near term (2030)", "Medium term (2050)", "Long term (2100)"),
                             ),
                              selectInput(
                              inputId="County1", label="Pick a county to generate report:",
                              choices = county_names, multiple = F
                             ),
                             selectInput(
                               inputId="Parameter", label="Pick a variable you would like to visualize",
                               choices = c("Population", "Forest cover", "Ecosystem services"), multiple = F
                             ),

                            tags$p(span("Forest cover maps may not accurately represent all forested patched in the county.", style = "color:red")),

                            downloadButton("report", "Generate report"),
                            ),
                           mainPanel("",
                                     plotOutput("varchange6"), plotOutput("varchange7"))
                                     ),
  #########################################################################################################
  #########################################################################################################
  ##########################################################################################################

                                     tabPanel("Model Validation"),
                                     tabPanel("Simulation Details")
                ))
################################################################################################################################################
###############################################################################################################################################
#################################################################################################################
