


########################################################
####Hair- Default
########################################################

library(dplyr)
library(leaflet)
library(tidyverse)
library(dict)
library(rgdal)
library(romato)### zomato api
library(magrittr)
library(leaflet)
library(htmltools)
library(shinydashboard)
library(dashboardthemes)
library(shinyjs)
library(shiny)
library(leaflet.extras)
library(shinyWidgets)
#devtools::install_github("dreamRs/shinyWidgets")
# library(romato)
# devtools::install_github('andrewsali/shinycssloaders')
library(shinycssloaders) # new package 
library(shinyalert) # new packeges for pop_up


####### function to get legal services based on a given suburb
business_count <- read.csv('data/hair_food_count.csv', stringsAsFactors = F)

get_business_count <- function(suburb){
  business_subset = subset(business_count, suburb == suburbs)
  no = 0
  if (length(business_subset) > 0){
    no = business_subset[[6]] ###### require update when changing map type
  }
  return(no)
}
# read in shape file
vic <- readOGR(dsn = path.expand('data/2016_SA2_shape'), layer = 'merged_all')
# load cuisine ranking file 
# cuisine_top10 <- read.csv('data/cuisine_top10.csv', stringsAsFactors = T)
#ranking <- read.csv('data/total_ranking_allbusiness.csv', stringsAsFactors = F)

# load childcare + legal services + school
legal <- read.csv('data/legal_services.csv', stringsAsFactors = F)
childcare <- read.csv('data/childcare.csv', stringsAsFactors = F)
school <- read.csv('data/greaterM_school.csv', stringsAsFactors = F)


name <- names(vic)
name <- c('suburb', 'Ratio', 'Population', 'income_class', 'LB', 'ME', 'TK')
names(vic) <- name

# create polygon suburbs for plotting recommended suburbs
#tk <- subset(vic, suburb %in% ranking$tk[1:3])

childcare_suburb <- subset(vic, suburb %in% childcare$Suburb)
legal_suburb <- subset(vic, suburb %in% legal$Suburb)
school_suburb <- subset(vic, suburb %in% school$Address_Town)


# cuisine id
cuisine_reference <- list()
cuisine_reference[["MDE"]] <- '137'
cuisine_reference[["TK"]] <- '142'
cuisine_reference[["LB"]] <- '66'
cuisine_to_search <- cuisine_reference[["TK"]]

# city id
cities <- list('Pearcedale','Dromana','Flinders','Hastings','Mornington','Mount Eliza','Rosebud','Somerville')
key1 = 'ff866ef6f69b8e3a15bf229dfaeb6de3'
key2 = '99378b51db2be03b10fcf53fa607f012'
key3 = '436ccd4578d0387765bc95d5aeafda4d'
key4 = '0271743913d22592682a7e8e502daad8'
key5 = 'fe6bcdd36b02e450d7bbc0677b745ab7'



### colour palette for heatmap  ----------------------------------

mypal <- colorQuantile(palette = "Blues", domain = vic$Ratio, n = 5, reverse = TRUE)
#mypal_tk <- colorQuantile(palette = "Blues", domain = vic$TK, n = 5, reverse = TRUE) 
# mypal_lb <- colorQuantile(palette = "Blues", domain = vic$LB, n = 5, reverse = TRUE)
# mypal_me <- colorQuantile(palette = "Blues", domain = vic$ME, n = 5, reverse = TRUE)

##################################################################################
### New codes - legend html for price 
##################################################################################
html_legend_price <- img(src="https://i.ibb.co/s13tvbN/price-range.jpg", width = 200, high = 100 )

#html_legend_price <- '<img src = "https://www.google.com/images/branding/googlelogo/1x/googlelogo_color_272x92dp.png"/>'
###control group
control_group <- c("<div style = 'position: relative; display: inline-block'><i class='fa fa-university fa-lg'></i></div> School", 
                   "<div style = 'display: inline-block'><i class='fa fa-gavel fa-lg'></i></div> Legal Facility",
                   "<div style = 'display: inline-block'><i class='fa fa-child fa-lg'></i></div> Childcare Facility",
                   "<div style = 'display: inline-block'><i class='fa fa-train fa-lg'></i></div>  Train Stations",
                   "<div style = 'display: inline-block'><i class='fa fa-subway fa-lg'></i></div>  Tram Stations",
                   "<div style = 'display: inline-block'><i class='fa fa-bus fa-lg'></i></div>  Bus Stations")



########################################################
#### funtion to get routes of bus and tram##############
########################################################
get_routes <- function(df){
  trams = subset(df, 0 == df$route_type)
  buses = subset(df, 3 == df$route_type)
  tram_routes = levels(factor(trams$route_short_name))
  bus_routes = levels(factor(buses$route_short_name))
  return(list(tram_routes,bus_routes))

}

#D4AF37

#new UI
ui = fluidPage(
  style = 'width: 100%; height: 100%',
  ####### keep app running #####
  tags$head(
    HTML(
      "
          <script>
          var socket_timeout_interval
          var n = 0
          $(document).on('shiny:connected', function(event) {
          socket_timeout_interval = setInterval(function(){
          Shiny.onInputChange('count', n++)
          }, 15000)
          });
          $(document).on('shiny:disconnected', function(event) {
          clearInterval(socket_timeout_interval)
          });
          </script>
          "
    )
  ),
  ####### keep app running #####
  setBackgroundColor('#F0F4FF'),
  tags$head(tags$style(HTML('#pop_up{background-color:#D4AF37}'))),
  shinyjs::useShinyjs(),
  tags$br(),
  
  mainPanel(style = "background: #F0F4FF; width: 100%; height: 100%",
            fluidRow(column(8, offset = 0,wellPanel(style = "background: white; width: 100%;",
                                                    leafletOutput(outputId = "map", width = '100%', height = '560px') %>% withSpinner(type = '6'),
                                                    div(id = 'controls', uiOutput("reset"))
            ),
            a(style = 'font-size: 1.5rem; color: #3A479B' ,"Comparing suburbs? Click Here", href="http://www.demeter.gq/Home/CompareME", target="_top")
            ), # return button
            # column(1, div(id = 'zoomed', style="margin-top: 100px; float: right", htmlOutput(outputId = 'detail'))), # zoomed in info boses. get information from output$detail
            column(4, offset = 0, 
                   wellPanel(style = "background: white; margin-left: -5%;margin-right:-60% ;height:220px; width: 100%",
                             div(id = 'default', style = 'text-align: center;',htmlOutput(outputId = 'help_text')),# default and help text
                             mainPanel(id = 'info_box', style="text-align: left; width: 100%",
                                       tags$div(style='color:#D4AF37; font-size:2rem; font-weight:bold', id = 'highlevel1', htmlOutput(outputId = 'suburb_box')),# zoomed out info boses
                                       
                                       div(id = 'highlevel2', htmlOutput(outputId = 'customer_box')), # zoomed out info boses
                                       div(id = 'highlevel3', htmlOutput(outputId = 'income_box')),# zoomed out info boses
                                       # zoomed
                                       div(id = 'school_zoomed', htmlOutput(outputId = 'school_text')), # shools in zoomed view
                                       div(id = 'childcare_zoomed', htmlOutput(outputId = 'childcare_text')), # childcare in zoomed view
                                       div(id = 'legal_zoomed', htmlOutput(outputId = 'legal_text'))  #legal in zoomed view
                                       
                                       
                                       
                             )),
                   
                   
                   
                   #        style = "background: white; height: 150px",
                   #     tags$p(style='color:#D4AF37; margin-right: 150px; font-size:25px; font-weight:bold', "Details"),
                   # HTML("<p style = 'float:left; font-size: 18px; color:black; 
                   #      font-weight:bold'>Customer Size: </p>"),
                   # HTML("<p style = 'float:left; font-size: 18px; color:black; 
                   #      font-weight:bold'>Income Class: </p>")),
                   
                   
                   # radioGroupButtons(
                   #   inputId = "Id071",
                   #   label = "Label",
                   #   choices = c("A", 
                   #               "B", "C", "D"),
                   #   status = "primary",
                   # checkIcon = list(
                   #   yes = icon("ok",
                   #              lib = "glyphicon"),
                   #   no = icon("remove",
                   #             lib = "glyphicon"))
                   # )
                   
                   wellPanel(style = "margin-left: -5%;background: white; height: 372px;width: 100%; margin-top: -3%",
                             
                             tags$br(),
                             div(id = 'radio button',radioGroupButtons("radio_subset", label = HTML({paste('<p style="color:#D4AF37; margin-top:-5px;margin-bottom: 10%;margin-left: 10%;margin-right: -10%; font-size:18px"><strong>Show Suburbs Only Having:</strong></p>')}), # link to output$radio_subet in the server 
                                                                       
                                                                       choiceNames = list(
                                                                         HTML("<p style = 'font-size: 14px; margin-top:1px; color:black; font-weight:bold'>Schools</p>"),
                                                                         HTML("<p style = 'font-size: 14px; margin-top:1px; color:black; font-weight:bold'>Legal Services</p>"),
                                                                         HTML("<p style = 'font-size: 14px; margin-top:1px; color:black; font-weight:bold'>Childcare Facilities</p>"),
                                                                         HTML("<p style = 'font-size: 14px; margin-top:1px; color:black; font-weight:bold'>All Facilities</p>")
                                                                       ),
                                                                       choiceValues = list(
                                                                         "school", "legal", "childcare", 'reset'
                                                                       ),
                                                                       # status = "primary",
                                                                       direction = 'vertical',
                                                                       selected = 'reset',
                                                                       justified = T
                                                                       
                             )),
                             # showing text instead of radio buttons when zoomed
                             div(id = 'zoomed_text', style='color:#D4AF37; font-size:20px; font-weight:bold; margin-left: 29%; margin-top: -5%; margin-bottom:3%','Business Value'),
                             mainPanel(id = 'zoomed_box', 
                                       style = 'width: 100%; height: 90%;',
                                       div(id = 'business1', 
                                           style = 'text-align: center; margin-bottom:3%',
                                           htmlOutput(outputId = 'existing_business')),
                                       div(id = 'business2',
                                           style = 'text-align: center; margin-bottom:3%',
                                           htmlOutput(outputId = 'restaurant_text')),
                                       
                                       #div(hr()),
                                       
                                       
                                       
                                       mainPanel(style = 'height: 80%; width:100%',
                                                 div(style='margin-top: 5%; margin-bottom: 5%;color:#D4AF37; font-size:20px; font-weight:bold;text-align: center','Acessibility'),
                                                 fluidRow(
                                                   column(4,
                                                          div(style = 'width: 30%; margin: auto',
                                                              HTML(
                                                                '<div style="width: 3.1rem; height: 3.1rem; margin-bottom: 13px  ">
            <svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="bus" class="svg-inline--fa fa-bus fa-w-16" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512">
            <path fill="currentColor" d="M488 128h-8V80c0-44.8-99.2-80-224-80S32 35.2 32 80v48h-8c-13.25 0-24 10.74-24 24v80c0 13.25 10.75 24 24 24h8v160c0 17.67 14.33 32 32 32v32c0 17.67 14.33 32 32 32h32c17.67 0 32-14.33 32-32v-32h192v32c0 17.67 14.33 32 32 32h32c17.67 0 32-14.33 32-32v-32h6.4c16 0 25.6-12.8 25.6-25.6V256h8c13.25 0 24-10.75 24-24v-80c0-13.26-10.75-24-24-24zM112 400c-17.67 0-32-14.33-32-32s14.33-32 32-32 32 14.33 32 32-14.33 32-32 32zm16-112c-17.67 0-32-14.33-32-32V128c0-17.67 14.33-32 32-32h256c17.67 0 32 14.33 32 32v128c0 17.67-14.33 32-32 32H128zm272 112c-17.67 0-32-14.33-32-32s14.33-32 32-32 32 14.33 32 32-14.33 32-32 32z">
            </path>
            </svg>
            </div >')
                                                              
                                                          ),
                                                          HTML('<div style=" width: 100px; text-font: 1.5rem; text-align: center;">Bus Routes</div>')
                                                   ),
                                                   column(4,
                                                          div(style = 'width: 30%;margin: auto',
                                                              HTML(
                                                                '<div style="width: 3rem; height: 3rem; margin: auto; margin-bottom: 15px ">
          <svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="train" class="svg-inline--fa fa-train fa-w-14" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 448 512"><path fill="currentColor" d="M448 96v256c0 51.815-61.624 96-130.022 96l62.98 49.721C386.905 502.417 383.562 512 376 512H72c-7.578 0-10.892-9.594-4.957-14.279L130.022 448C61.82 448 0 403.954 0 352V96C0 42.981 64 0 128 0h192c65 0 128 42.981 128 96zm-48 136V120c0-13.255-10.745-24-24-24H72c-13.255 0-24 10.745-24 24v112c0 13.255 10.745 24 24 24h304c13.255 0 24-10.745 24-24zm-176 64c-30.928 0-56 25.072-56 56s25.072 56 56 56 56-25.072 56-56-25.072-56-56-56z"></path></svg>
          </div >')
                                                              
                                                          ),
                                                          HTML('<div style=" width: 100px;text-font: 1.5rem; text-align: center; ">Tram Routes</div>')
                                                          
                                                   ),
                                                   column(4,
                                                          div(style = 'width: 30%;margin: auto',
                                                              HTML(
                                                                '<div style="width: 3rem; height: 3rem;margin: auto; margin-bottom: 15px ">
          <svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="subway" class="svg-inline--fa fa-subway fa-w-14" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 448 512"><path fill="currentColor" d="M448 96v256c0 51.815-61.624 96-130.022 96l62.98 49.721C386.905 502.417 383.562 512 376 512H72c-7.578 0-10.892-9.594-4.957-14.279L130.022 448C61.82 448 0 403.954 0 352V96C0 42.981 64 0 128 0h192c65 0 128 42.981 128 96zM200 232V120c0-13.255-10.745-24-24-24H72c-13.255 0-24 10.745-24 24v112c0 13.255 10.745 24 24 24h104c13.255 0 24-10.745 24-24zm200 0V120c0-13.255-10.745-24-24-24H272c-13.255 0-24 10.745-24 24v112c0 13.255 10.745 24 24 24h104c13.255 0 24-10.745 24-24zm-48 56c-26.51 0-48 21.49-48 48s21.49 48 48 48 48-21.49 48-48-21.49-48-48-48zm-256 0c-26.51 0-48 21.49-48 48s21.49 48 48 48 48-21.49 48-48-21.49-48-48-48z"></path></svg>
          </div >')
                                                              
                                                          ),
                                                          HTML('<div style="width: 100px; text-font: 1.5rem; text-align: center;">Train Stations</div>')
                                                   )
                                                 )
                                                 
                                       ),
                                       
                                       mainPanel(style = 'margin-top: -20%; width: 100%',
                                                 fluidRow(
                                                   column(4,
                                                          div(style = 'font-size: 3rem;text-align: center',textOutput('bus_routes_count'))), #Bus Route
                                                   column(4,
                                                          div(style = 'font-size: 3rem;text-align: center',textOutput('tram_routes_count'))),#Tram Route
                                                   column(4,
                                                          div(style = 'font-size: 3rem;text-align: center',textOutput(outputId = 'train_stations_count')))#Train Station
                                                 )
                                       )
                             )
                   )
                   
            )
            )
            
  ),
  div(style = '' ,textOutput("keepAlive"))
)



### Server -----------------------------------------------------

server = function(input, output, session){
  #### keep alive
  output$keepAlive <- renderText({
    req(input$count)
    paste("")
  })
  

  
  startup <- reactiveVal(0)
  is_zoomed <- reactiveVal(FALSE) # new code: to track whether is zoomed in view or not to disable mouseover
  
  shinyjs::hide('controls') # hiding the control panel of reset button (whose div id is controls) in ui
  #shinyjs::hide('recommendation_explain') # hide the panel that has the recommendation text in ui
  
  #  (help text)
  output$help_text <- renderText({ 
    help_text = HTML('<p style="color:royalblue;  font-weight:bold; font-size: 16px;">Please hover on suburb to see more infomation</p>')
    #help_text = 'Please hover on suburb to see more infomation'
  })
  shinyjs::show('default')
  shinyjs::hide('info_box')
  # shinyjs::hide('highlevel1')
  # shinyjs::hide('highlevel2')
  # shinyjs::hide('highlevel3')
  # shinyjs::hide('school_zommed')
  # shinyjs::hide('legal_zoomed')
  # shinyjs::hide('childcare_zommed')

  shinyjs::hide('zoomed_text')
  shinyjs::hide('zoomed_box')

  
  # ### only show at start up - default suburb information to show in the information boxes -----------------------------------------------------
  # suburb_to_show <- subset(vic, suburb == 'Caulfield')
  # 
  # # all the texts are styled in HTML at the moment then send to htmlOutput(outputId = 'suburb_box')) in ui 
  # 
  # output$suburb_box <- renderUI(HTML({paste('<p style = "font-size:20px; color:#D4AF37">', suburb_to_show[1,]$suburb, '</p>')}))
  # 
  # # all the texts are styled in HTML at the moment then send to htmlOutput(outputId = 'income_box')) in ui 
  # output$income_box <- renderUI(HTML({paste('<p style = "font-size:15px; color:black">', '<strong>Income Class:</strong>', suburb_to_show[1,]$income_class, '</p>')}))
  # 
  # # all the texts are styled in HTML at the moment then send to htmlOutput(outputId = 'customer_box'))  in ui 
  # output$customer_box <- renderUI(HTML({paste('<p style = "font-size:15px; color:black">', '<strong>Customer Size:</strong>', suburb_to_show[1,]$TK, '</p>')}))
  
  
  
  
  ### initialise a global service side reactiveValues object to store supporting services information and to enable passing/changing this variable between
  ### observers on the server side
  detail_information <- reactiveValues()
  
  
  
  ### default heatmap -----------------------------------------------------
  base_map <- function(){
    
    leaflet(options = leafletOptions(zoomControl = T)) %>%
      # basemap - no shapes or markers
      addProviderTiles(provider = providers$Stamen.TonerLite,
                       options = providerTileOptions(opacity = 0.8,detectRetina = T,minZoom = 9)) %>%
      fitBounds(lng1 = 144.515897, lng2 = 145.626704, lat1 = -37.20, lat2 = -38.50) %>%
      setView(lng = (144.515897 + 145.626704) /2 , lat = (-37.20-38.50)/2, zoom = 9) %>%
      # plot all suburbs in polygon. colour the shapes based on mypal_me function
      # can change shape colour, label style, highlight colour, opacity, etc. 
      # basically everything visual about the map can be changed through different options
      addPolygons(data = vic,
                  weight = .7,
                  stroke = T,
                  fillColor = ~mypal(vic$Ratio),
                  fillOpacity = 0.5,
                  color = "black",
                  smoothFactor = 0.2,
                  label = ~suburb,
                  labelOptions = labelOptions(
                    opacity = 0),
                  highlight = highlightOptions(
                    fill = T,
                    fillColor = ~mypal(vic$Ratio),
                    fillOpacity = .8,
                    color = ~mypal(vic$Ratio),
                    opacity = .5,
                    bringToFront = TRUE,
                    sendToBack = TRUE),
                  group = 'Hair',
                  layerId = vic$suburb)%>%
      
      # search functions, can use parameters in options to change the looks and the behaviours
      addSearchFeatures(
        targetGroups = 'Hair',
        options = searchFeaturesOptions(
          position = 'topleft',
          textPlaceholder = 'Search Suburbs', # default  text 
          zoom=10, openPopup = TRUE, firstTipSubmit = TRUE,
          collapsed = FALSE, autoCollapse = FALSE, hideMarkerOnCollapse = TRUE )) %>%
      ##################################################################################
    ### New codes - add legend
    ##################################################################################
    addLegend("bottomright", 
              colors =c("#DCE8FF",  "#A0C0F6", "#81A4DF", "#6289CD", "#416FBD	"),
              labels= c("Less","","","", "More"),
              title= "Customer Size in Melbourne",
              opacity = 1)
    
  }
  ### map of suburbs with childcare facilities
  childcare_map <- function(){
    leaflet(options = leafletOptions(zoomControl = T)) %>%
      # childcare_map - no shapes or markers
      addProviderTiles(provider = providers$Stamen.TonerLite,
                       options = providerTileOptions(opacity = 0.8,detectRetina = T,minZoom = 9)) %>%
      fitBounds(lng1 = 144.515897, lng2 = 145.626704, lat1 = -37.20, lat2 = -38.50) %>%
      setView(lng = (144.515897 + 145.626704) /2 , lat = (-37.20-38.50)/2, zoom = 9) %>%
      # plot all suburbs in polygon. colour the shapes based on mypal_me function
      # can change shape colour, label style, highlight colour, opacity, etc. 
      # basically everything visual about the map can be changed through different options
      addPolygons(data = childcare_suburb,
                  weight = .7,
                  stroke = T,
                  fillColor = ~mypal(vic$Ratio),
                  fillOpacity = 0.5,
                  color = "black",
                  smoothFactor = 0.2,
                  label = ~suburb,
                  labelOptions = labelOptions(
                    opacity = 0),
                  highlight = highlightOptions(
                    fill = T,
                    fillColor = ~mypal(vic$Ratio),
                    fillOpacity = .8,
                    color = ~mypal(vic$Ratio),
                    opacity = .5,
                    bringToFront = TRUE,
                    sendToBack = TRUE),
                  group = 'childcare_suburb',
                  layerId = childcare_suburb$suburb) %>%
      
      # search functions, can use parameters in options to change the looks and the behaviours
      addSearchFeatures(
        targetGroups = 'childcare_suburb',
        options = searchFeaturesOptions(
          position = 'topleft',
          textPlaceholder = 'Search Suburbs', # default  text 
          zoom=10, openPopup = TRUE, firstTipSubmit = TRUE,
          collapsed = FALSE, autoCollapse = FALSE, hideMarkerOnCollapse = TRUE )) %>%
      ##################################################################################
    ### New codes - add legend
    ##################################################################################
    addLegend("bottomright", 
              colors =c("#DCE8FF",  "#A0C0F6", "#81A4DF", "#6289CD", "#416FBD	"),
              labels= c("Less","","","", "More"),
              title= "Customer Size in Melbourne",
              opacity = 1) 
    
  }
  
  school_map <- function(){
    leaflet(options = leafletOptions(zoomControl = T)) %>%
      # school_map - no shapes or markers
      addProviderTiles(provider = providers$Stamen.TonerLite,
                       options = providerTileOptions(opacity = 0.8,detectRetina = T,minZoom = 9)) %>%
      fitBounds(lng1 = 144.515897, lng2 = 145.626704, lat1 = -37.20, lat2 = -38.50) %>%
      setView(lng = (144.515897 + 145.626704) /2 , lat = (-37.20-38.50)/2, zoom = 9) %>%
      # plot all suburbs in polygon. colour the shapes based on mypal_me function
      # can change shape colour, label style, highlight colour, opacity, etc. 
      # basically everything visual about the map can be changed through different options
      addPolygons(data = school_suburb,
                  weight = .7,
                  stroke = T,
                  fillColor = ~mypal(vic$Ratio),
                  fillOpacity = 0.5,
                  color = "black",
                  smoothFactor = 0.2,
                  label = ~suburb,
                  labelOptions = labelOptions(
                    opacity = 0),
                  highlight = highlightOptions(
                    fill = T,
                    fillColor = ~mypal(vic$Ratio),
                    fillOpacity = .8,
                    color = ~mypal(vic$Ratio),
                    opacity = .5,
                    bringToFront = TRUE,
                    sendToBack = TRUE),
                  group = 'school_suburb',
                  layerId = school_suburb$suburb) %>%
      # search functions, can use parameters in options to change the looks and the behaviours
      addSearchFeatures(
        targetGroups = 'school_suburb',
        options = searchFeaturesOptions(
          position = 'topleft',
          textPlaceholder = 'Search Suburbs', # default text 
          zoom=10, openPopup = TRUE, firstTipSubmit = TRUE,
          collapsed = FALSE, autoCollapse = FALSE, hideMarkerOnCollapse = TRUE ))  %>%
      ##################################################################################
    ### New codes - add legend
    ##################################################################################
    addLegend("bottomright", 
              colors =c("#DCE8FF",  "#A0C0F6", "#81A4DF", "#6289CD", "#416FBD	"),
              labels= c("Less","","","", "More"),
              title= "Customer Size in Melbourne",
              opacity = 1)
    
  }
  
  ### suburbs with legal services 
  legal_map <- function(){
    leaflet(options = leafletOptions(zoomControl = T)) %>%
      # legal - no shapes or markers
      addProviderTiles(provider = providers$Stamen.TonerLite,
                       options = providerTileOptions(opacity = 0.8,detectRetina = T,minZoom = 9)) %>%
      fitBounds(lng1 = 144.515897, lng2 = 145.626704, lat1 = -37.20, lat2 = -38.50) %>%
      setView(lng = (144.515897 + 145.626704) /2 , lat = (-37.20-38.50)/2, zoom = 9) %>%
      # plot all suburbs in polygon. colour the shapes based on mypal_me function
      # can change shape colour, label style, highlight colour, opacity, etc. 
      # basically everything visual about the map can be changed through different options
      addPolygons(data = legal_suburb,
                  weight = .7,
                  stroke = T,
                  fillColor = ~mypal(vic$Ratio),
                  fillOpacity = 0.5,
                  color = "black",
                  smoothFactor = 0.2,
                  label = ~suburb,
                  labelOptions = labelOptions(
                    opacity = 0),
                  highlight = highlightOptions(
                    fill = T,
                    fillColor = ~mypal(vic$Ratio),
                    fillOpacity = .8,
                    color = ~mypal(vic$Ratio),
                    opacity = .5,
                    bringToFront = TRUE,
                    sendToBack = TRUE),
                  group = 'legal_suburb',
                  layerId = legal_suburb$suburb) %>%
      
      # search functions, can use parameters in options to change the looks and the behaviours
      addSearchFeatures(
        targetGroups = 'legal_suburb',
        options = searchFeaturesOptions(
          position = 'topleft',
          textPlaceholder = 'Search Suburbs', # default text 
          zoom=10, openPopup = TRUE, firstTipSubmit = TRUE,
          collapsed = FALSE, autoCollapse = FALSE, hideMarkerOnCollapse = TRUE )) %>%
      ##################################################################################
    ### New codes - add legend
    ##################################################################################
    addLegend("bottomright", 
              colors =c("#DCE8FF",  "#A0C0F6", "#81A4DF", "#6289CD", "#416FBD	"),
              labels= c("Less","","","", "More"),
              title= "Customer Size in Melbourne",
              opacity = 1) 
    
  }
  

  
  
  ### default map -----------------------------------------------------
  
  react_map <- reactiveVal(base_map())
  
  ### readio button map  
  output$map <- renderLeaflet({
    
    
    switch(input$radio_subset, # react to 
           'school' = school_map(),
           'legal' = legal_map(),
           'childcare' = childcare_map(),
           'reset' = react_map())
  }) 
  
  
  #newUI
  output$recommendation_text <- renderText(
    HTML('<p style="color:white; font-weight:bold; font-size: 14px; margin-left: -60px"> 
         <br/>Recommendation is calculated based on Recommendation is calculated based on Recommendation is calculated based onRecommendation is calculated based onRecommendation is calculated based onRecommendation is calculated based onRecommendation is calculated based on <span style="color:#D4AF37"></span> are:
         <br/><span style="color:red; font-size: 25px; margin-left: 20px; font-weight:bold"></span></p>')
  )
  
  
  ### function to subset supporting information  -----------------------------------------------------
  supporting_info <- function(suburb){
    
    
    
    if (suburb %in% legal$Suburb) {
      legal_to_show <- subset(legal, Suburb == suburb) 
      legal_count <- nrow(legal_to_show)
      
      childcare_to_show <- subset(childcare, Suburb == suburb) 
      childcare_count <- nrow(childcare_to_show)
      
      
    }
    
    if (!suburb %in% legal$Suburb)
      
    {
      legal_count <- 'Coming Soon'
      legal_to_show <- ''
      childcare_count <- 'Coming Soon'
      childcare_to_show <- ''
      
      
      
    } 
    
    if (suburb %in% school$Address_Town){
      
      school_to_show <- subset(school, Address_Town == suburb) # school 
      school_count <- nrow(school_to_show)
      
    }
    
    if (!suburb %in% school$Address_Town) {
      
      school_to_show <- ''
      school_count <- 'No Schools Found'
    }
    
    
    list(school_to_show, 
         school_count, 
         legal_to_show,
         legal_count,
         childcare_to_show,
         childcare_count)
    
  }
  
  
  
  
  
  #### hoverover behaviour -----------------------------------------------------
  #### hoverover suburb to see details
  observeEvent(input$map_shape_mouseover$id, {
    startup <- startup() + 1
    if (is_zoomed() == FALSE){
      req(input$map_shape_mouseover$id)
      
      shinyjs::hide('default') # hide help text
      shinyjs::show('info_box')
      # shinyjs::show('highlevel1') # correspond to div class id highlevel1 in UI file - just show panel, doesn't change the content
      # shinyjs::show('highlevel2') # correspond to div class id highlevel2 in UI file - just show panel, doesn't change the content
      # shinyjs::show('highlevel3') # correspond to div class id highlevel3 in UI file - just show panel, doesn't change the content
      
      
      
      
      
      selected_suburb <- input$map_shape_mouseover$id
      suburb_to_show <- subset(vic, suburb == selected_suburb)
      
      ### overwrite the default texts 
      # all the texts are styled in HTML at the moment then send to htmlOutput(outputId = 'suburb_box')) in ui 
      output$suburb_box <- renderUI(HTML({paste('<p style = "font-size:20px; color:#D4AF37">', suburb_to_show[1,]$suburb, '</p>')}))
      
      # all the texts are styled in HTML at the moment then send to htmlOutput(outputId = 'income_box')) in ui 
      output$income_box <- renderUI(HTML({paste('<p style = "color:black;  font-size: 1.5rem; ">', 'Income Class: ', '<strong>',suburb_to_show[1,]$income_class, '</strong></p>')}))
      
      # all the texts are styled in HTML at the moment then send to htmlOutput(outputId = 'customer_box'))  in ui 
      output$customer_box <- renderUI(HTML({paste('<p style = "color:black;  font-size: 1.5rem; ">', 'Customer Size:', '<strong>',suburb_to_show[1,]$TK, '</strong></p>')}))
      
      
      #### new information box ####
      ## edit here @Ting
      #  school_text
      output$school_text <- renderText({ 
        HTML('<p style="color:black;  font-size: 15px;">','Number of Schools: ','<strong>',length(subset(school, Address_Town == selected_suburb)[,1]),'</strong></p>')
        
      })
      #shinyjs::show('school_zoomed')
      
      #  childcare_text
      output$childcare_text <- renderText({ 
        HTML('<p style="color:black;  font-size: 15px; ">','Number of Childcare Facilities:<strong>', length(subset(childcare, Suburb == selected_suburb)[,1]),'</strong></p>')
        
      })
      #shinyjs::show('childcare_zoomed')
      
      #  legal_text
      output$legal_text <- renderText({ 
        HTML('<p style="color:black;  font-size: 15px;">','Number of Legal Services:<strong>', length(subset(legal, Suburb == selected_suburb)[,1]),'</strong></p>')
        
      })
    }
    
    #shinyjs::show('legal_zoomed')
    #### new information box end ####
    
    
  })
  
  #### observer to listen to the behaviour of reset button, when it's clicked do... -----------------------------------------------------
  observeEvent(input$reset_button, {
    output$map <- renderLeaflet({
      
      
      switch(input$radio_subset, # react to 
             'school' = school_map(),
             'legal' = legal_map(),
             'childcare' = childcare_map(),
             'reset' = react_map())
    }) 
    #react_map(base_map())  # show the default heatmap
    
    # new updated
    
    
    is_zoomed(FALSE)
    # hide everything and show helpetxt
    shinyjs::show('default')
    shinyjs::show('radio_subset')
    shinyjs::hide('controls')  # hiding the control panel of reset button in ui 
    shinyjs::hide('zoomed') # hiding the zoomed-in map in ui
    shinyjs::hide('info_box')
    # shinyjs::hide('highlevel1') 
    # shinyjs::hide('highlevel2') 
    # shinyjs::hide('highlevel3') 
    # shinyjs::hide('business2')
    # shinyjs::hide('school_zoomed')
    # shinyjs::hide('Legal_zoomed')
    # shinyjs::hide('childcare_zoomed')
    shinyjs::hide('zoomed_text')
    shinyjs::hide('zoomed_box')
    
    
    
    
    
    # shinyjs::hide('detail1') # correspond to div class id highlevel1 in UI file - just show panel, doesn't change the content
    # shinyjs::hide('detail2') # correspond to div class id highlevel2 in UI file - just show panel, doesn't change the content
    # shinyjs::hide('detail3') # correspond to div class id highlevel3 in UI file - just show panel, doesn't change the content
    
    
    
    # reset and show ui. correspond to div classes in ui 
    #shinyjs::reset('recommendation') # reset the checkbox option to FASLE in checkboxInput('recommendation', 'Show Recommendations', FALSE) 
    #shinyjs::show('checkbox1') # show the panel that has the recommendation checkbox in ui 
    
    
  })
  
  ### return button is in a panel feed to line 98 -----------------------------------------------------
  output$reset <- renderUI({
    absolutePanel(id = "controls", top = "auto", left = 50, 
                  right = "auto", bottom = 70, width = "auto", height = "auto",
                  actionButton(inputId = "reset_button", label = "Back to Overview Map", class = "btn-primary")  # can style the button here 
    )
  })
  
  # information boxes for zoomed in version, feed to div class 'zoomed' in UI file. line 101-----------------------------------------------------
  
  
  
  
  
  ### check box UI. text : Show Recommendations, default setting is FALSE (unchecked )
  #output$checkbox_rec <- renderUI({checkboxInput('recommendation', HTML({paste('<p style="color:#D4AF37; margin-top:-5px; font-size:20px"><strong>Recommended Suburbs</strong></p>')}), FALSE)})
  
  
  ################################################### 
  ##############  For Zomato API - start ############
  ################################################### 
  get_city_ID <- function(suburb){
    ID = 259
    for (s in cities){
      if (s == suburb){
        ID = 1543
      }
    }
    return(ID)
  }
  
  
  
  ## api request function
  search <- function(cityid, api_key, cuisine,query = NULL) {
    
    zmt <- zomato$new(api_key)
    an.error.occured <- FALSE
    
    ## catch the error when no result is found
    tryCatch( { restaurants <- zmt$search(entity_type = 'city', entity_id = cityid, query = query, cuisine = cuisine)}
              , error = function(e) {an.error.occured <<- TRUE})
    if (an.error.occured == FALSE){
      colnames(restaurants) <- make.unique(names(restaurants))
      data <- dplyr::select(restaurants, id,name,cuisines,locality,longitude,latitude,price_range, average_cost_for_two)
      return(data)
    }
    else{
      no_result = TRUE
    }
  }
  
  ## a function to get no_restaurants
  no_restaurants <- function(data) {
    if (typeof(data) =='logical'){
      return(0)
    }
    else{
      nn = length(data$name)
      return(nn)
    }
  }
  
  ################################################### 
  ##############  For Zomato API - end ############
  ################################################### 
  
  
  # observer to listen to clickig on a shape in the map. when there's a click on a suburb, do the following part 1-----------------------------------------------------
  observeEvent(input$map_shape_click, {
    
    is_zoomed(TRUE)
    print (paste0('mapshape is', input$map_shape_click$id))
    click <- input$map_shape_click
    selected_suburb <- click$id # return suburb name
    
    # show text info instead of the radio buttons
    # output$zoomed_text_value <- renderText(
    #   'In This Suburb:'
    # )
    
    shinyjs::show('zoomed_text')
    shinyjs::show('zoomed_box')
    
    
    if (!is.null(selected_suburb)){
      
      detail_information$school_to_show <- supporting_info(selected_suburb)[[1]] # school df
      detail_information$school_count <- supporting_info(selected_suburb)[[2]][1]
      
      detail_information$legal_to_show <- supporting_info(selected_suburb)[[3]]# legal df
      detail_information$legal_count <- supporting_info(selected_suburb)[[4]][1]
      
      detail_information$childcare_to_show <- supporting_info(selected_suburb)[[5]]# childcare df
      detail_information$childcare_count <- supporting_info(selected_suburb)[[6]][1]
      
      shinyjs::hide('radio_subset')
      shinyjs::show('zoomed') # show the ui panel that contains output$detail in ui
      
      #shinyjs::show('business2') # show zoomed in restaurant info
      
      # shinyjs::show('detail1')
      # shinyjs::show('detail2')
      # shinyjs::show('detail3')
      
      output$detail <- renderUI({
        req(input$map_shape_click)
        print (paste0('school count 1:' ,detail_information$school_count))
        absolutePanel(id = 'detail_box',
                      width = '100%',
                      height = 'auto',
                      top = 20,
                      right = 200,
                      class='class = "panel panel-default',
                      draggable = TRUE,
                      HTML({paste('<p style = "font-size:16px">', '<strong>Schools:</strong> ', detail_information$school_count, '</p>')}),
                      HTML({paste('<p style = "font-size:16px">','<strong>Childcare Facilities:</strong> ', detail_information$childcare_count, '</p>')}),
                      HTML({paste('<p style = "font-size:16px">','<strong>Legal Services:</strong>', detail_information$legal_count, '</p>')})
        )
        
      })
    }
  })
  
  # observer to listen to clickig on a shape in the map. when there's a click on a suburb, do the following part 2 -----------------------------------------------------
  observeEvent(input$map_shape_click, { 
    ### define trainsport function
    
    
    print (paste0('mapshape is', input$map_shape_click$id))
    shinyjs::show('controls') # show the absoluatePanel that has the control button object in ui
    shinyjs::hide('checkbox1') # # hide the checkbox panel in ui
    
    
    # subset data based on shape click 
    click <- input$map_shape_click
    selected_suburb <- click$id # return suburb name
    ### define trainsport function
    transport <- reactive({
      df = read.csv('data/transport.csv', stringsAsFactors = F)
      })
    
    
    if (!is.null(selected_suburb)){
      
      ####### existing business
      #HTML({paste('<p style="color:#3A479B;  font-size: 15px;">',paste('Number of Existing Business: <strong>',no),'</strong></p>')})
      output$existing_business <- renderText({
        HTML({paste('<p style="color:#3A479B;  font-size: 15px;"> Number of Existing Business: <strong>',get_business_count(selected_suburb),'</strong></p>')})
      })
      
      
      
      suburb_to_show <- subset(vic, suburb == selected_suburb) # suburb df, customer size, and income
      boundary <- suburb_to_show@polygons[[1]]@Polygons[[1]]@coords  # suburb boundary
      
      school_to_show <- supporting_info(selected_suburb)[[1]] # school df
      school_count <- supporting_info(selected_suburb)[[2]][1]
      
      legal_to_show <- supporting_info(selected_suburb)[[3]]# legal df
      legal_count <- supporting_info(selected_suburb)[[4]][1]
      
      childcare_to_show <- supporting_info(selected_suburb)[[5]]# childcare df
      childcare_count <- supporting_info(selected_suburb)[[6]][1]
      
      
      #### tooltip/popup styleing for clicking on a marker (school)  ----------------------------------
      labs_school <- sapply(seq(nrow(school_to_show)), function(i) {
        # paste0 is used to Concatenate Strings
        paste0( '<p>', 'Name: ', school_to_show[i,]$School_Name, '<br/>', 
                'Address: ', school_to_show[i,]$Address_Line, ' ', school_to_show[i,]$Address_Town,' ', school_to_show[i,]$Address_Postcode, '<br/>', 
                'Phone: ', school_to_show[i,]$Full_Phone_No, '<br/>',
                'Type: ', school_to_show[i,]$School_Type, 
                '<p>') 
      })
      
      #### tooltip/popup styleing for clicking on a marker (childcare) ----------------------------------
      labs_childcare <- sapply(seq(nrow(childcare_to_show)), function(i) {
        # paste0 is used to Concatenate Strings
        paste0( '<p>', 'Name: ', childcare_to_show[i,]$Name.of.Business, '<br/>', 
                'Address: ', childcare_to_show[i,]$Address, '<br/>', 
                'Phone: ', childcare_to_show[i,]$Phone.Number , '<p>') 
      })
      
      #### tooltip/popup styleing for clicking on a marker (legal)  ----------------------------------
      labs_legal <- sapply(seq(nrow(legal_to_show)), function(i) {
        # paste0 is used to Concatenate Strings
        paste0( '<p>', 
                'Name: ', legal_to_show[i,]$Name.of.Business, '<br/>', 
                'Type: ', legal_to_show[i,]$Business.Type, '<br/>',
                'Address: ', legal_to_show[i,]$Address, '<br/>', 
                'Phone: ', legal_to_show[i,]$Phone.Number , '<p>') 
      })

        #react_map()
      output$map <- renderLeaflet({
        #react_map()
        
        leaflet(options = leafletOptions(zoomControl = T)) %>%
          # basemap - no shapes or markers
          addProviderTiles(provider = providers$Stamen.TonerLite,
                           options = providerTileOptions(opacity = 0.8,detectRetina = T,minZoom = 9)) %>%
          fitBounds(lng1 = max(boundary[,1]),lat1 = max(boundary[,2]), # set the view to only see this suburb
                    lng2 = min(boundary[,1]),lat2 = min(boundary[,2]),
                    options = options(zoom = 9)) %>%
          
          ##################################################################################
        ### New codes - argument addLayersControl (add image behind checkbox filters
        ##################################################################################
        addLayersControl(overlayGroups = control_group,
                         options = layersControlOptions(collapsed = F)) %>%
          ##################################################################################
        ### New codes - argument hidecGroup (default setting to uncheck layers)
        ##################################################################################
        hideGroup(group = control_group[5:6]) %>%
          
          # plot all the suburbs polygon but don't show the shapes in order to keep the colouring the same for this suburb
          addPolygons(data = vic,
                      weight = 0,
                      stroke = 0,
                      fillColor = ~mypal(vic$Ratio), # heatmap colour 
                      fillOpacity = 0,
                      label = ~suburb,
                      labelOptions = labelOptions(
                        opacity = 0),
                      group = 'Hair',
                      layerId = vic$suburb) %>%
          # plot the selected suburb, colour it 
          addPolygons(data = suburb_to_show,
                      weight = 4, # the weight of the boundary line
                      stroke = T,  # the boundary
                      fillColor = ~mypal(vic$Ratio), # heatmap colour 
                      fillOpacity = 0.003, 
                      color = "black", 
                      smoothFactor = 0.7,
                      label = ~suburb, 
                      labelOptions = # dont show the label 
                        labelOptions(
                          opacity = 0)) 
      }) 

      #### send new commands to the leaflet instance "map" we create in line 133 (base_map) ----------------------------------
      #### since they have the same variable names, leaflet will just change it based on the following codes 
      
      
      ### if the suburb has legal/childcare services AND schools
      if (selected_suburb %in% legal$Suburb && selected_suburb %in% school$Address_Town)
      {
        
        
        
        # change the map view - zoom in and then add schools 
        leafletProxy('map') %>%
          # plot schools in this suburb
          addAwesomeMarkers(data = school_to_show, 
                            lng = ~ X, 
                            lat = ~ Y, 
                            icon = awesomeIcons( # use awesome icons. can look up icons online to 
                              icon = "graduation-cap",
                              library = "fa",
                              markerColor = "lightred"), 
                            popup = lapply(labs_school, HTML), 
                            popupOptions =  popupOptions(noHide = F, # use css to style pop up box 
                                                         direction = "center",
                                                         style = list(
                                                           "color" = "black",
                                                           "font-family" = "open sans",
                                                           "box-shadow" = "0.1px 0.1px rgba(0,0,0,0.25)",
                                                           "font-size" = "13px",
                                                           "border-color" = "rgba(0,0,0,0.5)")),
                            ##################################################################################
                            ### New codes - argument group (change group name checkbox display)
                            ##################################################################################
                            group = control_group[1]) %>%
          # plot legal in this suburb
          addAwesomeMarkers(data = legal_to_show, 
                            lng = ~ Longitude, 
                            lat = ~ Latitude, 
                            icon = awesomeIcons(
                              icon = "gavel",
                              library = "fa",
                              markerColor = "purple"), 
                            popup = lapply(labs_legal, HTML),
                            popupOptions =  popupOptions(noHide = F,
                                                         direction = "center",
                                                         style = list(
                                                           "color" = "black",
                                                           "font-family" = "open sans",
                                                           "box-shadow" = "0.1px 0.1px rgba(0,0,0,0.25)",
                                                           "font-size" = "13px",
                                                           "border-color" = "rgba(0,0,0,0.5)")),
                            ##################################################################################
                            ### New codes - argument group (change group name checkbox display)
                            ##################################################################################
                            group = control_group[2]) %>%
          # plot legal in this suburb
          addAwesomeMarkers(data = childcare_to_show, 
                            lng = ~ Longitude, 
                            lat = ~ Latitude, 
                            icon = awesomeIcons(
                              icon = "child",
                              library = "fa",
                              markerColor = "green"), 
                            popup = lapply(labs_childcare, HTML),
                            popupOptions =  popupOptions(noHide = F,
                                                         direction = "center",
                                                         style = list(
                                                           "color" = "black",
                                                           "font-family" = "open sans",
                                                           "box-shadow" = "0.1px 0.1px rgba(0,0,0,0.25)",
                                                           "font-size" = "13px",
                                                           "border-color" = "rgba(0,0,0,0.5)")),
                            ##################################################################################
                            ### New codes : argument group  (change group name checkbox display)
                            ##################################################################################
                            group = control_group[3]) 
          
          # search function
          # addSearchFeatures(
          #   targetGroups = 'Hair',
          #   options = searchFeaturesOptions(
          #     position = 'topleft',
          #     textPlaceholder = 'Search Suburbs',
          #     zoom=12, openPopup = TRUE, firstTipSubmit = TRUE,
          #     collapsed = FALSE, autoCollapse = FALSE, hideMarkerOnCollapse = TRUE ))  
        
        
      }
      
      
      ####transport####
      # read and subset transport data based on selected suburb
      transport_to_show <- subset(transport(), suburb == selected_suburb)
      
      
      ### if the suburb only has schools but no legal/childcare services, but has transport, do this 
      if (selected_suburb %in% school$Address_Town && length(transport_to_show$suburb) > 0) {
    
        # train #
        train_to_show <- subset(transport_to_show,route_type == 2)
        # no. of train stations
        output$train_stations_count <- renderText({
          length(train_to_show[[1]])
        })
        print(length(train_to_show[[1]]))
        # tram #
        tram_to_show <- subset(transport_to_show,route_type == 0)
        # tram routes count
        output$tram_routes_count <-renderText({
          length(get_routes(tram_to_show)[[1]])
        })
        
        # bus #
        bus_to_show <- subset(transport_to_show,route_type == 3)
        # bus routes count
        output$bus_routes_count <-renderText({
          length(get_routes(bus_to_show)[[2]])
        })
        
        
        
        leafletProxy('map') %>%
          # plot schools in this suburb
          addAwesomeMarkers(data = school_to_show, 
                            lng = ~ X, 
                            lat = ~ Y, 
                            icon = awesomeIcons(
                              icon = "graduation-cap",
                              library = "fa",
                              markerColor = "lightred"), 
                            popup = lapply(labs_school, HTML),
                            popupOptions =  popupOptions(noHide = F,
                                                         direction = "center",
                                                         style = list(
                                                           "color" = "black",
                                                           "font-family" = "open sans",
                                                           "box-shadow" = "0.1px 0.1px rgba(0,0,0,0.25)",
                                                           "font-size" = "13px",
                                                           "border-color" = "rgba(0,0,0,0.5)")),
                            ##################################################################################
                            ### New codes - argument group  (change group name checkbox display)
                            ##################################################################################
                            group = control_group[1]) %>%
          # plot Trains in this suburb
          addAwesomeMarkers(data = train_to_show, 
                            lat = train_to_show$stop_lat, 
                            lng = train_to_show$stop_lon, 
                            icon = awesomeIcons(
                              icon = "train",
                              library = "fa",
                              markerColor = "blue"), 
                            popup = train_to_show$stop_name,
                            popupOptions =  popupOptions(noHide = F,
                                                         direction = "center",
                                                         style = list(
                                                           "color" = "black",
                                                           "font-family" = "open sans",
                                                           "box-shadow" = "0.1px 0.1px rgba(0,0,0,0.25)",
                                                           "font-size" = "13px",
                                                           "border-color" = "rgba(0,0,0,0.5)")),
                            ##################################################################################
                            ### New codes : argument group  (change group name checkbox display)
                            ##################################################################################
                            group = control_group[4]) %>%
          # plot Tram in this suburb
          addAwesomeMarkers(data = tram_to_show, 
                            lat = tram_to_show$stop_lat, 
                            lng = tram_to_show$stop_lon, 
                            icon = awesomeIcons(
                              icon = "subway",
                              library = "fa",
                              markerColor = "pink"), 
                            popup = paste('Tram Route:',tram_to_show$route_short_name),
                            popupOptions =  popupOptions(noHide = F,
                                                         direction = "center",
                                                         style = list(
                                                           "color" = "black",
                                                           "font-family" = "open sans",
                                                           "box-shadow" = "0.1px 0.1px rgba(0,0,0,0.25)",
                                                           "font-size" = "13px",
                                                           "border-color" = "rgba(0,0,0,0.5)")),
                            ##################################################################################
                            ### New codes : argument group  (change group name checkbox display)
                            ##################################################################################
                            group = control_group[5]) %>%
          # plot Bus in this suburb
          addAwesomeMarkers(data = bus_to_show, 
                            lat = bus_to_show$stop_lat, 
                            lng = bus_to_show$stop_lon, 
                            icon = awesomeIcons(
                              icon = "bus",
                              library = "fa",
                              markerColor = "orange"), 
                            popup = paste('Bus Route:',bus_to_show$route_short_name),
                            popupOptions =  popupOptions(noHide = F,
                                                         direction = "center",
                                                         style = list(
                                                           "color" = "black",
                                                           "font-family" = "open sans",
                                                           "box-shadow" = "0.1px 0.1px rgba(0,0,0,0.25)",
                                                           "font-size" = "13px",
                                                           "border-color" = "rgba(0,0,0,0.5)")),
                            ##################################################################################
                            ### New codes : argument group  (change group name checkbox display)
                            ##################################################################################
                            group = control_group[6]) 
          # search function
          # addSearchFeatures(
          #   targetGroups = 'Hair',
          #   options = searchFeaturesOptions(
          #     position = 'topright',
          #     textPlaceholder = 'Search suburbs',
          #     zoom=12, openPopup = TRUE, firstTipSubmit = TRUE,
          #     collapsed = FALSE, autoCollapse = FALSE, hideMarkerOnCollapse = TRUE ))  
        
        
        
        
        
        
      }
      if (length(transport_to_show$suburb) > 0) {
        
        # train #
        train_to_show <- subset(transport_to_show,route_type == 2)
        # no. of train stations
        output$train_stations_count <- renderText({
          length(train_to_show[[1]])
        })
        print(length(train_to_show[[1]]))
        # tram #
        tram_to_show <- subset(transport_to_show,route_type == 0)
        # tram routes count
        output$tram_routes_count <-renderText({
          length(get_routes(tram_to_show)[[1]])
        })
        
        # bus #
        bus_to_show <- subset(transport_to_show,route_type == 3)
        # bus routes count
        output$bus_routes_count <-renderText({
          length(get_routes(bus_to_show)[[2]])
        })
        
        
        
        leafletProxy('map') %>%
          # plot Trains in this suburb
          addAwesomeMarkers(data = train_to_show, 
                            lat = train_to_show$stop_lat, 
                            lng = train_to_show$stop_lon, 
                            icon = awesomeIcons(
                              icon = "train",
                              library = "fa",
                              markerColor = "blue"), 
                            popup = train_to_show$stop_name,
                            popupOptions =  popupOptions(noHide = F,
                                                         direction = "center",
                                                         style = list(
                                                           "color" = "black",
                                                           "font-family" = "open sans",
                                                           "box-shadow" = "0.1px 0.1px rgba(0,0,0,0.25)",
                                                           "font-size" = "13px",
                                                           "border-color" = "rgba(0,0,0,0.5)")),
                            ##################################################################################
                            ### New codes : argument group  (change group name checkbox display)
                            ##################################################################################
                            group = control_group[4]) %>%
          # plot Tram in this suburb
          addAwesomeMarkers(data = tram_to_show, 
                            lat = tram_to_show$stop_lat, 
                            lng = tram_to_show$stop_lon, 
                            icon = awesomeIcons(
                              icon = "subway",
                              library = "fa",
                              markerColor = "pink"), 
                            popup = paste('Tram Route:',tram_to_show$route_short_name),
                            popupOptions =  popupOptions(noHide = F,
                                                         direction = "center",
                                                         style = list(
                                                           "color" = "black",
                                                           "font-family" = "open sans",
                                                           "box-shadow" = "0.1px 0.1px rgba(0,0,0,0.25)",
                                                           "font-size" = "13px",
                                                           "border-color" = "rgba(0,0,0,0.5)")),
                            ##################################################################################
                            ### New codes : argument group  (change group name checkbox display)
                            ##################################################################################
                            group = control_group[5]) %>%
          # plot Bus in this suburb
          addAwesomeMarkers(data = bus_to_show, 
                            lat = bus_to_show$stop_lat, 
                            lng = bus_to_show$stop_lon, 
                            icon = awesomeIcons(
                              icon = "bus",
                              library = "fa",
                              markerColor = "orange"), 
                            popup = paste('Bus Route:',bus_to_show$route_short_name),
                            popupOptions =  popupOptions(noHide = F,
                                                         direction = "center",
                                                         style = list(
                                                           "color" = "black",
                                                           "font-family" = "open sans",
                                                           "box-shadow" = "0.1px 0.1px rgba(0,0,0,0.25)",
                                                           "font-size" = "13px",
                                                           "border-color" = "rgba(0,0,0,0.5)")),
                            ##################################################################################
                            ### New codes : argument group  (change group name checkbox display)
                            ##################################################################################
                            group = control_group[6]) 
        
        
        
      }
      
     
      
      
      
    }
    
    
  })
  
  
  
  
  
}




shinyApp(ui = ui, server = server)
