
####### TK - Search and Comparison




# split <- list('Ferntree GullyUpper Ferntree Gully', 'Surrey HillsCanterbury','Surrey HillsMont Albert')
# update_suburb_name <- function(suburb_name){
#   if (suburb_name == split[1]){
#     suburb_name = 'Upper Ferntree Gully'
#   }
#   if (suburb_name == split[2]){
#     suburb_name = 'Surrey Hills Canterbury'
#   }
#   if (suburb_name == split[3]){
#     suburb_name = 'Surrey Hills Mont Albert'
#   }
#   return(suburb_name)
# }



########################################################
### Turkish
########################################################

library(rgdal)
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


#### suburb profile ----------------------------------
# 
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

# mypal <- colorQuantile(palette = "Reds", domain = vic$Ratio, n = 5, reverse = TRUE)
mypal_tk <- colorQuantile(palette = "Blues", domain = vic$TK, n = 5, reverse = TRUE) 
# mypal_lb <- colorQuantile(palette = "Greens", domain = vic$LB, n = 5, reverse = TRUE)
# mypal_me <- colorQuantile(palette = "Reds", domain = vic$ME, n = 5, reverse = TRUE)

##################################################################################
### New codes - legend html for price 
##################################################################################
#html_legend_price <- img(src="https://i.ibb.co/s13tvbN/price-range.jpg", width = 200, high = 100 )

#html_legend_price <- '<img src = "https://www.google.com/images/branding/googlelogo/1x/googlelogo_color_272x92dp.png"/>'
###control group
control_group <- c("<div style = 'position: relative; display: inline-block'><i class='fa fa-graduation-cap fa-lg'></i></div> School", 
                   "<div style = 'display: inline-block'><i class='fa fa-gavel fa-lg'></i></div> Legal Facility",
                   "<div style = 'display: inline-block'><i class='fa fa-child fa-lg'></i></div> Childcare Facility",
                   "<div style = 'display: inline-block'><i class='fa fa-train fa-lg'></i></div>  Train Stations",
                   "<div style = 'display: inline-block'><i class='fa fa-subway fa-lg'></i></div>  Tram Stations",
                   "<div style = 'display: inline-block'><i class='fa fa-bus fa-lg'></i></div>  Bus Stations")

####### function to get legal services based on a given suburb
business_count <- read.csv('data/hair_food_count.csv', stringsAsFactors = F)

get_business_count <- function(suburb){
  business_subset = subset(business_count, suburb == suburbs)
  no = 0
  if (length(business_subset) > 0){
    no = business_subset[[5]] ###### require update when changing map type
  }
  return(no)
}

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
  useShinyalert(),
  tags$br(),
  # checkboxInput('recommendation', HTML({paste('<p class="shinyjs-hide" style="color:#D4AF37; margin-top:-5px; font-size:20px"><strong>Recommended Suburbs</strong></p>')}), FALSE),
  
  
  mainPanel(style = "background: #F0F4FF; width: 100%; height: 100%",
            fluidRow(column(7, wellPanel(style = "background: white; width: 100%;",
                                         leafletOutput(outputId = "map", width = '100%', height = '560px') %>% withSpinner(type = '6'),
                                         div(id = 'controls', uiOutput("reset")))), # return button
                     # column(1, div(id = 'zoomed', style="margin-top: 100px; text-align: center;", htmlOutput(outputId = 'detail'))), # zoomed in info boses. get information from output$detail
                     column(5, offset = 0, 
                            wellPanel(style = "background: white; height:600px; width: 100%; margin-left: -4%",
                                      # fluidRow(
                                      #          div(style="margin-left : 5%; margin-right: 8%",
                                      #              div(id = 'default', htmlOutput(outputId = 'help_text'))# default and help text
                                      #          )
                                      # ),
                                      div(id = 'input_Panel',
                                          div(id = 'Description',
                                              HTML("<p style = 'color:balck; font-size:14px; margin-bottom: 5%;'>1. To select <span style = 'color : #D4AF37'><strong>Map Suburb</strong></span>, you can move your mouse (or click) on the map; Also you could use search bar on the map to locate<br><br>2. To add <span style = 'color : #D4AF37'><strong>Second Suburb</strong></span> for comparison, you could use search bar below</p>")
                                          ),
                                          pickerInput(
                                            inputId = "search",
                                            width = '70%',
                                            #label = HTML("<p style = 'color:royalblue; font-size:20px; font-weight:bold'>Search</p>"),
                                            #multiple = TRUE,
                                            choices = levels(vic$suburb)
                                            ,
                                            #selected = rownames(mtcars)[1:5]
                                            options = list(`live-search` = T,title = "Second Suburb",showIcon = F)
                                          ),
                                          h5(style = 'margin-top: 5%',hr())
                                      ),
                                      div(id = 'comparison_panel',
                                          style = 'height: 85%; margin-top: 6%',
                                          fluidRow(# headings
                                            column(5,
                                                   
                                                   div(HTML("<p style = 'color: #D4AF37; text-align: center; font-size:18px; margin-bottom: 10%; margin-left: -20px;'><strong>Map Suburb</strong></p>")) 
                                            ),
                                            column(2,
                                                   tags$br()
                                                   #div(HTML({paste('<p style = "font-size:12px; color:black; font-weight:bold; text-align: center; margin-top: 10px">Suburb Name', '</p>')}))
                                            ),
                                            column(5,
                                                   div(HTML("<p style = 'color:#D4AF37; text-align: center; font-size:18px; margin-bottom: 10%; margin-right: -15px'><strong>Second Suburb</strong></p>")) 
                                            )
                                          ),
                                          wellPanel(id = 'row0',
                                                    style = 'height: 9%;margin-bottom: 3px',
                                                    fluidRow( #suburb names
                                                      style = 'margin-top: -8px',
                                                      
                                                      column(5,
                                                             
                                                             div(style='font-size:1.6rem; color:#5067EB; text-align: center; margin-left: -140px; margin-right: -100px', id = 'highlevel1', htmlOutput(outputId = 'suburb_box')) 
                                                      ),
                                                      column(2,
                                                             div(HTML({paste('<p style = "font-size:1.5rem; color:black; font-weight:bold; text-align: center; margin-left: -100px; margin-right: -100px">Name', '</p>')}))
                                                      ),
                                                      column(5,
                                                             #div('Suburb found'),
                                                             div(style='font-size:1.6rem; color:#5067EB; text-align: center; margin-left: -100px; margin-right: -140px', id = 'matchresult', htmlOutput(outputId = 'match_result'))
                                                      )
                                                    )
                                          ),
                                          
                                          wellPanel(id = 'row01',
                                                    style = 'height: 8%;margin-bottom: 3px',
                                                    fluidRow(
                                                      style = 'margin-top: -8px',
                                                      column(4,
                                                             div(
                                                               id = 'highlevel2', htmlOutput(outputId = 'customer_box')) # customer size of suburb hovered 
                                                      ),
                                                      column(4,
                                                             div(HTML({paste('<p style = "font-size:1.5rem; color:black; font-weight:bold; text-align: center; margin-left: -100px; margin-right: -100px">Customer Size', '</p>')}))
                                                      ),
                                                      column(4,
                                                             div(
                                                               id = 'matchresult1', htmlOutput(outputId = 'customer_match')) # customer size of suburb searched
                                                      )
                                                    )),
                                          
                                          wellPanel(id = 'row02',
                                                    style = 'height: 8%;margin-bottom: 3px',
                                                    fluidRow(
                                                      style = 'margin-top: -8px',
                                                      column(4,
                                                             div(id = 'highlevel3', htmlOutput(outputId = 'income_box')) # income level of suburb hovered 
                                                      ),
                                                      column(4,
                                                             div(HTML({paste('<p style = "font-size:1.5rem; color:black; font-weight:bold; text-align: center;margin-left: -100px; margin-right: -100px">Income Level', '</p>')}))
                                                      ),
                                                      column(4,
                                                             div(id = 'matchresult2', htmlOutput(outputId = 'income_match')) # income level of suburb searched 
                                                      )
                                                    )),
                                          wellPanel(id='row03',
                                                    style = 'height: 8%;margin-bottom: 3px',
                                                    fluidRow(
                                                      style = 'margin-top: -8px',
                                                      column(4,
                                                             div(id = 'school_zoomed', htmlOutput(outputId = 'school_text'))
                                                      ),
                                                      column(4,
                                                             div(HTML({paste('<p style = "font-size:1.5rem; color:black; font-weight:bold; text-align: center;margin-left: -100px; margin-right: -100px">Schools', '</p>')}))
                                                      ),
                                                      column(4,
                                                             div(id = 'matchresult3', htmlOutput(outputId = 'school_match'))
                                                      )
                                                    )
                                          ),
                                          wellPanel(id = 'row04',
                                                    style = 'height: 8%;margin-bottom: 3px',
                                                    fluidRow(
                                                      style = 'margin-top: -8px',
                                                      column(4,
                                                             div(id = 'childcare_zoomed', htmlOutput(outputId = 'childcare_text'))
                                                      ),
                                                      column(4,
                                                             div(HTML({paste('<p style = "font-size:1.5rem; color:black; font-weight:bold; text-align: center;margin-left: -100px; margin-right: -100px">Childcare Facilities', '</p>')}))
                                                      ),
                                                      column(4,
                                                             div(id = 'matchresult4', htmlOutput(outputId = 'childcare_match'))
                                                      )
                                                    )
                                          ),
                                          wellPanel(id = 'row05',
                                                    style = 'height: 8%;margin-bottom: 3px',
                                                    fluidRow(
                                                      style = 'margin-top: -8px',
                                                      column(4,
                                                             div(id = 'legal_zoomed', htmlOutput(outputId = 'legal_text'))
                                                      ),
                                                      column(4,
                                                             div(HTML({paste('<p style = "font-size:1.5rem; color:black; font-weight:bold; text-align: center;margin-left: -100px; margin-right: -100px">Legal Services', '</p>')}))
                                                      ),
                                                      column(4,
                                                             div(id = 'matchresult5', htmlOutput(outputId = 'legal_match'))
                                                      )
                                                    )
                                          ),
                                          wellPanel(id = 'row06',
                                                    style = 'height: 8%;margin-bottom: 3px',
                                                    fluidRow(
                                                      style = 'margin-top: -8px',
                                                      column(4,
                                                             div(id = 'highlevel4', htmlOutput(outputId = 'existing_business'))
                                                      ),
                                                      column(4,
                                                             div(HTML({paste('<p style = "font-size:1.5rem; color:black; font-weight:bold; text-align: center;margin-left: -100px; margin-right: -100px">Existing Business', '</p>')}))
                                                      ),
                                                      column(4,
                                                             div(id = 'matchresult6', htmlOutput(outputId = 'existing_business_match'))
                                                      )
                                                    )
                                          ))
                                      
                                      
                                      
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
  
  shinyjs::hide('Legal_zoomed')
 
  
  startup <- reactiveVal(0)
  is_zoomed <- reactiveVal(FALSE) # new code: to track whether is zoomed in view or not to disable mouseover
  shinyjs::hide('controls') # hiding the control panel of reset button (whose div id is controls) in ui
  shinyjs::hide('recommendation_explain') # hide the panel that has the recommendation text in ui
  
  #  (help text)
  output$help_text <- renderText({ 
    help_text = HTML('<p style="color:royalblue;  font-weight:bold; font-size: 16px;text-align: center">Please hover on suburb to see more </p>')
    #help_text = 'Please hover on suburb to see more infomation'
  })
  # shinyjs::show('default')
  # shinyjs::hide('input_Panel')
  # shinyjs::hide('comparison_panel')
  # shinyjs::hide('highlevel2')
  # shinyjs::hide('highlevel3')
  # shinyjs::hide('school_zommed')
  # shinyjs::hide('legal_zommed')
  # shinyjs::hide('childcare_zommed')
  # shinyjs::hide('matchresult')
  # shinyjs::hide('matchresult1')
  # shinyjs::hide('matchresult2')
  # shinyjs::hide('matchresult3')
  # shinyjs::hide('matchresult4')
  # shinyjs::hide('matchresult5')
  # shinyjs::hide('matchresult6')
  
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
                  fillColor = ~mypal_tk(vic$TK),
                  fillOpacity = 0.5,
                  color = "black",
                  smoothFactor = 0.2,
                  label = ~suburb,
                  labelOptions = labelOptions(
                    opacity = 0),
                  highlight = highlightOptions(
                    fill = T,
                    fillColor = ~mypal_tk(vic$TK),
                    fillOpacity = .8,
                    color = ~mypal_tk(vic$TK),
                    opacity = .5,
                    bringToFront = TRUE,
                    sendToBack = TRUE),
                  group = 'Turkish Restaurants',
                  layerId = vic$suburb)%>%
      
      #search functions, can use parameters in options to change the looks and the behaviours
      addSearchFeatures(
        targetGroups = 'Turkish Restaurants',
        options = searchFeaturesOptions(
          position = 'topleft',
          textPlaceholder = 'Map Suburbs', # default  text
          zoom=10, openPopup = TRUE, firstTipSubmit = TRUE,
          collapsed = FALSE, autoCollapse = FALSE, hideMarkerOnCollapse = TRUE )) %>%
      ##################################################################################
    ### New codes - add legend
    ##################################################################################
    addLegend("bottomright", 
              colors =c("#DCE8FF",  "#A0C0F6", "#81A4DF", "#6289CD", "#416FBD	"),
              labels= c("Less","","","", "More"),
              title= "Market Size in Melbourne",
              opacity = 1)
    
  }

  
  react_map <- reactiveVal(base_map())
  
  ### readio button map  
  output$map <- renderLeaflet({
    react_map()
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
  
  
  #### search behaviour -----------------------------------------------------
  
  observeEvent(input$search, {
    selected_suburb <- input$search
    suburb_to_show <- subset(vic, suburb == selected_suburb)
    if (selected_suburb %in% levels(vic$suburb)){
    
    
    
    #### match information box ####
    #print(suburb_to_show@data[["Ratio"]])
    
    # # suburb name
    output$match_result <- renderText({ input$search })
    # # customer size
    customer_output = HTML({paste('<p style = "font-size:15px; color:black;text-align: center;;">', suburb_to_show@data[["Ratio"]], '</p>')})
    output$customer_match <- renderText({customer_output})
    # # income level

    income_output = HTML({paste('<p style = "font-size:15px; color:black;text-align: center;;">', suburb_to_show@data[["income_class"]], '</p>')})
    output$income_match<- renderText({income_output})
    print(income_output)
    # # school
    school_output = HTML('<p style="color:black; font-size: 15px; text-align: center;">',length(subset(school, Address_Town == selected_suburb)[,1]),'</p>')
    output$school_match <- renderText({school_output})
    # # childcare
    childcare_output = HTML('<p style="color:black; font-size: 15px; text-align: center;">5</p>')
    output$childcare_match <- renderText({childcare_output})
    # # legal
    legal_output = HTML('<p style="color:black; font-size: 15px; text-align: center;">5</p>')
    output$legal_match <- renderText({legal_output})
    ####### existing business
    output$existing_business_match <- renderText({
      HTML({paste('<p style="color:black; font-size: 15px; text-align: center;">',get_business_count(input$search),'</p>')})
    })
    
    
    #### match information box end ####
    
    # show match result when a suburb selceted
    
      print(selected_suburb)
      shinyjs::show('matchresult')
      shinyjs::show('matchresult1')
      shinyjs::show('matchresult2')
      shinyjs::show('matchresult3')
      shinyjs::show('matchresult4')
      shinyjs::show('matchresult5')
      shinyjs::show('matchresult6')
    }
    
    
    
  })
  #### hoverover behaviour -----------------------------------------------------
  #### hoverover suburb to see details
  observeEvent(input$map_shape_mouseover$id, {
    startup <- startup() + 1
    if (is_zoomed() == FALSE){
      req(input$map_shape_mouseover$id)
      
      #shinyjs::hide('default') # hide help text
      # shinyjs::show('input_Panel')
      # shinyjs::show('comparison_panel') # correspond to div class id highlevel1 in UI file - just show panel, doesn't change the content
      # shinyjs::show('highlevel2') # correspond to div class id highlevel2 in UI file - just show panel, doesn't change the content
      # shinyjs::show('highlevel3') # correspond to div class id highlevel3 in UI file - just show panel, doesn't change the content
      # shinyjs::show('highlevel4')
      
      
      
      
      selected_suburb <- input$map_shape_mouseover$id
      suburb_to_show <- subset(vic, suburb == selected_suburb)
      
      ### overwrite the default texts 
      # all the texts are styled in HTML at the moment then send to htmlOutput(outputId = 'suburb_box')) in ui 
      output$suburb_box <- renderUI(HTML({paste('<p>', suburb_to_show[1,]$suburb, '</p>')}))
      
      # all the texts are styled in HTML at the moment then send to htmlOutput(outputId = 'income_box')) in ui 
      output$income_box <- renderUI(HTML({paste('<p style = "color:black; font-size: 15px; text-align: center;;">',  suburb_to_show[1,]$income_class,'</p>')}))
      
      # all the texts are styled in HTML at the moment then send to htmlOutput(outputId = 'customer_box'))  in ui 
      output$customer_box <- renderUI(HTML({paste('<p style = "color:black; font-size: 15px; text-align: center;">', suburb_to_show[1,]$TK, '</p>')}))
      
      
      #### new information box ####
      ##Help @Ting
      #  school_text
      output$school_text <- renderText({ 
        HTML('<p style="color:black; font-size: 15px; text-align: center;">',length(subset(school, Address_Town == selected_suburb)[,1]),'</p>')
        #school_text = 'Please hover on suburb to see more infomation'
      })
      shinyjs::show('school_zoomed')
      
      #  childcare_text
      output$childcare_text <- renderText({ 
        HTML('<p style="color:black; font-size: 15px; text-align: center;">', length(subset(childcare, Suburb == selected_suburb)[,1]),'</p>')
        #school_text = 'Please hover on suburb to see more infomation'
      })
      shinyjs::show('childcare_zoomed')
      
      #  legal_text
      output$legal_text <- renderText({ 
        HTML('<p style="color:black; font-size: 15px; text-align: center;">',length(subset(legal, Suburb == selected_suburb)[,1]),'</p>')
        #school_text = 'Please hover on suburb to see more infomation'
      })
      shinyjs::show('legal_zoomed')
      #### new information box end ####
      
      ####### existing business
      output$existing_business <- renderText({
        HTML({paste('<p style="color:black;font-size: 15px; text-align: center;"> ',get_business_count(selected_suburb),'</p>')})
      })
    }
    
    
    
  })
  
  #### observer to listen to the behaviour of reset button, when it's clicked do... -----------------------------------------------------
  observeEvent(input$reset_button, {
    react_map(base_map())  # show the default heatmap
    output$map <- renderLeaflet({
      react_map()
    })
    is_zoomed(FALSE)
    # hide everything and show helpetxt
    #shinyjs::show('default')
    shinyjs::hide('controls')  # hiding the control panel of reset button in ui 
    # shinyjs::hide('input_Panel')
    # shinyjs::hide('comparison_panel') 
    # shinyjs::hide('highlevel2') 
    # shinyjs::hide('highlevel3') 
    # shinyjs::hide('highlevel4')
    # shinyjs::hide('school_zoomed')
    # shinyjs::hide('Legal_zoomed')
    # shinyjs::hide('childcare_zoomed')
    #shinyjs::hide('matchresult')
    
    

    legal_info = ' '
    output$legal_text <- renderText({ 
      legal_info
    })
    
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
                  actionButton(inputId = "reset_button", label = "Back", class = "btn-primary")  # can style the button here 
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
      
      
      #### send new commands to the leaflet instance "map" we create in line 133 (base_map) ----------------------------------
      #### since they have the same variable names, leaflet will just change it based on the following codes 
      # leafletProxy('map') %>% # telling leaflet which instance (map) to change
      #   clearControls() %>% # clear all the control filters
      #   clearShapes() %>% # clear all the polygons
      #   clearMarkers() %>% # clear all the markers 
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
                      fillColor = ~mypal_tk(vic$TK), # heatmap colour 
                      fillOpacity = 0,
                      label = ~suburb,
                      labelOptions = labelOptions(
                        opacity = 0),
                      group = 'Turkish Restaurants',
                      layerId = vic$suburb) %>%
          # plot the selected suburb, colour it 
          addPolygons(data = suburb_to_show,
                      weight = 4, # the weight of the boundary line
                      stroke = T,  # the boundary
                      fillColor = ~mypal_tk(vic$TK), # heatmap colour 
                      fillOpacity = 0.003, 
                      color = "black", 
                      smoothFactor = 0.7,
                      label = ~suburb, 
                      labelOptions = # dont show the label 
                        labelOptions(
                          opacity = 0)) 
      }) 
      
      
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
          #   targetGroups = 'Turkish Restaurants',
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
    
        # train
        train_to_show <- subset(transport_to_show,route_type == 2)
        print(train_to_show$stop_name)
        # tram
        tram_to_show <- subset(transport_to_show,route_type == 0)
        # bus
        bus_to_show <- subset(transport_to_show,route_type == 3)
        
        
        
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
                            popup = tram_to_show$stop_name,
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
                            popup = bus_to_show$stop_name,
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
          #   targetGroups = 'Turkish Restaurants',
          #   options = searchFeaturesOptions(
          #     position = 'topright',
          #     textPlaceholder = 'Search suburbs',
          #     zoom=12, openPopup = TRUE, firstTipSubmit = TRUE,
          #     collapsed = FALSE, autoCollapse = FALSE, hideMarkerOnCollapse = TRUE ))  
          # 
        
        
        
        
        
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
        # search function
        # addSearchFeatures(
        #   targetGroups = 'Turkish Restaurants',
        #   options = searchFeaturesOptions(
        #     position = 'topright',
        #     textPlaceholder = 'Search suburbs',
        #     zoom=12, openPopup = TRUE, firstTipSubmit = TRUE,
        #     collapsed = FALSE, autoCollapse = FALSE, hideMarkerOnCollapse = TRUE ))  
        
        
        
        
        
        
      }
      
      
      
      
      
      
      
      
    }
    
    
  })
  
  
  
  
  
}




shinyApp(ui = ui, server = server)
