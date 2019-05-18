#################
# TK - preference map#
#################
library(shinyWidgets)
library(shiny)
library(rgdal)
library(magrittr)
library(leaflet)
library(htmltools)
library(shinyjs)
library(shiny)
library(leaflet.extras)
library(geosphere)
# devtools::install_github('andrewsali/shinycssloaders')
library(shinycssloaders) # new package 
library(shinyalert) # new packeges for pop_up


##### define the type of the map :  hairdressing
###### require update when changing map type
# map_type <- 'Ratio'
# map_type <- 'LB'
# map_type <- 'ME'
map_type <- 'TK'

######################
######## map #########
######################
# read in shape file
vic <- readOGR(dsn = path.expand('data/2016_SA2_shape'), layer = 'merged_all')


# load childcare + legal services + school
legal <- read.csv('data/legal_services.csv', stringsAsFactors = F)
childcare <- read.csv('data/childcare.csv', stringsAsFactors = F)
school <- read.csv('data/greaterM_school.csv', stringsAsFactors = F)
business_count <- read.csv('data/hair_food_count.csv', stringsAsFactors = F)


name <- names(vic)
name <- c('suburb', 'Ratio', 'Population', 'income_class', 'LB', 'ME', 'TK')
names(vic) <- name





### colour palette for heatmap  ----------------------------------

mypal <- colorQuantile(palette = "Blues", domain = vic$Ratio, n = 5, reverse = TRUE)
# mypal_tk <- colorQuantile(palette = "Blues", domain = vic$TK, n = 5, reverse = TRUE)
# mypal_lb <- colorQuantile(palette = "Greens", domain = vic$LB, n = 5, reverse = TRUE)
#mypal_me <- colorQuantile(palette = "Blues", domain = vic$ME, n = 5, reverse = TRUE)





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

####### function to calculate center of polygons and setview() point
get_center <- function(suburbs){
  coords_1 = rbind(suburbs@polygons[[1]]@Polygons[[1]]@coords)
  coords_2 = rbind(suburbs@polygons[[2]]@Polygons[[1]]@coords)
  coords_3 = rbind(suburbs@polygons[[3]]@Polygons[[1]]@coords)
  center_1 = centroid(coords_1)
  center_2 = centroid(coords_2)
  center_3 = centroid(coords_3)
  
  view_center = centroid(rbind(center_1,center_2,center_3))
  return(view_center)
}

####### function to get school based on a given suburb
get_school <- function(suburb){
  school_subset = subset(school, suburb == Address_Town)
  return(length(school_subset[[1]]))
}
####### function to get childcare based on a given suburb
get_childcare <- function(suburb){
  childcare_subset = subset(childcare, suburb == Suburb)
  return(length(childcare_subset[[1]]))
}
####### function to get legal services based on a given suburb
get_legal <- function(suburb){
  legal_subset = subset(legal, suburb == Suburb)
  return(length(legal_subset[[1]]))
}
####### function to get legal services based on a given suburb
get_business_count <- function(suburb){
  business_subset = subset(business_count, suburb == suburbs)
  no = 0
  if (length(business_subset) > 0){
    no = business_subset[[5]] ###### require update when changing map type
  }
  return(no)
}




# top1 <- supporting_info('Pakenham')
# top2 <- supporting_info('Berwick')
# top3 <- supporting_info('Mornington')


### store detail counts of supporting services in variables

# school_to_show <- do.call("rbind",list(top1[[1]], top2[[1]], top3[[1]])) # coordinates
# legal_to_show <- do.call("rbind",list(top1[[3]], top2[[3]], top3[[3]])) # coordinates
# childcare_to_show <- do.call("rbind",list(top1[[5]], top2[[5]], top3[[5]])) # coordinates
# school_count <- do.call("rbind",list(top1[[2]], top2[[2]], top3[[2]])) # school_count[1] is Pakenham, [2] is Berwick, [3] is Mornington
# legal_count <- do.call("rbind",list(top1[[4]], top2[[4]], top3[[4]]))# legal_count[1] is Pakenham, [2] is Berwick, [3] is Mornington
# childcare_count <- do.call("rbind",list(top1[[6]], top2[[6]], top3[[6]]))# childcare_count[1] is Pakenham, [2] is Berwick, [3] is Mornington

###dummy data

dummy <- read.csv('data/ranking_tk.csv', stringsAsFactors = F)
dummy <- dummy[!duplicated(dummy[c('Suburb')]),] # remove duplicated suburb rows



ui <- fluidPage(
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
  
  useShinyjs(),
  
  
  
  tags$head(
    
    tags$style(HTML( ### src css
      "@import url('//fonts.googleapis.com/css?family=Montserrat');
    p{color:red}

    h3{color: darkorange; font-family: 'Montserrat'; font-weight: bold; margin-top: -5px}
    
    h5{margin-bottom: -5px; margin-top: -5px}
    "
    ))
  ),
  style = 'width: 100%',
  
  tags$br(),
  mainPanel (
    
    style = 'width: 100%; background-color:#F0F4FF',
    
    fluidRow(
      column(7,
             
             shinyjs::hidden(wellPanel(id = "panelB", 
                                       style = "background:	white; width: 100%;",
                                       leafletOutput(outputId = "map", width = '100%', height = '500px') %>% withSpinner(type = '6')
             )),
             wellPanel(
               id = 'panelA',
               style = "background:	white; width: 100%; height: 540px",
               
               div(htmlOutput('title1')),
               tags$br(),
               
               radioGroupButtons(
                 size = 'sm',
                 inputId = "Id001",
                 label = "Customer Size",
                 #status = "primary",
                 choices = c("Not Important", 
                             "Slightly Important", 
                             "Important",
                             "Very Important", 
                             "Extremely Important"),
                 checkIcon = list(
                   yes = icon("ok", 
                              lib = "glyphicon")),
                 justified = T
                 #individual = T
                 #direction = "vertical"
               ),
               h5(hr()), # add break lines
               radioGroupButtons(
                 size = 'sm',
                 inputId = "Id002",
                 label = "Income Level",
                 #status = "primary",
                 choices = c("Not Important", 
                             "Slightly Important", 
                             "Important",
                             "Very Important", 
                             "Extremely Important"),
                 checkIcon = list(
                   yes = icon("ok", 
                              lib = "glyphicon")),
                 justified = T
                 #individual = T
                 #direction = "vertical"
               ),
               h5(hr()),
               radioGroupButtons(
                 size = 'sm',
                 inputId = "Id003",
                 label = "Schools",
                 #status = "primary",
                 choices = c("Not Important", 
                             "Slightly Important", 
                             "Important",
                             "Very Important", 
                             "Extremely Important"),
                 checkIcon = list(
                   yes = icon("ok", 
                              lib = "glyphicon")),
                 justified = T
                 #individual = T
                 #direction = "vertical"
               ),
               h5(hr()),
               radioGroupButtons(
                 size = 'sm',
                 inputId = "Id004",
                 label = "Legal Services",
                 #status = "primary",
                 choices = c("Not Important", 
                             "Slightly Important", 
                             "Important",
                             "Very Important", 
                             "Extremely Important"),
                 checkIcon = list(
                   yes = icon("ok", 
                              lib = "glyphicon")),
                 justified = T
                 #individual = T
                 #direction = "vertical"
               ),
               h5(hr()),
               radioGroupButtons(
                 size = 'sm',
                 inputId = "Id005",
                 label = "Childcare Facilities",
                 
                 #status = "primary",
                 choices = c("Not Important", 
                             "Slightly Important", 
                             "Important",
                             "Very Important", 
                             "Extremely Important"),
                 checkIcon = list(
                   yes = icon("ok", 
                              lib = "glyphicon")),
                 justified = T
                 #individual = T
                 #direction = "vertical"
               )
               
             ),
             
             mainPanel(style = 'color: #3A479B; font-size: 2rem;  font-family: Montserrat;left: 23%; top: 3rem',
                       div(style = 'display:inline-block' ,prettySwitch(inputId = "test",
                                                                        label = 'Show Map',
                                                                        fill = TRUE, 
                                                                        status = "primary",
                       )),
                       div( style = 'margin-left: -23%; margin-bottom: -2%;border: 1px ;background-color: grey; width: 1px; height: 30px; display:inline-block'),
                       #h5(hr(style = 'margin-left: -10% ;border: 1px solid black; width: 50%')),
                       a(style = 'margin-right: -20%; font-size: 2rem; margin-left: 5%; color: #3A479B' ,"Click here to explore!", href="http://www.demeter.gq/Home/TurkishCuisine", target="_top")
             )
             
             
             
             
             
      ),
      
      column(5,
             wellPanel(
               style = "background: white; width: 100%; height: 620px; margin-left: -3%; margin-right:10%",
               
               div(htmlOutput('title2')),
               tags$br(),
               wellPanel(style = "height: 28%; width: 100%; background-color: #F0F4FF; margin-top:-5px",
                         fluidRow(style = "margin-left: 0.5%",
                                  tags$h3(textOutput('top1'))
                         ),
                         fluidRow(
                           column(6,
                                  tags$p(htmlOutput('customer_size01')),
                                  tags$p(htmlOutput('income01')),
                                  tags$p(htmlOutput('business01'))
                           ),
                           column(6,
                                  tags$p(htmlOutput('school01')),
                                  tags$p(htmlOutput('legal01')),
                                  tags$p(htmlOutput('childcare01')))
                         )
               ),
               wellPanel(style = "height: 28%; width: 100%; background-color: #F0F4FF",
                         fluidRow(style = "margin-left: 0.5%",
                                  tags$h3(textOutput('top2'))
                         ),
                         fluidRow(
                           column(6,
                                  tags$p(htmlOutput('customer_size02')),
                                  tags$p(htmlOutput('income02')),
                                  tags$p(htmlOutput('business02'))
                           ),
                           column(6,
                                  tags$p(htmlOutput('school02')),
                                  tags$p(htmlOutput('legal02')),
                                  tags$p(htmlOutput('childcare02')))
                         )
                         
                         
                         
                         
                         
                         
               ),
               wellPanel(style = "height: 28%; width: 100%; background-color: #F0F4FF",
                         fluidRow(style = "margin-left: 0.5%",
                                  tags$h3(textOutput('top3'))
                         ),
                         fluidRow(
                           column(6,
                                  tags$p(htmlOutput('customer_size03')),
                                  tags$p(htmlOutput('income03')),
                                  tags$p(htmlOutput('business03'))
                           ),
                           column(6,
                                  tags$p(htmlOutput('school03')),
                                  tags$p(htmlOutput('legal03')),
                                  tags$p(htmlOutput('childcare03')))
                         )
                         
                         
                         
                         
                         
                         
               )
             )
             
             
      )
    )
  ),
  div(style = '' ,textOutput("keepAlive"))
)


server <- function(input, output, session){
  
  #### keep alive
  output$keepAlive <- renderText({
    req(input$count)
    paste("")
  })
  
  #####################################
  ############ map start ##############
  #####################################
  get_data <- reactive({
    subset(vic, suburb %in% list(recommendation$top1,recommendation$top2,recommendation$top3))
  })

  observe({
    
    output$map <- renderLeaflet({
      # create polygon suburbs for plotting recommended suburbs
      df <- get_data()

      # Get view center
      view_center = get_center(df)
      
      leaflet(options = leafletOptions(doubleClickZoom= FALSE)) %>% # new code: take out zoom control
        # basemap - no shapes or markers
        
        addProviderTiles(provider = providers$Stamen.TonerLite,
                         options = providerTileOptions(opacity = 0.8,detectRetina = T,minZoom = 9)) %>%
        fitBounds(lng1 = 144.515897, lng2 = 145.626704, lat1 = -37.20, lat2 = -38.50) %>%
        setView(lng = view_center[1] , lat = view_center[2], zoom = 10) %>%
        # plot all suburbs in polygon. colour the shapes based on mypal_me function
        # can change shape colour, label style, highlight colour, opacity, etc. 
        # basically everything visual about the map can be changed through different options
        addPolygons(data = df,
                    weight = .7,
                    stroke = T,
                    fillColor = '#4169E1',
                    fillOpacity = 0.5,
                    color = '#4169E1',
                    smoothFactor = 0.2,
                    label = ~suburb,
                    labelOptions = labelOptions(direction = 'top', noHide = T),
                    highlight = highlightOptions(
                      fill = T,
                      fillColor = '#4169E1',
                      fillOpacity = .8,
                      color = '#4169E1',
                      opacity = .5,
                      bringToFront = TRUE,
                      sendToBack = TRUE),
                    group = 'Recommendation - Turkish Restaurants',
                    layerId = df$suburb)
    })
  })
  
  ###################################
  ############ map end ##############
  ###################################

  
  
  output$title1 <-renderText({ 
    help_text = HTML('<p style="color:#16224D;  font-weight:bold; font-size: 2rem;">Please Choose Your Preferences</p>')
    #help_text = 'Please hover on suburb to see more infomation'
  })
  output$title2 <-renderText({ 
    help_text = HTML('<p style="color:#16224D;  font-weight:bold; font-size: 2rem;">Best Suburbs Based On Your Preferences</p>')
    #help_text = 'Please hover on suburb to see more infomation'
  })
  
  ### weights of importance (0 for very not important, 0.35 for very important)
  scaler <- c(0.00, 0.05, 0.15, 0.25, 0.35)
  
  ### user preference. used in the calculation later 
  factors <- reactiveValues(f1 = 0,
                            f2 = 0,
                            f3 = 0,
                            f4 = 0,
                            f5 = 0)
  
  
  ### observer to listen to user choice 
  observe({
    
    factors$f1 <- 
      switch(input$Id001,
             "Not Important" = scaler[1], 
             "Slightly Important" = scaler[2], 
             "Important" = scaler[3],
             "Very Important" = scaler[4], 
             "Extremely Important" = scaler[5])
    
    factors$f2 <- 
      switch(input$Id002,
             "Not Important" = scaler[1], 
             "Slightly Important" = scaler[2], 
             "Important" = scaler[3],
             "Very Important" = scaler[4], 
             "Extremely Important" = scaler[5])
    
    factors$f3 <- 
      switch(input$Id003,
             "Not Important" = scaler[1], 
             "Slightly Important" = scaler[2], 
             "Important" = scaler[3],
             "Very Important" = scaler[4], 
             "Extremely Important" = scaler[5])
    
    factors$f4 <- 
      switch(input$Id004,
             "Not Important" = scaler[1], 
             "Slightly Important" = scaler[2], 
             "Important" = scaler[3],
             "Very Important" = scaler[4], 
             "Extremely Important" = scaler[5])
    
    
    factors$f5 <- 
      switch(input$Id005,
             "Not Important" = scaler[1], 
             "Slightly Important" = scaler[2], 
             "Important" = scaler[3],
             "Very Important" = scaler[4], 
             "Extremely Important" = scaler[5])
  })
  
  
  ### store the calculation based on user preferences
  score <- reactiveValues(preference = ' ') 
  
  ### use matrix to perform calculation. output gives the row id of the top 3 suburb based on total score
  observe({
    score$preference <- list(order(as.matrix(dummy[,2:6]) %*% matrix(c(factors$f1, factors$f2, factors$f3, factors$f4, factors$f5)), 
               decreasing = T)[1:3]) 
    
    })
  
  ### function to look up row id and suburb name 
  lookup_suburb <- function(suburb_no_list){
    list(dummy$Suburb[suburb_no_list[[1]][1]], dummy$Suburb[suburb_no_list[[1]][2]], dummy$Suburb[suburb_no_list[[1]][3]])
  }

  
  recommendation <- reactiveValues(
    top1 = 0,
    top2 = 0,
    top3 = 0
  )

  
  ### update the output 
  observe({

    recommendation$top1 <- lookup_suburb(score$preference)[[1]] # result from lookup function. [[1]] is top1 
    recommendation$top2 <-lookup_suburb(score$preference)[[2]]
    recommendation$top3 <-lookup_suburb(score$preference)[[3]]
    
    #get data for top3 suburbs
    df_top_suburbs <- get_data()
    ######
    #top1#
    ######
    suburb01 <- recommendation$top1
    
    #Suburb name
    output$top1 <- renderText(paste0(suburb01))
    
    #customer size
    output$customer_size01 <- renderText({
      HTML({paste('<p style="color:#16224D;  font-weight:bold; font-size: 12px"> Customer Size: ',df_top_suburbs@data[[map_type]][1],'</p>')}) ###### require update when changing map type
    })
    # income
    output$income01 <- renderText({
      HTML({paste('<p style="color:#16224D;  font-weight:bold; font-size: 12px"> Income Level: ',df_top_suburbs@data[["income_class"]][1],'</p>')})
    })
    
    # existing business
    output$business01 <- renderText({
      HTML({paste('<p style="color:#16224D;  font-weight:bold; font-size: 12px"> Existing Business: ',get_business_count(suburb01),'</p>')})
    })
    
    # school
    output$school01 <- renderText({
      HTML({paste('<p style="color:#16224D;  font-weight:bold; font-size: 12px"> Schools: ',get_school(suburb01),'</p>')})
    })
    
    #Legal
    output$legal01 <- renderText({
      HTML({paste('<p style="color:#16224D;  font-weight:bold; font-size: 12px"> Legal Services : ',get_legal(suburb01),'</p>')})
    })
    
      
    #childcare
    output$childcare01 <- renderText({
      HTML({paste('<p style="color:#16224D;  font-weight:bold; font-size: 12px"> Childcare Facilities : ',get_childcare(suburb01),'</p>')})
    })
    
    
    
    
    #print(get_school(suburb01))
    
    ######
    #top2#
    ######
    suburb02 <- recommendation$top2
    
    #Suburb name
    output$top2 <- renderText(paste0(suburb02))
    
    #customer size
    output$customer_size02 <- renderText({
      HTML({paste('<p style="color:#16224D;  font-weight:bold; font-size: 12px"> Customer Size: ',df_top_suburbs@data[[map_type]][2],'</p>')}) ###### require update when changing map type
    })
    # income
    output$income02 <- renderText({
      HTML({paste('<p style="color:#16224D;  font-weight:bold; font-size: 12px"> Income Level: ',df_top_suburbs@data[["income_class"]][2],'</p>')})
    })
    
    # existing business
    output$business02 <- renderText({
      HTML({paste('<p style="color:#16224D;  font-weight:bold; font-size: 12px"> Existing Business: ',get_business_count(suburb02),'</p>')})
    })
    
    # school
    output$school02 <- renderText({
      HTML({paste('<p style="color:#16224D;  font-weight:bold; font-size: 12px"> Schools: ',get_school(suburb02),'</p>')})
    })
    
    #Legal
    output$legal02 <- renderText({
      HTML({paste('<p style="color:#16224D;  font-weight:bold; font-size: 12px"> Legal Services : ',get_legal(suburb02),'</p>')})
    })
    
    
    #childcare
    output$childcare02 <- renderText({
      HTML({paste('<p style="color:#16224D;  font-weight:bold; font-size: 12px"> Childcare Facilities : ',get_childcare(suburb02),'</p>')})
    })
    
    
    
    ######
    #top3#
    ######
    suburb03 <- recommendation$top3
    
    #Suburb name
    output$top3 <- renderText(paste0(suburb03))
    
    #customer size
    output$customer_size03 <- renderText({
      HTML({paste('<p style="color:#16224D;  font-weight:bold; font-size: 12px"> Customer Size: ',df_top_suburbs@data[[map_type]][3],'</p>')}) ###### require update when changing map type
    })
    # income
    output$income03 <- renderText({
      HTML({paste('<p style="color:#16224D;  font-weight:bold; font-size: 12px"> Income Level: ',df_top_suburbs@data[["income_class"]][3],'</p>')})
    })
    
    # existing business
    output$business03 <- renderText({
      HTML({paste('<p style="color:#16224D;  font-weight:bold; font-size: 12px"> Existing Business: ',get_business_count(suburb03),'</p>')})
    })
    
    # school
    output$school03 <- renderText({
      HTML({paste('<p style="color:#16224D;  font-weight:bold; font-size: 12px"> Schools: ',get_school(suburb03),'</p>')})
    })
    
    #Legal
    output$legal03 <- renderText({
      HTML({paste('<p style="color:#16224D;  font-weight:bold; font-size: 12px"> Legal Services : ',get_legal(suburb03),'</p>')})
    })
    
    
    #childcare
    output$childcare03 <- renderText({
      HTML({paste('<p style="color:#16224D;  font-weight:bold; font-size: 12px"> Childcare Facilities : ',get_childcare(suburb03),'</p>')})
    })

  })
  
  
  
 
  observeEvent(input$test, {
    if (input$test == TRUE){
      shinyjs::show('panelB')
      shinyjs::hide('panelA')
    }
    else {
      shinyjs::show('panelA')
      shinyjs::hide('panelB')
    }
    

  })


}
  




shinyApp(ui, server)


