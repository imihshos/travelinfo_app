library(tidyverse)
library(magrittr)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)
library(shinyTime)
library(htmlwidgets)
library(htmltools)
library(ggmap)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(rworldmap)
library(sp)
library(jsonlite)
library(DataCombine)
library(DT)
library(lubridate)
library(xml2)
library(rvest)

# ========================= Section 1: Data Preparation =========================

## Tab 1 - Europe Travel Map

# Data Preparation and Loading
wiki <- read.csv('wikivoyage-listings-en-latest.csv') %>% 
        select(-wifi, -accessibility)

wiki.complete <- wiki[!is.na(wiki$longitude), ]
wiki.complete <- wiki.complete[!is.na(wiki.complete$latitude), ]
wiki.complete$type <- as.factor(wiki.complete$type)

# Extract long lat data to match with country/continent data
wiki.points <- wiki.complete %>% select(longitude, latitude)

# Function to obtain country from coordinates
coords2country <- function(points){
  countriesSP <- getMap(resolution='low')
  pointsSP <- SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  indices <- over(pointsSP, countriesSP)
  
  return(indices$ADMIN)
}

# Function to obtain continent from coordinates
coords2continent <- function(points){
  countriesSP <- getMap(resolution='low')
  pointsSP <- SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  indices <- over(pointsSP, countriesSP)
  
  return(indices$REGION) 
}

# Match data lat long to continent and country
wiki.complete$continent <- coords2continent(wiki.points)
wiki.complete$country   <- coords2country(wiki.points)

# Sort and Factor Types of Landmarks
wiki.complete$type.sorted <- ifelse(wiki.complete$type %in% c("hotel", "sleep"), "Accommodation", 
                                    ifelse(wiki.complete$type %in% c("drink", "cafe", "eat"), "Food", 
                                           ifelse(wiki.complete$type %in% c("view", "see", "island", 
                                                                            "learn", "do", "city", 
                                                                            "park", "beach", "vicinity"), "Attractions", 
                                                  ifelse(wiki.complete$type == "buy", "Shopping", 
                                                         ifelse(wiki.complete$type == "go", "Transport", "Others")))))

# Filter data to only Europe and remove countries with fewer than 30 rows of data
wiki.complete.Europe <- wiki.complete %>% filter(continent == "Europe")

entries.countries.30 <- wiki.complete.Europe %>% 
                        group_by(country) %>% 
                        summarise(no.of.entries = length(country)) %>% 
                        filter(no.of.entries > 30) 

countries.30 <- entries.countries.30$country

wiki.complete.Europe <- wiki.complete.Europe %>% 
                        filter(country %in% countries.30) %>% 
                        select(-lastEdit, -image, -fax, -wikipedia, -wikidata)

# Cleaning up description for labels
wiki.complete.Europe$title <- str_to_title(wiki.complete.Europe$title)

# Html for description for labels
wiki.complete.Europe$label_text <- ifelse(
  wiki.complete.Europe$description == "" & wiki.complete.Europe$url == "", 
  paste0('<strong>', 'City: ', '</strong>', wiki.complete.Europe$article, '<br/>', 
         '<strong>', 'Place: ', '</strong>', wiki.complete.Europe$title, '<br/>'),
  
  ifelse(wiki.complete.Europe$description == "" & wiki.complete.Europe$url != "",
         paste0('<strong>', 'City: ', '</strong>', wiki.complete.Europe$article, '<br/>', 
                '<strong>', 'Place: ', '</strong>', wiki.complete.Europe$title, '<br/>',
                '<strong>', '<a href = "', wiki.complete.Europe$url, '"',
                '" title = "', wiki.complete.Europe$url, '">',
                'Click here to find out more', '</a>', '</strong>'), 
                                          
         ifelse(wiki.complete.Europe$description != "" & wiki.complete.Europe$url != "", 
                paste0('<strong>', 'City: ', '</strong>', wiki.complete.Europe$article, '<br/>', 
                       '<strong>', 'Place: ', '</strong>', wiki.complete.Europe$title, '<br/>',
                       '<strong>','Description: ', '</strong>', wiki.complete.Europe$description, '</br>',
                       '<strong>', '<a href = "', wiki.complete.Europe$url, '"',
                       '" title = "', wiki.complete.Europe$url, '">',
                       'Click here to find out more', '</a>', '</strong>'),
                                                       
                paste0('<strong>', 'City: ', '</strong>', wiki.complete.Europe$article, '<br/>', 
                       '<strong>', 'Place: ', '</strong>', wiki.complete.Europe$title, '<br/>', 
                       '<strong>','Description: ', '</strong>', wiki.complete.Europe$description)
)))

# Creating dataframes for each type of attraction
attraction.type.list.Europe <- c("Accommodation", "Attractions", "Food", "Transport", "Shopping")

# Legend Html generator:
markerLegendHTML <- function(IconSet) {
  # container div:
  legendHtml <- "<div style='padding: 6px; padding-bottom: 6px;'><strong><h5 style='padding-top:0; padding-bottom:10px; margin: 0;'> Map Marker Legend </h5></strong>"
  
  n <- 1
  # add each icon for font-awesome icons icons:
  for (Icon in IconSet) {
    if (Icon[["library"]] == "fa") {
      legendHtml<- paste0(legendHtml, "<div style='width: auto; height: 45px'>",
                          "<div style='position: relative; top: -6px; display: inline-block; width: 36px; height: 45px' class='awesome-marker-icon-",Icon[["markerColor"]]," awesome-marker'>",
                          "<i style='margin-left: 1px; margin-top: 11px; 'class= 'fa fa-",Icon[["icon"]]," fa-inverse'></i>",
                          "</div>",
                          "<p style='position: relative; top: 4px; display: inline-block; ' >", names(IconSet)[n] ,"</p>",
                          "</div>")    
    }
    n<- n + 1
  }
  paste0(legendHtml, "</div>")
}

IconSet <- awesomeIconList(
  "Accommodation"   = makeAwesomeIcon(icon= 'hotel', markerColor = 'green', iconColor = 'black', library = "fa"),
  "Attractions" = makeAwesomeIcon(icon= 'star', markerColor = 'blue', iconColor = 'black', library = "fa"),
  "Food" = makeAwesomeIcon(icon= 'coffee', markerColor = 'purple', iconColor = 'black', library = "fa"),
  "Transport" = makeAwesomeIcon(icon= 'bus', markerColor = 'red', iconColor = 'black', library = "fa"),
  "Shopping" = makeAwesomeIcon(icon= 'shopping-bag', markerColor = 'orange', iconColor = 'black', library = "fa")
)

# Create travelbasemap
travelbasemap <- leaflet() %>%
  addTiles(group="Default") %>%
  addProviderTiles("Stamen.Toner", group = "Toner") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addLayersControl(baseGroups=c("Default", "Toner", "Toner Lite"),
                   overlayGroups=attraction.type.list.Europe, 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addEasyButton(easyButton(
    icon="fa-globe", title="Back to Default Zoom",
    onClick=JS("function(btn, map){ map.setZoom(2); }")))


## Tab 2 - Covid-19 Data

# Obtaining real time worldwide Covid-19 data from the European Centre for Disease Prevention and Control website
covid_url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/json"
covid_data <- fromJSON(file=covid_url)


# Storing the data as a dataframe and only keeping the Europe data
covid.df <- as.data.frame(t(sapply(covid_data$records,c)))%>% filter(continentExp == 'Europe')
covid.countries <- as.character(unique(covid.df$countriesAndTerritories))

# Appending latitude and longitude data to the dataframe 
covid.coords    <- cbind(covid.countries, ggmap::geocode(covid.countries))

# Keeping only the relevant columns 
covid.df.2 <- covid.df %>% 
  inner_join(covid.coords, by = c('countriesAndTerritories' = 'covid.countries')) %>% 
  select(contains('date'), contains('countries'), contains('cases'), contains('death'), lon, lat) 

# Cumulative refers to cumulative number of cases for 14 days per 100,000
colnames(covid.df.2) <- c('Date', 'Countries', 'Cases', 'Cumulative', 'Deaths', 'lon', 'lat')

covid.df.2$Cases <- as.numeric(covid.df.2$Cases)
covid.df.2$Cumulative <- round(as.numeric(covid.df.2$Cumulative))
covid.df.2$Deaths <- as.numeric(covid.df.2$Deaths)

if (class(covid.df.2$Date) == 'character'){
  covid.df.2$Date <- dmy(covid.df.2$Date)
}

cv_min_date <- min(covid.df.2$Date)
cv_max_date <- max(covid.df.2$Date)


## Tab 3 - Weather

# Web scrapping monthly temperature data from Current Results website
temp <- data.frame(Month = as.character(), City = as.character(), High = as.numeric(), Low = as.numeric())

months <- c("january", "february", "march", "april", "may", "june", 
            "july", "august", "september", "october", "november", "december")

for (month in months){
  url  <- paste0(paste0("https://www.currentresults.com/Weather/Europe/Cities/temperature-", month), ".php")
  page <- read_html(url)
  
  City <- html_text(html_nodes(page, "td:nth-child(3)"))
  HighTemp <- html_text(html_nodes(page, "td:nth-child(4)"))
  LowTemp <- html_text(html_nodes(page, "td:nth-child(5)"))
  
  temp_month <- data.frame(Month = str_to_title(month),City = City, TempHigh = HighTemp, TempLow = LowTemp)
  tempsorted <- temp_month %>% separate(City, c("City","Country"), ", ") %>% select(Country, everything()) %>% arrange(Country)
  temp <- rbind(temp, tempsorted)  
}

temp$TempHigh <- as.numeric(temp$TempHigh)
temp$TempLow  <- as.numeric(temp$TempLow)
temp$TempAverage <- (temp$TempHigh + temp$TempLow)/2

# Web scrapping monthly precipitation data from Current Results website 

precipitation <- data.frame(Month = as.character(), City = as.character(), Precipitation = as.numeric())

for (month in months){
  url2 <- paste0(paste0("https://www.currentresults.com/Weather/Europe/Cities/precipitation-",month), ".php")
  page <- read_html(url2) 
  
  RCity <- html_text(html_nodes(page, "td:nth-child(2)"))
  Rainfall <- html_text(html_nodes(page, "td:nth-child(4)"))
  
  rainfall_month  <- data.frame( Month=str_to_title(month),City=RCity, Precipitation=Rainfall)
  rainfall_sorted <- rainfall_month %>% separate(City, c("City","Country"), ", ") %>% 
                                        select(Country, everything()) %>% 
                                        arrange(Country)
  
  precipitation <- rbind(precipitation, rainfall_sorted)  
}

precipitation$Precipitation<-as.numeric(precipitation$Precipitation)

# Combining temperature and precipitation dataframes together
joincol <- c("City","Month", "Country")
weather <- full_join(temp, precipitation, by=joincol)
weather$Month <- substr(weather$Month, 1, 3)
weather[is.na(weather)] <- 0


## Tab 4 - Tourist Nights

# Data Loading and Preparation
tour1 <- read.csv("TOUR_OCC_NIM1603551018014.csv")
time  <- as.character(tour1[9, ])

tour2 <- tour1[-(1:14), ]
tour  <- tour2[-(39:45), ]

rm(list = c('tour1', 'tour2'))

colnames(tour) <- gsub("-", "", time)

# Selecting relevant columns
values <- select(tour[, 2:64], starts_with("2"))
touristdf <- cbind(tour[, 1],values)

touristdf_colnames  <- c("Country", "Jan-18", "Feb-18", "Mar-18", "Apr-18", "May-18", "Jun-18", "Jul-18", "Aug-18", 
                         "Sep-18", "Oct-18", "Nov-18", "Dec-18","Jan-19", "Feb-19", "Mar-19", "Apr-19", "May-19", 
                         "Jun-19", "Jul-19", "Aug-19", "Sep-19", "Oct-19", "Nov-19", "Dec-19", "Jan-20", "Feb-20", 
                         "Mar-20", "Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20")
colnames(touristdf) <- touristdf_colnames

# Replacing missing data with NA
touristdf[touristdf == ":"] <- NA

# Cleaning up country names and data 
touristdf$Country <- gsub(" \\(.*?\\)", "", touristdf$Country)

for (i in 2:33){
  touristdf[1:38, i] <- gsub(",", "", touristdf[1:38, i])
}


## Tab 5 - Restaurants

rest <- read.csv("EuropeRestaurants.csv", blank.lines.skip = TRUE)
rest[rest == ""] <- NA
rest <- na.omit(rest)
colnames(rest) <- c('Ranking', 'Name', 'City', 'Type', 'Rating', 'Price', 'Price Level', 
                    'Number of Reviews', 'Review 1', 'Review 2', 'Review Date')

## Tab 6 - Luxurious Hotel Ratings

hotel <- read.csv('Hotel_Reviews.csv')

hotel <- hotel %>% 
         select(Hotel_Name, Hotel_Address, Review_Date, Total_Number_of_Reviews, Reviewer_Score, lat, lng) %>% 
         mutate(Review_Date = mdy(Review_Date)) %>%
         group_by(Hotel_Name, Hotel_Address, Total_Number_of_Reviews, lat, lng) %>% 
         summarize(Earliest_Review = min(Review_Date),
                   Latest_Review = max(Review_Date), 
                   Average_Score = round(mean(Reviewer_Score, na.rm = T), 2),
                   Median_Score = median(Reviewer_Score, na.rm = T),
                  Lowest_Score = min(Reviewer_Score, na.rm = T)) %>% 
         ungroup() %>% 
         mutate(country = word(trimws(Hotel_Address), -1), 
                city = word(trimws(Hotel_Address), -2),
                Country = ifelse(country == 'Kingdom', 'United Kingdom', country),
                City = ifelse(city == 'United', 'London', city))

hotel_table <- hotel %>% select(Country, City, Hotel_Name, Hotel_Address,
                                Average_Score, Median_Score, Lowest_Score, 
                                Total_Number_of_Reviews, Earliest_Review, Latest_Review)

colnames(hotel_table) <- c('Country', 'City', 'Name', 'Address', 'Average Score', 'Median Rating',
                           'Lowest Score', 'Number of Reviews', 'Earliest Review', 'Latest Review')

hotel_map <- hotel %>% select(Country, City, Hotel_Name, Hotel_Address, Total_Number_of_Reviews, Average_Score, lat, lng) %>% 
                       filter(!is.na(lat), !is.na(lng)) %>%
                       mutate(Country = as.factor(Country))


# ========================= Section 2: User Interface =========================

ui <- fluidPage(
  navbarPage(
    theme = shinytheme("cosmo"), collapsible = TRUE, "Overview of European Countries", id = "nav",
 
    # About Us Tab =================================================
    tabPanel('About Us', id="aboutus",
             
             mainPanel(
               h3("Introduction"),
               h5(p("Hello, we are a group of NUS Students from the School of Business.
                    Through this page, we hope that you will be able to find key information to help in your travel planning! ")),
               p(),
               h3(p("Philosophy of Site")),
               h5((tags$li("Travel Map - Use this to get a snippet of what your country of interest has to offer in terms of attractions, food and more.")),
               h5(tags$li("Covid-19 - Worried about your health? Fret Not! With this map, you can see the live distribution of Covid-19 cases.")),
               h5(tags$li("Weather - This interactive graph tells you what to expect with regards to climate of your country of interest. Know more to pack less clothes!")),
               h5(tags$li("Tourist Nights - Curious about how popular your country of interest is? All you need to know is located here!")),
               h5(tags$li("Restaurants - Look for your favourite cuisine and food places, based on multiple reviews!")),
               h5(tags$li("Luxurious Hotels - Fancy a nice accomodation? Here you can find the ratings of some of the most luxurious hotels!")),
                  ),
               p(),
               h3(p("Can't wait for your next holiday? Join us on this journey to your next destination using our app now!")),
               tags$img(src = "https://images.unsplash.com/photo-1566371486490-560ded23b5e4?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=crop&w=900&q=60")
               )
    ),
    
    # Travel Map Tab =================================================
    tabPanel('Travel Map', id="maptab",
             sidebarPanel(
               shinyjs::useShinyjs(),
               id = "side-panel2",
               h5(tags$strong("Select the country/countries that you wish to find out more about from the drop-down bar below.")),
               h6(tags$ol(tags$li("To view only one country, click the reset button, and pick the country of interest."),
                          tags$li("To get an overview of multiple countries, select the countries of interest."),
                          tags$li("You may also type your country of interest in the search bar."),
                          tags$li("The landmark markers are clustered based on the number of landmarks in each sub-area. 
                                           Click to zoom in for details of individual markers."),
                          tags$li("Click on each marker to find out more about that landmark."))),
               p(),
               pickerInput("travel_country", "Country:",   
                           choices = as.character(unique(sort(wiki.complete.Europe$country))), 
                           options = list(`actions-box` = TRUE, `none-selected-text` = "Choose your destination!", `live-search`=TRUE, `live-search-placeholder`="Type your destination!"),
                           selected = NULL,
                           multiple = TRUE),
               actionButton("travel_reset_input", "Reset"),
               p(),
               markerLegendHTML(IconSet = IconSet) %>% lapply(htmltools::HTML)
             ),
             
             mainPanel(
               h5("Select the type of landmarks you wish to find out more about or toggle the map display type from the layer options."),
               tags$style(type = "text/css", "#mymap {height: calc(100vh - 80px) !important;}"),
               leafletOutput('travelmap') %>% withSpinner())
             
    ), 
 
    # Covid-19 Tab =================================================
    tabPanel('Covid-19', id = 'covid',
             headerPanel("COVID-19 Cases In Europe"),
             sidebarPanel(
               h6("The slider could be used for the map to view the number of COVID cases on a particular day, and to see the progression of cases in the graph."),
               
               # Slider bar for input of dates
               sliderInput(inputId = "covid_date",
                           label = h5(tags$strong("Select Date:")),
                           value = cv_max_date,
                           min = cv_min_date,
                           max = cv_max_date,
                           timeFormat = "%d %b"),
               
               h6("The drop down bar could be used for the graph to view the trend of COVID cases for countries of your choice."),
               
               # Adding drop down menu for input of country
               pickerInput(inputId = "covid_country", 
                           label = "Select Country:",   
                           choices = unique(covid.df.2$Countries), 
                           options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!",
                                          `live-search` = TRUE, `live-search-placeholder`= "Type a Country!"),
                           selected = unique(covid.df.2$Countries)[1],
                           multiple = TRUE),
             ), 
             
             # Having 2 sub-tabs: 1 for the map and 1 for the graph 
             mainPanel(
               tabsetPanel(id = 'tab_being_displayed',
                           tabPanel('Map', leafletOutput('covidmap')),
                           tabPanel('Graph', plotlyOutput('covidgraph'))
               )
             )             
    ),    
    
    # Weather Tab =================================================
    tabPanel('Weather', id = 'weather',
             headerPanel("Monthly Weather Data"),
             sidebarPanel(
               selectInput(inputId = "weather_country", 
                           label = "Select Country:",
                           choices = as.character(unique(weather$Country)), 
                           selected = NULL),    
               selectInput(inputId = "weather_city", 
                           label = "Select City:",
                           choices = as.character(unique(weather$City)),
                           selected = NULL), 
               helpText("Note: Temperation/Precipitation data not available for some countries")
               ),
               mainPanel(
                 h5("Temperature and Precipitation of major cities in Europe based on historical weather data collected over 30 years"), 
                 plotlyOutput("weatherplot")      
               )
    ),

    # Tourist Nights Tab =================================================
    tabPanel('Tourist Nights', id = 'nights',
             headerPanel("Monthly Visitor Nights"),
               sidebarPanel(
                 shinyjs::useShinyjs(),
                 id = "side-panel",
                 selectInput(inputId = "tourist_country1", 
                             label = "Select Country 1:",
                             choices = as.character(unique(sort(touristdf$Country))), 
                             selected = NULL), 
                 selectInput(inputId = "tourist_country2", 
                             label = "Select Country 2:",
                             choices = c("", as.character(unique(sort(touristdf$Country)))), 
                             selected = NULL),
                 actionButton("reset_input", "Reset"),
                 helpText("Note: Users can select up to 2 countries for comparison")
               ), 
               mainPanel(
                 h5("Nights spent at tourist accomodations by international tourists can be used as a proxy for international tourist arrivals"), 
                 plotlyOutput("touristplot")
               )
    ),

    # Restaurant Tab ================================================= 
    tabPanel('Restaurants', id = 'restaurant',
             headerPanel('Restaurant Reviews by City'),
             sidebarPanel(
               selectInput(inputId = "r_city",
                           label = "City",
                           choices = unique(rest$City),
                           selected = "London"),
               selectInput(inputId = "r_type",
                           label = "Cuisine/Food Type/Place Type",
                           choices = unique(rest$Type),
                           selected = "International"),
               sliderInput(inputId = 'r_rating', 
                           tags$label("Minimum Ratings: 1 = Awful, 3 = Average, 5 = Fantastic"),
                           value = 5,
                           min = 0,
                           max = 5,
                           step = 0.5),
               sliderInput(inputId = 'r_review', 
                           label = 'Number of Reviews',
                           value = c(0, 10000),
                           min = min(rest$`Number of Reviews`),
                           max = max(rest$`Number of Reviews`),
                           step = 500),
               awesomeCheckboxGroup(inputId = 'r_price', 
                                    label = 'Price Level:',
                                    choices = unique(rest$`Price Level`),
                                    selected = 'Moderate')
               ),
               
               mainPanel(
                 DT::DTOutput('restauranttable')
               )
    ),
    
    # Luxurious Hotel Tab ================================================= 
    tabPanel('Luxurious Hotels', id = 'hotel',
             headerPanel('Luxurious Hotel Ratings in Popular Cities'), 
             sidebarPanel(
               awesomeCheckboxGroup(inputId = 'h_country', 
                                    label = 'Choose Country:',
                                    choices = unique(hotel_table$Country),
                                    selected = 'United Kingdom'),
               sliderInput(inputId = 'h_number', 
                           label = 'Minimum Number of Reviews:',
                           value = min(hotel_table$`Number of Reviews`),
                           min = 0,
                           max = 16000,
                           step = 200),
               sliderInput(inputId = 'h_score',
                           label = 'Minimum Review Score:',
                           value = 1,
                           min = 1, 
                           max = 9.5,
                           step = 0.5)
               ),
               
               mainPanel(tabsetPanel(tabPanel('Table', DT::DTOutput('hotel_table')), 
                                     tabPanel('Map', leaflet::leafletOutput('hotel_plot')))
               )
    ),
    
        
    # Credits Tab ==================================
    tabPanel('Credits', id = "credits",
             mainPanel(p("This website was built using RShiny."),
                       p("Other than Shiny, the following were used:"),
                       p(tags$ul(tags$li("Symbols from Font Awesome"),
                                 tags$li("Image from unsplash.com"))),
                       p("The following R packages were used to build this RShiny application:"),
                       p(),
                       tags$code("base"), tags$code("dpylr"), tags$code("leaflet"), tags$code("plotly"),
                       tags$code("ggplot2"), tags$code("ggmap"), tags$code("tidyverse"), tags$code("leaflet"), tags$code("leaflet.extras"),
                       tags$code("rworldmap"), tags$code("jsonlite"), tags$code("lubridate"), tags$code("DT"), tags$code("DataCombine"),
                       tags$code("xml2"), tags$code("rvest"), tags$code("htmltools"), tags$code("htmlwidgets"),
                       tags$code("magrittr"), tags$code("shinythemes"), tags$code("shinyWidgets"), tags$code("shinyjs"),
                       tags$code("shinycssloaders"), tags$code("shinyTime")
             )
    )
  )
)


# ========================= Section 3: Server Information =========================

server <- function(input, output, session){
  
  # Travel Map Tab ================================
  output$travelmap <- renderLeaflet({
    travelbasemap
  })
  
  zoom <- reactive({
    wiki.complete.Europe %>% filter(country %in% input$travel_country)
  })
  
  travelmap_select1 <- reactive({
    wiki.complete.Europe %>% filter(country %in% input$travel_country, type.sorted == attraction.type.list.Europe[1])
  })
  
  travelmap_select2 <- reactive({
    wiki.complete.Europe %>% filter(country %in% input$travel_country, type.sorted == attraction.type.list.Europe[2])
  })
  
  travelmap_select3 <- reactive({
    wiki.complete.Europe %>% filter(country %in% input$travel_country, type.sorted == attraction.type.list.Europe[3])
  })
  
  travelmap_select4 <- reactive({
    wiki.complete.Europe %>% filter(country %in% input$travel_country, type.sorted == attraction.type.list.Europe[4])
  })
  
  travelmap_select5 <- reactive({
    wiki.complete.Europe %>% filter(country %in% input$travel_country, type.sorted == attraction.type.list.Europe[5])
  })
  
  observeEvent(input$travel_country, {
    
    leafletProxy("travelmap") %>%
      clearMarkerClusters() %>%
      clearMarkers() %>%
      fitBounds(lng1 =  min(zoom()$longitude), lat1 = min(zoom()$latitude), 
                lng2 =  max(zoom()$longitude), lat2 = max(zoom()$latitude)) %>%
      addAwesomeMarkers(lng = travelmap_select1()$longitude,
                        lat = travelmap_select1()$latitude, 
                        popup = travelmap_select1()$label_text %>% lapply(htmltools::HTML),
                        icon = IconSet$Accommodation,
                        group = attraction.type.list.Europe[[1]],
                        clusterOptions = markerClusterOptions()) %>%
      addAwesomeMarkers(lng = travelmap_select2()$longitude,
                        lat = travelmap_select2()$latitude, 
                        popup = travelmap_select2()$label_text %>% lapply(htmltools::HTML),
                        icon = IconSet$Attractions,
                        group = attraction.type.list.Europe[[2]],
                        clusterOptions = markerClusterOptions()) %>%
      addAwesomeMarkers(lng = travelmap_select3()$longitude,
                        lat = travelmap_select3()$latitude, 
                        popup = travelmap_select3()$label_text %>% lapply(htmltools::HTML),
                        icon = IconSet$Food,
                        group = attraction.type.list.Europe[[3]],
                        clusterOptions = markerClusterOptions()) %>%
      addAwesomeMarkers(lng = travelmap_select4()$longitude,
                        lat = travelmap_select4()$latitude, 
                        popup = travelmap_select4()$label_text %>% lapply(htmltools::HTML),
                        icon = IconSet$Transport,
                        group = attraction.type.list.Europe[[4]],
                        clusterOptions = markerClusterOptions()) %>%
      addAwesomeMarkers(lng = travelmap_select5()$longitude,
                        lat = travelmap_select5()$latitude, 
                        popup = travelmap_select5()$label_text %>% lapply(htmltools::HTML),
                        icon = IconSet$Shopping,
                        group = attraction.type.list.Europe[[5]],
                        clusterOptions = markerClusterOptions())
  })
  
  #reset button - map
  observeEvent(input$travel_reset_input, {
    shinyjs::reset("side-panel2")
  })

  
  # Covid-19 Tab ================================
  # Covid-19 Line Graph 
  output$covidgraph <- renderPlotly({
    
    req(input$covid_country)
    covid.plot <- covid.df.2 %>% filter(Countries %in% input$covid_country, Date <= input$covid_date)
    p <- plot_ly(covid.plot)
    
    # Using bar graphs to show the number of new Covid cases per day 
    p <- p %>% add_bars(x = ~Date, 
                        y = ~Cases,
                        name = ~Countries,
                        color = ~Countries,
                        hovertemplate = ~paste('Date: %{x}', '<br>New Cases: %{y}'))
    
    # Using line graphs to show the cumulative cases for 14 days per 100,000
    p <- p %>% add_lines(x = ~Date, 
                         y = ~Cumulative, 
                         line = list(color=~Countries, width=2, dash="dot"),
                         name = ~Countries,
                         yaxis = "y2",
                         hovertemplate = ~paste('Date: %{x}', '<br>Cumulative Cases for 14 days per 100000: %{y}'))
    
    # Having 2 y-axes, 1 for the line graph and 1 for the bar graph     
    p <- p %>% layout(yaxis2 = list(tickfont = list(size=8), overlaying = "y", side = "right",
                                    automargin = TRUE, title = "New Cases", 
                                    titlefont = list(size = 10), range = c(0, max(covid.df.2$Cumulative))),
                      xaxis = list(title="Date", titlefont = list(size = 10), 
                                   tickfont = list(size = 10), tickangle = 30),
                      yaxis = list(side = 'left', title = 'Cumulative Cases for 14 days per 100000', 
                                   titlefont = list(size = 10), tickfont = list(size = 9)), 
                      legend = list(orientation = 'h', y=-0.2, font = list(size = 10))) 
    
    p
    
  }) 
  
  # Covid-19 Map
  reactive_db <- reactive({covid.df.2 %>% filter(Date == input$covid_date)})
  
  output$covidmap <- renderLeaflet({
    
    leaflet() %>% 
      addTiles(group = "Default") %>% 
      addProviderTiles("Stamen.Toner", group = "Toner") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      addLayersControl(position = "bottomright",
                       baseGroups = c("Default", "Toner", "Toner Lite"),
                       overlayGroups = c("COVID - New Cases", "COVID - Cumulative Cases for 14 days per 100,000"),
                       options = layersControlOptions(collapsed = TRUE)) %>% 
      fitBounds(0, -25, 90, 65) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addEasyButton(easyButton(icon = "fa-globe", 
                               title = "Back to Default Zoom",
                               onClick = JS("function(btn, map){ map.setZoom(2); }")))
  })
  
  # Adding circle points to the map to show both new cases and cumulative cases for 14 days per 100,000 and allow for comparisons across different countries.
  observeEvent(input$covid_date, {
    
    req(input$tab_being_displayed == "Map")
    leafletProxy("covidmap") %>% 
      clearMarkers() %>% 
      clearShapes() %>%
      addCircleMarkers(data = reactive_db(), 
                       lat = ~lat, 
                       lng = ~lon, 
                       weight = 1, 
                       radius = ~(Cases)^(1/4), 
                       fillOpacity = 0.2, 
                       color = "#cc0264", 
                       group = 'COVID - New Cases', 
                       label = sprintf("<strong>%s (New)</strong><br/>Confirmed cases: %s<br/>Deaths: %d", 
                                       reactive_db()$Countries, 
                                       reactive_db()$Cases, 
                                       reactive_db()$Deaths) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                   textsize = "15px", direction = "auto")) %>%
      addCircleMarkers(data = reactive_db(), 
                       lat = ~lat, 
                       lng = ~lon, 
                       weight = 1, 
                       radius = ~(Cumulative)^(1/4), 
                       fillOpacity = 0.2, 
                       color = "#0267cc", 
                       group = 'COVID - Cumulative Cases for 14 days per 100,000', 
                       label = sprintf("<strong>%s (Cumulated)</strong><br/>Confirmed cases: %s<br/>Deaths: %d", 
                                       reactive_db()$Countries, 
                                       reactive_db()$Cumulative, 
                                       reactive_db()$Deaths) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                   textsize = "15px", direction = "auto")) 
  })
  
  
  # Weather Tab ================================
  # City drop-down according to country input 
  observe({
    x = input$weather_country
    updateSelectInput(session,'weather_city', 
                      choices = weather[weather$Country == x, "City"], 
                      selected = NULL)
  })
  
  # Weather data plot
  output$weatherplot <-renderPlotly({
    
    citydata <- weather[weather$Country == input$weather_country & weather$City == input$weather_city, ]
    citydata$Month <- factor(citydata$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                                        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
    
    ay <- list(tickfont = list(size = 8),
               overlaying = "y",
               side = "right",
               automargin = TRUE,
               title = "Average Temperature(°C)", 
               titlefont = list(size = 10),
               range = c(min(citydata$TempLow), 35))
    
    fig <- plot_ly(citydata)
    
    #Using line graph to show monthly average temperature
    fig <- fig %>% add_trace(x = ~Month,
                             y = ~TempAverage, 
                             type = "scatter",
                             mode = "line",
                             name = "Temperature", 
                             yaxis = "y2",
                             hovertemplate = ~paste('Month: %{x}', '<br>Average Temperature: %{y:.0}°C'))
    
    #Using ribbon graph to show monthly maximum and minimum temperature 
    fig <- fig %>% add_ribbons(x = ~Month, 
                               ymin = ~TempLow, 
                               ymax = ~TempHigh,
                               name = "Temperature", 
                               showlegend= FALSE, 
                               yaxis = "y2",
                               line = list(color = 'rgba(7, 164, 181, 0.05)'),
                               fillcolor = 'rgba(7, 164, 181, 0.2)', 
                               hovertemplate = ~paste('Month: %{x}', '<br>Highest/Lowest Temperature: %{y:.0}°C'))
    
    #Using bar graphs to show the monthly precipitation
    fig <- fig %>% add_bars(x = ~Month, 
                            y = ~Precipitation,
                            width = 0.6,
                            name = "Precipitation",
                            marker = list(color = '#F08080'), 
                            hovertemplate = ~paste('Month: %{x}', '<br>Average Precipitation: %{y:.0}mm'))
    
    #Using a line graph to show annual average temperature 
    fig <- fig %>% add_lines(x = ~Month, 
                             y = mean(citydata$TempAverage), 
                             line = list(width=1, dash="dot"),
                             name = "Annual Average Temperature",
                             yaxis = "y2",
                             hoverinfo = "none")
    
    #Using a line graph to show average temperature (from Jan 2018 to latest data)
    fig <- fig %>% add_lines(x = ~Month, 
                             y = mean(citydata$Precipitation), 
                             line = list(width=1, dash="dot"),
                             name = "Annual Average Precipitation",
                             hoverinfo = "none")
    
    #Creating a secondary axis for Temperature data (Primary axis is for Precipitation data)
    fig <- fig %>% layout(yaxis2 = ay,
                          xaxis = list(title="Month", titlefont = list(size = 10), tickfont = list(size=10), tickangle = 30),
                          yaxis = list(side = 'left', title = 'Average Precipitation(mm)', 
                                       titlefont = list(size = 10), range = c(0, 120), tickfont = list(size=9)), 
                          legend = list(orientation = 'h', y=-0.2, font = list(size=10)))
  })

  
  # Tourist Nights Tab ================================
  output$touristplot <-renderPlotly({
    
    countrydata <- gather(touristdf[touristdf$Country == input$tourist_country1 | touristdf$Country == input$tourist_country2, ],
                          Month, Nights, "Jan-18":"Aug-20")
    
    countrydata$Month  <- factor(countrydata$Month, levels = touristdf_colnames[-1])
    countrydata$Nights <- as.numeric(countrydata$Nights)
    
    country <-  countrydata[complete.cases(countrydata), ]
    country1data <- country[country$Country == input$tourist_country1, ]
    country2data <- country[country$Country == input$tourist_country2, ]
    
    if (nrow(country1data) > nrow(country2data)){
      allmonths <- country1data$Month
    } else {
      allmonths <- country2data$Month
    }
    
    fig1 <- plot_ly()
    
    #Using bar graphs to show monthly tourist nights for first country 
    fig1 <- fig1 %>% add_bars(data = country1data,
                              x = ~Month, 
                              y = ~Nights, 
                              name = input$tourist_country1,
                              marker = list(color = '#FF9673'), 
                              hovertemplate = ~paste('Month: %{x}', '<br>Nights: %{y:.0}'))
    
    #Using bar graphs to show monthly tourist nights for second country 
    fig1 <- fig1 %>% add_bars(data = country2data, 
                              x = ~Month, 
                              y = ~Nights, 
                              name = input$tourist_country2,
                              marker = list(color = '#CC1480'), 
                              hovertemplate = ~paste('Month: %{x}', '<br>Nights: %{y:.0}'))
    
    #Using line graph to show average tourist nights for first country
    fig1 <- fig1 %>% add_lines(data = country1data,
                               x = allmonths,
                               y = mean(country1data$Nights), 
                               line = list(width = 1, dash = "dot", color = "green"),
                               name = paste("Average Nights for", input$tourist_country1),
                               hoverinfo = "none")
    
    #Using line graph to show average tourist nights for second country
    fig1 <- fig1 %>% add_lines(data = country2data, 
                               x = allmonths,
                               y = mean(country2data$Nights), 
                               line = list(width = 1, dash = "dot", color = "blue"),
                               name = paste("Average Nights for", input$tourist_country2),
                               hoverinfo = "none")
    
    fig1 <- fig1 %>% layout(xaxis = list(title = list(text="Month", standoff=3), titlefont = list(size = 10), 
                                         tickfont = list(size=8), tickangle = 45),
                            legend = list(orientation = 'h', y=-0.2, font = list(size=10)))
    fig1
    
  })
  
  # Reset button for Tourist Nights
  observeEvent(input$reset_input, {shinyjs::reset("side-panel")})
  
  
  # Restaurants Tab ================================
  output$restauranttable <- DT::renderDT({
    rest %>% filter(City == input$r_city, 
                    Type == input$r_type,
                    Rating <= input$r_rating,
                    `Number of Reviews` >= input$r_review[1] & `Number of Reviews` <= input$r_review[2],
                    `Price Level` == input$r_price) %>% 
      select(Name, Rating, `Price Level`, `Number of Reviews`, `Review 1`, `Review 2`)
  })
  
  
  # Luxurious Hotels Tab ================================ 
  # Table  
  output$hotel_table <- DT::renderDT({
    
    hotel_table %>% filter(`Country` %in% input$h_country,
                           `Number of Reviews` >= input$h_number,
                           `Average Score` >= input$h_score) %>% 
                    select(`Name`, `Address`, `Average Score`, `Lowest Score`, 
                           `Number of Reviews`, `Latest Review`)
  })
  
  # Map
  output$hotel_plot  <- leaflet::renderLeaflet({
    req(input$h_country)
    m <- leaflet()
    
    for (i in 1:length(input$h_country)){
      map_data <- hotel_map %>% filter(Country == input$h_country[i],
                                       Total_Number_of_Reviews >= input$h_number, 
                                       Average_Score >= input$h_score)
      
      labs <- lapply(seq(nrow(map_data)), function(i) {
        paste0('<p>', map_data[i, "Hotel_Name"], '</p>', 
               map_data[i, "Hotel_Address"],'<p>', 
               'Average Rating: ', map_data[i, "Average_Score"], '</p>' ) 
      })
      
      m <- m %>% addAwesomeMarkers(lng = map_data$lng,
                                   lat = map_data$lat, 
                                   popup = lapply(labs, htmltools::HTML), 
                                   icon = makeAwesomeIcon(icon = 'hotel', markerColor = 'blue', 
                                                          iconColor = 'white', library = "fa"),
                                   group = input$h_country[i])
    }
    
    m <- m %>% addTiles(group = "Default") %>%
               addProviderTiles("Stamen.Toner", group = "Toner") %>%
               addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>% 
               addLayersControl(baseGroups = c('Default', 'Toner', 'Toner Lite'), 
                                overlayGroups = input$h_country) %>% 
               addEasyButton(easyButton(icon = "fa-globe", 
                                        title = "Back to Default Zoom",
                                        onClick = JS("function(btn, map){ map.setZoom(2); }")))
    
    m
  })  
  
}

shinyApp(ui, server)