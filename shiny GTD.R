#loading the required libraries
library(shiny)
library(sf)
library(tidyverse)
library(tmap)
library(ggplot2)
library(leaflet)
library()

# Loading the GTD data
gtd_sf <- st_read("GTD.shp")

# Defining the UI
ui <- shinyUI(fluidPage(

  titlePanel("Global Terrorism Database Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      # creating a drop down menu that allows to select a value from a list.
      #the default year is 2010, the default region is "Middle East & 
      #North Africa", and the default attack type is "Bombing/Explosion"
      selectInput("year", "Select a year (map only):", 
                  choices = unique(gtd_sf$year),
                  selected = "2010"),
      selectInput("region", "Select a region (map only):",
                  choices = c("North America", "Central America & Caribbean", 
                              "South America", "East Asia", "Southeast Asia", 
                              "South Asia", "Central Asia", "Western Europe", 
                              "Eastern Europe", "Middle East & North Africa", 
                              "Sub-Saharan Africa"),
                  selected = "Middle East & North Africa"),
      selectInput("attckty", "Select an attack type (map only):",
                  choices = c("Assassination", "Bombing/Explosion", "Armed Assault",
                              "Hijacking", "Hostage Taking (Barricade Incident)",
                              "Hostage Taking (Kidnapping)", 
                              "Facility/Infrastructure Attack",
                              "Unarmed Assault", "Unknown"),
                  selected = "Bombing/Explosion")
    ),
    mainPanel(
      tabsetPanel(
        #"Map" tab: this will display a leaflet map using the leafletOutput function.
        tabPanel("Map", 
                 leafletOutput("map")),
        #This tab will display a time series plot using the plotOutput function.
        tabPanel("Time Series",
                 
                 sidebarPanel(
                   selectInput("country_choice", "Select a country:", 
                               choices = unique(gtd_sf$country))
                 ),
                 plotOutput("time_series")
        ),
        #This tab will display a bar chart using the plotOutput function
        tabPanel("Bar Chart",
                 #creating a dropdown menu, which allow to select a variable 
                 #to display on the chart (region, type of attack, target, 
                 sidebarPanel(
                   #using the selectInput function to create this dropdown menu
                   selectInput("bar_var", "Select a variable to display:", 
                               choices = c("Region" ="region", "Type of attack" 
                                           ="attckty", "Target" = "trgttyp", 
                                           "Weapon Type"="wepntyp"),
                               selected = "region")
                 ), 
                 plotOutput("bar_chart")
        )
      )
    )
  )
)
)
# Defining the server
server <- shinyServer(function(input, output) {
  #creating a reactive subset of the GTD data based on user inputs,
  #subseting the gtd_sf dataset based on the user's selection of country_choice 
  #input. It will update automatically when the input changes.
  gtd_country <- reactive({
    gtd_sf %>% 
      filter(country == input$country_choice)
  })
  
  # Creating a reactive subset of GTD data based on user inputs
  #gtd_subset subsets the gtd_sf data based on the user inputs selected 
  #in the sidebar of the "Map" tab panel. The inputs are year, region, and 
  #attckty, which correspond to the year of the attack, the region of the world 
  #where the attack occurred, and the type of attack, respectively.
  gtd_subset <- reactive({
    gtd_sf %>% 
      filter(year == input$year,
             region == input$region,
             attckty == input$attckty)
  })
  
  #Rendering the leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      #adding a base map layer to the map
      addTiles() %>%
      #adding markers to the map at the locations of terrorist attacks.
      addMarkers(data = gtd_subset(), 
                 clusterOptions = markerClusterOptions(),
                 #creating the popup that displays the country and attack type 
                 #information for the selected terrorist attack.
                 popup = paste("<strong>Country:</strong> ", 
                               gtd_subset()$country, "<br>",
                               "<strong>Attack type:</strong>", 
                               gtd_subset()$attckty
                               ))
  })
  
  # Rendering the time series plot
  #The plot shows a time series analysis of the number of terrorist attacks 
  #over time for a specific country that is selected using the selectInput
  #widget in the sidebarPanel.
  output$time_series <- renderPlot({
    #The group_by and count functions are used to aggregate the data 
    #by year and count the number of attacks in each year
    ggplot(gtd_country() %>% group_by(year) %>% count(), aes(x = year, y = n)) +
      geom_line() +
      labs(title = paste0("Terrorist Attacks by Year in ", input$country_choice),
           x = "Year", y = "Number of Attacks")
  })
  
  # Defining server function for bar chart
  #It creates a bar chart that shows the number of terrorist attacks by a 
  #variable selected, such as region, type of attack, target, or weapon type.
  
  
  # Render bar chart
  output$bar_chart <- renderPlot({
    #using the !!sym() function to evaluate the user input as a symbol and 
    #unquote it in the context of the ggplot object and the reorder() function
    #to reorder the categories based on the number of attacks in descending order
    ggplot(gtd_sf %>% group_by(!!sym(input$bar_var)) %>% count(), aes(x = reorder(!!sym(input$bar_var), -n), y = n)) +
      geom_col(fill = "red") +
      labs(title = paste0("Terrorist Attacks by ", input$bar_var),
           x = input$bar_var, y = "Number of Attacks")
  })

})


#displaying the shiny app  
shinyApp(ui, server)
               