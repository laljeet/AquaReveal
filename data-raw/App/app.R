library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(ggplot2)
library(shinycssloaders) # for spinner
library(shinythemes)

# Load your spatial data
merged_data_sf <- readRDS("./Data/merged_data_sf.rds")
merged_data_sf$PopUpContent <- paste(merged_data_sf$County, round(merged_data_sf$AcreageUnderTh, 1))
merged_data_sf <- st_transform(merged_data_sf, crs = 4326)

ui <- fluidPage(
  theme = shinytheme("united"),
  titlePanel("Acreage under the reporting threshold (Less than 25 acres per operation)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("yearSelect", "Select Year:",
                  choices = c("2022", "2017", "2012", "2007", "2002"), selected = "2022"),
      selectInput("countySelect", "Select County:",
                  choices = unique(merged_data_sf$County)) # Dynamically populate choices based on available counties
    ),
    mainPanel(
      h4("Select the YEAR for the acreage under reporting threshold"), # Title for the map
      leafletOutput("map") %>% withSpinner(),
      h4("Select the COUNTY NAME to see County level results over the years"), # Title for the line plot
      textOutput("selectedCounty"), # Display selected county name
      plotOutput("linePlot") %>% withSpinner()
    )
  )
)

server <- function(input, output) {
  # Map rendering
  output$map <- renderLeaflet({
    req(input$yearSelect) # Ensure a year has been selected

    data_for_year <- merged_data_sf %>%
      filter(Year == as.numeric(input$yearSelect))

    leaflet(data_for_year) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~colorNumeric(palette = "viridis", domain = merged_data_sf$AcreageUnderTh)(AcreageUnderTh),
        weight = 1,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        popup = ~PopUpContent
      ) %>%
      addLegend(position = "bottomright",
                title = "Acreage Under Threshold",
                pal = colorNumeric(palette = "viridis", domain = merged_data_sf$AcreageUnderTh),
                values = ~AcreageUnderTh)
  })

  # Display the selected county name
  output$selectedCounty <- renderText({
    paste("County Selected:", input$countySelect)
  })

  # Line plot rendering
  output$linePlot <- renderPlot({
    req(input$countySelect) # Ensure a county has been selected

    # Filter data for the selected county
    county_data <- merged_data_sf %>%
      filter(County == input$countySelect) %>%
      arrange(Year)

    # Calculate averages for all counties per year for comparison
    averages <- merged_data_sf %>%
      group_by(Year) %>%
      summarise(`State Average` = mean(AcreageUnderTh, na.rm = TRUE))

    # Generate the line plot
    ggplot() +
      geom_line(data = county_data, aes(x = Year, y = AcreageUnderTh, group = 1, color = "Selected County"), linewidth = 1) +
      geom_point(data = county_data, aes(x = Year, y = AcreageUnderTh, color = "Selected County"), size = 2) +
      geom_text(data = county_data, aes(x = Year, y = AcreageUnderTh, label = round(AcreageUnderTh, 0)), vjust = -0.5, color = "#2c7fb8") + # Add numeric values for selected county
      geom_line(data = averages, aes(x = Year, y = `State Average`, group = 1, color = "State Average"), linewidth = 1) +
      geom_point(data = averages, aes(x = Year, y = `State Average`, color = "State Average"), size = 2) +
      geom_text(data = averages, aes(x = Year, y = `State Average`, label = round(`State Average`, 0)), vjust = 1.5, color = "#fec44f") + # Add numeric values for averages
      labs(x = "Year", y = "Acres Under Threshold", color = "Legend") +
      theme_minimal() +
      scale_color_manual(values = c("Selected County" = "#2c7fb8", "State Average" = "#fec44f")) +
      scale_x_continuous(breaks = seq(2002, 2022, by = 5)) # Set x-axis breaks
  })
}

# Run the application
shinyApp(ui, server)

