#
# Title: World Bank SPI Data Relating to UN Sustainable Development Goal 4: Quality Education
#
# Description: World Bank SPI app with interactive map showing SDG 4 data availability change over time between 2004 and 2019 and exploratory area for the dataset
#
# Author: Cassandra Sperow (K.C. Sperow)
#
# Date: Summer 2022 
#


# libraries 
library(shiny)
library(leaflet)
library(readr)
library(ggplot2)
library(sf)
library(viridis)

# data imports
sdsn_spi <- read_csv("./data/sdsn_spi.csv")
num_data <- sdsn_spi %>% dplyr::select(WB_Income_Level, Population, SDSN_Goal_4_Score:WB_SPI_Index)

# map data 
read_rds("./data/sdg_map_layer_nov26.rds") -> sdg_map_layer_nov26
world_boundaries <- read_sf("./data/World_Countries_(Generalized)")

# text data
disclaimer <- read_lines("./text/disclaimer.txt")
map_page_text <- read_lines("./text/map_intro_text.txt")
intro_page <- read_lines("./text/intro_page.txt")
cit1 <- read_lines("./text/cit1.txt")
cit2 <- read_lines("./text/cit2.txt")


#################################################################
#####################  U I   ##################################
ui <- fluidPage(theme = bslib::bs_theme(version=4, bootswatch = "journal"),
  
    # Application title
    titlePanel("World Bank Statistical Progress Indicator  & Sustainable Development Solutions Network Data Exploration"),

    tabsetPanel(
      type = "pills",
      tabPanel(title = "Home",
               p(), 
               htmlOutput("intro_page_text"),
               p(), 
               HTML("The goal of this app is to discover relationships between a country's statistical performance and its ability to achieve Goal 4: Quality Education."),
               p(),
               a(href = "https://datacatalog.worldbank.org/search/dataset/0037996/Statistical-Performance-Indicators", "World Bank Data Catalogue"),
               p(),
               htmlOutput("citation1"),
               p(), 
               a(href = "https://www.sdgindex.org/reports/sustainable-development-report-2019/", "Sustainable Development Solutions Network 2019 Data"),
               p(), 
               htmlOutput("citation2"),
               p(),
               htmlOutput("disclaimer")
               
      ),
      
      tabPanel("Map",
               h3("Gradient Map of the Change Over Time of Data Available for Sustainable Development Goal 4: Quality Education"),
               p(),
               h6("Once the map loads, use the '+' and '-' buttons to zoom in or out."),
               p(), 
               htmlOutput("map_page_text"),
               leafletOutput("spi_sdg4_map"),
               a(href = "https://leafletjs.com", "This map was created using leaflet.")
               ),
      
      tabPanel("Data Visualization",
               p(), 
               HTML("When choosing a variable to plot, variables that begin with 'WB' are World Bank data in 2019 and 'SDSN' indicates data from the Sustainable Development Solutions Network in 2019. There are 218 countries in this dataset."),
               p(), 
               sidebarLayout(
                 sidebarPanel(
                   varSelectInput("var_x", "Variable", data = num_data, selected = "WB_SPI_Index"),
                   sliderInput("hist_bins",
                               "Number of bins:",
                               min = 1,
                               max = 50,
                               value = 30), 
                 #  radioButtons("plot_type", "Choose plot type for numeric variables: ", c( "Density Plot" = "density", 
                                                                                  #          "Histogram" = "hist",
                                                                                  #          "Frequency Polygon" = "freqpoly")),
                   
                 ),
                 ### Main Data Viz
                 mainPanel(
                   plotOutput("x_plot"),
                   
                   )
               ),
               ), # end tabPanel
      tabPanel("Spreadsheet", 
               mainPanel(
                 dataTableOutput("table")
               ))
      ) # end TabsetPanel
    
  
) # end fluidpage()

#################################################################
#####################  Server  ##################################

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # # # # # # #    H O M E   # # # # # # #
  output$disclaimer <- renderText(HTML(disclaimer))
  
  output$intro_page_text <- renderText(HTML(intro_page))
  
  output$citation1 <- renderText(cit1)
  
  output$citation2 <- renderText(cit2)

  
  # # # # # # #   M A P    # # # # # # #
  
  output$map_page_text <-  renderText(map_page_text)
  
  output$spi_sdg4_map <- renderLeaflet({
  
  pal2 <- colorNumeric(
    palette = "inferno",
    domain = sdg_map_layer_nov26$growth_rate
  )
  
  leaflet() %>% 
    addTiles() %>% # base layer
    setView(0,0, zoom = 1) %>% # default center world view
    addPolygons(data = world_boundaries, # outline of the polygon countries for gradient
                color = 'white', 
                weight = 1.5,
                opacity = 1,
                fillColor = ~pal2(sdg_map_layer_nov26$growth_rate), # filling gradient
                fillOpacity = .8,
                highlightOptions = highlightOptions(color = "#FFF1BE", # yellow highlight
                                                    weight = 5)) %>% 
    addCircleMarkers(data = sdg_map_layer_nov26, # circle markers for each country
                     popup = ~popup_label, # popup label within sf data object with html
                     stroke=F,
                     radius = 4,
                     fillColor = "gray",
                     fillOpacity = 1) %>% 
    addLegend(pal = pal2,  # legend
              values = sdg_map_layer_nov26$growth_rate,
              title = "Growth Rate")
  })
  
  # # # # # # #    E D A  # # # # # # #
  
  ###  reactive data for the plot  ###
  df <- reactive({
     sdsn_spi 
      # filter(!is.na(!!input$var_x))
    #  browser()
    })
  
  # BEGINNING OF RENDER PLOT AREA
  output$x_plot <- renderPlot({
    
    hist_plot <- ggplot(data = df(), aes(x = !!input$var_x, 
                                         color = WB_Income_Level, 
                                         fill = WB_Income_Level)) +
      geom_histogram(bins = input$hist_bins) +
      theme_bw() +
      ggtitle(paste0("Distribution of ", input$var_x, " in 2019 for Countries with SPI Data"))
    
    hist_plot 
    
   # browser()
    
   
   
  })
  
 
  
  # # # # # # # # Table Tab # # # # # # # # # # # # # # # # # # # #
  
  
  output$table <- renderDataTable(
    sdsn_spi
  )
   
} # end of Server

# Run the application 
shinyApp(ui = ui, server = server)
