library(shiny)
library(shinydashboard)
library(networkD3)
library(ggplot2)
library(geojsonio)
library(leaflet)

marriage_by_birthplace = read.csv("marriage_by_birthplace.csv")
marriage_by_birthplace = subset(marriage_by_birthplace, source != 'Australia')

divorce_by_length_type = read.csv("divorce_by_length_and_applicant_type.csv")

divorce_by_number_age = read.csv("divorce_by_number_and_age.csv")

marriage_by_state = read.csv("marriage_by_state.csv", fileEncoding = "UTF-16LE")


ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Marriage and Divorce in Australia"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Marriage", tabName = "marriage", icon = icon("th")),
      menuItem("Divorce", tabName = "divorce", icon = icon("th")),
      sliderInput(inputId = "year",
                  label = "Year",
                  min = 2007,
                  max = 2017,
                  value = 2017)
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "marriage",
              fluidRow(
                box(title = "Marriage by birthplace", status = "primary", solidHeader = T, 
                    width = 6, sankeyNetworkOutput("marriage_sankey")),
                box(title = "Marriage by state", status = "warning", solidHeader = T,
                    width = 6, leafletOutput("marriage_state")
              ),
              fluidRow(
                box(selectInput(inputId = "gender",
                                label = "Gender",
                                choices = c("total", "female", "male")), width = 2)
                )
              )),
      tabItem(tabName = "divorce",
              fluidRow(
                box(title = "Divorce by length and applicant type", status = "warning", solidHeader = T,
                    width = 12, plotOutput("divorce_length_type_line"))
              ),
              fluidRow(
                box(title = "DIvorce by children number and the age of youngest child", status = "danger", solidHeader = T,
                    width = 10, plotOutput("divorce_number_age")),
                box(selectInput(inputId = "kids_number",
                                label = "Number of children",
                                choices = c(1,2,3,4,"5 and over")),
                    width = 2)
              ))
    )
    
  )
)

server <- function(input, output) {
  
  selected_year = reactive({
    input$year
  })
  
  selected_gender = reactive({
    input$gender
  })
  
  selected_kid_number = reactive({
    input$kids_number
  })
  
  output$marriage_sankey <- renderSankeyNetwork({
    selected_data = subset(marriage_by_birthplace, year == selected_year() & type == selected_gender())
    
    temp_nodes = data.frame("name"=c(unique(as.character(selected_data$source)), 'Australia'))
    
    temp_links2 = data.frame("source" = 0:13, "target"=rep(14,14), "value"=selected_data$amount)
    
    sankeyNetwork(Links = temp_links2, Nodes = temp_nodes,
                  Source = "source", Target = "target",
                  Value = "value", NodeID = "name",
                  fontSize= 12, nodeWidth = 30)
  })
  
  output$divorce_length_type_line <- renderPlot({
    selected_data = subset(divorce_by_length_type, year == selected_year())
    
    selected_data$length <- factor(selected_data$length, levels = c(1:29, ">29"))
    
    ggplot(selected_data, aes(length, amount, group = type, color = type)) +
      geom_line(size = 2) +
      coord_cartesian(ylim = c(0,1800))
  })
  
  output$divorce_number_age <- renderPlot({
    selected_data = subset(divorce_by_number_age, year == selected_year() & number == selected_kid_number())
    
    selected_data$age <- factor(selected_data$age, levels = c(0:17))
    
    ggplot(selected_data, aes(age, amount, group = 1)) +
      geom_line(size = 2) +
      coord_cartesian(ylim = c(0,1100))
  })
  
  output$marriage_state <- renderLeaflet({
    states <- geojsonio::geojson_read("states.geojson",
                                      what = "sp")
    
    pal <- colorNumeric("Greens", NULL)
    selected_data = subset(marriage_by_state, year == selected_year())
    leaflet(states) %>%
      addTiles() %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                  fillColor = ~pal(selected_data$amount)) %>%
      addLegend(pal = pal, values = ~selected_data$amount, opacity = 1.0,
                labFormat = labelFormat(transform = function(x) x)) %>%
      setView(lat = -25.2744, lng = 133.7751, zoom = 4)
  })
  
}

shinyApp(ui, server)