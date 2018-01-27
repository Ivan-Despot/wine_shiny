# Ivan Despot
# Jan 2018
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com/
#

# Load dependencies
library(shiny)
library(shinyWidgets)
library(gapminder)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(forcats)
library(scales)
library(plotly)


# Load in the dataset
dat <- read.csv(file = "winemag.csv", header = T)
dat <- na.omit(dat)

  # Define UI for application that draws a histogram
ui <- dashboardPage(skin = "red",
  dashboardHeader(title = "The Wine Aficianado"),
  
  # Create side dash board interface
  dashboardSidebar(
    sidebarMenu(
  
      sliderInput("priceInput",
                  "Price Range",
                  min = min(dat$price),
                  max = max(dat$price),
                  value = c(min, max),
                  step = 20,
                  round = T,
                  sep = "",
                  ticks = F),
      
      selectInput("countryInput", "Country Selector",
                  choices = sort(unique(as.character(dat$country))),
                  selected = "Canada",
                  multiple = T),
      
      pickerInput("pickerInput2", "Variety Selector",
                  choices = sort(unique(as.character(dat$variety))),
                  options = list("actions-box" = T,
                                 size = 10,
                                 "selected-text-format" = 'count > 5'),
                  multiple = T,
                  selected = "Pinot Noir"),
      
      # Dashboard menu item names
      menuItem("Rankings", tabName = "rankings", icon = icon("thumbs-up")),
      
      menuItem("Exploration", tabName = "exploration", icon = icon("search")),
      
      menuItem("Resources", tabName = "resources", icon = icon("link"))
      
      
      
    )
  ), 
  # Features under each unique tab name
  dashboardBody(tabItems(
    
    tabItem(tabName = "rankings", h2("The Best Wine"),
            infoBoxOutput("bestval"),
            infoBoxOutput("bestvalinfo"),
            infoBoxOutput("worstval"),
           
            
            dataTableOutput("df_table")),
            
            
    tabItem(tabName = "exploration", h2("Explore the World of Wine!"),
            
            
            dropdownButton(
              tags$h3("Change the aesthetics"),
              knobInput("alphaInput",
                        "Alpha (Transparency)",
                        min = 0,
                        max = 1,
                        value = 0.2,
                        step = 0.05,
                        cursor = T,
                        width = 150,
                        height = 150),
              
              numericInput("sizeInput", "Point Size",
                           min = 0.5,
                           max = 10,
                           value = 1,
                           step = 0.25
              ),
              
              # Radio buttons to select variables of interest
              
              
              circle = T,
              status = "danger",
              icon = icon("gear"),
              width = "300px",
              tooltip = tooltipOptions(title = "Click to change the graph!")
            ),
            
            plotOutput("gapplot"),
            textOutput("selected_var"),
            fluidRow(valueBoxOutput("winecount"),
                     valueBoxOutput("province"),
                     valueBoxOutput("score")),
            
            dropdownButton(
              tags$h3("Change the aesthetics"),
              knobInput("alphaInput2",
                        "Alpha (Transparency)",
                        min = 0,
                        max = 1,
                        value = 1,
                        step = 0.05,
                        cursor = T,
                        width = 150,
                        height = 150),
              
              numericInput("sizeInput2", "Point Size",
                           min = 0.5,
                           max = 10,
                           value = 2,
                           step = 0.25
              ),
              
              radioButtons("var_plot", "X-axis Variable:",
                           choices = c("Score" = "points", "Price" = "price"),
                           selected = "points"),
              
              radioButtons("var_plot_y", "Y-axis Variable:",
                           choices = c("Score" = "points", "Price" = "price"),
                           selected = "price"),
              
              circle = T,
              status = "danger",
              icon = icon("gear"),
              width = "300px",
              tooltip = tooltipOptions(title = "Click to change the graph!")
            ),
            
            plotOutput("scatplot", hover = hoverOpts(id = "plot_hover")),
            verbatimTextOutput("hover_info")
            
            
    ),
    
    tabItem(tabName = "resources", h2("Resources"),
            uiOutput("kaggle"),
            uiOutput("we")
            )
    ),
    
    
    
    fluidPage(
   
  
     
   )
)
)
# Define server logic required to draw a panels
server <- function(input, output) {
  
  output$selected_var <- renderText({
    paste0(input$countryInput, ",")
    
  })

  
  output$gapplot <- renderPlot({
    
    # Create axis labels depending on user input     
    if(input$var_plot == "points")x_axis_label <- "Score"
    if(input$var_plot == "price")x_axis_label <- "Price (USD)"

    # Generate plot         
    x <- input$countryInput
    
    ggplot(na.omit(dat) %>% 
             select(country, title, points, price, region_1, region_2, province, winery, designation, variety) %>% 
             filter(country %in% input$countryInput,
                    variety %in% input$pickerInput2,
                    price >= min(input$priceInput) & price <= max(input$priceInput))) +
      geom_density(aes_string(x = input$var_plot, colour = "country", fill = "country"), alpha = input$alphaInput, size = input$sizeInput) + 
      
      labs(title = "Distribution of Price and Score - Density Plot",
           x = x_axis_label,
           y = "Density",
           fill = "Country",
           colour = "Country") +
      theme_minimal()
  })
  #generate scatter plot
  
  output$scatplot <- renderPlot({
    
    # Create axis labels depending on user input     
    if(input$var_plot == "points")x_axis_label <- "Score"
    if(input$var_plot == "price")x_axis_label <- "Price (USD)"
    
    if(input$var_plot_y == "points")y_axis_label <- "Score"
    if(input$var_plot_y == "price")y_axis_label <- "Price (USD)"
    
    # Generate plot         
    x <- input$countryInput
    
    ggplot(na.omit(dat) %>% 
             select(country, title, points, price, region_1, region_2, province, winery, designation, variety) %>% 
             filter(country %in% input$countryInput,
                    variety %in% input$pickerInput2,
                    price >= min(input$priceInput) & price <= max(input$priceInput))) +
      geom_point(aes_string(x = input$var_plot, y = input$var_plot_y, colour = "country", fill = "country"), alpha = input$alphaInput2, size = input$sizeInput2) + 
      
      labs(title = "Spread of Price and Score - Scatterplot",
           x = x_axis_label,
           y = y_axis_label,
           fill = "Country",
           colour = "Country") +
      theme_minimal()
  })
  
  output$hover_info <- renderPrint({
    if(!is.null(input$plot_hover))
      paste0(input$plot_hover$description)
  })
  
  # Create reactive table
  react_country <- reactive({
    dat %>% 
      select(country, title, points, price, province, winery, designation, variety) %>% 
      filter(country %in% input$countryInput,
             variety %in% input$pickerInput2,
             price >= min(input$priceInput) & price <= max(input$priceInput)) %>% 
      mutate(best_val = points/price) %>% 
      arrange(desc(best_val))
  })
  
  output$df_table <- renderDataTable({
    react_country()
    
  })
  #create value boxes
  output$winecount <- renderValueBox({
    valueBox("Number of Unique Wines:", nrow(react_country()),
    icon = icon("spinner"),
    color = "red",
    width = )
             
  })
  
  
  output$province <- renderValueBox({df <- react_country()

    valueBox("Average Price of Wine:", round(mean(df$price), 2),
             icon = icon("dollar"),
             color = "red",
             width = 3) }
  )
  
  output$score <- renderValueBox({ df <- react_country()
    valueBox("Average Wine Rating (/100):", round(mean(df$points), 2),
             icon = icon("heart"),
             color = "red",
             width = 3)
    
  })
  
  output$bestval <- renderInfoBox({ df <- react_country()
  infoBox(title  = h2("Best Value:"), value = round(max(df$best_val), 2), subtitle = df$title[which(df$best_val == max(df$best_val))],
           icon = icon("heart"),
           color = "red",
           width = 3)
  
  })
  
  output$bestvalinfo <- renderInfoBox({ df <- react_country()
  infoBox(title  = "The best and worst value for your buck!", subtitle = "These are the ratios between Score and Price.",
          icon = icon("pie-chart"),
          color = "red",
          width = 3)
  
  })
  
  
  output$worstval <- renderInfoBox({ df <- react_country()
  infoBox(title  = h2("Worst Value:"), value = round(min(df$best_val),2), subtitle = df$title[which(df$best_val == min(df$best_val))],
          icon = icon("thumbs-down"),
          color = "red",
          width = 3)
  
  })
  
  #create URL for resource page
  
  url1 <- a("Kaggle Data Set", href = "https://www.kaggle.com/zynicide/wine-reviews/data")
  output$kaggle <- renderUI({
    tagList(url1)
  })
  
  url2 <- a("WineEnthusiast", href = "http://www.winemag.com/?s=&drink_type=wine")
  output$we <- renderUI({
    tagList(url2)
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

