library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(shinythemes)

# Load dataset
data <- read.csv("superstore_data.csv")

# Data preparation
data$Dt_Customer <- as.Date(data$Dt_Customer, "%m/%d/%Y")
data$Year <- year(data$Dt_Customer)
data$Age <- 2025 - data$Year_Birth
data$Total_Spending <- rowSums(data[, c("MntWines", "MntFruits", "MntMeatProducts",
                                        "MntFishProducts", "MntSweetProducts", "MntGoldProds")])

# UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Superstore Customer Purchase Analysis"),
  
  tabsetPanel(
    tabPanel("1. Year-wise Analysis",
             br(),
             fluidRow(
               column(6, plotOutput("enrollment_plot")),
               column(6, plotOutput("year_spend_plot"))
             ),
             hr(),
             fluidRow(
               column(12, plotOutput("marital_spend_plot"))
             )
    ),
    
    tabPanel("2. Age-based Purchase Behavior",
             br(),
             sidebarLayout(
               sidebarPanel(
                 sliderInput("age_range", "Select Age Range:", 
                             min = min(data$Age), max = max(data$Age), value = c(25, 60), step = 1)
               ),
               mainPanel(
                 plotOutput("age_purchase_plot")
               )
             )
    ),
    
    tabPanel("3. Income vs Spending",
             br(),
             sidebarLayout(
               sidebarPanel(
                 selectInput("edu_filter", "Select Education Level:", 
                             choices = unique(data$Education),
                             selected = unique(data$Education),
                             multiple = TRUE),
                 selectInput("marital_filter", "Select Marital Status:", 
                             choices = unique(data$Marital_Status),
                             selected = unique(data$Marital_Status),
                             multiple = TRUE)
               ),
               mainPanel(
                 plotOutput("income_vs_spend")
               )
             )
    ),
    
    tabPanel("4. Best-selling Category",
             br(),
             sidebarLayout(
               sidebarPanel(
                 selectInput("filter_year", "Select Year:",
                             choices = c("All", sort(unique(data$Year))),
                             selected = "All"),
                 selectInput("filter_marital", "Select Marital Status:",
                             choices = c("All", unique(data$Marital_Status)),
                             selected = "All")
               ),
               mainPanel(
                 plotOutput("categoryPlot")
               )
             )
    ),
    
    tabPanel("5. In-store vs Online",
             br(),
             fluidRow(
               column(12, plotOutput("purchase_channel_plot"))
             ),
             br(),
             fluidRow(
               column(4, offset = 4, align = "center",
                      downloadButton("downloadPlot", "Download Plot", class = "btn-primary"))
             )
    )
  )
)

# Server
server <- function(input, output) {
  
  # 1. Year-wise Plots
  output$enrollment_plot <- renderPlot({
    enroll_data <- data %>%
      group_by(Year) %>%
      summarise(Count = n())
    
    ggplot(enroll_data, aes(x = Year, y = Count)) +
      geom_col(fill = "skyblue") +
      labs(title = "Customer Enrollment by Year", x = "Year", y = "Number of Customers")
  })
  
  output$year_spend_plot <- renderPlot({
    spend_data <- data %>%
      group_by(Year) %>%
      summarise(TotalSpending = sum(Total_Spending))
    
    ggplot(spend_data, aes(x = Year, y = TotalSpending/1000)) +
      geom_col(fill = "yellow") +
      labs(title = "Total Spending by Year", x = "Year", y = "Total Spending")
  })
  
  output$marital_spend_plot <- renderPlot({
    marital_spend <- data %>%
      group_by(Marital_Status) %>%
      summarise(TotalSpending = sum(Total_Spending))
    
    ggplot(marital_spend, aes(x = Marital_Status, y = TotalSpending/1000, fill = Marital_Status)) +
      geom_col() +
      labs(title = "Total Spending by Marital Status", x = "Marital Status", y = "Spending") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # 2. Age-based Purchase Behavior
  output$age_purchase_plot <- renderPlot({
    filtered <- data %>%
      filter(Age >= input$age_range[1], Age <= input$age_range[2]) %>%
      mutate(TotalPurchases = NumWebPurchases + NumCatalogPurchases + NumStorePurchases)
    
    ggplot(filtered, aes(x = Age, y = TotalPurchases)) +
      geom_point(color = "purple", alpha = 0.6) +
      geom_smooth(method = "loess") +
      labs(title = "Purchases by Age", x = "Age", y = "Total Purchases")
  })
  
  # 3. Income vs Spending
  output$income_vs_spend <- renderPlot({
    filtered <- data %>%
      filter(Education %in% input$edu_filter, Marital_Status %in% input$marital_filter)
    
    ggplot(filtered, aes(x = Income/1000, y = Total_Spending)) +
      geom_point(color = "violet", alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Income vs. Total Spending", x = "Income in Thousands", y = "Total Spending")
  })
  
  # 4. Best-selling Product Category
  output$categoryPlot <- renderPlot({
    filtered_data <- data
    
    if (input$filter_year != "All") {
      filtered_data <- filtered_data %>% filter(Year == input$filter_year)
    }
    if (input$filter_marital != "All") {
      filtered_data <- filtered_data %>% filter(Marital_Status == input$filter_marital)
    }
    
    category_data <- filtered_data %>%
      summarise(
        Wine = sum(MntWines, na.rm = TRUE),
        Meat = sum(MntMeatProducts, na.rm = TRUE),
        Fish = sum(MntFishProducts, na.rm = TRUE),
        Fruits = sum(MntFruits, na.rm = TRUE),
        Sweets = sum(MntSweetProducts, na.rm = TRUE),
        Gold = sum(MntGoldProds, na.rm = TRUE)
      ) %>%
      pivot_longer(cols = everything(), names_to = "Category", values_to = "Sales")
    
    ggplot(category_data, aes(x = Category, y = Sales/1000, fill = Category)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Best-selling Product Categories", x = "Category", y = "Total Sales in thousands")
  })
  
  # 5. In-store vs Online
  output$purchase_channel_plot <- renderPlot({
    channel_data <- data %>%
      summarise(
        InStore_Purchases = sum(NumStorePurchases),
        Online_Purchases = sum(NumWebPurchases + NumCatalogPurchases),
        InStore_Spending = sum(Total_Spending * NumStorePurchases /
                                 (NumWebPurchases + NumCatalogPurchases + NumStorePurchases)),
        Online_Spending = sum(Total_Spending * (NumWebPurchases + NumCatalogPurchases) /
                                (NumWebPurchases + NumCatalogPurchases + NumStorePurchases))
      )
    
    channel_long <- data.frame(
      Channel = c("In-Store", "Online"),
      Purchases = c(channel_data$InStore_Purchases, channel_data$Online_Purchases),
      Spending = c(channel_data$InStore_Spending, channel_data$Online_Spending)
    )
    
    ggplot(channel_long, aes(x = Channel)) +
      geom_col(aes(y = Purchases, fill = "Purchases"), position = "dodge") +
      geom_col(aes(y = Spending, fill = "Spending"), position = "dodge") +
      labs(title = "In-store vs. Online Purchases and Spending", y = "Count/Amount") +
      scale_fill_manual(values = c("Purchases" = "pink", "Spending" = "grey"))
  })
  
  # Download Handler for Channel Plot
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("channel_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file)
      channel_data <- data %>%
        summarise(
          InStore_Purchases = sum(NumStorePurchases),
          Online_Purchases = sum(NumWebPurchases + NumCatalogPurchases)
        )
      barplot(height = as.numeric(channel_data), names.arg = c("In-Store", "Online"),
              col = c("red", "green"), main = "In-store vs Online Purchases")
      dev.off()
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
