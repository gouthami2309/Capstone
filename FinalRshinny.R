library(shiny)
library(ggplot2)
library(readr)
library(caTools)

Adidas_US_Sales_Datasets <- read_csv("C:/Users/GOUTHAMI VENKATEH/Desktop/Adidas US Sales Datasets.csv")
Adidas_US_Sales_Datasets$Total_Sales <- as.numeric(gsub("[^0-9.]", "", Adidas_US_Sales_Datasets$Total_Sales))
top_products <- aggregate(Total_Sales ~ Product, data = Adidas_US_Sales_Datasets, FUN = sum)
top_retailers <- aggregate(Total_Sales ~ Retailer, data = Adidas_US_Sales_Datasets, FUN = sum)
percentage <- round(100 * top_retailers$Total_Sales / sum(top_retailers$Total_Sales), 1)
labels <- paste(top_retailers$Retailer, "(", percentage, "%)", sep = "")

# Defining UI
ui <- fluidPage(
  titlePanel("Adidas US Sales Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plot_type", "Choose Plot Type:",
                  choices = c("Pie Chart", "Bar Plot"))
    ),
    mainPanel(
      plotOutput("selected_plot")
    )
  ),
  
  tabsetPanel(
    tabPanel("Scatter Plot - Variable",
             fluidPage(
               sidebarPanel(
                 selectInput("scatter_var", "Choose Variable:",
                             choices = c("Price per Unit", "Units Sold", "Operating Profit"))
               ),
               mainPanel(
                 plotOutput("scatter_plot_variable")
               )
             )
    ),
    
    )
  )

# Defining server
server <- function(input, output) {
  
  Adidas_US_Sales_Datasets$Price_per_Unit <- as.numeric(gsub("[^0-9.]", "", Adidas_US_Sales_Datasets$Price_per_Unit))
  Adidas_US_Sales_Datasets$Total_Sales <- as.numeric(gsub("[^0-9.]", "", Adidas_US_Sales_Datasets$Total_Sales))
  Adidas_US_Sales_Datasets$Operating_Profit <- as.numeric(gsub("[^0-9.]", "", Adidas_US_Sales_Datasets$Operating_Profit))
  Adidas_US_Sales_Datasets$Operating_Margin <- as.numeric(gsub("[^0-9.]", "", Adidas_US_Sales_Datasets$Operating_Margin))
  
  # Saving the modified dataset
  write.csv(Adidas_US_Sales_Datasets, "Adidas_US_Sales_Datasets_modified.csv", row.names = FALSE)
  numeric_columns <- c("Price_per_Unit", "Units_Sold", "Total_Sales", "Operating_Profit","Operating_Margin")
  str(Adidas_US_Sales_Datasets)
  scaled_data <- as.data.frame(scale(Adidas_US_Sales_Datasets[, numeric_columns]))
  Adidas_US_Sales_Datasets[, numeric_columns] <- scaled_data
#-----------------------------------
  # Min-Max Scaling function
  min_max_scaling <- function(x) {
    (x - min(x)) / (max(x) - min(x))
  }
  
  numeric_columns <- c("Price_per_Unit", "Units_Sold", "Total_Sales", "Operating_Profit","Operating_Margin")
  Adidas_US_Sales_Datasets[numeric_columns] <- apply(Adidas_US_Sales_Datasets[numeric_columns], 2, min_max_scaling)
  standardization <- function(x) {
    (x - mean(x)) / sd(x)
  }
  Adidas_US_Sales_Datasets[numeric_columns] <- apply(Adidas_US_Sales_Datasets[numeric_columns], 2, standardization)
  any_missing <- any(is.na(Adidas_US_Sales_Datasets))
  output$selected_plot <- renderPlot({
    if (input$plot_type == "Pie Chart") {
      pie(top_retailers$Total_Sales, 
          labels = labels, 
          main = "Total sales per Retailer", 
          col = rainbow(length(top_retailers$Retailer)), 
          border = "white", 
          clockwise = TRUE,
          cex = 0.8, 
          cex.main = 1, 
          cex.lab = 0.8, 
          cex.axis = 0.8, 
          font.main = 2, 
          font.lab = 2, 
          font.axis = 2, 
          label.dist = 1.1, 
          legend.text = top_retailers$Retailer, 
          args.legend = list(x = "topright", cex = 0.8), 
          shadow = TRUE)
    } else if (input$plot_type == "Bar Plot") {
      ggplot(top_products, aes(x = Product, y = Total_Sales)) +
        geom_bar(stat = "identity", fill = "skyblue") +
        labs(title = "Total Sales per Product", x = "Product", y = "Total Sales") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  output$scatter_plot_variable <- renderPlot({
    if (input$scatter_var == "Price per Unit") {
      ggplot(Adidas_US_Sales_Datasets, aes(x = Price_per_Unit, y = Total_Sales)) +
        geom_point() +
        labs(x = "Price per Unit", y = "Total Sales", 
             title = "Scatter Plot of Total Sales vs. Price per Unit")
    } else if (input$scatter_var == "Units Sold") {
      ggplot(Adidas_US_Sales_Datasets, aes(x = Units_Sold, y = Total_Sales)) +
        geom_point() +
        labs(x = "Units Sold", y = "Total Sales", 
             title = "Scatter Plot of Total Sales vs. Units Sold")
    } else if (input$scatter_var == "Operating Profit") {
      ggplot(Adidas_US_Sales_Datasets, aes(x = Operating_Profit, y = Total_Sales)) +
        geom_point() +
        labs(x = "Operating Profit", y = "Total Sales", 
             title = "Scatter Plot of Total Sales vs. Operating Profit")
    }
  })
  }

shinyApp(ui = ui, server = server)


