library(arules)
library(reader)
library(ggplot2)
library(shiny)

ui <- fluidPage(
  titlePanel("Customer's Data analysis"),
  sidebarLayout(
    mainPanel(
      tabsetPanel(
        tabPanel("Data Input",
                 fluidRow(
                   fileInput("file", "Choose CSV File", accept = ".csv")
                 )
        ),
        tabPanel("Customers' behaviour's data analysis",
                 fluidRow(
                   column(6, plotOutput("pie")),
                   column(6, plotOutput("bar")),
                   column(6, plotOutput("scatter")),
                   column(6, plotOutput("box"))
                 )
        ),
        tabPanel("Customer's clustering",
                 sidebarLayout(
                   sidebarPanel(
                     numericInput("n", "Enter the number of clusters", value = 2, min = 2, max = 4)
                   ),
                   mainPanel(
                     fluidRow(
                       column(width = 6,
                              plotOutput("Kmeans")
                       ),
                       column(width = 6,
                              div(style = "max-height: 800px; margin-left: -300px;",
                                  tableOutput("Kmeans_table")
                              )
                       )
                     )
                   )
                 )
        ),
        tabPanel("Itemsets algorithm",
                 sidebarLayout(
                   sidebarPanel(
                     numericInput("support", "Support", value = 0.04, min = 0, max = 1, step = 0.001),
                     numericInput("confidence", "Confidence", value = 0.25, min = 0, max = 1, step = 0.001)
                   ),
                   mainPanel(
                     tableOutput("rules_output")
                   )
                 )
        )
      )
    ),
    sidebarPanel(
      # Any sidebar content that appears globally can go here
    )
  )
)

server <- function(input, output) {
  
  # Reactive expression to read the uploaded CSV file
  data <- reactive({
    req(input$file)  # Require file input
    validate(need(input$file$datapath, "Please upload a file"))  # Validate file input
    read.csv(input$file$datapath)
  })
  
  observeEvent(data(), {
    # Check if the uploaded CSV file is empty
    if (is.null(data())) {
      return()
    }
    
    # Calculate Cash and Credit Transactions
    CashTransactions <- sum(data()[data()$paymentType == "Cash", ]$total)
    CreditTransactions <- sum(data()[data()$paymentType == "Credit", ]$total)
    paymentmethods <- c(CreditTransactions, CashTransactions)
    Percentage <- paste0(round(100 * paymentmethods / sum(paymentmethods)), "%")
    
    # Render Pie Chart
    output$pie <- renderPlot({
      ggplot(NULL, aes(x = "", y = paymentmethods, fill = c("Credit", "Cash"))) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar(theta = "y") +
        labs(title = "Cash/Credit Totals", fill = NULL) +
        theme_void() +
        scale_fill_manual(values = c("darkgreen", "darkblue")) +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        geom_text(aes(label = Percentage), position = position_stack(vjust = 0.5))
    })
    
    # Render Bar Chart
    output$bar <- renderPlot({
      Cities <- unique(data()$city)
      Cityspending <- numeric(length(Cities))
      for (i in 1:length(Cities)) {
        Cityspending[i] <- sum(data()$total[data()$city == Cities[i]])
      }
      CitiesSpending <- data.frame(City = Cities, CityTotalSpending = Cityspending)
      CitiesSpending <- CitiesSpending[order(-CitiesSpending$CityTotalSpending), ]
      
      ggplot(CitiesSpending, aes(x = reorder(City, -CityTotalSpending), y = CityTotalSpending)) +
        geom_bar(stat = "identity", fill = "deepskyblue") +
        labs(title = "Each city's total spending", x = "City", y = "Total Spending") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
        scale_y_continuous(labels = scales::number_format())
    })
    
    # Render Scatter Plot
    output$scatter <- renderPlot({
      Ages <- unique(data()$age)
      Agespending <- numeric(length(Ages))
      for (i in 1:length(Ages)) {
        Agespending[i] <- sum(data()$total[data()$age == Ages[i]])
      }
      AgeSpending <- data.frame(Age = Ages, TotalAgeSpending = Agespending)
      AgeSpending <- AgeSpending[order(-AgeSpending$TotalAgeSpending), ]
      
      ggplot(AgeSpending, aes(x = Age, y = TotalAgeSpending)) +
        geom_point(color = "black") +
        labs(title = "Each age's total spendings", x = "Customers age", y = "Age's total spending")
    })
    
    # Render Box Plot
    output$box <- renderPlot({
      ggplot(data(), aes(x = "", y = total)) +
        geom_boxplot(color = "grey") +
        labs(title = "Distribution of total spending", x = "total spending")
    })
    
    # Clustering data
    observeEvent(input$n, {
      max_rnd <- max(data()$rnd)
      Total <- numeric(max_rnd)
      age <- numeric(max_rnd)
      customer <- character(max_rnd)
      
      for (i in 1:max_rnd) {
        Total[i] <- sum(data()$total[data()$rnd == i])
        age[i] <- mean(data()$age[data()$rnd == i])
        customer[i] <- unique(data()$customer[data()$rnd == i])
      }
      
      kmeanTable <- data.frame(customer, rnd = 1:max_rnd, age, Total)
      
      if (input$n %in% c(2, 3, 4)) {
        kmean <- kmeans(kmeanTable[, c("age", "Total")], centers = input$n)
        cluster <- kmean$cluster
        output$Kmeans <- renderPlot({
          # Your code to plot the Kmeans results
        })
        output$Kmeans_table <- renderTable({
          cbind(kmeanTable, Cluster = cluster)
        })
      } else {
        output$Kmeans <- renderPlot(NULL)
        output$Kmeans_table <- renderTable(NULL)
      }
    })
    
    # Apriori 
    observeEvent(c(input$confidence, input$support), {
      if (input$confidence <= 0 || input$confidence >= 1 || input$support <= 0 || input$support >= 1) {
        output$rules_output <- renderTable(NULL)
      }
      trans <- data()$items
      temp_file <- tempfile()
      writeLines(trans, temp_file)
      
      transactions <- read.transactions(temp_file, format = "basket", sep = ",")
      
      if (input$confidence > 0.001 && input$confidence < 1 &&
          input$support > 0.001 && input$support < 1) {
        
        rules <- apriori(transactions, 
                         parameter = list(support = input$support, 
                                          confidence = input$confidence, 
                                          minlen = 2))
        
        output$rules_output <- renderTable({
          inspect(rules)
        })
      } 
    })
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
