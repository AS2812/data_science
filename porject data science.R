library(dplyr)
library(arules)
library(ggplot2)
library(shiny)

ui <- navbarPage(
  title = "Data Analysis App",
  tabPanel("Data Analysis Dashboard",
           sidebarLayout(
             sidebarPanel(
               fileInput("file", "Choose CSV File",
                         accept = c(
                           "text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")
               ),
               selectInput("plot_choice", "Select a Plot:",
                           c("Payment Type Pie Chart", "Age Group Bar Chart",
                             "Distribution of Total Spending", "Total Spending by City", "All Visualizations"))
             ),
             mainPanel(
               uiOutput("dynamic_ui")
             )
           )),
  tabPanel("K-Means Clustering",
           sidebarLayout(
             sidebarPanel(
               numericInput("num_clusters", "Number of Clusters", value = 3, min = 2, max = 4),
               actionButton("cluster", "Run K-Means")
             ),
             mainPanel(
               dataTableOutput("cluster_table")
             )
           )),
  tabPanel("Apriori Association Rule Mining",
           sidebarLayout(
             sidebarPanel(
               sliderInput("support",
                           "Support:",
                           min = 0.001, max = 1, value = 0.01, step = 0.001),
               sliderInput("confidence",
                           "Confidence:",
                           min = 0.001, max = 1, value = 0.01, step = 0.001),
               actionButton("run_analysis", "Run Analysis")
             ),
             mainPanel(
               tableOutput("rules_table")
             )
           ))
)

server <- function(input, output) {
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    df <- unique(df)  # Remove duplicates
    return(df)
  })
  
  output$dynamic_ui <- renderUI({
    if (input$plot_choice == "All Visualizations") {
      fluidRow(
        plotOutput("payment_type_plot"),
        plotOutput("age_group_plot"),
        plotOutput("total_spending_plot"),
        plotOutput("city_spending_plot")
      )
    } else {
      plotOutput("selected_plot")
    }
  })
  output$selected_plot <- renderPlot({
    plot_choice <- input$plot_choice  # Store input$plot_choice in a variable
    
    if (plot_choice == "Payment Type Pie Chart") {
      payment_type_table <- table(data()$paymentType)
      percentage <- paste0(round(100 * payment_type_table / sum(payment_type_table)), "%")
      
      pie(payment_type_table, labels = percentage,
          main = "Pie Chart of Payment Type",
          col = c("steelblue1", "tomato"))
      legend("bottomright", legend = c("Cash", "Credit"),
             fill = c("steelblue1", "tomato"))
    } else if (plot_choice == "Age Group Bar Chart") {
      # Convert age groups to factors to maintain order
      age_groups <- factor(cut(data()$age, breaks = c(0, 18, 35, 50, 65, 100),
                               labels = c("0-18", "19-35", "36-50", "51-65", "66+")))
      data_summary <- data.frame(age_groups = age_groups, total_spending = data()$rnd)
      
      # Create a boxplot with colors
      ggplot(data_summary, aes(x = age_groups, y = total_spending, fill = age_groups)) +
        geom_boxplot() +
        scale_fill_manual(values = c("0-18" = "red", "19-35" = "blue",
                                     "36-50" = "green", "51-65" = "yellow", "66+" = "purple")) +
        labs(title = "Boxplot of Age Groups", x = "Age Groups", y = "Total Spending (RND)") +
        theme_bw()
    } else if (plot_choice == "Distribution of Total Spending") {
      hist(data()$rnd,
           col = "darkblue",   # Set bar color
           border = "lightblue",  # Set border color
           main = "Distribution of Total Spending",  # Set title
           xlab = "Total Spending (rnd)",  # Set x-axis label
           ylab = "Frequency")  # Set y-axis label
    } else if (plot_choice == "Total Spending by City") {
      city_totals <- data() %>%
        group_by(city) %>%
        summarize(total = sum(total))
      colors <- c("steelblue", "royalblue", "orange", "forestgreen", "darkred",
                  "purple", "gold", "grey", "magenta", "darkgoldenrod")
      ggplot_chart <- ggplot(city_totals, aes(x = reorder(city, -total), y = total)) +
        geom_bar(stat = "identity", fill = colors[1:nrow(city_totals)]) +
        labs(title = "Total Spending by City", x = "City", y = "Total Spent") +
        theme_classic()
      
      ggplot_chart
    }
  }) 
  
  
  output$payment_type_plot <- renderPlot({
    payment_type_table <- table(data()$paymentType)
    percentage <- paste0(round(100 * payment_type_table / sum(payment_type_table)), "%")
    
    pie(payment_type_table, labels = percentage,
        main = "Pie Chart of Payment Type",
        col = c("steelblue1", "tomato"))
    legend("bottomright", legend = c("Cash", "Credit"),
           fill = c("steelblue1", "tomato"))
  })
  
  output$age_group_plot <- renderPlot({
    age_groups <- factor(cut(data()$age, breaks = c(0, 18, 35, 50, 65, 100),
                             labels = c("0-18", "19-35", "36-50", "51-65", "66+")))
    data_summary <- data.frame(age_groups = age_groups, total_spending = data()$rnd)
    
    # Create a boxplot with colors
    ggplot(data_summary, aes(x = age_groups, y = total_spending, fill = age_groups)) +
      geom_boxplot() +
      scale_fill_manual(values = c("0-18" = "red", "19-35" = "blue",
                                   "36-50" = "green", "51-65" = "yellow", "66+" = "purple")) +
      labs(title = "Boxplot of Age Groups", x = "Age Groups", y = "Total Spending (RND)") +
      theme_bw()
  })
  
  output$total_spending_plot <- renderPlot({
    hist(data()$rnd,
         col = "darkblue",   # Set bar color
         border = "lightblue",  # Set border color
         main = "Distribution of Total Spending",  # Set title
         xlab = "Total Spending (rnd)",  # Set x-axis label
         ylab = "Frequency")  # Set y-axis label
  })
  
  output$city_spending_plot <- renderPlot({
    # 4. City Spending Bar Chart (ggplot2)
    city_totals <- data() %>%
      group_by(city) %>%
      summarize(total = sum(total)) %>%
      arrange(desc(total))
    
    colors <- c("steelblue", "royalblue", "orange", "forestgreen", "darkred",
                "purple", "gold", "grey", "magenta", "darkgoldenrod")
    
    ggplot_chart <- ggplot(city_totals, aes(x = reorder(city, -total), y = total)) +
      geom_bar(stat = "identity", fill = colors[1:nrow(city_totals)]) +
      labs(title = "Total Spending by City", x = "City", y = "Total Spent") +
      theme_classic()
    
    ggplot_chart
  })
  run_kmeans <- reactive({
    req(data())
    n_clusters <- input$num_clusters
    scaled_data <- scale(data()[, c("age", "total")])
    kmeans.results <- kmeans(scaled_data, centers = n_clusters, nstart = 20)
    clustered_data <- cbind(data(), cluster = as.factor(kmeans.results$cluster))
    return(clustered_data[!duplicated(clustered_data$customer), ])  # Remove duplicates based on customer
  })
  
  output$cluster_table <- renderDataTable({
    kmeans_results <- run_kmeans()
    if (!is.null(kmeans_results)) {
      kmeans_results[, c("customer", "age", "total", "cluster")]
    }
  })
  
  observeEvent(input$run_analysis, {
    item <- strsplit(data()$items, ",")
    items <- as(item, "transactions")
    apriori_result <- apriori(items,
                              parameter = list(supp = input$support,
                                               conf = input$confidence,
                                               minlen = 2))
    output$rules_table <- renderTable({
      as(apriori_result, "data.frame")
    })
  })
}

shinyApp(ui = ui, server=server)
