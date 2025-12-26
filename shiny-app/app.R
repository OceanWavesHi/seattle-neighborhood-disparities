library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(plotly)

# Load Data 
df <- read.csv("seattle_neighborhood_analysis.csv")

# UI 
ui <- fluidPage(
  
  titlePanel("Seattle Neighborhood Health Analysis"),
  
  tabsetPanel(
    
# ================================ OVERVIEW TAB
    tabPanel("Overview",
             fluidRow(
               column(8, offset = 2,
                      h2("Project Overview"),
                      tags$hr(),
                      p("This project explores how social and demographic factors relate to neighborhood-level health outcomes in Seattle. 
                        We combine two key datasets: one detailing disability and health insurance status across Seattle neighborhoods, 
                        and another capturing racial and social equity metrics, including socioeconomic and health disadvantages."),
                      p("Our goal is to examine patterns of neighborhood disadvantage, identify key dimensions of community health,
                        and cluster Seattle neighborhoods with similar profiles. The interactive components in this application allow users to 
                        explore differences in socioeconomic and health-related factors and observe how these differences contribute to health inequity."),
                      tags$hr(),
                      h4("Use the tabs above to explore:"),
                      tags$ul(
                        tags$li("Interactive visualizations of health outcome patterns"),
                        tags$li("A clustering model grouping neighborhoods by shared characteristics"),
                        tags$li("A searchable, interactive dataset of Seattle neighborhoods")
                      )
               )
             )
    ),
    
# ================================ VISUALIZATIONS TAB
    tabPanel(
      "Visualizations",
      
      fluidRow(
        column(
          width = 3,
          wellPanel(
            sliderInput("bins", "Histogram bins:", min = 5, max = 40, value = 15),
            checkboxInput("show_kde", "Show KDE (density curve)", TRUE),
            selectInput(
              "scatter_yvar",
              "Select Y-axis for scatter plot:",
              choices = c(
                "% Uninsured" = "percent_uninsured",
                "% Adults with Disability" = "percent_adults_with_disability",
                "Health Disadvantage Score" = "health_disadvantage_score_AVG",
                "Disability Rate Mean" = "disability_rate_mean_of_ratios_AVG"
              ),
              selected = "percent_uninsured"
            )
          )
        ),
        
        column(
          width = 9,
          
          # Create a 2-column grid for all charts
          fluidRow(
            column(width = 6, plotOutput("hist_plot")),
            column(width = 6, plotOutput("uninsured_scatter"))
          ),
          
          fluidRow(
            column(width = 6, plotOutput("uninsured_vs_disability")),   # <-- Boxplot sits flush left
            column(width = 6, plotOutput("uninsured_by_disadvantage"))
          )
        )
      )
    ),
    
# ================================ MODEL TAB
 
tabPanel("Model (K-means)",
             sidebarLayout(
               sidebarPanel(
                 h4("K-means Controls"),
                 numericInput("clusters", "Number of Clusters:", value = 3, min = 2, max = 10),
                 checkboxGroupInput("kmeans_vars", "Select variables for clustering:",
                                    choices = c("Health Disadvantage Score" = "health_disadvantage_score_AVG",
                                                "Poverty Rate" = "poverty_rate_avg",
                                                "% Adults with Disability" = "percent_adults_with_disability",
                                                "% Uninsured" = "percent_uninsured",
                                                "Population" = "population_count"),
                                    selected = c("health_disadvantage_score_AVG",
                                                 "poverty_rate_avg",
                                                 "percent_adults_with_disability"))
               ),
               mainPanel(
                 h3("Cluster Visualization"),
                 plotlyOutput("kmeans_plot", height = "450px"),
                 tags$hr(),
                 h3("Model Summary"),
                 tableOutput("kmeans_summary"),
                 tags$hr(),
                 tags$p(
                   "Note: Cluster values represent the average of each variable for that cluster. ",
                   "Higher numbers indicate higher values of the variable. For example, a higher ",
                   "Health Disadvantage Score means the neighborhoods in that cluster are more disadvantaged; ",
                   "a higher Poverty Rate means more residents live in poverty; a higher % Adults with Disability ",
                   "means more adults in the cluster have a disability. Compare values across clusters to interpret 'low' vs 'high'."
                 )
               )
             )
    ),
    
# ================================ DATA TAB
    tabPanel("Data Table",
             h3("Seattle Neighborhood Dataset"),
             DTOutput("table")
    )
  )
)

# ================================ SERVER
server <- function(input, output, session) {
  
  # Uninsured Histogram
  output$hist_plot <- renderPlot({
    p <- ggplot(df, aes(x = percent_uninsured)) +
      geom_histogram(bins = input$bins, fill = "steelblue", color = "white") +
      labs(title = "Distribution of % Uninsured",
           x = "Percentage Uninsured", 
           y = "Count of Neighborhoods")
    
    if(input$show_kde){
      p <- p + geom_density(aes(y = ..count..), color = "red", size = 1)
    }
    p
  })
  
  #Labels for uninsured scatterplot 
  y_labels <- c(
    percent_uninsured = "% Uninsured",
    percent_adults_with_disability = "% Adults with Disability",
    health_disadvantage_score_AVG = "Health Disadvantage Score",
    disability_rate_mean_of_ratios_AVG = "Disability Rate Mean"
  )
  
  # Uninsured Scatterplot
  output$uninsured_scatter <- renderPlot({
    yvar <- input$scatter_yvar  # Get user-selected variable
    y_label <- y_labels[yvar] #Get friendly label
    
    ggplot(df, aes_string(x = "poverty_rate_avg", y = yvar,
                          size = "population_count",
                          color = "health_disadvantage_score_AVG",
                          label = "neighborhood_name")) +
      geom_point(alpha = 0.7) +
      scale_color_gradient(low = "lightblue", high = "red") +
      labs(title = paste(y_label, "vs Poverty Rate (Size = Population)"),
           x = "Poverty Rate",
           y = y_label,
           color = "Health Disadvantage",
           size = "Population") +
      theme_minimal()
  })

  # Uninsured vs disability
  output$uninsured_vs_disability <- renderPlot({
    ggplot(df, aes(x = percent_adults_with_disability, y = percent_uninsured)) +
      geom_point(color = "steelblue", alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE, color = "red") +
      labs(title = "% Uninsured vs % Adults with Disability",
           x = "% Adults with Disability",
           y = "% Uninsured") +
      theme_minimal()
  })
  
  # % Uninsured by health disadvantage score
  df <- df %>% mutate(disadv_quartile = ntile(health_disadvantage_score_AVG, 4))
  
  output$uninsured_by_disadvantage <- renderPlot({
    ggplot(df, aes(x = factor(disadv_quartile), y = percent_uninsured)) +
      geom_boxplot(fill = "steelblue") +
      labs(title = "% Uninsured by Health Disadvantage Quartile",
           x = "Health Disadvantage Quartile (1=Lowest, 4=Highest)",
           y = "% Uninsured") +
      theme_minimal()
  })

  
 
  # Friendly labels for clustering variables
  kmeans_labels <- c(
    health_disadvantage_score_AVG = "Health Disadvantage Score",
    poverty_rate_avg = "Poverty Rate",
    percent_adults_with_disability = "% Adults with Disability",
    percent_uninsured = "% Uninsured",
    population_count = "Population"
  )
  
  # K-means 
  output$kmeans_plot <- renderPlotly({
    req(input$kmeans_vars)
    
    # Prepare data
    kdata <- scale(df[, input$kmeans_vars])
    km <- kmeans(kdata, centers = input$clusters, nstart = 25)
    
    df$cluster <- factor(km$cluster)
    
    # Extract selected variable names
    xvar <- input$kmeans_vars[1]
    yvar <- input$kmeans_vars[2]
    
    plot_ly(
      df,
      x = ~df[[xvar]],
      y = ~df[[yvar]],
      color = ~cluster,
      text = ~neighborhood_name,
      type = "scatter",
      mode = "markers"
    ) %>%
      layout(
        title = "K-means Clustering of Neighborhoods",
        xaxis = list(title = kmeans_labels[xvar]),
        yaxis = list(title = kmeans_labels[yvar])
      )
  })
  
  #K-means summary table 
  output$kmeans_summary <- renderTable({
    req(input$kmeans_vars)
    
  # Run K-means
    kdata <- scale(df[, input$kmeans_vars])
    km <- kmeans(kdata, centers = input$clusters, nstart = 25)
    
   #Compute cluster centroids 
    centroids <- aggregate(df[, input$kmeans_vars], 
                           by = list(Cluster = km$cluster), 
                           FUN = mean)
    # Rename columns with friendly labels
    colnames(centroids)[-1] <- kmeans_labels[input$kmeans_vars]
    
    # Add cluster size
    centroids$Size <- as.vector(table(km$cluster))
    
    # Reorder columns: Cluster, Size, then variables
    centroids <- centroids[, c("Cluster", "Size", colnames(centroids)[2:(1+length(input$kmeans_vars))])]

    
  })
  
  # Data table 
  output$table <- renderDT({
    datatable(df, options = list(pageLength = 15, scrollX = TRUE))
  })
}

shinyApp(ui = ui, server = server)