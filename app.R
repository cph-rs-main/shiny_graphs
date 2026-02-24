library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)
library(irrCAC)

# ============================================
# UI DEFINITION
# ============================================
ui <- fluidPage(
  titlePanel("Inter-Rater Reliability: KA vs AC2 Comparison Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h4("About This Demo"),
      p("This dashboard demonstrates how Gwet's AC2 can detect agreement 
        even when Krippendorff's Alpha is low due to systematic rater bias."),
      hr(),
      h4("Scenarios:"),
      tags$ul(
        tags$li(strong("Good KA & AC2:"), "High agreement with occasional flaws like 2,4,4,4"),
        tags$li(strong("Bad KA, Good AC2:"), "Systematic scale bias (strict vs lenient) but preserved ranking")
      ),
      hr(),
      actionButton("refresh", "Generate New Data", class = "btn-primary"),
      br(), br(),
      downloadButton("downloadData", "Download Sample Data")
    ),
    
    mainPanel(
      fluidRow(
        column(6, 
               h3("Good KA & AC2 Scores", style = "text-align: center; color: #2E7D32;"),
               plotOutput("goodPlot", height = "350px"),
               uiOutput("goodMetrics")
        ),
        column(6, 
               h3("Bad KA, Good AC2 Scores", style = "text-align: center; color: #C62828;"),
               plotOutput("badPlot", height = "350px"),
               uiOutput("badMetrics")
        )
      ),
      hr(),
      h4("Sample Data Preview"),
      fluidRow(
        column(6, tableOutput("goodTable")),
        column(6, tableOutput("badTable"))
      ),
      hr(),
      h4("Interpretation Guide"),
      fluidRow(
        column(6,
               wellPanel(
                 h5("Krippendorff's Alpha (KA)"),
                 p("Measures agreement beyond chance. Sensitive to systematic bias 
                   (e.g., strict vs. lenient raters)."),
                 tags$ul(
                   tags$li("< 0.67: Poor agreement"),
                   tags$li("0.67-0.80: Moderate agreement"),
                   tags$li("> 0.80: Good agreement")
                 )
               )
        ),
        column(6,
               wellPanel(
                 h5("Gwet's AC2"),
                 p("Alternative reliability coefficient. More robust to systematic 
                   bias and skewed distributions."),
                 tags$ul(
                   tags$li("< 0.50: Poor agreement"),
                   tags$li("0.50-0.75: Moderate agreement"),
                   tags$li("> 0.75: Good agreement")
                 )
               )
        )
      )
    )
  )
)

# ============================================
# DATA GENERATION FUNCTIONS
# ============================================

generate_good_data <- function(seed_val = 123) {
  set.seed(seed_val)
  
  data <- data.frame(
    Rater1 = integer(60),
    Rater2 = integer(60),
    Rater3 = integer(60),
    Rater4 = integer(60)
  )
  
  for(i in 1:60) {
    if(runif(1) > 0.25) {  # 75% agreement
      common_rating <- sample(3:5, 1, prob = c(0.25, 0.50, 0.25))
      data[i, ] <- common_rating
    } else {
      # Occasional flaws/mismatches
      base <- sample(2:5, 1, prob = c(0.15, 0.35, 0.35, 0.15))
      data$Rater1[i] <- max(1, min(5, base + sample(c(-1, 0, 1), 1, prob = c(0.3, 0.4, 0.3))))
      data$Rater2[i] <- max(1, min(5, base + sample(c(-1, 0, 1), 1, prob = c(0.3, 0.4, 0.3))))
      data$Rater3[i] <- max(1, min(5, base + sample(c(-1, 0, 1), 1, prob = c(0.3, 0.4, 0.3))))
      data$Rater4[i] <- max(1, min(5, base + sample(c(-1, 0, 1), 1, prob = c(0.3, 0.4, 0.3))))
    }
  }
  
  # Add specific flaw examples like 2,4,4,4
  data[9, ] <- c(2, 4, 4, 4)
  data[19, ] <- c(3, 5, 5, 4)
  data[34, ] <- c(2, 3, 5, 5)
  data[42, ] <- c(4, 4, 2, 5)
  data[51, ] <- c(2, 5, 4, 4)
  
  return(data)
}

generate_bad_ka_data <- function(seed_val = 456) {
  set.seed(seed_val)
  
  # True scores for items
  true_scores <- sample(1:5, 60, replace = TRUE, prob = c(0.15, 0.25, 0.30, 0.20, 0.10))
  
  data <- data.frame(
    Rater1 = integer(60),
    Rater2 = integer(60),
    Rater3 = integer(60),
    Rater4 = integer(60)
  )
  
  for(i in 1:60) {
    true <- true_scores[i]
    
    # Strict raters: map to 1-2 range
    if(true <= 2) {
      data$Rater1[i] <- 1
      data$Rater2[i] <- 1
    } else if(true == 3) {
      data$Rater1[i] <- sample(1:2, 1)
      data$Rater2[i] <- sample(1:2, 1)
    } else {
      data$Rater1[i] <- 2
      data$Rater2[i] <- 2
    }
    
    # Lenient raters: map to 4-5 range
    if(true <= 2) {
      data$Rater3[i] <- 4
      data$Rater4[i] <- 4
    } else if(true == 3) {
      data$Rater3[i] <- sample(4:5, 1)
      data$Rater4[i] <- sample(4:5, 1)
    } else {
      data$Rater3[i] <- 5
      data$Rater4[i] <- 5
    }
  }
  
  # Add some flaws
  data[6, ] <- c(1, 2, 5, 4)
  data[16, ] <- c(2, 1, 4, 5)
  data[31, ] <- c(2, 4, 4, 4)
  data[45, ] <- c(1, 2, 4, 5)
  data[55, ] <- c(2, 2, 5, 4)
  
  return(data)
}

# ============================================
# SERVER LOGIC
# ============================================
server <- function(input, output, session) {
  
  # Reactive data
  good_data <- reactive({
    input$refresh
    seed <- ifelse(input$refresh > 0, sample(1:10000, 1), 123)
    generate_good_data(seed)
  })
  
  bad_data <- reactive({
    input$refresh
    seed <- ifelse(input$refresh > 0, sample(1:10000, 1), 456)
    generate_bad_ka_data(seed)
  })
  
  # Helper function for plots
  create_freq_plot <- function(data, title, color_scheme) {
    data_long <- data %>%
      pivot_longer(cols = everything(), values_to = "Rating") %>%
      mutate(Rating = as.numeric(Rating))
    
    freq_table <- data_long %>%
      count(Rating, name = "Count") %>%
      mutate(Percentage = round(Count / sum(Count) * 100, 1))
    
    ggplot(freq_table, aes(x = factor(Rating), y = Count, fill = factor(Rating))) +
      geom_bar(stat = "identity", color = "black", size = 0.3) +
      geom_text(aes(label = paste0(Count, " (", Percentage, "%)")), 
                vjust = -0.5, size = 3.5) +
      scale_fill_manual(values = color_scheme) +
      labs(title = title, x = "Rating", y = "Frequency") +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(size = 12, face = "bold"),
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 11))
  }
  
  # Good data plot
  output$goodPlot <- renderPlot({
    colors <- c("#FFCDD2", "#E57373", "#F44336", "#D32F2F", "#B71C1C")
    create_freq_plot(good_data(), "Rating Distribution (with occasional flaws)", colors)
  })
  
  # Bad data plot
  output$badPlot <- renderPlot({
    colors <- c("#C8E6C9", "#81C784", "#4CAF50", "#388E3C", "#1B5E20")
    create_freq_plot(bad_data(), "Rating Distribution (systematic scale shift)", colors)
  })
  
  # Good metrics
  output$goodMetrics <- renderUI({
    data <- good_data()
    
    alpha_result <- tryCatch({
      krippen.alpha.raw(data, weights = "ordinal")
    }, error = function(e) list(est = NA))
    
    gwet_result <- tryCatch({
      gwet.ac1.raw(data, weights = "ordinal")
    }, error = function(e) list(est = NA))
    
    alpha_val <- round(alpha_result$est, 4)
    gwet_val <- round(gwet_result$est, 4)
    
    wellPanel(
      style = "background-color: #E8F5E9; border-color: #4CAF50;",
      h5("Reliability Metrics", style = "color: #2E7D32; font-weight: bold;"),
      div(
        style = "display: flex; justify-content: space-around;",
        div(
          h4("KA =", alpha_val, 
             style = ifelse(alpha_val > 0.7, "color: green;", "color: orange;")),
          p("Krippendorff's Alpha", style = "font-size: 12px;")
        ),
        div(
          h4("AC2 =", gwet_val,
             style = ifelse(gwet_val > 0.7, "color: green;", "color: orange;")),
          p("Gwet's AC2", style = "font-size: 12px;")
        )
      ),
      p(paste("Rater means:", 
              paste(round(colMeans(data), 2), collapse = ", ")),
        style = "font-size: 11px; color: #666;")
    )
  })
  
  # Bad metrics
  output$badMetrics <- renderUI({
    data <- bad_data()
    
    alpha_result <- tryCatch({
      krippendorff.alpha.raw(data, weights = "ordinal")
    }, error = function(e) list(est = NA))
    
    gwet_result <- tryCatch({
      gwet.ac1.raw(data, weights = "ordinal")
    }, error = function(e) list(est = NA))
    
    alpha_val <- round(alpha_result$est, 4)
    gwet_val <- round(gwet_result$est, 4)
    
    wellPanel(
      style = "background-color: #FFEBEE; border-color: #F44336;",
      h5("Reliability Metrics", style = "color: #C62828; font-weight: bold;"),
      div(
        style = "display: flex; justify-content: space-around;",
        div(
          h4("KA =", alpha_val,
             style = ifelse(alpha_val < 0.5, "color: red;", "color: orange;")),
          p("Krippendorff's Alpha", style = "font-size: 12px;")
        ),
        div(
          h4("AC2 =", gwet_val,
             style = ifelse(gwet_val > 0.5, "color: green;", "color: orange;")),
          p("Gwet's AC2", style = "font-size: 12px;")
        )
      ),
      p(paste("Rater means:", 
              paste(round(colMeans(data), 2), collapse = ", ")),
        style = "font-size: 11px; color: #666;")
    )
  })
  
  # Sample tables
  output$goodTable <- renderTable({
    head(good_data(), 8)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$badTable <- renderTable({
    head(bad_data(), 8)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("sample-data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      good <- good_data()
      good$Scenario <- "Good_KA_AC2"
      bad <- bad_data()
      bad$Scenario <- "Bad_KA_Good_AC2"
      write.csv(rbind(good, bad), file, row.names = FALSE)
    }
  )
}

# ============================================
# RUN APP
# ============================================
shinyApp(ui = ui, server = server)