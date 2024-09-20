library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)
library(showtext)
library(writexl)
library(ggplot2)

showtext_auto()
rm(list=ls())

# Function to process data from uploaded files
process_data <- function(files, update_progress) {
  update_progress(0.1, "Reading CSV files")
  
  df_list <- map_df(files$datapath, function(file_path) {
    df <- read_csv(file_path)
    file_name <- basename(file_path)
    df$file <- file_name
    # Extract position from filename (13th character from the end, including .csv)
    df$position <- as.numeric(substr(file_name, nchar(file_name) - 12, nchar(file_name) - 12))
    df
  })
  
  df_list <- df_list %>%
    mutate(cell = str_sub(`CELL ID`, -3, -1)) %>%
    mutate(split = case_when(cell %in% c("A01", "B02", "C04", "D05", "A06", "B07", "C09", "D10") ~ "Sp1",
                             cell %in% c("A03", "C03","A08", "C08") ~ "Sp2",
                             cell %in% c("B03", "D03", "B08", "D08") ~ "Sp3" )) %>%
    mutate(position = parse_number(str_sub(file, -13, -1))) %>%
    select(`Glass ID`, no, `Avg Offset`, cell, position) %>%
    mutate(x = (no-1)*10.96) %>%
    mutate(side = case_when(position == 1 ~ "Left",
                            position == 2 ~ "Right",
                            position == 3 ~ "Top",
                            position == 4 ~ "Bottom"
    )) %>%
    rename(glass = `Glass ID`, y = `Avg Offset`) %>%
    filter(!is.na(side))
  
  update_progress(0.5, "Calculating hump heights")
  
  processed_data <- df_list %>%
    group_by(glass, cell, side) %>%
    summarise(
      hump_dy = if(side != "Bottom") {
        round(max(y, na.rm=T) - last(y), 1)
      } else {
        round(max(y, na.rm=T) - min(y, na.rm=T), 1)
      },
      hump_dx = round(10.96 * which.max(y) - 10.96, 1),
      .groups = "drop"
    ) %>%
    arrange(glass, cell, side)
  
  update_progress(0.9, "Calculating averages by side")
  
  avg_by_side <- df_list %>%
    group_by(side, x) %>%
    summarise(y_avg = mean(y), .groups = "drop") %>%
    mutate(y_avg = y_avg - min(y_avg))
  
  list(processed_data = processed_data, avg_by_side = avg_by_side, raw_data = df_list)
}



# UI definition
ui <- dashboardPage(
  dashboardHeader(title = "SIP Data Processor"),
  
  dashboardSidebar(
    sidebarMenu(
      fileInput("files", "Upload CSV Files", multiple = TRUE, accept = c(".csv")),
      actionButton("process", "Process Data"),
      tags$div(
        style = "display: flex; flex-direction: column; gap: 10px; padding-top: 10px;",
        downloadButton("download_data", "Download Data"),
        downloadButton("download_plot", "Download Plot"),
        downloadButton("download_avg_plot", "Download Avg Plot")
      )
    )
  ),
  
  dashboardBody(
    fluidRow(  
      column(width = 6,
             box(title = "Data Visualization", status = "warning", solidHeader = TRUE,
                 plotOutput("data_plot", width = "100%", height = "400px"), width = NULL)),
      column(width = 6,
             box(title = "Average by Side", status = "warning", solidHeader = TRUE,
                 plotOutput("avg_plot", width = "100%", height = "400px"), width = NULL))
    ),
    
    fluidRow(
      box(title = "Data Preview", status = "primary", solidHeader = TRUE,
          tableOutput("data_preview"), width = 12)
    )
  )
)

# Server logic
server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  
  observeEvent(input$process, {
    req(input$files)
    withProgress(message = 'Processing data', value = 0, {
      progress <- 0
      result <- process_data(input$files, function(value, message) {
        incProgress(amount = value - progress, message = message)
      })
      data(result)
    })
  })
  
  output$data_preview <- renderTable({
    req(data())
    head(data()$processed_data, 8)
  })
  
  output$data_plot <- renderPlot({
    req(data())
    df <- data()$processed_data
    
    ggplot(df, aes(x = side, y = hump_dy, fill = side)) +
      geom_col(position = "dodge") +
      facet_wrap(glass ~ cell, nrow = 2, scales = "free") +
      theme_bw(base_size = 14) +
      labs(title = "Hump Height vs Position of Panel",
           x = "side",
           y = "Hump DY [um]") +
      theme(legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  output$avg_plot <- renderPlot({
    req(data())
    df_avg <- data()$avg_by_side
    
    ggplot(df_avg, aes(x, y_avg, col = side)) +
      geom_point() +
      labs(title = "8K, 상,하,좌,우 위치별 SIP 잉크젯 Edge Profile",
           x = "x[um]", y = "SIP_height [um]") +
      theme_bw()
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("processed_data_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(data()$processed_data, file)
    }
  )
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("SIP_visualization_", Sys.Date(), ".jpg", sep = "")
    },
    content = function(file) {
      req(data())
      df <- data()$processed_data
      
      p <- ggplot(df, aes(x = side, y = hump_dy, fill = side)) +
        geom_col(position = "dodge") +
        facet_wrap(glass ~ cell, nrow = 2, scales = "free") +
        theme_bw(base_size = 14) +
        labs(title = "Hump Height vs Position of Panel",
             x = "side",
             y = "Hump DY [um]") +
        theme(legend.position = "bottom",
              plot.title = element_text(hjust = 0.5, face = "bold"))
      
      ggsave(file, plot = p, width = 12, height = 8, units = "in", dpi = 300)
    },
    contentType = "image/jpeg"
  )
  output$download_avg_plot <- downloadHandler(
    filename = function() {
      paste("SIP_avg_visualization_", Sys.Date(), ".jpg", sep = "")
    },
    content = function(file) {
      req(data())
      df_avg <- data()$avg_by_side
      
      p <- ggplot(df_avg, aes(x, y_avg, col = side)) +
        geom_point() +
        labs(title = "8K, 상,하,좌,우 위치별 SIP 잉크젯 Edge Profile",
             x = "x[um]", y = "SIP_height [um]") +
        theme_bw()
      
      ggsave(file, plot = p, width = 12, height = 8, units = "in", dpi = 300)
    },
    contentType = "image/jpeg"
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
