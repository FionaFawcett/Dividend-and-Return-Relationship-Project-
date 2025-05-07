library(shiny)
library(tidyverse)
library(quantmod)
library(DT)
library(bslib)

# --- Data Preparation: Monthly NVDA Data ---
getSymbols("NVDA", src = "yahoo", from = "2010-01-01", to = Sys.Date())

# Monthly prices and returns
monthly_prices_xts <- to.monthly(Ad(NVDA), indexAt = "lastof", drop.time = TRUE)
monthly_returns_xts <- monthlyReturn(Ad(NVDA), leading = FALSE)

# Convert to data frames
monthly_prices_df <- data.frame(
  date = index(monthly_prices_xts),
  Adjusted = coredata(monthly_prices_xts)[, 1]
)

monthly_returns_df <- data.frame(
  date = index(monthly_returns_xts),
  Monthly_Return = coredata(monthly_returns_xts)[, 1]
)

# Get dividend data and safely merge
divNVDA <- tryCatch(getDividends("NVDA", auto.assign = FALSE), error = function(e) NULL)

if (!is.null(divNVDA) && nrow(divNVDA) > 0) {
  div_df <- data.frame(
    date = as.Date(format(index(divNVDA), "%Y-%m-01")),
    dividend_value = as.numeric(divNVDA)
  ) %>%
    group_by(date) %>%
    summarise(dividends = sum(dividend_value, na.rm = TRUE)) %>%
    ungroup()
} else {
  div_df <- data.frame(
    date = as.Date(character()),
    dividends = numeric()
  )
}

# Merge all data by date
NVDA_mergedDF <- left_join(monthly_prices_df, monthly_returns_df, by = "date") %>%
  left_join(div_df, by = "date")

# Add features
NVDA_DF <- NVDA_mergedDF %>%
  mutate(
    AvPrice = cummean(Adjusted),
    volatility = map_dbl(seq_along(Adjusted), ~ sd(Adjusted[1:.x], na.rm = TRUE))
  )

# --- UI ---
ui <- fluidPage(
  tags$head(
    tags$style(HTML("body { background-color: black !important; }"))
  ),
  theme = bs_theme(bg = "black", fg = "white", primary = "#3f51b5", base_font = font_google("Poppins")),
  
  titlePanel("NVIDIA Stock Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      tags$style(HTML(".form-control, label, h5 { color: white !important; }")),
      selectInput("metric", "Select a Metric:",
                  choices = c("Monthly Return" = "Monthly_Return",
                              "Volatility" = "volatility",
                              "Average Price" = "AvPrice")),
      sliderInput("dateRange", "Date Range:",
                  min = min(NVDA_DF$date),
                  max = max(NVDA_DF$date),
                  value = c(min(NVDA_DF$date), max(NVDA_DF$date)),
                  timeFormat = "%Y-%m"),
      hr(),
      h5("Created by Caleb and Fiona")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Metric Plot", plotOutput("metricPlot")),
        tabPanel("Regression Analysis",
                 plotOutput("regPlot"),
                 verbatimTextOutput("regSummary")),
        tabPanel("Dashboard",
                 h4("Dividends and Volatility Over Time", style = "color: white;"),
                 DT::dataTableOutput("divTable"))
      )
    )
  )
)

# --- Server ---
server <- function(input, output) {
  filtered_data <- reactive({
    NVDA_DF %>%
      filter(date >= input$dateRange[1], date <= input$dateRange[2])
  })
  
  output$metricPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = date, y = .data[[input$metric]])) +
      geom_line(color = "#1e88e5", size = 1.2) +
      labs(title = paste("NVIDIA", input$metric, "Over Time"),
           x = "Date", y = input$metric) +
      theme_minimal(base_family = "Poppins") + theme(plot.background = element_rect(fill = "black"), panel.background = element_rect(fill = "black"), text = element_text(color = "white"), axis.text = element_text(color = "white"), axis.title = element_text(color = "white"), plot.title = element_text(color = "white"))
  })
  
  output$regPlot <- renderPlot({
    NVDAMetal_clean <- NVDAMetal %>%
      filter(
        is.finite(`Monthly Percent Returns`) &
          is.finite(`Historical Percent Copper Returns`) &
          is.finite(`Historical Percent Aluminum Returns`)
      )
    
    ggplot(NVDAMetal_clean, aes(x = `Historical Percent Copper Returns`, y = Log_Monthly_Returns)) +
      geom_point(color = "#e91e63", alpha = 0.7) +
      geom_smooth(method = "lm", formula = y ~ x, color = "blue", se = FALSE) +
      labs(title = "Regression: Log Monthly Return ~ Copper + Aluminum",
           x = "Copper Returns", y = "Log Monthly Return") +
      theme_classic(base_family = "Poppins") +
      theme(plot.background = element_rect(fill = "black"), panel.background = element_rect(fill = "black"),
            text = element_text(color = "white"), axis.text = element_text(color = "white"),
            axis.title = element_text(color = "white"), plot.title = element_text(color = "white"))
  })
  
  output$regSummary <- renderPrint({
    NVDAMetal_clean <- NVDAMetal %>%
      filter(
        is.finite(`Monthly Percent Returns`) &
          is.finite(`Historical Percent Copper Returns`) &
          is.finite(`Historical Percent Aluminum Returns`)
      )
    
    model <- lm(Log_Monthly_Returns ~ `Historical Percent Copper Returns` + `Historical Percent Aluminum Returns`,
                data = NVDAMetal_clean)
    summary(model)
  })
  
  output$divTable <- DT::renderDataTable({
    options = list(
      initComplete = JS(
        "function(settings, json) {",
        "  $(this.api().table().body()).css({ 'color': 'white' });",
        "}"
      )
    )
    filtered_data() %>%
      select(date, dividends, volatility) %>%
      arrange(desc(date))
  })
}

# --- Run App ---
shinyApp(ui = ui, server = server)
