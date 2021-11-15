# LIBRARIES LOADING
library(plotly)
library(Metrics)
library(lubridate)
library(data.table)
library(previsionio)
library(shinydashboard)

# RETRIEV ENV VARIABLES
pio_tkn <<- Sys.getenv("PIO_TKN")
pio_url <<- Sys.getenv("PIO_URL")

# INIT PREVISION.IO
pio_init(pio_tkn, pio_url)

# SHINY APP TEMPLATE
ui = dashboardPage(dashboardHeader(title = "Electricity Forecast"),
                   dashboardSidebar(
                     sidebarMenu(
                       menuItem("Forecast", tabName = "forecast"),
                       uiOutput("dateRange")
                     )
                   ),
                   dashboardBody(
                     box(title = "KPIs",
                         status = "primary",
                         solidHeader = T,
                         fluidRow(
                           box(title = "MAPE", textOutput("mape_prevision"), status = "info", solidHeader = F, width = 6),
                           box(title = "RMSE", textOutput("rmse_prevision"), status = "info", solidHeader = F, width = 6)),
                         width = 12),
                     tabItems(
                       tabItem(tabName = "forecast",
                               box(title = "Forecast versus actual consumption",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   plotlyOutput("consumption_plot", height = "650px"),
                                   width = 12
                               )
                       )
                     )
                   )
)

load_data = function() {
  # RETRIEVING DATA FROM THE "VALID" DATASET
  test_id = get_dataset_id_from_name(project_id = get_project_id_from_name("Electricity Forecast"),
                                     dataset_name = "valid")
  test = create_dataframe_from_dataset(dataset_id = test_id,
                                       path = tempdir())
  
  # RETRIEVING PREDICTIONS FROM DEPLOYED MODEL
  pred_id = get_dataset_id_from_name(project_id = get_project_id_from_name("Electricity Forecast"),
                                     dataset_name = "prediction_pipeline_sdk")
  pred = create_dataframe_from_dataset(dataset_id = pred_id,
                                       path = tempdir())
  
  test$pred = pred$pred_TARGET
  
  test$TS = ymd_hms(test$TS, tz = "Europe/Paris")
  test
}

server = function(input, output) {
  data = reactiveVal(load_data())
  
  output$dateRange = renderUI({
    dateRangeInput('dateRange',
                   label = "Select a date : ",
                   start = as.Date("2021-02-01"),
                   end = as.Date("2021-02-01"),
                   min = as.Date("2021-01-01"),
                   max = as.Date("2021-09-30"),
                   weekstart = 1)
  })
  
  # GET THE DATE
  getDate = eventReactive({input$dateRange},{})
  
  # GET FORECAST DATA TO PLOT OF A GIVEN DATE
  toPlot = eventReactive({
    input$dateRange},{
      temp = data()
      temp = temp[as.Date(temp$TS, tz = "Europe/Paris") >= min(input$dateRange) &
                    as.Date(temp$TS, tz = "Europe/Paris") <= max(input$dateRange), ]
      temp
    })
  
  # GET FORECAST DATA FOR THE WHOLE RANGE
  fullData = eventReactive({
    input$dateRange},{
      temp = data()
      temp
    })
  
  output$consumption_plot = renderPlotly({
    temp = toPlot()
    plot_ly(temp, x = ~ TS) %>%
      add_trace(
        y = ~ pred,
        name = 'FORECAST_PIO',
        type = 'scatter',
        mode = 'lines',
        line = list(color = '#19293C', width = 4),
        showlegend = TRUE
      ) %>%
      add_trace(
        y = ~ TARGET,
        name = 'REALISED',
        type = 'scatter',
        mode = 'lines',
        line = list(color = '#FB5C02', dash = 'dot', width = 4),
        showlegend = TRUE
      ) %>%
      layout(
        xaxis = list(
          title = "Time",
          gridcolor = 'rgb(255,255,255)',
          zeroline = FALSE
        ),
        yaxis = list(
          title = "Consumption (MW)",
          gridcolor = 'rgb(255,255,255)',
          zeroline = FALSE
        ),
        legend = list(x = 0.92, y = 0.1)
      )
  })
  
  output$rmse_prevision = renderText({
    ds = toPlot()
    round(rmse(ds$TARGET, ds$pred))
  })
  
  output$mape_prevision = renderText({
    ds = toPlot()
    paste(round(100*mape(ds$TARGET, ds$pred), 2), "%")
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)
