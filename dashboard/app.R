library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinylive)
library(shinycssloaders)
library(ems)
library(httr)
library(httr2)
library(ggplot2)

url <- "https://slos-api-48722054238.us-central1.run.app"
options(shiny.usecps = FALSE)
Sys.setlocale("LC_NUMERIC", "C")
ui <- dashboardPage(
  dashboardHeader(title = "ICU Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Patient Predictor", tabName = "predictor", icon = icon("stethoscope")),
      menuItem("ICU Efficiency Analysis", tabName = "efficiency", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    
    tags$head(tags$style(HTML("
      .content-wrapper, .right-side { min-height: 100vh !important; }, .shiny-spinner-output-container {
    display: inline-block !important;
    width: auto !important;
  }
    "))),
    tags$html(lang = "en"),
    tags$script(HTML("
  document.addEventListener('shiny:connected', function() {
    const allNumericInputs = document.querySelectorAll('input[type=\"number\"]');
    allNumericInputs.forEach(el => {
      el.setAttribute('lang', 'en');
      el.setAttribute('inputmode', 'decimal');
    });
  });
")),
    
    tabItems(
      tabItem(tabName = "efficiency",
              fluidRow(
                box(title = "Upload Data", status = "primary", solidHeader = TRUE, width = 3,
                    fileInput("data_file", "Upload RData File", accept = c(".RData")),
                    textOutput("eval_status"),
                    conditionalPanel(
                      condition = "output.predictions_ready",
                      
                      div(style = "font-size: 14px; margin-bottom: 5px;",
                          strong("R² Value: "), textOutput("r_squared", inline = TRUE)
                      ),
                      
                      div(style = "font-size: 14px; margin-bottom: 5px;",
                          strong("Median SLOS: "), textOutput("median_slos", inline = TRUE)
                      ),
                      
                      div(style = "font-size: 14px; margin-bottom: 5px;",
                          strong("Q1: "), textOutput("q1", inline = TRUE)
                      ),
                      
                      div(style = "font-size: 14px; margin-bottom: 5px;",
                          strong("Q3: "), textOutput("q3", inline = TRUE)
                      ),
                      
                      div(style = "font-size: 14px; margin-bottom: 5px;",
                          strong("IQR: "), textOutput("iqr", inline = TRUE)
                      ),
                      
                      downloadButton("download_rdata", "Download Predictions (RData)"),
                      downloadButton("download_csv", "Download Predictions (CSV)")
                    ),
                    
                ),
                
                box(
                  title = "Documentation",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  tags$p(
                    "For details on the data format, see the ",
                    tags$a(href = "https://github.com/igor-peres/ICU-Length-of-Stay-Prediction/raw/main/DataDictionary.pdf",
                           "Data Dictionary (PDF)", target = "_blank")
                  )
                ),
                
                
                
                box(title = "Analysis Plots", status = "info", solidHeader = TRUE, width = 9,
                    plotOutput("funnel_plot"),
                    plotOutput("slos_plot")
                )
              )
      ),
      
      tabItem(tabName = "predictor",
              fluidRow(
                box(title = "Patient Information", status = "primary", solidHeader = TRUE, width = 3,
                    pickerInput("gender", "Gender:", choices = c("Female", "Male"), selected = "Female"),
                    numericInput("age", "Age (>= 16):", value = 22, min = 16),
                    pickerInput("admission_source", "Source of ICU Admission:", 
                                choices = c("Cardiovascular intervention room", "Emergency room", 
                                            "Operating room", "Other", "Other unit at your hospital", 
                                            "Ward/Floor"), selected = "Emergency room"),
                    pickerInput("admission_type", "Type of ICU Admission:", 
                                choices = c("Medical", "Emergency Surgery", "Scheduled Surgery"), selected = "Medical")
                ),
                
                box(title = "Clinical Measurements", status = "warning", solidHeader = TRUE, width = 4,
                    numericInput("urea", "Urea (first 1h)(mg/dL):", value = 37, min = 0),
                    numericInput("length_hospital_stay", "Length of Hospital Stay Prior to ICU (days):", value = 0, min = 0),
                    numericInput("creatinine", "Highest Creatinine (first 1h)(mg/dL)", value = 0.9, min = 0),
                    numericInput("high_heart", "Highest Heart Rate (first 1h)(beats per minute):", value = 79, min = 0, max = 200),
                    numericInput("high_temp", "Highest temperature (first 1h)(Celsius):", value = 36.1, min = 0, max = 50),
                    numericInput("high_leuko", "Highest Leukocyte count (first 1h):(10³/µL)", value = 8.9, min = 0),
                    numericInput("high_resp", "Highest Respiratory Rate (first 1h)(breaths per minute):", value = 18, min = 0),
                    numericInput("lowest_gcs", "Lowest Glasgow Coma Scale (3-15):", value = 15, min = 3, max = 15)
                ),
                
                box(title = "Complications (first 24h)", status = "danger", solidHeader = TRUE, width = 4,
                    checkboxInput("ventilation", "Non-invasive Ventilation", FALSE),
                    checkboxInput("resp_failure", "Respiratory Failure", FALSE),
                    checkboxInput("is_mechanical_ventilation", "Mechanical Ventilation", FALSE),
                    checkboxInput("is_vasopressors", "Vasopressors", FALSE),
                    checkboxInput("aaf", "Acute Atrial Fibrillation", FALSE),
                    checkboxInput("aki", "Acute Kidney Injury", FALSE),
                ),
                
                box(title = "Comorbidities", status = "danger", solidHeader = TRUE, width = 4,
                    pickerInput("cirrhosis", "Cirrhosis:", choices = c("ChildAB", "ChildC", "No"), selected = "No"),
                    pickerInput("crf", "Chronic Renal Failure:", choices = c("Dialysis", "NoDialysis", "No"), selected = "No"),
                    pickerInput("ChfNyha", "Cardiac Heart Failure:", choices = c("Class23", "Class4", "No"), selected = "No"),
                    fluidRow(
                      column(6,
                             checkboxInput("dementia", "Dementia", FALSE),
                             checkboxInput("aids", "AIDS", FALSE),
                             checkboxInput("alcohol", "Alcoholism", FALSE),
                             checkboxInput("hypertension", "Arterial Hypertension", FALSE),
                             checkboxInput("asthma", "Asthma", FALSE),
                             checkboxInput("arrhythmia", "Cardiac Arrhythmia", FALSE)
                      ),
                      column(6,
                             checkboxInput("angina", "Angina", FALSE),
                             checkboxInput("asystole", "Asystole", FALSE),
                             checkboxInput("transplant", "Cardiac Transplant", FALSE),
                             checkboxInput("chemo", "Chemotherapy", FALSE),
                             checkboxInput("caf", "Chronic Atrial Fibrillation", FALSE)
                      )
                    )
                ),
                box(
                  title = "Predicted Length of Stay:",
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  fluidRow(
                    column(width = 4,
                           textOutput("predict_status"),
                           actionButton("predict", "Predict", icon = icon("calculator"), class = "btn-success")
                    ),
                    column(width = 3,
                           withSpinner(
                             verbatimTextOutput("prediction"),
                             type = 6,
                             size = 0.4
                           )
                    )
                  )
                ),
                box(
                  title = "Documentation",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  tags$p(
                    "For details on the data format, see the ",
                    tags$a(href = "https://github.com/igor-peres/ICU-Length-of-Stay-Prediction/raw/main/DataDictionary.pdf",
                           "Data Dictionary (PDF)", target = "_blank")
                  )
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  status <- reactiveValues(eval = "", predict = "")
  
  ### ICU EFFICIENCY
  
  output$eval_status <- renderText({ req(status$eval); status$eval })
  
  data_reactive <- reactive({
    
    req(input$data_file)
    
    env <- new.env()
    load(input$data_file$datapath, envir = env)
    
    objs <- ls(env)
    
    #validate(
    #  need(length(objs) > 0, "No objects found in the uploaded file.")
    #)
    
    data <- env[[objs[1]]]
    
    #validate(
    #  need(is.data.frame(data), "The uploaded file must contain a data frame.")
    #)
    
    return(data)
  })
  
  result_metrics_reactive <- reactive({
    req(data_reactive())  
    status$eval <- "" 
    
    #########################
    #       SLOS TEST       #
    #########################
    # result <- SLOS(data_reactive())
    
    temp_file <- tempfile(fileext =".rds")
    saveRDS(data_reactive(), temp_file)
    url_SLOS <- paste(url, "/SLOS_API", sep ="")
    
    response <- request(url_SLOS) %>%
      req_body_multipart(file = upload_file(temp_file)) %>%
      req_timeout(200) %>%
      req_perform()
    
    temp_rds_file <- tempfile(fileext = ".rds")
    
    writeBin(resp_body_raw(response), temp_rds_file)
    
    result <- readRDS(temp_rds_file)
    
    unlink(temp_rds_file)
    
    return(result)  
  })
  
  output$funnel_plot <- renderPlot({
    req(result_metrics_reactive())  
    plot(result_metrics_reactive()$funnel_plot)  
  })
  
  
  output$slos_plot <- renderPlot({
    req(result_metrics_reactive())
    
    p <- result_metrics_reactive()$plot_SLOS_obs_prev
    df <- p$data
    
    ggplot(df, aes(x = soma_los_esp, y = soma_los_obs)) +
      geom_point() +
      geom_smooth(se = TRUE) +
      geom_abline(intercept = 0, slope = 1) +
      labs(
        x = "Sum of predicted ICU LoS",
        y = "Sum of observed ICU LoS",
        title = "Grouped LoS per Unit (days)"
      )
  })
  
  output$r_squared <- renderText({
    req(result_metrics_reactive())
    round(result_metrics_reactive()$r_squared, 4)
  })
  
  output$median_slos <- renderText({
    req(result_metrics_reactive())
    round(result_metrics_reactive()$theta, 4)
  })
  
  output$q1 <- renderText({
    req(result_metrics_reactive())
    paste(round(result_metrics_reactive()$slos_summary$Q1, 4))
  })
  
  output$q3 <- renderText({
    req(result_metrics_reactive())
    paste(round(result_metrics_reactive()$slos_summary$Q3, 4))
  })
  
  output$iqr <- renderText({
    req(result_metrics_reactive())
    paste(round(result_metrics_reactive()$slos_summary$IQR, 4))
  })
  
  output$predictions_ready <- reactive({
    return(!is.null(result_metrics_reactive()))
  })
  outputOptions(output, "predictions_ready", suspendWhenHidden = FALSE)
  
  output$download_rdata <- downloadHandler(
    filename = function() { "SLOS_predictions.RData" },
    content = function(file) {
      predictions <- result_metrics_reactive()$slos$df_unit_slos
      save(predictions, file = file)
    }
  )
  
  output$download_csv <- downloadHandler(
    filename = function() { "SLOS_predictions.csv" },
    content = function(file) {
      predictions <- result_metrics_reactive()$slos$df_unit_slos
      write.csv(predictions, file, row.names = FALSE)
    }
  )
  
  output$eval_status <- renderText({ status$eval })
  
  ### PATIENT PREDICTOR
  predicted_los <- eventReactive(input$predict, {
    
    predictors <- data.frame(
      UnitCode = "00",
      UnitLengthStay_trunc = 0,
      Gender = factor(ifelse(input$gender == "Female", "F", "M"), levels = c("F", "M")),
      Age = input$age,
      AdmissionSourceName = input$admission_source,
      AdmissionTypeName = input$admission_type,
      LowestGlasgowComaScale1h = input$lowest_gcs,
      Urea = input$urea,
      LengthHospitalStayPriorUnitAdmission = input$length_hospital_stay,
      IsMechanicalVentilation = factor(as.integer(input$is_mechanical_ventilation), levels = c(0, 1)),
      IsVasopressors = factor(as.integer(input$is_vasopressors), levels = c(0, 1)),
      HighestCreatinine1h = as.numeric(input$creatinine),
      IsNonInvasiveVentilation = factor(as.integer(input$ventilation), levels = c(0, 1)),
      IsRespiratoryFailure = factor(as.integer(input$resp_failure), levels = c(0, 1)),
      IsDementia = factor(as.integer(input$dementia), levels = c(0, 1)),
      HighestHeartRate1h = as.numeric(input$high_heart),
      HighestTemperature1h = as.numeric(input$high_temp),
      ChfNyha = factor(input$ChfNyha),
      HighestLeukocyteCount1h = as.numeric(input$high_leuko),
      HighestRespiratoryRate1h = as.numeric(input$high_resp),
      IsAcuteAtrialFibrilation = factor(as.integer(input$aaf), levels = c(0, 1)),
      IsAcuteKidneyInjury = factor(as.integer(input$aki), levels = c(0, 1)),
      IsAids = factor(as.integer(input$aids), levels = c(0, 1)),
      IsAlcoholism = factor(as.integer(input$alcohol), levels = c(0, 1)),
      IsAngina = factor(as.integer(input$angina), levels = c(0, 1)),
      IsArterialHypertension = factor(as.integer(input$hypertension), levels = c(0, 1)),
      IsAsthma = factor(as.integer(input$asthma), levels = c(0, 1)),
      IsAsystole = factor(as.integer(input$asystole), levels = c(0, 1)),
      IsCardiacArrhythmia = factor(as.integer(input$arrhythmia), levels = c(0, 1)),
      IsCardiacTransplant = factor(as.integer(input$transplant), levels = c(0, 1)),
      IsChemotherapy = factor(as.integer(input$chemo), levels = c(0, 1)),
      IsChronicAtrialFibrilation = factor(as.integer(input$caf), levels = c(0, 1)),
      IsCirrhosis = factor(input$cirrhosis),
      IsCrf = factor(input$crf)
    )
    
    temp_file <- tempfile(fileext = ".rds")
    saveRDS(predictors, temp_file)
    
    response <- request(paste(url, "/predict_and_evaluate_API", sep="")) %>%
      req_body_multipart(file = upload_file(temp_file)) %>%
      req_timeout(200) %>%
      req_perform()
    
    temp_rds_file <- tempfile(fileext = ".rds")
    writeBin(resp_body_raw(response), temp_rds_file)
    
    result <- readRDS(temp_rds_file)
    
    unlink(temp_file)
    unlink(temp_rds_file)
    
    result
  })
  
  output$prediction <- renderText({
    req(predicted_los())
    paste0(round(predicted_los()$predictions, 2), " days")
  })
}

shinyApp(ui, server, options = list(height = 980))
