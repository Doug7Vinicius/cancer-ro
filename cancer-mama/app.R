library(shiny)
library(shinydashboard)
library(survival)
library(survminer)
library(ggplot2)
library(plotly)
library(DT)

# Interface do Usuário (UI)
ui <- dashboardPage(
  dashboardHeader(title = "Análise de Sobrevivência - Câncer de Mama"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Carregar Dados", tabName = "upload", icon = icon("upload")),
      menuItem("Visualizar Dados", tabName = "data", icon = icon("table")),
      menuItem("Curva Kaplan-Meier", tabName = "km_plot", icon = icon("heartbeat")),
      menuItem("Regressão de Cox", tabName = "cox_model", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Aba de Carregar Dados
      tabItem(tabName = "upload",
              fluidRow(
                box(title = "Carregar Arquivo CSV", width = 12, status = "primary", solidHeader = TRUE,
                    fileInput("file1", "Escolha um arquivo CSV", accept = ".csv"),
                    actionButton("load_data", "Carregar Dados"))
              )
      ),
      
      # Aba de Visualizar Dados
      tabItem(tabName = "data",
              fluidRow(
                box(title = "Visualizar Dados", width = 12, dataTableOutput("data_table"))
              )
      ),
      
      # Aba de Curva Kaplan-Meier
      tabItem(tabName = "km_plot",
              fluidRow(
                box(title = "Curva Kaplan-Meier", width = 12, status = "primary", solidHeader = TRUE,
                    selectInput("time_col", "Coluna de Tempo", choices = NULL),
                    selectInput("status_col", "Coluna de Status", choices = NULL),
                    selectInput("group_col", "Coluna de Grupo", choices = NULL),
                    actionButton("plot_km", "Gerar Curva Kaplan-Meier"),
                    plotlyOutput("km_plot_output"))
              )
      ),
      
      # Aba de Regressão de Cox
      tabItem(tabName = "cox_model",
              fluidRow(
                box(title = "Modelo de Regressão de Cox", width = 12, status = "primary", solidHeader = TRUE,
                    selectInput("time_cox", "Coluna de Tempo", choices = NULL),
                    selectInput("status_cox", "Coluna de Status", choices = NULL),
                    selectInput("covariates", "Covariáveis", choices = NULL, multiple = TRUE),
                    actionButton("run_cox", "Executar Regressão de Cox"),
                    dataTableOutput("cox_table_output"))
              )
      )
    )
  )
)

# Servidor (Server)
server <- function(input, output, session) {
  # Carregar os dados
  data <- reactiveVal(NULL)
  
  observeEvent(input$load_data, {
    req(input$file1)
    data <- read.csv(input$file1$datapath)
    reactiveVal(data)
    
    # Atualizar as colunas para os menus dropdown
    updateSelectInput(session, "time_col", choices = colnames(data))
    updateSelectInput(session, "status_col", choices = colnames(data))
    updateSelectInput(session, "group_col", choices = colnames(data))
    updateSelectInput(session, "time_cox", choices = colnames(data))
    updateSelectInput(session, "status_cox", choices = colnames(data))
    updateSelectInput(session, "covariates", choices = colnames(data))
  })
  
  # Exibir a tabela de dados
  output$data_table <- renderDataTable({
    req(data())
    datatable(data())
  })
  
  # Plotar a Curva Kaplan-Meier
  observeEvent(input$plot_km, {
    req(data())
    
    fit <- survfit(Surv(as.numeric(data()[[input$time_col]]), as.numeric(data()[[input$status_col]])) ~ data()[[input$group_col]])
    
    output$km_plot_output <- renderPlotly({
      ggsurv <- ggsurvplot(fit, data = data())
      ggplotly(ggsurv$plot)
    })
  })
  
  # Executar o Modelo de Regressão de Cox
  observeEvent(input$run_cox, {
    req(data())
    
    formula <- as.formula(paste0("Surv(as.numeric(", input$time_cox, "), as.numeric(", input$status_cox, ")) ~ ", paste(input$covariates, collapse = "+")))
    cox_model <- coxph(formula, data = data())
    
    output$cox_table_output <- renderDataTable({
      summary(cox_model)$coefficients %>%
        as.data.frame() %>%
        datatable()
    })
  })
}

# Executar o aplicativo
shinyApp(ui, server)
