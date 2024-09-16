# Pacotes - Shiny & Análise de Sobrevivência.
library(knitr)
library(kableExtra)
library(DataExplorer)
library(dlookr)
library(summarytools)
library(corrplot)
library(survival)
library(survminer)
library(powerSurvEpi)
library(rms)
library(sm)
library(glmulti)
library(condsurv)
library(survtools)
library(tidyverse)
library(readxl)
library(patchwork) 
library(magrittr)
library(flexsurv)
library(eha)
library(shiny)
library(shinydashboard)
library(readr)

# Importar os dados.
#-------------------------------------------------------------------------------
base <- read_delim("~/PROJETOS/CANCER/Projeto/Mama/dataset/base_nao_identificada_2864.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(encoding = "Latin1"), 
                   trim_ws = TRUE)


# Resumo mais detalhado da estrutura dos dados.
glimpse(base)

# Verificar duplicidades.
base %>% 
  group_by(`Código do Paciente`) %>% 
  count() %>% 
  arrange(desc(n))

# Retirar pacientes que apresentaram duplicidades no conjunto de dados.
base <- base %>% 
  filter(!`Código do Paciente` %in% c(1125273,1319524,1322075))

#

# Converter colunas as.Date.

#
can_fm <- base %>% filter(str_detect(`Código da Topografia`, "C50"))


colSums(is.na(can_fm))

#
can_fm <- can_fm %>% 
  mutate(across(c(`Data de Nascimento`,`Data do Óbito`,`Data de Último Contato`,`Data de Diagnostico`), ~ as.Date(., format = "%d/%m/%Y")))

#
can_fm <- can_fm %>% 
  mutate(Data_iguais = ifelse(is.na(`Data do Óbito`) | is.na(`Data de Diagnostico`),
                              NA,
                              `Data do Óbito` == `Data de Diagnostico`))

# Criar a variável Status
can_fm <- can_fm %>% 
  mutate(Status = ifelse(!is.na(`Data do Óbito`), 1, 0))

# Antes de criar a variável tempo, verificar o tempo máximo.
max(can_fm$`Data do Óbito`, na.rm = T)

# Substituir NA pela maior data do óbito.
can_fm$`Data do Óbito` <- replace(can_fm$`Data do Óbito`, is.na(can_fm$`Data do Óbito`), as.Date("2018-10-11"))

# Contagem do tempo.
can_fm <- can_fm %>% 
  mutate(Tempo = as.numeric(difftime(`Data do Óbito`, `Data de Diagnostico`, units = "days")))




df <- can_fm %>% 
  mutate(indice = rank(-Tempo, ties.method = "first"))

df <- df %>% 
  filter(Tempo != 0)

#-------------------------------------------------------------------------------
#
ui <- dashboardPage(
  dashboardHeader(
    title = tagList(
      # Adiciona a logo
      tags$img(src = 'mama.png', height = '50px', style = 'float: left; margin-right: 10px;'),
      "Meu Dashboard"
    ),
    # Outras opções do cabeçalho, se necessário
    titleWidth = 250
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Geral", tabName = "geral", icon = icon("dashboard"),
               menuSubItem("Demográficas", tabName = "demograficas"),
               menuSubItem("Morfológicas", tabName = "morfologicas"),
               menuSubItem("Patológicas", tabName = "patologicas"),
               menuSubItem("Tabela", tabName = "clinicas")
      ),
      menuItem("Modelo Kaplan-Meier", tabName = "km", icon = icon("line-chart"),
               menuSubItem("Kaplan-Meier", tabName = "km"),
               menuSubItem("Teste de Log-Rank", tabName = "log-rank")),
      menuItem("Modelos Paramétricos", tabName = "parametric", icon = icon("bar-chart"),
               menuSubItem("Demográficas", tabName = "demograficas"),
               menuSubItem("Demográficas", tabName = "demograficas"),
               menuSubItem("Demográficas", tabName = "demograficas"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$script(src = "custom.js") # Inclui o JavaScript
    ),
    #
    tabItems(
      # Aba Geral
      tabItem(tabName = "geral",
              fluidRow(
                box(title = "Demográficas", status = "primary", solidHeader = TRUE, 
                    dataTableOutput("demograficas")),
                box(title = "Morfológicas", status = "primary", solidHeader = TRUE, 
                    dataTableOutput("morfologicas")),
                box(title = "Patológicas", status = "primary", solidHeader = TRUE, 
                    dataTableOutput("patologicas")),
                box(title = "Clínicas", status = "primary", solidHeader = TRUE, 
                    dataTableOutput("clinicas"))
              )
      ),
      # Modelo Kaplan-Meier
      tabItem(tabName = "km",
              fluidRow(
                box(title = "Curva Kaplan-Meier", status = "primary", solidHeader = TRUE,
                    plotOutput("km_plot"))
              )
      ),
      # Modelos Paramétricos
      tabItem(tabName = "parametric",
              fluidRow(
                box(title = "Modelos Paramétricos", status = "primary", solidHeader = TRUE,
                    plotOutput("parametric_plot"))
              )
      )
    )
  )
)

#-------------------------------------------------------------------------------
#
server <- function(input, output) {
  
  output$demograficas <- renderDataTable({
    # Substitua pelo seu dataset demográfico
    head(lung)
  })
  
  output$morfologicas <- renderDataTable({
    # Substitua pelo seu dataset morfológico
    head(lung)
  })
  
  output$patologicas <- renderDataTable({
    # Substitua pelo seu dataset patológico
    head(lung)
  })
  
  output$clinicas <- renderDataTable({
    # Substitua pelo seu dataset clínico
    head(lung)
  })
  
  output$km_plot <- renderPlot({
    fit <- survfit(Surv(Tempo, Status) ~ 1, data = df)
    g <- ggsurvplot(
      fit, data = df,
      pval = TRUE, 
      conf.int = TRUE,
      xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
      break.time.by = 100,
      ggtheme = theme_light(),
      ylim = c(0.85,1),
      xlim = c(0,1400)
      # risk.table = "abs_pct",
      # risk.table.y.text.col = TRUE,
      # risk.table.y.text = TRUE,
      # ncensor.plot = TRUE,
    )
    print(g)
  })
  
  output$parametric_plot <- renderPlot({
    # Exemplo com modelo Weibull
    fit2 <- survreg(Surv(Tempo, Status) ~ 1, data = df, dist = "weibull")
    pred <- predict(fit2, newdata = df, type = "quantile", p = 0.5)
    g <- ggplot(df, aes(x = time, y = pred)) +
      geom_point() +
      labs(title = "Modelo Paramétrico (Weibull)", x = "Tempo", y = "Predição")
    print(g)
  })
}

#
shinyApp(ui, server)
