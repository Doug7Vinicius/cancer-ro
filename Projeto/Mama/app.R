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
library(DT)
library(plotly)

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
can_fm$`Data do Óbito` <- replace(can_fm$`Data do Óbito`, is.na(can_fm$`Data do Óbito`), as.Date("2019-12-31"))

# Contagem do tempo.
can_fm <- can_fm %>% 
  mutate(Tempo = as.numeric(difftime(`Data do Óbito`, `Data de Diagnostico`, units = "days")))




df <- can_fm %>% 
  mutate(indice = rank(-Tempo, ties.method = "first")) %>% 
  filter(!(Tempo == '0' | Sexo == "MASCULINO")) %>% 
  select(`Código do Paciente`,`Data de Nascimento`,Idade,`Raca/Cor`,`Grau de Instrução`,`Estado Civil`,
         `Cidade Endereço`,`Código da Topografia`,`Data do Óbito`,`Data de Diagnostico`,Tempo,Status) %>% 
  rename(
    Cod_Paciente = `Código do Paciente`,
    Data_Nasc = `Data de Nascimento`,
    Etnia = `Raca/Cor`,
    Escolaridade = `Grau de Instrução`,
    Estado_Civil = `Estado Civil`,
    Cidade = `Cidade Endereço`,
    Cod_Topografia = `Código da Topografia`,
    Data_Obito = `Data do Óbito`,
    Data_Diagnostico = `Data de Diagnostico`
  )

#-------------------------------------------------------------------------------
#
ui <- dashboardPage(
  dashboardHeader(
    #
    title = tagList(
      # Adiciona a logo
      tags$img(src = 'mama.png', height = '50px', style = 'float: left; margin-right: 10px;'),
      "Câncer de Mama"
    ),
    # Outras opções do cabeçalho, se necessário
    titleWidth = 250
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Geral", tabName = 'geral', icon = icon("dashboard")),
      menuItem("Análise Exploratória", tabName = "AED", icon = icon("dashboard"),
               menuSubItem("Mapa", tabName = "map"),
               menuSubItem("Demográficas", tabName = "demograficas"),
               menuSubItem("Morfológicas", tabName = "morfologicas"),
               menuSubItem("Tempo", tabName = "tempo"),
               menuSubItem("Observação", tabName = "janela-obs"),
               menuSubItem("Tabela", tabName = "tabela")
      ),
      menuItem("Modelo Kaplan-Meier", tabName = "km", icon = icon("line-chart"),
               menuSubItem("Curva de Sobrevida", tabName = "km-geral"),
               menuSubItem("Taxa de Falha", tabName = "km-"),
               menuSubItem("Taxa de Falha Acumulada", tabName = "km-escolaridade")),
      menuItem("Modelos Paramétricos", tabName = "parametric", icon = icon("bar-chart"),
               menuSubItem("Paramétricos", tabName = "exponencial"),
               menuSubItem("Avaliação dos Modelos", tabName = "demograficas"),
               menuSubItem("Diagnóstico dos Modelos", tabName = "demograficas"))
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
                box(title = "Janela de Observação", status = "primary", solidHeader = TRUE, 
                    plotOutput("janela"))
              )
      ),
      
      # 
      tabItem(tabName = "tabela",
              h2("Conjunto de Dados"),
              dataTableOutput("tabela")
        
      ),
      ###################################################
      tabItem(tabName = "km-geral",
              fluidRow(
                tabBox(
                  title = "Curva de Kaplan-Meier",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset2", height = "300px",
                  tabPanel("Geral", plotOutput('km_geral')),
                  tabPanel("Estado Civil", plotOutput('km_estado_civil')),
                  tabPanel("Escolaridade", plotOutput('km_escolaridade')),
                  tabPanel("Etnia", plotOutput('km_etnia'))
                )
              )
      ),
      
     # Modelos Paramétricos
      tabItem(tabName = "exponencial",
              fluidRow(
                tabBox(
                  title = "Modelos Paramétricos",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1", height = "250px",
                  tabPanel("Exponencial", "First tab content"),
                  tabPanel("Weibull", "Tab content 2"),
                  tabPanel("Log-normal", "Tab content 2"),
                  tabPanel("Log-logístico", "Tab content 2")
                )
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
  
  })
  
  output$morfologicas <- renderDataTable({
    # Substitua pelo seu dataset morfológico
    
  })
  
  output$janela<- renderPlot({
    # Substitua pelo seu dataset patológico
    df1 <- df %>% 
      mutate(indice = rank(-Tempo, ties.method = "first"))
    
    
    # Criar o gráfico
    j1 <- ggplot(df1, aes(x = Tempo, y = indice, color = as.factor(Status), label = ifelse(Status == 1, "Falha", "Censura"))) +
      geom_segment(aes(x = 0, xend = Tempo, y = indice, yend = indice), size = 1) +  # Linhas horizontais iniciadas em zero
      geom_point(aes(shape = as.factor(Status)), size = 1) +  # Pontos para eventos
      scale_color_manual(values = c("#8AC", "red"), labels = c("Censura", "Falha")) +
      scale_shape_manual(values = c(16, 16), labels = c("Censura", "Falha")) +  # Define os formatos dos pontos
      labs(
        title = "Tempo de Sobrevida",
        x = "Tempo (dias)",
        y = "Pacientes",
        color = "Status",
        shape = "Status"
      ) +
      theme_minimal() +
      theme(
        legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    print(j1)
    
  })
    #
  output$tabela <- renderDT({
    datatable(df, options = list(
      scrollX = TRUE, # rolagem horizontal
      scrollY = "400px", # Define altura fixa para rolagem vertical
      pageLength = 5, 
      autoWidth = TRUE,
      language = list(
        search = "Pesquisar"
      )))
  })
  
  output$km_geral <- renderPlot({
    fit <- survfit(Surv(Tempo, Status) ~ 1, data = df)
    g1 <- ggsurvplot(
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
    g1$plot <- g1$plot + theme(legend.title = element_blank())
    print(g1)
  })
  
  # 
  output$km_estado_civil <- renderPlot({
    fit <- survfit(Surv(Tempo, Status) ~ Estado_Civil, data = df)
    g2 <- ggsurvplot(
      fit, data = df,
      pval = TRUE, 
      conf.int = FALSE,
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
    g2$plot <- g2$plot + theme(legend.title = element_blank())
    print(g2)
  })
  
  # 
  output$km_escolaridade <- renderPlot({
    fit <- survfit(Surv(Tempo, Status) ~ Escolaridade, data = df)
    g3 <- ggsurvplot(
      fit, data = df,
      pval = TRUE, 
      conf.int = FALSE,
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
    g3$plot <- g3$plot + theme(legend.title = element_blank())
    print(g3)
  })
  
  # 
  output$km_etnia <- renderPlot({
    fit <- survfit(Surv(Tempo, Status) ~ Etnia, data = df)
    g4 <- ggsurvplot(
      fit, data = df,
      pval = TRUE, 
      conf.int = FALSE,
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
    g4$plot <- g4$plot + theme(legend.title = element_blank())
    print(g4)
  })
  
  output$parametric_plot <- renderPlot({
    # Modelo Exponencial
    
    # Modelo Weibull
    
    # Modelo Log-normal
    
    # Modelo Log-logístico
    
    
  })
}

#
shinyApp(ui, server)
