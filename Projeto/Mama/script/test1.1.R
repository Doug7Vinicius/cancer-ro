# Carregar pacotes necessários
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
#library(shiny.semantic)
#library(semantic.dashboard)
library(readr)
library(DT)
library(plotly)
library(sf)
library(leaflet)

# Importar os dados
base <- read_delim("~/PROJETOS/CANCER/Projeto/Mama/dataset/base_nao_identificada_2864.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(encoding = "Latin1"), 
                   trim_ws = TRUE)

# Resumo da estrutura dos dados
glimpse(base)

# Verificar duplicidades
duplicados <- base %>% 
  group_by(`Código do Paciente`) %>% 
  count() %>% 
  arrange(desc(n))

# Retirar pacientes com duplicidades
base <- base %>% 
  filter(!`Código do Paciente` %in% c(1125273, 1319524, 1322075))

# Carregar dados geográficos (shapefile de Rondônia)
shapefile_path <- "C:\\Users\\44735\\Downloads\\RO_Municipios_2022\\RO_Municipios_2022.shp"
rondonia_map <- st_read(shapefile_path)

# Função para ajustar nomes de municípios
ajustar_nomes <- function(nome) {
  nome <- gsub("Do Oeste", "D'Oeste", nome, ignore.case = TRUE)
  nome <- gsub("D'oeste", "D'Oeste", nome, ignore.case = TRUE)
  nome <- gsub("do oeste", "D'Oeste", nome, ignore.case = TRUE)
  return(nome)
}

# Ajustar nomes e preparar os dados
can_m <- base %>% filter(str_detect(`Código da Topografia`, "C50"))
can_p <- base %>% filter(str_detect(`Código da Topografia`, "C61"))



can_m$`Cidade Endereço` <- str_to_title(can_m$`Cidade Endereço`)
can_p$`Cidade Endereço` <- str_to_title(can_p$`Cidade Endereço`)

can_m$`Cidade Endereço` <- str_replace_all(can_m$`Cidade Endereço`, "\\b(Do|Dos|De)\\b", function(m) tolower(m))
can_p$`Cidade Endereço` <- str_replace_all(can_p$`Cidade Endereço`, "\\b(Do|Dos|De)\\b", function(m) tolower(m))

# Aplicar ajuste de nomes
can_m$`Cidade Endereço` <- ajustar_nomes(can_m$`Cidade Endereço`)
can_p$`Cidade Endereço` <- ajustar_nomes(can_p$`Cidade Endereço`)

rondonia_map$NM_MUN <- ajustar_nomes(rondonia_map$NM_MUN)

# Contar incidências por cidade
df_m <- can_m %>% group_by(`Cidade Endereço`) %>% count() %>% rename(Cidade = `Cidade Endereço`, Incidencia = n)
df_p <- can_p %>% group_by(`Cidade Endereço`) %>% count() %>% rename(Cidade = `Cidade Endereço`,Incidencia = n)

# Juntar dados geográficos com incidências de Mama e Próstata
map_data_mama <- rondonia_map %>%
  left_join(df_m, by = c("NM_MUN" = "Cidade")) %>%
  mutate(Incidencia = ifelse(is.na(Incidencia), 0, Incidencia))

map_data_prostata <- rondonia_map %>%
  left_join(df_p, by = c("NM_MUN" = "Cidade")) %>%
  mutate(Incidencia = ifelse(is.na(Incidencia), 0, Incidencia))

# Interface do usuário (UI)
ui <- dashboardPage(
  title = 'Câncer Mama - RO',
  header = dashboardHeader(title = ""),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = 'home', icon = icon("dashboard")),
      menuItem("Cenário", tabName = "cenario", icon = icon("dashboard"),
               menuSubItem("Mapa", tabName = "map"))
    )
  ),
  
  dashboardBody(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$script(src = "custom.js"),
    tags$style(HTML("
      .skin-blue .main-header {
        background-color: #C55888 !important;
      }
      .skin-blue .main-header .logo {
        background-color: #C55888 !important;
        color: white !important;
      }
      .skin-blue .main-header .navbar {
        background-color: #C55888 !important;
      }
      .box-header {
        background-color: #C55888 !important;
        color: white !important;
      }
      .box {
        border-color: #C55888 !important;
      }
    ")),
    
    
    
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                column(width = 12,
                       box(title = "Apresentação do Projeto: Câncer de Mama em Rondônia",
                           status = "primary", solidHeader = TRUE,
                           "Este projeto visa analisar a incidência de câncer de mama no estado de Rondônia, 
                            utilizando dados coletados entre 2015 e 2017. O objetivo principal é identificar 
                            padrões geográficos de incidência e fornecer insights que possam auxiliar na 
                            formulação de políticas públicas de saúde.",
                           br(), br(),
                           "Através de uma interface interativa, os usuários podem explorar os dados por meio de mapas 
                            e gráficos, permitindo uma visualização clara das informações.",
                           br(), br(),
                           "Os dados foram obtidos de fontes oficiais e foram tratados para garantir sua qualidade. 
                            A análise inclui a criação de mapas de incidência por município, além de comparações entre 
                            diferentes tipos de câncer.",
                           br(), br(),
                           "Esperamos que este trabalho contribua para o entendimento e o combate ao câncer de mama 
                            em Rondônia, facilitando a identificação de áreas prioritárias para intervenções de saúde 
                            pública."
                       )
                )
              )
      ),
      
      tabItem(tabName = "map", 
              fluidRow(
                column(
                  width = 7,
                  box(
                    title = "Incidência de Câncer de Mama no Estado de Rondônia", 
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    height = 800,
                    leafletOutput("cancer_map", height = "700px")
                  )
                )
                
            ),
            column(
              width = 5,
              box(
                title = "Indicadores de Câncer de Mama",
                status = "primary",
                solidHeader = TRUE,
                width = 10,
                valueBoxOutput("totalPatients"),
                valueBoxOutput("totalDeaths"),
                valueBoxOutput("survivorPercentage")
              )
            )
      
      )
    )
  )
)

# Servidor
server <- function(input, output, session) {
  
  # Exemplo de dados resumidos
  output$data_summary <- DT::renderDataTable({
    base %>% 
      select(`Código do Paciente`, `Cidade Endereço`, `Código da Topografia`) %>%
      head(10)  # Exibir apenas as 10 primeiras linhas
  })
  
  # Exemplo de gráfico de incidência
  output$incidence_plot <- plotly::renderPlotly({
    df_plot <- can_m %>% 
      group_by(`Cidade Endereço`) %>% 
      summarise(Incidencia = n())
    
    plot_ly(data = df_plot, x = ~`Cidade Endereço`, y = ~Incidencia, type = 'bar') %>%
      layout(title = "Incidência de Câncer de Mama por Cidade",
             xaxis = list(title = "Cidade"),
             yaxis = list(title = "Incidência"))
  })
  
  
  selected_data <- reactive({
    if (input$tipo_cancer == "Mama") {
      map_data_mama
    } else {
      map_data_prostata
    }
  })
  
  
  output$totalPatients <- renderValueBox({
    total_patients <- nrow(df)  # Total de pacientes
    valueBox(total_patients, "Total de Pacientes", icon = icon("user"))
  })
  
  output$totalDeaths <- renderValueBox({
    total_deaths <- sum(df$`Status` == 1)  # Supondo que 'Status' indica falecimentos
    valueBox(total_deaths, "Total de Mortes", icon = icon("skull"))
  })
  
  output$survivorPercentage <- renderValueBox({
    total_patients <- nrow(df)
    total_deaths <- sum(df$`Status` == 1)
    survivor_percentage <- (total_patients - total_deaths) / total_patients * 100
    valueBox(round(survivor_percentage, 2), "Percentual de Sobreviventes", icon = icon("heartbeat"), color = "green")
  })
  
  # Renderizar o mapa
  output$cancer_map <- renderLeaflet({
    df_map <- map_data_mama  # Apenas câncer de mama
    
    # Criar categorias para incidências
    df_map$incidencia_cat <- cut(df_map$Incidencia,
                                 breaks = c(0, 10, 50, 100, 150, Inf),
                                 labels = c("0 - 10", "11 - 50", "51 - 100", "101 - 150", " > 151"),
                                 include.lowest = TRUE)
    
    # Definindo cores para cada classe com a paleta de Outubro Rosa
    pal <- colorFactor(palette = c("#FBB4BA", "#F28EAC", "#F867A2", "#C61A8A", "#953495"), 
                       domain = df_map$incidencia_cat)
    
    leaflet(data = df_map) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(df_map$incidencia_cat),
        color = "black",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.9,
        label = ~paste(NM_MUN, ": ", Incidencia),
        labelOptions = labelOptions(style = list("font-size" = "15px")),
        highlightOptions = highlightOptions(weight = 3, color = "#666")
      ) %>%
      addLegend(pal = pal,
                values = df_map$incidencia_cat,
                title = "Câncer de Mama (2015-2017)",
                position = "bottomleft",
                labFormat = labelFormat()) %>%
      addScaleBar(position = "bottomright", 
                  options = scaleBarOptions(maxWidth = 100, 
                                            metric = TRUE, 
                                            imperial = FALSE))
  })
}

# Executar o aplicativo
shinyApp(ui, server)
