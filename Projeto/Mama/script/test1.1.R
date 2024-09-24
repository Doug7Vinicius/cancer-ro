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

# Aplicar ajuste de nomes
can_m$`Cidade Endereço` <- ajustar_nomes(can_m$`Cidade Endereço`)
can_p$`Cidade Endereço` <- ajustar_nomes(can_p$`Cidade Endereço`)

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
  dashboardHeader(
    title = tagList(
      tags$img(src = 'cancer.png', height = '50px', style = 'float: left; margin-right: 10px;'),
      "Distribuição de Câncer por Município"
    ),
    titleWidth = 250
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = 'geral', icon = icon("dashboard")),
      menuItem("Cenário", tabName = "cenario", icon = icon("dashboard"),
               menuSubItem("Mapa", tabName = "map"),
               menuSubItem("Demográficas", tabName = "demograficas")
      )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$script(src = "custom.js")
    ),
    
    fluidRow(
      column(
        width = 7,
        box(
          title = "Mapa de Incidência de Câncer no Estado de Rondônia", 
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          height = 800,
          leafletOutput("cancer_map", height = "700px")
        )
      ),
      
      column(
        width = 4,
        box(
          title = "Tipo de Câncer",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          selectInput("tipo_cancer", "Tipo de Câncer", choices = c("Mama", "Prostata"), selected = "Mama")
        )
      )
    )
  )
)

# Servidor
server <- function(input, output, session) {
  
  selected_data <- reactive({
    if (input$tipo_cancer == "Mama") {
      map_data_mama
    } else {
      map_data_prostata
    }
  })
  
  # Renderizar o mapa
  output$cancer_map <- renderLeaflet({
    df_map <- selected_data()
    
    # Criar categorias para incidências
    df_map$incidencia_cat <- cut(df_map$Incidencia,
                                 breaks = c(0, 1, 10, 50, 100, Inf),
                                 labels = c("0-0", "1-10", "11-50", "51-100", "101+"),
                                 include.lowest = TRUE)
    
    # Definindo cores para cada classe com a paleta de Outubro Rosa
    pal <- colorFactor(palette = c("#F5A6B3", "#D5006D", "#A30037", "#F28EAC", "#C9B3D5", "#6D4C6C"), 
                       domain = df_map$incidencia_cat)
    
    leaflet(data = df_map) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(df_map$incidencia_cat),
        color = "black",  # Contornos dos municípios
        weight = 1,
        opacity = 0.7,
        fillOpacity = 0.5,
        label = ~paste(NM_MUN, ": ", Incidencia),
        highlightOptions = highlightOptions(weight = 3, color = "#666")
      ) %>%
      addLegend(pal = pal,
                values = df_map$incidencia_cat,
                title = "Câncer de Mama (2015-2017)",
                position = "bottomleft",
                labFormat = labelFormat(prefix = "Incidência: "))
  })
}

# Executar o aplicativo
shinyApp(ui, server)
