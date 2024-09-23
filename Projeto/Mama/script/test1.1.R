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

can_m$`Cidade Endereço` <- str_to_title(can_m$`Cidade Endereço`)
can_p$`Cidade Endereço` <- str_to_title(can_p$`Cidade Endereço`)

# Aplicar ajuste de nomes aos dados
can_m$`Cidade Endereço` <- ajustar_nomes(can_m$`Cidade Endereço`)
can_p$`Cidade Endereço` <- ajustar_nomes(can_p$`Cidade Endereço`)

# Criar dois objetos de dados para os tipos de câncer "Mama" e "Próstata"
cancer_mama <- can_m %>% 
  rename(
    Cidade = `Cidade Endereço`
  )

cancer_prostata <- can_p %>% 
  rename(
    Cidade = `Cidade Endereço`
  )

df_m <- cancer_mama %>% group_by(Cidade) %>% count() %>% arrange(desc(n)) %>% rename(Incidencia = n)
df_p <- cancer_prostata %>% group_by(Cidade) %>% count() %>% arrange(desc(n)) %>% rename(Incidencia = n)

# Juntar os dados geográficos com os dados de incidência de Mama
map_data_mama <- rondonia_map %>%
  left_join(df_m, by = c("NM_MUN" = "Cidade")) %>%
  mutate(Incidencia = ifelse(is.na(Incidencia), 0, Incidencia))

# Juntar os dados geográficos com os dados de incidência de Próstata
map_data_prostata <- rondonia_map %>%
  left_join(df_p, by = c("NM_MUN" = "Cidade")) %>%
  mutate(Incidencia = ifelse(is.na(Incidencia), 0, Incidencia))

# Interface do usuário (UI)
# Continuando do seu código anterior...

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
      tags$script(src = "custom.js") # Inclui o JavaScript
    ),
    
    # Layout com a caixa de seleção à direita do gráfico
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
    ),
    
    # Adicionando o gráfico de ranking abaixo do caixa de seleção
    fluidRow(
      column(
        width = 10,
        box(
          title = "Ranking de Incidência por Cidade",
          status = "primary",
          solidHeader = TRUE,
          width = 8,
          plotOutput("ranking_plot", height = "700px")
        )
      )
    )
  )
)

# Servidor
server <- function(input, output, session) {
  
  # Função para escolher o conjunto de dados baseado no tipo de câncer selecionado
  selected_data <- reactive({
    if (input$tipo_cancer == "Mama") {
      return(map_data_mama)
    } else {
      return(map_data_prostata)
    }
  })
  
  # Carregar e transformar os dados geográficos para WGS84
  shapefile_path <- "C:\\Users\\44735\\Downloads\\RO_Municipios_2022\\RO_Municipios_2022.shp"
  rondonia_map <- st_read(shapefile_path) %>%
    st_transform(crs = 4326)  # Transformar para WGS84
  
  # Renderizar o mapa com base no tipo de câncer selecionado
  output$cancer_map <- renderLeaflet({
    leaflet(data = selected_data()) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~colorNumeric("YlOrRd", Incidencia)(Incidencia),
        weight = 1,  # Peso da borda
        opacity = 10,
        color = "black",  # Cor da borda dos municípios
        dashArray = "",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 3,  # Peso da borda ao destacar
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~paste(NM_MUN, ":", Incidencia),
        layerId = ~NM_MUN  # Identificador do município
      ) %>%
      addLegend(pal = colorNumeric("YlOrRd", selected_data()$Incidencia),
                values = selected_data()$Incidencia, opacity = 0.7,
                title = paste("Incidência de Câncer -", input$tipo_cancer),
                position = "bottomleft")
  })
  
  # Observar cliques nos municípios e aplicar zoom
  observeEvent(input$cancer_map_shape_click, {
    # Capturar o ID do município clicado
    clicked_municipio <- input$cancer_map_shape_click$id
    
    # Filtrar o município clicado para obter suas coordenadas
    municipio_data <- selected_data() %>% filter(NM_MUN == clicked_municipio)
    
    if (nrow(municipio_data) > 0) {
      # Obter as coordenadas do município
      coords <- st_coordinates(municipio_data$geometry)
      lng <- coords[1, 1]  # Longitude
      lat <- coords[1, 2]  # Latitude
      
      # Aplicar zoom ao mapa focando no município clicado
      leafletProxy("cancer_map") %>% setView(lng = lng, lat = lat, zoom = 10)
    }
  })
  
  # Observar cliques nos municípios e aplicar zoom
  observeEvent(input$cancer_map_shape_click, {
    # Capturar o ID do município clicado
    clicked_municipio <- input$cancer_map_shape_click$id
    
    # Filtrar o município clicado para obter suas coordenadas
    municipio_data <- selected_data() %>% filter(NM_MUN == clicked_municipio)
    
    if (nrow(municipio_data) > 0) {
      # Obter as coordenadas do município
      coords <- st_coordinates(municipio_data$geometry)
      lng <- coords[1, 1]  # Longitude
      lat <- coords[1, 2]  # Latitude
      
      # Aplicar zoom ao mapa focando no município clicado
      leafletProxy("cancer_map") %>% setView(lng = lng, lat = lat, zoom = 10)
    }
  })
  
  # Renderizar o gráfico de ranking de incidência
  output$ranking_plot <- renderPlot({
    # Obter dados de incidência
    df_rank <- base %>%
      group_by(`Cidade Endereço`) %>%
      count() %>% 
      arrange(desc(n)) %>% 
      rename(Incidencia = n) %>% 
      arrange(desc(Incidencia))
    
    # Criar o gráfico
    df_rank %>% filter(Incidencia != 0) %>% 
    ggplot(aes(x = fct_reorder(`Cidade Endereço`, Incidencia), y = Incidencia)) +
      geom_col(fill = "#8C0F1A") +
      coord_flip() +
      geom_text(aes(label = round(Incidencia)), hjust = -0.2) +
      xlab("") + 
      ylab("Número de Incidência")
  })
}

# Executar o aplicativo
shinyApp(ui, server)
