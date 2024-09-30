library(fresh)
# Create the theme
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#C55888"
  ),
  adminlte_sidebar(
    width = "250px",
    dark_bg = "#D8DEE9",
    dark_hover_bg = "#81A1C1",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#D8DEE9", 
    info_box_bg = "#D8DEE9"
  )
)



#"#FBB4BA", "#F28EAC", "#F867A2", "#C61A8A", "#953495"

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(
    title = tagList(
      tags$div(
        style = "display: flex; justify-content: center; align-items: center; height: 100%;",
        tags$img(src = 'mama.png', height = '100px', style = 'margin-right: 10px;')
    ))
    ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Análise de Sobrevida", tabName = 'geral', icon = icon("heart")),
      menuItem("Dados do INCA-RO", tabName = 'inca', icon = icon("database")),
      menuItem("Cenário", tabName = "cenario", icon = icon("globe"),
               menuSubItem("Mapa", tabName = "map", icon = icon("map")),
               menuSubItem("Demográficas", tabName = "demograficas", icon = icon("users")),
               menuSubItem("Morfológicas", tabName = "morfologica", icon = icon("microscope")),
               menuSubItem("Tabela", tabName = "tabela", icon = icon("table"))
      ),
      menuItem("Análise Exploratória", tabName = 'AED', icon = icon("binoculars"),
               menuSubItem("Tempo", tabName = "tempo", icon = icon("clock")),
               menuSubItem("Tabela", tabName = "tabela1", icon = icon("th")),
               menuSubItem("Observação", tabName = "janela-obs", icon = icon("eye"))
      ),
      menuItem("Modelo Kaplan-Meier", tabName = "km", icon = icon("chart-line"),
               menuSubItem("Curva de Sobrevida", tabName = "km-geral", icon = icon("line-chart")),
               menuSubItem("Taxa de Falha", tabName = "taxa-falha", icon = icon("exclamation-triangle")),
               menuSubItem("Taxa de Falha Acumulada", tabName = "taxa-acumulada", icon = icon("chart-bar"))
      ),
      menuItem("Modelos Paramétricos", tabName = "parametricos", icon = icon("cogs"),
               menuSubItem("Paramétricos", tabName = "Dist", icon = icon("poll")),
               menuSubItem("Avaliação dos Modelos", tabName = "Aval", icon = icon("check")),
               menuSubItem("Diagnóstico do Modelo", tabName = "Diag", icon = icon("wrench"))
      )
      
    )
    ),
  dashboardBody(
    
    use_theme(mytheme), # <-- use the theme
    
    tags$style(HTML("
    .sidebar-menu .treeview-menu>li>a {
      color: #2E3440 !important; /* Cor do texto */
    }
    .sidebar-menu .treeview-menu>li>a:hover {
      color: #2E3440 !important; /* Cor do texto ao passar o mouse */
    }
    .sidebar-menu .treeview-menu>li.active>a {
      color: #2E3440 !important; /* Cor do texto quando ativo */
    }
  ")),
    
    
    fluidRow(
      column(width = 4,
             box(
               title = "Box title", width = NULL, status = "primary",
               "Box content"
             ),
             box(
               title = "Title 1", width = NULL, solidHeader = TRUE, status = "primary",
               "Box content"
             ),
             box(
               width = NULL, background = "black",
               "A box with a solid black background"
             )
      ),
      
      column(width = 4,
             box(
               status = "warning", width = NULL,
               "Box content"
             ),
             box(
               title = "Title 3", width = NULL, solidHeader = TRUE, status = "warning",
               "Box content"
             ),
             box(
               title = "Title 5", width = NULL, background = "light-blue",
               "A box with a solid light-blue background"
             )
      ),
      
      column(width = 4,
             box(
               title = "Title 2", width = NULL, solidHeader = TRUE,
               "Box content"
             ),
             box(
               title = "Title 6", width = NULL, background = "maroon",
               "A box with a solid maroon background"
             )
      )
    )
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)