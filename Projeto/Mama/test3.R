library(fresh)
# Create the theme
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#C55888"
  ),
  adminlte_sidebar(
    width = "240px",
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

library(shinydashboard)
library(shiny)

ui <- dashboardPage(
  title = "Câncer de Mama - RO",
  dashboardHeader(
    title = tagList(
      tags$div(
        style = "display: flex; justify-content: center; align-items: center; height: 50px;",  # Reduzindo a altura aqui
        tags$img(src = 'mama.png', height = '40px', style = 'margin-right: 10px;')  # Ajustando o tamanho da imagem
      )),
    dropdownMenu(headerText = "Contato", type = "messages", badgeStatus = "success",
                 messageItem("E-mail", "doug7statistic@gmail.br", icon = icon("envelope"))
    ),
    dropdownMenu(headerText = "Aviso", type = "notifications", badgeStatus = "warning",
                 notificationItem(icon = icon("users"), status = "info", "Sobre Nós"),
                 notificationItem(icon = icon("cog"), status = "info", "Metodologia"))
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
    tags$head(
      tags$style(HTML("
    /* Estilo para o cabeçalho */
    .skin-blue .main-header .logo {
      color: #fff;
      border-bottom: 0 solid transparent;
      height: 100px; /* Mantendo a altura do cabeçalho */
    }
    
    /* Ajustar o navbar */
    .main-header .navbar {
      height: 50px; /* Alinhando a altura do navbar à do cabeçalho */
    }

    /* Ajuste o menu lateral para descer após o cabeçalho */
    .main-sidebar {
      margin-top: 50px; /* Espaçamento para evitar sobreposição */
    }

    /* Estilo do conteúdo */
    .content-wrapper {
      padding-top: 100px; /* Certificar que o conteúdo comece após o cabeçalho */
    }
  ")
                 )
    )
  )
)

server <- function(input, output) {
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Overview", icon = icon("tachometer-alt"))
      
    )
  })
}
shinyApp(ui, server)