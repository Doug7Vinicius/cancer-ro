library(shinydashboard)
library(shiny)
ui <- dashboardPage(
  dashboardHeader(title = tags$img(src='mama.png', height = '120px', width ='80px')),
  dashboardSidebar(
    sidebarMenuOutput("menu")
  ),
  
  dashboardBody(
    tags$head(
      tags$style(".skin-blue .main-header .logo {
color: #fff;
border-bottom: 0 solid transparent;
height: 125px;
}"),
      tags$style(".skin-blue .sidebar a {
    color: #b8c7ce;
    padding-top: 50%;
}"), 
      tags$style(".main-header .navbar{
           max-height: 500px;")
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