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



#"#FBB4BA", "#F28EAC", "#F867A2", "#C61A8A", "#953495"



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
library(shinydashboardPlus)
#library(shiny.semantic)
#library(semantic.dashboard)
library(readr)
library(DT)
library(plotly)
library(sf)
library(leaflet)

ui <- dashboardPage(
  title = "Câncer de Mama - RO",
  dashboardHeader(
    title = tagList(
      tags$div(
        style = "display: flex; justify-content: center; align-items: center; height: 100%;",
        tags$img(src = 'mama.png', height = '80px', style = 'margin-right: 10px;')
    )),
    #
    dropdownMenu(headerText="Contato", type = "messages", badgeStatus = "success",
                 messageItem("E-mail", "doug7statistic@gmail.br", icon = icon("envelope"))
    ),
    
    dropdownMenu(headerText="Aviso", type = "notifications", badgeStatus = "warning",
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
    use_theme(mytheme),
    tags$head(
      tags$style(".skin-blue .main-header .logo {
                    color: #fff;
                    border-bottom: 0 solid transparent;
                    height: 100px;
                    }
      "),
      
      tags$style(".main-header .navbar{
                    max-height: 7px;
                    }
                 ")),
    tags$style(HTML("
    .sidebar-menu .treeview-menu>li>a {
      color: #2E3440 !important; /* Cor do texto */
    }
    .sidebar-menu .treeview-menu>li>a:hover {
      color: #81A1C1 !important; /* Cor do texto ao passar o mouse */
    }
    .sidebar-menu .treeview-menu>li.active>a {
      color: #FFF !important; /* Cor do texto quando ativo */
      background-color: #81A1C1 !important; /* Cor de fundo quando ativo */
    }
    

  ")),
    
    
    tabItems(
      
      tabItem(tabName = "inca",
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
      ),
      
      # Aba Geral
        tabItem(tabName = "janela-obs",
                fluidRow(
                  box(title = "Janela de Observação", 
                      status = "primary", 
                      solidHeader = TRUE,
                      height = 800,
                      width = 7,
                      plotlyOutput("janela", height = "700px"))
                )
        ),
        
        
        ## Menu Análise Exploratória
        
        # 
        tabItem(tabName = "tabela",
                h2("Conjunto de Dados"),
                dataTableOutput("tabela")
                
        ),
        #
        tabItem(tabName = "km-geral",
                fluidRow(
                  tabBox(
                    title = "Curvas de Kaplan-Meier",
                    id = "tabset2", 
                    height = "400px",
                    width = 7,
                    tabPanel("Geral", plotOutput('km_geral', height = "700px")),
                    tabPanel("Estado Civil", plotOutput('km_estado_civil', height = "700px")),
                    tabPanel("Escolaridade", plotOutput('km_escolaridade', height = "700px")),
                    tabPanel("Etnia", plotOutput('km_etnia', height = "700px")),
                    tabPanel("Idade", plotlyOutput('km_idade', height = 700))
                  )
                )
        ),
        
        ##
        tabItem(tabName = "taxa-falha",
                fluidRow(
                  tabBox(
                    title = "Taxa de Falha",
                    id = "tabset3", 
                    height = "400px",
                    width = 7,
                    tabPanel("Geral", plotOutput('tx_geral', height = "400px")),
                    tabPanel("Estado Civil", plotOutput('tx_estado_civil')),
                    tabPanel("Escolaridade", plotOutput('tx_escolaridade', height = "800px")),
                    tabPanel("Etnia", plotOutput('tx_etnia'))
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
                
              )
              
      ),
      
      tabItem(tabName = "Dist",
            fluidRow(
              box(
                title = "Modelos Paramétricos",
                status = "primary",
                width = 8,
                height = 800,
                solidHeader = TRUE,
                tabPanel("Dist", plotlyOutput("parametric", height = "740px")))
            )
      )
    )
  )
)

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
    
    leaflet(data = df_map, options = leafletOptions(scrollWheelZoom = FALSE, zoomControl = FALSE)) %>%
      addTiles() %>%
      setView(lng = -63.3, lat = -10.8, zoom = 7.4) %>% 
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
                                            imperial = FALSE))%>%
      htmlwidgets::onRender("
    function(el, x) {
      var map = this;
      map.dragging.disable();  // Desabilita o arrasto (dragging)
      map.touchZoom.disable(); // Desabilita zoom por toque
      map.doubleClickZoom.disable(); // Desabilita zoom por duplo clique
      map.scrollWheelZoom.disable(); // Desabilita zoom por rolagem do mouse
    }
  ")
  })
  
  
  
  
  
  
  
  
  
  
  output$demograficas <- renderDataTable({
    # Substitua pelo seu dataset demográfico
    
  })
  
  output$morfologica <- renderPlot({
    # Substitua pelo seu dataset morfológico
    m1 <- plot_bar(df %>%
                     select(Estado_Civil, Escolaridade, Etnia, Status),
                   theme_config=list(text = element_text(size = 10)),
                   ncol = 2,
                   order_bar=TRUE) + geom_text(stat='count', aes(label=..count..), vjust=-0.5, size=3)
    print(m1)
  })
  
  output$janela <- renderPlotly({
    # Substitua pelo seu dataset patológico
    df1 <- df %>% 
      mutate(indice = rank(-Tempo, ties.method = "first"))
    
    # Criar o gráfico usando ggplot
    j1 <- ggplot(df1, aes(x = Tempo, y = indice, color = as.factor(Status), label = ifelse(Status == 1, "Falha", "Censura"))) +
      geom_segment(aes(x = 0, xend = Tempo, y = indice, yend = indice), size = 1) +  # Linhas horizontais iniciadas em zero
      geom_point(aes(shape = as.factor(Status)), size = 0) +  # Pontos para eventos
      scale_color_manual(values = c("#8AC", "red"), labels = c("Censura", "Falha")) +
      scale_shape_manual(values = c(16, 16), labels = c("Censura", "Falha")) +  # Define os formatos dos pontos
      labs(
        title = "",
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
    
    # Converter para plotly para interatividade
    ggplotly(j1)
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
  
  
  
  # Função de Risco.
  output$tx_geral <- renderPlot({
    #
    hazard_geral <- -log(fit1$surv) / fit1$time
    #
    h1 <- plot(fit1$time, hazard_geral, type = "l", xlab = "Tempo", ylab = "Taxa de Falha")
    
    # Função de Risco Acumulado.
    
    
  })
  
  ## Menu Modelo Kaplan-Meier
  #
  fit1 <- survfit(Surv(Tempo, Status) ~ 1, data = df)
  output$km_geral <- renderPlot({
    
    g1 <- ggsurvplot(
      fit1, data = df,
      pval = TRUE, 
      conf.int = TRUE,
      xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
      break.time.by = 100,
      ggtheme = theme_light(),
      ylim = c(0.8,1),
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
  fit2 <- survfit(Surv(Tempo, Status) ~ Estado_Civil, data = df)
  output$km_estado_civil <- renderPlot({
    
    g2 <- ggsurvplot(
      fit2, data = df,
      pval = TRUE, 
      conf.int = FALSE,
      xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
      break.time.by = 100,
      ggtheme = theme_light(),
      ylim = c(0.8,1),
      xlim = c(0,1400)
      # risk.table = "abs_pct",
      # risk.table.y.text.col = TRUE,
      # risk.table.y.text = TRUE,
      # ncensor.plot = TRUE,
    )
    g2$plot <- g2$plot + theme(legend.title = element_blank())
    print(g2)
    
  })  
  # Função de Risco.
  output$tx_estado_civil <- renderPlot({
    #
    hazard_estado_civil <- -log(fit2$surv) / fit2$time
    #
    h2 <- plot(fit2$time, hazard_estado_civil, type = "l", xlab = "Tempo", ylab = "Taxa de Falha")
    
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
      ylim = c(0.75,1),
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
      ylim = c(0.8,1),
      xlim = c(0,1400)
      # risk.table = "abs_pct",
      # risk.table.y.text.col = TRUE,
      # risk.table.y.text = TRUE,
      # ncensor.plot = TRUE,
    )
    g4$plot <- g4$plot + theme(legend.title = element_blank())
    print(g4)
  })
  
  # 
  output$km_idade <- renderPlotly({
    # Ajuste do modelo Kaplan-Meier
    fit <- survfit(Surv(Tempo, Status) ~ faixa_etaria, data = df)
    
    # Criação do gráfico Kaplan-Meier com ggsurvplot
    g5 <- ggsurvplot(
      fit, data = df,
      pval = TRUE, 
      conf.int = FALSE,
      xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
      break.time.by = 100,
      ggtheme = theme_light(),
      ylim = c(0.8, 1),
      xlim = c(0, 1400)
      # risk.table = "abs_pct",
      # risk.table.y.text.col = TRUE,
      # risk.table.y.text = TRUE,
      # ncensor.plot = TRUE,
    )
    
    # Ajuste adicional no tema do gráfico
    g5$plot <- g5$plot + theme(legend.title = element_blank())
    
    # Conversão do gráfico para um objeto plotly
    ggplotly(g5$plot)
  })
  
  
  
  
  
  
  
  ## Menu Modelos Paramétricos
  #
  fit_overall = survfit(Surv(Tempo, Status) ~ 1, data = df)
  
  # Distribuição Exponencial
  ajust1 <- survreg(Surv(Tempo, Status) ~ 1, dist = 'exponential', data = df)
  ajust1
  
  alpha <- exp(ajust1$coefficients[1])
  alpha
  
  ajust2 <- survreg(Surv(Tempo, Status) ~ 1, dist = 'weibull', data = df)
  ajust2
  
  alpha <- exp(ajust2$coefficients[1])
  alpha
  gama <- 1/ajust2$scale
  gama
  cbind(gama, alpha)
  
  # Distribuição Log-normal
  ajust3 <- survreg(Surv(Tempo, Status) ~ 1, dist = 'lognorm', data = df)
  ajust3
  
  # Distribuição Log-logístico
  ajust4 <- survreg(Surv(Tempo, Status) ~ 1, dist = 'loglogistic', data = df)
  ajust4
  
  intercept4 <- ajust4$coefficients[1]
  scale4 <- ajust4$scale
  
  # Distribuição Gompertz
  ajust5 <- flexsurvreg(Surv(Tempo, Status) ~ 1, dist = 'gompertz', data = df)
  
  intercept5 <- ajust5$coefficients[1]  # Localização (Intercepto)
  scale5 <- ajust5$scale  # Parâmetro de forma
  
  lambda5 <- exp(ajust5$coefficients[1])  # Converte o intercepto para o parâmetro de taxa
  theta5 <- exp(ajust5$coefficients[2])  # O inverso da escala é o parâmetro de forma
  
  
  
  # Calcular as funções de sobrevivência
  time <- fit_overall$time
  st <- fit_overall$surv
  ste <- exp(-time / 7674.815)
  stw <- exp(-(time / 13143.86) ^ 0.8021454)
  stln <- pnorm((-log(time) + 10.03029) / 2.633146)
  stlog <- 1 / (1 + (exp(-intercept4) * time)^(1 / scale4))
  #stgom <- (scale5*exp(intercept5*time)) * (exp((-scale5/intercept5)* (exp(intercept5*time) - 1)))
  
  # Criar um dataframe
  data <- data.frame(
    time = time,
    st = st,
    ste = ste,
    stw = stw,
    stln = stln,
    stlog = stlog
    #stgom = stgom
  )
  
  output$parametric <- renderPlotly({
    # Criar o gráfico com todas as linhas em ggplot2
    p1 <- ggplot(data, aes(x = time)) +
      geom_step(aes(y = st), color = "black", size = 0.25, linetype = "solid") +  # Kaplan-Meier
      geom_line(aes(y = ste, color = "Exponencial"), linetype = "dashed") +
      geom_line(aes(y = stw, color = "Weibull"), linetype = "dashed") +
      geom_line(aes(y = stln, color = "Log-normal"), linetype = "dashed") +
      geom_line(aes(y = stlog, color = "Log-logístico"), linetype = "dashed") +
      #geom_line(aes(y = stgom, color = "Gompertz"), linetype = "dashed") +
      labs(title = "",
           x = "Tempo em dias",
           y = "Probabilidade de Sobrevivência",
           color = "Modelos") +
      ylim(0.85, 1) +
      scale_color_manual(values = c(
        "Exponencial" = "#1f77b4",   # Azul
        "Weibull" = "#ff7f0e",       # Laranja
        "Log-normal" = "#2ca02c",    # Verde
        "Log-logístico" = "#d62728" # Vermelho
      #  "Gompertz" = "#9467bd"       # Roxo
      )) +
      theme_minimal()
    
    # Converter para plotly
    ggplotly(p1)
  })
  
}

shinyApp(ui, server)