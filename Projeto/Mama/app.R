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
library(sf)
library(leaflet)

#


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
  mutate(across(c(`Data de Nascimento`,`Data do Óbito`,
                  `Data de Último Contato`,`Data de Diagnostico`), ~ as.Date(., format = "%d/%m/%Y")))

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


df <- df %>%
  filter(!((Estado_Civil == "SEM INFORMAÇÃO") | 
             (Escolaridade == "SEM INFORMAÇÃO") | 
             (Etnia == "SEM INFORMAÇÃO") | 
             (Cidade == "SEM INFORMAÇÃO")))



df <- df %>%
  mutate(Etnia = case_when(
    Etnia == "PARDA" ~ "PARDA",
    Etnia == "BRANCO" ~ "BRANCO",
    TRUE ~ "OUTROS"
  ))

df <- df %>%
  mutate(Estado_Civil = case_when(
    Estado_Civil == "CASADO" ~ "CASADO",
    TRUE ~ "OUTROS"
  ))

df <- df %>%
  mutate(Escolaridade = case_when(
    Escolaridade == "FUNDAMENTAL I (1ª A 4ª SÉRIE)" ~ "FUNDAMENTAL I (1ª A 4ª SÉRIE)",
    Escolaridade == "FUNDAMENTAL II (5ª A 8ª SÉRIE)" ~ "FUNDAMENTAL II (5ª A 8ª SÉRIE)",
    Escolaridade == "MÉDIO (ANTIGO SEGUNDO GRAU)" ~ "MÉDIO (ANTIGO SEGUNDO GRAU)",
    TRUE ~ "OUTROS"
  ))


colSums(is.na(df))

df <- df %>%
  mutate(faixa_etaria = case_when(
    Idade < 40 ~ "< 40 anos",
    Idade >= 40 & Idade <= 49 ~ "40 a 49 anos",
    Idade >= 50 & Idade <= 59 ~ "50 a 59 anos",
    Idade >= 60 & Idade <= 69 ~ "60 a 69 anos",
    Idade >= 70 ~ ">= 70 anos"
  ))

df <- df %>%
  mutate(Subject_ID = row_number())

df <- df %>%
  mutate(Event_Category = ifelse(Status == 1, "Falha", "Censura"))

# Criar um gráfico com janelas de observação
c1 <- ggplot(df, aes(y = Subject_ID)) +
  geom_segment(aes(x = df$Data_Diagnostico, xend = df$Data_Obito, y = Subject_ID, yend = Subject_ID, color = Event_Category), size = 1) +
  scale_color_manual(values = c("Falha" = "lightblue", "Censura" = "lightpink")) +
  labs(x = "Data", y = "Pacientes - Id",
       title = "Calendário de Observação por Paciente") +
  theme_classic() +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(t1) %>% 
  layout(dragmode = "zoom") %>% 
  config(displayModeBar = TRUE, 
         modeBarButtons = c(
           "resetScale","zoom","pan"), 
         displaylogo = FALSE)


###############################
#-------------------------------------------------------------------------------
can_m <- base %>% filter(str_detect(`Código da Topografia`, "C50"))

can_m$`Cidade Endereço` <- str_to_title(can_m$`Cidade Endereço`)

can_m$`Cidade Endereço` <- str_replace_all(can_m$`Cidade Endereço`, "\\b(Do|Dos|De)\\b", function(m) tolower(m))

can_m$`Cidade Endereço` <- ajustar_nomes(can_m$`Cidade Endereço`)

rondonia_map$NM_MUN <- ajustar_nomes(rondonia_map$NM_MUN)

# Contar incidências por cidade
df_m <- can_m %>% group_by(`Cidade Endereço`) %>% count() %>% rename(Cidade = `Cidade Endereço`, Incidencia = n)

# Juntar dados geográficos com incidências de Mama e Próstata
map_data_mama <- rondonia_map %>%
  left_join(df_m, by = c("NM_MUN" = "Cidade")) %>%
  mutate(Incidencia = ifelse(is.na(Incidencia), 0, Incidencia))

#-------------------------------------------------------------------------------

# Carregar dados geográficos (shapefile de Rondônia)
shapefile_path <- "C:\\Users\\44735\\Downloads\\RO_Municipios_2022\\RO_Municipios_2022.shp"
rondonia_map <- st_read(shapefile_path)

#
base1$Cidade <- str_to_title(base1$Cidade)

ajustar_nomes <- function(nome) {
  nome <- gsub("Do Oeste", "D'Oeste", nome, ignore.case = TRUE)
  nome <- gsub("D'oeste", "D'Oeste", nome, ignore.case = TRUE)
  nome <- gsub("do oeste", "D'Oeste", nome, ignore.case = TRUE)
  return(nome)
}

base1$Cidade <- ajustar_nomes(base1$Cidade)

# Dados fictícios de incidência de câncer por município
cancer_data <- data.frame(
  Municipio = base1$Cidade,
  Incidencia = base1$n
)


# Juntar os dados geográficos com os dados de incidência
map_data <- rondonia_map %>%
  left_join(cancer_data, by = c("NM_MUN" = "Municipio")) %>%
  mutate(Incidencia = ifelse(is.na(Incidencia), 0, Incidencia))  # Substituir NA por 0

#####
##### Frontend
#-------------------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(
    #
    title = tagList(
      # Adiciona a logo
      tags$img(src = 'mama.png', height = '50px', style = 'float: left; margin-right: 10px;'),
      ""
    ),
    # Outras opções do cabeçalho, se necessário
    titleWidth = 250,
    
    tags$li(class = "dropdown", style = "background-color: #C55888;")  # Cor rosa
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Análise de Sobrevida", tabName = 'geral', icon = icon("dashboard")),
      menuItem("Dados do INCA-RO", tabName = 'geral', icon = icon("dashboard")),
      menuItem("Cenário", tabName = "cenario", icon = icon("dashboard"),
               menuSubItem("Mapa", tabName = "map"),
               menuSubItem("Demográficas", tabName = "demograficas"),
               menuSubItem("Morfológicas", tabName = "morfologica"),
               menuSubItem("Tabela", tabName = "tabela")
      ),
      menuItem("Análise Exploratória", tabName = 'AED', icon = icon("dashboard"),
               menuSubItem("Tempo", tabName = "tempo"),
               menuSubItem("Observação", tabName = "janela-obs")),
      menuItem("Modelo Kaplan-Meier", tabName = "km", icon = icon("line-chart"),
               menuSubItem("Curva de Sobrevida", tabName = "km-geral"),
               menuSubItem("Taxa de Falha", tabName = "taxa-falha"),
               menuSubItem("Taxa de Falha Acumulada", tabName = "taxa-acumulada")),
      menuItem("Modelos Paramétricos", tabName = "parametricos", icon = icon("bar-chart"),
               menuSubItem("Paramétricos", tabName = "Dist"),
               menuSubItem("Avaliação dos Modelos", tabName = "Aval"),
               menuSubItem("Diagnóstico dos Modelos", tabName = "Diag"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$script(src = "custom.js"),
      tags$style(HTML("
      .skin-blue .main-header {
        background-color: #C55888;  /* Cor de fundo rosa */
      }
      .skin-blue .main-header .logo {
        background-color: #C55888; /* Logo rosa */
        color: white; /* Cor do texto */
      }
      .skin-blue .main-header .navbar {
        background-color: #C55888; /* Navbar rosa */
      }
      .box-header {
        background-color: #C55888; /* Cor rosa clara para os boxes */
        color: white; /* Cor do título */
      }
    "))
    ),
    
    #
    tabItems(
      tabItem(tabName = "morfologica",
              fluidRow(
                box(title = "Freq",
                    status = "primary",
                    solidHeader = TRUE,
                    height = 700,
                    width = 12,
                    plotOutput("morfologica", height = "700px"))
              ))
    ),
    
    #
    tabItems(
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
      
      
      #
      tabItem(tabName = "map",
              fluidRow(
                box(title = "Mapa de Incidência de Câncer no Estado de Rondônia", 
                    status = "primary",
                    width = 8,
                    height = 800,
                    solidHeader = TRUE,
                    leafletOutput("cancer_map", height = "740px")),
                br(),
                textOutput("selected_municipio")
              )),
      

      
      # Menu Análise Exploratória
      
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
                  tabPanel("Geral", plotOutput('km_geral', height = "400px")),
                  tabPanel("Estado Civil", plotOutput('km_estado_civil')),
                  tabPanel("Escolaridade", plotOutput('km_escolaridade')),
                  tabPanel("Etnia", plotOutput('km_etnia')),
                  tabPanel("Idade", plotlyOutput('km_idade', 
                                                 height = 500))
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
      
     # Modelos Paramétricos
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

##### 
##### Beckend
#-------------------------------------------------------------------------------
server <- function(input, output) {
  
  output$cancer_map <- renderLeaflet({
    leaflet(data = map_data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~colorNumeric("YlOrRd", Incidencia)(Incidencia),
        weight = 2,  # Peso da borda
        opacity = 1,
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
      addLegend(pal = colorNumeric("YlOrRd", map_data$Incidencia),
                values = map_data$Incidencia, opacity = 0.7,
                title = "Incidência de Câncer",
                position = "bottomright")
  })
  
  # Observar cliques no mapa
  observeEvent(input$cancer_map_shape_click, {
    clicked_municipio <- input$cancer_map_shape_click$id  # Obter o ID do município clicado
    output$selected_municipio <- renderText({
      paste("Município selecionado:", clicked_municipio)
    })
  })
  
  

## Menu Modelos Paramétricos    

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
    ggplotly(g1$plot)
  })
  
  # Função de Risco.
  output$tx_geral <- renderPlot({
    #
    hazard_geral <- -log(fit1$surv) / fit1$time
    #
    h1 <- plot(fit1$time, hazard_geral, type = "l", xlab = "Tempo", ylab = "Taxa de Falha")
  
  # Função de Risco Acumulado.
    
    
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
  ste <- exp(-time / 9051.529)
  stw <- exp(-(time / 19118.27) ^ 0.7583723)
  stln <- pnorm((-log(time) + 10.61644) / 2.893923)
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
        "Log-logístico" = "#d62728", # Vermelho
        "Gompertz" = "#9467bd"       # Roxo
      )) +
      theme_minimal()
    
    # Converter para plotly
    ggplotly(p1)
  })
  
}


#####
##### Executor shinyapp.
#-------------------------------------------------------------------------------
shinyApp(ui, server)




