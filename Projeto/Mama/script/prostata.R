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
can_pro <- base %>% filter(str_detect(`Código da Topografia`, "C61"))


colSums(is.na(can_pro))

#
can_pro <- can_pro %>% 
  mutate(across(c(`Data de Nascimento`,`Data do Óbito`,`Data de Último Contato`,`Data de Diagnostico`), ~ as.Date(., format = "%d/%m/%Y")))

#
can_pro <- can_pro %>% 
  mutate(Data_iguais = ifelse(is.na(`Data do Óbito`) | is.na(`Data de Diagnostico`),
                              NA,
                              `Data do Óbito` == `Data de Diagnostico`))

# Criar a variável Status
can_pro <- can_pro %>% 
  mutate(Status = ifelse(!is.na(`Data do Óbito`), 1, 0))

# Antes de criar a variável tempo, verificar o tempo máximo.
max(can_pro$`Data do Óbito`, na.rm = T)

# Substituir NA pela maior data do óbito.
can_pro$`Data do Óbito` <- replace(can_pro$`Data do Óbito`, is.na(can_pro$`Data do Óbito`), as.Date("2020-07-08"))

# Contagem do tempo.
can_pro <- can_pro %>% 
  mutate(Tempo = as.numeric(difftime(`Data do Óbito`, `Data de Diagnostico`, units = "days")))




df_p <- can_pro %>% 
  filter(!(Tempo == '0')) %>% 
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

df$Etnia <- ifelse(df$Etnia == "PARDA", "PARDA", 
                   ifelse(df$Etnia == "BRANCO", "BRANCO", "OUTROS"))
df$Estado_Civil <- ifelse(df$Estado_Civil == "CASADO", "CASADO", "OUTROS")
df$Escolaridade <- ifelse(df$Escolaridade == "FUNDAMENTAL I (1ª A 4ª SÉRIE)", "FUNDAMENTAL I (1ª A 4ª SÉRIE)", 
                          ifelse(df$Escolaridade == "FUNDAMENTAL II (5ª A 8ª SÉRIE)", "FUNDAMENTAL II (5ª A 8ª SÉRIE)",
                                 ifelse(df$Escolaridade == 'MÉDIO (ANTIGO SEGUNDO GRAU)', "MÉDIO (ANTIGO SEGUNDO GRAU)", "OUTROS")))






fit <- survfit(Surv(Tempo, Status) ~ 1, data = df_p)

d <- data.frame(time = fit$time,
                n.risk = fit$n.risk,
                n.event = fit$n.event,
                n.censr = fit$n.censor,
                surv = fit$surv,
                upper = fit$upper,
                lower = fit$lower)

ggsurvplot(
  fit, data = df_p,
  pval = TRUE, 
  conf.int = TRUE,
  xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência",
  break.time.by = 100,
  ggtheme = theme_light(),
  # risk.table = "abs_pct",
  # risk.table.y.text.col = TRUE,
  # risk.table.y.text = TRUE,
  # ncensor.plot = TRUE,
  ylim = c(0.85,1)
)








































