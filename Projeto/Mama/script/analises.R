











mama <- df

mama <- mama %>% 
  mutate(Faixa_etária = cut(Idade,
                            breaks = c(-Inf, 39,40,50,60,Inf),
                            labels = c("Menos de 40 anos","Entre 40 a 49 anos",
                                       "Entre 50 a 60 anos", "Acima de 60 anos")))






km_sex = survfit(Surv(Tempo, Status) ~ Faixa_etária, data = mama)
print(km_sex)

mama2 <- mama %>% 
  filter(Cidade %in% c("PORTO VELHO","CACOAL"))
  
ggsurvplot(survfit(Surv(Tempo, Status) ~ Faixa_etária, data = mama), # modelo de sobrevivencia.
             xlab = "Dias", 
             ylab = "Probabilidade de Sobrevida",
             pval = TRUE, 
             risk.table = FALSE,
             conf.int = FALSE,
             ylim = c(0.85,1),
             surv.median.line = "hv")

  
  ggsurvplot(survfit(Surv(Tempo, Status) ~ Cidade, data = mama), # modelo de sobrevivencia.
           xlab = "Dias", 
           ylab = "Probabilidade de Sobrevida",
           pval = TRUE, 
           risk.table = TRUE,
           conf.int = FALSE,
           ylim = c(0.85,1),
           surv.median.line = "hv")


  
  
  
can_fm <- base %>% filter(str_detect(`Código da Topografia`, "C50|C53"))
  
can_fm <- base %>% filter(`Código da Topografia` %in% c("C509"))


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
can_fm$`Data do Óbito` <- replace(can_fm$`Data do Óbito`, is.na(can_fm$`Data do Óbito`), as.Date("2018-10-11"))

# Contagem do tempo.
can_fm <- can_fm %>% 
  mutate(Tempo = as.numeric(difftime(`Data do Óbito`, `Data de Diagnostico`, units = "days")))




df <- df %>% 
  mutate(indice = rank(-Tempo, ties.method = "first"))

df <- df %>% 
  filter(Tempo != 0)


# Criar o gráfico
ggplot(df, aes(x = Tempo, y = indice, color = as.factor(Status), label = ifelse(Status == 1, "Falha", "Censura"))) +
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


fit <- survfit(Surv(Tempo, Status) ~ as.factor(Extensão), data = df)

ggsurvplot(fit,
           conf.int = FALSE,
           )






can_fm$`Código da Topografia` <- ifelse(can_fm$`Código da Topografia` %in% c("C504","C508","C501","C503","C502","C500",
                                                                                     "C505","C506"), "C509", can_fm$`Código da Topografia`)


can_fm$`Código da Topografia` <- ifelse(can_fm$`Código da Topografia` %in% c("C530","C538"), "C539", can_fm$`Código da Topografia`)






#-------------------------------------------------------------------------------------------------------------------------
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

#
time <- fit_overall$time
st <- fit_overall$surv
ste <- exp(-time/9051.529)
stw <- exp(-(time/19118.27)^0.7583723)
stln <- pnorm((-log(time) + 10.61644)/2.893923)
stlog <- 1 / (1 + (exp(-intercept4) * time)^(1/scale4))
stgom <- exp(-lambda5 / theta5 * (exp(theta5 * time) - 1))
cbind(time, st, ste, stw, stln, stlog, stgom) %>% head()

#
par(mfrow = c(1,5))
plot(ste,st, pch = 16, ylim = range(c(0.85,1)), xlim = range(c(0.85,1)), ylab = 'S(t): Kaplan-Meier',
     xlab = 'S(t): Exponencial')
lines(c(0,1), c(0,1), type = 'l', lty = 1)

plot(stw,st, pch = 16, ylim = range(c(0.85,1)), xlim = range(c(0.85,1)), ylab = 'S(t): Kaplan-Meier',
     xlab = 'S(t): Weibull')
lines(c(0,1), c(0,1), type = 'l', lty = 1)

plot(stln,st, pch = 16, ylim = range(c(0.85,1)), xlim = range(c(0.85,1)), ylab = 'S(t): Kaplan-Meier',
     xlab = 'S(t): Log-normal')
lines(c(0,1), c(0,1), type = 'l', lty = 1)

plot(stlog,st, pch = 16, ylim = range(c(0.85,1)), xlim = range(c(0.85,1)), ylab = 'S(t): Kaplan-Meier',
     xlab = 'S(t): Log-logistico')
lines(c(0,1), c(0,1), type = 'l', lty = 1)

plot(stgom,st, pch = 16, ylim = range(c(0.85,1)), xlim = range(c(0.85,1)), ylab = 'S(t): Kaplan-Meier',
     xlab = 'S(t): Gompertz')
lines(c(0,1), c(0,1), type = 'l', lty = 1)

#
par(mfrow = c(1,5))
plot(fit_overall, conf.int = F, xlab = 'Tempo', ylab = 'S(t)', main = 'Exponencial', ylim = c(0.85,1))
lines(c(0,time), c(1,ste), lty = 2)

plot(fit_overall, conf.int = F, xlab = 'Tempo', ylab = 'S(t)', main = 'Weibull', ylim = c(0.85,1))
lines(c(0,time), c(1,stw), lty = 2)

plot(fit_overall, conf.int = F, xlab = 'Tempo', ylab = 'S(t)', main = 'Log-normal', ylim = c(0.85,1))
lines(c(0,time), c(1,stln), lty = 2)

plot(fit_overall, conf.int = F, xlab = 'Tempo', ylab = 'S(t)', main = 'Log-logistico', ylim = c(0.85,1))
lines(c(0,time), c(1,stlog), lty = 2)

plot(fit_overall, conf.int = F, xlab = 'Tempo', ylab = 'S(t)', main = 'Gompertz', ylim = c(0.85,1))
lines(c(0,time), c(1,stgom), lty = 2)

#-------------------------------------------------------------------------------
# Supondo que você já tenha os vetores de tempo e as probabilidades de sobrevivência
time <- fit_overall$time
st <- fit_overall$surv
ste <- exp(-time/9073.726)
stw <- exp(-(time/19940.62)^0.7498342)
stln <- pnorm((-log(time) + 10.7142)/2.957784)
stlog <- 1/(1 + (-0.2609508*time)^(9.7509464))
stgom <- NULL # Defina stgom conforme seus dados

# Criando um data frame com os dados
df <- data.frame(time = fit$time,
                 n.risk = fit$n.risk,
                 n.event = fit$n.event,
                 n.censr = fit$n.censor,
                 surv = fit$surv,
                 upper = fit$upper,
                 lower = fit$lower)

# Transformando os dados para o formato longo (long format) para ggplot2
df_long <- df %>%
  pivot_longer(cols = -time, names_to = "Model", values_to = "Survival")

# Gráfico de comparação dos modelos com ggplot2
ggplot(df_long, aes(x = Survival, y = surv, color = )) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  facet_wrap(~ Model, scales = "free_x") +
  theme_minimal() +
  labs(x = "S(t) do Modelo", y = "S(t): Kaplan-Meier",
       title = "Comparação das Funções de Sobrevivência: Kaplan-Meier vs Modelos")

#-------------------------------------------------------------------------------
# Ajustando o modelo Exponencial
fit_exp <- flexsurvreg(Surv(Tempo, Status) ~ 1, dist = "exp", data = df)

# Ajustando o modelo Weibull
fit_weibull <- flexsurvreg(Surv(Tempo, Status) ~ 1, dist = "weibull", data = df)

# Ajustando o modelo Log-normal
fit_lognormal <- flexsurvreg(Surv(Tempo, Status) ~ 1, data = df, dist = "lnorm")

# Ajustando o modelo Log-logístico
fit_loglogistic <- flexsurvreg(Surv(Tempo, Status) ~ 1, data = df, dist = "llogis")

# Ajustando o modelo Gompertz
fit_gompertz <- flexsurvreg(Surv(Tempo, Status) ~ 1, data = df, dist = "gompertz")

# Comparação dos AICs
aic_values <- c(
  Exponencial = fit_exp$AIC,
  Weibull = fit_weibull$AIC,
  LogNormal = fit_lognormal$AIC,
  LogLogistico = fit_loglogistic$AIC,
  Gompertz = fit_gompertz$AIC
)

# Exibir os valores de AIC
aic_values

# Função para calcular o BIC a partir do log-likelihood
bic_calc <- function(model){
  loglik <- model$loglik[2] # Log-likelihood do modelo ajustado
  n_params <- length(model$coefficients) # Número de parâmetros
  n_obs <- length(time) # Número de observações
  BIC <- log(n_obs) * n_params - 2 * loglik
  return(BIC)
}

# Comparando os BICs
bic_values <- c(
  Exponencial = bic_calc(fit_exp),
  Weibull = bic_calc(fit_weibull),
  LogNormal = bic_calc(fit_lognormal),
  LogLogistico = bic_calc(fit_loglogistic),
  Gompertz = bic_calc(fit_gompertz)
)

# Exibir os valores de BIC
bic_values

# Gráfico de comparação entre os modelos ajustados e Kaplan-Meier
plot(fit_exp, ci = TRUE, col = "red", main = "Comparação dos Modelos de Sobrevivência", ylim = c(0.8,1))
lines(fit_weibull, col = "blue")
lines(fit_lognormal, col = "green")
lines(fit_loglogistic, col = "purple")
lines(fit_gompertz, col = "orange")
legend("topright", legend = c("Exponencial", "Weibull", "Log-normal", "Log-logístico", "Gompertz"), 
       col = c("red", "blue", "green", "purple", "orange"), lty = 1)

# Verificando resíduos do modelo Weibull (exemplo)
resid_weibull <- residuals(fit_weibull, type = "martingale")
plot(resid_weibull)








snell <- -log(1 - pnorm(padr.log))
















# Calcular as funções de sobrevivência
time <- fit_overall$time
st <- fit_overall$surv
ste <- exp(-time / 9051.529)
stw <- exp(-(time / 19118.27) ^ 0.7583723)
stln <- pnorm((-log(time) + 10.61644) / 2.893923)
stlog <- 1 / (1 + (exp(-intercept4) * time)^(1 / scale4))
stgom <- exp(-lambda5 / theta5 * (exp(theta5 * time) - 1))

# Criar um dataframe
data <- data.frame(
  time = time,
  st = st,
  ste = ste,
  stw = stw,
  stln = stln,
  stlog = stlog,
  stgom = stgom
)

# Criar os gráficos
plot_exponential <- ggplot(data, aes(x = ste, y = st)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = 'S(t): Exponencial', y = 'S(t): Kaplan-Meier') +
  xlim(0.85, 1) + ylim(0.85, 1)

plot_weibull <- ggplot(data, aes(x = stw, y = st)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = 'S(t): Weibull', y = 'S(t): Kaplan-Meier') +
  xlim(0.85, 1) + ylim(0.85, 1)

plot_lognormal <- ggplot(data, aes(x = stln, y = st)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = 'S(t): Log-normal', y = 'S(t): Kaplan-Meier') +
  xlim(0.85, 1) + ylim(0.85, 1)

plot_loglogistic <- ggplot(data, aes(x = stlog, y = st)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = 'S(t): Log-logístico', y = 'S(t): Kaplan-Meier') +
  xlim(0.85, 1) + ylim(0.85, 1)

plot_gompertz <- ggplot(data, aes(x = stgom, y = st)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = 'S(t): Gompertz', y = 'S(t): Kaplan-Meier') +
  xlim(0.85, 1) + ylim(0.85, 1)

# Juntar os gráficos lado a lado
combined_plot <- plot_exponential + plot_weibull + plot_lognormal + plot_loglogistic + plot_gompertz +
  plot_layout(ncol = 5)

# Exibir o gráfico combinado
print(combined_plot)






# Criar os gráficos
plot_exponential <- ggplot(data, aes(x = time)) +
  geom_step(aes(y = st), color = "black") +
  geom_line(aes(y = ste), linetype = "dashed", color = "blue") +
  labs(title = "Exponencial", x = 'Tempo', y = 'S(t)') +
  ylim(0.85, 1)

plot_weibull <- ggplot(data, aes(x = time)) +
  geom_step(aes(y = st), color = "black") +
  geom_line(aes(y = stw), linetype = "dashed", color = "red") +
  labs(title = "Weibull", x = 'Tempo', y = 'S(t)') +
  ylim(0.85, 1)

plot_lognormal <- ggplot(data, aes(x = time)) +
  geom_step(aes(y = st), color = "black") +
  geom_line(aes(y = stln), linetype = "dashed", color = "green") +
  labs(title = "Log-normal", x = 'Tempo', y = 'S(t)') +
  ylim(0.85, 1)

plot_loglogistic <- ggplot(data, aes(x = time)) +
  geom_step(aes(y = st), color = "black") +
  geom_line(aes(y = stlog), linetype = "dashed", color = "purple") +
  labs(title = "Log-logístico", x = 'Tempo', y = 'S(t)') +
  ylim(0.85, 1)

plot_gompertz <- ggplot(data, aes(x = time)) +
  geom_step(aes(y = st), color = "black") +
  geom_line(aes(y = stgom), linetype = "dashed", color = "orange") +
  labs(title = "Gompertz", x = 'Tempo', y = 'S(t)') +
  ylim(0.85, 1)

# Juntar os gráficos lado a lado
combined_plot <- plot_exponential + plot_weibull + plot_lognormal + plot_loglogistic + plot_gompertz +
  plot_layout(ncol = 5)

# Exibir o gráfico combinado
print(combined_plot)









g6 <- ggplot(data, aes(x = time)) +
  geom_step(aes(y = st), color = "black", size = 1, linetype = "solid") +  # Kaplan-Meier
  geom_line(aes(y = ste, color = "Exponencial"), linetype = "dashed") +
  geom_line(aes(y = stw, color = "Weibull"), linetype = "dashed") +
  geom_line(aes(y = stln, color = "Log-normal"), linetype = "dashed") +
  geom_line(aes(y = stlog, color = "Log-logístico"), linetype = "dashed") +
  geom_line(aes(y = stgom, color = "Gompertz"), linetype = "dashed") +
  labs(title = "Funções de Sobrevivência",
       x = "Tempo em dias",
       y = "Probabilidade de Sobrevida",
       color = "Modelos") +
  ylim(0.85, 1) +
  theme_minimal()


ggplotly(g6)





# Criar o gráfico com todas as linhas em ggplot2
p <- ggplot(data, aes(x = time)) +
  geom_step(aes(y = st), color = "black", size = 1, linetype = "solid") +  # Kaplan-Meier
  geom_line(aes(y = ste, color = "Exponencial"), linetype = "dashed") +
  geom_line(aes(y = stw, color = "Weibull"), linetype = "dashed") +
  geom_line(aes(y = stln, color = "Log-normal"), linetype = "dashed") +
  geom_line(aes(y = stlog, color = "Log-logístico"), linetype = "dashed") +
  geom_line(aes(y = stgom, color = "Gompertz"), linetype = "dashed") +
  labs(title = "Funções de Sobrevivência",
       x = "Tempo",
       y = "S(t)",
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
ggplotly(p)


