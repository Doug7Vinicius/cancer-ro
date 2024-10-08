library("flexsurv")
library("data.table")
library("ggplot2")
ggplot2::theme_set(theme_minimal())

mapply(flexsurv::hweibull, 
       shape = c(.5, 1, 1.5),
       scale = c(.25, 1, 1.75),
       MoreArgs = list(x = 1:2))


hazfun <- function(FUN, param_vals, times){
  # Compute hazard for all possible combinations of parameters and times
  values <- do.call("mapply", c(list(FUN = FUN),
                                as.list(expand.grid(param_vals)),
                                list(MoreArgs = list(x = times))))
  x <- data.table(expand.grid(c(param_vals, list(time = times))),
                  value = c(t(values)))
  
  # Create factor variables and intuitive names for plotting
  param_names <- names(param_vals)
  x[[param_names[1]]] <- factor(x[[param_names[1]]],
                                levels = unique(x[[param_names[1]]]))
  if (length(param_vals) > 1){
    for (j in 2:length(param_vals)){
      ordered_vals <- unique(x[[param_names[j]]])
      x[[param_names[j]]] <- paste0(param_names[j], " = ", x[[param_names[j]]])
      factor_levels <- paste0(param_names[j], " = ", ordered_vals)
      x[[param_names[j]]] <- factor(x[[param_names[j]]],
                                    levels = factor_levels)
    }
  }
  
  # Return
  return(x)
}


survfun <- function(FUN, param_vals, times) {
  # Calcula a função de sobrevivência para todas as combinações possíveis de parâmetros e tempos
  values <- do.call("mapply", c(list(FUN = FUN),
                                as.list(expand.grid(param_vals)),
                                list(MoreArgs = list(x = times))))
  
  # Organiza os resultados em um data.table
  x <- data.table(expand.grid(c(param_vals, list(time = times))),
                  value = c(t(values)))
  
  # Cria variáveis fatoriais e nomes intuitivos para plotagem
  param_names <- names(param_vals)
  x[[param_names[1]]] <- factor(x[[param_names[1]]],
                                levels = unique(x[[param_names[1]]]))
  if (length(param_vals) > 1) {
    for (j in 2:length(param_vals)) {
      ordered_vals <- unique(x[[param_names[j]]])
      x[[param_names[j]]] <- paste0(param_names[j], " = ", x[[param_names[j]]])
      factor_levels <- paste0(param_names[j], " = ", ordered_vals)
      x[[param_names[j]]] <- factor(x[[param_names[j]]],
                                    levels = factor_levels)
    }
  }
  
  # Retorna o data.table com os resultados
  return(x)
}


times <- seq(1, 10, 1)
rate <- seq(1, 5, 1)
haz_exp <- hazfun(flexsurv::hexp, 
                  list(rate = rate),
                  times = times)
ggplot(haz_exp, aes(x = time, y = value, col = rate)) +
  geom_line() + xlab("Time") + ylab("Hazard") +
  scale_y_continuous(breaks = rate) 


surv_exp <- survfun(stats::dexp, 
                  list(rate = rate),
                  times = times)
ggplot(surv_exp, aes(x = time, y = value, col = rate)) +
  geom_line() + xlab("Tempo") + ylab("Sobrevida") +
  scale_y_continuous(breaks = rate)

wei_shape <- seq(.25, 3, .25)
haz_wei <- hazfun(stats::dweibull, 
                  list(scale = seq(2, 5, .5),
                       shape = wei_shape),
                  times = times)
ggplot(haz_wei, aes(x = time, y = value, col = scale)) +
  geom_line() + facet_wrap(~shape, scales = "free_y") +
  xlab("Time") + ylab("Hazard") 





haz_lnorm <- hazfun(flexsurv::hlnorm, 
                    list(meanlog = seq(0, 4, .5),
                         sdlog = seq(.5, 3, .5)),
                    times = seq(0, 20, .1))
ggplot(haz_lnorm, aes(x = time, y = value, col = meanlog)) +
  geom_line() + facet_wrap(~sdlog, scales = "free_y") +
  xlab("Time") + ylab("Hazard") 

haz_lnorm <- hazfun(stats::dlnorm, 
                    list(meanlog = seq(0, 4, .5),
                         sdlog = seq(.5, 3, .5)),
                    times = seq(0, 20, .1))
ggplot(haz_lnorm, aes(x = time, y = value, col = meanlog)) +
  geom_line() + facet_wrap(~sdlog, scales = "free_y") +
  xlab("Time") + ylab("Hazard") 




library("muhaz")
kernel_haz_est <- muhaz(df$Tempo, df$Status)
kernel_haz <- data.table(time = kernel_haz_est$est.grid,
                         est = kernel_haz_est$haz.est,
                         method = "Kernel density")


dists <- c("exp", "weibull", "lognormal", "llogis")
dists_long <- c("Exponential", "Weibull","Lognormal", "Log-logistic")
parametric_haz <- vector(mode = "list", length = length(dists))
for (i in 1:length(dists)){
  fit <- flexsurvreg(Surv(Tempo, Status) ~ 1, data = dat, dist = dists[i]) 
  parametric_haz[[i]] <- summary(fit, type = "hazard", 
                                 ci = FALSE, tidy = TRUE)
  parametric_haz[[i]]$method <- dists_long[i]
}
parametric_haz <- rbindlist(parametric_haz)

haz <- rbind(kernel_haz, parametric_haz)
haz[, method := factor(method,
                       levels = c("Kernel density",
                                  dists_long))]
n_dists <- length(dists) 
ggplot(haz, aes(x = time, y = est, col = method, linetype = method)) +
  geom_line() +
  xlab("Days") + ylab("Hazard") + 
  scale_colour_manual(name = "", 
                      values = c("black", rainbow(n_dists))) +
  scale_linetype_manual(name = "",
                        values = c(1, rep_len(2:6, n_dists)))































set.seed(123)  # Para reprodutibilidade
n <- 100  # Número de diagnósticos
data_diagnosticos <- data.frame(
  data = can_fm$`Data de Diagnostico`
)

# Extrair o ano da data
data_diagnosticos <- data_diagnosticos %>%
  mutate(ano = format(data, "%Y"))

# Contar a frequência de diagnósticos por ano
frequencia_por_ano <- data_diagnosticos %>%
  group_by(ano) %>%
  summarise(frequencia = n())

# Criar o gráfico de barras
ggplot(frequencia_por_ano, aes(x = ano, y = frequencia, fill = ano)) +
  geom_bar(stat = "identity") +
  labs(title = "Câncer por ano de diagnóstico",
       x = "Ano do diagnóstico",
       y = "Nº de casos") +
  theme_minimal() +
  theme(legend.position = "none") 






library(ggplot2)
library(dplyr)

# Calcular a frequência e percentual por faixa etária
dados_resumo <- df %>%
  group_by(faixa_etaria) %>%
  summarise(frequencia = n()) %>%
  mutate(percentual = (frequencia / sum(frequencia)) * 100)

# Ordenar a faixa etária na ordem desejada
dados_resumo <- dados_resumo %>%
  mutate(faixa_etaria = factor(faixa_etaria, levels = c(">= 60 anos", "50 a 59 anos", "40 a 49 anos",
  "< 40 anos")))

# Criar o gráfico de barras horizontais
ggplot(dados_resumo, aes(x = frequencia, y =  faixa_etaria)) +
  geom_bar(stat = "identity", fill = "#F867A2") +
  geom_text(aes(label = paste0(round(percentual, 1), "%")), 
            hjust = 6,  # Centraliza horizontalmente dentro da barra
            vjust = 0,    # Alinha o texto na parte inferior da barra
            color = "white", size = 4.5) +
  geom_text(aes(label = frequencia), 
            hjust = -.5,  # Coloca a frequência no final da barra
            color = "black", size = 4.5) +
  labs(x = "", y = "Faixa Etária") +
  theme_classic() +
  theme(panel.grid.major.y = element_blank())  # Remove as linhas de grade horizontais





f1 <- dados_resumo %>% 
  ggplot(aes(x = frequencia, y = faixa_etaria)) +
  geom_col(fill = "#F867A2") +
  # adicionar camada de texto
  geom_label(aes(label = frequencia), size = 4.5) +
  geom_text(aes(label = paste0(round(percentual, 2), "%")), 
            hjust = case_when(
              dados_resumo$faixa_etaria == "< 40 anos" ~ 6.5,  # Centraliza na barra
              dados_resumo$faixa_etaria == "40 a 49 anos" ~ 12.5,
              dados_resumo$faixa_etaria == "50 a 59 anos" ~ 13.59,
              dados_resumo$faixa_etaria == ">= 60 anos" ~ 12.2
            ),
            vjust = 0.5,  # Alinha verticalmente
            color = "white", size = 4.5) +
  labs(x = "", y = "Faixa Etária") +
  theme_classic()









"#FEECE2"












dat <- df
dat[, status := ifelse(status == 1, 0, 1)]
head(dat)

library("muhaz")
kernel_haz_est <- muhaz(dat$Tempo, dat$Status)
kernel_haz <- data.table(time = kernel_haz_est$est.grid,
                         est = kernel_haz_est$haz.est,
                         method = "Kernel density")

dists <- c("exp", "weibull", "gompertz", 
           "lognormal", "llogis")

dists_long <- c("Exponential", "Weibull",
                "Gompertz", "Lognormal", "Log-logistic")


parametric_haz <- vector(mode = "list", length = length(dists))
for (i in 1:length(dists)){
  fit <- flexsurvreg(Surv(Tempo, Status) ~ 1, data = dat, dist = dists[i]) 
  parametric_haz[[i]] <- summary(fit, type = "hazard", 
                                 ci = FALSE, tidy = TRUE)
  parametric_haz[[i]]$method <- dists_long[i]
}
parametric_haz <- rbindlist(parametric_haz)

parametric_haz <- rbindlist(parametric_haz)

haz <- rbind(kernel_haz, parametric_haz)
haz[, method := factor(method,
                       levels = c("Kernel density",
                                  dists_long))]
n_dists <- length(dists) 
ggplot(haz, aes(x = time, y = est, col = method, linetype = method)) +
  geom_line() +
  xlab("Days") + ylab("Hazard") + 
  scale_colour_manual(name = "", 
                      values = c("black", rainbow(n_dists))) +
  scale_linetype_manual(name = "",
                        values = c(1, rep_len(2:6, n_dists)))



































# Interactive plot to examine survival time and end
orca <- mutate(df, text = paste(
  "Subject ID = ", Cod_Paciente, "<br>", 
  "Time = ", Tempo, "<br>", 
  "Event = ", Status, "<br>", 
  "Age = ", Idade, "<br>"
))

ggplotly(
  ggplot(orca, aes(x = Cod_Paciente, y = Tempo, text = text)) +
    geom_linerange(aes(ymin = 0, ymax = Tempo)) +
    geom_point(aes(shape = Status, color = Status), stroke = 0.3, cex = 2) +
    scale_shape_manual(values = c(0,1)) +
    labs(y = "Time (years)", x = "Subject ID") + 
    coord_flip() + 
    theme_classic(),
  tooltip = "text"
)
