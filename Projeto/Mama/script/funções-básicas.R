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
