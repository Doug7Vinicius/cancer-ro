#-------------------------------------------------------------------------------
# Análise de Sobrevivência.


dim(df)

df1 <- df %>% 
  filter(Sexo != "MASCULINO")

df1 <- df1 %>% 
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















fit <- survfit(Surv(Tempo, Status) ~ 1, data = df)

d <- data.frame(time = fit$time,
                n.risk = fit$n.risk,
                n.event = fit$n.event,
                n.censr = fit$n.censor,
                surv = fit$surv,
                upper = fit$upper,
                lower = fit$lower)

ggsurvplot(
  fit, data = df,
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


tmp = df %>% 
  select(Idade, Tempo) %>% 
  pivot_longer(everything())

t1 <- ggplot(tmp, aes(x = value)) + 
  geom_histogram(aes(y = ..density..), alpha = 0.5) +
  geom_density() +
  facet_wrap(. ~ name, scales = "free") +
  theme(text = element_text(size = 15)) +
  xlab("") + ylab("Densidade")



tmp = df %>%
  select(Status,Idade,Tempo) %>%
  pivot_longer(-one_of("Status")) %>%
  mutate(had_event = ifelse(Status == 0,"Censura", "Falha"))
t2 <- ggplot(tmp, aes(x = value)) +
  geom_density(aes(color = had_event)) +
  facet_wrap(. ~ name, scales = "free") +
  theme(text = element_text(size = 15),
        legend.title = element_blank(),
        legend.position = "top") +
  xlab("") + ylab("Densidade")

t1 / t2

plot_bar(df %>%
           select(Estado_Civil, Escolaridade, Status),
         theme_config=list(text = element_text(size = 10)),
         ncol = 3,
         order_bar=TRUE)

tmp = df %>%
  select(Status, Estado_Civil, Escolaridade, Etnia) %>%
  mutate(Status = ifelse(Status == 0,"Censura", "Falha"),
         Estado_civil = factor(Estado_Civil)) %>%
  pivot_longer(-one_of("Status")) %>%
  group_by(Status,name,value) %>%
  summarise(count = n())
ggplot(tmp, aes(x = value, y = count)) +
  geom_bar(aes(fill = Status), position = "dodge", stat = "identity") +
  facet_wrap(. ~ name, scales = "free") +
  theme(text = element_text(size = 7),
        legend.title = element_blank(),
        legend.position = "top")

scattervars <- c("Idade", "Tempo")
time_df <- df %>% 
  select(all_of(scattervars)) %>%
  pivot_longer(cols=-Tempo, names_to="variable")

ggplot(time_df, aes(x=Tempo, y=value)) +
  geom_jitter() +
  facet_wrap(vars(variable), nrow=2, scales="free")

Surv(df$Tempo, df$Status)[1:10]

fit_overall = survfit(Surv(Tempo, Status) ~ 1, data = df)
print(fit_overall)

# Tabela de Sobrevida
f <- summary(fit_overall)
df_overall_fit <- data.frame(f$time, f$n.risk, f$n.event, f$n.censor, f$surv, f$lower, f$upper)
names(df_overall_fit) <- c("time", "n.risk", "n.event", "n.censor", "survival", "ci_95_lower", "ci_95_upper")
head(df_overall_fit, n=10)

ggsurvplot(fit_overall, 
           xlab = "Dias", 
           ylab = "Probabilidade de Sobrevida",
           risk.table = TRUE,
           conf.int = TRUE,
           ylim = c(0.85,1),
           surv.median.line = "hv")

# 
km_sex = survfit(Surv(Tempo, Status) ~ , data = df)
print(km_sex)

ggsurvplot(survfit(Surv(Tempo, Status) ~ Sexo, data = ), # modelo de sobrevivencia.
           xlab = "Dias", 
           ylab = "Probabilidade de Sobrevida",
           pval = TRUE, 
           risk.table = TRUE,
           conf.int = FALSE,
           ylim = c(0.85,1),
           surv.median.line = "hv")

ggsurvplot(
  survfit(Surv(Tempo, Status) ~ Sexo, data = baselimpa),                    
  pval = TRUE,              
  conf.int = TRUE,          
  xlab = "Tempo em dias",
  break.time.by = 150,      
  ggtheme = theme_light(),  
  risk.table = "abs_pct",   
  risk.table.y.text.col = T,
  risk.table.y.text = FALSE,
  ncensor.plot = TRUE,      
  surv.median.line = "hv",   
  legend.labs=c("Feminino", "Masculino"), legend.title="Sexo",  
  palette=c("orchid2", "dodgerblue2"), 
  title="Curva de Kaplan-Meier",
  ylim = c(0.65,1),
  risk.table.height=.15
)

survdiff(Surv(Tempo, Status) ~ Sexo, baselimpa)

ggsurvplot(survfit(Surv(Tempo, Status) ~ Sexo, data = baselimpa),
           conf.int = TRUE,
           ggtheme = theme_bw(), 
           fun = "log")

ggsurvplot(survfit(Surv(Tempo, Status) ~ Sexo, data = baselimpa),
           conf.int = TRUE,
           ggtheme = theme_bw(), 
           fun = "event")

ggsurvplot(survfit(Surv(Tempo, Status) ~ Sexo, data = baselimpa),
           conf.int = TRUE,
           ggtheme = theme_bw(), 
           fun = "cumhaz")

fit_overall = survfit(Surv(Tempo, Status) ~ 1, data = df)
surv_times <- c(0, 0.5*365, 365, 1.5*365, 2*365)

gg_conditional_surv(
  basekm = fit_overall, 
  at = surv_times,
  main = "Sobrevivência condicional",
  ylab = "Probabilidade de Sobrevivência",
  xlab = "Dias"
) +
  labs(color = "Tempo condicional") + ylim(c(0.85,1))

conditional_surv_est(fit_overall, t1=0, t2=2*365)

surv_times <- c(0.25*365, 0.5*365, 365, 1.5*365, 2*365)



cox_esc = coxph(Surv(Tempo, Status) ~ Escolaridade, data = df)
print(cox_esc)

survdiff(Surv(Tempo, Status) ~ Escolaridade, data = df)

test.ph <- cox.zph(cox_esc)
print(test.ph)

ggcoxzph(test.ph)
plot(test.ph)

m = survfit(Surv(Tempo, Status) ~ Escolaridade, data = df1)
s = summary(m)
s_table = data.frame(s$strata, s$time, s$n.risk, s$n.event, s$n.censor, s$surv, s$lower, s$upper)
s_table = s_table %>%
  rename(strata=s.strata, time=s.time, surv=s.surv, lower=s.lower, upper=s.upper) %>%
  mutate(negloglogsurv=-log(-log(surv)))

ggplot(s_table, aes(x=time, y=negloglogsurv, color=strata)) + 
  geom_line(size=1.25) +
  theme(text=element_text(size=16),
        plot.title=element_text(hjust=0.5)) +
  ylab("-ln(-ln(S(t)))") +
  ggtitle("-log-log plot of survival time by sex")

ggcoxdiagnostics(cox_esc, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())

ggcoxdiagnostics(cox_sex, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())


km_sex = survfit(Surv(Tempo, Status) ~ Sexo, data = baselimpa)
km.curve <- getKMcurve(km = km_sex, 
                       time.col = 'Tempo', 
                       event.col = 'Status', 
                       group.col = 'Sexo', 
                       data = baselimpa)
km.curve = km.curve %>% 
  select(group, Time, Survival) %>% 
  rename(sex=group, time=Time, surv=Survival) %>%
  mutate(sex=factor(sex, levels=c("FEMININO", "MASCULINO")))

cox.curve = data.frame(sex=factor(baselimpa$Sexo, levels=c("FEMININO", "MASCULINO")),
                       time=baselimpa$Tempo,
                       surv=exp(-predict(cox_sex, type="expected")))
ggplot() +
  geom_line(data=km.curve, aes(x=time, y=surv, color=sex), size=0.25) +
  geom_line(data=cox.curve, aes(x=time, y=surv, color=sex), size=1, linetype="dashed") +
  ggtitle("Kaplan-Meier vs. Modelo de Cox") +
  xlab("Tempo (dias)") + ylab("Probabilidade de Sobrevivência") + 
  theme(title=element_text(hjust=.5),
        text=element_text(size=10))

cox_age = coxph(Surv(Tempo, Status) ~ Idade, data = baselimpa)
print(cox_age)

test.ph <- cox.zph(cox_age)
ggcoxzph(test.ph)
plot(test.ph)

ggcoxdiagnostics(cox_age, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())

ggcoxdiagnostics(cox_age, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())

ggcoxdiagnostics(cox_age, type = "martingale",
                 linear.predictions = FALSE, ggtheme = theme_bw())


sm.options(
  list(
    xlab = "Idade (anos)",
    ylab = "Tempo de morte (dias)")
)

layout(matrix(c(1,2,3), ncol=3))

#par(mar=c(5.1, 4.1, 4.1, 2.1))
par(pty="s")  # make plot square

sm.survival(
  x = baselimpa$Idade,
  y = baselimpa$Tempo,
  status = baselimpa$Status,
  h = 1 * sd(baselimpa$Idade) / nrow(baselimpa)^(-1/4),
  p = .5,
  add=FALSE
)

sm.survival(
  x = baselimpa$Idade,
  y = baselimpa$Tempo,
  status = baselimpa$Status,
  h = 1 * sd(baselimpa$Idade) / nrow(baselimpa)^(-1/4),
  p = .25,
  add=TRUE
)

sm.survival(
  x = baselimpa$Idade,
  y = baselimpa$Tempo,
  status = baselimpa$Status,
  h = 1 * sd(baselimpa$Idade) / nrow(baselimpa)^(-1/4),
  p = .75,
  add=TRUE
)

title('1*h, quantiles .25, .5, .75')



par(pty="s")  # make plot square
sm.survival(
  x = baselimpa$Idade,
  y = baselimpa$Tempo,
  status = baselimpa$Status,
  h = .5 * sd(baselimpa$Idade) / nrow(baselimpa)^(-1/4),
  p = .5,
  add=FALSE
)

sm.survival(
  x = baselimpa$Idade,
  y = baselimpa$Tempo,
  status = baselimpa$Status,
  h = .5 * sd(baselimpa$Idade) / nrow(baselimpa)^(-1/4),
  p = .25,
  add=TRUE
)

sm.survival(
  x = baselimpa$Idade,
  y = baselimpa$Tempo,
  status = baselimpa$Status,
  h = .5 * sd(baselimpa$Idade) / nrow(baselimpa)^(-1/4),
  p = .75,
  add=TRUE
)

title('.5*h, quantiles .25, .5, .75')



par(pty="s")  # make plot square
sm.survival(
  x = baselimpa$Idade,
  y = baselimpa$Tempo,
  status = baselimpa$Status,
  h = .25 * sd(baselimpa$Idade) / nrow(baselimpa)^(-1/4),
  p = .5,
  add=FALSE
)







ggplotly(
  baselimpa %>%
    mutate(
      text = paste("Subject ID = ", '', "<br>", "Time = ", Tempo, "<br>", "Event = ",  
                   Status, "<br>", "Age = ", round(Idade, 2), "<br>", "Stage = ", Sexo)
    ) %>%
    ggplot(aes(x = '', y = Tempo, text = Tempo)) +
    geom_linerange(aes(ymin = 0, ymax = Tempo)) +
    geom_point(aes(shape = Status, color = Status), stroke = 1, cex = 2) +
    labs(y = "Time (years)", x = "Subject ID") + coord_flip() + theme_classic(),
  tooltip = "text"
)








ggplot(data = baselimpa, aes(x = Tempo, fill = Sexo)) + 
  geom_histogram() + 
  facet_grid(Status ~.) + 
  ggtitle("Figure 1. Distribution of time-to-event by type of treatment")













baselimpa_cat = baselimpa %>% 
  mutate(age_grp = cut(baselimpa$Idade, breaks=c(-Inf, 50, 60, 70, Inf), labels=c("<=50", "51-60", "61-70", ">70")))

cox_age_cat = coxph(Surv(Tempo, Status) ~ age_grp, data = baselimpa_cat)
print(cox_age_cat)

print(paste0("cox_age AIC = ", round(AIC(cox_age), 2)))

print(paste0("cox_age_cat AIC = ", round(AIC(cox_age_cat), 2)))

cox.zph(cox_age_cat)

baselimpa_cat = baselimpa %>% 
  mutate(age_grp = cut(baselimpa$Idade, breaks=c(-Inf, 69, Inf), labels=c("39-69", "70+")))

cox_age_70 = coxph(Surv(Tempo, Status) ~ age_grp, data = baselimpa_cat)
print(cox_age_70)

print(paste0("cox_age AIC = ", round(AIC(cox_age), 2)))

print(paste0("cox_age_70 AIC = ", round(AIC(cox_age_70), 2)))

cox.zph(cox_age_70)

m = survfit(Surv(Tempo, Status) ~ age_grp, baselimpa_cat)
s = summary(m)
s_table = data.frame(s$strata, s$time, s$n.risk, s$n.event, s$n.censor, s$surv, s$lower, s$upper)
s_table = s_table %>%
  rename(strata=s.strata, time=s.time, surv=s.surv, lower=s.lower, upper=s.upper) %>%
  mutate(negloglogsurv=-log(-log(surv)))

ggplot(s_table, aes(x=time, y=negloglogsurv, color=strata)) + 
  geom_line(size=1.25) +
  theme(text=element_text(size=16),
        plot.title=element_text(hjust=0.5)) +
  ylab("-log(-log(S(t)))") +
  ggtitle("-log-log plot of survival time by age")

ggsurvplot(survfit(Surv(Tempo, Status) ~ age_grp, data = baselimpa_cat), 
           xlab="Days", 
           ylab="Overall survival probability",
           pval = TRUE,
           risk.table=FALSE,
           conf.int=TRUE,
           ylim = c(0.6,1),
           surv.median.line="hv")


km_age = survfit(Surv(Tempo, Status) ~ age_grp, data = baselimpa_cat)
km.curve <- getKMcurve(km = km_age, 
                       time.col = 'Tempo', 
                       event.col = 'Status', 
                       group.col = 'age_grp', 
                       data = baselimpa_cat)
km.curve = km.curve %>% 
  select(group, Time, Survival) %>% 
  rename(age_grp=group, time=Time, surv=Survival) %>%
  mutate(age_grp=factor(age_grp, levels=c("39-69", "70+")))

cox.curve = data.frame(age_grp=factor(baselimpa_cat$age_grp, levels=c("39-69", "70+")),
                       time=baselimpa_cat$Tempo,
                       surv=exp(-predict(cox_age, type="expected")))
ggplot() +
  geom_line(data=km.curve, aes(x=time, y=surv, color=age_grp), size=0.25) +
  geom_line(data=cox.curve, aes(x=time, y=surv, color=age_grp), size=1, linetype="dashed") +
  ggtitle("Kaplan-Meier vs. Cox Model") +
  ylab("Survival Probability") + 
  theme(title=element_text(hjust=.5),
        text=element_text(size=10))


#-------------------------------------------------------------------------------------------------------------------------
# Distribuição Exponencial
ajust1 <- survreg(Surv(Tempo, Status) ~ 1, dist = 'exponential', data = df1)
ajust1

alpha <- exp(ajust1$coefficients[1])
alpha

ajust2 <- survreg(Surv(Tempo, Status) ~ 1, dist = 'weibull', data = df1)
ajust2

alpha <- exp(ajust2$coefficients[1])
alpha
gama <- 1/ajust2$scale
gama
cbind(gama, alpha)

# Distribuição Log-normal
ajust3 <- survreg(Surv(Tempo, Status) ~ 1, dist = 'lognorm', data = pulmao)
ajust3

# Distribuição Log-logístico
ajust4 <- survreg(Surv(Tempo, Status) ~ 1, dist = 'loglogistic', data = pulmao)
ajust4

#
time <- fit_overall$time
st <- fit_overall$surv
ste <- exp(-time/736.4182)
stw <- exp(-(time/830.9055)^0.6376552)
stln <- pnorm((-log(time) + 6.063783)/2.152414)
stlog <- 1/(1 + (-0.9856337*time)^(416))
cbind(time, st, ste, stw, stln, stlog, stgom) %>% head()

fit_overall = survfit(Surv(Tempo, Status) ~ 1, data = pulmao)

ajust5 <- flexsurvreg(Surv(Tempo, Status) ~ 1, dist = 'gompertz', data = pulmao)
ajust5

stgom <- exp((-1/-0.002829)*0.003064*(exp(-0.002829*time) - 1))

#
par(mfrow = c(1,4))
plot(ste,st, pch = 16, ylim = range(c(0.75,1)), xlim = range(c(0.6,1)), ylab = 'S(t): Kaplan-Meier',
     xlab = 'S(t): Exponencial')
lines(c(0,1), c(0,1), type = 'l', lty = 1)

plot(stw,st, pch = 16, ylim = range(c(0.75,1)), xlim = range(c(0.6,1)), ylab = 'S(t): Kaplan-Meier',
     xlab = 'S(t): Weibull')
lines(c(0.7,1), c(0,1), type = 'l', lty = 1)

plot(stln,st, pch = 16, ylim = range(c(0.75,1)), xlim = range(c(0.6,1)), ylab = 'S(t): Kaplan-Meier',
     xlab = 'S(t): Log-normal')
lines(c(0.7,1), c(0,1), type = 'l', lty = 1)

plot(stlog,st, pch = 16, ylim = range(c(0.75,1)), xlim = range(c(0.6,1)), ylab = 'S(t): Kaplan-Meier',
     xlab = 'S(t): Log-logistico')
lines(c(0.7,1), c(0,1), type = 'l', lty = 1)

#
par(mfrow = c(1,4))
plot(fit_overall, conf.int = F, xlab = 'Tempo', ylab = 'S(t)', ylim = c(0,1))
lines(c(0,time), c(1,ste), lty = 2)

plot(fit_overall, conf.int = F, xlab = 'Tempo', ylab = 'S(t)', ylim = c(0,1))
lines(c(0,time), c(1,stw), lty = 2)

plot(fit_overall, conf.int = F, xlab = 'Tempo', ylab = 'S(t)', ylim = c(0,1))
lines(c(0,time), c(1,stln), lty = 2)

plot(fit_overall, conf.int = F, xlab = 'Tempo', ylab = 'S(t)', ylim = c(0,1))
lines(c(0,time), c(1,stlog), lty = 2)
#-------------------------------------------------------------------------------




# 
km_civil = survfit(Surv(Tempo, Status) ~ Estado_Civil, data = baselimpa)
print(km_civil)

ggsurvplot(survfit(Surv(Tempo, Status) ~ Estado_Civil, data = baselimpa), # modelo de sobrevivencia.
           xlab = "Dias", 
           ylab = "Probabilidade de Sobrevida",
           pval = TRUE, 
           risk.table = TRUE,
           conf.int = FALSE,
           ylim = c(0.6,1),
           surv.median.line = "hv")












meninas <- filter(base_pvh, Codigo_Doenca == 'C539')
table(meninas$Sexo)


base_pvh %>% 
  group_by(Codigo_Doenca) %>%
  count() %>%
  ggplot(aes(x = fct_reorder(Codigo_Doenca, n), y = n)) +
  geom_col(fill = "#7a1f18") +
  coord_flip() +
  geom_text(aes(label = round(n)), hjust = -.2) +
  xlab("Promotorias") + ylab("Número de Manifestações")





fit_jipa = survfit(Surv(Tempo, Status) ~ 1, data = base_jipa)
print(fit_overall)

# Tabela de Sobrevida
f <- summary(fit_jipa)
df_overall_fit <- data.frame(f$time, f$n.risk, f$n.event, f$n.censor, f$surv, f$lower, f$upper)
names(df_overall_fit) <- c("time", "n.risk", "n.event", "n.censor", "survival", "ci_95_lower", "ci_95_upper")
head(df_overall_fit, n=10)

ggsurvplot(fit_jipa,
           xlab = "Dias", 
           ylab = "Probabilidade de Sobrevida",
           risk.table = TRUE,
           conf.int = FALSE,
           ylim = c(0.75,1),
           surv.median.line = "hv")


ggsurvplot(survfit(Surv(Tempo, Status) ~ 1, data = base_cacoal), # modelo de sobrevivencia.
           xlab = "Dias", 
           ylab = "Probabilidade de Sobrevida",
           pval = TRUE, 
           risk.table = TRUE,
           conf.int = FALSE,
           ylim = c(0.75,1),
           surv.median.line = "hv")








colon_Survfit <- survfit(Surv(Tempo, Status) ~ 1, data = baselimpa)
colon_ggSurvplot <- ggsurvplot (fit = colon_Survfit, 
                                data = baselimpa,
                                title = "Overall Survival",
                                subtitle = "Unstratified Curve",
                                font.title = c(22, "bold", "black"),
                                ggtheme = theme_dark() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
                                  # theme_dark will give a dark background with lines on the plot
                                  theme(plot.subtitle = element_text(hjust = 0.5, size = 16, face = "italic")),
                                ####### Censor Details ########
                                censor = TRUE, # logical value. If TRUE, censors will be drawn
                                censor.shape="|",
                                censor.size = 5,
                                ####### Confidence Intervals ########
                                conf.int = TRUE, # To Remove conf intervals use "FALSE"
                                surv.median.line = "hv", # allowed values include one of c("none", "hv", "h", "v"). v: vertical, h:horizontal
                                ####### Format Axes #######
                                xlab="Days", # changes xlabel,
                                ylab = "Survival Probability",
                                font.x=c(18,"bold"), # changes x axis labels
                                font.y=c(18,"bold"), # changes y axis labels
                                font.xtickslab=c(14,"plain"), # changes the tick label on x axis
                                font.ytickslab=c(14,"plain"),
                                ######## Format Legend #######
                                legend.title = "All Patients",
                                legend.labs = c("Total Cohort"), # Change the Strata Legend
                                legend = c(0.8,0.8), #c(0,0) corresponds to the "bottom left" and c(1,1) corresponds to the "top right" position
                                ######## Plot Dimensions #######
                                surv.plot.height = 0.75, # Default is 0.75
                                ######## Risk Table #######
                                risk.table = FALSE, # Adds Risk Table
                                risk.table.height = 0.35, # Adjusts the height of the risk table (default is 0.25)
                                ######## p-value details #######
                                pval = FALSE,
                                pval.size = 5,
                                pval.coord = c(1,1)
)
colon_ggSurvplot


model.colon <- coxph(Surv(Tempo, Status) ~ Etnia,
                     data = baselimpa)
model.colon

ggforest(model.colon)




library('carData')
Rossi[1:5,1:10]

mod.allison <- coxph(Surv(week, arrest) ~ fin + age + race + wexp + mar + paro + prio,
                     data = Rossi)
mod.allison

summary(mod.allison)

library('car')
Anova(mod.allison)

plot(survfit(mod.allison), ylim = c(0.7,1), xlab = 'Semanas',
     ylab = '')






res.cox1 <- coxph(Surv(Tempo, Status) ~ , 
                  data = baselimpa)

ggforest(res.cox1, data = baselimpa)

res.cox2 <- coxph(Surv(Tempo, Status) ~ Etnia, 
                  data = baselimpa)

ggforest(res.cox2, data = baselimpa)

res.cox3 <- coxph(Surv(Tempo, Status) ~ Escolaridade, 
                  data = baselimpa)

ggforest(res.cox3, data = baselimpa)

res.cox4 <- coxph(Surv(Tempo, Status) ~ Estado_Civil, 
                  data = baselimpa)

ggforest(res.cox4, data = baselimpa)

fit1 <- coxph(Surv(futime, fustat) ~ rx + age + resid.ds, data = ovarian)
fit1

imat <- solve(vcov(fit1))
acoef <- seq(0, .25, length = 100)
profile <- matrix(0, 100, 2)
for (i in 100) {
  icoef <- c(fit1$coef[1], acoef[i], fit1$coefficients[3])
  tfit <- coxph(Surv(futime, fustat) ~ rx + age + resid.ds, ovarian, init = icoef, iter = 0)
  profile[i,1] <- tfit$loglik[2]
  delta <- c(0, acoef[i] - fit1$coefficients[2], 0)
  profile[i,2] <- fit1$loglik[2] - delta%*% imat %*% delta/2
}

matplot(acoef, profile*2, type = 'l', lwd = 2, lty = 1, xlab = 'Coeficiente por Idade',
        ylab = '2*loglik')










# Load packages JM and lattice
library("JM")
library("lattice")


###############
# Section 1.2 #
###############

# Figure 1.1
PBC.samp <- subset(pbc2, id %in% c(38,134,70,82, 51,90,68,93,
                                   39,148,173,200, 216,242,269,290))

xyplot(log(serBilir) ~ year | id, data = PBC.samp,
       type = c("p", "smooth"), lwd = 2, layout = c(4, 4),
       as.table = TRUE, ylab = "log serum Bilirubin",
       xlab = "Time (years)")


# Figure 1.2
AIDS.samp <- subset(aids, patient %in% c(82,152,213,236,332,
                                         335,353,407,410,452))
KM <- survfit(Surv(Time, death) ~ 1, data = aids.id)

par(mfrow = c(1, 2))
plot(KM, mark.time = FALSE, ylab = "Survival Probability",
     xlab = "Time (months)")
plot(CD4 ~ obstime, data = AIDS.samp, type = "n",
     xlab = "Time (months)", ylab = expression(sqrt("CD4 Cell Count")))
for (i in unique(AIDS.samp$patient))
  lines(CD4 ~ obstime, data = AIDS.samp[AIDS.samp$patient == i, ],
        lty = match(i, unique(AIDS.samp$patient)))


# Figure 1.3
p1 <- dotplot(id ~ I(time - Time), data = prothro,
              subset = death == 0, xlab = "Time (years)",
              ylab = "", main = "Follow-up Times before Censoring", xlim = c(-15, 0.5),
              scales = list(y = list(at = c(5, 25), labels = c("", ""))))

p2 <- dotplot(id ~ I(time - Time), data = prothro,
              subset = death == 1, xlab = "Time (years)",
              ylab = "", main = "Follow-up Times before Death", xlim = c(-15, 0.5),
              scales = list(y = list(at = c(5, 25), labels = c("", ""))))

plot(p1, split = c(1, 1, 2, 1), more = TRUE)
plot(p2, split = c(2, 1, 2, 1), more = FALSE)


# Figure 1.4
xyplot(pro ~ time | treat, groups = id, data = prothro,
       type = "l", col = 1, xlab = "Time (years)", ylab = "Prothrombin Index")


# Figure 1.5
# Note: The following code is based on the non publickly available
# Aortic Valve dataset, and therefore it is not executable
fm <- lmList(sqrt(AoGradient) ~ time | id, data = AoValv, na.action = na.exclude)
cf <- coef(fm)
names(cf) <- c("b0", "b1")
cf$id <- row.names(cf)
DataAV <- merge(cf, AoValv.id, by = "id")

f1 <- xyplot(b0 ~ Time, data = DataAV,
             type = c("p", "smooth"), col = 1, xlab = "Event Time",
             ylab = "Intercepts")

f2 <- xyplot(b1 ~ Time, data = DataAV,
             type = c("p", "smooth"), col = 1, xlab = "Event Time",
             ylab = "Slopes")

plot(f1, split = c(1, 1, 2, 1), more = TRUE)
plot(f2, split = c(2, 1, 2, 1), more = FALSE)


# Load packages JM
library("JM")


#################
# Section 2.2.2 #
#################

# random-intercepts model for the AIDS dataset
lmeFit.int <- lme(CD4 ~ obstime, random = ~ 1 | patient,
                  data = aids)

summary(lmeFit.int)

# marginal covariance matrix for Patient 12
margCov.int <- getVarCov(lmeFit.int, individuals = 12,
                         type = "marginal")

margCov.int
cov2cor(margCov.int[[1]])

# random-intercepts and random-slopes model for the AIDS dataset
lmeFit.slp <- lme(CD4 ~ obstime, random = ~ obstime | patient, data = aids)

summary(lmeFit.slp)

# marginal covariance matrix for Patient 12
margCov.slp <- getVarCov(lmeFit.slp, individuals = 12,
                         type = "marginal")

margCov.slp
cov2cor(margCov.slp[[1]])
#--------------------------------------------------------------------------------------------------------------------
library(survival)
args(coxph)

library(carData)
Rossi[1:5, 1:10]

mod.allison <- coxph(Surv(week, arrest) ~ fin + age + race + wexp + mar + paro + prio,
                     data = Rossi)
mod.allison

summary(mod.allison)

library(car)

Anova(mod.allison)

plot(survfit(mod.allison), ylim = c(0.7,1), xlab = 'Semanas',
     ylab = '')

Rossi.fin <- with(Rossi, data.frame(fin = c(0, 1),
                                    age = rep(mean(age), 2), race = rep(mean(race == 'other'), 2),
                                    wexp=rep(mean(wexp == "yes"), 2), mar=rep(mean(mar == "not married"), 2), paro=rep(mean(paro == "yes"), 2), prio=rep(mean(prio), 2)))

Rossi[1,]


mod.allison.4 <- coxph(Surv(week, arrest) ~ fin + age + prio, 
                       data = Rossi)

par(mfrow = c(2, 2))
plot(cox.zph(mod.allison.4))

cox.zph(mod.allison.4)

dfbeta <- residuals(mod.allison.4, type = 'dfbeta')
par(mfrow = c(2, 2))
for (j in 1:3) {
  plot(dfbeta[, j], ylab = names(coef(mod.allison.4))[j])
  abline(h = 0, lty = 2)
}

par(mfrow = c(2,2))
res <- residuals(mod.doug.1, type = 'martingale')

X <- as.matrix(baselimpa[, c("Idade", "Escolaridade")]) # matrix of covariates 
par(mfrow=c(2, 2)) 
for (j in 1:2) { # residual plots 
  plot(X[, j], res, xlab=c("Idade", "Escolaridade")[j], ylab="residuals") 
  abline(h=0, lty=2) 
  lines(lowess(X[, j], res, iter=0)) 
} 

b <- coef(mod.doug.1)[c(2,3)] # regression coefficients 
for (j in 1:2) { # component-plus-residual plots 
  plot(X[, j], b[j]*X[, j] + res, xlab=c("Idade", "Escolaridade")[j], 
       ylab="component+residual") 
  abline(lm(b[j]*X[, j] + res ~ X[, j]), lty=2) 
  lines(lowess(X[, j], b[j]*X[, j] + res, iter=0)) 
}




mod.doug <- coxph(Surv(Tempo, Status) ~ Idade + Sexo + Etnia + Escolaridade + Estado_Civil,
                  data = baselimpa)
mod.doug

summary(mod.doug)

Anova(mod.doug)

mod.doug.1 <- coxph(Surv(Tempo, Status) ~ Idade + Escolaridade + Estado_Civil,
                    data = baselimpa)

par(mfrow = c(2, 2))
plot(cox.zph(mod.doug.1))

cox.zph(mod.doug.1)

dfbeta <- residuals(mod.doug.1, type = 'dfbeta')
par(mfrow = c(2, 2))
for (j in 1:3) {
  plot(dfbeta[, j], ylab = names(coef(mod.doug.1))[j])
  abline(h = 0, lty = 2)
}
















#----------------------------------------------------------------------------------
fit_overall = survfit(Surv(Tempo, Status) ~ 1, data = df1)
print(fit_overall)

# Distribuição Exponencial
ajust1 <- survreg(Surv(Tempo, Status) ~ 1, dist = 'exponential', data = df1)
ajust1

# Distribuição Weibull
alpha <- exp(ajust1$coefficients[1])
alpha

ajust2 <- survreg(Surv(Tempo, Status) ~ 1, dist = 'weibull', data = df1)
ajust2

alpha <- exp(ajust2$coefficients[1])
alpha
gama <- 1/ajust2$scale
gama
cbind(gama, alpha)

# Distribuição Log-normal
ajust3 <- survreg(Surv(Tempo, Status) ~ 1, dist = 'lognorm', data = df1)
ajust3

# Distribuição Log-logístico
ajust4 <- survreg(Surv(Tempo, Status) ~ 1, dist = 'loglogistic', data = baselimpa)
ajust4

#
time <- fit_overall$time
st <- fit_overall$surv
ste <- exp(-time/9051.529)
stw <- exp(-(time/19118.27)^0.7583723)
stln <- pnorm((-log(time) + 9.722542)/3.552516)
stlog <-   1/(1 + (0.0001327866*time)^(0.5194981))
cbind(time, st, ste, stw, stln, stlog) %>% head()

#
par(mfrow = c(1,5))
plot(ste,st, pch = 16, ylim = range(c(0.7,1)), xlim = range(c(0.6,1)), ylab = 'S(t): Kaplan-Meier',
     xlab = 'S(t): Exponencial')
lines(c(0,1), c(0,1), type = 'l', lty = 1)

plot(stw,st, pch = 16, ylim = range(c(0.7,1)), xlim = range(c(0.6,1)), ylab = 'S(t): Kaplan-Meier',
     xlab = 'S(t): Weibull')
lines(c(0,1), c(0,1), type = 'l', lty = 1)

plot(stln,st, pch = 16, ylim = range(c(0.7,1)), xlim = range(c(0.6,1)), ylab = 'S(t): Kaplan-Meier',
     xlab = 'S(t): Log-normal')
lines(c(0,1), c(0,1), type = 'l', lty = 1)

plot(stlog,st, pch = 16, ylim = range(c(0.7,1)), xlim = range(c(0.6,1)), ylab = 'S(t): Kaplan-Meier',
     xlab = 'S(t): Log-logistico')
lines(c(0,1), c(0,1), type = 'l', lty = 1)

plot(stgom,st, pch = 16, ylim = range(c(0.7,1)), xlim = range(c(0.6,1)), ylab = 'S(t): Kaplan-Meier',
     xlab = 'S(t): Gompertz')
lines(c(0,1), c(0,1), type = 'l', lty = 1)

#
par(mfrow = c(1,5))
plot(fit_overall, conf.int = F, xlab = 'Tempo', ylab = 'S(t)', main = 'Exponencial', ylim = c(0.7,1))
lines(c(0,time), c(1,ste), lty = 2)

plot(fit_overall, conf.int = F, xlab = 'Tempo', ylab = 'S(t)', main = 'Weibull', ylim = c(0.7,1))
lines(c(0,time), c(1,stw), lty = 2)

plot(fit_overall, conf.int = F, xlab = 'Tempo', ylab = 'S(t)', main = 'Log-normal', ylim = c(0.7,1))
lines(c(0,time), c(1,stln), lty = 2)

plot(fit_overall, conf.int = F, xlab = 'Tempo', ylab = 'S(t)', main = 'Log-logistico', ylim = c(0.7,1))
lines(c(0,time), c(1,stlog), lty = 2)

plot(fit_overall, conf.int = F, xlab = 'Tempo', ylab = 'S(t)', main = 'Gompertz', ylim = c(0.7,1))
lines(c(0,time), c(1,stgom), lty = 2)




#-------------------------------------------------------------------------------
































