library(readxl)
library(ggplot2)
library(PerformanceAnalytics)
library(psych)
library(MASS)
library(ggplot2)

hiv <- read_excel("HIV.xlsx")
View(hiv)

##Visualizando dados para ver distribuiçãoe possíveis correlações
# pairs.panels(hiv[,-9], 
#              method = "pearson", # correlation method
#              hist.col = "#650cff",
#              density = TRUE,  # show density plots
#              ellipses = TRUE # show correlation ellipses
# )
# 
# chart.Correlation(hiv, histogram=TRUE, pch=19)
# 
# hist(hiv$idade, breaks = 20)
# hist(hiv$pn, breaks = 20)
# hist(hiv$imc, breaks = 20)
# hist(hiv$homa, breaks = 20)
# hist(hiv$cd4, breaks = 20)
# hist(hiv$ct, breaks = 20)
# 
# plot(density(hiv$idade))
# plot(density(hiv$pn))
# plot(density(hiv$imc))
# plot(density(hiv$homa))
# plot(density(hiv$cd4))
# plot(density(hiv$ct))

#Parte1: Comparando os grupos para testar similaridade antes da intervenção
hiv$sex <- gsub("0","Fem", hiv$sex)
hiv$sex <- gsub("1","Masc", hiv$sex)
hiv$ip <- gsub("0","Sem IP", hiv$ip)
hiv$ip <- gsub("1","Com IP", hiv$ip)

compare_pn <- (t.test(hiv$pn~hiv$ip))$p.value
compare_imc <- (t.test(hiv$imc~hiv$ip))$p.value

compare_idade <- (wilcox.test(hiv$idade~hiv$ip))$p.value
compare_homa <- (wilcox.test(hiv$homa~hiv$ip))$p.value
compare_cd4 <- (wilcox.test(hiv$cd4~hiv$ip))$p.value
compare_ct <- (wilcox.test(hiv$ct~hiv$ip))$p.value

table(hiv$sex, hiv$ip)
compare_sex <- (chisq.test(hiv$sex, hiv$ip, correct=FALSE))$p.value

#Parte 2: Estabelecer se há relação entre IMC e CT e Aplicar modelo de regressão linear simples em ct e imc
lm <- lm(ct~imc, data=hiv)
summary(lm)
sum_lm <- summary(lm)

ggplot(hiv, aes(x = imc, y = ct, color = ip)) +
  geom_point(data = hiv, shape = 19) +
  geom_smooth(data = hiv, method = lm, se = FALSE, color = hiv$ip) +
  scale_color_manual(values = c("chocolate1", "slateblue3"), name = "Tratamento", breaks = c("Sem IP", "Com IP"), labels = c("Controle", "IP"))+
  labs(title = "Correlacao entre o Colesterol Total e o IMC", x = "IMC", y = "Colesterol Total" ) +
  geom_abline(intercept = -70.110, slope = 14.482, color="black", size=.5)

#Parte3: Aplicar modelo de regressão linear multipla para verificar interferencia de ip no anterior
lm2 <- lm(ct~imc + ip, data=hiv)
summary(lm2)
sum_lm2 <- summary(lm2)

ggplot(hiv, aes(x = imc, y = ct, color = ip)) +
  geom_point(data = hiv, shape = 19) +
  geom_smooth(data = hiv, method = lm, se = FALSE) +
  scale_color_manual(values = c("chocolate1", "slateblue3"), name = "Tratamento", breaks = c("Sem IP", "Com IP"), labels = c("Controle", "IP"))+
  labs(title = "Correlacao entre o Colesterol Total e o IMC", x = "IMC", y = "Colesterol Total" ) +
  geom_abline(intercept = -26.525, slope = 11.208, color="slateblue3", size=.5) +
  geom_abline(intercept = -53.786, slope = 14.031, color="chocolate1", size=.5)

##experimental

semip <- subset(hiv, ip == "Sem IP")
lm3 <- lm(ct~imc, data=semip)
summary(lm3)
sum_lm3 <- summary(lm3)

comip <- subset(hiv,ip == "Com IP")
lm4 <- lm(ct~imc, data=comip)
summary(lm4)
sum_lm4 <- summary(lm4)

##experimental

#Parte4:
