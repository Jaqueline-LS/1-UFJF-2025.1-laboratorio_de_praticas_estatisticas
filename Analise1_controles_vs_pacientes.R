source("dados.R")
suppressMessages(library("dplyr"))
suppressMessages(library("MASS"))
suppressMessages(library("readr"))
library("kableExtra")

# Funções de MLG
source("C:/ufjf/2024.3/MlG/MLG/Funcoes/envelope.R")

cores<-c("#FF9B95","#C9E69E","#BAF3DE","#FFC29A")
# ----------------------------- ANALISE 1----------------------------------
attach(analise1.new)

View(analise1.new)

Y<-soma_porcoes_dia_verduras_legumes
boxplot(Y~epilepsia)
boxplot(Y~sexo)
plot(Y~idade_atual_anos)

ind<-which(epilepsia=="E")
X<-soma_porcoes_dia_verduras_legumes[ind]
Y<-soma_porcoes_dia_verduras_legumes[-ind]
teste.mood.p<-mood.test(X,Y)$p.value
teste.wilcox.p<-(wilcox.test(X,Y)$p.value) # n e m são menores que 20. Ver o valor tabelado.
teste.ansari<-ansari.test(X,Y)$p.value

ind<-which(sexo=="F")
X<-soma_porcoes_dia_verduras_legumes[ind]
Y<-soma_porcoes_dia_verduras_legumes[-ind]
teste.mood.p<-mood.test(X,Y)$p.value
teste.wilcox.p<-(wilcox.test(X,Y)$p.value) # n e m são menores que 20. Ver o valor tabelado.
teste.ansari<-ansari.test(X,Y)$p.value

Y<-soma_porcoes_dia_verduras_legumes
fit1<-lm(Y~sexo+epilepsia, data = analise1.new)
summary(fit1)
