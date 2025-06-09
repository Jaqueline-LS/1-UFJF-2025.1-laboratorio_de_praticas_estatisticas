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


soma.E.vs.C<-function(Y)
{
  boxplot(Y~epilepsia)
  boxplot(Y~sexo)
  plot(Y~idade_atual_anos)
  
  ind<-which(epilepsia=="E")
  X1<-Y[ind]
  X2<-Y[-ind]
  cat("Mediana pacientes: ", median(X1), "\nMediana controles: ", median(X2),"\n")
  teste.mood.p<-mood.test(X1,X2)$p.value
  teste.wilcox.p<-(wilcox.test(X1,X2)$p.value) # n e m são menores que 20. Ver o valor tabelado.
  teste.ansari<-ansari.test(X1,X2)$p.value
  cat("Teste Wood: ", teste.mood.p, "\nTeste U: ", teste.wilcox.p)
  
  fit1<-lm(Y~sexo+epilepsia+idade_atual_anos, data = analise1.new)
  summary(fit1)
  
}
soma.E.vs.C(Y=soma_porcoes_dia_laticinios)
soma.E.vs.C(Y=soma_porcoes_dia_cereais_total)
soma.E.vs.C(Y=soma_porcoes_dia_cereais_saudaveis)
soma.E.vs.C(Y=soma_porcoes_dia_verduras_legumes)
soma.E.vs.C(Y=soma_porcoes_dia_frutas)
soma.E.vs.C(Y=soma_porcoes_dia_carnes_ovos)
soma.E.vs.C(Y=soma_porcoes_semana_embutidos)
soma.E.vs.C(Y=soma_porcoes_semana_salgados_preparacoes)

soma.E.vs.C(Y=soma_porcoes_semana_doces_salgadinhos_guloseimas)
fit1<-lm(Y~epilepsia, data = analise1.new)
summary(fit1)


Y=soma_porcoes_semana_salgados_preparacoes
fit1<-lm(Y~epilepsia+idade_atual_anos, data = analise1.new)
summary(fit1)



#------------------Regressão Logística---------------

adequacao.E.vs.C<-function(Y)
{
  mosaicplot(Y~epilepsia)
  mosaicplot(Y~sexo)
  boxplot(idade_atual_anos~Y)
  if(sum(table(epilepsia, Y)>=5)<4)
  {
    Teste<-fisher.test(table(epilepsia, Y))
  }else{
    Teste<-chisq.test(table(epilepsia, Y), correct = F)
    
  }
  Teste
  fit<-glm(Y~sexo+epilepsia+idade_atual_anos, data = analise1.new, family = binomial(link = "logit"))
  summary(fit)
  fit<-glm(Y~sexo+epilepsia, data = analise1.new, family = binomial(link = "logit"))
  summary(fit)
  
  
}

adequacao.E.vs.C(Y=adeq_porcoes_dia_verduras_legumes)
