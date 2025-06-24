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
  
  print(Teste)
  
  fit1<-glm(Y~sexo+epilepsia+idade_atual_anos, data = analise1.new, family = binomial(link = "logit"))
  summary(fit1)
  
  
  
}
adequacao.E.vs.C(Y=adeq_porcoes_dia_laticinios)
Y=adeq_porcoes_dia_laticinios
fit1<-glm(Y~sexo, data = analise1.new, family = binomial(link = "logit"))
summary(fit1)
# Somente a variável sexo é significativa

adequacao.E.vs.C(Y=adeq_porcoes_dia_cereais_total)
adequacao.E.vs.C(Y=adeq_porcoes_dia_cereais_saudaveis)
adequacao.E.vs.C(Y=adeq_porcoes_dia_verduras_legumes)
Y=adeq_porcoes_dia_verduras_legumes
fit1<-glm(Y~sexo+epilepsia, data = analise1.new, family = binomial(link = "logit"))
summary(fit1)
adequacao.E.vs.C(Y=adeq_porcoes_dia_frutas)
adequacao.E.vs.C(Y=adeq_porcoes_dia_carnes_ovos)
adequacao.E.vs.C(Y=adeq_porcoes_semana_embutidos)
Y=adeq_porcoes_semana_embutidos
fit1<-glm(Y~sexo+epilepsia, data = analise1.new, family = binomial(link = "logit"))
summary(fit1)
adequacao.E.vs.C(Y=adeq_porcoes_semana_salgados_preparacoes)

adequacao.E.vs.C(Y=adeq_porcoes_semana_doces_salgadinhos_guloseimas)
adequacao.E.vs.C(Y=adeq_porcoes_dia_doces_salgadinhos_guloseimas)
Y=adeq_porcoes_dia_doces_salgadinhos_guloseimas
fit1<-glm(Y~epilepsia+idade, data = analise1.new, family = binomial(link = "logit"))
summary(fit1)




adeq.resp<-c("adeq_porcoes_dia_laticinios","adeq_porcoes_dia_cereais_total","adeq_porcoes_dia_cereais_saudaveis",
             "adeq_porcoes_dia_verduras_legumes","adeq_porcoes_dia_frutas","adeq_porcoes_dia_carnes_ovos",
             "adeq_porcoes_semana_embutidos","adeq_porcoes_semana_salgados_preparacoes","adeq_porcoes_semana_doces_salgadinhos_guloseimas")


names(adeq.resp)<-c("Leite_e_produtos_lácteos", "Cerais,_pães_e_túrbeculos",
                    "Cerais,_pães_e_túrbeculos_saudáveis",
                    "Verduras_e_legumes", "Frutas","Carne_e_ovos",
                    "Embutidos","Salgados_e_preparações", "Doces,_salgadinhos_e_guloseimas")



for(i in c(seq_along(adeq.resp)))
{
  Y<-get(adeq.resp[i])

  if(sum(table(epilepsia, Y)<5)>=1) #Todas categorias devem ter uma freq maior ou igual que 5
  {
    teste.fisher.p[i]<-fisher.test(table(epilepsia, Y))$p.value
    teste.chisq.p[i]<-NA
  }else{
    teste.fisher.p[i]<--NA
    teste.chisq.p[i]<-chisq.test(table(epilepsia, Y), correct = F)$p.value
    
  }
  
}

# Resuminho dos testes 
tabela<-data.frame(variáveis=names(adeq.resp), fisher=round(teste.fisher.p,4), chi.quad=round(teste.chisq.p,4))
colnames(tabela)<-c("Variável", "Fisher","Qui-quadrado")
knitr::kable(tabela, caption = "Resultado dos testes (p-valor)", format = "latex", escape = FALSE, booktabs=T) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))



somas.resp<-c("soma_porcoes_dia_laticinios","soma_porcoes_dia_cereais_total", "soma_porcoes_dia_cereais_saudaveis",
                 "soma_porcoes_dia_verduras_legumes", "soma_porcoes_dia_frutas", "soma_porcoes_dia_carnes_ovos",
                 "soma_porcoes_semana_embutidos", "soma_porcoes_semana_salgados_preparacoes", 'soma_porcoes_semana_doces_salgadinhos_guloseimas')
names(somas.resp)<-c("Leite_e_produtos_lácteos", "Cerais,_pães_e_túrbeculos",
                     "Cerais,_pães_e_túrbeculos_saudáveis",
                     "Verduras_e_legumes", "Frutas","Carne_e_ovos",
                     "Embutidos","Salgados_e_preparações", "Doces,_salgadinhos_e_guloseimas")
length(somas.resp)

teste.mood.p<-numeric(length(somas.resp))
teste.wilcox.p<-numeric(length(somas.resp))
teste.aleat.p<-numeric(length(somas.resp))
for(i in seq_along(somas.resp))
{
  Y<-get(somas.resp[i])
  ind<-which(epilepsia=="E") # Pega as que são "E"
  X1<-Y[ind]
  X2<-Y[-ind]
  teste.mood.p[i]<-mood.test(X1,X2)$p.value
  teste.wilcox.p[i]<-wilcox.test(X1,X2)$p.value 
  teste.aleat.p[i]<-exactRankTests::perm.test(X1,X2)$p.value
}

# Resuminho dos testes 
tabela<-data.frame(variáveis=names(somas.resp), mood=c(teste.mood.p), wilcox=c(teste.wilcox.p),aleatorização=c(teste.aleat.p))

knitr::kable(tabela, caption = "Resultado dos testes", format = "latex", escape = FALSE, booktabs=T) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))
