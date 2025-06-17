source("dados.R")
suppressMessages(library("dplyr"))
suppressMessages(library("MASS"))
suppressMessages(library("readr"))
library("kableExtra")

# Funções de MLG
source("C:/ufjf/2024.3/MlG/MLG/Funcoes/envelope.R")

cores<-c("#FF9B95","#C9E69E","#BAF3DE","#FFC29A")
# ----------------------------- ANALISE 2----------------------------------


# Consumo adequado ou inadequado de verduras e legumes
variaveis<-colnames(analise2.new)[c(2:11)]
attach(analise2.new)

teste.mood.p<-numeric(length(variaveis)+2)
teste.wilcox.p<-numeric(length(variaveis)+2)
teste.aleat.p<-numeric(length(variaveis)+2)

Analise.exp<-function(Y, nome.plot)
{
  jpeg(file=paste0("Resultados/", nome.plot,"_boxplots.jpg"), width = 1500, height = 800, quality = 100, pointsize = 20)
  par(mfrow=c(3,5), mar=c(2,2,2,1),
      cex.main=0.8)
  for(i in seq_along(variaveis))
  {
    with(analise2.new,
         boxplot(Y~get(variaveis[i]), 
                 col=cores, main=paste0(variaveis[i]),
                 ylab="", xlab=""))
    ind<-which(get(variaveis[i])=="S") # Pega as que são "S
    X1<-Y[ind]
    X2<-Y[-ind]
    teste.mood.p[i]<-mood.test(X1,X2)$p.value
    teste.wilcox.p[i]<-wilcox.test(X1,X2)$p.value 
    teste.aleat.p[i]<-exactRankTests::perm.test(X1,X2)$p.value
  }
  
  colnames(analise2.new)[c(12,13,15)]
  with(analise2.new,
       boxplot(Y~get(colnames(analise2.new)[12]), 
               col=cores, main=paste0(colnames(analise2.new)[12]),
               ylab="", xlab=""))
  ind<-which(get(colnames(analise2.new)[12])=="F") # Pega as que são "F"
  X1<-Y[ind]
  X2<-Y[-ind]
  teste.mood.p[11]<-mood.test(X1,X2)$p.value
  teste.wilcox.p[11]<-wilcox.test(X1,X2)$p.value # n e m são menores que 20. Ver o valor tabelado.
  teste.aleat.p[11]<-exactRankTests::perm.test(X1,X2)$p.value
  
  with(analise2.new,
       boxplot(Y~get(colnames(analise2.new)[15]), 
               col=cores, main=paste0(colnames(analise2.new)[15]),
               ylab="", xlab=""))
  ind<-which(get(colnames(analise2.new)[15])=="F") # Pega as que são "F"
  X1<-Y[ind]
  X2<-Y[-ind]
  teste.mood.p[12]<-mood.test(X1,X2)$p.value
  teste.wilcox.p[12]<-wilcox.test(X1,X2)$p.value # n e m são menores que 20. Ver o valor tabelado.
  teste.aleat.p[12]<-exactRankTests::perm.test(X1,X2)$p.value
  
  with(analise2.new,
       boxplot(Y~get(colnames(analise2.new)[13]), 
               col=cores, main=paste0(colnames(analise2.new)[13]),
               ylab="", xlab=""))
  
  plot(Y~idade_atual_anos,pch=20, 
       xlab="", 
       ylab="",
       main=paste0("Idade vs. Porções(",nome.plot,") "))
  dev.off()
  
  # Kruskal-Wallis, que é uma extensão do teste Wilcoxon-Mann-Whitney 
  # para mais amostras independentes
  kruskal.test(Y,etiologia)
  
  # Não deu significativo
  
  kruskal.p<-c(rep(NA,12),kruskal.test(Y,etiologia)$p.value)
  
  # Resuminho dos testes 
  tabela<-data.frame(variáveis=c(variaveis,colnames(analise2.new)[c(12,15,13)]), mood=c(teste.mood.p,NA), wilcox=c(teste.wilcox.p,NA),aleatorização=c(teste.aleat.p,NA), kruskal=kruskal.p)
  
  knitr::kable(tabela, caption = "Resultado dos testes", format = "latex", escape = FALSE, booktabs=T) %>%
    kable_styling(latex_options = c("hold_position", "scale_down"))
  
}


somas.resp<-list("soma_porcoes_dia_laticinios","soma_porcoes_dia_cereais_total", "soma_porcoes_dia_cereais_saudaveis",
                 "soma_porcoes_dia_verduras_legumes", "soma_porcoes_dia_frutas", "soma_porcoes_dia_carnes_ovos",
                 "soma_porcoes_semana_embutidos", "soma_porcoes_semana_salgados_preparacoes", 'soma_porcoes_semana_doces_salgadinhos_guloseimas')
names(somas.resp)<-c("Leite_e_produtos_lácteos", "Cerais,_pães_e_túrbeculos",
                     "Cerais,_pães_e_túrbeculos_saudáveis",
                     "Verduras_e_legumes", "Frutas","Carne_e_ovos",
                     "Embutidos","Salgados_e_preparações", "Doces,_salgadinhos_e_guloseimas")
length(somas.resp)

analise.res<-function(fit,i){
  # Resíduo studentizado
  X <- model.matrix(fit)
  n <- nrow(X)
  p <- ncol(X)
  
  H <- X%*%solve(t(X)%*%X)%*%t(X)
  h <- diag(H)
  si <- lm.influence(fit)$sigma
  sigma2<-summary(fit)$sigma
  r <- resid(fit)
  res.press <- r/(si*sqrt(1-h))
  res.stu<- r/sqrt(sigma2*(1-h))
  ajustado<-fit$fitted.values
  jpeg(file=paste0("Resultados/", names(somas.resp)[i],"_residuos.jpg"), width = 1500, height = 500, quality = 100, pointsize = 20)
  par(mfrow=c(1,3), mar=c(4,4,3,3))
  plot(res.stu, pch=19, main="Resíduos Studentizados") 
  plot(ajustado, res.stu, pch=19,main="Resíduos x Valores ajustados")
  envelope(fit2,"envel_norm")
 
  dev.off()
  a<-summary(fit)
  
  tabela<-data.frame(a$coefficients)[-3]
  colnames(tabela)<-c("Estimativa","Erro padrão", "p-valor")
  
  knitr::kable(tabela, caption = "Coeficentes estimados", format = "latex", escape = FALSE, booktabs=T) %>%
    kable_styling(latex_options = c("hold_position", "scale_down"))
  
  
}
#------------------------------Soma de Leites e produtos lácteos--------------------
Y<-get(somas.resp[[1]])
Analise.exp(Y,nome.plot=names(somas.resp)[[1]])
# Modelo com as variáveis que tiveram teste de medias significativos
fit.inicial<-lm(Y ~  dificuldade_motora,
                  data = analise2.new)

summary(fit.inicial)

modelo.completo<-Y~tea+di+tdah+dificuldade_motora+disfagia+epilepsia_farmacorressistente+tipo_focal_generalizada+rec_vomito_diarreia+constipacao+etiologia+paralisia_cerebral+atraso_desenvolvimento_sn+idade_atual_anos+sexo
fit.inicial<-lm(modelo.completo, data=analise2.new)
summary(fit.inicial)


modelo.selecionado<-Y~dificuldade_motora+disfagia+sexo
fit2<-lm(modelo.selecionado, data=analise2.new)
summary(fit2)

analise.res(fit2,1)

#--------------------------Soma Cereais, pães e turbéculos---------

Y<-get(somas.resp[[2]])
Analise.exp(Y,nome.plot=names(somas.resp)[[2]])

modelo.completo<-Y~tea+di+tdah+dificuldade_motora+disfagia+epilepsia_farmacorressistente+tipo_focal_generalizada+rec_vomito_diarreia+constipacao+etiologia+paralisia_cerebral+atraso_desenvolvimento_sn+idade_atual_anos+sexo
fit.inicial<-lm(modelo.completo, data=analise2.new)
summary(fit.inicial)

modelo.selecionado<-Y~idade_atual_anos
fit2<-lm(modelo.selecionado, data=analise2.new)
summary(fit2)

#--------------------------Soma Cereais, pães e turbéculos saudáveis---------

Y<-get(somas.resp[[3]])
Analise.exp(Y,nome.plot=names(somas.resp)[[3]])

modelo.completo<-Y~tea+di+tdah+dificuldade_motora+disfagia+epilepsia_farmacorressistente+tipo_focal_generalizada+rec_vomito_diarreia+constipacao+etiologia+paralisia_cerebral+atraso_desenvolvimento_sn+idade_atual_anos+sexo
fit.inicial<-lm(modelo.completo, data=analise2.new)
summary(fit.inicial)

modelo.selecionado<-Y~dificuldade_motora
fit2<-lm(modelo.selecionado, data=analise2.new)
summary(fit2)


#--------------------------Soma verduras e legumes---------

Y<-get(somas.resp[[4]])
Analise.exp(Y,nome.plot=names(somas.resp)[[4]])

modelo.completo<-Y~tea+di+tdah+dificuldade_motora+disfagia+epilepsia_farmacorressistente+tipo_focal_generalizada+rec_vomito_diarreia+constipacao+etiologia+paralisia_cerebral+atraso_desenvolvimento_sn+idade_atual_anos+sexo
fit.inicial<-lm(modelo.completo, data=analise2.new)
summary(fit.inicial)

# Com a paralisia cerebral o resíduo da observação 23 é infito. Só uma observação tem essa caract
modelo.selecionado<-Y~di+dificuldade_motora+constipacao+sexo
fit2<-lm(modelo.selecionado, data=analise2.new)
summary(fit2)

analise.res(fit2,4)



#------------------- Frutas------------------
Y<-get(somas.resp[[5]])
Analise.exp(Y,nome.plot=names(somas.resp)[[5]])

modelo.completo<-Y~tea+di+tdah+dificuldade_motora+disfagia+epilepsia_farmacorressistente+tipo_focal_generalizada+rec_vomito_diarreia+constipacao+etiologia+paralisia_cerebral+atraso_desenvolvimento_sn+idade_atual_anos+sexo
fit.inicial<-lm(modelo.completo, data=analise2.new)
summary(fit.inicial)

modelo.selecionado<-Y~dificuldade_motora+atraso_desenvolvimento_sn
fit2<-lm(modelo.selecionado, data=analise2.new)
summary(fit2)

analise.res(fit2,5)



#------------------- Carnes e ovos------------------
Y<-get(somas.resp[[6]])
Analise.exp(Y,nome.plot=names(somas.resp)[[6]])

modelo.completo<-Y~tea+di+tdah+dificuldade_motora+disfagia+epilepsia_farmacorressistente+tipo_focal_generalizada+rec_vomito_diarreia+constipacao+etiologia+paralisia_cerebral+atraso_desenvolvimento_sn+idade_atual_anos+sexo
fit.inicial<-lm(modelo.completo, data=analise2.new)
summary(fit.inicial)

modelo.selecionado<-Y~dificuldade_motora+rec_vomito_diarreia+constipacao
fit2<-lm(modelo.selecionado, data=analise2.new)
summary(fit2)
# Não chegou em um modelo significativo

#------------------- Embutidos------------------
Y<-get(somas.resp[[7]])
Analise.exp(Y,nome.plot=names(somas.resp)[[7]])

modelo.completo<-Y~tea+di+tdah+dificuldade_motora+disfagia+epilepsia_farmacorressistente+tipo_focal_generalizada+rec_vomito_diarreia+constipacao+etiologia+paralisia_cerebral+atraso_desenvolvimento_sn+idade_atual_anos+sexo
fit.inicial<-lm(modelo.completo, data=analise2.new)
summary(fit.inicial)

modelo.selecionado<-Y~sexo
fit2<-lm(modelo.selecionado, data=analise2.new)
summary(fit2)
# Também não chegou em um modelo significativo

#------------------------

#------------------- Salgados e preparações------------------
Y<-get(somas.resp[[8]])
Analise.exp(Y,nome.plot=names(somas.resp)[[8]])

modelo.completo<-Y~tea+di+tdah+dificuldade_motora+disfagia+epilepsia_farmacorressistente+tipo_focal_generalizada+rec_vomito_diarreia+constipacao+etiologia+paralisia_cerebral+atraso_desenvolvimento_sn+idade_atual_anos+sexo
fit.inicial<-lm(modelo.completo, data=analise2.new)
summary(fit.inicial)

modelo.selecionado<-Y~0+rec_vomito_diarreia+atraso_desenvolvimento_sn+idade_atual_anos
fit2<-lm(modelo.selecionado, data=analise2.new)
summary(fit2)

analise.res(fit2,8)
#------------------------



#------------------- Doces, salgados e preparações------------------
Y<-get(somas.resp[[9]])
Analise.exp(Y,nome.plot=names(somas.resp)[[9]])

modelo.completo<-Y~tea+di+tdah+dificuldade_motora+disfagia+epilepsia_farmacorressistente+tipo_focal_generalizada+rec_vomito_diarreia+constipacao+etiologia+paralisia_cerebral+atraso_desenvolvimento_sn+idade_atual_anos+sexo
fit.inicial<-lm(modelo.completo, data=analise2.new)
summary(fit.inicial)

modelo.selecionado<-Y~tipo_focal_generalizada
fit2<-lm(modelo.selecionado, data=analise2.new)
summary(fit2)
analise.res(fit2,9)
#------------------------






