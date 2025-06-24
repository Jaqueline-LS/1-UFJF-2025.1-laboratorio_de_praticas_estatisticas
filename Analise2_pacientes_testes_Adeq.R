rm(list = ls(all = TRUE))

source("dados.R")
suppressMessages(library("dplyr"))
suppressMessages(library("MASS"))
suppressMessages(library("readr"))
library("kableExtra")

# Funções de MLG
source("envelope.R")

cores<-c("#FFC29A","#BAF3DE","#FF9B95","#C9E69E")
# ----------------------------- ANALISE 2----------------------------------


attach(analise2.new)
variaveis<-colnames(analise2.new)[c(2:13,15)]


teste.fisher.p<-numeric(length(variaveis))
teste.chisq.p<-numeric(length(variaveis))
residuos<-function(fit.model, dados)
{
  
  X <- model.matrix(fit.model)
  V <- fitted(fit.model)  # no Binomial, sao as proporcoes estimadas
  Vm <- diag(V)
  w <- fit.model$weights  # vetor de pesos
  W <- diag(w)
  H1 <- solve(t(X)%*%W%*%X)
  H <- sqrt(W)%*%X%*%H1%*%t(X)%*%sqrt(W)
  hii <- diag(H)  # vetor diagonal de H
  
  
  # Resíduos
  rD <- resid(fit.model, type= "deviance")
  fi<-summary(fit.model)$dispersion
  tD <- rD*sqrt(fi/(1-hii))   # residuo deviance padronizado
  rp1 <- resid(fit.model, type= "pearson")
  rP <- as.numeric(sqrt(fi)*rp1)
  tS <- rP/sqrt(V*(1 - hii))  # Residuo padronizado
  
  # LDi
  LD = hii*(tS^2)/(1-hii) # pode falhar de pi_i<0.1 ou pi_i>0.9. Neste caso, fazer  Ldi x pi_i
  #plot(tD, main="Resíduo")
 # plot(V,tD, main="Resíduo x Ajustado", xlab="Ajustado")
  return(list(tD=tD, tS=tS, hii=hii, V=V, LD=LD))
}
Analise.exp<-function(Y, nome.plot)
{
  jpeg(file=paste0("Resultados/", nome.plot,"_mosaic.jpg"), width = 1500, height = 800, quality = 100, pointsize = 20)
  par(mfrow=c(3,5), mar=c(1,1,1,1), cex.main=0.8)
  for(i in c(seq_along(variaveis)))
  {
    covar<-get(variaveis[i])
    with(analise2.new,
         mosaicplot(table(covar,Y), color = cores,  main=paste0(variaveis[i]),
                 ylab="", xlab="", cex.axis = 0.8, border="grey", las = 1))
    if(sum(table(covar, Y)<5)>=1)
    {
      teste.fisher.p[i]<-fisher.test(table(covar, Y))$p.value
      teste.chisq.p[i]<-NA
    }else{
      teste.fisher.p[i]<--NA
      teste.chisq.p[i]<-chisq.test(table(covar, Y), correct = F)$p.value
      
    }
  
  }
  
  
  boxplot(idade_atual_anos~Y,pch=20, 
       xlab="", 
       ylab="",
       main=paste0("Adequação (",nome.plot,") "), 
       cex.main=0.5,
       col=cores)
  dev.off()
  
  
  # Resuminho dos testes 
  tabela<-data.frame(variáveis=c(variaveis), fisher=round(teste.fisher.p,4), chi.quad=round(teste.chisq.p,4))
  colnames(tabela)<-c("Variável", "Fisher","Qui-quadrado")
  knitr::kable(tabela, caption = "Resultado dos testes (p-valor)", format = "latex", escape = FALSE, booktabs=T) %>%
    kable_styling(latex_options = c("hold_position", "scale_down"))
  
}

adeq.resp<-c("adeq_porcoes_dia_laticinios","adeq_porcoes_dia_cereais_total","adeq_porcoes_dia_cereais_saudaveis",
             "adeq_porcoes_dia_verduras_legumes","adeq_porcoes_dia_frutas","adeq_porcoes_dia_carnes_ovos",
             "adeq_porcoes_semana_embutidos","adeq_porcoes_semana_salgados_preparacoes","adeq_porcoes_semana_doces_salgadinhos_guloseimas")



names(adeq.resp)<-c("Leite_e_produtos_lácteos", "Cerais,_pães_e_túrbeculos",
                     "Cerais,_pães_e_túrbeculos_saudáveis",
                     "Verduras_e_legumes", "Frutas","Carne_e_ovos",
                     "Embutidos","Salgados_e_preparações", "Doces,_salgadinhos_e_guloseimas")



#--------------Leite e produtos lacteos-----------------------
Y<-get(adeq.resp[1])
Analise.exp(Y, nome.plot=names(adeq.resp)[1] )


modelo.completo<-Y~tea+di+tdah+dificuldade_motora+disfagia+epilepsia_farmacorressistente+tipo_focal_generalizada+rec_vomito_diarreia+constipacao+etiologia+paralisia_cerebral+atraso_desenvolvimento_sn+idade_atual_anos+sexo
fit<-glm(modelo.completo, family = binomial(link = "logit"))
summary(fit)


modelo<-Y~epilepsia_farmacorressistente
fit2<-glm(modelo, family = binomial(link = "logit"))
summary(fit2)


# Fazendo a seleção sobra 2 só etiologia com p-valor 0.057. E retirando ela a outra perde significancia


#--------------------Cereais

Y<-get(adeq.resp[2])
Analise.exp(Y, nome.plot=names(adeq.resp)[2] )


modelo.completo<-Y~tea+di+tdah+dificuldade_motora+disfagia+epilepsia_farmacorressistente+tipo_focal_generalizada+rec_vomito_diarreia+constipacao+etiologia+paralisia_cerebral+atraso_desenvolvimento_sn+idade_atual_anos+sexo
fit<-glm(modelo.completo, family = binomial(link = "logit"))
summary(fit)

fit.selecionado<-stepAIC(fit)
summary(fit.selecionado)


#--------------------Cereais saudaveis------

Y<-get(adeq.resp[3])
Analise.exp(Y, nome.plot=names(adeq.resp)[3] )


modelo.completo<-Y~tea+di+tdah+dificuldade_motora+disfagia+epilepsia_farmacorressistente+tipo_focal_generalizada+rec_vomito_diarreia+constipacao+etiologia+paralisia_cerebral+atraso_desenvolvimento_sn+idade_atual_anos+sexo
fit<-glm(modelo.completo, family = binomial(link = "logit"))
summary(fit)

fit.selecionado<-stepAIC(fit)
summary(fit.selecionado)

#------------------------------- Verduras e legumes-------------

Y<-get(adeq.resp[4])
nome.plot=names(adeq.resp)[4]
Analise.exp(Y, nome.plot )


modelo.completo<-Y~tea+di+tdah+dificuldade_motora+disfagia+epilepsia_farmacorressistente+tipo_focal_generalizada+rec_vomito_diarreia+constipacao+etiologia+paralisia_cerebral+atraso_desenvolvimento_sn+idade_atual_anos+sexo
fit<-glm(modelo.completo, family = binomial(link = "logit"))
summary(fit)


modelo<-Y~tea+constipacao+di+idade_atual_anos
fit2<-glm(modelo, family = binomial(link = "logit"))
summary(fit2)

jpeg(file=paste0("Resultados/", nome.plot,"_residuosglm.jpg"), width = 1500, height = 800, quality = 100, pointsize = 20)
par(mfrow=c(1,2))
resid<-residuos(fit2, analise2.new)
plot(resid$tD, main = "Resíduo Deviance Padronizado", ylab="Resíduo", pch=19)
envelope(fit2,"envel_bino_logit")
dev.off()

a<-summary(fit2)


tabela<-data.frame(a$coefficients)[-3]
row.names(tabela)
c1<-exp(tabela[,1])
c2<-exp(tabela[,1]-(1.96*tabela[,2]))
c3<-exp(tabela[,1]+(1.96*tabela[,2]))

colnames(tabela)<-c("Estimativa","Erro padrão", "p-valor")
knitr::kable(tabela, caption = "Coeficentes estimados", format = "latex", escape = FALSE, booktabs=T) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))

tabela2<-data.frame(c1,c2,c3)
colnames(tabela2)<-c("OR","LI", "LS")
row.names(tabela2)<-row.names(tabela)

tabela.final<-cbind(tabela,tabela2)
knitr::kable(tabela.final, caption = "Coeficientes estimados, erros padrão, p-valor do teste Wald, odds ratios estimadas (OR) e intervalo de confiança de 95% limite inferior
(LI) e limite superior (LS) ", format = "latex", escape = FALSE, booktabs=T) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))


#-----------------Frutas-------------

Y<-get(adeq.resp[5])
nome.plot=names(adeq.resp)[5] 
Analise.exp(Y,nome.plot)


modelo.completo<-Y~tea+di+tdah+dificuldade_motora+disfagia+epilepsia_farmacorressistente+tipo_focal_generalizada+rec_vomito_diarreia+constipacao+etiologia+paralisia_cerebral+atraso_desenvolvimento_sn+idade_atual_anos+sexo
fit<-glm(modelo.completo, family = binomial(link = "logit"))
summary(fit)


modelo<-Y~tipo_focal_generalizada+atraso_desenvolvimento_sn
fit2<-glm(modelo, family = binomial(link = "logit"))
summary(fit2)


jpeg(file=paste0("Resultados/", nome.plot,"_residuosglm.jpg"), width = 1500, height = 800, quality = 100, pointsize = 20)
par(mfrow=c(1,2))
resid<-residuos(fit2, analise2.new)
plot(resid$tD, main = "Resíduo Deviance Padronizado", ylab="Resíduo", pch=19)
envelope(fit2,"envel_bino_logit")
dev.off()

a<-summary(fit2)


tabela<-data.frame(a$coefficients)[-3]
row.names(tabela)
colnames(tabela)<-c("Estimativa","Erro padrão", "p-valor")

c1<-exp(tabela[,1])
c2<-exp(tabela[,1]-(1.96*tabela[,2]))
c3<-exp(tabela[,1]+(1.96*tabela[,2]))
tabela2<-data.frame(c1,c2,c3)
colnames(tabela2)<-c("OR","LI", "LS")
row.names(tabela2)<-row.names(tabela)

tabela.final<-cbind(tabela,tabela2)
knitr::kable(tabela.final, caption = "Coeficientes estimados, erros padrão, p-valor do teste Wald, odds ratios estimadas (OR) e intervalo de confiança de 95% limite inferior
(LI) e limite superior (LS) ", format = "latex", escape = FALSE, booktabs=T) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))



#----------------------------------Carnes e Ovos------------

Y<-get(adeq.resp[6])
nome.plot=names(adeq.resp)[6] 
Analise.exp(Y,nome.plot)


modelo.completo<-Y~tea+di+tdah+dificuldade_motora+disfagia+epilepsia_farmacorressistente+tipo_focal_generalizada+rec_vomito_diarreia+constipacao+etiologia+paralisia_cerebral+atraso_desenvolvimento_sn+idade_atual_anos+sexo
fit<-glm(modelo.completo, family = binomial(link = "logit"))
summary(fit)



#---------------------------------- Embutidos-------------


Y<-get(adeq.resp[7])
nome.plot=names(adeq.resp)[7] 
Analise.exp(Y,nome.plot)

modelo.completo<-Y~tea+di+tdah+dificuldade_motora+disfagia+epilepsia_farmacorressistente+tipo_focal_generalizada+rec_vomito_diarreia+constipacao+etiologia+paralisia_cerebral+atraso_desenvolvimento_sn+idade_atual_anos+sexo
fit<-glm(modelo.completo, family = binomial(link = "logit"))
summary(fit)


modelo.completo<-Y~di
fit<-glm(modelo.completo, family = binomial(link = "logit"))
summary(fit)


#----------------------------- Salgados e preparações--------------

Y<-get(adeq.resp[8])
nome.plot=names(adeq.resp)[8] 
Analise.exp(Y,nome.plot)

modelo.completo<-Y~tea+di+tdah+dificuldade_motora+disfagia+epilepsia_farmacorressistente+tipo_focal_generalizada+rec_vomito_diarreia+constipacao+etiologia+paralisia_cerebral+atraso_desenvolvimento_sn+idade_atual_anos+sexo
fit<-glm(modelo.completo, family = binomial(link = "logit"))
summary(fit)


modelo.completo<-Y~tipo_focal_generalizada
fit<-glm(modelo.completo, family = binomial(link = "logit"))
summary(fit)



#---------------Doces, salgadinhos e guloseimas---------------

Y<-get(adeq.resp[9])
nome.plot=names(adeq.resp)[9] 
Analise.exp(Y,nome.plot)

modelo.completo<-Y~tea+di+tdah+dificuldade_motora+disfagia+epilepsia_farmacorressistente+tipo_focal_generalizada+rec_vomito_diarreia+constipacao+etiologia+paralisia_cerebral+atraso_desenvolvimento_sn+idade_atual_anos+sexo
fit<-glm(modelo.completo, family = binomial(link = "logit"))
summary(fit)


#-------------------Doces, adequacao diaria

Y<-adeq_porcoes_dia_doces_salgadinhos_guloseimas
nome.plot= "Doces, guloseimas-Diaria"


modelo.completo<-Y~etiologia+tipo_focal_generalizada
fit2<-glm(modelo.completo, family = binomial(link = "logit"))
summary(fit2)


jpeg(file=paste0("Resultados/", nome.plot,"_residuosglm.jpg"), width = 1500, height = 800, quality = 100, pointsize = 20)
par(mfrow=c(1,2))
resid<-residuos(fit2, analise2.new)
plot(resid$tD, main = "Resíduo Deviance Padronizado", ylab="Resíduo", pch=19)
envelope(fit2,"envel_bino_logit")
dev.off()

a<-summary(fit2)


tabela<-data.frame(a$coefficients)[-3]
row.names(tabela)
colnames(tabela)<-c("Estimativa","Erro padrão", "p-valor")

c1<-exp(tabela[,1])
c2<-exp(tabela[,1]-(1.96*tabela[,2]))
c3<-exp(tabela[,1]+(1.96*tabela[,2]))
tabela2<-data.frame(c1,c2,c3)
colnames(tabela2)<-c("OR","LI", "LS")
row.names(tabela2)<-row.names(tabela)

tabela.final<-cbind(tabela,tabela2)
knitr::kable(tabela.final, caption = "Coeficientes estimados, erros padrão, p-valor do teste Wald, odds ratios estimadas (OR) e intervalo de confiança de 95% limite inferior
(LI) e limite superior (LS) ", format = "latex", escape = FALSE, booktabs=T) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))

