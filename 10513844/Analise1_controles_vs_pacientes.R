source("dados.R")
suppressMessages(library("dplyr"))
suppressMessages(library("MASS"))
suppressMessages(library("readr"))
library("kableExtra")

# Funções de MLG
source("envelope.R")

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
  plot(tD, main="Resíduo")
  plot(V,tD, main="Resíduo x Ajustado", xlab="Ajustado")
  return(list(tD=tD, tS=tS, hii=hii, V=V, LD=LD))
}



cores<-c("#FF9B95","#C9E69E","#BAF3DE","#FFC29A")
# ----------------------------- ANALISE 1----------------------------------
attach(analise1.new)


View(analise1.new)
nomes.var<-c("Leite_e_produtos_lácteos", "Cerais,_pães_e_túrbeculos",
                     "Cerais,_pães_e_túrbeculos_saudáveis",
                     "Verduras_e_legumes", "Frutas","Carne_e_ovos",
                     "Embutidos","Salgados_e_preparações", "Doces,_salgadinhos_e_guloseimas")
teste.mood.p<-numeric(length(nomes.var))
teste.wilcox.p<-numeric(length(nomes.var))
teste.aleat.p<-numeric(length(nomes.var))
soma.E.vs.C<-function(i)
{
  Y=Y<-get(somas.resp[[i]])
  nome<-nomes.var[i]
  jpeg(file=paste0("Resultados/", nome,"EvsC_boxplots.jpg"), width = 1500, height = 800, quality = 100, pointsize = 20)
  
  par(mfrow=c(1,3), mar=c(4,4,3,3), oma=c(0,0,2,0), pch=19)  # oma cria espaço para o título superior
  
  boxplot(Y~epilepsia, ylab=nome, col=cores[3:4])
  boxplot(Y~sexo,  ylab=nome, col=cores[3:4])
  plot(Y~idade_atual_anos, ylab=nome)
  
  mtext(paste("Análise do consumo alimentar por epilepsia, sexo e idade"), 
        outer=TRUE, cex=1.2, line=0.6)
  mtext(paste("Grupo alimentar: ",nome), 
        outer=TRUE, cex=1, line=-1)
  dev.off()
  ind<-which(epilepsia=="E")
  X1<-Y[ind]
  X2<-Y[-ind]
  teste.mood.p<-mood.test(X1,X2)$p.value
  teste.wilcox.p<-wilcox.test(X1,X2)$p.value 
  teste.aleat.p<-exactRankTests::perm.test(X1,X2)$p.value

  fit1<-lm(Y~sexo+epilepsia+idade_atual_anos, data = analise1.new)
  summary(fit1)
  return(testes=c(teste.mood.p,teste.wilcox.p,teste.aleat.p))
  
}
somas.resp<-list("soma_porcoes_dia_laticinios","soma_porcoes_dia_cereais_total", "soma_porcoes_dia_cereais_saudaveis",
                 "soma_porcoes_dia_verduras_legumes", "soma_porcoes_dia_frutas", "soma_porcoes_dia_carnes_ovos",
                 "soma_porcoes_semana_embutidos", "soma_porcoes_semana_salgados_preparacoes", 'soma_porcoes_semana_doces_salgadinhos_guloseimas')
soma.E.vs.C(i=1)
soma.E.vs.C(i=2)
soma.E.vs.C(i=3)
soma.E.vs.C(i=4)
soma.E.vs.C(i=5)
soma.E.vs.C(i=6)
soma.E.vs.C(i=7)
soma.E.vs.C(i=8)
soma.E.vs.C(i=9)

testes<-t(sapply(1:9, FUN=soma.E.vs.C))
colnames(testes)<-c("mood","wilcox","aleatorização")
rownames(testes)<-nomes.var
knitr::kable(testes, caption = "Resultado dos testes (p-valor) ", format = "latex", escape = FALSE, booktabs=T) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))

Y=soma_porcoes_semana_salgados_preparacoes
fit1<-lm(Y~epilepsia+idade_atual_anos, data = analise1.new)
summary(fit1)



#------------------Regressão Logística---------------

adequacao.E.vs.C<-function(i)
{
  Y<-get(adeq.resp[i])
  nome<-nomes.var[i]
  jpeg(file=paste0("Resultados/", nome,"EvsC_mosaic.jpg"), width = 1500, height = 800, quality = 100, pointsize = 20)
  
  par(mfrow=c(1,3), mar=c(3,4,3,3), oma=c(0,0,2,0), pch=19)  # oma cria espaço para o título superior
  
  mosaicplot(Y~epilepsia, col=cores[3:4], main="")
  mosaicplot(Y~sexo,col=cores[3:4], main="")
  boxplot(idade_atual_anos~Y, col=cores[3:4])
  
  mtext(paste("Adequação alimentar por epilepsia, sexo e idade"), 
        outer=TRUE, cex=1.2, line=0.6)
  mtext(paste("Grupo alimentar: ",nome), 
        outer=TRUE, cex=1, line=-1)
  
  dev.off()
  if(sum(table(epilepsia, Y)>=5)<4)
  {
    Fisher<-fisher.test(table(epilepsia, Y))$p.value
    Chisq<-NA
  }else{
    Fisher<-NA
    Chisq<-chisq.test(table(epilepsia, Y), correct = F)$p.value
    
  }
  
  fit1<-glm(Y~sexo+epilepsia, data = analise1.new, family = binomial(link = "logit"))
  summary(fit1)
  envelope(fit1,"envel_bino_logit")
  residuos(fit1, analise1.new)
  return(c(Fisher=Fisher, Chisq=Chisq))
}
adeq.resp<-c("adeq_porcoes_dia_laticinios","adeq_porcoes_dia_cereais_total","adeq_porcoes_dia_cereais_saudaveis",
             "adeq_porcoes_dia_verduras_legumes","adeq_porcoes_dia_frutas","adeq_porcoes_dia_carnes_ovos",
             "adeq_porcoes_semana_embutidos","adeq_porcoes_semana_salgados_preparacoes","adeq_porcoes_semana_doces_salgadinhos_guloseimas")
adequacao.E.vs.C(i=1)
adequacao.E.vs.C(i=2)
adequacao.E.vs.C(i=3)
adequacao.E.vs.C(i=4)
adequacao.E.vs.C(i=5)
adequacao.E.vs.C(i=6)
adequacao.E.vs.C(i=7)
adequacao.E.vs.C(i=8)
adequacao.E.vs.C(i=9)

testes<-t(sapply(1:9, FUN=adequacao.E.vs.C))
colnames(testes)<-c("Fisher","Qui-quadrado")
rownames(testes)<-nomes.var
knitr::kable(testes, caption = "Resultado dos testes (p-valor) ", format = "latex", escape = FALSE, booktabs=T) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))


mosaicplot(table(epilepsia,adeq_porcoes_dia_verduras_legumes))
chisq.test(table(epilepsia,adeq_porcoes_dia_verduras_legumes))

