source("dados.R")
suppressMessages(library("dplyr"))
suppressMessages(library("MASS"))
suppressMessages(library("readr"))
library("kableExtra")

# Funções de MLG
source("C:/ufjf/2024.3/MlG/MLG/Funcoes/envelope.R")

cores<-c("#FF9B95","#C9E69E","#BAF3DE","#FFC29A")
# ----------------------------- ANALISE 2----------------------------------


# Consumo adequado ou inadequado de verduras e legumes # Fazer separado para 13 e 12 e 15
variaveis<-colnames(analise2.new)[c(2:11)]
attach(analise2.new)

par(mfrow=c(3,5), mar=c(2,2,1,1))
teste.mood.p<-numeric(length(variaveis)+2)
teste.wilcox.p<-numeric(length(variaveis)+2)
teste.ansari<-numeric(length(variaveis)+2)

for(i in seq_along(variaveis))
{
  with(analise2.new,
       boxplot(soma_porcoes_dia_verduras_legumes~get(variaveis[i]), 
               col=cores, main=paste0(variaveis[i]),
               ylab="", xlab=""))
  ind<-which(get(variaveis[i])=="S") # Pega as que são "S
  X<-soma_porcoes_dia_verduras_legumes[ind]
  Y<-soma_porcoes_dia_verduras_legumes[-ind]
  teste.mood.p[i]<-mood.test(X,Y)$p.value
  teste.wilcox.p[i]<-(wilcox.test(X,Y)$p.value) # n e m são menores que 20. Ver o valor tabelado.
  teste.ansari[i]<-ansari.test(X,Y)$p.value
}

colnames(analise2.new)[c(12,13,15)]
with(analise2.new,
     boxplot(soma_porcoes_dia_verduras_legumes~get(colnames(analise2.new)[12]), 
             col=cores, main=paste0(colnames(analise2.new)[12]),
             ylab="", xlab=""))
ind<-which(get(colnames(analise2.new)[12])=="F") # Pega as que são "F"
X<-soma_porcoes_dia_verduras_legumes[ind]
Y<-soma_porcoes_dia_verduras_legumes[-ind]
teste.mood.p[11]<-mood.test(X,Y)$p.value
teste.wilcox.p[11]<-(wilcox.test(X,Y)$p.value) # n e m são menores que 20. Ver o valor tabelado.
teste.ansari[11]<-ansari.test(X,Y)$p.value

with(analise2.new,
     boxplot(soma_porcoes_dia_verduras_legumes~get(colnames(analise2.new)[15]), 
             col=cores, main=paste0(colnames(analise2.new)[15]),
             ylab="", xlab=""))
ind<-which(get(colnames(analise2.new)[15])=="F") # Pega as que são "F"
X<-soma_porcoes_dia_verduras_legumes[ind]
Y<-soma_porcoes_dia_verduras_legumes[-ind]
teste.mood.p[12]<-mood.test(X,Y)$p.value
teste.wilcox.p[12]<-(wilcox.test(X,Y)$p.value) # n e m são menores que 20. Ver o valor tabelado.
teste.ansari[12]<-ansari.test(X,Y)$p.value

with(analise2.new,
     boxplot(soma_porcoes_dia_verduras_legumes~get(colnames(analise2.new)[13]), 
             col=cores, main=paste0(colnames(analise2.new)[13]),
             ylab="", xlab=""))


#plot(soma_porcoes_dia_verduras_legumes~idade_atual_anos,pch=19, xlab="Idade atual em anos", ylab="Soma de porções por dia de verduras e legumes")

# Kruskal-Wallis, que é uma extensão do teste Wilcoxon-Mann-Whitney 
# para mais amostras independentes
kruskal.test(soma_porcoes_dia_verduras_legumes,etiologia)

# Não deu significativo

kruskal.p<-c(rep(NA,12),kruskal.test(soma_porcoes_dia_verduras_legumes,etiologia)$p.value)

# Resuminho dos testes para as variáveis explicativas em relação a soma de verduras e legumes
tabela<-data.frame(variáveis=c(variaveis,colnames(analise2.new)[c(12,15,13)]), mood=c(teste.mood.p,NA), wilcox=c(teste.wilcox.p,NA), kruskal=kruskal.p)

knitr::kable(tabela, caption = "Resultado dos testes", format = "latex", escape = FALSE, booktabs=T) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))



# Não poderia ter aplicado o teste U da Mann-Whitney para verduras e legumes 
# Para a comparação entre adequado e farmaco resistente.


# Modelo com as variáveis que tiveram teste de medias significativos
fit.inicial<-lm(soma_porcoes_dia_verduras_legumes ~ tea + 
                  dificuldade_motora + constipacao  + 
                  tipo_focal_generalizada +
                  paralisia_cerebral + sexo, data = analise2.new)

summary(fit.inicial)

# Sem tea
fit.inicial<-lm(soma_porcoes_dia_verduras_legumes ~  
                  dificuldade_motora  + atraso_desenvolvimento_sn  , data = analise2.new)

summary(fit.inicial)

fit.inicial<-lm(soma_porcoes_dia_verduras_legumes ~  
                  dificuldade_motora  , data = analise2.new)


summary(fit.inicial)

# Ao retirar as variáveis não significativas sobra só a dificuldade motora no final-----------------------------

#------------------------------Seleção de variáveis-------------------------

modelo.completo<-soma_porcoes_dia_verduras_legumes~tea+di+tdah+dificuldade_motora+disfagia+epilepsia_farmacorressistente+tipo_focal_generalizada+rec_vomito_diarreia+constipacao+etiologia+paralisia_cerebral+atraso_desenvolvimento_sn+idade_atual_anos+sexo
fit.inicial<-lm(modelo.completo, data=analise2.new)
summary(fit.inicial)

modelo.selecionado<-stepAIC(fit.inicial, trace=1, direction = c("backward"))
summary(modelo.selecionado)
ncol(model.matrix(modelo.selecionado))

modelo2<-soma_porcoes_dia_verduras_legumes ~ di + tdah + 
  dificuldade_motora + tipo_focal_generalizada + constipacao + 
  paralisia_cerebral + sexo

summary(modelo.selecionado)

# modelo sem tdah
modelo2<-soma_porcoes_dia_verduras_legumes ~ di + dificuldade_motora + tipo_focal_generalizada + constipacao  + sexo
fit2<-lm(modelo2, data=analise2.new)
summary(fit2)

# Modelo sem tipo_focal e paralisia
modelo2<-soma_porcoes_dia_verduras_legumes ~ di + dificuldade_motora  + constipacao  + sexo
fit2<-lm(modelo2, data=analise2.new)
summary(fit2)

fit.selecionado<-fit2

# Resíduo studentizado
X <- model.matrix(fit.selecionado)
n <- nrow(X)
p <- ncol(X)

H <- X%*%solve(t(X)%*%X)%*%t(X)
h <- diag(H)
si <- lm.influence(fit.selecionado)$sigma
sigma2<-summary(fit.selecionado)$sigma
r <- resid(fit.selecionado)
res.press <- r/(si*sqrt(1-h))
res.stu<- r/sqrt(sigma2*(1-h))

par(mfrow=c(1,1), mar=c(2,2,2,1))
plot(res.stu, pch=19) # Não apresenta nenhum padrão

which.max(res.stu)

# Quem é a observação 26? 
analise2.new[26,c(1:15,24)]
summary(analise2.new[c(1:15,24)]) # Ela possui um valor alto na resposta

# Nenhum ponto de alavanca
plot(h, ylim = c(0,1))
abline(h=2*p/n, lty=2,lwd=2 ,col="maroon")


analise2.new[23,c(1:15,24)]
summary(analise2.new[,c(1:15,24)])
# Essa observação é a única que possui a condição de paralisia cerebral


envelope(fit2,"envel_norm")

which.max(res.stu)

# Quem é a observação 26? 
indices<-c(3,5,9,15,24)
analise2.new[26,indices]
summary(analise2.new[indices]) # Ela possui um valor alto na resposta

# Nenhum ponto de alavanca
plot(h, ylim = c(0,1))
abline(h=2*p/n, lty=2,lwd=2 ,col="maroon")



#------------- Interpretação do modelo---------------
a<-summary(fit2)

tabela<-data.frame(a$coefficients)[-3]
colnames(tabela)<-c("Estimativa","Erro padrão", "p-valor")

knitr::kable(tabela, caption = "Coeficentes estimados", format = "latex", escape = FALSE, booktabs=T) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))






#------------------------------------- Modelo logístico-----------------------------
analise2.new$adeq_porcoes_dia_verduras_legumes<-factor(analise2.new$adeq_porcoes_dia_verduras_legumes, levels=c("A","I"))
Y<-adeq_porcoes_dia_verduras_legumes
modelo<-Y ~ di + dificuldade_motora  + constipacao  + sexo

fit<-glm(modelo, family = binomial(link = "logit"))
envelope(fit,"envel_bino_logit")

# Resíduos

modelo.completo<-Y~tea+di+tdah+dificuldade_motora+disfagia+epilepsia_farmacorressistente+tipo_focal_generalizada+rec_vomito_diarreia+constipacao+etiologia+paralisia_cerebral+atraso_desenvolvimento_sn+idade_atual_anos+sexo


selecaoModeloBinomial<-function(dados, modelo, ligacao, step="AIC")
{
  n<-nrow(dados)
  fit1<-try(switch(ligacao,
                   logit = glm(modelo, family=binomial(logit)),
                   probit = glm(modelo, family=binomial(link=probit)),
                   cauchit = glm(modelo, family=binomial(link=cauchit)),
                   log = glm(modelo, family=binomial(link=log)),
                   cloglog = glm(modelo, family=binomial(link=cloglog)),
                   stop("Link não reconhecido")), silent = T)
  summary(fit1)
  
  aux<-sum(class(fit1) != "try-error")
  if(step=="JAQ")
  {
    fit<-try(stepJAQ(fit1, trace = 0), silent = T)
  }else{
    fit<-try(stepAIC(fit1, trace = 0), silent = T)
    
  }
  aux2<-sum(class(fit) != "try-error")
  if( aux!=0  && aux2!=0 )
  {
    fit.model<-fit
  }else{
    fit.model<-0
  }
  return(fit.model)
}

ResultadosBinom<-function(dados, modelo, ligacao, tipo, alfa=0.05, step="AIC")
{
  n<-nrow(dados)
  familia<-paste0("Binomial-Link(",ligacao,")")
  print(familia)
  fits<-selecaoModeloBinomial(dados, modelo, ligacao, step)
  if(is.numeric(fits))
  {
    bic<-NA
    qualidade<-"Não Rodou"
    modelo.s<-"-----"
    
  }else{
    bic<-as.numeric(round(AIC(fits,k=log(n)),6))
    modelo.s<-deparse(formula(fits))
    teste<-try(envelope(fits,tipo), silent=T)
    aux<-sum(class(teste) != "try-error")
    if(aux==0)
    {
      qualidade<-"Não Rodou"
    }else{
      qualidade<-teste[[1]]
    }
  }
  return(data.frame(Family=familia,
                    QQplot=qualidade,
                    BIC=bic,
                    Modelo=modelo.s))
  
}


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



