source("dados.R")
suppressMessages(library("dplyr"))
suppressMessages(library("MASS"))
suppressMessages(library("readr"))
library("kableExtra")

# Funções de MLG
source("C:/ufjf/2024.3/MlG/MLG/Funcoes/envelope.R")

cores<-c("#C9E69E","#FF9B95","#FFC29A","#BAF3DE")



# Para as variáveis de soma, foram aplicados o teste de Mediana Wilcox.test
# Verificar se as amsotras atendem aos presupostos

# Isso foi o que foi feito para cada grupo alimentar para as variáveis de adequação
T2<-chisq.test(table(analise1$epilepsia, analise1$adeq_porcoes_dia_laticinios), correct = F)
fisher.test(table(analise1$epilepsia,analise1$adeq_porcoes_dia_cereais_saudaveis))
fisher.test(table(analise1$epilepsia, analise1$adeq_porcoes_dia_laticinios))

boxplot(analise1$idade_atual_anos~analise1$sexo)
boxplot(analise1$soma_cereais_saudaveis~analise1$epilepsia)
boxplot(analise1$soma_cereais_saudaveis~analise1$epilepsia)
boxplot(analise1$soma_laticinios~analise1$epilepsia)
boxplot(analise1$soma_frutas~analise1$epilepsia)
boxplot(analise1$soma_carnes_ovos~analise1$epilepsia)







# ----------------------------- ANALISE 2----------------------------------

# Retirar as colunas que não serão utilizadas
analise2.new<-analise2[,-c(c(1,19,20,23,24,27,28,31,32,35,36,39,40,43,44,47,48))]
str(analise2.new)

# Transformando em fatores as variáveis categóricas com o número correto de levels
analise2.new$tea<-factor(as.character(analise2.new$tea), levels=c("N","S"))
analise2.new$di<-factor(as.character(analise2.new$di), levels=c("N","S"))
analise2.new$tdah<-factor(as.character(analise2.new$tdah), levels=c("N","S"))
analise2.new$dificuldade_motora<-factor(as.character(analise2.new$dificuldade_motora), levels=c("N","S"))
analise2.new$paralisia_cerebral<-factor(as.character(analise2.new$paralisia_cerebral), levels=c("N","S"))
analise2.new$disfagia<-factor(as.character(analise2.new$disfagia), levels=c("N","S"))
analise2.new$rec_vomito_diarreia<-factor(as.character(analise2.new$rec_vomito_diarreia), levels=c("N","S"))
analise2.new$constipacao<-factor(as.character(analise2.new$constipacao), levels=c("N","S"))
analise2.new$atraso_desenvolvimento_sn<-factor(as.character(analise2.new$constipacao), levels=c("N","S"))
analise2.new$disfagia<-factor(as.character(analise2.new$disfagia), levels=c("N","S"))
analise2.new$epilepsia_farmacorressistente<-factor(as.character(analise2.new$epilepsia_farmacorressistente), levels=c("N","S"))
analise2.new$tipo_focal_generalizada<-factor(as.character(analise2.new$tipo_focal_generalizada), levels=c("F","G"))
analise2.new$etiologia<-factor(as.character(analise2.new$etiologia), levels=c("E","G","I"))

View(analise2.new)
str(analise2.new)


# Consumo adequado ou inadequado de verduras e legumes # Fazer separado para 13 e 12
variaveis<-colnames(analise2.new)[c(2:11,15)]
attach(analise2.new)
par(mfrow=c(4,4), mar=c(2,1,1,1))
for(i in seq_along(variaveis))
{
  with(analise2.new,
       boxplot(soma_porcoes_dia_verduras_legumes~get(variaveis[i]), 
               col=cores, main=paste0(variaveis[i]),
               ylab="Porções-VerdurasLegumes"))
  ind<-which(get(variaveis[i])=="S") # Pega as que são "S
  X<-soma_porcoes_dia_verduras_legumes[ind]
  Y<-soma_porcoes_dia_verduras_legumes[-ind]
  stats::mood.test(X,Y)
  stats::wilcox.test(X,Y) # n e m são menores que 20. Ver o valor tabelado.

}

ind.tea<-which(analise2.new$tea=="S")
X<-analise2.new$soma_porcoes_dia_verduras_legumes[ind.tea]
Y<-analise2.new$soma_porcoes_dia_verduras_legumes[-ind.tea]
hist(X)
hist(Y)
median(X)
median(Y)
stats::mood.test(X,Y)
stats::wilcox.test(X,Y) # n e m são menores que 20. Ver o valor tabelado.

# Postos de X
postos<-analise2.new |>
  select(soma_porcoes_dia_verduras_legumes, tea)|>
  arrange(soma_porcoes_dia_verduras_legumes)
postos.x<-which(postos$tea=="S") 

R.x<- sum(postos.x) # soma dos postos de X, a função wilcox.test dá o valor corrigido(menos o minimo)
n<-length(X)
m<-length(Y)
N<-n+m
Total.rank<-N*(N+1)/2 
Wp.025<-278 # Tabela A7 Conover
Wp.975<- n*(N+1)-Wp.025

# A estatística foi 427, a região de não rejeição é [278, 444]. 
# Não há evidências significativas para rejeitar a hipótese nula ao nível de 5%.
# Ou seja, não parece haver diferenças nas porções diárias de verduras e legumes entre os pacientes com TEA e sem.

# A variável é continua

require(exactRankTests)

perm.test(X,Y, alternative = "two.sided")

#-----------------------------------Modelos-------------------------------------------

# Adequação das porções de verduras e legumes pelas características clínicas que tiveram diferença significativa
fit=glm(adeq_porcoes_dia_verduras_legumes~tea+dificuldade_motora+constipacao, data=analise2.new, family = binomial((link="logit")))
summary(fit)

# Porções de verduras e legumes explicadas pelas características clinicas
fit2=glm(soma_porcoes_dia_verduras_legumes~tea+dificuldade_motora+constipacao+tipo_focal_generalizada,data=analise2.new, family=gaussian(link = "log"))
summary(fit2)

fit2=glm(soma_porcoes_dia_verduras_legumes~dificuldade_motora,data=analise2.new, family=gaussian)
summary(fit2)

qqnorm(fit2$residuals)
qqline(fit2$residuals)

attach(analise2.new)


modelo<-soma_porcoes_dia_verduras_legumes~tea+tdah+di+constipacao+dificuldade_motora+disfagia+epilepsia_farmacorressistente+tipo_focal_generalizada+rec_vomito_diarreia+etiologia+paralisia_cerebral+atraso_desenvolvimento_sn+idade_atual_anos+sexo

selecaoModeloNormal<-function(dados, modelo, ligacao)
{
  fit1<-try(switch(ligacao,
                   identity = glm(modelo, family=gaussian),
                   log = glm(modelo, family=gaussian(link=log)),
                   inverse = glm(modelo, family=gaussian(link=inverse)),
                   stop("Link não reconhecido")), silent = T)
  
  aux<-sum(class(fit1) != "try-error")
  if(aux==0)
  {
    fit.model<-0
  }else{
    fit.model<-stepAIC(fit1, trace=1)
  }
  return(fit.model)
}

ResultadosNormal<-function(dados, modelo, ligacao, tipo, alfa=0.05)
{
  n<-nrow(dados)
  familia<-paste0("Normal-Link(",ligacao,")")
  print(familia)
  fits<-selecaoModeloNormal(dados, modelo, ligacao)
  if(is.numeric(fits))
  {
    bic<-"Não Rodou"
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

normal<-rbind(
  ResultadosNormal(analise2.new, modelo, ligacao = "identity", tipo ="envel_norm"),
  ResultadosNormal(analise2.new, modelo, ligacao = "log", tipo ="envel_norm_log"),
  ResultadosNormal(analise2.new, modelo, ligacao = "inverse", tipo ="envel_norm_inverse"))

fit.4.inicial<-lm(modelo, data=analise2.new)
summary(fit.4.inicial)
fit.4<-lm(soma_porcoes_dia_verduras_legumes ~ tdah + di + constipacao + dificuldade_motora + tipo_focal_generalizada + paralisia_cerebral + sexo, data=analise2.new)
summary(fit.4)
a<-summary(fit.4)
fit.4<-lm( soma_porcoes_dia_verduras_legumes ~  di + constipacao + dificuldade_motora + tipo_focal_generalizada + paralisia_cerebral + sexo, data=analise2.new)
a<-summary(fit.4)

tabela<-data.frame(a$coefficients)[-3]
colnames(tabela)<-c("Estimativa","Erro padrão", "p-valor")
envelope(fit.4,tipo="envel_norm")

knitr::kable(tabela, caption = "Coeficentes estimados", format = "latex", escape = FALSE, booktabs=T) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))
 

# Resíduo studentizado
X <- model.matrix(fit.4)
n <- nrow(X)
p <- ncol(X)

H <- X%*%solve(t(X)%*%X)%*%t(X)
h <- diag(H)
si <- lm.influence(fit.4)$sigma
r <- resid(fit.4)
res.stu <- r/(si*sqrt(1-h))
plot(res.stu, pch=19) # Não apresenta nenhum padrão

# Verificar quem é o ponto com alto resíduo
which.max(res.stu)
analise2.new[26,]
summary(analise2.new)
# O modelo foi validado, ele parece atender aos pressupostos de normalidade dos resíduos e de homocedasticidade

#---------- Interpretação
# A porção de verduras e legumes consumidos pelos pacientes aumenta em média 1.9192 unidades 
# para os que apresentam dificuldade motora em relação aos que não apresentam.




#---------------------- Porções de cereais, pães e tubérculos saudáveis------------

modelo<-soma_porcoes_dia_cereais_saudaveis~dificuldade_motora+tea+constipacao+disfagia+tipo_focal_generalizada+











