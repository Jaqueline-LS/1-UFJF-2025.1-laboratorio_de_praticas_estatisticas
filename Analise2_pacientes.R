source("dados.R")
suppressMessages(library("dplyr"))
suppressMessages(library("MASS"))
suppressMessages(library("readr"))
library("kableExtra")

# Funções de MLG
source("C:/ufjf/2024.3/MlG/MLG/Funcoes/envelope.R")

cores<-c("#C9E69E","#FF9B95","#FFC29A","#BAF3DE")
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


# Consumo adequado ou inadequado de verduras e legumes # Fazer separado para 13 e 12 e 15
variaveis<-colnames(analise2.new)[c(2:11)]
attach(analise2.new)

par(mfrow=c(3,5), mar=c(2,1,1,1))
teste.mood.p<-numeric(length(variaveis)+2)
teste.wilcox.p<-numeric(length(variaveis)+2)
teste.ansari<-numeric(length(variaveis)+2)

for(i in seq_along(variaveis))
{
  with(analise2.new,
       boxplot(soma_porcoes_dia_verduras_legumes~get(variaveis[i]), 
               col=cores, main=paste0(variaveis[i]),
               ylab="Porções-VerdurasLegumes"))
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
               ylab="Porções-VerdurasLegumes"))
ind<-which(get(colnames(analise2.new)[12])=="F") # Pega as que são "F"
X<-soma_porcoes_dia_verduras_legumes[ind]
Y<-soma_porcoes_dia_verduras_legumes[-ind]
teste.mood.p[11]<-mood.test(X,Y)$p.value
teste.wilcox.p[11]<-(wilcox.test(X,Y)$p.value) # n e m são menores que 20. Ver o valor tabelado.
teste.ansari[11]<-ansari.test(X,Y)$p.value
  
with(analise2.new,
     boxplot(soma_porcoes_dia_verduras_legumes~get(colnames(analise2.new)[15]), 
             col=cores, main=paste0(colnames(analise2.new)[15]),
             ylab="Porções-VerdurasLegumes"))
ind<-which(get(colnames(analise2.new)[15])=="F") # Pega as que são "F"
X<-soma_porcoes_dia_verduras_legumes[ind]
Y<-soma_porcoes_dia_verduras_legumes[-ind]
teste.mood.p[12]<-mood.test(X,Y)$p.value
teste.wilcox.p[12]<-(wilcox.test(X,Y)$p.value) # n e m são menores que 20. Ver o valor tabelado.
teste.ansari[12]<-ansari.test(X,Y)$p.value

with(analise2.new,
     boxplot(soma_porcoes_dia_verduras_legumes~get(colnames(analise2.new)[13]), 
             col=cores, main=paste0(colnames(analise2.new)[13]),
             ylab="Porções-VerdurasLegumes"))
plot(soma_porcoes_dia_verduras_legumes~idade_atual_anos,pch=19)

# Kruskal-Wallis, que é uma extensão do teste Wilcoxon-Mann-Whitney 
# para mais amostras independentes
kruskal.test(soma_porcoes_dia_verduras_legumes,etiologia)

# Não deu significativo


# Resuminho dos testes para as variáveis explicativas em relação a soma de verduras e legumes
data.frame(variáveis=c(variaveis,colnames(analise2.new)[c(12,15)]), mood=teste.mood.p, wilcox=teste.wilcox.p, ansari=teste.ansari)

# Não poderia ter aplicado o teste U da Mann-Whitney para verduras e legumes 
# Para a comparação entre adequado e farmaco resistente.


modelo<-soma_porcoes_dia_verduras_legumes~tea+di+tdah+dificuldade_motora+disfagia+epilepsia_farmacorressistente+tipo_focal_generalizada+rec_vomito_diarreia+constipacao+etiologia+paralisia_cerebral+atraso_desenvolvimento_sn+idade_atual_anos+sexo
fit.inicial<-lm(modelo, data=analise2.new)
summary(fit.inicial)
modelo.selecionado<-stepAIC(fit.inicial, trace=1)
summary(modelo.selecionado)


fit.inicial<-lm(modelo,soma_porcoes_dia_verduras_legumes ~ di + tdah + 
  dificuldade_motora + tipo_focal_generalizada + constipacao + 
  paralisia_cerebral + sexo, data = analise2.new)

