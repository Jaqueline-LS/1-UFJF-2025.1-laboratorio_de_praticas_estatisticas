source("dados.R")
library("readr")
library("glmnet")


# Consumo adequado ou inadequado de verduras e legumes
attach(analise2.new)

t(summary(analise2.new[,c(2:13,15)]))

summary(analise2.new)
str(analise2.new)


#Exemplo para seu caso:
# matriz de preditores
vegetable_intake<-analise2.new$soma_porcoes_dia_verduras_legumes


library(car)
modelo_lm<-lm(vegetable_intake~idade_atual_anos + sexo + idade_1a_crise_meses + tea + di + tdah + dificuldade_motora + 
                disfagia + rec_vomito_diarreia + constipacao + atraso_desenvolvimento_sn +
                epilepsia_farmacorressistente+ tipo_focal_generalizada + etiologia, data=analise2.new)
summary(modelo_lm)
vif(modelo_lm)

X <- model.matrix(
  vegetable_intake ~ idade_atual_anos + sexo + idade_1a_crise_meses + tea + di + tdah + dificuldade_motora + 
    disfagia + rec_vomito_diarreia + constipacao + atraso_desenvolvimento_sn +
    epilepsia_farmacorressistente+ tipo_focal_generalizada + etiologia
  ,
  data = analise2.new
)[,-1]
ncol(X)

y <- vegetable_intake

colnames(X)
#Agora definir penalização:
penalty <- rep(1, ncol(X))

penalty[colnames(X) == "idade_atual_anos"] <- 0
penalty[colnames(X) == "sexoM"] <- 0
#(se sexo gerar dummy com outro nome, ajuste o nome corretamente)
#Rodar LASSO com validação cruzada:

set.seed(123)

cvfit <- cv.glmnet(
  X,
  y,
  alpha = 1, #1  LASSO
  penalty.factor = penalty,
  standardize = TRUE
)
# Ver lambda:
cvfit$lambda.min
cvfit$lambda.1se
plot(cvfit)
# Coeficientes selecionados:
betas<-coef(cvfit, s = "lambda.min")
betas

ajuste.min<-lm(vegetable_intake~idade_atual_anos+sexo+tea+di+tdah+dificuldade_motora+
     rec_vomito_diarreia+constipacao+epilepsia_farmacorressistente+etiologia)
summary(ajuste.min)


ajuste.min.2<-lm(vegetable_intake~idade_atual_anos+sexo+tea+dificuldade_motora
               +constipacao+epilepsia_farmacorressistente+etiologia)
summary(ajuste.min.2)
vif(ajuste.min.2)
# não parece ter problemas de multicolinearidade 

coef(cvfit, s = "lambda.1se")
#Depois pega as variáveis com coeficiente ≠ 0 e roda o refit:
lm(vegetable_intake ~ idade_atual_anos + sexo, data=analise2.new)


# Eu usaria lambda.1se no seu caso porque n=37 → tende a gerar modelo mais #parcimonioso.

#(se essas forem as selecionadas)
# 
# No fluxo:
#   LASSO → seleciona variáveis → refit OLS
# o objetivo do refit é principalmente:
#   •	obter coeficientes interpretáveis 
# •	erros padrão 
# •	ICs 
# •	p-valores 
# Quando remover? Após o refit ( o estudo abaixo de bootstrap pode ajudar)
# Só se houver:
#   •	coeficiente absurdamente instável 
# •	colinearidade severa 
# •	problema clínico de interpretação 
# •	variável muito rara (ex.: dysphagia n=4) 
# Exemplo:
#   dysphagia = 4 pacientes
# Mesmo se o LASSO selecionar, eu avaliaria remover por baixa frequência e reportaria isso como limitação.
#

# Você pode fazer assim:
#   1.	reamostrar os 37 indivíduos com reposição; 
# 2.	rodar o LASSO em cada amostra bootstrap; 
# 3.	registrar quais variáveis clínicas foram selecionadas; 
# 4.	calcular a frequência de seleção.
# library(glmnet)

set.seed(123)

B <- 2000

formula_lasso <-  vegetable_intake ~ idade_atual_anos + sexo + idade_1a_crise_meses +
  tea + di + tdah + dificuldade_motora + 
  disfagia + rec_vomito_diarreia + constipacao + atraso_desenvolvimento_sn +
  epilepsia_farmacorressistente+ tipo_focal_generalizada + etiologia

X <- model.matrix(formula_lasso, data = analise2.new)[, -1]
y <- vegetable_intake

# idade e sexo fixos
penalty <- rep(1, ncol(X))
penalty[colnames(X) == "idade_atual_anos"] <- 0
penalty[grepl("^sex", colnames(X))] <- 0

selected <- matrix(0, nrow = B, ncol = ncol(X))
colnames(selected) <- colnames(X)

for (b in 1:B) {
  
  idx <- sample(1:nrow(analise2.new), replace = TRUE)
  
  Xb <- X[idx, ]
  yb <- y[idx]
  
  cvfit <- cv.glmnet(
    Xb, yb,
    alpha = 1,
    penalty.factor = penalty,
    standardize = TRUE
  )
  
  coef_b <- coef(cvfit, s = "lambda.1se")
  
  vars_b <- rownames(coef_b)[as.numeric(coef_b) != 0]
  vars_b <- setdiff(vars_b, "(Intercept)")
  
  selected[b, vars_b] <- 1
}

freq_selection <- colMeans(selected)

variaveis<-sort(freq_selection, decreasing = TRUE)
tabela<-data.frame(variaveis)
colnames(tabela)<-c("freq_selection")

# Por exemplo,
# motor_impairment      0.82
# ID                    0.74
# constipation          0.41
# ASD                   0.28
# dysphagia             0.09
# 
# Você poderia dizer:
#   •	motor impairment: selecionada em 82% das amostras bootstrap → mais estável;
# •	ID: selecionada em 74% → relativamente estável;
# •	constipation: 41% → instável;
# •	dysphagia: 9% → sem evidência de estabilidade.
# Como idade e sexo estão com penalty.factor = 0, elas tendem a aparecer sempre. Por isso, eu avaliaria a estabilidade apenas das variáveis clínicas penalizadas.
# 

ajuste.vegetables<-lm(vegetable_intake~idade_atual_anos+sexo+dificuldade_motora+di+constipacao)
summary(ajuste.vegetables)
vif(ajuste.vegetables)


t(apply(analise2.new[,c(2:11)], MARGIN = 2, table))
