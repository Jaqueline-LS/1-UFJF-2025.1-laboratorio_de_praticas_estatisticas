source("dados.R")
library("readr")
library("glmnet")
library("car")

# Consumo adequado ou inadequado de verduras e legumes
attach(analise2.new)

t(summary(analise2.new[,c(2:13,15)]))

summary(analise2.new)
str(analise2.new)

fruits<-analise2.new$soma_porcoes_dia_frutas


modelo_lm<-lm(fruits~idade_atual_anos + sexo + idade_1a_crise_meses + tea + di + tdah + dificuldade_motora + 
                disfagia + rec_vomito_diarreia + constipacao + atraso_desenvolvimento_sn +
                epilepsia_farmacorressistente+ tipo_focal_generalizada + etiologia, data=analise2.new)
summary(modelo_lm)
vif(modelo_lm)


X <- model.matrix(
  fruits ~ idade_atual_anos + sexo + idade_1a_crise_meses + tea + di + tdah + dificuldade_motora + 
    disfagia + rec_vomito_diarreia + constipacao + atraso_desenvolvimento_sn +
    epilepsia_farmacorressistente+ tipo_focal_generalizada + etiologia
  ,
  data = analise2.new
)[,-1]
ncol(X)
colnames(X)

penalty <- rep(1, ncol(X))
penalty[colnames(X) == "idade_atual_anos"] <- 0
penalty[colnames(X) == "sexoM"] <- 0

set.seed(123)
y<-fruits

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


coef(cvfit, s = "lambda.1se")

set.seed(123)

B <- 2000

formula_lasso <-  fruits ~ idade_atual_anos + sexo + idade_1a_crise_meses +
  tea + di + tdah + dificuldade_motora + 
  disfagia + rec_vomito_diarreia + constipacao + atraso_desenvolvimento_sn +
  epilepsia_farmacorressistente+ tipo_focal_generalizada + etiologia

X <- model.matrix(formula_lasso, data = analise2.new)[, -1]
y <- fruits

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

tabela



B <- 1000

formula_lasso <-  fruits ~ idade_atual_anos + sexo + idade_1a_crise_meses +
  tea + di + tdah + dificuldade_motora + 
  disfagia + rec_vomito_diarreia + constipacao + atraso_desenvolvimento_sn +
  epilepsia_farmacorressistente+ tipo_focal_generalizada + etiologia

X <- model.matrix(formula_lasso, data = analise2.new)[, -1]
y <- fruits

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
  
  coef_b <- coef(cvfit, s = "lambda.min")
  
  vars_b <- rownames(coef_b)[as.numeric(coef_b) != 0]
  vars_b <- setdiff(vars_b, "(Intercept)")
  
  selected[b, vars_b] <- 1
}

freq_selection <- colMeans(selected)

variaveis<-sort(freq_selection, decreasing = TRUE)
tabela<-data.frame(variaveis)
colnames(tabela)<-c("freq_selection")

tabela


ajuste.min<-lm(fruits~idade_atual_anos + sexo + 
  tdah + atraso_desenvolvimento_sn)
summary(ajuste.min)




