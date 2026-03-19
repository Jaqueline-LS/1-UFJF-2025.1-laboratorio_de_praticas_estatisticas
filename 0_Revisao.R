source("dados.R")
attach(analise2.new)
suppressMessages(library("dplyr"))
suppressMessages(library("MASS"))
suppressMessages(library("readr"))
suppressMessages(library("stringr"))
library("kableExtra")

# Funções de MLG
source("envelope.R")

cores<-c("#FF9B95","#C9E69E","#BAF3DE","#FFC29A")
# ----------------------------- ANALISE 2----------------------------------

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
#Analise.exp(Y,nome.plot=names(somas.resp)[[1]])
# Modelo com as variáveis que tiveram teste de medias significativos
fit.inicial<-lm(Y ~  dificuldade_motora,
                  data = analise2.new)
summary(fit.inicial)

modelo.completo<-Y~tea+di+tdah+dificuldade_motora+disfagia+epilepsia_farmacorressistente+tipo_focal_generalizada+rec_vomito_diarreia+constipacao+etiologia+paralisia_cerebral+atraso_desenvolvimento_sn+idade_atual_anos+sexo
fit.inicial<-lm(modelo.completo, data=analise2.new)
summary(fit.inicial)
resumo<-summary(fit.inicial)

selecao.pvalor<-function(fit.inicial, alpha=0.05, steps=1000)
{
  while(steps>0)
  {
      steps<-steps-1
      resumo<-summary(fit.inicial)
      scope<-drop.scope(fit.inicial)
      # Avaliar p-valor para cada variável/categoria que ainda esta no modelo
      pvalues <- resumo$coefficients[,4]
      scope.cat <- names(pvalues) # Conjunto de variáveis no modelo atual
      if (length(scope) == 0) break  # Não há mais variáveis para remover
  
      # Encontrar a variável com o maior p-valor
      max_pvalue <- max(pvalues)
      term_to_remove <- scope.cat[which.max(pvalues)]

      base <- substr(term_to_remove, 1, nchar(term_to_remove) - 1)
      term_to_remove<-scope[startsWith(scope, base)]
      if (max_pvalue > alpha) {
        # Remover a variável/categoria com o maior p-valor
        modelo <- update(fit.inicial, paste("~ . -", term_to_remove))
        cat("\nVariável removida:", term_to_remove, "( p-valor =", max_pvalue,")\n")
        cat("Novo modelo:", deparse(formula(modelo)), "\n")
        fit.inicial<-modelo

      } else {
        break  # Nenhuma variável com p-valor maior que alpha
      }
      print(summary(fit.inicial))
  }
  return(fit.inicial)
 
}

fit.2<-selecao.pvalor(fit.inicial, alpha=0.05, steps = 1000)


analise.res(fit2,1)



#--------------------------Soma Cereais, pães e turbéculos---------

Y<-get(somas.resp[[2]])

modelo.completo<-Y~tea+di+tdah+dificuldade_motora+disfagia+epilepsia_farmacorressistente+tipo_focal_generalizada+rec_vomito_diarreia+constipacao+etiologia+paralisia_cerebral+atraso_desenvolvimento_sn+idade_atual_anos+sexo
fit.inicial<-lm(modelo.completo, data=analise2.new)
summary(fit.inicial)
fit.2<-selecao.pvalor(fit.inicial, alpha=0.05, steps = 1000)

#--------------------------Soma Cereais, pães e turbéculos saudáveis---------

Y<-get(somas.resp[[3]])

modelo.completo<-Y~tea+di+tdah+dificuldade_motora+disfagia+epilepsia_farmacorressistente+tipo_focal_generalizada+rec_vomito_diarreia+constipacao+etiologia+paralisia_cerebral+atraso_desenvolvimento_sn+idade_atual_anos+sexo
fit.inicial<-lm(modelo.completo, data=analise2.new)
summary(fit.inicial)

fit.2<-selecao.pvalor(fit.inicial, alpha=0.05, steps = 1000)


#--------------------------Soma verduras e legumes---------

Y<-get(somas.resp[[4]])
Analise.exp(Y,nome.plot=names(somas.resp)[[4]])

modelo.completo<-Y~tea+di+tdah+dificuldade_motora+disfagia+epilepsia_farmacorressistente+tipo_focal_generalizada+rec_vomito_diarreia+constipacao+etiologia+paralisia_cerebral+atraso_desenvolvimento_sn+idade_atual_anos+sexo
fit.inicial<-lm(modelo.completo, data=analise2.new)
summary(fit.inicial)

fit.2<-selecao.pvalor(fit.inicial, alpha=0.05, steps = 1000)
# Com a paralisia cerebral o resíduo da observação 23 é infito. Só uma observação tem essa caract
modelo.selecionado<-Y~di+dificuldade_motora+constipacao+sexo
fit2<-lm(modelo.selecionado, data=analise2.new)
summary(fit2)


#------------------- Frutas------------------
Y<-get(somas.resp[[5]])

modelo.completo<-Y~tea+di+tdah+dificuldade_motora+disfagia+epilepsia_farmacorressistente+tipo_focal_generalizada+rec_vomito_diarreia+constipacao+etiologia+paralisia_cerebral+atraso_desenvolvimento_sn+idade_atual_anos+sexo
fit.inicial<-lm(modelo.completo, data=analise2.new)
summary(fit.inicial)

fit.2<-selecao.pvalor(fit.inicial, alpha=0.05, steps = 1000)
