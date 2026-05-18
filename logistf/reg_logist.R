source("dados.R")
library("logistf")
library("kableExtra")
suppressMessages(library("dplyr"))

attach(analise2.new)

y<-adeq_porcoes_dia_verduras_legumes=="I" # inadequação
table(y)
t(summary(analise2.new[,c(2:13,15)]))

covariaveis.clinicas<-c("idade_1a_crise_meses", "tea", "di", "tdah", 
  "dificuldade_motora", "disfagia", "rec_vomito_diarreia", "constipacao", 
  "atraso_desenvolvimento_sn", "epilepsia_farmacorressistente", 
  "tipo_focal_generalizada", "etiologia")

covariaveis.clinicas.frequentes<-c("idade_1a_crise_meses", "tea", "di",
  "dificuldade_motora", "constipacao", 
  "atraso_desenvolvimento_sn", "epilepsia_farmacorressistente", 
  "tipo_focal_generalizada", "etiologia")

get(covariaveis.clinicas.frequentes[1])
p.valores<-numeric(length(covariaveis.clinicas.frequentes))
for(i in seq_along(covariaveis.clinicas.frequentes))
{
  modelo<-logistf(y ~ idade_atual_anos + sexo + get(covariaveis.clinicas.frequentes[i]))
  resumo<-summary(modelo)
  p.valores[i]<-resumo$prob[4]
}
table(y)
tabela<-data_frame(covariaveis.clinicas.frequentes,p.valores)
tabela |>
  arrange(p.valores)

modelo.final<-logistf(y ~idade_atual_anos + sexo + constipacao+
                        tea + dificuldade_motora + di)
resumo<-summary(modelo.final)

erros.padrao<-sqrt(diag(resumo$var))
coeficientes<-resumo$coefficients

c1<-exp(coeficientes)
c2<-exp(coeficientes-(1.96*erros.padrao))
c3<-exp(coeficientes+(1.96*erros.padrao))

tabela2<-data.frame(c1,c2,c3)
colnames(tabela2)<-c("OR","LI", "LS")
tabela2
# knitr::kable(tabela2, caption = "Razão de chances estimadas", format = "latex", escape = FALSE, booktabs=T) %>%
#   kable_styling(latex_options = c("hold_position", "scale_down"))
# 




y<-adeq_porcoes_semana_doces_salgadinhos_guloseimas=="I" # inadequação
table(y)
t(summary(analise2.new[,c(2:13,15)]))

covariaveis.clinicas<-c("idade_1a_crise_meses", "tea", "di", "tdah", 
  "dificuldade_motora", "disfagia", "rec_vomito_diarreia", "constipacao", 
  "atraso_desenvolvimento_sn", "epilepsia_farmacorressistente", 
  "tipo_focal_generalizada", "etiologia")

covariaveis.clinicas.frequentes<-c("idade_1a_crise_meses", "tea", "di",
  "dificuldade_motora", "constipacao", 
  "atraso_desenvolvimento_sn", "epilepsia_farmacorressistente", 
  "tipo_focal_generalizada", "etiologia")

get(covariaveis.clinicas.frequentes[1])
p.valores<-numeric(length(covariaveis.clinicas.frequentes))
for(i in seq_along(covariaveis.clinicas.frequentes))
{
  modelo<-logistf(y ~ idade_atual_anos + sexo + get(covariaveis.clinicas.frequentes[i]))
  resumo<-summary(modelo)
  p.valores[i]<-resumo$prob[4]
}
table(y)
tabela<-data_frame(covariaveis.clinicas.frequentes,p.valores)
tabela |>
  arrange(p.valores)

modelo.final<-logistf(y ~idade_atual_anos + sexo + tipo_focal_generalizada+
                        dificuldade_motora+ constipacao+ di)
resumo<-summary(modelo.final)

erros.padrao<-sqrt(diag(resumo$var))
coeficientes<-resumo$coefficients

c1<-exp(coeficientes)
c2<-exp(coeficientes-(1.96*erros.padrao))
c3<-exp(coeficientes+(1.96*erros.padrao))

tabela2<-data.frame(c1,c2,c3)
colnames(tabela2)<-c("OR","LI", "LS")
tabela2
