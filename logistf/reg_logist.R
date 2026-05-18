source("dados.R")
library("logistf")
suppressMessages(library("dplyr"))

attach(analise2.new)

y<-adeq_porcoes_dia_verduras_legumes=="I" # inadequação
table(y)
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
