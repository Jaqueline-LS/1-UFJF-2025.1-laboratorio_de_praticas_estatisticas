library("dplyr")
dados_completos <- read.delim2("C:/ufjf/2025.1/laboratorio_de_praticas_estatisticas/Dados/Estatistica.xlsx - Dados Completos.tsv", stringsAsFactors=TRUE)
View(dados_completos)
colnames(dados_completos)<-c(
  "numero_pesquisa",
  "idade_1a_crise_meses",
  "tea",
  "di",
  "tdah",
  "dificuldade_motora",
  "paralisia_cerebral",
  "disfagia",
  "rec_vomito_diarreia",
  "constipacao",
  "atraso_desenvolvimento_sn",
  "epilepsia_farmacorressistente",
  "tipo_focal_generalizada",
  "etiologia",
  "idade_atual_anos",
  "sexo",
  "epilepsia",
  "classe_idade",
  "soma_laticinios",
  "media_laticinios",
  "soma_porcoes_dia_laticinios",
  "adeq_porcoes_dia_laticinios",
  "soma_cereais_total",
  "media_cereais_total",
  "soma_porcoes_dia_cereais_total",
  "adeq_porcoes_dia_cereais_total",
  "soma_cereais_saudaveis",
  "media_cereais_saudaveis",
  "soma_porcoes_dia_cereais_saudaveis",
  "adeq_porcoes_dia_cereais_saudaveis",
  "soma_verduras_legumes",
  "media_verduras_legumes",
  "soma_porcoes_dia_verduras_legumes",
  "adeq_porcoes_dia_verduras_legumes",
  "soma_frutas",
  "media_frutas",
  "soma_porcoes_dia_frutas",
  "adeq_porcoes_dia_frutas",
  "soma_carnes_ovos",
  "media_carnes_ovos",
  "soma_porcoes_dia_carnes_ovos",
  "adeq_porcoes_dia_carnes_ovos",
  "soma_embutidos",
  "media_embutidos",
  "soma_porcoes_semana_embutidos",
  "adeq_porcoes_semana_embutidos",
  "soma_salgados_preparacoes",
  "media_salgados_preparacoes",
  "soma_porcoes_semana_salgados_preparacoes",
  "adeq_porcoes_semana_salgados_preparacoes",
  "soma_doces_salgadinhos_guloseimas",
  "media_doces_salgadinhos_guloseimas",
  "soma_porcoes_semana_doces_salgadinhos_guloseimas",
  "adeq_porcoes_semana_doces_salgadinhos_guloseimas",
  "adeq_porcoes_dia_doces_salgadinhos_guloseimas"
)

analise1<-dados_completos |>
  select(-c(2:14,)) 

str(analise1)

# Isso foi o que foi feito para cada grupo alimentar
chisq.test(table(analise1$epilepsia, analise1$adeq_porcoes_dia_laticinios), correct = F)
fisher.test()

# A observação com numero de pesquisa 59 esta faltando a idade
# Na variável adequação por dia laticinios em um dado errado numero de pesquisa 42
     ## Olhando na classificação ele é I de inadequado, mudei na planilha.

#attach(analise1)

# Fazer uma análise exploratória dos dados.



#-------------------------------------------Objetivo 2-------------------------------

analise2<-dados_completos |>
  filter(epilepsia=="E")
