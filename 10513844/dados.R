library("dplyr")
dados_completos <- read.delim2("Dados/Estatistica.xlsx - Dados Completos.tsv", stringsAsFactors=TRUE)
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
  dplyr::select(-c(2:14,)) 

analise1.new<-analise1[,-c(1,5,7,11,15,19,23,27,31,35,39)]





# O indivíduo com numero de pesquisa 59 esta faltando a idade
# O indivíduo com número de pesquisa 34 apresenta na variável atraso no desenvolvimento o valor "S*" isso indica algo ou é erro de digitação
# considerei erro de digitação, era só um
dados_completos$atraso_desenvolvimento_sn[which(dados_completos$atraso_desenvolvimento_sn=="S*")]="S"


# Na variável adequação por dia laticinios em um dado errado numero de pesquisa 42
     ## Olhando na classificação ele é I de inadequado, mudei na planilha.
# Mudei no arquivo:
  #  apaguei um espaço que estava junto com a classificação do n° de pesquisa 53 na variável tipo Focal ou Generalizada

#attach(analise1)



# Fazer uma análise exploratória dos dados.



#-------------------------------------------Objetivo 2-------------------------------

analise2<-dados_completos |>
  filter(epilepsia=="E")


# Retirar as colunas que não serão utilizadas
analise2.new<-analise2[,-c(1,19,20,23,24,27,28,31,32,35,36,39,40,43,44,47,48)]

# Transformando em fatores as variáveis categóricas com o número correto de levels
analise2.new$tea<-factor(as.character(analise2.new$tea), levels=c("N","S"))
analise2.new$di<-factor(as.character(analise2.new$di), levels=c("N","S"))
analise2.new$tdah<-factor(as.character(analise2.new$tdah), levels=c("N","S"))
analise2.new$dificuldade_motora<-factor(as.character(analise2.new$dificuldade_motora), levels=c("N","S"))
analise2.new$paralisia_cerebral<-factor(as.character(analise2.new$paralisia_cerebral), levels=c("N","S"))
analise2.new$disfagia<-factor(as.character(analise2.new$disfagia), levels=c("N","S"))
analise2.new$rec_vomito_diarreia<-factor(as.character(analise2.new$rec_vomito_diarreia), levels=c("N","S"))
analise2.new$constipacao<-factor(as.character(analise2.new$constipacao), levels=c("N","S"))
analise2.new$atraso_desenvolvimento_sn<-factor(as.character(analise2.new$atraso_desenvolvimento_sn), levels=c("N","S"))
analise2.new$epilepsia_farmacorressistente<-factor(as.character(analise2.new$epilepsia_farmacorressistente), levels=c("N","S"))
analise2.new$tipo_focal_generalizada<-factor(as.character(analise2.new$tipo_focal_generalizada), levels=c("F","G"))
analise2.new$etiologia<-factor(as.character(analise2.new$etiologia), levels=c("E","G","I"))

