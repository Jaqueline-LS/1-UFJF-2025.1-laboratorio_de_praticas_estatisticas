source("dados.R")
# Isso foi o que foi feito para cada grupo alimentar
T2<-chisq.test(table(analise1$epilepsia, analise1$adeq_porcoes_dia_laticinios), correct = F)
fisher.test(table(analise1$epilepsia,analise1$adeq_porcoes_dia_cereais_saudaveis))
fisher.test(table(analise1$epilepsia, analise1$adeq_porcoes_dia_laticinios))

boxplot(analise1$idade_atual_anos~analise1$sexo)
boxplot(analise1$soma_cereais_saudaveis~analise1$epilepsia)

boxplot(analise1$soma_cereais_saudaveis~analise1$epilepsia)
boxplot(analise1$soma_laticinios~analise1$epilepsia)
boxplot(analise1$soma_frutas~analise1$epilepsia)
boxplot(analise1$soma_carnes_ovos~analise1$epilepsia)


analise2$tipo_focal_generalizada<-factor(as.character(analise2$tipo_focal_generalizada), levels=c("F","G"))
analise2$constipacao<-factor(as.character(analise2$constipacao), levels=c("S","N"))


boxplot(analise2$soma_porcoes_semana_doces_salgadinhos_guloseimas~analise2$tipo_focal_generalizada)
View(analise2)

boxplot(analise2$soma_porcoes_dia_verduras_legumes~analise2$constipacao)
View(analise2)


boxplot(analise2$soma_doces_salgadinhos_guloseimas~analise2$disfagia)
View(analise2)

