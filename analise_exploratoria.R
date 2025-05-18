source("dados.R")
cores<-c("#C9E69E","#FF9B95","#FFC29A","#BAF3DE")
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
analise2$disfagia<-factor(as.character(analise2$disfagia), levels=c("S","N"))

analise2$<-factor(as.character(analise2$disfagia), levels=c("S","N"))



boxplot(analise2$soma_porcoes_semana_doces_salgadinhos_guloseimas~analise2$tipo_focal_generalizada, col=cores)

boxplot(analise2$soma_porcoes_dia_verduras_legumes~analise2$constipacao, col=cores)

boxplot(analise2$soma_doces_salgadinhos_guloseimas~analise2$disfagia, col=cores)



analise1.new<-analise1[,-c(6,7,10,11,14,15,18,19,22,23,26,27,30,31,34,35,38,39)]
fit<-glm(epilepsia~., data=analise1.new, family = binomial(link=logit))
summary(fit)



analise2.new<-analise2[,-c(c(19,20,23,24,27,28,31,32,35,36,39,40,43,44,47,48))]
fit<-glm(constipacao~., data=analise2.new, family = binomial(link=logit))
summary(fit)


str(analise2$disfagia)


