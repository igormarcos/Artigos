## INSTALACAO DE GGPLOT2 ##

install.packages("ggplot2")
library(ggplot2)

## IMPORTAR TABELA ##

library(readxl)
comparativo <- read_excel("~/comparativo.xlsx")
View(comparativo)

## Correlacao Linear IDHM ~ RENDA ##
ggplot(data=comparativo, aes(x=IDHM, y=RENDA))  +geom_point(aes(colour = factor(UF))) +geom_smooth(method="lm",se=FALSE)
m1 <- lm(data=comparativo, IDHM ~ RENDA)
summary(m1)
qqnorm(m1$residuals)
qqline(m1$residuals)
t.test(comparativo$IDHM, comparativo$RENDA, alternative = "two.sided")

## Correlacao Linear IDHM ~ ESPVIDA ##
ggplot(data=comparativo, aes(x=IDHM, y=ESPVIDA))  +geom_point(aes(colour = factor(UF))) +geom_smooth(method="lm",se=FALSE)
m1 <- lm(data=comparativo, IDHM ~ ESPVIDA)
summary(m1)
qqnorm(m1$residuals)
qqline(m1$residuals)
t.test(comparativo$IDHM, comparativo$ESPVIDA, alternative = "two.sided")

## Correlacao Linear IDHM ~ TRANSPORTE ##
ggplot(data=comparativo, aes(x=IDHM, y=TRANSPORTE))  +geom_point(aes(colour = factor(UF))) +geom_smooth(method="lm",se=FALSE)
m1 <- lm(data=comparativo, IDHM ~ TRANSPORTE)
summary(m1)
qqnorm(m1$residuals)
qqline(m1$residuals)
t.test(comparativo$IDHM, comparativo$TRANSPORTE, alternative = "two.sided")

## Correlacao Linear IDHM ~ VIOLENCIA ##
ggplot(data=comparativo, aes(x=IDHM, y=VIOLENCIA))  +geom_point(aes(colour = factor(UF))) +geom_smooth(method="lm",se=FALSE)
m1 <- lm(data=comparativo, IDHM ~ VIOLENCIA)
summary(m1)
qqnorm(m1$residuals)
qqline(m1$residuals)
t.test(comparativo$IDHM, comparativo$VIOLENCIA, alternative = "two.sided")

## Correlacao Linear IDHM ~ CELULAR ##
ggplot(data=comparativo, aes(x=IDHM, y=CELULAR))  +geom_point(aes(colour = factor(UF))) +geom_smooth(method="lm",se=FALSE)
m1 <- lm(data=comparativo, IDHM ~ CELULAR)
summary(m1)
qqnorm(m1$residuals)
qqline(m1$residuals)
t.test(comparativo$IDHM, comparativo$CELULAR, alternative = "two.sided")

## Correlacao Linear IDHM ~ HABITANTES ##
ggplot(data=comparativo, aes(x=IDHM, y=HABITANTES))  +geom_point(aes(colour = factor(UF))) +geom_smooth(method="lm",se=FALSE)
m1 <- lm(data=comparativo, IDHM ~ HABITANTES)
summary(m1)
qqnorm(m1$residuals)
qqline(m1$residuals)
t.test(comparativo$IDHM, comparativo$HABITANTES, alternative = "two.sided")

## Dados tabela II - Correlacao de pearson ##
cor(comparativo$IDHM, comparativo$RENDA)
cor(comparativo$IDHM, comparativo$VIOLENCIA)
cor(comparativo$IDHM, comparativo$ESPVIDA)
cor(comparativo$IDHM, comparativo$TRANSPORTE)
cor(comparativo$IDHM, comparativo$CELULAR)
cor(comparativo$IDHM, comparativo$HABITANTES)

## Plotar HeatMap ##
comparativo2 <- comparativo[,-1] #Remove a primeira coluna
heatmap(abs(cor(comparativo2)))

## Correlacao Pearson base inteira ##
cor(comparativo2)

## Dados tabela III - Correlacao linear multivariada ##
m1 <- lm(IDHM ~ RENDA + CELULAR + ESPVIDA, data=comparativo)
summary(m1)

##Links ??teis
#http://rstudio-pubs-static.s3.amazonaws.com/10539_9a0d69971efd414d96bfb4b8cc20e76f.html
#https://www.r-bloggers.com/wilcoxon-mann-whitney-rank-sum-test-or-test-u
#http://rstudio-pubs-static.s3.amazonaws.com/15297_0f32b77b1a0047f09929397b47fb3997.html
#https://docs.google.com/document/d/147p4ieWBFcmeMiIsytMv6lUW6-bjdu7x3vsb0ovtCI8/edit?hl=pt_BR
#http://posgraduando.com/como-fazer-analise-de-variancia-one-way-anova-one-way-no-r
#https://cran.r-project.org/web/packages/inference/inference.pdf
#http://ggplot2.tidyverse.org
