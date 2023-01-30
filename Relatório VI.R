# Relatório Econometria VI - Heterocedasticidade

library(pacman)

pacman::p_load(dplyr,stringr,fastDummies, ggplot2, sandwich, lmtest, car, xlsx)

# Código de ETL (Extração, Tratamento e Carregamento dos Dados)

dados_enade <- read.csv('Dados/microdados_enade_2018econ.csv', sep = ';')

dados_enade_analisados <- dados_enade[c('Nt.Ger','Qe.I23','Qe.I21','Qe.I17','Qe.I10')]

tail(dados_enade_analisados)

nrow(dados_enade_analisados)

dados_enade_analisados_e_filtrados <- dados_enade_analisados %>% filter(Nt.Ger != '' &
                                                                        Qe.I23 != '' &
                                                                        Qe.I21 != '' &
                                                                        Qe.I17 != '' &
                                                                        Qe.I10 != '')

head(dados_enade_analisados_e_filtrados)

dados_enade_analisados_e_filtrados_e_tratados_nivel_1 <- rename(dados_enade_analisados_e_filtrados, Nota.geral.enade = Nt.Ger,
                                             Horas.estudadas.por.semana = Qe.I23, Pais.com.ensino.superior = Qe.I21,
                                              Ensino.medio.cursado = Qe.I17, Tempo.disposto.ao.trabalho = Qe.I10)

head(dados_enade_analisados_e_filtrados_e_tratados_nivel_1$Nota.geral.enade)

dados_enade_analisados_e_filtrados_e_tratados_nivel_2 <- dados_enade_analisados_e_filtrados_e_tratados_nivel_1 %>%
  mutate(across('Nota.geral.enade', str_replace, ',', '.'))

dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Horas.estudadas.por.semana <- str_replace_all(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Horas.estudadas.por.semana,
                                                                     c("A" = "Nenhuma","B" = "Ate.duas","C" = "Ate.cinco","D" = "Ate.oito","E" = "Oito.mais"))

dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Ensino.medio.cursado <- str_replace_all(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Ensino.medio.cursado,
                                                                                                    c("A" = "Toda.publica","B" = "Toda.privada","C" = "Toda.exterior",
                                                                                                      "D" = "Maior.parte.publica","E" = "Maior.parte.privada","F" = "Maior.parte.exterior"))

dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Pais.com.ensino.superior <- str_replace_all(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Pais.com.ensino.superior,
                                                                                                    c("A" = "Sim","B" = "Nao"))

dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Tempo.disposto.ao.trabalho <- str_replace_all(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Tempo.disposto.ao.trabalho,
                                                                                                    c("A" = "Nao.trabalha","B" = "Trabalha.20.horas","C" = "Trabalha.39.horas",
                                                                                                      "D" = "Trabalha.40.horas","E" = "Trabalha.mais.40.horas"))

dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Nota.geral.enade <- as.numeric(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Nota.geral.enade)

nrow(dados_enade_analisados_e_filtrados_e_tratados_nivel_2)

# Código de análise descritiva

hist(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Nota.geral.enade, main = "Histograma notas gerais economia enade",
     xlab = "Nota geral obtida", ylab = "Quantidade")

summary.geral <- data.frame(
  Total = round(length(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Nota.geral.enade),2),
  Media = round(mean(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Nota.geral.enade),2),
  FirstQ = round(quantile(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Nota.geral.enade, 0.25),2),
  Mediana = round(median(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Nota.geral.enade),2),
  ThirdQ = round(quantile(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Nota.geral.enade, 0.75),2),
  DesvioPadrao = round(sd(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Nota.geral.enade),2)
)

write.csv(summary.geral, "summarygeral.csv")

print(summary.geral)

boxplot(formula = Nota.geral.enade ~ Horas.estudadas.por.semana,
        data = dados_enade_analisados_e_filtrados_e_tratados_nivel_2,
        main = 'Notas por tempo de estudo semanal',
        xlab = 'Horas estudadas', ylab = 'Nota geral')

summary.notas.por.tempo.de.estudo.semanal <- data.frame(
  u = round(aggregate(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Nota.geral.enade,
                          list(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Horas.estudadas.por.semana),
                          mean)[2],2),
  v = round(aggregate(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Nota.geral.enade,
                          list(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Horas.estudadas.por.semana),
                          FUN = quantile ,prob = c(0.25))[2],2),
  w = round(aggregate(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Nota.geral.enade,
                      list(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Horas.estudadas.por.semana),
                      median)[2],2),
  x = round(aggregate(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Nota.geral.enade,
                            list(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Horas.estudadas.por.semana),
                            FUN = quantile ,prob = c(0.75))[2],2),
  y = round(aggregate(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Nota.geral.enade,
                            list(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Horas.estudadas.por.semana),
                            sd)[2],2)
)

write.csv(summary.notas.por.tempo.de.estudo.semanal, "summarynotasportempodeestudosemanal.csv")

print(summary.notas.por.tempo.de.estudo.semanal)

boxplot(formula = Nota.geral.enade ~ Pais.com.ensino.superior,
        data = dados_enade_analisados_e_filtrados_e_tratados_nivel_2,
        main = 'Notas por pais com ensino superior',
        xlab = 'Pais com ensino superior', ylab = 'Nota geral')

summary.notas.por.pais.com.ensino.superior <- data.frame(
  u = round(aggregate(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Nota.geral.enade,
                      list(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Pais.com.ensino.superior),
                      mean)[2],2),
  v = round(aggregate(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Nota.geral.enade,
                      list(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Pais.com.ensino.superior),
                      FUN = quantile ,prob = c(0.25))[2],2),
  w = round(aggregate(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Nota.geral.enade,
                      list(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Pais.com.ensino.superior),
                      median)[2],2),
  x = round(aggregate(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Nota.geral.enade,
                      list(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Pais.com.ensino.superior),
                      FUN = quantile ,prob = c(0.75))[2],2),
  y = round(aggregate(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Nota.geral.enade,
                      list(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Pais.com.ensino.superior),
                      sd)[2],2)
)

write.csv(summary.notas.por.pais.com.ensino.superior, "summarynotasporpaiscomensinosuperior.csv")

print(summary.notas.por.pais.com.ensino.superior)

boxplot(formula = Nota.geral.enade ~ Ensino.medio.cursado,
        data = dados_enade_analisados_e_filtrados_e_tratados_nivel_2,
        main = 'Notas por ensino medio cursado',
        xlab = 'Ensino medio cursado', ylab = 'Nota geral')

summary.notas.por.ensino.medio.cursado <- data.frame(
  u = round(aggregate(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Nota.geral.enade,
                      list(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Ensino.medio.cursado),
                      mean)[2],2),
  v = round(aggregate(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Nota.geral.enade,
                      list(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Ensino.medio.cursado),
                      FUN = quantile ,prob = c(0.25))[2],2),
  w = round(aggregate(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Nota.geral.enade,
                      list(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Ensino.medio.cursado),
                      median)[2],2),
  x = round(aggregate(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Nota.geral.enade,
                      list(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Ensino.medio.cursado),
                      FUN = quantile ,prob = c(0.75))[2],2),
  y = round(aggregate(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Nota.geral.enade,
                      list(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Ensino.medio.cursado),
                      sd)[2],2)
)

write.csv(summary.notas.por.ensino.medio.cursado, "summarynotasporensinomediocursado.csv")

print(summary.notas.por.ensino.medio.cursado)

boxplot(formula = Nota.geral.enade ~ Tempo.disposto.ao.trabalho,
        data = dados_enade_analisados_e_filtrados_e_tratados_nivel_2,
        main = 'Notas por tempo disposto ao trabalho',
        xlab = 'Tempo disposto ao trabalho', ylab = 'Nota geral')

summary.notas.por.tempo.disposto.ao.trabalho <- data.frame(
  u = round(aggregate(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Nota.geral.enade,
                      list(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Tempo.disposto.ao.trabalho),
                      mean)[2],2),
  v = round(aggregate(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Nota.geral.enade,
                      list(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Tempo.disposto.ao.trabalho),
                      FUN = quantile ,prob = c(0.25))[2],2),
  w = round(aggregate(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Nota.geral.enade,
                      list(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Tempo.disposto.ao.trabalho),
                      median)[2],2),
  x = round(aggregate(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Nota.geral.enade,
                      list(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Tempo.disposto.ao.trabalho),
                      FUN = quantile ,prob = c(0.75))[2],2),
  y = round(aggregate(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Nota.geral.enade,
                      list(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Tempo.disposto.ao.trabalho),
                      sd)[2],2)
)

write.csv(summary.notas.por.tempo.disposto.ao.trabalho, "summarynotasportempodispostoaotrabalho.csv")

print(summary.notas.por.tempo.disposto.ao.trabalho)

# Testes de normalidade de distribuição

set.seed(121)

qqnorm(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Nota.geral.enade, main='Teste QQ de Normalidade')
qqline(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Nota.geral.enade)

resultado.shapiro <- shapiro.test(sample(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Nota.geral.enade, 5000))
if(resultado.shapiro$p.value < 0.05){
  sprintf('Sendo H0 a hipotese de que a distribuicao segue uma normal com p_value em %s devemos rejeitar H0', round(resultado.shapiro$p.value,2))
} else {sprintf('Sendo H0 a hipotese de que a distribuicao segue uma normal com p_value em %s não devemos rejeitar H0', round(resultado.shapiro$p.value,2))}


resultado.ks <- ks.test(dados_enade_analisados_e_filtrados_e_tratados_nivel_2$Nota.geral.enade, 'pnorm')
if(resultado.ks$p.value < 0.05){
  sprintf('Sendo H0 a hipotese de que a distribuicao segue uma normal com p_value em %s devemos rejeitar H0', round(resultado.ks$p.value,2))
} else {sprintf('Sendo H0 a hipotese de que a distribuicao segue uma normal com p_value em %s não devemos rejeitar H0', round(resultado.ks$p.value,2))}

# Criando as variaveis dummy

dados_enade_analisados_e_filtrados_e_tratados_nivel_3 <- dummy_cols(.data = dados_enade_analisados_e_filtrados_e_tratados_nivel_2,
                                                                    select_columns = c("Horas.estudadas.por.semana", "Pais.com.ensino.superior",
                                                                                      "Ensino.medio.cursado","Tempo.disposto.ao.trabalho"))

head(dados_enade_analisados_e_filtrados_e_tratados_nivel_3)

# Primeira regressão linear multipla

primeira.reg <- lm(formula = Nota.geral.enade ~ Horas.estudadas.por.semana_Ate.duas +
  Horas.estudadas.por.semana_Ate.cinco + Horas.estudadas.por.semana_Ate.oito + Horas.estudadas.por.semana_Oito.mais +
  Pais.com.ensino.superior_Sim + Ensino.medio.cursado_Maior.parte.exterior + Ensino.medio.cursado_Maior.parte.privada +
  Ensino.medio.cursado_Maior.parte.publica + Ensino.medio.cursado_Toda.exterior + Ensino.medio.cursado_Toda.privada +
  Tempo.disposto.ao.trabalho_Trabalha.20.horas + Tempo.disposto.ao.trabalho_Trabalha.39.horas +
  Tempo.disposto.ao.trabalho_Trabalha.40.horas + Tempo.disposto.ao.trabalho_Trabalha.mais.40.horas, data = dados_enade_analisados_e_filtrados_e_tratados_nivel_3)

resultado.primeira.reg <- summary(primeira.reg)

tabela.resultados.primeira.reg <- data.frame(resultado.primeira.reg$coefficients)

write.xlsx(tabela.resultados.primeira.reg, "tabelaresultadosprimeirareg.xlsx")

dados_enade_analisados_e_filtrados_e_tratados_nivel_3["Nota.geral.enade.prevista"] <- primeira.reg$fitted.values

dados_enade_analisados_e_filtrados_e_tratados_nivel_3["Nota.geral.enade.residuo"] <- primeira.reg$residuals

# Averiguando eficiência do modelo

hist(resultado.primeira.reg$residuals, main = "Distribuicao dos residuos")

set.seed(121)

qqnorm(resultado.primeira.reg$residuals, main='Teste QQ de Normalidade dos residuos')
qqline(resultado.primeira.reg$residuals)

resultado.shapiro.residuos <- shapiro.test(sample(resultado.primeira.reg$residuals, 5000))
if(resultado.shapiro.residuos$p.value < 0.05){
  sprintf('Sendo H0 a hipotese de que a distribuicao segue uma normal com p_value em %s devemos rejeitar H0', round(resultado.shapiro.residuos$p.value,2))
} else {sprintf('Sendo H0 a hipotese de que a distribuicao segue uma normal com p_value em %s não devemos rejeitar H0', round(resultado.shapiro.residuos$p.value,2))}


resultado.ks.residuos <- ks.test(resultado.primeira.reg$residuals, 'pnorm')
if(resultado.ks.residuos$p.value < 0.05){
  sprintf('Sendo H0 a hipotese de que a distribuicao segue uma normal com p_value em %s devemos rejeitar H0', round(resultado.ks.residuos$p.value,2))
} else {sprintf('Sendo H0 a hipotese de que a distribuicao segue uma normal com p_value em %s não devemos rejeitar H0', round(resultado.ks.residuos$p.value,2))}

ggplot(data = dados_enade_analisados_e_filtrados_e_tratados_nivel_3, aes(x = Nota.geral.enade.prevista, y = Nota.geral.enade.residuo^2)) +
  geom_point(size = 1.5, stroke = 0) +
  xlab("Nota prevista") +
  ylab("Resiuos") +
  ggtitle('Resiuos vs Nota prevista') +
  theme(
    plot.title=element_text(size = 12, hjust = 0.5),
    axis.title.y=element_text(size = 10, vjust = +0.2),
    axis.title.x=element_text(size = 10, vjust = -0.2),
    axis.text.y=element_text(size = 10),
    axis.text.x=element_text(size = 10),
  )

# Teste de heterocedasticidade

bptest(primeira.reg)

# Motivos da heterocedasticidade

cor.matrix <- data.frame(cor(dados_enade_analisados_e_filtrados_e_tratados_nivel_3[c("Horas.estudadas.por.semana_Ate.duas",
  "Horas.estudadas.por.semana_Ate.cinco","Horas.estudadas.por.semana_Ate.oito","Horas.estudadas.por.semana_Oito.mais",
  "Pais.com.ensino.superior_Sim","Ensino.medio.cursado_Maior.parte.exterior","Ensino.medio.cursado_Maior.parte.privada",
  "Ensino.medio.cursado_Maior.parte.publica","Ensino.medio.cursado_Toda.exterior","Ensino.medio.cursado_Toda.privada",
  "Tempo.disposto.ao.trabalho_Trabalha.20.horas","Tempo.disposto.ao.trabalho_Trabalha.39.horas",
  "Tempo.disposto.ao.trabalho_Trabalha.40.horas","Tempo.disposto.ao.trabalho_Trabalha.mais.40.horas")]))

cor.matrix

write.xlsx(cor.matrix, "matrizcor.xlsx")

valor.vif <- vif(primeira.reg)

barplot(valor.vif, main = "Valor VIF (Variance Inflation Factor)", horiz = T, xlab = "Valor VIF", ylab = "Variavel")

# Resolução da heterocedasticidade

reg.heterocedasticidade <- coeftest(x = primeira.reg, vcov. = vcovHC, type = "HC1")

reg.heterocedasticidade