install.packages('BatchGetSymbols')
install.packages('GetTDData')
install.packages('GetBCBData')
install.packages('dplyr')
install.packages('lmtest')
install.packages('tidyverse')
install.packages('bstats')
install.packages('aTSA')
install.packages('ggplot2')
install.packages('pacman')

pacman::p_load(dplyr,BatchGetSymbols,tidyverse,GetTDData,GetBCBData,lmtest,bstats,aTSA,ggplot2)

taxa.selic <- gbcbd_get_series(id = 432, first.date = '2010-01-01', last.date = '2022-12-31')

taxa.selic <- taxa.selic %>% 
  mutate(ref.date = as.Date(ref.date, "%d/%m/%Y")) %>% arrange(ref.date)

taxa.selic <- taxa.selic %>% drop_na()

options(repr.plot.width = 12, repr.plot.height = 6)

taxa.selic %>% ggplot() +
  geom_line(mapping = aes(x = ref.date, y = value), colour = 'darkgray') +
  geom_hline(yintercept = mean(taxa.selic$value),  linetype = 'dashed') +
  theme_classic() +
  theme(plot.title = element_text(size = 14, hjust = 0.5)) +
  ggtitle('Taxa Selic 2010 a 2022') +
  xlab('Ano') +
  ylab('Taxa')

descritiva.taxa.selic <- summary(taxa.selic$value)

descritiva.taxa.selic

sd(taxa.selic$value)

adf.test(taxa.selic$value)

taxa.selic$value <- taxa.selic$value / 100

taxa.selic <- taxa.selic[c('ref.date','value')]

asset.codes <- 'LFT'
cache_folder <- paste0(tempdir(), '/TD_cache')
requisicao <- download.TD.data(asset.codes = asset.codes, dl.folder = cache_folder)

firt.date <- as.Date('2010-01-01')
last.date <- as.Date('2022-12-31')

preco.selic <- read.TD.files(dl.folder = cache_folder)

preco.selic <- preco.selic %>% filter(ref.date >= firt.date & ref.date <= last.date)

preco.selic <- preco.selic %>% 
  mutate(ref.date = as.Date(ref.date, "%d/%m/%Y")) %>% arrange(ref.date)

preco.selic <- preco.selic %>% group_by(ref.date) %>% summarize(price.bid = mean(price.bid, na.rm = T))

options(repr.plot.width = 12, repr.plot.height = 6)

preco.selic %>% ggplot() +
  geom_line(mapping = aes(x = ref.date, y = price.bid), colour = 'darkgray') +
  geom_hline(yintercept = mean(preco.selic$price.bid),  linetype = 'dashed') +
  theme_classic() +
  theme(plot.title = element_text(size = 14,hjust = 0.5))+
  ggtitle('Preço LFT (Selic)') +
  xlab('Ano') +
  ylab('Preço')

descritiva.preco.selic <- summary(preco.selic$price.bid)

descritiva.preco.selic

sd(preco.selic$price.bid)

adf.test(preco.selic$price.bid)

preco.selic <- preco.selic[c('ref.date','price.bid')]

acoes.petr4 <- BatchGetSymbols('PETR4.SA', first.date = firt.date, last.date = last.date)

acoes.petr4$df.tickers %>% ggplot() +
  geom_line(mapping = aes(x = ref.date, y = price.close), colour = 'darkgray') +
  geom_hline(yintercept = mean(acoes.petr4$df.tickers$price.close),  linetype = 'dashed') +
  theme_classic() +
  theme(plot.title = element_text(size = 14, hjust = 0.5))+
  ggtitle('Preço Ações PETR4') +
  xlab('Ano') +
  ylab('Preço')

descritiva.preco.petr4 <- summary(as.numeric(acoes.petr4$df.tickers$price.close))

descritiva.preco.petr4

sd(acoes.petr4$df.tickers$price.close)

adf.test(acoes.petr4$df.tickers$price.close)

acoes.petr4 <- acoes.petr4$df.tickers

acoes.petr4 <- acoes.petr4 %>% replace(is.na(.), 0)

options(repr.plot.width = 12, repr.plot.height = 6)

acoes.petr4 %>% ggplot() +
  geom_line(mapping = aes(x = ref.date, y = ret.adjusted.prices), colour = 'darkgray') +
  geom_hline(yintercept = mean(acoes.petr4$ret.adjusted.prices),  linetype = 'dashed') +
  theme_classic() +
  theme(plot.title = element_text(size = 14,hjust = 0.5))+
  ggtitle('Retorno Índice Ibovespa') +
  xlab('Ano') +
  ylab('Retorno')

adf.test(acoes.petr4$ret.adjusted.prices)

acoes.petr4 <- acoes.petr4[c('ref.date','price.close','ret.adjusted.prices')]

indice.ibov <- BatchGetSymbols('^BVSP', first.date = firt.date, last.date = last.date)

options(repr.plot.width = 12, repr.plot.height = 6)

indice.ibov$df.tickers %>% ggplot() +
  geom_line(mapping = aes(x = ref.date, y = price.adjusted), colour = 'darkgray') +
  geom_hline(yintercept = mean(indice.ibov$df.tickers$price.adjusted),  linetype = 'dashed') +
  theme_classic() +
  theme(plot.title = element_text(size = 14, hjust = 0.5))+
  ggtitle('Pontos Índice Ibovespa') +
  xlab('Ano') +
  ylab('Pontos')

descritiva.pontos.ibov <- summary(indice.ibov$df.tickers$price.adjusted)

descritiva.pontos.ibov

sd(indice.ibov$df.tickers$price.adjusted)

adf.test(indice.ibov$df.tickers$price.adjusted)

indice.ibov <- indice.ibov$df.tickers

indice.ibov <- indice.ibov %>% replace(is.na(.), 0)

options(repr.plot.width = 12, repr.plot.height = 6)

indice.ibov %>% ggplot() +
  geom_line(mapping = aes(x = ref.date, y = ret.adjusted.prices), colour = 'darkgray') +
  geom_hline(yintercept = mean(indice.ibov$ret.adjusted.prices),  linetype = 'dashed') +
  theme_classic() +
  theme(plot.title = element_text(size = 14,hjust = 0.5))+
  ggtitle('Retorno Índice Ibovespa') +
  xlab('Ano') +
  ylab('Retorno')

adf.test(indice.ibov$ret.adjusted.prices)

indice.ibov <- indice.ibov[c('ref.date','price.adjusted','ret.adjusted.prices')]

dados.totais <- left_join(x = acoes.petr4, y = indice.ibov, by = 'ref.date')

dados.totais <- left_join(x = dados.totais, y = preco.selic, by = 'ref.date')

dados.totais <- left_join(x = dados.totais, y = taxa.selic, by = 'ref.date')

nrow(dados.totais)

dados.totais <- drop_na(dados.totais)

nrow(dados.totais)

dados.totais <- dados.totais %>% rename(
  data = ref.date,
  preco.petr4 = price.close,
  retorno.petr4 = ret.adjusted.prices.x,
  pontos.ibov = price.adjusted,
  retorno.ibov = ret.adjusted.prices.y,
  preco.lft = price.bid,
  tx.selic = value
)

dados.totais['selicibov.nivel'] <- dados.totais$pontos.ibov - dados.totais$preco.lft
dados.totais['selicibov.diferenca'] <- dados.totais$retorno.ibov - dados.totais$tx.selic


coint.test(dados.totais$pontos.ibov, dados.totais$preco.petr4, nlag = 1)
coint.test(dados.totais$preco.lft, dados.totais$preco.petr4, nlag = 1)
coint.test(dados.totais$selicibov.nivel, dados.totais$preco.petr4, nlag = 1)

coint.test(dados.totais$retorno.ibov, dados.totais$retorno.petr4, nlag = 1)
coint.test(dados.totais$tx.selic, dados.totais$retorno.petr4, nlag = 1)
coint.test(dados.totais$retorno.ibov, dados.totais$retorno.petr4, nlag = 1)

capm.nivel <- lm(formula = preco.petr4 ~ preco.lft + selicibov.nivel, data = dados.totais)

summary(capm.nivel)

dados.totais['residuos.nivel'] <- capm.nivel$residuals

options(repr.plot.width = 12, repr.plot.height = 6)

dados.totais %>% ggplot() +
  geom_line(mapping = aes(x = data, y = residuos.nivel), colour = 'darkgray') +
  geom_hline(yintercept = mean(dados.totais$residuos.nivel),  linetype = 'dashed') +
  theme_classic() +
  theme(plot.title = element_text(size = 14,hjust = 0.5))+
  ggtitle('Resultado dos Residuos MQO') +
  xlab('Ano') +
  ylab('Residuo')

adf.test(dados.totais$residuos.nivel)

capm.diferenca <- lm(formula = retorno.petr4 ~ tx.selic + selicibov.diferenca, data = dados.totais)

summary(capm.diferenca)

options(repr.plot.width = 12, repr.plot.height = 6)

dados.totais %>% ggplot() +
  geom_point(mapping = aes(x = retorno.ibov, y = retorno.petr4), size = 0.5, colour = 'darkgray') +
  geom_abline(intercept = 0.0003445, slope = 1.3827652, linetype = "dashed", size = 1) +
  theme_classic() +
  theme(plot.title = element_text(size = 14,hjust = 0.5)) +
  ggtitle('Regressão CAPM') +
  xlab('Retorno Ibovespa') +
  ylab('Retorno PETR4')

dados.totais['residuos.diferenca'] <- capm.diferenca$residuals

options(repr.plot.width = 12, repr.plot.height = 6)

dados.totais %>% ggplot() +
  geom_line(mapping = aes(x = data, y = residuos.diferenca), colour = 'darkgray') +
  geom_hline(yintercept = mean(dados.totais$residuos.diferenca),  linetype = 'dashed') +
  theme_classic() +
  theme(plot.title = element_text(size = 14,hjust = 0.5))+
  ggtitle('') +
  xlab('Ano') +
  ylab('')

indice.brent <- BatchGetSymbols('BZ=F', first.date = firt.date, last.date = last.date)

options(repr.plot.width = 12, repr.plot.height = 6)

indice.brent$df.tickers %>% ggplot() +
  geom_line(mapping = aes(x = ref.date, y = price.adjusted), colour = 'darkgray') +
  geom_hline(yintercept = mean(indice.brent$df.tickers$price.adjusted),  linetype = 'dashed') +
  theme_classic() +
  theme(plot.title = element_text(size = 14, hjust = 0.5))+
  ggtitle('Preço Brent Oil') +
  xlab('Ano') +
  ylab('Preço')

descritiva.preco.bret <- summary(indice.brent$df.tickers$price.adjusted)

descritiva.preco.bret

sd(indice.brent$df.tickers$price.adjusted)

adf.test(indice.brent$df.tickers$price.adjusted)


indice.brent <- indice.brent$df.tickers

indice.brent <- indice.brent %>% replace(is.na(.), 0)

indice.brent <- indice.brent %>% rename(
  data = ref.date)

indice.brent <- indice.brent[c('data','price.adjusted','ret.adjusted.prices')]

dados.totais <- left_join(x = dados.totais, y = indice.brent, by = 'data')

nrow(dados.totais)

dados.totais <- drop_na(dados.totais)

nrow(dados.totais)

coint.test(dados.totais$price.adjusted, dados.totais$preco.petr4, nlag = 1)

capm.brent<- lm(formula = preco.petr4 ~ preco.lft + selicibov.nivel + price.adjusted, data = dados.totais)

summary(capm.brent)
