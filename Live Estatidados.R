install.packages("quantmod")
install.packages("PerformanceAnalytics")
install.packages("data.table")

library(quantmod)
library(PerformanceAnalytics)
library(data.table)
library(RColorBrewer)

dt = '2014-1-1'

aapl = getSymbols.yahoo("AAPL", from = dt, auto.assign = F)
# XTS é 'Extensible Time Series" object
# Nele armazenamos dados de séries temporais
# Trabalhamos com esse tipo de dados bastante no mercado financeiro

# Queremos apenas a coluna 6, logo:
aapl_adjusted = getSymbols.yahoo("AAPL", from = dt, auto.assign = F)[,6]

returns = dailyReturn(aapl_adjusted, type = 'arithmetic')

# Mencionar sobre o problema dos NAs, especialmente para papéis de pouca
# liquidez que as vezes não são negociados

returns = na.omit(dailyReturn(aapl_adjusted, type = 'arithmetic'))

weeklyReturn(aapl_adjusted)
monthlyReturn(aapl_adjusted)

chartSeries(aapl)

# Lista com 20 ativos
tickers = c('EQTL3.SA', 'PETR4.SA', 'VALE3.SA', 'WEGE3.SA', 'EMBR3.SA',
            'CSNA3.SA', 'USIM5.SA','TOTS3.SA',
            #'LWSA3.SA',
            'ABEV3.SA', 
            'LREN3.SA', 'CIEL3.SA', 'RADL3.SA', 'RENT3.SA', 'MDIA3.SA', 
            'EZTC3.SA', 'FLRY3.SA', 'OIBR3.SA', 'CVCB3.SA'
            #, 'BIDI11.SA'
            )

precos_carteira = NULL

for(ticker in tickers){
  precos_carteira = cbind(precos_carteira, 
                          getSymbols.yahoo(ticker,from = '2014-1-1', auto.assign = F)[,4])
}

# Temos um grande número de NAs, talvez seja melhor reduzir o tempo analisado

precos_carteira = data.frame(precos_carteira)

colnames(precos_carteira) = c('EQTL3', 'PETR4', 'VALE3', 'WEGE3', 'EMBR3',
            'CSNA3', 'USIM5','TOTS3','ABEV3','LREN3', 
            'CIEL3', 'RADL3', 'RENT3', 'MDIA3', 
            'EZTC3', 'FLRY3', 'OIBR3', 'CVCB3'
            )
precos_carteira$data = row.names(precos_carteira)

# Função para fazer o 'forward filling'
replaceNaWithLatest = function( dfIn, nameColsNa = names(dfIn) ){ 
  dtTest <- data.table(dfIn) 
  invisible(lapply(nameColsNa, 
                   function(nameColNa){ 
                     setnames(dtTest, nameColNa, "colNa") 
                     dtTest[, segment := cumsum(!is.na(colNa))] 
                     dtTest[, colNa := colNa[1], by = "segment"] 
                     dtTest[, segment := NULL] 
                     setnames(dtTest, "colNa", nameColNa) 
                   })) 
  return(dtTest)
}

novo_carteira = replaceNaWithLatest(data.frame(precos_carteira[,-19]))

novo_carteira = data.frame(novo_carteira, row.names = precos_carteira$data)

colSums(is.na(data.frame(precos_carteira)))

colSums(is.na(novo_carteira))

IBOV = getSymbols.yahoo('^BVSP',from = '2014-1-1', auto.assign = F)[,4]

novo_carteira$data = row.names(novo_carteira)


# Adicionar aqui algumas métricas de avaliação do portfólio



# Como normalizar uma carteira para exibi-la no gráfico
normalizado = data.frame(lapply(novo_carteira[,-19],function(x) x/x[1]))
normalizado$data = novo_carteira$data
normalizado$data = as.Date(normalizado$data, format = "%Y-%m-%d")


d = melt(novo_carteira, id.vars='data')
e = melt(normalizado, id.vars='data')

# Everything on the same plot
ggplot(d, aes(data,value, col=variable, group = variable)) + 
  #geom_point(aes(color = variable), size = 0.2)
  geom_line()

library(scales)

p = ggplot(e, aes(x = as.POSIXct(data),y = value, col=variable, group = variable)) + 
  #geom_point(aes(color = variable), size = 0.2)
  geom_line(size = 1) +
  scale_x_datetime(date_labels = "%b-%Y")+
  ggtitle("Cotação de 18 ações escolhidas")+ # for the main title
  xlab("Data")+ # for the x axis label
  ylab("Preço") # for the y axis label

set3 = colorRampPalette(brewer.pal('Set3',n=18))
  
p + scale_color_manual(values = setNames(set3(18), levels(e$variable)))


# Existe uma função chamada ROC que calcula os retornos de todas as colunas
# Já verificamos que essas colunas não possuem missings
retornos_carteira = na.omit(ROC(novo_carteira[,-19]))

rendimento_carteira = Return.portfolio(retornos_carteira)

meant = apply(retornos_carteira,2,function (x) mean(x))

sdev = apply(retornos_carteira,2,function (x) sd(x))

tovar = rbind (meant,sdev)

tovarintermediario = data.frame(tovar)

#tovarspecial = tovarintermediario[,!(colnames(tovarintermed) %in% c("FJTA4"))]

tovar = data.frame(t(tovar))

#tovarspecial = data.frame(t(tovarspecial))

# Plot mais simples
ggplot(tovar, aes(x= sdev, y= meant)) + 
  geom_text(aes(label=rownames(tovar))) + 
  xlab('Volatilidade ou Desvio-Padrão') + ylab('Média dos Retornos')

# Plot complexidade média
ggplot(tovar, aes(x= sdev, y= meant, colour="green")) + 
  geom_text(aes(label=rownames(tovar), colour=sdev,size=meant)
            ,hjust=0, vjust=0,check_overlap = F) + 
  xlab('Volatilidade ou Desvio-Padrão') + ylab('Média dos Retornos') + 
  labs(size = "Retorno Médio",colour = "Volatilidade")


# Plot complexidade alta
ggplot(tovar, aes(x= sdev, y= meant, colour="green")) + 
  geom_text(aes(label=rownames(tovar), colour=sdev,size=meant), hjust=0, 
            vjust=0,check_overlap = F)  + scale_radius(range = c(4,6)) +
  xlab('Volatilidade ou Desvio-Padrão') + ylab('Média dos Retornos') + 
  theme(text = element_text(size=12))   + 
  coord_cartesian(xlim = c(0.015, 0.058), ylim = c(-0.0015, 0.0023)) +
  labs(size = "Retorno Médio",colour = "Volatilidade")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))
