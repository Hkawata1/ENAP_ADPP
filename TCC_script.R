####################################################### Importacao dos dados #######################################################
dados_anatel <- read.csv2("https://raw.githubusercontent.com/Hkawata1/ENAP_ADPP/master/Base_Pados_Anatel.csv", sep = ';', dec = ',',
                          header = TRUE)
head(dados_anatel)
str(dados_anatel)
dados_anatel$Valor_multa <- as.character(dados_anatel$Valor_multa)
dados_anatel$Valor_multa <- gsub(',', '.', fixed = TRUE)
dados_anatel$Valor_multa <- as.numeric(dados_anatel$Valor_multa)
dados_anatel$Duracao_processo <- as.character(dados_anatel$Duracao_processo)
dados_anatel$Duracao_processo <- gsub(',', '.', fixed = TRUE)
dados_anatel$Duracao_processo <- as.numeric(dados_anatel$Duracao_processo)
dados_anatel$Saldo_devedor <- as.character(dados_anatel$Saldo_devedor)
dados_anatel$Saldo_devedor <- gsub(',', '.', fixed = TRUE)
dados_anatel$Saldo_devedor <- as.numeric(dados_anatel$Saldo_devedor)

###################################################### Analise descritiva ##########################################################
library(ggplot2)
library(corrplot)
summary(dados_anatel)
# Aparentemente as variaveis quantitativas estão sendo lidas corretamente com as transformações realizadas acima.

# Para a variavel resposta duas possibilidades foram consideradas: "Renuncia" e "Situacao da multa".
graf_renuncia <- ggplot(dados_anatel, aes(x = Renuncia, fill = Renuncia)) +
                  geom_bar() +
                  xlab("Renúncia") +
                  ylab("Quantidade de processos") +
                  ggtitle("Variável Resposta - Renúncia") +
                  scale_fill_manual(values = c("lightblue", "blue"))
graf_renuncia

graf_situacao_multa <- ggplot(dados_anatel, aes(x = Situacao_multa, fill = Situacao_multa)) +
                      geom_bar() +
                      xlab("Situação da multa") +
                      ylab("Quantidade de processos") +
                      ggtitle("Variável Resposta - Situação da multa") +
                      scale_fill_manual(values = c("lightblue", "blue", "darkblue"))
graf_situacao_multa

# Avaliacao das variaveis categoricas: teste Qui-quadrado com a variavel "Renuncia"
for(i in c("Superintendencia",
          "Tipologia_processo",
          "Servico_telecom",
          "Grupo_economico"))
{
  print(i)
  print(table(dados_anatel$Renuncia,dados_anatel[,i]))
  print(chisq.test(table(dados_anatel$Renuncia,dados_anatel[,i])))
}

# As análises demonstram que a hipotese nula e rejeitada para cada uma das variaveis analisadas, indicando que não há dependencia entre
# as variaveis independentes em relacao a variavel dependente.

# Analise das variaveis qualitativas

# Boxplot da variavel "Duracao do processo"
graf_duracao_boxplot <- ggplot(dados_anatel, aes(x = Renuncia, y = Duracao_processo)) +
                        geom_boxplot(aes(fill = Renuncia), outlier.colour = "red", outlier.shape = 1, show.legend = FALSE) +
                        stat_boxplot(geom ='errorbar', width = 0.3, size= 0.7) +
                        xlab("Renúncia") +
                        ylab("Duração em dias") +                    
                        ggtitle("Duração do processo") +
                        scale_fill_manual(values = c("lightblue", "blue"))
graf_duracao_boxplot

# Muitos outliers desta variavel. Neste caso, para a aplicacao da regressao logistica tera que ser feito o tratamento dos outliers.
graf_duracao_boxplot_log <- ggplot(dados_anatel, aes(x = Renuncia, y = log(Duracao_processo))) +
                            geom_boxplot(aes(fill = Renuncia), outlier.colour = "red", outlier.shape = 1, show.legend = FALSE) +
                            stat_boxplot(geom ='errorbar', width = 0.3, size= 0.7) +
                            xlab("Renúncia") +
                            ylab("Duração em dias") +                    
                            ggtitle("Duração do processo") +
                            scale_fill_manual(values = c("lightblue", "blue"))
graf_duracao_boxplot_log

# Boxplot da variavel "Valor da multa"
graf_multa_boxplot <- ggplot(dados_anatel, aes(x = Renuncia, y = Valor_multa)) +
                              geom_boxplot(aes(fill = Renuncia), outlier.colour = "red", outlier.shape = 1, show.legend = FALSE) +
                              stat_boxplot(geom ='errorbar', width = 0.3, size= 0.7) +
                              xlab("Renúncia") +
                              ylab("Valores em R$") +                    
                              ggtitle("Valor da multa aplicada") +
                              scale_fill_manual(values = c("lightblue", "blue"))
graf_multa_boxplot

# Tratamento dos outlier da variavel "Valor da multa"
graf_multa_boxplot_sin <- ggplot(dados_anatel, aes(x = Renuncia, y = sin(Valor_multa))) +
                          geom_boxplot(aes(fill = Renuncia), outlier.colour = "red", outlier.shape = 1, show.legend = FALSE) +
                          stat_boxplot(geom ='errorbar', width = 0.3, size= 0.7) +
                          xlab("Renúncia") +
                          ylab("Valores em R$") +                    
                          ggtitle("Valor da multa aplicada") +
                          scale_fill_manual(values = c("lightblue", "blue"))
graf_multa_boxplot_sin

# Boxplot da variavel "Saldo devedor"
graf_saldo_boxplot <- ggplot(dados_anatel, aes(x = Renuncia, y = Saldo_devedor)) +
                      geom_boxplot(aes(fill = Renuncia), outlier.colour = "red", outlier.shape = 1, show.legend = FALSE) +
                      stat_boxplot(geom ='errorbar', width = 0.5, size= 0.7) +
                      xlab("Renúncia") +
                      ylab("Valores em R$") +                    
                      ggtitle("Saldo devedor principal") +
                      scale_fill_manual(values = c("lightblue", "blue"))
graf_saldo_boxplot

# Tratamento dos outlier da variavel "Saldo devedor"
graf_saldo_boxplot_sin <- ggplot(dados_anatel, aes(x = Renuncia, y = sin(Saldo_devedor))) +
                          geom_boxplot(aes(fill = Renuncia), outlier.colour = "red", outlier.shape = 1, show.legend = FALSE) +
                          stat_boxplot(geom ='errorbar', width = 0.5, size= 0.7) +
                          xlab("Renúncia") +
                          ylab("Valores em R$") +                    
                          ggtitle("Saldo devedor principal") +
                          scale_fill_manual(values = c("lightblue", "blue"))
graf_saldo_boxplot_sin

# Grafico com as variaveis qualitativas e variavel resposta
graf_serv_telecom <- ggplot(dados_anatel, aes(x = Servico_telecom, fill = Renuncia) ) +
                      geom_bar(position="fill") +
                      xlab("Tipos de serviços de telecomunicações") +
                      ylab("Quantidade de processos") +
                      ggtitle("Serviços de Telecomunicações") +
                      scale_fill_manual(values = c("lightblue", "blue")) 
graf_serv_telecom
graf_sit_multa <- ggplot(dados_anatel, aes(x = Situacao_multa, fill = Renuncia) ) +
                  geom_bar(position="fill") +
                  xlab("Situação da multa") +
                  ylab("Quantidade de processos") +
                  ggtitle("Situação da multa aplicada") +
                  scale_fill_manual(values = c("lightblue", "blue")) 
graf_sit_multa
graf_rec_jur <- ggplot(dados_anatel, aes(x = Recurso_juridico, fill = Renuncia) ) +
                geom_bar(position="fill") +
                xlab("Processos com Recurso jurídico") +
                ylab("Quantidade de processos") +
                ggtitle("Processos em 2ª instância") +
                scale_fill_manual(values = c("lightblue", "blue")) 
graf_rec_jur
graf_cadin <- ggplot(dados_anatel, aes(x = Cadin, fill = Renuncia) ) +
              geom_bar(position="fill") +
              xlab("Processos em Cadin") +
              ylab("Quantidade de processos") +
              ggtitle("Registro no Cadin") +
              scale_fill_manual(values = c("lightblue", "blue")) 
graf_cadin
graf_div_ativa <- ggplot(dados_anatel, aes(x = Divida_ativa, fill = Renuncia) ) +
                  geom_bar(position="fill") +
                  xlab("Processos em Dívida Ativa") +
                  ylab("Quantidade de processos") +
                  ggtitle("Inscrição em Dívida Ativa") +
                  scale_fill_manual(values = c("lightblue", "blue")) 
graf_div_ativa
graf_tipo_proc <- ggplot(dados_anatel, aes(x = Tipologia_processo, fill = Renuncia) ) +
                  geom_bar(position="fill") +
                  xlab("Tipologias de processo") +
                  ylab("Quantidade de processos") +
                  ggtitle("Tipos de processos") +
                  scale_fill_manual(values = c("lightblue", "blue")) +
                  coord_flip()
graf_tipo_proc
graf_multa <- qplot(Valor_multa, data = dados_anatel, fill=Renuncia) +
              scale_fill_manual(values = c("lightblue", "blue")) +
              xlab("Valor da multa aplicada") +
              ylab("Quantidade de processos") +
              ggtitle("Processos com multa aplicada")
graf_multa
graf_duracao_proc <- qplot(Duracao_processo, data = dados_anatel, geom = "density", color = Renuncia) +
                      scale_color_manual(values = c("lightblue", "blue")) +
                      xlab("Duração em dias") +
                      ylab("Quantidade de processos") +
                      ggtitle("Duração dos processos")
graf_duracao_proc

# Teste: sugestao de graficos adicionais
base_cor_dados_anatel <- dados_anatel[,c("Duracao_processo", "Valor_multa", "Saldo_devedor")]
corrplot(cor(base_cor_dados_anatel), method="color",order="hclust",addCoef.col = "black",type="upper",diag=FALSE)
heatmap(x = cor(base_cor_dados_anatel), symm = TRUE, Rowv = FALSE,cexRow = 0.9,cexCol = 0.9,margins=c(7,7))

######################################## Balanceamento e Geracao da tabela de treino e teste ########################################
library(lme4)
library(ROSE)

# Balanceamento dos dados
dados_anatel_balanceados <- ovun.sample(Renuncia ~ ., data = dados_anatel, method = "over", N = 3104, seed = 1)$data
table(dados_anatel_balanceados$Renuncia)
set.seed(123)  
amostra_balanceado <- sample.int(n = nrow(dados_anatel_balanceados), size = floor(.70*nrow(dados_anatel_balanceados)), replace = FALSE)
treino_balanceado <- dados_anatel_balanceados[amostra, ]
teste_balanceado  <- dados_anatel_balanceados[-amostra, ]
table(treino_balanceado$Renuncia)

# Sem Balanceamento
set.seed(123)  
amostra <- sample.int(n = nrow(dados_anatel), size = floor(.70*nrow(dados_anatel)), replace = FALSE)
treino <- dados_anatel[amostra, ]
teste  <- dados_anatel[-amostra, ]
table(treino$Renuncia)

################################################## Construcao da arvore de decisao #################################################
library(rpart)
library(rpart.plot)
library(partykit)
names(dados_anatel)

# Arvore com a variavel "Renuncia" com dados nao balanceados
cart <- rpart(Renuncia ~ Tipologia_processo + Servico_telecom + Grupo_economico + Valor_multa, data = treino)
prp(cart, main = "Classificação de multas aplicadas - Variável Renúncia",type = 4, extra = 4,box.palette = "auto",faclen = 0)

# Teste de validacao cruzada
printcp(cart)
plotcp(cart)

# Pela regra one-standard-deviation a árvore deve ser podada no cp = 0.019, ficando com 9 nós terminais. Como a árvore inicialmente
# tinha 14 nós terminais, a poda resultará numa árvore diferente da obtida inicialmente.

# Poda da arvore
pcart<- prune(cart, cp=0.019)
summary(pcart)

# Arvore "podada"
prp(pcart, main = "Classificação de multas aplicadas - Variável Renúncia",type = 4, extra = 4,box.palette = "auto",faclen = 0)
# outra forma de plotar a arvore (pacote partykit)
plot(as.party(pcart), type="simple")

# Avaliacao do modelo arvore de decisao
p.treino <- predict(pcart, type = "class", treino)
c.treino <- table(treino$Renuncia, p.treino)
c.treino #matriz de confusao
sum(diag(prop.table(c.treino)))

p.teste <- predict(pcart, type = "class", teste)
c.teste <- table(teste$Renuncia, p.teste)
c.teste #matriz de confusao
sum(diag(prop.table(c.teste)))

# Taxas de assertividade da base de treino e teste demonstram que não há indícios de overfitting.

# Arvore com a variavel "Renuncia" com dados balanceados
cart_balanceado <- rpart(Renuncia ~ Tipologia_processo + Servico_telecom + Grupo_economico + Valor_multa, data = treino_balanceado)
prp(cart_balanceado, main = "Classificação de multas aplicadas - Variável Renúncia",type = 4, extra = 4,box.palette = "auto",faclen = 0)

# Teste de validacao cruzada
printcp(cart_balanceado)
plotcp(cart_balanceado)

# Pela regra one-standard-deviation a árvore deve ser podada no cp = 0.025, ficando com 9 nós terminais. Como a árvore inicialmente
# tinha 15 nós terminais, a poda resultará numa árvore diferente da obtida inicialmente.

# Poda da arvore
pcart_balanceado <- prune(cart_balanceado, cp=0.025)
summary(pcart_balanceado)

# Arvore "podada"
prp(pcart_balanceado, main = "Classificação de multas aplicadas - Variável Renúncia",type = 4, extra = 4,box.palette = "auto",faclen = 0)
# Grafico alternativo da arvore
plot(as.party(pcart_balanceado), type="simple")

# Avaliacao do modelo arvore de decisao
p.treino_balanceado <- predict(pcart_balanceado, type = "class", treino_balanceado)
c.treino_balanceado <- table(treino_balanceado$Renuncia, p.treino_balanceado)
c.treino_balanceado #matriz de confusao
sum(diag(prop.table(c.treino_balanceado)))

p.teste_balanceado <- predict(pcart_balanceado, type = "class", teste_balanceado)
c.teste_balanceado <- table(teste_balanceado$Renuncia, p.teste_balanceado)
c.teste_balanceado #matriz de confusao
sum(diag(prop.table(c.teste_balanceado)))

# A avaliacao do modelo com a base treino e teste balanceada indica que o modelo tem taxa de assertividade parecida em relacao a base de
# treino nao balanceado. Entretanto, em relacao a base de teste a taxa de assertividade diminui consideravelmente.

# Teste: Modelo RandomForest - dados nao balanceados
library(randomForest)
rf <- randomForest(Renuncia ~ Tipologia_processo + Servico_telecom + Grupo_economico + Valor_multa, data = treino, importance = TRUE)
rf
plot(rf$err.rate[,1],type="l")
importance(rf)
varImpPlot(rf)

# Teste: Modelo RandomForest - dados balanceados
rf_balanceado <- randomForest(Renuncia ~ Tipologia_processo + Servico_telecom + Grupo_economico + Valor_multa, data = treino_balanceado,
                              importance = TRUE)
rf_balanceado
plot(rf_balanceado$err.rate[,1],type="l")
importance(rf_balanceado)
varImpPlot(rf_balanceado)

# Rodei o modelo "random forest" apenas para gerar o grafico sobre a importancia das variaveis escolhidas. O grafico dos dados balanceados
# indica onde pode estar a diferenca na taxa de acerto quando se utiliza a base de teste.

# Regressao logistica com dados nao balanceados
rlog <- glm(formula = Renuncia ~ Tipologia_processo + Servico_telecom + Grupo_economico + Valor_multa, data = treino, family = binomial())
summary(rlog)
step(rlog, trace = 1, direction = c("both"))

rlog2 <- glm(formula = Renuncia ~ Tipologia_processo + Servico_telecom + Grupo_economico + Valor_multa + Tipologia_processo*Valor_multa +
               Servico_telecom*Valor_multa + Grupo_economico*Valor_multa, data = treino, family = binomial())
summary(rlog2)
step(rlog2, trace = 1, direction = c("both"))

# Aparentemente a maior parte das categorias das variaveis selecionadas nao sao significativas.

# Avaliacao do modelo de regressao logistica
probabilidades.treino <- predict(rlog, type = "response", treino)
predicted.classes.treino <- ifelse(probabilidades.treino > 0.5, "Yes", "No")
table(treino$Renuncia, predicted.classes.treino)
sum(diag(prop.table(table(treino$Renuncia, predicted.classes.treino))))

probabilidades.teste <- predict(rlog, type = "response", teste)
predicted.classes.teste <- ifelse(probabilidades.teste > 0.5, "Yes", "No")
table(teste$Renuncia, predicted.classes.teste)
sum(diag(prop.table(table(teste$Renuncia, predicted.classes.teste))))

################################################### Avaliacao dos modelos construidos ##############################################
# Modelo Arvore de decisao com variavel "Renuncia" (dados nao balanceados)
p.Treino_cart <- predict(pcart, type = "class", treino)
c.treino_cart <- table(treino$Renuncia, p.Treino_cart)
c.treino_cart
sum(diag(prop.table(c.treino_cart)))
p.Teste_cart <- predict(pcart, type = "class", teste)
c.teste_cart <- table(teste$Renuncia, p.Teste_cart)
c.teste_cart
sum(diag(prop.table(c.teste_cart)))
# Comparando a taxa de acerto do modelo nos dois conjuntos (de treinamento e de teste), é possível afirmar que o modelo não 
# apresenta overfitting.

# Modelo Regressao logistica com variavel "Renuncia" (dados nao balanceados)
p.Treino_rlog2 <- predict(rlog2, type = "class", treino)
c.treino_rlog2 <- table(treino$Renuncia, p.Treino_rlog2)
c.treino_rlog2
sum(diag(prop.table(c.treino_rlog2)))
p.Teste_rlog2 <- predict(rlog2, type = "class", teste)
c.teste_rlog2 <- table(teste$Renuncia, p.Teste_rlog2)
c.teste_rlog2
sum(diag(prop.table(c.teste_rlog2)))

# Teste: avaliacao dos tipos de classificadores
library(InformationValue)
tab_cart <- ifelse(teste$Renuncia == "Sim",1,0)
pred.cart <- predict(pcart, type = "prob", teste)
confusionMatrix(tab_cart, pred.cart[,2], threshold = 0.5)
precision(tab_cart, pred.cart[,2], threshold = 0.5)
sensitivity(tab_cart, pred.cart[,2], threshold = 0.5)
specificity(tab_cart, pred.cart[,2], threshold = 0.5)
ks_stat(tab_cart, pred.cart[,2])
plotROC(tab_cart, pred.cart[,2])
AUROC(tab_cart, pred.cart[,2])
ks_plot(tab_cart, pred.cart[,2])

tab_rlog <- ifelse(teste$Renuncia == "Sim",1,0)
pred.rlog <- predict(rlog2, type = "response", teste)
confusionMatrix(tab_rlog, pred.rlog[,2], threshold = 0.5)
precision(tab_rlog, pred.rlog[,2], threshold = 0.5)
sensitivity(tab_rlog, pred.cart[,2], threshold = 0.5)
specificity(tab_rlog, pred.cart[,2], threshold = 0.5)
ks_stat(tab_rlog, pred.rlog[,2])
plotROC(tab_rlog, pred.rlog[,2])
AUROC(tab_rlog, pred.rlog[,2])
ks_plot(tab_rlog, pred.rlog[,2])

# Grafico de comparacao dos modelos
library(pROC)
roc.cart <- plot.roc(tab_cart, pred.cart[,1], main="Comparação de modelos", percent=TRUE, xlim=c(100,0), col="blue")
roc.rlog <- lines.roc(tab_rlog, pred.rlog[,2], percent=TRUE, col="red")
legend("bottomright", legend = c("Árvore de decisão", "Regressão logística"),col = c("blue", "red"),lwd = 2)
roc.cart.ci <- plot.roc(tab_cart, pred.cart[,1], main = "Intervalo de confiança", percent=TRUE, ci = TRUE, print.auc = TRUE)           
ci.cart <- ci.se(roc.cart, specificities = seq(0, 100, 5)) 
plot(ci.cart, type = "shape", col = "#1c61b6AA")
roc.rlog.ci <- plot.roc(tab_rlog, pred.rlog[,1], main = "Intervalo de confiança", percent=TRUE, ci = TRUE, print.auc = TRUE)           
ci.rlog <- ci.se(roc.rlog, specificities = seq(0, 100, 5)) 
plot(ci.rlog, type = "shape", col = "#1c61b6AA")