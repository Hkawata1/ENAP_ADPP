####################################################### Importacao dos dados #######################################################
dados_anatel <- read.csv2("https://raw.githubusercontent.com/Hkawata1/ENAP_ADPP/master/Base_Anatel_TCC.csv", sep = ';', dec = ',',
                          header = TRUE)
head(dados_anatel)
str(dados_anatel)
dados_anatel$Processo <- as.factor(dados_anatel$Processo)
dados_anatel$Superintendencia <- as.factor(dados_anatel$Superintendencia)
dados_anatel$Ano_instauracao <- as.factor(dados_anatel$Ano_instauracao)
dados_anatel$Tipologia_processo <- as.factor(dados_anatel$Tipologia_processo)
dados_anatel$Grupo_economico <- as.factor(dados_anatel$Grupo_economico)
dados_anatel$Servico_telecom <- as.factor(dados_anatel$Servico_telecom)
dados_anatel$Renuncia <- as.factor(dados_anatel$Renuncia)
dados_anatel$Valor_multa <- as.character(dados_anatel$Valor_multa)
dados_anatel$Valor_multa <- gsub(',', '.', fixed = TRUE)
dados_anatel$Valor_multa <- as.numeric(dados_anatel$Valor_multa)

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

# Avaliacao das variaveis categoricas: teste Qui-quadrado com a variavel "Renuncia"
for(i in c("Superintendencia",
           "Ano_instauracao",
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

# Boxplot da variavel "Valor da multa"
graf_multa_boxplot <- ggplot(dados_anatel, aes(x = Renuncia, y = Valor_multa)) +
                              geom_boxplot(aes(fill = Renuncia), outlier.colour = "red", outlier.shape = 1, show.legend = FALSE) +
                              stat_boxplot(geom ='errorbar', width = 0.3, size= 0.7) +
                              xlab("Renúncia") +
                              ylab("Valores em R$") +                    
                              ggtitle("Valor da multa aplicada") +
                              scale_fill_manual(values = c("lightblue", "blue"))
graf_multa_boxplot

# Grafico com as variaveis qualitativas e variavel resposta
graf_superintendecia <- ggplot(dados_anatel, aes(x = Superintendencia, fill = Renuncia) ) +
                        geom_bar(position="fill") +
                        xlab("Superintendência responsável") +
                        ylab("Quantidade de processos") +
                        ggtitle("Distribuição de processos por Superintendência") +
                        scale_fill_manual(values = c("lightblue", "blue")) 
graf_superintendecia

graf_ano_instauracao <- ggplot(dados_anatel, aes(x = Ano_instauracao, fill = Renuncia) ) +
                        geom_bar(position="fill") +
                        xlab("Ano de instauração") +
                        ylab("Quantidade de processos") +
                        ggtitle("Distribuição de processos por ano") +
                        scale_fill_manual(values = c("lightblue", "blue")) 
graf_ano_instauracao

graf_serv_telecom <- ggplot(dados_anatel, aes(x = Servico_telecom, fill = Renuncia) ) +
                      geom_bar(position="fill") +
                      xlab("Tipos de serviços de telecomunicações") +
                      ylab("Quantidade de processos") +
                      ggtitle("Serviços de Telecomunicações") +
                      scale_fill_manual(values = c("lightblue", "blue")) 
graf_serv_telecom

graf_grupo_economico <- ggplot(dados_anatel, aes(x = Grupo_economico, fill = Renuncia) ) +
                        geom_bar(position="fill") +
                        xlab("Grupos econômicos") +
                        ylab("Quantidade de processos") +
                        ggtitle("Distribuição por grupo econômico") +
                        scale_fill_manual(values = c("lightblue", "blue")) +
                        coord_flip()
graf_grupo_economico

graf_tipo_proc <- ggplot(dados_anatel, aes(x = Tipologia_processo, fill = Renuncia) ) +
                  geom_bar(position="fill") +
                  xlab("Tipologias de processo") +
                  ylab("Quantidade de processos") +
                  ggtitle("Tipos de processos") +
                  scale_fill_manual(values = c("lightblue", "blue")) +
                  coord_flip()
graf_tipo_proc

######################################## Balanceamento e Geracao da tabela de treino e teste ########################################
library(lme4)
library(ROSE)

# Balanceamento dos dados
dados_anatel_balanceados <- ovun.sample(Renuncia ~ ., data = dados_anatel, method = "over", N = 3154, seed = 1)$data
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
cart <- rpart(Renuncia ~ Superintendencia + Ano_instauracao + Tipologia_processo + Servico_telecom + Grupo_economico + Valor_multa, data = treino)
prp(cart, main = "Classificação de multas aplicadas - Variável Renúncia",type = 4, extra = 4,box.palette = "auto",faclen = 0)

# Teste de validacao cruzada
printcp(cart)
plotcp(cart)

# Pela regra one-standard-deviation a árvore deve ser podada no cp = 0.013, ficando com 6 nós terminais. Como a árvore inicialmente
# tinha 8 nós terminais, a poda resultará numa árvore diferente da obtida inicialmente.

# Poda da arvore
pcart<- prune(cart, cp=0.013)
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
cart_balanceado <- rpart(Renuncia ~ Superintendencia + Ano_instauracao + Tipologia_processo + Servico_telecom + Grupo_economico +
                           Valor_multa, data = treino_balanceado)
prp(cart_balanceado, main = "Classificação de multas aplicadas - Variável Renúncia",type = 4, extra = 4,box.palette = "auto",faclen = 0)

# Teste de validacao cruzada
printcp(cart_balanceado)
plotcp(cart_balanceado)

# Pela regra one-standard-deviation a árvore deve ser podada no cp = 0.02, ficando com 4 nós terminais. Como a árvore inicialmente
# tinha 6 nós terminais, a poda resultará numa árvore diferente da obtida inicialmente.

# Poda da arvore
pcart_balanceado <- prune(cart_balanceado, cp=0.021)
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
# treino nao balanceado. Entretanto, em relacao a base de teste a taxa de assertividade diminui consideravelmente. Desta forma, será
# utilizada a base nao balanceada para fins deste estudo.

# Regressao logistica com dados nao balanceados
rlog <- glm(formula = Renuncia ~ Superintendencia + Ano_instauracao + Tipologia_processo + Servico_telecom + Grupo_economico + Valor_multa,
            data = treino, family = binomial())
summary(rlog)
step(rlog, trace = 1, direction = c("backward"))
step(rlog, trace = 1, direction = c("both"))

# Optou-se por testar primeiramente todas as variaveis (rlog) e depois combinando-as com a variavel "valor de multa" (rlog2)

rlog2 <- glm(formula = Renuncia ~ Superintendencia + Ano_instauracao + Tipologia_processo + Servico_telecom + Grupo_economico + Valor_multa +
               Tipologia_processo*Valor_multa + Servico_telecom*Valor_multa + Grupo_economico*Valor_multa, data = treino, family = binomial())
summary(rlog2)
step(rlog2, trace = 1, direction = c("backward"))
step(rlog2, trace = 1, direction = c("both"))
warnings()

# Apesar do modelo rlog possuir melhor AIC, optou-se por utilizar os ajustes sugeridos para o modelo rlog2, pois este considera uma combinacao
# de variaveis que permite uma visao diferente do modelo de arvore de decisao acima.

# Ajuste do modelo "rlog2" pelas variaveis selecionadas acima

rlog3 <- glm(formula = Renuncia ~ Tipologia_processo + Servico_telecom + Grupo_economico + Valor_multa + Tipologia_processo*Valor_multa +
               Servico_telecom*Valor_multa + Grupo_economico*Valor_multa, family = binomial(), data = treino)
summary(rlog3)

# Avaliacao do modelo de regressao logistica
probabilidades.treino <- predict(rlog3, type = "response", treino)
predicted.classes.treino <- ifelse(probabilidades.treino > 0.5, "Yes", "No")
table(treino$Renuncia, predicted.classes.treino)
sum(diag(prop.table(table(treino$Renuncia, predicted.classes.treino))))

probabilidades.teste <- predict(rlog3, type = "response", teste)
predicted.classes.teste <- ifelse(probabilidades.teste > 0.5, "Yes", "No")
table(teste$Renuncia, predicted.classes.teste)
sum(diag(prop.table(table(teste$Renuncia, predicted.classes.teste))))

# O modelo rlog3 apresenta taxas de acerto similares ao modelo arvore de decisao.

################################################### Avaliacao dos modelos construidos ##############################################
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
pred.rlog <- predict(rlog3, type = "terms", teste)
confusionMatrix(tab_rlog, pred.rlog[,2], threshold = 0.5)
precision(tab_rlog, pred.rlog[,2], threshold = 0.5)
sensitivity(tab_rlog, pred.cart[,2], threshold = 0.5)
specificity(tab_rlog, pred.cart[,2], threshold = 0.5)
ks_stat(tab_rlog, pred.rlog[,2])
plotROC(tab_rlog, pred.rlog[,2])
AUROC(tab_rlog, pred.rlog[,2])
ks_plot(tab_rlog, pred.rlog[,2])

# Na comparacao dos modelos, a arvore de decisao possui AUROC um pouco maior, bem como na estatistica KS, por isso sera o modelo a ser
# apresentado no artigo.

# Grafico de comparacao dos modelos
library(pROC)
roc.cart <- plot.roc(tab_cart, pred.cart[,2], main="Comparação de modelos", percent=TRUE, xlim=c(100,0), col="blue")
roc.rlog <- lines.roc(tab_rlog, pred.rlog[,2], percent=TRUE, col="red")
legend("bottomright", legend = c("Árvore de decisão", "Regressão logística"),col = c("blue", "red"),lwd = 2)

roc.cart.ci <- plot.roc(tab_cart, pred.cart[,2], main = "Intervalo de confiança", percent=TRUE, ci = TRUE, print.auc = TRUE)           
ci.cart <- ci.se(roc.cart, specificities = seq(0, 100, 5)) 
plot(ci.cart, type = "shape", col = "#1c61b6AA")
roc.rlog.ci <- plot.roc(tab_rlog, pred.rlog[,2], main = "Intervalo de confiança", percent=TRUE, ci = TRUE, print.auc = TRUE)           
ci.rlog <- ci.se(roc.rlog, specificities = seq(0, 100, 5)) 
plot(ci.rlog, type = "shape", col = "#1c61b6AA")

