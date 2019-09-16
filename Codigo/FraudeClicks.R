setwd("C:/Deisi/Github/DSA_Projeto01/DSA_Projeto01_FraudeClicks/Codigo")


library("dplyr")
library("caret")
library("DMwR")

test <- read.csv("test.csv")
head(test)
table(test$is_attributed)


df <- read.csv("train_sample.csv")
#View(df)

#Não existe registros NA com 100.000 observações e 8 variáveis
any(is.na(df))
dim(df)
str(df)

#variavel target desbalanceada 0-99773    1-227
table(df$is_attributed)
df$is_attributed <- as.factor(df$is_attributed)
df_balanc <- SMOTE(is_attributed ~.,df )
table(df_balanc$is_attributed)



#tranformar as variaveis em factor
colunas <- c("ip", "app", "device", "os", "channel", "is_attributed")
df[colunas] <- lapply(df[colunas], as.factor)
str(df)

#tratar data $click_time
#attributed_time somente preenchida quando is_attributed=1. 
#Portanto não usá-lo como variavel preditiva
#Dividir os dias dos horarios
#Dividir os dias em dias de semana e final de semana
#Dividir os horarios em peridos: manha, tarde e noite
#Analisar quais dias e quais os periodos que possem maior possibilidade de click e nao click


Hours <- format(as.POSIXct(strptime(df$click_time,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M:%S")

Dates <- format(as.POSIXct(strptime(df$click_time,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%d/%m/%Y")

df$Dates <- Dates
df$Hours <- Hours

dias_semana <- c('segunda-feira', 'terça-feira', 'quarta-feira', 'quinta-feira', 'sexta-feira')
df$UtilDay <- factor((weekdays(as.POSIXct(df$Dates)) %in% dias_semana), 
                   levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))

#resultado zero(0) para weekend, indica que os dados foram coletados apenas nos weekday e não nos weekend.
#não será efetuado a divisão por weekend e weekday 
#mas as datas serão transformadas em dias da semana ( de segunda a domingo)
table(df$UtilDay)
df$UtilDay <- NULL

df$week <- factor(weekdays(as.POSIXct(df$Dates)))
barplot(table(df$week))

week_df <- df %>% group_by(week,is_attributed )  %>%  summarise(count=n())
ggplot(week_df, aes(week, count, fill = is_attributed)) + 
  geom_bar(stat="identity", position = "dodge")

time2 <- as.numeric(format(as.POSIXct(df$Hours, tz = "" , format = "%H: %M:%S"), "%H"))

df$Hours_periodo <- ifelse(time2 >= 00 & time2 <= 05, "madrugada",
                          ifelse(time2 > 05 & time2 <= 12, "manha",
                                ifelse(time2 > 12 & time2 <= 18, "tarde","noite")))



graf_df <- df %>% group_by(Hours_periodo,is_attributed )  %>%  summarise(count=n())
ggplot(graf_df, aes(Hours_periodo, count, fill = is_attributed)) + 
  geom_bar(stat="identity", position = "dodge")





#dividir dados em teste e treino
indices <- sample(seq_len(nrow(df)),nrow(df)*0.7 )
treino <- df[indices,]
teste <-  df[-indices,]

#identificar variaveis mais relevantes
#Tentei usar o randomForest mas não foi possivel por possuir muitos levels nas variaveis fator.
#Tentei usar train mas o sistema não pode alocar vetor com 48Gb
#control <- trainControl(method= "repeatedcv", number=7, repeats=2 )
#model_import <- train(is_attributed ~. , data = treino, method="glm", trControl = control)


#################aplicar randonForest nos dados originais transformados
#embora o resultado da acurácia tenha sido alto, a impressão é que os dados não balanceados 
#influenciaram no resultado
library(rpart)

rf_v1 = rpart(is_attributed~ip+app+device+os+channel+week+Hours_periodo,
              data=treino)
pred_rf_v1 = predict(rf_v1, teste, type='class')
table(pred_rf_v1, teste$is_attributed)
mean(pred_rf_v1==teste$is_attributed)

#################aplicar randonForest no modelo balanceado sem transformação 
table(df_balanc$is_attributed)

indices <- sample(seq_len(nrow(df_balanc)),nrow(df_balanc)*0.7 )
balanc_treino <- df_balanc[indices,]
balanc_teste <-  df_balanc[-indices,]


rf_v2_balanc = rpart(is_attributed~ip+app+device+os+channel,
              data=balanc_treino)
pred_rf_v2_balanc = predict(rf_v2_balanc, balanc_teste, type='class')
table(pred_rf_v2_balanc, balanc_teste$is_attributed)
mean(pred_rf_v2_balanc==balanc_teste$is_attributed)

#################utilizando dados balanceados e normalizados
df_balanc_norm <-df_balanc

df_balanc_norm["ip"] <- scale(df_balanc_norm["ip"])
df_balanc_norm["device"] <- scale(df_balanc_norm["device"])
df_balanc_norm["os"] <- scale(df_balanc_norm["os"])
df_balanc_norm["channel"] <- scale(df_balanc_norm["channel"])

indices <- sample(seq_len(nrow(df_balanc_norm)),nrow(df_balanc_norm)*0.7 )
balanc_norm_treino <- df_balanc_norm[indices,]
balanc_norm_teste <-  df_balanc_norm[-indices,]

rf_v2_balanc_norm = rpart(is_attributed~ip+app+device+os+channel,
                     data=balanc_norm_treino)
pred_rf_v2_balanc_norm = predict(rf_v2_balanc_norm, balanc_norm_teste, type='class')
table(pred_rf_v2_balanc_norm, balanc_norm_teste$is_attributed)
mean(pred_rf_v2_balanc_norm==balanc_norm_teste$is_attributed)

#como este modelo apresentou melhor acurária, aplicando os dados de teste ao modelo
test["ip"] <- scale(test["ip"])
test["device"] <- scale(test["device"])
test["os"] <- scale(test["os"])
test["channel"] <- scale(test["channel"])

test_predic = predict(rf_v2_balanc_norm, test, type='class')

result<- cbind.data.frame(test$click_id,test_predic )
write.csv(result, file = "resultado.csv")
?write.csv

#################aplicar randonForest no modelo balanceado com transformação: 

df_balanc_transf <- df_balanc


colunas <- c("ip", "app", "device", "os", "channel", "is_attributed")
df_balanc_transf[colunas] <- lapply(df_balanc_transf[colunas], as.factor)

Hours <- format(as.POSIXct(strptime(df_balanc_transf$click_time,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M:%S")

Dates <- format(as.POSIXct(strptime(df_balanc_transf$click_time,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%d/%m/%Y")

df_balanc_transf$Dates <- Dates
df_balanc_transf$Hours <- Hours

df_balanc_transf$week <- factor(weekdays(as.POSIXct(df_balanc_transf$Dates)))
week_df <- df_balanc_transf %>% group_by(week,is_attributed )  %>%  summarise(count=n())
ggplot(week_df, aes(week, count, fill = is_attributed)) + 
  geom_bar(stat="identity", position = "dodge")


time2 <- as.numeric(format(as.POSIXct(df_balanc_transf$Hours, tz = "" , format = "%H: %M:%S"), "%H"))

df_balanc_transf$Hours_periodo <- ifelse(time2 >= 00 & time2 <= 05, "madrugada",
                           ifelse(time2 > 05 & time2 <= 12, "manha",
                                  ifelse(time2 > 12 & time2 <= 18, "tarde","noite")))

graf_df <- df_balanc_transf %>% group_by(Hours_periodo,is_attributed )  %>%  summarise(count=n())
ggplot(graf_df, aes(Hours_periodo, count, fill = is_attributed)) + 
  geom_bar(stat="identity", position = "dodge")

indices <- sample(seq_len(nrow(df_balanc_transf)),nrow(df_balanc_transf)*0.7 )
balanc_transf_treino <- df_balanc_transf[indices,]
balanc_transf_teste <-  df_balanc_transf[-indices,]


rf_v3_balanc_transf = rpart(is_attributed~ip+app+device+os+channel+week+Hours_periodo,
                     data=balanc_transf_treino)
pred_rf_v3_balanc_transf = predict(rf_v3_balanc_transf, balanc_transf_teste, type='class')
table(pred_rf_v3_balanc_transf, balanc_transf_teste$is_attributed)
mean(pred_rf_v3_balanc_transf==balanc_transf_teste$is_attributed)





