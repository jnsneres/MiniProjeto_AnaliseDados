library(ggplot2)
library(tidyverse)
library("scales")
library("dplyr")
setwd("~/workspace/MiniProjeto")
#ler arquivo
arq <- read.csv('dados_turma.csv'); arq 

#RENOMEANDO COLUNAS
colnames(arq)[1]<- "I"
colnames(arq)[2]<- "S"
colnames(arq)[3]<- "SEM"
colnames(arq)[4]<- "DP"
colnames(arq)[5]<- "t"
colnames(arq)[6]<- "sal"
colnames(arq)[7]<- "TR"
colnames(arq)[8]<- "DIST"
colnames(arq)[9]<- "ENT"
arq

#SUBSTITUINDO VALORES
arq$sal[arq$sal == "Prefiro não responder"] <- "N.A"
arq
arq$sal[arq$sal == "Menos de 1 salário mínimo"] <- "-1 S.M."
arq
arq$sal[arq$sal == "De 1 a 3 salários mínimos"] <- "1-3 S.M."
arq
arq$sal[arq$sal == "De 4 a 6 salários mínimos"] <- "4-6 S.M."
arq
arq$sal[arq$sal == "7 salários mínimos ou mais"] <- "7+ S.M."
arq

arq$t[arq$t == "Não"] <- "N�o"
arq

arq$TR[arq$TR == "Aplicativos (Uber, 99 etc)"] <- "1"
arq
arq$TR[arq$TR == "Transporte público"] <- "2"
arq
arq$TR[arq$TR == "Particular (carro ou moto)"] <- "3"
arq

arq$DIST[arq$DIST == "De 1 a 3 KM"] <- "1-3 Km"
arq
arq$DIST[arq$DIST == "De 4 a 6 KM"] <- "4-6 Km"
arq
arq$DIST[arq$DIST == "Mais de 6 KM"] <- "+6 Km"
arq

arq$ENT[arq$ENT == "Menos de 1 hora"] <- "0"
arq
arq$ENT[arq$ENT == "1 hora"] <- "1"
arq
arq$ENT[arq$ENT == "2 horas"] <- "2"
arq
arq$ENT[arq$ENT == "3 horas"] <- "3"
arq
arq$ENT[arq$ENT == "Mais de 3 horas"] <- "4"
arq


# 2. Mostre um gr�fico apropriado para os seguintes itens

# 2.1 Percentual por I.

ggplot(data = arq) +
  geom_bar(mapping = aes(x = I, y =..prop.., group = 1), stat = "count")+
  scale_y_continuous(labels = scales::percent_format())+
  labs(title = "Idade", x = 'Idade',
     y = "%")


#2.2 Percentual por T.

ggplot(data = arq) +
  geom_bar(mapping = aes(x = t, y =..prop.., group = 1), stat = "count")+
  scale_y_continuous(labels = scales::percent_format())+
  labs(title = "Trabalha na �rea?", x = 'Sim ou n�o',
       y = "%")

#2.3 DP por DIST.
ggplot(data = arq) +
  geom_col(mapping = aes(x = DIST
, y = DP)) +
  labs(title = "DP por DIST", x = "Distancia",
       y = "DP'S")

#2.4 DP por ENT.

ggplot(data = arq) +
  geom_point(mapping = aes(x = ENT, y = DP)) +
  labs(title = "DP por ENT", x = "Horas de Entretenimento", y="DP'S")
#2.5 I por ENT.

ggplot(data = arq) +
  geom_col(mapping = aes(x = I, y = DP)) +
  labs(title = "DP por DIST", x = "Idade", y = "DP's") 

#2.6 SEM por SAL.

ggplot(data = arq) +
  geom_boxplot(mapping = aes(x = sal, y = SEM)) +
  labs(title = "SEM por SAL", x = "Salarios", y = "Semestres") 
#T por SAL.

ggplot(data = arq) +
  geom_point(mapping = aes(x = t, y = sal)) +
  labs(title = "T por SAL", x = "Trabalha na �rea?", y = "Sal�rios") 

#3.1 � verdade que as pessoas maiores de idade tendem a ter menos DPs?

ggplot(data = arq) +
  geom_point(mapping = aes(x = I, y = DP)) +
  labs(title = "� verdade que as pessoas maiores de idade tendem a ter menos DPs", x = "Idade", y = "DP's") 
#3.2 � verdade que as pessoas que trabalham na �rea desejada tendem a passar menos tempo nas redes sociais?

ggplot(data = arq) +
  geom_point(mapping = aes(x = t, y = ENT)) +
  labs(title = "� verdade que as pessoas que trabalham na �rea desejada tendem a passar menos tempo nas redes sociais?", x = "Trabalha na �rea?", y = "Horas de Entretenimento") 

#3.3 � verdade que as pessoas que moram mais longe da faculdade tendem a ter mais DPs?

ggplot(data = arq) +
  geom_point(mapping = aes(x = DIST, y = DP)) +
  labs(title = "� verdade que as pessoas que moram mais longe da faculdade tendem a ter mais DPs?", x = "Distancia", y = "DP's")

#3.4 � verdade que as pessoas que trabalham na �rea desejada tendem a ter os maiores sal�rios?

ggplot(data = arq) +
  geom_point(mapping = aes(x = t, y = sal)) +
  labs(title = "� verdade que as pessoas que trabalham na �rea desejada tendem a ter os maiores sal�rios?", x="Trabalha na �rea?", y = "Sal�rios")
