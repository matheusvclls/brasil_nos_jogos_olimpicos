# File:    analise_brasil_olimpiadas.R
# Author:  Matheus Vasconcellos, matheusvclls97@gmail.com
# Date:    2021-08-01



############################################################
# BRASIL NOS JOGOS OLÍMPICOS ###############################
############################################################




# Índex ####################################################
# I. Objetivo do projeto
# II. Bibliotecas
# III. Análise dos datasets
# IV. Tratamento dos dados
# V. Análise do Brasil nas Olimpíadas




# I. Objetivo do projeto #####################################
# O objetivo do projeto é analisar o histórico do desempenho brasileiro nos Jogos Olímpicos.
# Para isso, é utilizado o dataset do Kaggle, disponível no seguinte endereço
# "https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results", que tem
# como fonte inicial o site "www.sports-reference.com".
# Esta base de dados contém informações desde os jogos de Atenas 1986 até o Rio 2016.
# A análise dos dados seguirão as seguintes perguntas, que tentaremos responder ao longo do
# projeto:
# 1. Participação do Brasil nas Olimpíadas
# 2. Desempenho do Brasil nas Olimpíadas




# II. Bibliotecas #####################################
library(ggplot2)
library(tidyverse)



# III. Análise dos datasets ###########################
# No total são dois datasets:

# -  athlete_events.csv: 
# Cada linha corresponde a um atleta individual competindo em um evento olímpico individual (eventos-atleta)
# ID - número único para cada atleta
# Name - nome do atleta
# Sex - M ou F
# Age - idade do atleta
# Height - altura em centimetros
# Weight - peso em kilogramas
# Team - país do atleta
# NOC - National Olympic Committee - abreviação do nome do país em 3 letras
# Games - Ano e tipo (inverno ou verão)
# Year - Ano da realização dos jogos
# Season - Verão ou Inverno
# City - Cidade sede dos jogos
# Sport - Esporte
# Event - Modalidade do esporte
# Medal - medalha do atleta

# -  noc_regions.csv: 
# NOC - National Olympic Committee - abreviação do nome do país em 3 letras
# region - país do atleta
# notes - observação sobre este país


# Dataset athlete_events.csv
# Carregando o dataset
atletas_dados <- read_csv(".\\dados\\athlete_events.csv")

head(atletas_dados)

dim(atletas_dados)
# O dataset contém 271116 linhas e 15 colunas

summary(atletas_dados)

# Verificar os valores nulos
colSums(is.na(atletas_dados))
# As únicas colunas que apresentam valores nulos são Age Height Weight e Medal.
# Na coluna "Medal" é normal ter valores nulos por conta do atleta não ter conquistado uma medalha
# No entanto, na coluna idade, altura e peso é devido a falta de dado, pois, obviamente todo
# atleta tem esses atributos. No entanto, como temos problemas a serem respondidos que não são
# influenciados por essas variáveis, elas permanecerão no dataset.

# Verificar o tipo das colunas do dataframe
str(atletas_dados)
# Os tipos estão como esperados, variáveis numéricas estão como numéricas e de textos estão
# como string.

# Verificar se existe valores duplicados (verificar esta parte se está certa!!!)
ind <- duplicated(atletas_dados[,c('ID', 'Games', 'Event')],)
i <- atletas_dados[ind,]
# Estudar como remover dados duplicados


# Dataset noc_regions.csv
# Carregando o dataset
regiao_dados <- read_csv(".\\dados\\noc_regions.csv")

head(regiao_dados)

dim(regiao_dados)
# O dataset contém 230 linhas e 3 colunas

# Verificar os valores nulos
colSums(is.na(regiao_dados))
# O dataframe contém três valores nulos na coluna region, que iremos analisar a 
# seguir. Além desta, a coluna notes também conta com valores nulos, no entanto,
# esta não é um problema.

# display valores missings
regiao_dados[!complete.cases(regiao_dados$region),]




# IV. Tratamento dos dados ############################
# 1. Valores nulos
regiao_dados$region <- ifelse(is.na(regiao_dados$region), regiao_dados$notes, regiao_dados$region)
regiao_dados[!complete.cases(regiao_dados$region),] #check do resultado (valores nulos da coluna "region")
# A função acima preencheu todos os valores nulos da coluna "region" com os valores das "notes"

# 2. Criar dataframe por medalhas de modalidades
n_medalahas_modalidades <- atletas_dados %>%
  group_by(NOC,Games ,Sport , Event ,Medal,Year) %>%
  summarize(n = n())

# 3. Filtro apenas do Brasil
n_medalahas_modalidades_brasil <- n_medalahas_modalidades[n_medalahas_modalidades$NOC == 'BRA',]




# V. Análise do Brasil nas Olimpíadas ############################
# Total de medalhas
total_medalhas <- sum(complete.cases(n_medalahas_modalidades_brasil$Medal))

# Total de modalidades nos Jogos Olímpicos
participacao_atletas <- atletas_dados %>%filter(Season == 'Summer')%>%
  group_by(Year) %>%
  summarize(num_atletas =  length(unique(Event)))

ggplot(participacao_atletas, aes(x=Year, y=num_atletas)) +
  geom_point() +
  labs(title="Total de modalidades nos Jogos Olímpicos", y="Total de atletas", x = 'Ano') +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Ano") +  #remover a legenda do eixo x
  ylab('Quantidade de modalidades') +  #alterar a legenda do eixo y
  geom_line()
ggsave(".\\graficos\\evolucao_modalidades.png")


# Evolução do total de participantes por gênero
participacao_atletas <- atletas_dados %>%filter(Season == 'Summer')%>%
  group_by(Year, Sex) %>%
  summarize(num_atletas =  length(unique(ID)))

ggplot(participacao_atletas, aes(x=Year, y=num_atletas, group=Sex, color=Sex)) +
  geom_point() +
  labs(title="Evolução do total de participantes por gênero", y="Total de atletas", x = 'Ano') +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line()

ggsave(".\\graficos\\evolucao_participacao_mundo_genero.png")



# Evolução do total de participantes
participacao_atletas <- atletas_dados %>% filter(Season == 'Summer')%>%
  group_by(Year) %>%
  summarize(num_atletas =  length(unique(ID)))

ggplot(participacao_atletas, aes(x=Year, y=num_atletas)) +
  geom_point() +
  scale_y_continuous() +
  labs(title="Evolução do total de participantes", y="Total de atletas", x = 'Ano') +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line()
ggsave(".\\graficos\\evolucao_participacao_mundo.png")



# Evolução da participação brasileira nos jogos por sexo
participacao_atletas <- atletas_dados %>% filter(NOC == 'BRA')%>%filter(Season == 'Summer')%>%
  group_by(Year, Sex) %>%
  summarize(num_atletas =  length(unique(ID)))

ggplot(participacao_atletas, aes(x=Year, y=num_atletas, group=Sex, color=Sex)) +
  geom_point() +
  scale_y_continuous(limits=c(0,270)) +
  labs(title="Evolução da participação brasileira nos jogos por sexo", y="Total de atletas", x = 'Ano') +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line()

ggsave(".\\graficos\\evolucao_participacao_brasil_genero.png")


# Evolução da participação brasileira nos jogos
participacao_atletas <- atletas_dados %>% filter(NOC == 'BRA')%>%filter(Season == 'Summer')%>%
  group_by(Year) %>%
  summarize(num_atletas =  length(unique(ID)))

ggplot(participacao_atletas, aes(x=Year, y=num_atletas)) +
  geom_point() +
  scale_y_continuous(limits=c(0,270)) +
  labs(title="Evolução da participação brasileira nos jogos", y="Total de atletas", x = 'Ano') +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line()
ggsave(".\\graficos\\evolucao_participacao_brasil.png")


#### Total de medalhas do Brasil
# Consolidar a base de dados para o gráfico
contagem_medalhas <- n_medalahas_modalidades_brasil %>% filter(!is.na(Medal))%>%  #filtrar todas as medalhas que são diferentes de valores nulos
  group_by(Medal) %>%  #agrupar por tipo de medalhas (ouro, prata e bronze) 
  summarize(Count=length(Medal))  #sumarizar pela quantidade de medalhas

# Ordenar o tipo de medalhas
contagem_medalhas$Medal <- factor(contagem_medalhas$Medal,  #label a ser ordenada
                                  levels=c( "Bronze", "Silver","Gold"),  #ordernar
                                  labels=c("Bronze", "Prata", "Ouro")  #renomear as labels
                                  )  

# Construção do gráfico
ggplot(medalha_counts, aes( y=Count, fill=Medal, x ='', label = Count)) +
  geom_col( width = 0.35) + 
  geom_text(aes(label = Count), position = position_stack(vjust = .85)) + 
  coord_flip() +  #orientar o gráfico na horizontal
  scale_fill_manual(values=c( "#cd7f32","#C0C0C0","#d4af37")) +  #definir as cores das colunas
  ggtitle("Total de medalhas do Brasil") +  #definir o título
  xlab("") +  #remover a legenda do eixo x
  ylab('Contagem de medalhas') +  #alterar a legenda do eixo y
  theme(legend.position = "none") +  #remover a legenda
  theme(plot.title = element_text(hjust = 0.5)) 

# Salvar o gráfico em ".png"
ggsave(".\\graficos\\desempenho_brasil.png")


#Distribuição de medalhas por esporte
medalha_counts <- n_medalahas_modalidades_brasil %>% filter(!is.na(Medal))%>%
  group_by(Sport, Medal) %>% 
  summarize(Count=length(Medal))

medalha_counts$Medal <- factor(medalha_counts$Medal, levels=c( "Bronze", "Silver","Gold"), labels=c("Bronze", "Prata", "Ouro"))

lev <- medalha_counts %>%
  group_by(Sport) %>%
  summarize(Total=sum(Count)) %>%
  arrange(Total) %>%
  select(Sport)
medalha_counts$Sport <- factor(medalha_counts$Sport, levels=lev$Sport)

ggplot(medalha_counts, aes(x=Sport, y=Count, fill=Medal, label = Count)) +
  geom_col() +
  geom_text(aes(label = Count), position = position_stack(vjust = .95)) + 
  coord_flip() +
  scale_fill_manual(values=c( "#cd7f32", "#C0C0C0", "#d4af37")) +
  ggtitle("Distribuição de medalhas por esporte") +
  xlab("") +
  ylab('Contagem de medalhas') +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(".\\graficos\\desempenho_brasil_por_esporte.png")


# Total de medalhas por edição
medalha_counts <- atletas_dados %>% filter(!is.na(Medal))%>%filter(Season == 'Summer')%>%
  group_by(Games, Medal) %>% 
  summarize(Count=length(Medal))

medalha_counts$Medal <- factor(medalha_counts$Medal, levels=c( "Bronze", "Silver","Gold"))

ggplot(medalha_counts, aes(x=Games, y=Count, fill=Medal, label = Count)) +
  geom_col() +
  geom_text(aes(label = Count), position = position_stack(vjust = .95)) + 
  coord_flip() +
  scale_fill_manual(values=c( "#cd7f32", "#C0C0C0", "#d4af37")) +
  ggtitle("Total de medalhas por edição") +
  xlab("") +
  ylab('Contagem de medalhas') +
  theme(plot.title = element_text(hjust = 0.5)) 



# Total de medalhas brasileiras por edição
medalha_counts <- n_medalahas_modalidades_brasil %>% filter(!is.na(Medal))%>%
  group_by(Year, Medal) %>% 
  summarize(Count=length(Medal))

medalha_counts$Medal <- factor(medalha_counts$Medal, levels=c( "Bronze", "Silver","Gold"))

ggplot(medalha_counts, aes(x=Year, y=Count, fill=Medal, label = Count)) +
  geom_col() +
  geom_text(aes(label = Count), position = position_stack(vjust = .95)) + 
  coord_flip() +
  scale_fill_manual(values=c( "#cd7f32", "#C0C0C0", "#d4af37")) +
  ggtitle("Total de medalhas por edição") +
  xlab("") +
  ylab('Contagem de medalhas') +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5)) 
ggsave(".\\graficos\\desempenho_brasil_por_edicao.png")