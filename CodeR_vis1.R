
# ----------------------------------
# Leitura dos dados da PNADC
# Visita 1 - Ano XXXX 
# ----------------------------------

rm(list = ls())

# Selecione o diretorio do seu trabalho -----

setwd("Colocar o seu diretório aqui - barras invertidas") 
getwd()

# Pacotes ------

  # Usando o pacote PNADcIBGE e' possivel fazer o download da
  # base de dados direto do site do IBGE - demora alguns minutos (~5 min)
  # Instale e chame o pacote
  # Requer conexao com a internet

install.packages("PNADcIBGE")

library(PNADcIBGE)
library(tidyverse)

# Download da base de dados ------

  # criamos um vetor com as variaveis que queremos trazer da PNADC - visita1,
  # porque a base completa tem muitas variaveis que não fazem sentido para as
  # analises propostas

variables<- c("Ano" , "Trimestre", "UF","V1022", "V1023", "V1032",  
            "V2007", "V2008", "V20081", "V20082", "V2009", "V2010", "V3001",
            "V3002", "V4009", "V4018", "V40181", "V40182", "V40183", "V4029",
            "V4032", "V4040", "V40401", "V40402", "V40403", "V4071", 
#            "V5001A",
#            "V5001A2","V5002A","V5002A2","V5003A","V5003A2", "V5004A","V5004A2",
#            "V5005A", "V5005A2","V5006A","V5006A2","V5007A","V5007A2","V5008A",
#            "V5008A2","S01001","S01005","S01006","S01007A","S01011A","S01017",
#            "S01018", "S01019","S01021","S01029","S01031",
"VD2002","VD2003",
            "VD2004","VD3004","VD3005","VD4001","VD4002","VD4003","VD4009",
            "VD4010", "VD4011","VD4012","VD4020","VD4035")

# Variáveis disponíveis para análise:
# V1022 - Situação do domicílio - rural/urbano
# V1023 - Tipo de área - capital/região metropolitana/etc
# V1032 - Peso do domicílio e das pessoas
# V2007 - Sexo - masc/fem
# V2008 - Dia do nascimento
# V20081 - Mês de nascimento
# V20082 - Ano de nascimento
# V2009 - Idade do morador na data de referência
# V2010 - Cor ou raça - Branca/Preta/Amarela/Parda/Indígena/Ignorado
# V3001 - Sabe ler e escrever? 
# 






# V4001 - 



# Onde esta XXXX no comando abaixo colocar o ano 
pnadcvis1 <- get_pnadc(2014,
                       interview = 1,
                       deflator = TRUE,
                       labels = TRUE,
                       vars=variables,
                       design=FALSE)

# Mesmo escolhendo as variaveis, o pacote traz algumas que nao iremos usar
# entao fazemos uma selecao para obter uma base com 67 variaveis


# `V5001A`, `V5001A2`, `V5002A`, `V5002A2`, `V5003A`,
# V5005A`, `V5005A2`, `V5006A`, `V5006A2`, `V5007A`,

pnadc <- pnadcvis1  %>% select(all_of(variables))

# rm(pnadcvis1) # remove o objeto anterior

# Essa e' a base que voce deve utilizar no seu trabalho

# Salvar, ler e detalhes da base ----

save(pnadc,file = "base.RData")
rm(list = ls())

# Acessando a base que foi salva 

load("base.RData") 

# Consultando detalhes sobre a base e variaveis que ela possui

class(pnadc)
str(pnadc)
glimpse(pnadc)

# A partir daqui e' com você
# Bom trabalho !

# Importando a base de dados com as informações sobre a análise a ser realizada
obj <- readxl::read_xlsx("Analises_alunos_turma1.xlsx")
names(obj) <- obj[3,]
obj <- obj[4:nrow(obj),]

# Obtendo as minhas informações a partir da consulta do número USP
infos <- obj %>% 
  filter(`Número USP` == '11765067') %>% 
  view()

# Selecionando as variáveis a serem usadas a partir do diciionário
library(readxl)
dict <- readxl::read_xls("Dicionario_PNADC_microdados_2019_visita1_20230811.xls") %>% 
  select(...3, ...5)
names(dict) <- c("codigo", "descricao")
dict <- dict %>% 
  filter(!is.na(codigo) & !is.na(descricao)) %>% 
  filter(codigo %in% variables) %>% 
  view()

# Consolidando a base de dados com os códigos transformados em descrição
pnadcvis <- pnadcvis1 %>% 
  select(dict$codigo)
names(pnadcvis) <- dict$descricao

# Removendo os objetos desnecessários
rm(dict, obj, pnadcvis2)


# Selecionando a amostra por idade (18-65):
pnadcvis <- pnadcvis %>% 
  mutate(`Idade do morador na data de referência` = as.numeric(`Idade do morador na data de referência`)) %>% 
  filter(`Idade do morador na data de referência` > 17 & `Idade do morador na data de referência` < 66) 


colnames(pnadcvis)

# Regressão simples: Condição de ocupação na semana de 
pnadcvis %>% 
  count(`Condição de ocupação na semana de referência para pessoas de 14 anos ou mais de idade`)

pnadcvis %>% 
  lm(`Condição de ocupação na semana de referência para pessoas de 14 anos ou mais de idade` ~ )


infos %>% 
  view()


pnadcvis1 %>% 
  select()












