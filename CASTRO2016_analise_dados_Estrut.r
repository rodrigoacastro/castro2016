####################################################################
#
# SCRIPT PARA ANALISE DE DADOS ESTRUTURADOS
#
####################################################################

####### PRELIMINARES

# confere diretorio
getwd()

# confere objetos na memoria 
ls()

# confere arquivos no diretorio
dir()

# limpa memoria
rm(list=ls())

####### 

# Lendo as funcoes necessarias dentro do script

source("CASTRO_2016_funcoes.R")   

# Conferindo objetos na memoria para verificar se funcoes foram carregadas
ls() 

#OK!

##################
# Lendo planilhas na pasta (diretorio) onde eles estao (em formato .csv)
##################

# IMPORTANDO PLANILHA dos questionarios dos pesquisadores (R)

# Selecionar arquivo "RSQ_R_Responses.csv"
# RSQ = Researchers survey questionnaire

planilhaR = read.csv(choose.files(caption="ESCOLHA UM ARQUIVO NO FORMATO CSV"),
                      check.names=F,sep=";")

# copiando nomes das linhas
nomesLinhasR = as.character(planilhaR[,1])

# Eliminando nome do sujeito e timestamp
planR = planilhaR[,-c(1)]

# colocando nomes das linhas
row.names(planR) = nomesLinhasR

# conferindo classes da planilha importada e depois de todas as colunas

str(planR)

lapply(planR,class)


####################################################################

### PREPARANDO DADOS DE FATORES DA PLANILHA

# convertendo colunas de "main attributes of good scientific text (43 a 47)
#  em fatores, pois representam ranks,
# ou seja, categorias em vez de dados numéricos
	
planR[,43:47] = lapply(planR[,43:47],as.factor)


### Criando funcao para rotular fatores de um dataframe 
### com base em uma lista de rotulos (categorias)

rotular_fator = function(coluna,rotulos,niveis=1:length(rotulos)) {

##### Validacao

if (length(coluna) < 2) {
	stop("coluna deve ser uma coluna/variavel de um dataframe")
                        }

if (!is.character(rotulos)) {
	stop("rotulos deve ser um vetor de caracteres")
                            }
                            
if (!is.integer(niveis)) {
	stop("niveis deve ser um vetor de numeros inteiros")
                         }                            
#####

coluna_aux = numeric(length(coluna))
aux = data.frame(rotulos,niveis)

for (i in 1:length(coluna)) {
	coluna_aux[i] = aux[as.character(aux$rotulos)==coluna[i],2]
}

coluna_rotulada = factor(as.numeric(coluna_aux),levels = niveis,labels = rotulos)

return(coluna_rotulada)

}   ### FIM da funcao rotular_fator

################

# COLUNAS 1-16 de dados de proficiencia 

# preparando para transformar todos os dados (ja fatores) para numericos

# selecionando colunas para aplicar funcao rotular_fator

colunas = c(1:16)

# definindo rotulos (nesta ordem)
Labels = c("none","poor","average","good")

# aplicando funcao
for (i in colunas) { planR[,i] = rotular_fator(planR[,i],Labels) }

####

# COLUNAS 31,33,34,36 de habitos de leitura


# selecionando colunas para aplicar funcao rotular_fator

colunas = c(31,33,34,36)

# definindo rotulos (nesta ordem)
Labels2 = c("never","rarely","sometimes","often","very often",
		"always")

# aplicando funcao
for (i in colunas) { planR[,i] = rotular_fator(planR[,i],Labels2) }

####

# COLUNA 51 ("percent of translated/written texts published or in press")


# selecionando colunas para aplicar funcao rotular_fator

colunas = c(51)

# definindo rotulos (nesta ordem)
Labels3 = c("40% or below","40% to 70%","above 70%")

for (i in colunas) { planR[,i] = rotular_fator(planR[,i],Labels3) }

#### Criando matriz com os dados corrigidos

# matriz pronta para testar a funcao
matrizR = data.frame(lapply(planR,as.numeric),check.names=FALSE,
		row.names = nomesLinhasR)

matrizR = as.matrix(matrizR)

##############################################################

### UTILIZANDO ESTATISTICA DESCRITIVA

summary(planR)

# exportando sumarizacao dos dados
#write.csv2(summary(planR),"summary_planR.csv",row.names=F)

# Importando pacote psych

if (!require("psych")) {
	install.packages("psych") 
	require("psych")
} else {
	require("psych")
}

describeBy(planR)

### UTILIZANDO TECNICAS MULTIVARIADAS

# FAZENDO DENDOGRAMA

dendograma(matrizR, N_GRUPOS = 2, DISTANCIA = "euclidean",
		METODO = "ward.D", TITULO = "Agrupamento dos pesquisadores em dois grupos",
		LEGENDA = "Nivel")

# Dendrograma salvo em Arquivo > Salvar como > png


##############################################################

##############################################################

# IMPORTANDO PLANILHA dos questionarios dos tradutores (T)

# Selecionar arquivo "RSQ_T_Responses.csv"
# RSQ = Researchers survey questionnaire

planilhaT = read.csv(choose.files(caption="ESCOLHA UM ARQUIVO NO FORMATO CSV"),
                      check.names=F,sep=";")

# copiando nomes das linhas
nomesLinhasT = as.character(planilhaT[,1])

# Eliminando nome do sujeito e timestamp
planT = planilhaT[,-c(1)]

# colocando nomes das linhas
row.names(planT) = nomesLinhasT

# conferindo classes da planilha importada e depois de todas as colunas

str(planT)

lapply(planT,class)

planT[,9]
#[1] more than 10 2 or fewer   4 to 6       6 to 10     
#Levels: 2 or fewer 4 to 6 6 to 10 more than 10



####################################################################

### PREPARANDO DADOS DE FATORES DA PLANILHA


# convertendo colunas 25 a 30 em fatores, pois representam ranks,
# ou seja, categorias em vez de dados numéricos
	
planT[,25:30] = lapply(planT[,25:30],as.factor)



# COLUNA 9

# selecionando colunas para aplicar funcao rotular_fator
colunas = c(9)

# preparando para transformar todos os dados (ja fatores) para numericos

# definindo rotulos (nesta ordem)
Labels = c("2 or fewer","2 to 4","4 to 6","6 to 10","above 10")

# aplicando funcao
for (i in colunas) { planT[,i] = rotular_fator(planT[,i],Labels) }

# COLUNAS 12,13

# selecionando colunas para aplicar funcao rotular_fator
colunas = c(12,13)

# definindo rotulos (nesta ordem)
Labels2 = c("40% or below","40% to 70%","above 70%")

for (i in colunas) { planT[,i] = rotular_fator(planT[,i],Labels2) }

str(planT)

#### Criando matriz com os dados corrigidos

# matriz pronta para testar a funcao
matrizT = data.frame(lapply(planT,as.numeric),check.names=FALSE,
		row.names = nomesLinhasT)

matrizT = as.matrix(matrizT)


##############################################################

### UTILIZANDO ESTATISTICA DESCRITIVA

summary(planT)

# exportando sumarizacao dos dados
#write.csv2(summary(planT))

# Importando pacote psych

if (!require("psych")) {
	install.packages("psych") 
	require("psych")
} else {
	require("psych")
}

describeBy(planT)

### UTILIZANDO TECNICAS MULTIVARIADAS

# FAZENDO DENDOGRAMA

dendograma(matrizT, N_GRUPOS = 2, DISTANCIA = "euclidean",
		METODO = "ward.D", TITULO = "Agrupamento dos tradutores em dois grupos",
		LEGENDA = "Nivel")

# Dendrogramas salvos em Arquivo > Salvar como > png


##############################################################

### UTILIZANDO TECNICAS MULTIVARIADAS

###	Comparando categorias comuns entre Pesquisadores (R) e Tradutores (T)

###################
# Fonte: https://stackoverflow.com/questions/16529327/compare-column-names-and-make-new-table/16529581#16529581
#c <- rbind(b[, which(colnames(b)%in% colnames(a))],
#    a[, which(colnames(a)%in% colnames(b))])
###################

# Selecionando categorias comuns e fazendo um dataframe
planRT = rbind(planR[, which(colnames(planR)%in% colnames(planT))],
			   planT[, which(colnames(planT)%in% colnames(planR))])

# Acertando o nome das linhas
nomeslinhasRT = row.names(planRT)

# Transformando em dados numericos em uma matriz
matrizRT = sapply(planRT,as.numeric)

# Acertando o nome das linhas
row.names(matrizRT) = nomeslinhasRT 

# Verificando os dados

str(matrizRT)

matrizRT

# Salvando dados em planilhas

#write.csv2(matrizRT,"matrizRT.csv",row.names=F)

# Fazendo Dendogramas

# 2 grupos
dendograma(matrizRT,TITULO="Agrupamento dos pesquisadores e tradutores em dois grupos",N_GRUPOS=2)

# 3 grupos
dendograma(matrizRT,TITULO="Agrupamento dos pesquisadores e tradutores em três grupos",N_GRUPOS=3)

# 4 grupos
dendograma(matrizRT,TITULO="Agrupamento dos pesquisadores e tradutores em quatro grupos",N_GRUPOS=4)

# Dendrogramas salvos em Arquivo > Salvar como > png


# FIM DO SCRIPT DE ANALISE DOS DADOS ESTRUTURADOS
#############################################################################
