####################################################################
#
# SCRIPT PARA ANALISE DE DADOS NAO ESTRUTURADOS
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


## SEPARANDO ARQUIVOS DOS TRADUTORES E PESQUISADORES

# TRADUTORES: pasta "Transcricoes_protocolos_TRAD_partic_apenas"
# PESQUISADORES: pasta "Transcricoes_protocolos_PESQ_partic_apenas"

# TRADUTORES


# LENDO textos na pasta (diretorio) onde eles estao
# usando a funcao le_txt_pasta
# a partir da pasta "Transcricoes_protocolos_TRAD_partic_apenas

dados_TRAD_sem_perg = le_txt_pasta (CODIFICACAO = "UTF-8")


# conferindo objeto
str(dados_TRAD_sem_perg)


# PESQUISADORES


# LENDO textos na pasta (diretorio) onde eles estao
# usando a funcao le_txt_pasta
# a partir da pasta "Transcricoes_protocolos_PESQ_partic_apenas

dados_PESQ_sem_perg = le_txt_pasta (CODIFICACAO = "UTF-8")

# conferindo objeto
str(dados_PESQ_sem_perg)


##### REMOVENDO ITENS entre parenteses angulares (< e >)
          
# importando e carregando o pacote stringr          
if (!require ("stringr")) {
	install.packages("stringr")
	require ("stringr")
} else { 
	require("stringr")
}


# TRADUTORES

##### removendo itens entre parenteses angulares (< e >)
dados_TRAD = lapply(dados_TRAD_sem_perg,FUN = removeMarcador)
#edit(dados_TRAD_sem_perg)


# unindo linhas de texto em apenas uma lista
dados_TRAD = unlist(dados_TRAD)

#retirar "<U+FEFF>" que restou nos vetores de caracteres
dados_TRAD = str_replace_all(dados_TRAD,"<U+FEFF>","")

#retirar "ufeff" que restou nos vetores de caracteres
dados_TRAD = str_replace_all(dados_TRAD,"ufeff","")

# conferindo
#grep("<U+FEFF>",dados_TRAD)
#grep("ufeff",dados_TRAD)



# PESQUISADORES

##### Removendo itens entre parenteses angulares (< e >)
dados_PESQ = lapply(dados_PESQ_sem_perg,FUN = removeMarcador)
#edit(dados_PESQ_sem_perg)

# unindo linhas de texto em apenas uma lista
dados_PESQ = unlist(dados_PESQ)

# retirar "<U+FEFF>" que restou nos vetores de caracteres
dados_PESQ = str_replace_all(dados_PESQ,"<U+FEFF>"," ")

#retirar "ufeff" que restou nos vetores de caracteres
dados_PESQ = str_replace_all(dados_PESQ,"ufeff","")

# conferindo
#grep("<U+FEFF>",dados_PESQ)
#grep("ufeff",dados_PESQ)


str(unlist(dados_PESQ))

### PREPARANDO o texto PARA BUSCAR "a gente"

# TRADUTORES


dados_TRAD = prepara_palavra_composta(dados_TRAD,"a gente")

# mostrando ocorrencias    
#dados_TRAD[grep("a-gente",dados_TRAD)]

# DADOS PARA CONCORD
dados_TRAD_concord = dados_TRAD

#############	SALVANDO ARQUIVOS ATÉ O MOMENTO
write.table(dados_TRAD,"dados_TRAD_partic.txt",sep="\t",row.names=FALSE,col.names=FALSE)

###	RE-IMPORTANDO ARQUIVOS NOVAMENTE JÁ DIVIDINDO EM STRINGS PARA OBTER ALGUNS RESULTADOS

dados_TRAD = scan("dados_TRAD_partic.txt",what="char",quote="",comment.char="")


# PESQUISADORES


dados_PESQ = prepara_palavra_composta(dados_PESQ,"a gente")
    
# mostrando ocorrencias
#dados_PESQ[grep("a-gente",dados_PESQ)]

#################		Unindo textos num so arquivo
dados_PESQ = unlist(dados_PESQ)
str(dados_PESQ) 

# DADOS PARA CONCORD
dados_PESQ_concord = dados_PESQ


#############	SALVANDO ARQUIVOS ATE O MOMENTO
write.table(dados_PESQ,"dados_PESQ_partic.txt",sep="\t",row.names=FALSE,col.names=FALSE)

###	RE-IMPORTANDO ARQUIVOS NOVAMENTE JA DIVIDINDO EM STRINGS PARA OBTER ALGUNS RESULTADOS

dados_PESQ = scan("dados_PESQ_partic.txt",what="char",quote="",comment.char="")

######

#########	Aplicando Funcao de Limpeza - PARTE DEMORADA SE O TEXTO FOR GRANDE
     
# importando e carregando o pacote stringr          
if (!require ("stringr")) {
	install.packages("stringr")
	require ("stringr")
} else { 
	require("stringr")
}

# TRADUTORES

# Fazendo a limpeza
dados_TRAD = lapply(dados_TRAD,limpeza)

## Transformando de lista para vetor para eliminar espacos em branco
dados_TRAD = unlist(dados_TRAD)

#retirar "<U+FEFF>" que restou nos vetores de caracteres
dados_TRAD = str_replace_all(dados_TRAD,"<U+FEFF>","")

#retirar "ufeff" que restou nos vetores de caracteres
dados_TRAD = str_replace_all(dados_TRAD,"ufeff","")

# conferindo
#grep("<U+FEFF>",dados_TRAD)
#grep("ufeff",dados_TRAD)

## Retirando espacos em branco
dados_TRAD = dados_TRAD[dados_TRAD!=""]


# PESQUISADORES

# Fazendo a limpeza
dados_PESQ = lapply(dados_PESQ,limpeza)

## Transformando de lista para vetor para eliminar espacos em branco
dados_PESQ = unlist(dados_PESQ)


# retirar "<U+FEFF>" que restou nos vetores de caracteres
dados_PESQ = str_replace_all(dados_PESQ,"<U+FEFF>","")

#retirar "ufeff" que restou nos vetores de caracteres
dados_PESQ = str_replace_all(dados_PESQ,"ufeff","")

# conferindo
#grep("<U+FEFF>",dados_PESQ)
#grep("ufeff",dados_PESQ)

## Retirando espacos em branco
dados_PESQ = dados_PESQ[dados_PESQ!=""]

################

###  Lista de Frequencias

# GERAL

dados_GERAL = c(dados_TRAD,dados_PESQ)

lista_FREQ_GERAL = lista_frequencia(dados_GERAL, ALFAB = FALSE)

lista_FREQ_GERAL

head(lista_FREQ_GERAL,20)


# exportando lista_freq
#write.csv2(lista_FREQ_GERAL,file= "lista_FREQ_GERAL.csv",row.names=F)


# TRADUTORES

lista_FREQ_TRAD = lista_frequencia(dados_TRAD, ALFAB = FALSE)
lista_FREQ_TRAD

# exportando lista_freq
write.csv2(lista_FREQ_TRAD,file= "lista_freq_TRAD.csv",row.names=F)


type1 = nrow(lista_FREQ_TRAD)   # numero de palavras diferentes
type1

token1 = sum(lista_FREQ_TRAD[,2])  # numero total de palavras
token1

TypeTokenRatio1 = type1/token1   # Medida de densidade lexical (riqueza de vocabulario)
TypeTokenRatio1

# PESQUISADORES


lista_FREQ_PESQ = lista_frequencia(dados_PESQ, ALFAB = FALSE)
lista_FREQ_PESQ

# exportando lista_freq
write.csv2(lista_FREQ_PESQ,file= "lista_freq_PESQ.csv",row.names=F)


type2 = nrow(lista_FREQ_PESQ)   # numero de palavras diferentes
type2

token2 = sum(lista_FREQ_PESQ[,2])  # numero total de palavras
token2

TypeTokenRatio2 = type2/token2   # Medida de densidade lexical (riqueza de vocabulario)
TypeTokenRatio2



################

## Nuvem de palavras GERAL

# TRADUTORES


# lista de frequencia com stopwords Default

lista_FREQ_TRAD_NP = nuvem_palavras(dados_TRAD, ALFAB = FALSE,
				                     STOPWORDS_default = TRUE)
# exportando lista_FREQ_TRAD_NP
write.csv2(lista_FREQ_TRAD_NP,file= "lista_freq_TRAD_stopwords_padrao.csv",row.names=F)

# lista de frequencia com stopwords personalizadas
									 
lista_FREQ_TRAD_NP_Pers = nuvem_palavras(dados_TRAD, ALFAB = FALSE,
				                      STOPWORDS_default = F)


# exportando lista_FREQ_TRAD_NP_Pers
write.csv2(lista_FREQ_TRAD_NP_Pers,file= "lista_freq_TRAD_stopwords_personal.csv",row.names=F)



# PESQUISADORES


# lista de frequencia com stopwords Default

lista_FREQ_PESQ_NP = nuvem_palavras(dados_PESQ, ALFAB = FALSE,
				                     STOPWORDS_default = TRUE)
# exportando lista_FREQ_PESQ_NP
write.csv2(lista_FREQ_PESQ_NP,file= "lista_freq_PESQ_stopwords_padrao.csv",row.names=F)

# lista de frequencia com stopwords personalizadas
									 
lista_FREQ_PESQ_NP_Pers = nuvem_palavras(dados_PESQ, ALFAB = FALSE,
				                      STOPWORDS_default = F)

# exportando lista_FREQ_PESQ_NP_Pers
write.csv2(lista_FREQ_PESQ_NP_Pers,file= "lista_freq_PESQ_stopwords_personal.csv",row.names=F)


# PREPARACAO PARA NUVEM DE PALAVRAS TOP 20 E TOP 20 MAIS RELEVANTES

##
#	NUVEM DE PALAVRAS top 20 palavras da lista de frequencia
##


# TRADUTORES

## Nuvem de palavras top20, com foco no "eu" e no "a gente"
## e nos verbos de primeira e terceira conjugacao

# ler "lista_freq_TRAD_top20.csv"
lista_FREQ_TRAD_top20 = read.csv(choose.files(),sep=";")

lista_FREQ_TRAD_top20

str(lista_FREQ_TRAD_top20)

# PESQUISADORES


## Nuvem de palavras top20, com foco no "eu" e no "a gente"
## e nos verbos de primeira e terceira conjugacao

# ler "lista_freq_PESQ_top20.csv"
lista_FREQ_PESQ_top20 = read.csv(choose.files(),sep=";")

lista_FREQ_PESQ_top20

str(lista_FREQ_PESQ_top20)


##
#	NUVEM DE PALAVRAS top 20 palavras mais relevantes da lista de frequencia
##


# TRADUTORES

## Nuvem de palavras top20 mais frequentes, com foco no "eu" e no "a gente"
## e nos verbos de primeira e terceira conjugacao

# ler "lista_freq_TRAD_top20_Relev.csv"
lista_FREQ_TRAD_top20_rel = read.csv(choose.files(),sep=";")

lista_FREQ_TRAD_top20_rel

str(lista_FREQ_TRAD_top20_rel)

# PESQUISADORES


## Nuvem de palavras top20, com foco no "eu" e no "a gente"
## e nos verbos de primeira e terceira conjugacao

# ler "lista_freq_PESQ_top20_Relev.csv"
lista_FREQ_PESQ_top20_rel = read.csv(choose.files(),sep=";")

lista_FREQ_PESQ_top20_rel

str(lista_FREQ_PESQ_top20_rel)


##########################


# Usando funcao para fazer nuvem de palavras dos 20 termos mais relevantes

# TRADUTORES


texto_nuvem_TRAD = lista_freq_para_texto_nuvem(lista_FREQ_TRAD_top20)

# acertando ortografia de "a gente" no texto
texto_nuvem_TRAD = gsub("agente","a-gente",texto_nuvem_TRAD)

nuvem_palavras(texto_nuvem_TRAD)
# Salva como .png no "Salvar como" do R


# PESQUISADORES


texto_nuvem_PESQ = lista_freq_para_texto_nuvem(lista_FREQ_PESQ_top20_rel)

# acertando ortografia de "a gente" no texto
texto_nuvem_PESQ = gsub("agente","a-gente",texto_nuvem_PESQ)

nuvem_palavras(texto_nuvem_PESQ)
# Salva como .png no "Salvar como" do R


#########	GERANDO UMA FIGURA COM AS 2 NUVENS JUNTAS

# configurando janela para mostrar as 2 nuvens juntas
par(mfrow=c(1,2))

# plotando a nuvem dos pesquisadores
nuvem_palavras(texto_nuvem_PESQ,MIN=2,TITULO="Grupo dos pesquisadores")

# plotando a nuvem dos tradutores
nuvem_palavras(texto_nuvem_TRAD,MIN=2,TITULO="Grupo dos pesquisadores")

# voltando para a configuracao default da janela
par(mfrow=c(1,1))


##########################

# NUVEM DE PALAVRAS DOS PROCESSOS MENTAIS COMUNS

# TRADUTORES


# ler "Proc_mentais_Trad.csv"
lista_FREQ_Mentais_TRAD = read.csv(choose.files(),sep=";")

lista_FREQ_Mentais_TRAD

str(lista_FREQ_Mentais_TRAD)

# transformando lista de frequencia em texto para nuvem
texto_nuvem2_TRAD = lista_freq_para_texto_nuvem(lista_FREQ_Mentais_TRAD)

# gerando nuvem com termos de frequencia minima 1
nuvem_palavras(texto_nuvem2_TRAD,MIN=1)

# Salva como .png no "Salvar como" do R
# SALVA COMO nuvem_proc_ment_comuns_TRAD.png

# gerando nuvem com termos de frequencia minima 2

nuvem_palavras(texto_nuvem2_TRAD,MIN=2)

# Salva como .png no "Salvar como" do R
# SALVA COMO nuvem_proc_ment_comuns_TRAD_MIN2.png


# PESQUISADORES


# ler "Proc_mentais_Pesq.csv"
lista_FREQ_Mentais_PESQ = read.csv(choose.files(),sep=";")

lista_FREQ_Mentais_PESQ

str(lista_FREQ_Mentais_PESQ)


# transformando lista de frequencia em texto para nuvem
texto_nuvem2_PESQ = lista_freq_para_texto_nuvem(lista_FREQ_Mentais_PESQ)

# gerando nuvem com termos de frequencia minima 2

nuvem_palavras(texto_nuvem2_PESQ,MIN=1)

# Salva como .png no "Salvar como" do R
# SALVA COMO nuvem_proc_ment_comuns_PESQ.png	

# gerando nuvem com termos de frequencia minima 2

nuvem_palavras(texto_nuvem2_PESQ,MIN=2)

# Salva como .png no "Salvar como" do R
# SALVA COMO nuvem_proc_ment_comuns_PESQ_MIN2.png	


#########	GERANDO UMA FIGURA COM AS 2 NUVENS JUNTAS

## CONSIDERANDO FREQUENCIA MINIMA 1


# configurando janela para mostrar as 2 nuvens juntas
par(mfrow=c(1,2))

# plotando a nuvem dos pesquisadores
nuvem_palavras(texto_nuvem2_PESQ,MIN=1,TITULO="Grupo dos pesquisadores")

# plotando a nuvem dos tradutores
nuvem_palavras(texto_nuvem2_TRAD,MIN=1,TITULO="Grupo dos tradutores")

## IMAGEM SALVA NO SALVAR COMO com o nome de "nuvens_min1_v2.png"

# voltando para a configuracao default da janela
par(mfrow=c(1,1))



## CONSIDERANDO FREQUENCIA MINIMA 2

# configurando janela para mostrar as 2 nuvens juntas
par(mfrow=c(1,2))

# plotando a nuvem dos pesquisadores
nuvem_palavras(texto_nuvem2_PESQ,MIN=2,TITULO="Grupo dos pesquisadores")

# plotando a nuvem dos tradutores
nuvem_palavras(texto_nuvem2_TRAD,MIN=2,TITULO="Grupo dos tradutores")

## IMAGEM SALVA NO SALVAR COMO com o nome de "nuvens_min2_v2.png"

# voltando para a configuracao default da janela
par(mfrow=c(1,1))

####################################################

# FAZENDO CONCORDANCIA DOS PROCESSOS MENTAIS DE CADA GRUPO
# TRADUTORES E PESQUISADORES


#######################

# gerando listas de processos materiais de cada grupo
# retirados das listas de frequencia
# para filtrar colocados do "que"

# TRADUTORES

# ler lista no arquivo txt
# lista_proc_mentais_corpus_TRAD = scan(choose.files())

lista_proc_mentais_corpus_TRAD = c("achar", "achei", "acredito", "conhecia",
									"descobri","esquecido", "gostou", "lembrando",
									"lembrar", "lembrava", "olhei", "optei",
									"preferem", "preferi", "prefiro", "preocupa",
									"quer", "quiser", "resolvo", "sabe", "sabia", "vi"
									)

lista_proc_mentais_corpus_TRAD




# PESQUISADORES
									

# ler lista no arquivo txt
# lista_proc_mentais_corpus_PESQ = scan(choose.files())

lista_proc_mentais_corpus_PESQ = c("acha", "achar", "achava", "achei", "acho", "achou", "arrependi",
									"avaliado", "avaliar", "conhece", "conhecia", "conhecido", "conheço",
									"considerar", "considerou", "entender", "entendeu", "entendi", "entendido",
									"enxergar", "escolhi", "espera", "esperar", "esperava", "esperei",
									"esquecendo", "esqueci", "gostei", "gosto", "lembrando", "lembrar",
									"lembre", "lembrei", "olhando", "olhar", "olhei", "optei", "pensando", "pensar",
									"preciso", "preocupar", "querendo", "querer", "queria", "quero", "quis", "quiser",
									"quisesse", "raciocinar", "resolver", "resolvi", "sabemos", "sabendo",
									"saber", "sabia", "sei", "senti", "vendo"
									)

lista_proc_mentais_corpus_PESQ


######################

# Gerando linhas de concordancia dos processos 
                
                                           
## Linhas de CONCORDANCIA de "que"

# TRADUTORES

concord_que_TRAD = concordancia(dados_TRAD_concord,"que")

# lista de processos mentais usados pelos tradutores
lista_proc_mentais_corpus_TRAD


# lista de processos em comum pros tradutores
lista_proc_comum_TRAD = intersect(concord_que_TRAD[[2]][,"A5"],
						lista_proc_mentais_corpus_TRAD
					   )

lista_proc_comum_TRAD

# exportando dados

#importando e carregando pacote stringr
     
#if (!require ("xlsx")) { install.packages("xlsx"); require ("xlsx")} else { require("xlsx")}

# exportando dados
#write.xlsx(lista_proc_comum_PESQ,"lista_proc_comum_PESQ.xlsx")


# PESQUISADORES

concord_que_PESQ = concordancia(dados_PESQ_concord,"que")

# lista de processos mentais usados pelos pesquisadores
lista_proc_mentais_corpus_PESQ

# lista de processos em comum pros pesquisadores
lista_proc_comum_PESQ = intersect(concord_que_PESQ[[2]][,"A5"],
						lista_proc_mentais_corpus_PESQ
					   )

lista_proc_comum_PESQ

# exportando dados

#importando e carregando pacote xlsx

#if (!require ("xlsx")) { install.packages("xlsx"); require ("xlsx")} else { require("xlsx")}

# exportando dados
#write.xlsx(lista_proc_comum_PESQ,"lista_proc_comum_PESQ.xlsx")

#############

# EXEMPLOS DE CONCORD DO "ACHO" E "SABIA"

# CONCORDANCIA "ACHO"

# buscando linhas
#pos_acho = grep("acho",concord_que_PESQ[[2]][,"A5"])

# gerando concordancia
#conc_acho = concord_que_PESQ[[2]][pos_acho,]

#conc_acho

# CONCORDANCIA "SABIA"

# buscando linhas
#pos_sabia = grep("sabia",concord_que_PESQ[[2]][,"A5"])

# gerando concordancia
#conc_sabia = concord_que_PESQ[[2]][pos_sabia,]

#conc_sabia

#############

# UNINDO linhas PARA GERAR CONCORDANCIA

# PESQUISADORES

# gerando matriz vazia para preencher
linhas_processos_com_PESQ = matrix(nrow=1,ncol=11)

# loop para buscar linhas de ocorrencia e gerar linhas de concordancia
for (processos in seq_along(1:length(lista_proc_comum_PESQ))) {
	
	# buscando linhas
	pos_temp = grep(lista_proc_comum_PESQ[processos],concord_que_PESQ[[2]][,"A5"])

	# gerando concordancia
	linhas_processos_com_PESQ = rbind(linhas_processos_com_PESQ,concord_que_PESQ[[2]][pos_temp,])

}

# eliminando linhas de NAs
linhas_processos_com_PESQ = linhas_processos_com_PESQ[-1,]

#visualizando dados

linhas_processos_com_PESQ

#edit(linhas_processos_com_PESQ)

# Exportar linhas pro excel

#importando e carregando pacote xlsx
#if (!require ("xlsx")) { install.packages("xlsx"); require ("xlsx")} else { require("xlsx")}
	
# exportando dados
#write.xlsx(linhas_processos_com_PESQ,"linhas_processos_com_PESQ.xlsx",row.names=T)

# IGNORAR LINHA DO CONHECE (nao relevante porque nao e projecao)


# TIRANDO AMOSTRAS PARA GERAR TABELA


# gerando matriz vazia provisoria a ser preenchida pela concordancia
amostras_processos_PESQ = matrix(nrow=1,ncol=11)

# acertando nomes de colunas
colnames(amostras_processos_PESQ) = colnames(concord_que_PESQ[[2]])


# Loop para preencher linhas e gerar amostras

for (processos in seq_along(1:length(lista_proc_comum_PESQ))) {
	
	# gerando palavra de busca usando expressao regular pra evitar erros
	pal_busca = paste(lista_proc_comum_PESQ[processos],"$",sep="")

	# buscando linhas
	pos_temp = grep(pal_busca,concord_que_PESQ[[2]][,"A5"])
	
	# gerando semente (seed) para amostra
	set.seed(processos)
	
	# gerando amostra de 1 item dentre os indices
	# para amostra de tamanho 1, necessario loop para evitar erro no resultado

	if (length(pos_temp) == 1) {
		ind_amostra = pos_temp
	} else {
		ind_amostra = sample(pos_temp,1) # se necessario, mudar tamanho de amostra aqui
	}
	
	amostras_processos_PESQ = rbind(amostras_processos_PESQ,concord_que_PESQ[[2]][ind_amostra,])
		
}

# eliminando variavel "processos" que nao e mais util
rm(processos)

# eliminando linha com NAs
amostras_processos_PESQ	= amostras_processos_PESQ[-1,]

# retirando nomes de linhas
row.names(amostras_processos_PESQ) = NULL

# conferindo objeto
amostras_processos_PESQ

# edit(amostras_processos_PESQ)

# gerando lista de verbos pra eliminacao
lista_verbos_PESQ = c("acha","conhece","saber","sabendo","vendo")

# gerando indices
ind_elim_PESQ = gera_indices(lista_verbos_PESQ,amostras_processos_PESQ[,"A5"])

# conferindo indices
ind_elim_PESQ


#eliminando linhas nao relevantes
amostras_processos_PESQ	= amostras_processos_PESQ[-ind_elim_PESQ,]

# visualizando resultado
amostras_processos_PESQ

# exportando planilha com amostras

#importando e carregando pacote xlsx
#if (!require ("xlsx")) { install.packages("xlsx"); require ("xlsx")} else { require("xlsx")}
	
# exportando amostras
#write.xlsx(amostras_processos_PESQ,"amostras_processos_PESQ_novo.xlsx",row.names=T)
#write.xlsx(amostras_processos_PESQ,"amostras_processos_PESQ_novo2.xlsx",row.names=F)


# TRADUTORES

# gerando matriz vazia para preencher
linhas_processos_com_TRAD = matrix(nrow=1,ncol=11)

# loop para buscar linhas de ocorrencia e gerar linhas de concordancia
for (processos in seq_along(1:length(lista_proc_comum_TRAD))) {
	
	# buscando linhas
	pos_temp = grep(lista_proc_comum_TRAD[processos],concord_que_TRAD[[2]][,"A5"])

	# gerando concordancia
	linhas_processos_com_TRAD = rbind(linhas_processos_com_TRAD,concord_que_TRAD[[2]][pos_temp,])

}

# eliminando variavel "processos" que nao e mais util
rm(processos)

# eliminando linhas de NAs
linhas_processos_com_TRAD = linhas_processos_com_TRAD[-1,]


#visualizando dados

linhas_processos_com_TRAD

#edit(linhas_processos_com_TRAD)


# Exportar linhas pro excel

#importando e carregando pacote xlsx
#if (!require ("xlsx")) { install.packages("xlsx"); require ("xlsx")} else { require("xlsx")}
	
# exportando linhas

#write.xlsx(linhas_processos_com_TRAD,"linhas_processos_com_TRAD.xlsx",row.names=T)


# TIRANDO AMOSTRAS PARA GERAR TABELA

# gerando lista provisoria a ser preenchida
amostras_processos_TRAD = matrix(nrow=1,ncol=11)

colnames(amostras_processos_TRAD) = colnames(concord_que_TRAD[[2]])

# Loop para preencher linhas e gerar amostras

for (processos in seq_along(1:length(lista_proc_comum_TRAD))) {
	
	# gerando palavra de busca usando expressao regular pra evitar erros
	pal_busca = paste(lista_proc_comum_TRAD[processos],"$",sep="")

	# buscando linhas
	pos_temp = grep(pal_busca,concord_que_TRAD[[2]][,"A5"])
	
	# gerando semente (seed) para amostra
	set.seed(processos)
	
	# gerando amostra de 1 item dentre os indices
	# para amostra de tamanho 1, necessario loop para evitar erro no resultado

	if (length(pos_temp) == 1) {
		ind_amostra = pos_temp
	} else {
		ind_amostra = sample(pos_temp,1)
	}
	
	amostras_processos_TRAD = rbind(amostras_processos_TRAD,concord_que_TRAD[[2]][ind_amostra,])
		
}

# eliminando linha com NAs
amostras_processos_TRAD	= amostras_processos_TRAD[-1,]

# retirando nomes de linhas
row.names(amostras_processos_TRAD) = NULL

# gerando lista de verbos pra eliminacao
lista_verbos_TRAD = c("conhece","preferem","lembrando")

# gerando indices
ind_elim_TRAD = gera_indices(lista_verbos_TRAD,amostras_processos_TRAD[,"A5"])


# conferindo indices
ind_elim_TRAD


#eliminando linhas nao relevantes
amostras_processos_TRAD	= amostras_processos_TRAD[-ind_elim_TRAD,]

# visualizando resultado
amostras_processos_TRAD

# exportando planilha com amostras

#importando e carregando pacote xlsx
#if (!require ("xlsx")) { install.packages("xlsx"); require ("xlsx")} else { require("xlsx")}
	
# exportando amostras
#write.xlsx(amostras_processos_TRAD,"amostras_processos_TRAD_novo.xlsx",row.names=F)
#write.xlsx(amostras_processos_TRAD,"amostras_processos_TRAD_novo2.xlsx",row.names=T)


###############################################################################

### LISTAS DE COLOCADOS DOS TERMOS "EU", "A GENTE" E "QUE"

###############################################################################

## Lista de colocados de EU

# TRADUTORES

# gerando listas de colocados de "eu" (a esquerda e a direita)
coloc_EU_TRAD = colocados(dados_TRAD,"eu")

# conferindo resultado 

str(coloc_EU_TRAD)

coloc_EU_TRAD

# Exportando colocados

# exportando lista de colocados de "eu" a direita
#write.csv2(coloc_EU_TRAD[[2]],file= "coloc_EU_TRAD_direita.csv",row.names=F)

# exportando lista de colocados de "a gente" a esquerda
#write.csv2(coloc_EU_TRAD[[3]],file= "coloc_EU_TRAD_esquerda.csv",row.names=F)


# PESQUISADORES

# gerando listas de colocados de "eu" (a esquerda e a direita)
coloc_EU_PESQ = colocados(dados_PESQ,"eu")

# conferindo resultado 

str(coloc_EU_PESQ)

coloc_EU_PESQ

# Exportando colocados

# exportando lista de colocados de "eu" a direita
#write.csv2(coloc_EU_PESQ[[2]],file= "coloc_EU_PESQ_direita.csv",row.names=F)

# exportando lista de colocados de "a gente" a esquerda
#write.csv2(coloc_EU_PESQ[[3]],file= "coloc_EU_PESQ_esquerda.csv",row.names=F)


###############

## Lista de colocados de A GENTE

# TRADUTORES

# gerando listas de colocados de "a gente" (a esquerda e a direita)
coloc_A_GENTE_TRAD = colocados(dados_TRAD,"agente")

# conferindo resultado 

str(coloc_A_GENTE_TRAD)

coloc_A_GENTE_TRAD

# Exportando colocados

# exportando lista de colocados de "a gente" a direita
#write.csv2(coloc_A_GENTE_TRAD[[2]],file= "coloc_A_GENTE_TRAD_direita.csv",row.names=F)

# exportando lista de colocados de "a gente" a esquerda
#write.csv2(coloc_A_GENTE_TRAD[[3]],file= "coloc_A_GENTE_TRAD_esquerda.csv",row.names=F)



# PESQUISADORES

# gerando listas de colocados de "a gente" (a esquerda e a direita)
coloc_A_GENTE_PESQ = colocados(dados_PESQ,"agente")

# conferindo resultado 

str(coloc_A_GENTE_PESQ)

coloc_A_GENTE_PESQ

# Exportando colocados

# exportando lista de colocados de "a gente" a direita
#write.csv2(coloc_A_GENTE_PESQ[[2]],file= "coloc_A_GENTE_PESQ_direita.csv",row.names=F)

# exportando lista de colocados de "a gente" a esquerda
#write.csv2(coloc_A_GENTE_PESQ[[3]],file= "coloc_A_GENTE_PESQ_esquerda.csv",row.names=F)

###############

## Lista de colocados de QUE

# TRADUTORES

# gerando listas de colocados de "a gente" (a esquerda e a direita)
coloc_QUE_TRAD = colocados(dados_TRAD,"que")

# conferindo resultado 

str(coloc_QUE_TRAD)

coloc_QUE_TRAD


# Exportando colocados

# exportando lista de colocados de "a gente" a direita
#write.csv2(coloc_QUE_TRAD[[2]],file= "coloc_QUE_TRAD_direita.csv",row.names=F)

# exportando lista de colocados de "a gente" a esquerda
#write.csv2(coloc_QUE_TRAD[[3]],file= "coloc_QUE_TRAD_esquerda.csv",row.names=F)


# PESQUISADORES

# gerando listas de colocados de "a gente" (a esquerda e a direita)
coloc_QUE_PESQ = colocados(dados_PESQ,"que")

# conferindo resultado 

str(coloc_QUE_PESQ)

coloc_QUE_PESQ


# Exportando colocados

# exportando lista de colocados de "a gente" a direita
#write.csv2(coloc_QUE_PESQ[[2]],file= "coloc_QUE_PESQ_direita.csv",row.names=F)

# exportando lista de colocados de "a gente" a esquerda
#write.csv2(coloc_QUE_PESQ[[3]],file= "coloc_QUE_PESQ_esquerda.csv",row.names=F)


################################################################################
# FIM DO SCRIPT DE ANALISE DOS DADOS NAO ESTRUTURADOS
################################################################################
