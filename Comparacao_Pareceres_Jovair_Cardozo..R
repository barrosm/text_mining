library(tm)
library(wordcloud)
library(RColorBrewer)
install.packages("gdata", dependencies=TRUE)
library(gdata)

setwd("C:\\Users\\barro\\Dropbox\\paper_com_Ze_Francisco\\artigo_wordcloud")

# LEITURA ARQUIVO PDF
# uri <- sprintf("file://%s", system.file(file.path("doc", "tm.pdf"), package = "tm"))
# pdf <- readPDF(control = list(text = "-layout"))(elem = list(uri = uri),language = "en",id = "id1")

# Leitura Parecer Jovair 
pdf_1 <- readPDF(control = list(text = "-layout"))(elem = list(uri = "Parecer Dep. Jovair Arantes.pdf"),language = "pt",id = "id1")
myCorpus_1 = Corpus(VectorSource(pdf_1$content))
myCorpus_1 = tm_map(myCorpus_1, removePunctuation)
myCorpus_1 = tm_map(myCorpus_1, removeNumbers)


# LISTA DE STOPWORDS E PALAVRAS REMOVIDAS SERÁ A MESMA PARA AMBOS OS DOCS
# =================================================================================
lista=c("art","caso","atos","ser","sobre","caso","federal","obtenção","deputados","câmara","legislativo","executivo","nacional","poder","resultado","política","qualquer","diante","pode","assim","apenas")
lista=c(lista,"vez","deve","paulo","tais","somente","país","dessa","sido","desta","assim","desde","segundo","disso","conforme","tal","portanto","ainda","deste","diate","âmbito","nesse","ano")
lista=c(lista,"ponto","ano","base","nesse","maior","sob","qualquer","ato","além","momento","diante","porque","todas","todos","então","seguintes","especial","quanto","dos")
lista=c(lista,"presente","vista","durante","preliminar","sentido","politico","apenas","setor","pode","ter","âmbito","item","partir","bem","parte","neste","casa","abertura","congresso")
lista=c(lista,"senado","poderes","edição","estado","instauração","natureza","forma","presidente","república","processo","união","brasil","ser","parte","nacional","tais","ainda","conforme")
lista = c(lista, "por", "dos", "após", "sr", "sra", "neste", "nesta", "do", "da", "dos", "das")

mystopwords=c(stopwords("portuguese"),lista)
# ========================================================
# Stemming - junta palavras diferentes com mesmo radical
# ========================================================
install.packages("SnowballC")
require("SnowballC")


# myCorpus_1  é o Corpus baseado no parecer do Dep. Jovair
# ============================================================
myCorpus_1 = tm_map(myCorpus_1, removeWords,mystopwords)
myCorpus_1 <- tm_map(myCorpus_1, PlainTextDocument)
# NAO GOSTEI DO RESULTADO DO STEMMING - VAI PRECISAR DE AJUSTES
# myCorpus_1 <- tm_map(myCorpus_1, stemDocument)  # pacote SnowballC precisa estar instalado
myDTM_1 = TermDocumentMatrix(myCorpus_1, control = list(removePunctuation = TRUE,stopwords = TRUE, minWordLength = 2))
dim(myDTM_1)   # tamanho da term document matrix
m_1 = as.matrix(myDTM_1)
v_1 = sort(rowSums(m_1), decreasing = TRUE)  # Freqs. dos termos em ordem decrescente
# Exibe os 20 termos mais frequentes no parecer do Jovair
head(v_1, 20)

# Lê defesa da pres. Dilma pela AGU - José Eduardo Cardozo
# ===============================================================
pdf_2 <- readPDF(control = list(text = "-layout"))(elem = list(uri = "Manifestacao da Denunciada.pdf"),language = "pt",id = "id1")
myCorpus_2 = Corpus(VectorSource(pdf_2$content))
myCorpus_2 = tm_map(myCorpus_2, removePunctuation)
myCorpus_2 = tm_map(myCorpus_2, removeNumbers)

# myCorpus_2  é o Corpus baseado no parecer do Cardozo
# ============================================================
myCorpus_2 = tm_map(myCorpus_2, removeWords,mystopwords)
myCorpus_2 <- tm_map(myCorpus_2, PlainTextDocument)
# NAO GOSTEI DO RESULTADO DO STEMMING - VAI PRECISAR DE AJUSTES
# myCorpus_2 <- tm_map(myCorpus_2, stemDocument)  # pacote SnowballC precisa estar instalado
myDTM_2 = TermDocumentMatrix(myCorpus_2, control = list(removePunctuation = TRUE,stopwords = TRUE, minWordLength = 2))
dim(myDTM_2)   # tamanho da term document matrix
m_2 = as.matrix(myDTM_2)
v_2 = sort(rowSums(m_2), decreasing = TRUE) 
colnames(m_2)
# Exibe os 20 termos mais frequentes no documento da defesa
head(v_2, 20)

#myDTM_2$dimnames[[1]][1:100]  # mostra os 100 primeiros termos no documento de defesa
#rownames(myDTM_1)[1:10]
#rownames(myDTM_2)[1:10]
#inspect(myDTM_2[1:20,1:2])

# Exibe os termos mais frequentes nos dois docs que ocorrem pelo menos 50 vezes
findFreqTerms(myDTM_1, lowfreq=50)
findFreqTerms(myDTM_2, lowfreq=50)

# ===========================================================================
# Concatena as matrizes de termos dos dois documentos - requer o pacote
# gdata, pois as dimensões das matrizes m_1 e m_2 são diferentes
# ===========================================================================
concat_matrix <- cbindX(m_1, m_2) 
dim(concat_matrix)
# E comparacao com as dimensoes das matrizes originais
dim(m_1)
dim(m_2)
concat_v = sort(rowSums(concat_matrix), decreasing = TRUE)

# Exibe os 50 termos mais frequentes nos dois documentos combinados
head(concat_v, 50)

set.seed(4363)
# ===========================================================================
# Desenha nuvem com as palavras mais frequentes nos 2 documentos combinados
# ===========================================================================
wordcloud(names(concat_v),concat_v, scale=c(2.5,.5),min.freq=50,max.words=100, random.order=F, rot.per=.3, colors=brewer.pal(8, "Dark2"))


wordcloud(names(concat_v), concat_v, min.freq = 30)
# scale basically controls the difference between the largest and smallest font
# rot.per is the percentage of vertical text, and colors provides a wide choice 
# of symbolising your data, from single colours (e.g. colors="black") 
# to pre-set colour palettes from the ColorBrewer package

png("wordcloud_combinada_Jovair_Cardozo.png", width=1280,height=800)
wordcloud(names(concat_v),concat_v, scale=c(8,.3),min.freq=30,max.words=100, random.order=F, rot.per=.3, colors=brewer.pal(8, "Dark2"))
dev.off()

min_freq = 20
wordcloud(names(concat_v), concat_v, min.freq = min_freq, random.order=FALSE, rot.per=0.10,max.words = 40, colors=brewer.pal(8, "Dark2"), scale = c(2.5,0.5))
