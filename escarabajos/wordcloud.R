rm( list=ls() )
gc()

setwd( 'C:/Users/cosa/Documents/romi/' )
library(stringi)

ff = readLines('C:/Users/cosa/Documents/romi/confmat.txt', encoding='UTF-8')

#convert to ASCII to remove tildes y otras cosas
ff = tolower(stri_trans_general(ff,"Latin-ASCII"))


mylist <- list()

#esto depende del formato en que esté el archivo, ios te exporta el chat distinto que android
#Por ejemplo, el primero es de android, el sengundo de ios
#6/5/21, 19:48 - Txomin: Ahi estamos todos
#[10/04/21 03:07:02] Donut: No estas viendo la peli?
#la idea es elminar hasta luego del nombre:

for (lin in ff){
  lin2 = regmatches(lin,gregexpr("(?<=-).*",lin,perl=TRUE))
  lin3 = regmatches(lin2,gregexpr("(?<=:).*",lin2,perl=TRUE))
  if (nchar(lin3) != 0){
    mylist <- append(mylist, list(lin3))
  }
  #print(lin3)
}

#algunas librerias necesarias
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")


docs <- Corpus(VectorSource(mylist))

inspect(docs)

#aqui viene la limpieza de las palabras
#esta es una forma de eliminar caractéres
#toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
#docs <- tm_map(docs, toSpace, "/")
#docs <- tm_map(docs, toSpace, "@")
#docs <- tm_map(docs, toSpace, "\\|")



# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)


inspect(docs)

# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)


# remove spanish stopwords
docs <- tm_map(docs, removeWords, stopwords("spanish")) 

#palabras a eliminar
sw = c("list", "dale", "")
docs <- tm_map(docs, removeWords, sw) 
inspect(docs)



dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# hay algunas palabras que por más que las elimines siguen apareciendo, sospecho que tiene que ver con el encoding o esas cosas
# si aparecen, las elimino manualmente, usar con cuidado 
#d = d[-c(4),]


#finalmente, esto te genera la imagen
set.seed(1234)
X11()
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
