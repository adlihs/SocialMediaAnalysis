####ANALISIS DE SENTIMIENTO - MICROSOFT COGNITIVE####
#LINK: http://www.evoketechnologies.com/blog/sentiment-analysis-r-language/
#LINK PARA RENOVAR CLAVES DE API: https://azure.microsoft.com/es-es/try/cognitive-services/

####Libraries####
ipak(c("devtools", "rjson", "bit64", "httr"))
ipak("httpuv")
ipak('devtools')
ipak('twitteR')
ipak('plyr')
ipak('ggplot2')
ipak('wordcloud')
ipak('RColorBrewer')
ipak("dplyr")
ipak("tidytext")
ipak("tm")
ipak("SnowballC")
ipak("wordcloud")
ipak("RColorBrewer")
ipak("mscstexta4r")
ipak("xlsx")
ipak("readxl")
ipak("sqldf")
ipak("Rfacebook")
install_github("pablobarbera/Rfacebook/Rfacebook")
ipak("Rook")
ipak('tidyverse')
ipak('stringr')
ipak('lubridate')
ipak('ggplot2')
ipak('readr')

#debug(utils:::unpackPkgZip)
#install.packages("berryFunctions")
library("berryFunctions") #For timer() function

###VARIABLES PARA INICIALIZAR EL SERVICIO
Sys.setenv(MSCS_TEXTANALYTICS_URL="https://westcentralus.api.cognitive.microsoft.com/text/analytics/v2.0/")
Sys.setenv(MSCS_TEXTANALYTICS_KEY="XXXXXXXXX")

###INICIALIZAR EL SERVICIO
textaInit()

#######FACEBOOK ACCESS#############
###FB authentication
app_id <- "XXXXXX"
app_secret <- "XXXXXXX"  
options("browser" = NULL)
fb_oauth <- fbOAuth(app_id, app_secret, extended_permissions = FALSE,legacy_permissions = FALSE)

#we will just save them to be able to use them later
save(fb_oauth, file="fb_oauth")

#so if you want to connect to Facebook again you just have to call
load("fb_oauth")

#######FACEBOOK DATA##############



comentarios <- c()
output.path <- "C:/Users/Ed-Shany/Documents/Ed/R directory/outputs/"
fb.pages <- "carlosalvaradoquesada"
fb.pages <- gsub("[[:punct:]]", " ", fb.pages)
since.data <- '2018/02/04'
period <- format(Sys.Date(), "%Y%m")
fb.page.post <- getPage(page=fb.pages, n=2000, token=fb_oauth, since = since.data, until = Sys.Date())
#fb.page.post <- getPage(page=fb.pages, n=2000, token=fb_oauth, since = since.data, until = '2018/02/05' )

cantidad_post <- as.numeric(nrow(fb.pages))



######## PAGE POST OPERATIONS ########

######## PAGE POSTS COMMENTS OPERATIONS ########

###Export to Excel the page post list

fb.page.post.file.name <- paste0(output.path,"fb.page.post","_",fb.pages,"_",period,".xlsx") 
write.xlsx(fb.page.post, fb.page.post.file.name)

####CLEAN POST TEXT

# remove retweet entities
fb.page.post$message <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", fb.page.post$message)
fb.page.post$message <- gsub("(rt|via)((?:\\b\\W*@\\w+)+)", " ", fb.page.post$message)

# remove at people
fb.page.post$message <- gsub("@\\w+", " ", fb.page.post$message)

# remove punctuation
fb.page.post$message <- gsub("[[:punct:]]", " ", fb.page.post$message)
fb.page.post$message <- str_replace_all(fb.page.post$message,"?","a")
fb.page.post$message <- str_replace_all(fb.page.post$message,"?","e")
fb.page.post$message <-str_replace_all(fb.page.post$message,"?","i")
fb.page.post$message <- str_replace_all(fb.page.post$message,"?","o")
fb.page.post$message <- str_replace_all(fb.page.post$message,"?","u")
fb.page.post$message <- str_replace_all(fb.page.post$message,"?","u")

# remove numbers
fb.page.post$message <- gsub("[[:digit:]]", " ", fb.page.post$message)

# remove html links
fb.page.post$message <- gsub("http\\w+", "", fb.page.post$message)

# remove unnecessary spaces
fb.page.post$message <- gsub("[ \t]{2,}", " ", fb.page.post$message)
fb.page.post$message <- gsub("^\\s+|\\s+$", " ", fb.page.post$message)
fb.page.post$message <- gsub("[^[:alnum:]///' ]", "", fb.page.post$message)

# lower case using try.error with sapply
# define "tolower error handling" function 
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}
fb.page.post$message <- sapply(fb.page.post$message, try.error)

# remove empty posts
fb.page.post <- filter(fb.page.post,message!=" ")
fb.page.post <- filter(fb.page.post,message!="")


###SPLIT POST TEXT IN WORDS##
fb.page.post.word.df <- fb.page.post %>%
  unnest_tokens(word,message)

##REMOVE THE STOPWORDS
stop.words.es <- read_excel("C:/Users/Ed-Shany/Documents/Ed/R directory/stop_words_es.xlsx")
fb.page.post.word.df <- fb.page.post.word.df %>%
  anti_join(stop.words.es)

###COUNT POST WORDS
fb.page.word.count <- count(fb.page.post.word.df,word)

### RENAME WORDS DATA FRAME COLUMNS
colnames(fb.page.word.count) <- c('word','cantidad')


#CREATE A WORD CLOUD WITH THE PAGE POST WORDS
set.seed(1234)
wordcloud(words = fb.page.word.count$word, freq = fb.page.word.count$cantidad, min.freq = 5, scale = c(5,0.2),
          max.words =300, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(5, "Set1"))



######## PAGE POSTS COMMENTS OPERATIONS ########

###Get the comments from the page post
cantidad_post <- as.numeric(nrow(fb.page.post))

for (id in 2:cantidad_post){
  print(id)
  page_id <- as.numeric(id)
  post <- getPost(post=fb.page.post$id[page_id], n=2000, token=fb_oauth)
  comentarios <- rbind(comentarios,post$comments)
}

###Clean comments text

# remove retweet entities
comentarios$message <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", comentarios$message)
comentarios$message <- gsub("(rt|via)((?:\\b\\W*@\\w+)+)", " ", comentarios$message)

# remove at people
comentarios$message <- gsub("@\\w+", " ", comentarios$message)

# remove punctuation
comentarios$message <- gsub("[[:punct:]]", " ", comentarios$message)
comentarios$message <- str_replace_all(comentarios$message,"?","a")
comentarios$message <- str_replace_all(comentarios$message,"?","e")
comentarios$message <-str_replace_all(comentarios$message,"?","i")
comentarios$message <- str_replace_all(comentarios$message,"?","o")
comentarios$message <- str_replace_all(comentarios$message,"?","u")
comentarios$message <- str_replace_all(comentarios$message,"?","u")

# remove numbers
comentarios$message <- gsub("[[:digit:]]", " ", comentarios$message)

# remove html links
comentarios$message <- gsub("http\\w+", "", comentarios$message)

# remove unnecessary spaces
comentarios$message <- gsub("[ \t]{2,}", " ", comentarios$message)
comentarios$message <- gsub("^\\s+|\\s+$", " ", comentarios$message)
comentarios$message <- gsub("[^[:alnum:]///' ]", "", comentarios$message)


# lower case using try.error with sapply
comentarios$message <- sapply(comentarios$message, try.error)

# remove empty comments
comentarios <- filter(comentarios,message!=" ")
comentarios <- filter(comentarios,message!="")


#####ANALISIS DE SENTIMIENTO DE LOS COMENTARIOS#####


#Cantidad tital de comentarios
cantidad.comentarios <- length(comentarios$message)


#Se genera una secuencia con el fin de definir los rangos de comentarios que se van
#a extrar del dataframe [comentarios], sera un rango de 100 comentarios
#ya que el api tiene un limite de caracteres, lineas, etc para procesar
iteracciones <- seq(1,cantidad.comentarios, by=100)


#Se limpia el dataframe para comenzar con el proceso de generacion de sentimiento
comentarios.sentimiento.df <- comentarios.sentimiento.df[0,]

for (id in iteracciones ){
  timer(interval = 1, n = 2, write = FALSE) # uso este timer, porque el api tiene restriccion de llamados por segundo
  print(id)
  df.inicio <- id
  df.fin <- (id + 100)-1
  comentarios_df <- comentarios[df.inicio:df.fin,]
  comentarios_df <- comentarios_df[complete.cases(comentarios_df),]
  
  # Perform sentiment analysis
  analysis.sentiment <- textaSentiment(
    comentarios_df$message,                  # Input sentences or documents
    languages = rep("es", length(comentarios_df$message))
    
    # "en"(English, default)|"es"(Spanish)|"fr"(French)|"pt"(Portuguese)
    
  )
  comentarios.sentimiento.df <- rbind(comentarios.sentimiento.df,analysis.sentiment$results[1:2])
  #comentarios.sentimiento.df <- analysis.sentiment$results
}


###Agregar el resultado del sentimiento al dataframe de [comentarios]
comentarios <- comentarios %>%
  mutate(sentiment = comentarios.sentimiento.df$score) %>%
  mutate(polarity = ifelse(comentarios.sentimiento.df$score >=0.7 ,"positive",
                           ifelse(comentarios.sentimiento.df$score < 0.5,"negative",
                                  ifelse(comentarios.sentimiento.df$score >= 0.5 & comentarios.sentimiento.df$score < 0.7,"Neutral",0)
                           )
  )
  ) %>%
  mutate(source=fb.pages)

    

##Export posts comments with sentiment score and polarity
fb.comentarios.file.name <- paste0(output.path,"fb_comentarios","_",fb.pages,"_",period,".xlsx")
write.xlsx(comentarios, fb.comentarios.file.name)


###SE SEPARA EN PALABRAS INDIVIDUALES###
comentarios.word.df <- comentarios.sentimiento.df %>%
  unnest_tokens(word,text)

##SE ELIMINAN LAS STOPWORDS
comentarios.word.df <- comentarios.word.df %>%
  anti_join(stop.words.es)


###SE CUENTA CUANTAS VECES ESTA CADA PALABRA
word.count <- count(comentarios.word.df,word)
#View(word_count)
colnames(word.count) <- c('word','cantidad')


#SE GRAFICA LA FRECUENCIA DE LAS PALABRAS EN UN WORDCLOUD GRAPH
set.seed(1234)
wordcloud(words = word.count$word, freq = word.count$cantidad, min.freq = 3, scale = c(5,0.2),
          max.words =300, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
dev.off()



#Export the wordcloud
wordcloud.file.name<- paste0(output.path,"wordCloud","_",fb.pages,"_",period,".png")
png(filename = wordcloud.file.name)

#Get fan by country from FB Insight
insights.page.fans.country <- getInsights(object_id=fb.pages, 
                                          token=fb_oauth, metric='page_fans_country', period='lifetime')
#Export fan by country to excel
fb.fans.country.file.name <- paste0(output.path,"fb_fans_country","_",fb.pages,"_",period,".xlsx")
write.xlsx(insights.page.fans.country, fb.fans.country.file.name)

# getEvents(fb.pages, token=fb_oauth, api = "v2.9")
# 
# 
# gi <- getInsights(object_id="All1Place", token=fb_oauth, metric='page_fan_adds', period='day')
# View(gi)
# 
# insights_post_impressions <- getInsights(object_id='All1Place', 
#                         token=fb_oauth, metric='post_impressions', period='days_28')
# 
# insights_page_fans <- getInsights(object_id='All1Place',
#                         token=fb_oauth, metric='page_fans', period='lifetime')
# 

# 
# insights_page_posts_impressions <- getInsights(object_id='All1Place', 
#                                           token=fb_oauth, metric='page_posts_impressions', period='lifetime')
# 
# 
# 
# View(insights_page_fans_country)
# ?getInsights
