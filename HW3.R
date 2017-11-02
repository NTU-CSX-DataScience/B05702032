library(httr)
library(rjson)
library(plyr)

id <- "136845026417486"
query <-"posts.limit(50){id,message,type,created_time,updated_time,shares,comments.limit(0).summary(true),likes.limit(0).summary(true)}&since=2017-07-01&until=2017-10-31"
token <- "EAACEdEose0cBAGLCbz9WCadyUVZCZCKQVMiQvJaZBLrMwpJLYFwxqZCEuSXanCYvREBmsZCQ0ZB33SIXTeQQHdjrcOfNIrIOEn3gQG3q93Gaid3ViJZByALobjCmUTymHRIg9c0rynex9D7w8NwbjlclZCjewR8ZCeOoLCDe6byjv3lv9q6bFIMT5evXR5IAncdhpJYTZB4CMTZCwZDZD"
url <- sprintf("https://graph.facebook.com/v2.10/%s?fields=%s&access_token=%s",id,query,token)

res <- fromJSON(content(GET(url),'text'))

parsePost <- function(x){
  data.frame(created_time = x$created_time,
             id = x$id,
             updated_time = x$updated_time,
             type = if(!is.null(x$type)) x$type else NA,
             message = if(!is.null(x$message)) x$message else NA,
             shares_count = if(!is.null(x$shares)) x$shares$count else NA,
             likes_count = if(!is.null(x$likes)) x$likes$summary$total_count else NA,
             comments_count = if(!is.null(x$comments)) x$comments$summary$total_count else NA
  )
}

tiwfb.df <- do.call("rbind.fill", lapply(res$posts$data, parsePost))
nexturl <- res$posts$paging$"next"

while(T){
  nextres <- fromJSON(content(GET(nexturl),'text'))
  nex.df <- do.call("rbind.fill", lapply(nextres$data, parsePost))
  tiwfb.df <- rbind.fill(tiwfb.df, nex.df)
  print(nrow(tiwfb.df))
  nexturl <- nextres$paging$"next"
  if(is.null(nexturl)){break}
} 

id <- "136845026417486"
query <- "comments.limit(200)"
token <- "EAACEdEose0cBAGLCbz9WCadyUVZCZCKQVMiQvJaZBLrMwpJLYFwxqZCEuSXanCYvREBmsZCQ0ZB33SIXTeQQHdjrcOfNIrIOEn3gQG3q93Gaid3ViJZByALobjCmUTymHRIg9c0rynex9D7w8NwbjlclZCjewR8ZCeOoLCDe6byjv3lv9q6bFIMT5evXR5IAncdhpJYTZB4CMTZCwZDZD"
url <- sprintf("https://graph.facebook.com/v2.10/%s?fields=%s&access_token=%s",id, query, token)
url

res_ry <- fromJSON(content(GET(url),'text'))
parsePost <- function(x){
  data.frame(id = x$id,
             created_time =  x$created_time,
             message = if(!is.null(x$message)) x$message else NA
  )
}


tiwfb_post <- tiwfb.df[4,]
tiwfb_ry.df <- data.frame()
for( i in 1:nrow(tiwfb_post)){
  url <- sprintf("https://graph.facebook.com/v2.8/%s?fields=%s&access_token=%s",tiwfb_post$id[i], query, token)
  res_ry <- fromJSON(content(GET(url),'text'))
  nexturl <- res_ry$comments$paging$"next"
  post_nex.df <- do.call("rbind.fill", lapply(res_ry$comments$data, parsePost))
  tiwfb_ry.df <- rbind(tiwfb_ry.df,post_nex.df) 
  if (is.null(res_ry$comments$paging$"next")){
    print(tiwfb_post$id[i])
    print(i)
    next
  }
  while(T){
    nextres <-fromJSON(content(GET(nexturl),'text'))
    post_nex.df <- do.call("rbind.fill", lapply(nextres$data, parsePost))
    tiwfb_ry.df <- rbind(tiwfb_ry.df,post_nex.df)
    print(tiwfb_post$id[i])
    print(nrow(tiwfb_ry.df))
    nexturl <- nextres$paging$"next"
    if(is.null(nexturl)){
      break}
  }
}


write.csv(tiwfb_ry.df, file = "tiwfb_ry.csv", row.names = FALSE,fileEncoding = "big-5")


library(NLP)
library(tm)
library(jiebaRD)
library(jiebaR)
library(RColorBrewer)
library(wordcloud)
filesname <- list.files(getwd(), pattern="*.csv")
files <- lapply(filesname, readLines)
docs <- Corpus(VectorSource(files))
#移除可能有問題的符號
toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))
}

docs <- tm_map(docs, toSpace, "※")
docs <- tm_map(docs, toSpace, "◆")
docs <- tm_map(docs, toSpace, "‧")
docs <- tm_map(docs, toSpace, "的")
docs <- tm_map(docs, toSpace, "我")
docs <- tm_map(docs, toSpace, "是")
docs <- tm_map(docs, toSpace, "[a-zA-Z]")
#移除標點符號 (punctuation)
#移除數字 (digits)、空白 (white space)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)



mixseg = worker()
jieba_tokenizer=function(d){
  unlist(segment(d[[1]],mixseg))
}
seg = lapply(docs, jieba_tokenizer)
freqFrame = as.data.frame(table(unlist(seg)))
freqFrame = freqFrame[order(freqFrame$Freq,decreasing=TRUE), ]
library(knitr)
kable(head(freqFrame), format = "markdown")


wordcloud(freqFrame$Var1,freqFrame$Freq,
          scale=c(5,0.1),min.freq=15,max.words=1500,
          random.order=TRUE, random.color=FALSE, 
          rot.per=.1, colors=brewer.pal(8, "Dark2"),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)