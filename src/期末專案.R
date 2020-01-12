library(jsonlite)
library(dplyr)
library(jiebaR)
library(ggplot2)
library(stringr)

jlist <- read_json("gossip.json")

jdataframe <- fromJSON("gossip.json") %>% 
  select(日期)

library(readr)
emotion <- read_csv("emotion.csv")
emotion$kind[emotion$kind %in% 0] <- -1


seg <- worker()


f1 <- function(x){
  x2 <- x$推文
  sapply(x2,f2)
}

f2 <- function(c){
  c2 <- c$留言內容
  segged <- segment(c2,seg)
}


comlist <- sapply(jlist,f1) %>% sapply(.,unlist)



f3 <- function(x){
  c <- emotion$kind[emotion$WORD %in% x] 
  c2 <- sum(c==1)
}

f4 <- function(x){
  c <- emotion$kind[emotion$WORD %in% x]
  c2 <- sum(c==-1)
}


pos <- sapply(comlist,f3)
neg <- sapply(comlist,f4)


jdataframe <- jdataframe %>% mutate(positive = pos,negative = neg)


d <- jdataframe$日期
mon <- str_extract(d,"(?<=\\s)[a-zA-Z]*(?=\\s{1,2}[0-9])") %>% match(.,month.abb)
date <- str_extract(d,"(?<=\\s{1,2})[0-9]{1,2}(?=\\s)")
time <- paste(mon,date, sep="-")
jdataframe$日期 <- as.Date(time, format = "%m-%e")



summar <- jdataframe %>% 
  group_by(日期) %>%
  summarise(fre_pos = sum(positive)/n(), fre_neg = sum(negative)/n())

ggplot(summar[1:31,])+
  geom_line(aes(x = 日期,y =fre_pos),color = "red")+
  geom_line(aes(x = 日期, y =fre_neg), color = "black")+
  labs(x="Date", y="Frequency", 
       title="八卦版情緒正負頻率")+
  xlim(summar$日期[1], summar$日期[31])+
  ylim(7.5, 10.5)+
  theme(text=element_text(family="宋體-繁 標準體", size=14))






