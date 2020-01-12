# 原始碼說明文件

#1. 使用pttcrawler.py抓12月份PTT留言資料，存為gossip.json

#2. 載入套件
  library(jsonlite)
  library(dplyr)
  library(jiebaR)
  library(ggplot2)
  library(stringr)

#3. 讀入資料
#讀入json資料，為list格式
  jlist <- read_json("gossip.json")

#讀入json資料，為dataframe格式，取出前面欄位
  jdataframe <- fromJSON("gossip.json") %>% 
    select(日期)

#讀入情緒辭典
  library(readr)
  emotion <- read_csv("emotion.csv")
#把negative值改成-1
  emotion$kind[emotion$kind %in% 0] <- -1

#4. 建立斷詞
  seg <- worker()

#5. 建立所需function
#f1與f2用來取出jlist中的推文內容
f1 <- function(x){
  x2 <- x$推文
  sapply(x2,f2)
}

f2 <- function(c){
  c2 <- c$留言內容
  segged <- segment(c2,seg)
}

#f3與f4用來取出comlist中符合emotion裡的詞語
f3 <- function(x){
  c <- emotion$kind[emotion$WORD %in% x] 
  c2 <- sum(c==1)
}

f4 <- function(x){
  c <- emotion$kind[emotion$WORD %in% x]
  c2 <- sum(c==-1)
}

#6. 取出所需資料
#各貼文留言list(已斷詞)
  comlist <- sapply(jlist,f1) %>% sapply(.,unlist)


#各貼文的留言情緒正負向次數，存在向量中
  pos <- sapply(comlist,f3)
  neg <- sapply(comlist,f4)

#加到jdataframe中
  jdataframe <- jdataframe %>% mutate(positive = pos,negative = neg)


#7. 修正日期，變為年/月/日格式
d <- jdataframe$日期
mon <- str_extract(d,"(?<=\\s)[a-zA-Z]*(?=\\s{1,2}[0-9])") %>% match(.,month.abb)
date <- str_extract(d,"(?<=\\s{1,2})[0-9]{1,2}(?=\\s)")
time <- paste(mon,date, sep="-")
jdataframe$日期 <- as.Date(time, format = "%m-%e")


#8. summary要的資料(日期、pos頻、neg頻)，篩掉不要的日期
summar <- jdataframe %>% 
  group_by(日期) %>%
  summarise(fre_pos = sum(positive)/n(), fre_neg = sum(negative)/n())

#9. 作圖
ggplot(summar[1:31,])+
  geom_line(aes(x = 日期,y =fre_pos),color = "red")+
  geom_line(aes(x = 日期, y =fre_neg), color = "black")+
  labs(x="Date", y="Frequency", 
       title="八卦版情緒正負頻率")+
  xlim(summar$日期[1], summar$日期[31])+
  ylim(7.5, 10.5)+
  theme(text=element_text(family="宋體-繁 標準體", size=14))
