########################################
#제 3부 텍스트 데이터 분석 및 결과제시##
#03 감정분석############################
########################################

#감정어휘 사전을 활용한 텍스트 감정분석 
library(tidytext)
library(tidyr)

#AFINN 감정어휘 사전 호출
get_sentiments("afinn")

#감정어휘 사전은 별도로 저장하여 사용도 가능함
AFINN <- data.frame(get_sentiments("afinn"))
summary(AFINN)
hist(AFINN$score,breaks=20,xlim=c(-6,6),col='blue',
     xlab='sentiment score in AFINN',
     main='')

#빙류의 opinion lexicon 사전 호출
get_sentiments("bing")

#EmoLex 사전 호출
get_sentiments("nrc")

#opinion lexicon, EmoLex 사전 저장 후 기초통계 분석
oplex <- data.frame(get_sentiments("bing"))
table(oplex$sentiment)

emolex <- data.frame(get_sentiments("nrc"))
table(emolex$sentiment)
emolex$word[emolex$sentiment=='anger']

# 텍스트 데이터 호출
library('tm')
library('stringr')
library('dplyr')
my.text.location <- "D:/data/ymbaek_papers"
mypaper  <- VCorpus(DirSource(my.text.location),
                    readerControl = list(language="en"))
mytxt <- c(rep(NA),24)
for (i in 1:24) {
  mytxt[i] <- as.character(mypaper[[i]][1])
}
#tidytext 형태의 데이터 구성 
my.df.text <- data_frame(paper.id=1:24, doc=mytxt)
#총 24개의 가로줄과 2개의 세로줄로 구성된 행렬
my.df.text

#이제 문서-단어로 구성된 행렬을 구성 
#unnest_tokens() 함수는 주어진 tidytext 오브젝트의 text 변수를 word로 구분한다는 의미 
#%>%는 파이프라고 불리며 해당 작업을 부여한다는 것 의미 
my.df.text.word <- my.df.text %>% 
  unnest_tokens(word,doc)
my.df.text.word

#inner_join() 함수를 사용하면 결과를 쉽게 얻을 수 있다. 
myresult.sa <- my.df.text.word %>%
  inner_join(get_sentiments("bing")) %>%
  count(word,paper.id,sentiment) %>%
  spread(sentiment,n,fill=0)
myresult.sa

#각주
myresult.sa <- my.df.text.word %>% inner_join(get_sentiments("bing"))
myresult.sa 
myresult.sa <- myresult.sa %>% count(word,paper.id,sentiment)
myresult.sa
myresult.sa <- myresult.sa %>% spread(sentiment,n,fill=0)
myresult.sa

#문서별로 긍정단어와 부정단어를 합쳐보자. 
#긍정적 감정어휘와 부정적 감정어휘의 차이값도 구하였다.
myagg <- summarise(group_by(myresult.sa, paper.id),
                   pos.sum = sum(positive),
                   neg.sum = sum(negative),
                   pos.sent = pos.sum-neg.sum)
myagg

#문서의 메타데이터를 구한 후 합쳐보자. 
myfilenames <- list.files(path=my.text.location,
                          pattern=NULL,all.files=TRUE)
paper.name <- myfilenames[3:26]
pub.year <- as.numeric(unlist(
  str_extract_all(paper.name,"[[:digit:]]{4}")))
paper.id <- 1:24
pub.year.df <- data.frame(paper.id,paper.name,pub.year)

# myagg <- myagg %>% full_join(pub.year.df)
myagg <- merge(myagg,pub.year.df,by='paper.id',all=TRUE)
# 다음과 같이 해도 동일하다
myagg
myagg$pos.sent <- ifelse(is.na(myagg$pos.sent),0,myagg$pos.sent)
myagg$pos.sum <- ifelse(is.na(myagg$pos.sum),0,myagg$pos.sum)
myagg$neg.sum <- ifelse(is.na(myagg$neg.sum),0,myagg$neg.sum)
myagg

myagg.long <- reshape(myagg,idvar='paper.id',
                      varying=list(2:4),timevar = "category",
                      v.names='value',direction="long")
myagg.long$cate[myagg.long$category==1] <- 'Positive words'
myagg.long$cate[myagg.long$category==2] <- 'Negative words'
myagg.long$cate[myagg.long$category==3] <- 'Positivity score'

library('ggplot2')
ggplot(data=myagg.long,aes(x=pub.year,y=value)) + 
  geom_bar(stat="identity")+
  labs(x='Publication year',y='value')+
  scale_x_continuous(limits=c(2009,2015))+
  facet_grid(cate~.)

myagg.long.year <- aggregate(value~pub.year+cate,myagg.long,mean)
myagg.long.year
ggplot(data=myagg.long.year,aes(x=pub.year,y=value)) + 
  geom_line()+
  labs(x='Publication year',y='value')+
  scale_x_continuous(limits=c(2009,2015))+
  facet_grid(cate~.)

#지도 기계학습을 이용한 감정분석
library(RTextTools)
#https://github.com/victorneo/Twitter-Sentimental-Analysis
#에서 다운로드 받은 데이터를 사용합니다.   
setwd("D:/data/Twitter-Sentimental-Analysis-master/")
#각 데이터를 저장합니다. 
h.train <- readLines("happy.txt")
s.train <- readLines("sad.txt")
h.test <- readLines("happy_test.txt")
s.test <- readLines("sad_test.txt")
#데이터를 각각 훈련데이터, 테스트데이터 순으로 결합합니다. 
tweet <- c(h.train,s.train,h.test,s.test)
#해당 트윗에 맞는 감정을 부여(happy=1,sad=0)
sent <- c(rep(1,length(h.train)), 
          rep(0,length(s.train)),
          rep(1,length(h.test)),
          rep(0,length(s.test)))

#데이터의 사전처리 
tweet.pp <- create_matrix(tweet,language="english", 
                          removeStopwords=FALSE,removeNumbers=TRUE, 
                          stripWhitespace=TRUE,removePunctuation=TRUE,
                          toLower=TRUE,stemWords=FALSE,tm::weightTfIdf)
tweet.pp <- as.matrix(tweet.pp)

#여러 기계학습 알고리즘을 적용
train.end <- length(h.train)+length(s.train)
all.end <- length(sent)
container <- create_container(tweet.pp, as.numeric(sent),
                              trainSize=1:train.end,testSize=(1+train.end):all.end,
                              virgin=FALSE)

myclassifier <- train_models(container, 
                             algorithms=c("SVM","GLMNET","SLDA",
                                          "TREE","BAGGING","RF","MAXENT","BOOSTING"))

#기계학습 알고리즘을 활용해 테스트 데이터의 문서에 드러난 감정을 예측
myresult <- classify_models(container,myclassifier)
dim(myresult)
head(myresult)

#테스트 데이터 문서에 대한 인간의 판단을 추출
mytestlabel <- as.numeric(sent)[(1+train.end):all.end]
#SVM 알고리즘 적용시
table(myresult$SVM_LABEL,mytestlabel)

#GLMNET 알고리즘 적용시
table(myresult$GLMNET_LABEL,mytestlabel)
#SLDA 알고리즘 적용시 
table(myresult$SLDA_LABEL,mytestlabel)
#TREE 알고리즘 적용시 
table(myresult$TREE_LABEL,mytestlabel)
#BAGGING 알고리즘 적용시 
table(myresult$BAGGING_LABEL,mytestlabel)
#RF 알고리즘 적용시 
table(myresult$FORESTS_LABEL,mytestlabel)
#MAXENT 알고리즘 적용시 
table(myresult$MAXENTROPY_LABEL,mytestlabel)
#LOGITBOOST 알고리즘 적용시 
table(myresult$LOGITBOOST_LABEL,mytestlabel)

#일반적이지는 않지만, 코더간 신뢰도를 구하는 것도 가능함. 
library('irr')
RF.label <- as.numeric(as.character(myresult$FORESTS_LABEL))
#크리펜도르프의 알파를 계산하면 다음과 같다. 
kripp.alpha(rbind(RF.label,mytestlabel))
#코헨의 카파를 계산하면 다음과 같다. 
kappa2(cbind(RF.label,mytestlabel))

#정밀도, 재현도, f-점수 계산 
myanalytics <- create_analytics(container, myresult)
summary(myanalytics)