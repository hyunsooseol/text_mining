########################################
#제 3부 텍스트 데이터 분석 및 결과제시##
#02 토픽모형############################
########################################

#LDA를 시작하기 이전에, 다음의 문헌이 어렵더라도 3-4번 정도 훑어보기 바란다.
#Blei, D. M., Ng, A. Y., & Jordan, M. I. (2003). Latent dirichlet allocation. 
#Journal of machine Learning research, 3, 993-1022.
#영문학술논문 말뭉치에서 5개의 잠재토픽을 추출하자. 
#control=list(seed=11) 옵션을 지정하지 않으면 매번 실행한 결과가 달라진다. 
library('topicmodels')
lda.out <- LDA(dtm.e,control=list(seed=11),k=5)

#아래는 문서X토픽 행렬이다. 
dim(lda.out@gamma)
#아래는 토픽X단 행렬이다. 
dim(lda.out@beta)

#5개의 잠재토픽으로 분류된 상위12개 단어들을 살펴보자. 
terms(lda.out,12)
#순서대로 다음과 같이 이름을 붙여보자. 
#Topic 1=media psychology
#Topic 2=cross-culturalSNS
#Topic 3=SNS
#Topic 4=privacy
#Topic 5=politics

#각 문서가 담고있는 잠재토픽의 확률점수를 계산하였다. 
posterior_lda <- posterior(lda.out)
round(posterior_lda$topics,3)

#아래와 같이 향후 분석에 사용할 수 있도록 데이터 프레임으로 저장하였다. 
lda.topics <- data.frame(posterior_lda$topics)
#아래는 문서별 토픽 등장 확률의 총합이다. 
apply(lda.topics,1,sum)

#아래는 문서들 전체에서의 토픽등장 확률의 총합이다. 
apply(lda.topics,2,sum)

#년도별로 각각의 잠재토픽이 어떤 변화를 보이는지 살펴보자. 
tempyear <- rownames(lda.topics)
pubyear <- as.numeric(unlist(str_extract_all(tempyear,"[[:digit:]]{4}")))
#아래의 결과를 보면 년도별 잠재토픽 변화패턴을 살펴볼 수 있다.
topic.by.year <- round(aggregate(as.matrix(lda.topics)~pubyear,lda.topics,sum),5)
topic.by.year

#그래프를 그려보았다. 
library('ggplot2')
topic.by.year <- reshape(topic.by.year, idvar = "pubyear", varying = list(2:6),
                         v.names = "X", direction = "long")
colnames(topic.by.year) <- c('year','topic_i','score')
topic.by.year$topic_i[topic.by.year$topic_i==1] <- 'media psychology'
topic.by.year$topic_i[topic.by.year$topic_i==2] <- 'cross-cultural'
topic.by.year$topic_i[topic.by.year$topic_i==3] <- 'SNS'
topic.by.year$topic_i[topic.by.year$topic_i==4] <- 'privacy'
topic.by.year$topic_i[topic.by.year$topic_i==5] <- 'politics'
head(topic.by.year)

ggplot(data=topic.by.year, aes(x=year, y=score, fill=topic_i)) + 
  geom_bar(stat="identity")+
  scale_x_continuous(breaks=2009:2015,labels=2009:2015)+
  scale_y_continuous(breaks=2*(0:5),labels=2*(0:5))+
  scale_fill_manual(values=1:5)+
  labs(x="publication year",y="score",fill='latent topic')+
  ggtitle("LDA: English journal papers corpus")

#alpha 값이 LDA 결과에 미치는 효과 
#시나리오1
round(posterior(LDA(dtm.e,control=list(seed=11,alpha=10),k=5))$topics,3)

#시나리오2
round(posterior(LDA(dtm.e,control=list(seed=11,alpha=100),k=5))$topics,3)


#우선 LDA와 마찬가지로 5개의 잠재토픽을 추정하고, 상위 12개까지의 단어들을 점검한다. 
ctm.out <- CTM(dtm.e,control=list(seed=44),k=5)
terms(ctm.out,12)
# Topic 1=politics
# Topic 2=cross-cultural
# Topic 3=privacy
# Topic 4=media psychology
# Topic 5=SNS 

#잠재토픽과 단어들에 부여된 가중치를 살펴보고, 문서별 잠재토픽의 확률을 점검하였다. 
posterior_ctm <- posterior(ctm.out)
ctm.topics <- data.frame(posterior_ctm$topics)
round(ctm.topics,3)

#년도에 따라 CTM으로 분류한 잠재토픽의 변화를 살펴보았다. 
tempyear <- rownames(ctm.topics)
pubyear <- as.numeric(unlist(str_extract_all(tempyear,"[[:digit:]]{4}")))
topic.by.year <- round(aggregate(as.matrix(ctm.topics)~pubyear,ctm.topics,sum),3)
topic.by.year

topic.by.year <- reshape(topic.by.year, idvar = "pubyear", varying = list(2:6),
                         v.names = "X", direction = "long")
colnames(topic.by.year) <- c('year','topic_i','score')
topic.by.year$topic_i[topic.by.year$topic_i==1] <- 'politics'
topic.by.year$topic_i[topic.by.year$topic_i==2] <- 'cross-cultural'
topic.by.year$topic_i[topic.by.year$topic_i==3] <- 'privacy'
topic.by.year$topic_i[topic.by.year$topic_i==4] <- 'media psychology'
topic.by.year$topic_i[topic.by.year$topic_i==5] <- 'SNS'
#시간에 따른 변화를 살펴보자. 
ggplot(data=topic.by.year, aes(x=year, y=score, fill=topic_i)) + 
  geom_bar(stat="identity")+
  scale_x_continuous(breaks=2009:2015,labels=2009:2015)+
  scale_y_continuous(breaks=2*(0:5),labels=2*(0:5))+  
  scale_fill_manual(values=1:5)+
  labs(x="publication year",y="score",fill='latent topic')+
  ggtitle("CTM: English journal papers corpus")

#LDA와 CTM의 추정결과의 label을 통일시켜 보자. 
myctmclass <- myldaclass <- rep(NA,24)
myldaclass[as.vector(topics(lda.out))==1] <- 'media psy'
myldaclass[as.vector(topics(lda.out))==2] <- 'cross-cultural'
myldaclass[as.vector(topics(lda.out))==3] <- 'SNS'
myldaclass[as.vector(topics(lda.out))==4] <- 'privacy'
myldaclass[as.vector(topics(lda.out))==5] <- 'politics'
myctmclass[as.vector(topics(ctm.out))==1] <- 'politics'
myctmclass[as.vector(topics(ctm.out))==2] <- 'cross-cultural'
myctmclass[as.vector(topics(ctm.out))==3] <- 'Privacy'
myctmclass[as.vector(topics(ctm.out))==4] <- 'media psy'
myctmclass[as.vector(topics(ctm.out))==5] <- 'SNS'

#교차표를 그리면 아래와 같다. 
as.matrix(table(myldaclass,myctmclass))

#코더간 신뢰도 크리펜도르프의 알파를 구하면 아래와 같다. 
library("irr")
lda.vs.ctm <- as.matrix(rbind(as.factor(myldaclass),as.factor(myctmclass)))
kripp.alpha(lda.vs.ctm)


#한국어 학술논문 말뭉치를 대상으로도 LDA와 CTM을 실시해 보자.
#먼저 LDA를 실시하였다. 
lda.out <- LDA(dtm.k,control=list(seed=22),k=5)
terms(lda.out,12)
#topic1=문화
#topic2=정치
#topic3=효과연구-1
#topic4=효과연구-2
#topic5=조직
posterior_lda <- posterior(lda.out)
lda.topics <- data.frame(posterior_lda$topics)
tempyear <- rownames(lda.topics)
pubyear <- as.numeric(unlist(str_extract_all(tempyear,"[[:digit:]]{4}")))
topic.by.year <- round(aggregate(as.matrix(lda.topics)~pubyear,lda.topics,sum),5)
#결과는 아래와 같다. 
topic.by.year

#그래프를 그려보았다. 
topic.by.year <- reshape(topic.by.year, idvar = "pubyear", varying = list(2:6),
                         v.names = "score", direction = "long")
colnames(topic.by.year) <- c('year','topic_i','score')
topic.by.year$topic_i[topic.by.year$topic_i==1] <- '문화'
topic.by.year$topic_i[topic.by.year$topic_i==2] <- '정치'
topic.by.year$topic_i[topic.by.year$topic_i==3] <- '효과연구-1'
topic.by.year$topic_i[topic.by.year$topic_i==4] <- '효과연구-2'
topic.by.year$topic_i[topic.by.year$topic_i==5] <- '조직'
ggplot(data=topic.by.year, aes(x=year, y=score, fill=topic_i)) + 
  geom_bar(stat="identity")+
  scale_x_continuous(breaks=2004:2016,labels=2004:2016)+
  scale_y_continuous(breaks=2*(0:5),labels=2*(0:5))+
  scale_fill_manual(values=1:5)+
  labs(x="출판년도",y="점수",fill='잠재토픽')+
  ggtitle("잠재적 디리쉴레 할당모형: 한국어 학술논문 말뭉치") 

#다음은 CTM이다. 
ctm.out <- CTM(dtm.k,control=list(seed=44),k=5)
terms(ctm.out,12)
#topic1=조직
#topic2=정치 
#topic3=효과연구-2
#topic4=문화
#topic5=효과연구-1

posterior_ctm <- posterior(ctm.out)
ctm.topics <- data.frame(posterior_ctm$topics)
tempyear <- rownames(ctm.topics)
pubyear <- as.numeric(unlist(str_extract_all(tempyear,"[[:digit:]]{4}")))
topic.by.year <- round(aggregate(as.matrix(ctm.topics)~pubyear,ctm.topics,sum),5)
#결과는 아래와 같다. 
topic.by.year
#그래프를 그려보았다. 
topic.by.year <- reshape(topic.by.year, idvar = "pubyear", varying = list(2:6),
                         v.names = "score", direction = "long")
colnames(topic.by.year) <- c('year','topic_i','score')
topic.by.year$topic_i[topic.by.year$topic_i==1] <- '조직'
topic.by.year$topic_i[topic.by.year$topic_i==2] <- '정치'
topic.by.year$topic_i[topic.by.year$topic_i==3] <- '효과연구-2'
topic.by.year$topic_i[topic.by.year$topic_i==4] <- '문화'
topic.by.year$topic_i[topic.by.year$topic_i==5] <- '효과연구-1'
ggplot(data=topic.by.year, aes(x=year, y=score, fill=topic_i)) + 
  geom_bar(stat="identity")+
  scale_x_continuous(breaks=2004:2016,labels=2004:2016)+
  scale_y_continuous(breaks=2*(0:5),labels=2*(0:5))+
  scale_fill_manual(values=1:5)+
  labs(x="출판년도",y="점수",fill='잠재토픽')+
  ggtitle("상관토픽모형: 한국어 학술논문 말뭉치")

#LDA와 CTM의 추정결과의 label을 통일시켜 보자. 
myctmclass <- myldaclass <- rep(NA,19)
myldaclass[as.vector(topics(lda.out))==1] <- '문화'
myldaclass[as.vector(topics(lda.out))==2] <- '정치'
myldaclass[as.vector(topics(lda.out))==3] <- '효과연구-1'
myldaclass[as.vector(topics(lda.out))==4] <- '효과연구-2'
myldaclass[as.vector(topics(lda.out))==5] <- '조직'
myctmclass[as.vector(topics(ctm.out))==1] <- '조직'
myctmclass[as.vector(topics(ctm.out))==2] <- '정치'
myctmclass[as.vector(topics(ctm.out))==3] <- '효과연구-2'
myctmclass[as.vector(topics(ctm.out))==4] <- '문화'
myctmclass[as.vector(topics(ctm.out))==5] <- '효과연구-1'

#교차표를 그리면 아래와 같다. 
as.matrix(table(myldaclass,myctmclass))

#코더간 신뢰도 크리펜도르프의 알파를 구하면 아래와 같다. 
lda.vs.ctm <- as.matrix(rbind(as.factor(myldaclass),as.factor(myctmclass)))
kripp.alpha(lda.vs.ctm)



# 영문논문초록에 대한 STM 추정
library('tm')
library('stringr')
library('stm')
my.text.location <- "D:/data/ymbaek_papers"
mypaper  <- VCorpus(DirSource(my.text.location),
                    readerControl = list(language="en"))

#STM에서 사용되는 데이터가 어떤 형태인지 살펴보자. 
dim(gadarian)
colnames(gadarian)
summary(gadarian)

#list.files() 함수를 이용하면 해당폴더의 화일이름을 추출할 수 있다. 
myfilenames <- list.files(path=my.text.location,
                          pattern=NULL,all.files=TRUE)
#화일이름을 데이터 프레임으로 저장하였다. 
mytxtdf <- data.frame(myfilenames[3:26])
#해당 영문논문의 발간년도를 추출하였다. 
mytxtdf$year <- as.numeric(unlist(
  str_extract_all(mytxtdf[,1],"[[:digit:]]{4}")))
colnames(mytxtdf)[1] <- 'file.name'
#저자가 미국에 체류할 때를 0, 귀국하였을 때를 1로 하는 가변수를 생성한다. 
mytxtdf$return.kor <- ifelse(mytxtdf$year>2011,1,0) 
#논문의 초록을 문자형 자료로 입력하였다. 
mytxtdf$abstract <- NA
for (i in 1:24) {
  mytxtdf$abstract[i] <- as.character(mypaper[[i]][1])
}
#gadarian 데이터와 같은 형식이 된 것을 확인할 수 있다. 
summary(mytxtdf)

#텍스트의 사전처리(tm 라이브러리 이용) 
mypreprocess <- textProcessor(mytxtdf$abstract,metadata=mytxtdf)

mypreprocess$documents[1]

#DTM 구성함 
myout <- prepDocuments(mypreprocess$documents, 
                       mypreprocess$vocab, mypreprocess$meta,
                       lower.thresh=0)

#STM 추정 (5개의 토픽)
mystm <- stm(myout$documents, myout$vocab, K=5, 
             prevalence =~ return.kor, data=myout$meta,
             seed=2494,init.type="Spectral")

#각 토픽이 발현되는 단어들 점검
labelTopics(mystm,topics=1:5)

#토픽들 사이의 관계
mystm.corr <- topicCorr(mystm)
#토픽들 사이의 관계의 시각화
library('igraph')
plot(mystm.corr)

#각주
mytemp <- stm(myout$documents, myout$vocab, K=30, 
              prevalence =~ return.kor, data=myout$meta,
              seed=2494,init.type="Spectral")
mystm.corr <- topicCorr(mytemp)
plot(mystm.corr)

#메타데이터와 토픽발현가능성의 관계 테스트  
myresult <- estimateEffect(c(1:5) ~ return.kor, mystm, mytxtdf)
summary(myresult)

plot(myresult,covariate="return.kor",
     topics=4,model=mystm,xlim=c(-1.5,1.5))
