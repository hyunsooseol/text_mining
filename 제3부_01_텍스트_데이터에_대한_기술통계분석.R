########################################
#제 3부 텍스트 데이터 분석 및 결과제시##
#01 텍스트 데이터에 대한 기술통계분석###
########################################

#tm 라이브러리 함수를 설명했던 dtm.e 오브젝트를 사용할 것이다. 
#먼저 단어의 발현빈도를 구해보자. 
word.freq <- apply(dtm.e[,],2,sum)
#아래와 같은 형식의 벡터가 구성되어 있고, 총 단어수는 678로 나타났다. 
head(word.freq)
length(word.freq)

#빈도가 높은 것부터 낮은 것으로 정렬하자. 
sort.word.freq <- sort(word.freq,decreasing=TRUE)
sort.word.freq[1:20]

#다음과 같이 하면 누적빈도를 계산할 수 있다. 
cumsum.word.freq <- cumsum(sort.word.freq)
cumsum.word.freq[1:20]

#다음과 같이 하면 전체합이 1이 되는 비율을 계산할 수 있다. 
prop.word.freq <- cumsum.word.freq/cumsum.word.freq[length(cumsum.word.freq)]
prop.word.freq[1:20]

#단어빈도에 대한 간단한 분석  
plot(1:length(word.freq),prop.word.freq,type='l',
     xlab='Order of word frequency',ylab='Cumulative proportion',
     main="",axes=FALSE)
axis(1,at=round(67.8*(0:10)),labels=paste(10*(0:10),"%",sep=""))
axis(2,at=0.20*(0:5),labels=paste(20*(0:5),"%",sep=""))
for (i in 1:9) {
  text(6.8*10*i,0.05+prop.word.freq[6.8*10*i],
       labels=paste(round(100*prop.word.freq[6.8*10*i]),"%",sep=""))
  points(6.8*10*i,prop.word.freq[6.8*10*i],pch=19)
}

#wordcloud 라이브러리 구동 
library('wordcloud')
wordcloud(names(word.freq),freq=word.freq,scale=c(4,0.2),
          rot.per=0.0,min.freq=5,random.order=FALSE)

#색조를 넣으려면 RColorBrewer 라이브러리를 추가로 구동
library('RColorBrewer') 
#어떤 색이 가능한지는 다음을 실행하세요: display.brewer.all()
pal <- brewer.pal(4, "Dark2")
wordcloud(names(word.freq),freq=word.freq,scale=c(4,0.2),
          rot.per=0.0,min.freq=5,random.order=FALSE,col=pal)

#이제는 한국어 학술논문들의 말뭉치를 분석해 보자. 
#동일한 과정을 밟았다. 
word.freq <- apply(dtm.k[,],2,sum)
sort.word.freq <- sort(word.freq,decreasing=TRUE)
cumsum.word.freq <- cumsum(sort.word.freq)
#다음과 같이 하면 전체합이 1이 되는 비율을 계산할 수 있다. 
prop.word.freq <- cumsum.word.freq/cumsum.word.freq[length(cumsum.word.freq)]
#단어빈도에 대한 간단한 분석  
plot(1:length(word.freq),prop.word.freq,type='l',
     xlab='단어의 발현빈도',ylab='누적비율',
     main="",axes=FALSE)
axis(1,at=round(24.3*(0:10)),labels=paste(10*(0:10),"%",sep=""))
axis(2,at=0.20*(0:5),labels=paste(20*(0:5),"%",sep=""))
for (i in 1:9) {
  text(2.43*10*i,0.05+prop.word.freq[2.43*10*i],
       labels=paste(round(100*prop.word.freq[2.43*10*i]),"%",sep=""))
  points(2.43*10*i,prop.word.freq[2.43*10*i],pch=19)
}

pal <- brewer.pal(4, "Dark2")
wordcloud(names(word.freq),freq=word.freq,scale=c(4,0.05),
          rot.per=0.0,min.freq=5,random.order=FALSE,col=pal)

#findAssocs 함수를 사용하면 DTM, TDM에 등장하는 단어가 지정된 수치를 이상인 값으로 연관된 단어들 목록을 알 수 있다. 
#즉 아래는 dtm.e라는 DTM에 속한 polit라는 단어와 .50이상의 상관관계를 갖는 단어들의 목록을 구하는 방법이다. 
findAssocs(dtm.e,"polit",0.50)

#findAssocs는 사실 피어슨 상관계수다. 예를 들어 polit와 agenda 두 단어의 상관계수를 구해보자. 
var1 <- as.vector(dtm.e[,"polit"])
var2 <- as.vector(dtm.e[,"agenda"])
cor.test(var1,var2)

#위와 같은 과정을 개인맞춤형 함수로 지정해 보자. 
my.assoc.func <- function(mydtm,term1,term2){
  myvar1 <- as.vector(mydtm[,term1])
  myvar2 <- as.vector(mydtm[,term2])
  cor.test(myvar1,myvar2)
}
#동일한 결과를 얻을 수 있다. 
my.assoc.func(dtm.e,"polit","agenda")

#DTM을 TDM으로 전치시킨 후 문서와 문서의 상관계수도 구할 수 있다. 
my.assoc.func(t(dtm.e),"p2014a.txt","p2014f.txt")

#아래와 같이 문서와 문서의 상관계수 행렬을 계산할 수도 있다. 
length.doc <- length(rownames(dtm.e))
my.doc.cor <- matrix(NA,nrow=length.doc,ncol=length.doc)
for (i in 1:length.doc) {
  for (j in 1:length.doc) {
    my.doc.cor[i,j] <- my.assoc.func(t(dtm.e),rownames(dtm.e)[i],rownames(dtm.e)[j])$est
  }
}
rownames(my.doc.cor) <- colnames(my.doc.cor) <- rownames(dtm.e)
#상관계수 행렬의 일부만 살펴보자. 
round(my.doc.cor[1:4,1:4],2)

#예를 들어 첫번째 문서, 즉 p2009a.txt는 다른 문서들과 어떤 상관관계를 갖는지 살펴볼 수 있다. 
round(my.doc.cor[,1],2)

#아래와 같은 방법으로 문서간 상관계수의 히스토그램을 살펴보자. 
hist(my.doc.cor[lower.tri(my.doc.cor)],breaks=30,
     col='lightblue',xlim=c(-0.1,0.6),xlab="correlations",
     main="Correlations between English journal papers")
summary(my.doc.cor[lower.tri(my.doc.cor)])

#위의 상관계수 행렬을 이용해 EFA나 PCA를 실시하는 것도 가능하다. 
factanal(factors=5,covmat=my.doc.cor,rotation="promax")

#한국어 학술논문 말뭉치에 대해서도 문서간 유사도를 계산해보자. 
#영어 학술논문 말뭉치를 분석한 경우와 동일한 과정을 밟았다. 
length.doc <- length(rownames(dtm.k))
my.doc.cor <- matrix(NA,nrow=length.doc,ncol=length.doc)
for (i in 1:length.doc) {
  for (j in 1:length.doc) {
    my.doc.cor[i,j] <- my.assoc.func(t(dtm.k),rownames(dtm.k)[i],rownames(dtm.k)[j])$est
  }
}
rownames(my.doc.cor) <- colnames(my.doc.cor) <- rownames(dtm.k)
#상관계수의 히스토그램은 다음과 같다. 
hist(my.doc.cor[lower.tri(my.doc.cor)],breaks=30,
     col='lightblue',xlim=c(-0.1,0.6),xlab="상관계수",
     main="한국어 학술논문들 사이의 상관계수 분포")
#이번에는 가장 마지막 문서인 p2015d.txt 다른 문서들과 맺고 있는 상관관계를 살펴보았다. 
round(my.doc.cor[,nrow(my.doc.cor)],2)
summary(my.doc.cor[lower.tri(my.doc.cor)])
#마찬가지로 위의 상관계수 행렬을 이용해 EFA나 PCA를 실시하는 것도 가능하다. 
#하지만 별도로 제시하지는 않았다. 

#발현단어빈도에 기반하여 문서들의 유사도/거리를 살펴 보자.  
dist.dtm.e <- dist(dtm.e)
as.matrix(dist.dtm.e)[1:4,1:4]

#Ward’s method 사용 군집분석을 실시함. 
myclusters <- hclust(dist.dtm.e,method="ward.D2")
#군집분석결과를 그래프로 그리면 아래와 같다.
plot(myclusters)

#라벨이 별로 보기 좋지 않다. 년도와 년도 뒤의 ab..만 두고 나머지 텍스트는 지우자. 
myclusters$labels <- str_extract(myclusters$labels, "[[:digit:]]{4}[[:alpha:]]{1}")
#군집분석결과를 그래프로 그리면 아래와 같다.
plot(myclusters)

#원하는 군집의 수는 다음과 같이 계산하면 된다. 만약 5개를 원한다면
mygroup <- cutree(myclusters,k=5)
mygroup
table(mygroup)

#8개의 군집을 설정하자
mygroup <- cutree(myclusters,k=8)
table(mygroup)

#만약 색조가 들어간 군집분석결과를 그래프로 그리면 다음과 같다. 
#우선은 dendextend 라이브러리를 구동해야 한다. 
library('dendextend')
#우선은 덴드로그램 그림을 저장한다. 
dend <- as.dendrogram(myclusters)
#몇 개의 집단을 선정할지를 결정하였다. 
myk <- 8
#덴드로그램의 선의 색깔을 다르게 설정하였다. 
dend <- dend %>%
  color_branches(k = myk) %>%
  color_labels(dend, k=myk) %>%
  set("branches_lwd", 2) %>%
  set("branches_lty", 1)
plot(dend,main="Clustering documents",ylab="Height",
     ylim=c(0,30))

#8개의 군집을 설정하고, 각 군집별로 출간년도의 빈도를 구해보자
mytable <- table(str_extract(myclusters$labels,"[[:digit:]]*"),
                 +                  cutree(myclusters,k=8))
mytable


#위의 결과를 막대그래프로 그리면 보다 효율적이다. 
#사전준비작업을 합시다. 응용편을 참조하라. 
library('ggplot2')
cluster.by.year <- data.frame(mytable)
colnames(cluster.by.year) <- c('year','cluster','publish')
cluster.by.year$cluster <- paste('cluster',cluster.by.year$cluster,sep='')
cluster.by.year$year <- as.numeric(as.character(cluster.by.year$year))
#시간에 따른 변화를 살펴보자 
ggplot(data=cluster.by.year, aes(x=year, y=publish, fill=cluster)) + 
  geom_bar(stat="identity")+
  scale_x_continuous(breaks=2009:2015,labels=2009:2015)+
  scale_fill_manual(values=1:8,labels=paste('군집',1:8,sep=''))+
  labs(x="출판년도",y="출간논문수",fill='군집')

#한국어 학술논문 말뭉치에 대한 분석결과는 아래와 같다. 
#위에서 소개한 영문 학술논문 말뭉치에 대한 분석결과에서의 설명을 참조하라. 
dist.dtm.k <- dist(dtm.k)
myclusters <- hclust(dist.dtm.k,method="ward.D2")
#년도와 년도 뒤의 ab..만 두고 나머지 텍스트는 지우자. 
myclusters$labels <- str_extract(myclusters$labels, "[[:digit:]]{4}[[:alpha:]]{1}")
#군집분석결과를 그래프로 그리면 아래와 같다.
plot(myclusters)

#군집의 수를 5로 정하거나 8, 심지어 10으로 정해도 군집이 잘 나누어지지는 못한다. 
#적어도 국문 학술논문 말뭉치를 구성하는 문서들을 적절하게 군집화하는 것은 쉽지 않다.
#여기서는 영어 학술논문 말뭉치에서 적용했던 8을 그대로 이용하자. 
mygroup <- cutree(myclusters,k=5)
table(mygroup)
mygroup <- cutree(myclusters,k=8)
table(mygroup)
mygroup <- cutree(myclusters,k=10)
table(mygroup)

dend <- as.dendrogram(myclusters)
myk <- 8
dend <- dend %>%
  color_branches(k = myk) %>%
  color_labels(dend, k=myk) %>%
  set("branches_lwd", 2) %>%
  set("branches_lty", 1)
plot(dend,main="Clustering documents",ylab="Height",
     ylim=c(0,25))

mytable <- table(str_extract(myclusters$labels,"[[:digit:]]*"),
                 cutree(myclusters,k=8))
cluster.by.year <- data.frame(mytable)
colnames(cluster.by.year) <- c('year','cluster','publish')
cluster.by.year$cluster <- paste('cluster',cluster.by.year$cluster,sep='')
cluster.by.year$year <- as.numeric(as.character(cluster.by.year$year))
#시간에 따른 변화를 살펴보자 
ggplot(data=cluster.by.year, aes(x=year, y=publish, fill=cluster)) + 
  geom_bar(stat="identity")+
  scale_x_continuous(breaks=2004:2016,labels=2004:2016)+
  scale_y_continuous(breaks=2*(0:5),labels=2*(0:5))+
  scale_fill_manual(values=1:8,labels=paste('군집',1:8,sep=''))+
  labs(x="출판년도",y="출간논문수",fill='군집')

