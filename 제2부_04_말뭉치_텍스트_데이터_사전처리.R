##################################
#제 2부 텍스트 데이터 처리########
#04 말뭉치 텍스트 데이터 사전처리#
##################################

#공란처리(stripping white space) 과정
mytext <- c("software environment","software  environment","software\tenvironment")
mytext

#단어를 ' '으로 구분해보자. 
str_split(mytext,' ')

#각 오브젝트별 단어수와 문자수를 세보자. 
sapply(str_split(mytext, " "),length)
sapply(str_split(mytext, " "),str_length)

#공란처리과정을 거친 후에 어떻게 되는지 살펴보자. 
mytext.nowhitespace <- str_replace_all(mytext,"[[:space:]]{1,}"," ")
mytext.nowhitespace
#각 오브젝트별 단어수와 문자수를 세보자. 
sapply(str_split(mytext.nowhitespace,' '),length)
sapply(str_split(mytext.nowhitespace,' '),str_length)

#대소문자 통일과정
mytext <- "The 45th President of the United States, Donald Trump, states that he knows how to play trump with the former president"
myword <- unlist(str_extract_all(mytext,boundary("word")))
table(myword)

#Trump 와 States가 고유명사임이 드러나게 _unique_표현을 덧붙였다. 
myword <- str_replace(myword,"Trump","Trump_unique_")
myword <- str_replace(myword,"States","States_unique_")
#대문자를 소문자로 전환한 후 빈도표를 도출했다. 
table(tolower(myword))

mytext <- c("He is one of statisticians agreeing that R is the No. 1 statistical software.","He is one of statisticians agreeing that R is the No. one statistical software.")
str_split(mytext," ")

#숫자자료를 제거하는 방법
mytext2 <- str_split(str_replace_all(mytext,"[[:digit:]]{1,}[[:space:]]{1,}","")," ")
str_c(mytext2[[1]],collapse=" ")
str_c(mytext2[[2]],collapse=" ")

#숫자자료임을 표시하고 수치의 구체적인 내용은 고려하지 않는 방법
mytext3 <- str_split(str_replace_all(mytext,"[[:digit:]]{1,}[[:space:]]{1,}","_number_ ")," ")
str_c(mytext3[[1]],collapse=" ")
str_c(mytext3[[2]],collapse=" ")

mytext <- "Baek et al. (2014) argued that the state of default-setting is critical for people to protect their own personal privacy on the Internet."
#사례로 든 문장에서 al.의 .는 문장구분 표시가 아니다.
str_split(mytext,"\\. ")

#단어들을 살펴보면 다음과 같다. 
str_split(mytext," ")

#default-setting을 두 단어로? 아니면 한단어로? 개인적으로 2단어가 맞다고 봄.
#"성 다음의 et al. (년도)"의 형식을 띨 경우 하나의 단어로 교체.
#단어를 기준으로 분석하기 때문에, 마침표는 없어도 될 듯. 
mytext2 <- str_replace_all(mytext,"-"," ")
mytext2 <- str_replace_all(mytext2,"[[:upper:]]{1}[[:alpha:]]{1,}[[:space:]](et al\\.)[[:space:]]\\([[:digit:]]{4}\\)","_reference_")
mytext2 <- str_replace_all(mytext2,"\\.[[:space:]]{0,}","")
mytext2

mytext <- c("She is an actor","She is the actor")
#다음과 같이 불용단어들의 목록을 오브젝트로 만들어 사용하면 편하다
mystopwords <- "(\\ba )|(\\ban )|(\\bthe )"
str_replace_all(mytext,mystopwords,"")

#tm 라이브러리에 포함된 영어 불용단어 목록들은 두 가지다. 
library('tm')
#짧은 불용단어 목록들에는 총 174개의 단어들이 포함되어 있다. 
length(stopwords("en"))
#긴 불용단어 목록들에는 총 571개의 단어들이 포함되어 있다. 
length(stopwords("SMART"))

#length() 함수표현을 없애면 어떤 단어들이 불용단어 목록에 포함되었는지 알 수 있다. 
stopwords("en")
stopwords("SMART")

#다음과 같은 약식 어근동일화 프로그램을 만들어 보자. 
mystemmer.func <- function(mytextobject,mystemmer,mystemmed){
  mytext <- str_replace_all(mytext,
                            "(\\bam )|(\\bare )|(\\bis )|(\\bwas )|(\\bwere )|(\\bbe )","be ")
  mytext
}

#예시 텍스트 데이터 
mytext <- c("I am a boy. You are a boy. He might be a boy.")
mytext.stem <- mystemmer.func(mytext)
#어근 동일화이전과 이후의 텍스트 데이터의 단어들이 어떻게 다른 빈도표를 갖고 있는지 살펴보자. 
table(str_split(mytext," "))
table(str_split(mytext.stem," "))

#아래의 텍스트 데이터에서는 the/The, United, States가 3회 같은 순서로 제시되고 있다. 
mytext <- "The United States comprises fifty states. In the United States, each state has its own laws. However, federal law overrides state law in the United States."
myword <- unlist(str_extract_all(mytext,boundary("word")))
length(table(myword))
sum(table(myword))
#United, States를 붙어있는 단어, 즉 2-gram, 혹은 bigram이라고 가정하자. 
mytext.2gram <- str_replace_all(mytext,"\\bUnited States","United_States")
myword2 <- unlist(str_extract_all(mytext.2gram,boundary("word")))
length(table(myword2))
sum(table(myword2))
#the/The. United, States를 붙어있는 단어, 즉 3-gram, 혹은 trigram이라고 가정하자. 
mytext.3gram <- str_replace_all(mytext,"\\b(t|T)he United States","The_United_States")
myword3 <- unlist(str_extract_all(mytext.3gram,boundary("word")))
length(table(myword3))
sum(table(myword3))

#tm 라이브러리 구동 
library('tm')
#모아놓은 텍스트 자료의 화일 위치를 설정합니다. 
#2009년부터 2015년까지 백영민의 출간논문 초록의 말뭉치를 구성합니다. 
my.text.location <- "D:/data/ymbaek_papers"
mypaper  <- VCorpus(DirSource(my.text.location))
#말뭉치를 검토해 봅시다
mypaper
summary(mypaper)

#말뭉치를 구성하는 두 번째 문서는?
mypaper[[2]]

#말뭉치를 구성하는 두 번째 문서의 내용은? 
mypaper[[2]]$content

#말뭉치를 구성하는 두 번째 문서의 메타데이터는? 
mypaper[[2]]$meta

#메타데이터 입력
meta(mypaper[[2]],tag='author') <- 'Y. M. Baek'
meta(mypaper[[2]])

#다음과 방식을 사용하면 특수기호 사용 전후의 단어들이 무엇인지 살펴볼 수 있다.
myfunc <- function(x) {str_extract_all(x,"[[:alnum:]]{1,}[[:punct:]]{1}?[[:alnum:]]{1,}")}
mypuncts <- lapply(mypaper, myfunc)
table(unlist(mypuncts))

#다음과 방식을 사용하면 수치로 표시된 자료가 어떤 것인지 살펴볼 수 있다. 
myfunc <- function(x) {str_extract_all(x,"[[:digit:]]{1,}")}
mydigits <- lapply(mypaper, myfunc)
table(unlist(mydigits))

#다음과 방식을 사용하면 대문자로 시작하는 단어를 확인할 수 있다(고유명사 확인에 유용함)
myfunc <- function(x) {str_extract_all(x,"[[:upper:]]{1}[[:alpha:]]{1,}")}
myuppers <- lapply(mypaper, myfunc)
table(unlist(myuppers))

#우선 숫자표현들을 모두 삭제하였다. 
mycorpus <- tm_map(mypaper, removeNumbers)

#다음으로 특수문자가 들어간 표현들 중 하나의 단어로 취급되어야 하는 것들을 처리하였다.
#removePunctuation의 경우 텍스트 성격을 고려한 아래의 사항들을 반영하지 못한다.
mytempfunc <- function(myobject,oldexp,newexp){
newobject <- tm_map(myobject,
content_transformer(function(x, pattern) gsub(pattern, newexp, x)), oldexp)
newobject
}
mycorpus <- mytempfunc(mycorpus,"-collar","collar")
mycorpus <- mytempfunc(mycorpus,"\\b((c|C)o-)","co")
mycorpus <- mytempfunc(mycorpus,"\\b((c|C)ross-)","cross")
mycorpus <- mytempfunc(mycorpus,"e\\.g\\.","for example")
mycorpus <- mytempfunc(mycorpus,"i\\.e\\.","that is")
mycorpus <- mytempfunc(mycorpus,"\\'s","")
mycorpus <- mytempfunc(mycorpus,"s’","s")
mycorpus <- mytempfunc(mycorpus,"ICD-","ICD")
mycorpus <- mytempfunc(mycorpus,"\\b((i|I)nter-)","inter")
mycorpus <- mytempfunc(mycorpus,"K-pop","Kpop")
mycorpus <- mytempfunc(mycorpus,"\\b((m|M)eta-)","meta")
mycorpus <- mytempfunc(mycorpus,"\\b((o|O)pt-)","opt")
mycorpus <- mytempfunc(mycorpus,"\\b((p|P)ost-)","post")
mycorpus <- mytempfunc(mycorpus,"-end","end")
mycorpus <- mytempfunc(mycorpus,"\\b((w|W)ithin-)","within")
mycorpus <- mytempfunc(mycorpus,"=","is equal to")
mycorpus <- mytempfunc(mycorpus,"and/or","and or")
mycorpus <- mytempfunc(mycorpus,"his/her","his her")
mycorpus <- mytempfunc(mycorpus,"-"," ")

#나머지 특수문자들은 공란으로 처리하여 제거하였다. 
#removePunctuation의 경우 and/oR을 and or로 바꾸지 않고, andor로 바꾸는 등 여기서는 문제가 적지 않다. 
mycorpus <- tm_map(mycorpus, removePunctuation)

#공란처리과정 
mycorpus <- tm_map(mycorpus, stripWhitespace)

#대소문자를 통합하였다. 
mycorpus <- tm_map(mycorpus, content_transformer(tolower))

#불용단어들 삭제(SMART 목록 사용)
mycorpus <- tm_map(mycorpus, removeWords, words=stopwords("SMART"))

#스테밍(어근동일화) 처리하였다. 
mycorpus <- tm_map(mycorpus, stemDocument, language="en")

#우선 문자수와 단어수를 계산하기 위한 개인맞춤형 함수 설정
mycharfunc <- function(x) {str_extract_all(x,".")}
mywordfunc <- function(x) {str_extract_all(x,boundary("word"))}
#사전처리과정 적용이전
mychar <- lapply(mypaper, mycharfunc)
myuniquechar0 <- length(table(unlist(mychar)))
mytotalchar0 <- sum(table(unlist(mychar)))
myword <- lapply(mypaper, mywordfunc)
myuniqueword0 <- length(table(unlist(myword)))
mytotalword0 <- sum(table(unlist(myword)))

#사전처리과정 적용이후
mychar <- lapply(mycorpus, mycharfunc)
myuniquechar1 <- length(table(unlist(mychar)))
mytotalchar1 <- sum(table(unlist(mychar)))
myword <- lapply(mycorpus, mywordfunc)
myuniqueword1 <- length(table(unlist(myword)))
mytotalword1 <- sum(table(unlist(myword)))

#사전처리로 인해 어떻게 변화했는지를 살펴보자. 
results.comparing <- rbind(
  c(myuniquechar0,myuniquechar1),
  c(mytotalchar0,mytotalchar1),
  c(myuniqueword0,myuniqueword1),
  c(mytotalword0,mytotalword1))
colnames(results.comparing) <- c("before","after")
rownames(results.comparing) <- c("고유문자수","총문자수","고유단어수","총단어수")
results.comparing

#이제 문서X단어행렬을 구축하자.
#TermDocumentMatrix를 사용하면 가로줄에는 단어가, 세로줄에는 문서가 배치된다. 
dtm.e <- DocumentTermMatrix(mycorpus) 
dtm.e

#행렬식으로 표현
rownames(dtm.e[,])
colnames(dtm.e[,])

#행렬의 일부만 살펴보고 싶다면 
inspect(dtm.e[1:3,50:55])

#아래와 같이 하면 tf-idf 함수를 적용해 가중치가 부여된 행렬이 계산된다. 
dtm.e.tfidf <- DocumentTermMatrix(mycorpus,
               control=list(weighting=function(x) weightTfIdf(x, normalize=FALSE))) 
dtm.e.tfidf

#mycorpus 말뭉치에서 TF는 크지만 TF-IDF는 상대적으로 작은 단어들을 살펴보자. 
#아래와 같이 TF값과 TF-IDF값을 벡터로 추출하였다. 
value.tf.dtm <- as.vector(as.matrix(dtm.e[,]))
value.tfidf.dtm <- as.vector(as.matrix(dtm.e.tfidf[,]))
#단어명과 문서명을 추출하였다. 
word.label.dtm <- rep(colnames(dtm.e[,]),each=dim(dtm.e[,])[1])
doc.label.dtm <- rep(rownames(dtm.e[,]),dim(dtm.e[,])[2])
#단어, 문서, TF, TF-IDF의 값을 하나로 모아 데이터프레임을 만들었다.
mydata <- data.frame(word.label.dtm,doc.label.dtm,value.tf.dtm,value.tfidf.dtm)
colnames(mydata) <- c('word','doc','tf','tfidf')
mydata[120:130,]

#상관계수는 다음과 같다.
cor.test(mydata$tf,mydata$tfidf,method="kendall")

cor.test(mydata$tf[mydata$tf>0],mydata$tfidf[mydata$tfidf>0],method="kendall")

#그렇다면 어떤 단어들이 TF는 높지만 TF-IDF는 낮을까?
mydata2 <- subset(mydata, tf>0&tfidf>0)
mydata3 <- subset(mydata2, tf>median(mydata2$tf) & tfidf<median(mydata2$tfidf))
table(mydata3$word)[table(mydata3$word)>0]

#엔그램 도출과정
library('RWeka')
bigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=3))
#앞에서 사용한 The United States 사례를 살펴보자. 
mytext <- c("The United States comprises fifty states.","In the United States, each state has its own laws.","However, federal law overrides state law in the United States.")
mytemp <- VCorpus(VectorSource(mytext))
ngram.tdm <- TermDocumentMatrix(mytemp, control=list(tokenize=bigramTokenizer))
bigramlist <- apply(ngram.tdm[,],1,sum)
#빈도수가 높은 바이그램부터 살펴보자. 
sort(bigramlist,decreasing=TRUE)

#mycorpus에 적용한 결과는 아래와 같다. 
ngram.tdm <- TermDocumentMatrix(mycorpus, control=list(tokenize=bigramTokenizer))
bigramlist <- apply(ngram.tdm[,],1,sum)
#최상위 10개의 바이그램들을 살펴보자.
sort(bigramlist,decreasing=TRUE)[1:10]


