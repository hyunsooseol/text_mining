#######################################
#제 2부 텍스트 데이터 처리############# 
#01 텍스트 분석을 위한 기초적 R 함수들#
#######################################

#리스트와 벡터의 구분
myvector <- c(1:6,'a')
myvector # 한개의 문자만 같이 들어가 있으면 숫자도  문자로 인식해서 반환한다.

mylist <- list(1:6,'a')
mylist

#list 형식의 오브젝트 소개
obj1 <- 1:4
obj2 <- 6:10
#간단한 리스트 형식의 오브젝트는 다음과 같다.
obj3 <- list(obj1,obj2)
obj3

#list함수는 연이어 사용할 수도 있다.
mylist <- list(obj1,obj2,obj3)
mylist
mylist[[3]][[2]][2]

#벡터로 구성된 자료의 경우 []를 사용하지만, 리스트형식은 [[]]을 사용한다
#예를 들어 아래와 같이 []을 사용하면 리스트를, [[]]을 사용하면 벡터를 얻을 수 있다.
mylist[[3]][1]

mylist[[3]][[1]]

#[[]]와 []에 익숙해지면 아래의 표현도 이해할 수 있다. 
mylist[[3]][[1]][2]

#unlist함수는 리스트를 벡터형식으로 돌려주는 역할이다. 
#unlist 함수는 유용하지만 조심하여 사용하기 바란다. 
myvector <- c(1:6,'a')
mylist <- list(1:6,'a')
unlist(mylist)
unlist(mylist) == myvector

#예를 들어 mylist의 모든 관측값의 평균을 구한다고 가정하자. 
mean(mylist[[1]][1:6])
mean(unlist(mylist)[1:6])

#텍스트형 자료(문자에서 단어로)
name1 <- "Donald"
myspace <- " "
name2 <- "Trump"
list(name1, myspace, name2)
unlist(list(name1, myspace, name2))

#오브젝트의 속성이 입력된 경우가 많으며, 상황에 따라 속성값을 저장할 필요가 있다
name <- c('갑','을','병','정')
gender <- c(2,1,1,2)
mydata <- data.frame(name,gender)
attr(mydata$name,"what the variable means") <- "응답자의 이름"
mydata$name
attr(mydata$gender,"what the variable means") <- "응답자의 성별"
myvalues <- gender
for (i in 1:length(gender)) {myvalues[i] <- ifelse(gender[i]==1,"남성","여성")}
attr(mydata$gender,"what the value means") <- myvalues
mydata$gender

#속성값 추출
attr(mydata$gender,"what the value means")
#속성값을 추출한 후 mydata에 새로운 변수로 추가해 보자. 
mydata$gender.character <- attr(mydata$gender,"what the value means")
mydata

#리스트 형식인 경우 lapply 함수가 매우 유용하다. 
mylist <- list(1:4,6:10,list(1:4,6:10))
lapply(mylist[[3]],mean)

#그러나 [[]][[]]의 형태를 가질 경우 주의할 필요가 있다. 
lapply(mylist,mean)
lapply(mylist[c(1,2,c(1,2))],mean)

#sapply 함수의 경우 lapply 함수 결과와 유사하지만 결과값에 unlist 함수를 적용한다는 점이 다르다
sapply(mylist[c(1,2,c(1,2))],sum)
unlist(lapply(mylist[c(1,2,c(1,2))],sum))

#tapply는 텍스트 데이터에서 종종 사용된다.
#다음과 같은 빈도표 2개를 가정해 보자. 
wordlist <- c("the","is","a","the")
doc1freq <- c(3,4,2,4)
doc2freq <- rep(1,4)
#아래의 사례를 보면 tapply() 함수가 어떤 역할을 하는지 알 수 있다. 
tapply(doc1freq,wordlist,length)
tapply(doc2freq,wordlist,length)
tapply(doc1freq,wordlist,sum)
tapply(doc2freq,wordlist,sum)

#다음과 같은 단어들의 연쇄로 구성된 세 문장들을 생각해 보자. 
sent1 <- c("earth","to","earth")
sent2 <- c("ashes","to","ashes")
sent3 <- c("dust","to","dust")
#한 문장에서 to는 1회, to가 아닌 단어는 2회 등장했다. 
#3문장에서 등장한 단어빈도가 어떠한지 tapply함수를 이용해 계산하자. 
myfreq <- c(rep(1,length(sent1)),rep(1,length(sent2)),rep(1,length(sent3)))
tapply(myfreq,c(sent1,sent2,sent3),sum)

  
  

