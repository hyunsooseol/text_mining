##############################
#제 2부 텍스트 데이터 처리####
#05 한국어 텍스트 데이터 처리#
##############################

#R을 이용한 한국어 자연어처리
library('KoNLP')
library('tm')
library('stringr')
#자바의 문제가 있을 수 있으며, 문제발생시 자신의 PC 사양에 맞는 자바를 설치하고 재실행. 

#2016년까지 출간된 저자의 한국어 논문들의 말뭉치를 구성하였다.
mytextlocation <- "D:/data/ymbaek_논문"
mypaper <- VCorpus(DirSource(mytextlocation))
mypaper

#우선 가장 최근 논문초록을 대상으로 한국어 자연어 처리를 진행해 보자. 
mykorean <- mypaper[[19]]$content
mykorean

#간단한 사전처리를 실시하여 봅시다. 
#영문표현들은 모두 삭제하였다(한국어 분석이기 때문에)
mytext <- str_replace_all(mykorean, "[[:lower:]]","")
#괄호를 삭제하였다. 
mytext <- str_replace_all(mytext, "\\(","")
mytext <- str_replace_all(mytext, "\\)","")
#따옴표를 삭제하였다. 
mytext <- str_replace_all(mytext, "‘","")
mytext <- str_replace_all(mytext, "’","")
#가운뎃점을 삭제하였다. 
mytext <- str_replace_all(mytext, " · ",", ")
mytext

#의미의 핵심이라고 할 수 있는 명사를 추출하였다. 
noun.mytext <- extractNoun(mytext)
noun.mytext
#명사들의 빈도 분석을 해보자. 
table(noun.mytext)

#한국어 말뭉치 텍스트 데이터 분석 
#숫자표현은 어떤 것들이 사용되었는지 확인 
mydigits <- lapply(mypaper, function(x) (str_extract_all(x, "[[:digit:]]{1,}")))
table(unlist(mydigits))
#특수기호는 어떤 것들이 사용되었고, 그 전후의 표현은 어떤가?
mypuncts <- lapply(mypaper, function(x) (str_extract_all(x, "\\b[[:alpha:]]{1,}[[:punct:]]{1,}[[:alpha:]]{1,}\\b")))
table(unlist(mypuncts))

#숫자표현들 모두 삭제 
mycorpus <- tm_map(mypaper, removeNumbers)

#지정된 표현들 교체
mytempfunct <- function(myobject, oldexp, newexp) {
  newobject <- tm_map(myobject,
                      content_transformer(function(x,pattern) gsub(pattern,newexp,x)), oldexp)
  newobject 
}
#특수기호들 처리 
mycorpus <- mytempfunct(mycorpus, "[[:lower:]]","")
mycorpus <- mytempfunct(mycorpus, "[[:upper:]]","")
mycorpus <- mytempfunct(mycorpus, "\\("," ")
mycorpus <- mytempfunct(mycorpus, "\\)"," ")
mycorpus <- mytempfunct(mycorpus, "‘"," ")
mycorpus <- mytempfunct(mycorpus, "’"," ")
mycorpus <- mytempfunct(mycorpus, " · ",", ")
mycorpus <- mytempfunct(mycorpus, "·",", ")
mycorpus <- mytempfunct(mycorpus, "ㆍ",", ")
mycorpus <- mytempfunct(mycorpus, "/","")
mycorpus <- mytempfunct(mycorpus, "-","")
mycorpus <- mytempfunct(mycorpus, "－","")
mycorpus <- mytempfunct(mycorpus, "\\?"," ")

#공란 처리 
mycorpus <- tm_map(mycorpus, stripWhitespace)

#명사 추출 후 문서를 명사들의 나열로 바꾸어주는 개인맞춤 함수 
myNounFun <- function(mytext){
  myNounList <- paste(extractNoun(mytext),collapse='  ')
  myNounList
}

myNounCorpus <- mycorpus 
for (i in 1:length(mycorpus)) {
  myNounCorpus[[i]]$content <- myNounFun(mycorpus[[i]]$content)
}

#전체 말뭉치 단어를 확인해 보자. 
table(unlist(lapply(myNounCorpus,function(x) str_extract_all(x,boundary("word")))))

#추출된 단어들중 몇몇은 문제들을 발견할 수 있다. 
#예를 들면 포퓰리즘, 포퓰리즘과, 포퓰리즘에 등이 모두 개별단어로 처리되어 있다. 
#추가적인 작업을 통해 이런 표현들을 정리하는 것도 괜찮지만, 양이 많을 경우 노력이 많이 소요된다. 
#몇개만 실시해 보자. 
temp <- myNounCorpus
for (i in 1:length(myNounCorpus)) {
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,"포퓰리즘[[:alpha:]]{1,}","포퓰리즘")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,"커뮤니케이션[[:alpha:]]{1,}","커뮤니케이션")
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,"참여[[:alpha:]]{1,}","참가")  
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,"참가[[:alpha:]]{1,}","참가") 
  myNounCorpus[[i]]$content <- str_replace_all(temp[[i]]$content,"위키리크스[[:alpha:]]{1,}","위키리크스") 
}
#DTM을 구축
dtm.k <- DocumentTermMatrix(myNounCorpus)
dtm.k
colnames(dtm.k[,])


