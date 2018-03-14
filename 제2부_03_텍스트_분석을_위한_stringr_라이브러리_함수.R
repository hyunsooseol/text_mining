###############################################
#제 2부 텍스트 데이터 처리#####################
#03 텍스트 분석을 위한 stringr 라이브러리 함수#
###############################################

#stringr 라이브러리 구동
library('stringr')
#위키피디아에서 R을 설명하는 첫 두 단락의 텍스트 데이터 
R_wiki <- "R is a programming language and software environment for statistical computing and graphics supported by the R Foundation for Statistical Computing. The R language is widely used among statisticians and data miners for developing statistical software and data analysis. Polls, surveys of data miners, and studies of scholarly literature databases show that R's popularity has increased substantially in recent years.
R is a GNU package. The source code for the R software environment is written primarily in C, Fortran, and R. R is freely available under the GNU General Public License, and pre-compiled binary versions are provided for various operating systems. While R has a command line interface, there are several graphical front-ends available."
#software environment표현을 추출해 보자. 
str_extract(R_wiki, "software environment")
#software environment표현들을 모두 추출해 보자. 
str_extract_all(R_wiki, "software environment")
#unlist 함수를 적용한 결과를 원한다면 simplify 옵션을 사용하자. 
str_extract_all(R_wiki, "software environment",simplify=TRUE)

#특수표현을 이용해 첫글자가 대문자로 시작되는 단어들을 찾아보자. 
myextract <- str_extract_all(R_wiki, "[[:upper:]]{1}[[:alpha:]]{0,}")
myextract

#해당되는 단어빈도는 다음과 같다. 
table(myextract)

#software environment표현이 등장하는 위치를 추출해 보자. 
str_locate(R_wiki, "software environment")
#software environment표현들이 등장하는 위치를 추출해 보자. 
str_locate_all(R_wiki, "software environment")

#특수표현을 이용해 첫글자가 대문자로 시작되는 단어들의 위치를 알아보자. 
mylocate <- str_locate_all(R_wiki, "[[:upper:]]{1}[[:alpha:]]{0,}")
head(mylocate)
dim(mylocate[[1]])

#해당되는 단어들의 시작위치/종료위치를 데이터로 정리해 보자. 
mydata <- data.frame(mylocate[[1]])
myextract <- str_extract_all(R_wiki, "[[:upper:]]{1}[[:alpha:]]{0,}")
#앞서 추출한 단어들 리스트인 myextract을 변수화 하여 데이터에 포함시키자
mydata$myword <- myextract[[1]]
#다음과 같이 해당 단어의 문자수를 계산할 수도 있다. 
mydata$myword.length <- mydata$end - mydata$start + 1
#아래와 같은 형식의 데이터를 얻었다. 
head(mydata)

#software environment표현을 software_environment로 바꾸어 보자. 
str_replace(R_wiki, "software environment", "software_environment")

#software environment표현들을 software_environment로 일괄교체해 보자. 
str_replace_all(R_wiki, "software environment", "software_environment")

#다음을 살펴보면 str_replace_all 함수가 어떤 역할을 했는지 알 수 있을 것이다. 
temp <- str_replace_all(R_wiki, "software environment", "software_environment")
table(str_extract_all(R_wiki,"software_environment|software|environment"))
table(str_extract_all(temp,"software_environment|software|environment"))

#특별한 의미를 부여하고 싶은 부분을 표시할 때 다음과 같이 해보자. 
#R, C는 프로그램 이름이기 때문에 뒤에 _computer.language_라는 표현을 붙여보자.  
temp <- str_replace_all(R_wiki, "R\\b", "R_computer.language_")
temp <- str_replace_all(temp, "C\\b", "C_computer.language_")
#_computer.language_라는 표현이 붙은 부분에는 어떤 단어들이 있고, 빈도는 어떤지 살펴보자
table(str_extract_all(temp, "[[:alnum:]]{1}_computer.language_"))

#str_split 함수는 strsplit와 유사하다. 
#우선 텍스트 데이터의 문단을 구분해 보자. 
R_wiki_para <- str_split(R_wiki, "\n")
R_wiki_para

#다음으로 문단별로 문장을 구분해 보자.
R_wiki_sent <- str_split(R_wiki_para[[1]], "\\. ")
R_wiki_sent

#str_split_fixed 함수를 설명하기 위해 4번째와 7번째 문장을 추출해 보자. 
my2sentences <- unlist(R_wiki_sent)[c(4,7)]
#각 문장의 단어수를 세어보자. 
mylength1 <- length(unlist(str_split(my2sentences[1], " ")))
mylength2 <- length(unlist(str_split(my2sentences[2], " ")))
mylength1; mylength2 

myfixed.short <- str_split_fixed(my2sentences, " ", 5)
myfixed.short

#이제 긴 단어수에 맞도록 옵션을 정한 후 str_split_fixed 함수를 적용한 예를 살펴보자
myfixed.long <- str_split_fixed(my2sentences, " ", 13)
myfixed.long

#이제 7개의 문장들 모두를 이용해 문장순서와 각문장의 제시단어 순서정보가 표시된 
#행렬을 만들어 보자. 
#먼저 각 문장의 단어수를 먼저 계산해 보았다.
#반복계산을 위해 각 문장의 단어수를 계산해서 투입할 수 있는 빈 오브젝트를 만들었다.
length.sentences <- rep(NA,length(unlist(R_wiki_sent)))
length.sentences
#반복계산을 통해 해당 문장 순서에 맞는 단어수를 계산해 투입하였다. 
for (i in 1:length(length.sentences)) {
  length.sentences[i] <- length(unlist(str_split(unlist(R_wiki_sent)[i], " ")))
}
#이제 각 문장의 단어수가 어떤지는 다음과 같다. 
length.sentences
#최대 단어수가 얼마인지 추출하여 오브젝트로 만들었다. 
max.length.sentences <- max(length.sentences)
#최대 단어수를 기준으로 문장X단어 행렬을 구성하였다. 
sent.word.matrix <- str_split_fixed(unlist(R_wiki_sent), " ", max.length.sentences)
mydata <- data.frame(sent.word.matrix)
rownames(mydata) <- paste('sent',1:length(unlist(R_wiki_sent)),sep='.')
colnames(mydata) <- paste('word',1:max.length.sentences,sep='.')
mydata

#인덱싱을 통해 원하는 문장의 단어, 단어들을 쉽게 알 수 있다. 
mydata[,1]
mydata[3,1:10]

#'R'라는 표현이 등장하는지 살펴보자. 
str_count(R_wiki, "R")
str_count(R_wiki_para[[1]], "R")
str_count(unlist(R_wiki_sent),"R")

#R이라는 단어가 등장한 후에 stat으로 시작하는 단어가 등장하는 빈도는 어떨까? 
str_count(unlist(R_wiki_sent), "R.{1,}stat[[:lower:]]{1,}")

#각 문장을 살펴보자. 
unlist(R_wiki_sent)[1:2]

#일단 s와 S를 구분할 필요는 없다고 가정해 보자. 
str_count(unlist(R_wiki_sent), "R.{1,}(s|S)tat[[:alpha:]]{1,}")

#어떻게 추출되었기에 풀리지 않는 것일까?
str_extract_all(unlist(R_wiki_sent)[1], "R.{1,}(s|S)tat[[:alpha:]]{1,}")

#또한 R과 stat 사이에는 'R'이라는 표현이 절대 들어가면 안된다. 
str_count(unlist(R_wiki_sent), "R[[:lower:][A-Q][S-Z][:digit:][:space:]]{1,}(s|S)tat[[:alpha:]]{1,}")

#보다 간단하게는 다음과 같이 할 수 있다. 
str_count(unlist(R_wiki_sent), "R{1}[^R]{1,}(s|S)tat[[:alpha:]]{1,}")

#시작위치와 종료위치를 지정하면 해당 텍스트를 추출해준다. 
str_sub(unlist(R_wiki_sent)[1],1,30)

#지정된 표현을 지정된 횟수만큼 반복입력한다. 
str_dup("software",3)
rep("software",3)

paste(rep("software",3),collapse='')
str_dup("software",3) == paste(rep("software",3),collapse='')

#지정된 표현의 글자수(공란포함)를 계산한다. 
str_length(unlist(R_wiki_sent))
nchar(unlist(R_wiki_sent))

#다음과 같은 사례가 있다고 가정해보자. 
name <- c("Joe","Jack","Jackie","Jefferson")
donation <- c("$1","$111","$11111","$1111111")
mydata <- data.frame(name,donation)
mydata

#총 글자수 15를 기준으로 이름은 왼쪽 정렬을, 기부금은 가운데 정렬을 하였다. 
name2 <- str_pad(mydata$name,width=15,side='right',pad=' ')
donation2 <- str_pad(mydata$donation,width=15,side='both',pad='~')
mydata2 <- data.frame(name2,donation2)
mydata2

#Joe의 글자수가 어떻게 달라졌는지 살펴보자. 
str_length(mydata$name[1])
str_length(mydata2$name2[1])

#패딩을 없애고 원래 데이터로 돌아가보자. 
name3 <- str_trim(mydata2$name2,side='right')
donation3 <- str_trim(str_replace_all(mydata2$donation2,'~',' '),side='both')
mydata3 <- data.frame(name3,donation3)
all(mydata == mydata3)

#R_wiki_sent 오브젝트의 문장들을 다시 다 연결하자. 
str_c(unlist(R_wiki_sent),collapse='. ')

str_c(unlist(R_wiki_sent),collapse='. ') == paste(unlist(R_wiki_sent),collapse='. ')

