#######################################
#제 2부 텍스트 데이터 처리############# 
#02 텍스트 분석을 위한 R의 베이스 함수#
#######################################

#알파벳 출력 함수 
letters[3]
LETTERS[3]
letters[1:26]
LETTERS[1:26]

tolower("Eye for eye")
toupper("Eye for eye")

#nchar() 함수의 디폴트는 문자수를 세는 것이다. 
nchar('Korea') 
nchar('Korea',type='bytes') 
nchar('한국') 
nchar('한국',type='bytes')

#공란이 있으면 다르게 취급된다. 
nchar('Korea')
nchar('Korea ')
#스페이스로 표현된 공란이 아니라 탭(tab)으로 구분된 공란일 경우는 
#\t으로 표현하며, 이 때는 2개의 문자수가 아니라 1개의 문자수를 갖는다. 
nchar('Korea\t')
nchar('Korea\t',type='bytes') 
#줄바꾸기를 했을 경우 \n으로 표현한다. 마찬가지로 1개의 문자수를 갖는다.
nchar('Korea, Republic of')
nchar('Korea, 
       Republic of')
#다음과 같이 하면 줄바꾸기가 된 형태로 인식된다. 
nchar('Korea, \nRepublic of')

#단어 단위로 문장을 분해하는 방법
mysentence <- 'Learning R is so interesting'
strsplit(mysentence, split=' ')

#문자 단위로 단어를 분해하는 방법
mywords <- strsplit(mysentence, split=' ')
strsplit(mywords[[1]][5], split='')

#각 단어들이 각각 어떤 문자들로 구성되었는지를 표현할 경우 
#우선 5개의 하위 오브젝트를 갖는 리스트 오브젝트를 만든다. 
myletters <- list(rep(NA,5))
#myletters 오브젝트에 문자분해된 단어를 입력하였다. 
for (i in 1:5) {
  myletters[i] <- strsplit(mywords[[1]][i], split='')
}
myletters

#문자들을 다시 합쳐서 단어로 구성해 보자. 
#예를 들어 myletters[[1]]에 들어있는 8개의 문자를 합쳐보자
paste(myletters[[1]],collapse='')

#반복문을 이용해서 myletters에 들어있는 문자들을 합쳐 
#다시 5개의 단어들로 구성된 리스트 형식의 오브젝트를 만들어보자. 
mywords2 <- list(rep(NA,5))
for (i in 1:5) {
  mywords2[i] <- paste(myletters[[i]], collapse='')
}
mywords2

#다섯 개의 단어들을 공란을 구분해서 합치면 문장이 된다. 
paste(mywords2, collapse=' ')

#위키피디아에서 R을 설명하는 첫 두 단락을 텍스트 데이터로 입력하였다. 
R_wiki <- "R is a programming language and software environment for statistical computing and graphics supported by the R Foundation for Statistical Computing. The R language is widely used among statisticians and data miners for developing statistical software and data analysis. Polls, surveys of data miners, and studies of scholarly literature databases show that R's popularity has increased substantially in recent years.
R is a GNU package. The source code for the R software environment is written primarily in C, Fortran, and R. R is freely available under the GNU General Public License, and pre-compiled binary versions are provided for various operating systems. While R has a command line interface, there are several graphical front-ends available."
#문단단위로 구분하면 아래와 같다. 
R_wiki_para <- strsplit(R_wiki,split='\n')
R_wiki_para

#문단단위를 문장단위로 다시 분해하면 다음과 같다.
R_wiki_sent <- strsplit(R_wiki_para[[1]],split='\\. ')
R_wiki_sent

#문단 문장 단어단위로 분해하면 다음과 같다. 
R_wiki_word <- list(NA,NA)
for (i in 1:2) {
  R_wiki_word[[i]] <- strsplit(R_wiki_sent[[i]],split=' ')
}
R_wiki_word
R_wiki_word[[1]][[2]][3]

#regexpr은 지정된 패턴이 처음 등장하는 텍스트의 위치를 보고한다
mysentence <- "Learning R is so interesting"
regexpr('ing',mysentence)

#시작위치는 다음과 같이 하면 된다. 
loc.begin <- as.vector(regexpr('ing',mysentence))
loc.begin
#원하는 패턴의 길이는 다음과 같이 구할 수 있다. 
loc.length <- as.vector(attr(regexpr('ing',mysentence),'match.length'))
loc.length
#종료되는 위치는 다음과 같이 구하면 된다. 
loc.end <- loc.begin+loc.length-1
loc.end

#반면 gregexpr은 텍스트 데이터에서 지정된 패턴이 등장하는 모든 텍스트의 위치를 보고한다
gregexpr('ing',mysentence)

#해당 표현이 몇 번 등장했는지는 다음과 같이 확인할 수 있다. 
length(gregexpr('ing',mysentence)[[1]])
#시작위치는 다음과 같이 하면 된다. 이 때 인덱싱에 주의하기 바란다
loc.begin <- as.vector(gregexpr('ing',mysentence)[[1]])
loc.begin
#원하는 패턴의 길이를 구할 때도 인덱싱 주의
loc.length <- as.vector(attr(gregexpr('ing',mysentence)[[1]],'match.length'))
loc.length
#종료되는 위치는 다음과 같이 구하면 된다. 
loc.end <- loc.begin+loc.length-1
loc.end

#regexec는 regexpr과 비슷해 보이지만, 동일하지는 않다. 
#일단 간단한 표현을 쓸 경우 regexec와 regexpr는 거의 동일하다.
regexpr('interesting',mysentence)

#다음과 같이 하면 보다 간단할 수도 있다. 
regexec('interestin(g)',mysentence)

#원하는 부분이 3개 이상일 경우에는 더 유용할 수 있다. 
regexec('so (interestin(g))',mysentence)

#위의 함수들은 텍스트 데이터에서 원하는 부분이 어디에서 나타나는지를 인덱싱하는 역할을 한다. 
#이 부분과 관련해서는 regmatches 함수와 substr 함수 부분에서 다시 설명하겠지만,
#조합을 어떻게 하는가에 따라 매우 유용하게 사용가능하다. 간단한 한 가지 사례만 살펴보기 바란다. 
#software라는 단어가 앞서 소개한 위키피디아의 R 소개글의 7개 문장 중 어디에서 등장하는지, 
#그리고 어떤 위치에서 등장하는지를 살펴보자. 
mysentences <- unlist(R_wiki_sent)
regexpr("software",mysentences)

#2회이상 등장여부를 확인하는 방법은 아래와 같다. 
gregexpr("software",mysentences)

#시작과 종료위치를 정리하는 방법
mytemp <- regexpr("software",mysentences)
#시작위치를 추출한 후, 해당 표현이 나오지 않은 경우 결측값으로 처리하였다. 
my.begin <- as.vector(mytemp)
my.begin[my.begin == -1] <- NA 
#종료위치를 계산하였다. 
my.end <- my.begin + as.vector(attr(mytemp,"match.length")) - 1
#시작과 종료위치를 문장의 수만큼 확정할 수 있는 행렬 데이터를 만들었다. 
mylocs <- matrix(NA,nrow=length(my.begin),ncol=2)
colnames(mylocs) <- c('begin','end')
rownames(mylocs) <- paste('sentence',1:length(my.begin),sep='.')
#반복계산을 통해 정리하였다. 
for (i in 1:length(my.begin)) {
  mylocs[i,] <- cbind(my.begin[i],my.end[i])
}
mylocs

#아래와 같이 해당 표현이 등장하는지 여부를 확인할 수 있다. 
grep('software',mysentences)
grepl('software',mysentences)

#sub함수의 경우 처음에 등장한 표현만을, gsub는 모든 표현만을 교체한다. 
sub('ing','ING',mysentence)
gsub('ing','ING',mysentence)

#고유명사를 처리하는 방법
#위키피디아의 R관련 설명들 중 첫 문장을 선택하자. 
sent1 <- R_wiki_sent[[1]][1]
#5개 단어로 이루어진 기관명을 _을 이용해 하나의 단어처럼 만들었다. 
new.sent1 <- gsub("R Foundation for Statistical Computing","R_Foundation_for_Statistical_Computing",sent1)
#문장에 포함된 단어의 수는 다음과 같이 바뀐다. 
sum(table(strsplit(sent1,split=' ')))
sum(table(strsplit(new.sent1,split=' ')))

#해당문장에서 and, by, for, the의 네 단어들을 지워보자. 
drop.sent1 <- gsub("and |by |for |the ","",new.sent1)
sum(table(strsplit(drop.sent1,split=' ')))

#regmatches함수는 텍스트 데이터에서 지정된 표현에 해당되는 부분만을 추출한다.
#앞에서 소개한 regexpr, gregexpr, regexec 함수등과 같이 사용된다. 
mypattern <- regexpr('ing',mysentence)
regmatches(mysentence,mypattern)

mypattern <- gregexpr('ing',mysentence)
regmatches(mysentence,mypattern)

#invert 옵션을 사용하면 해당 표현만을 뺀 나머지 텍스트 데이터를 확인할 수 있다. 
mypattern <- regexpr('ing',mysentence)
regmatches(mysentence,mypattern,invert=TRUE)
mypattern <- gregexpr('ing',mysentence)
regmatches(mysentence,mypattern,invert=TRUE)

#strsplit 함수를 사용하였을 경우와 유사하지만 동일하지 않다. 
strsplit(mysentence,split='ing')

#gsub 함수를 사용하였을 경우와도 유사하지만 동일하지 않다. 
gsub('ing','',mysentence)

#특정 표현이 아니라 위치를 이용한다면 substr 함수를 사용하면 된다. 
substr(mysentences,1,30)

#ing로 끝나는 모든 단어를 찾아보자.
my2sentence <- c("Learning R is so interesting","He is a fascinating singer")
mypattern0 <- gregexpr("ing",my2sentence)
regmatches(my2sentence,mypattern0)

#ing앞에 올 알파벳 표현을 확인하기 위해 [[:alpha:]]를 앞에 덧붙였다. 
mypattern1 <- gregexpr("[[:alpha:]](ing)",my2sentence)
regmatches(my2sentence,mypattern1)

#ing앞에 오는 최소 1회 이상의 알파벳 표현을 확인하는 방법은 아래와 같다. 
#아래는 다음과 동일하다: mypattern2 <- gregexpr("[[:alpha:]]{1,}(ing)",my2sentence)
mypattern2 <- gregexpr("[[:alpha:]]+(ing)",my2sentence)
regmatches(my2sentence,mypattern2)

#[[:alpha:]]+ing 로 끝나야 하기에 \\b를 덧붙였다. 
mypattern3 <- gregexpr("[[:alpha:]]+(ing)\\b",my2sentence)
regmatches(my2sentence,mypattern3)

#위키피디어에서 가져온 7개 문장에 적용해 봅시다. 
mypattern <- gregexpr('[[:alpha:]]+(ing)\\b',mysentences)
myings <- regmatches(mysentences,mypattern)
#어떤 단어들이 있는지 한번 살펴보겠습니다. 
table(unlist(myings))

#computing 과 Computing은 동일한데 다르게 인식이 되어 있다. 
#해당 텍스트 데이터를 모두 소문자로 통일한 다음에 위의 과정을 반복해 보자
mypattern <- gregexpr('[[:alpha:]]+(ing)\\b',tolower(mysentences))
myings <- regmatches(tolower(mysentences),mypattern)
#아래를 보면 문제없이 바뀐 것을 알 수 있다. 
table(unlist(myings))

#해당 텍스트에서 stat~ 이 포함된 표현을 한번 살펴보겠습니다. 
mypattern <- gregexpr('(stat)[[:alpha:]]+',tolower(mysentences))
regmatches(tolower(mysentences),mypattern)

#해당 텍스트에서 사용된 대문자와 소문자를 구해보면 다음과 같다. 
mypattern <- gregexpr('[[:upper:]]',mysentences)
my.uppers <- regmatches(mysentences,mypattern)
table(unlist(my.uppers))

mypattern <- gregexpr('[[:lower:]]',mysentences)
my.lowers <- regmatches(mysentences,mypattern)
table(unlist(my.lowers))

#toupper 함수를 통해 대소문자를 구분하지 말자. 
mypattern <- gregexpr('[[:upper:]]',toupper(mysentences))
my.alphas <- regmatches(toupper(mysentences),mypattern)
mytable <- table(unlist(my.alphas))
mytable

#가장 많이 쓰인 대문자는? 
mytable[mytable == max(mytable)]

#몇 개의 알파벳이 쓰였을까? 
length(mytable)
#총 몇 개의 알파벳이 쓰였을까? 
sum(mytable)

#다음과 같이 하면 각 알파벳의 등장빈도를 시각화할 수 있다. 
library('ggplot2')
mydata <- data.frame(mytable)
ggplot(data=mydata, aes(x=Var1, y=Freq, fill=Var1)) + 
  geom_bar(stat="identity")+guides(fill=FALSE)+
  geom_hline(aes(yintercept=median(mytable)))+
  xlab("알파벳(대문자와 소문자 구분없음)") + ylab("빈도수")

