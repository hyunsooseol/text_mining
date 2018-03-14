##############################
#제 2부 텍스트 데이터 처리####
#06 품사(Part-Of-Speech)분석##
##############################

#POS분석을 위한 라이브러리들 구동
library ('NLP')
library ('openNLP')
library ('tm')
library ('stringr')

#예시로 R을 소개하는 위키피디아 두 문단의 텍스트에 대해 POS분석을 실시하자
R.wiki <- "R is a programming language and software environment for statistical computing and graphics supported by the R Foundation for Statistical Computing. The R language is widely used among statisticians and data miners for developing statistical software and data analysis. Polls, surveys of data miners, and studies of scholarly literature databases show that R's popularity has increased substantially in recent years.
R is a GNU package. The source code for the R software environment is written primarily in C, Fortran, and R. R is freely available under the GNU General Public License, and pre-compiled binary versions are provided for various operating systems. While R has a command line interface, there are several graphical front-ends available."
#위의 예시 텍스트를 문장단위로 주석(annotation) 처리
R.wiki.sent <- annotate(R.wiki,Maxent_Sent_Token_Annotator())
R.wiki.sent
#위의 문장단위로 주석처리된 것에 추가적으로 단어단위로 주석처리
R.wiki.word <- annotate(R.wiki,Maxent_Word_Token_Annotator(),
                        R.wiki.sent)
R.wiki.word
#각 단어별 품사분석을 실시(POS-tagging)
POStag <- annotate(R.wiki,Maxent_POS_Tag_Annotator(),R.wiki.word)
POStag
#여기서 POS-tagging 된 단어의 갯수를 구해보자. 
word.start <- 1 + length(R.wiki.sent)
word.end <- length(R.wiki.word)
all.POS.tagged <- unlist(POStag$features[word.start:word.end])
all.POS.tagged
#POS분석된 단어
table(all.POS.tagged)
sum(table(all.POS.tagged))
#문장기호는 몇 개가 사용되었나? 
my.PUNCT <- str_detect('[[:punct:]]',all.POS.tagged)
sum(my.PUNCT)
#여기서 NN이라고 표지된 단어의 개수를 구해보자. 
my.NN <- str_detect("NN",all.POS.tagged)
sum(my.NN)
#다음과 같이 표현하면 NN, NNS, NNP, NNPS로 표지된 단어 개수를 구할 수 있다. 
my.NN <- str_detect("NN{1,}",all.POS.tagged)
sum(my.NN)


#개인맞춤함수 설정
my.POStag.func <- function(mytext){
  sent.annotate <- annotate(mytext,Maxent_Sent_Token_Annotator())
  word.annotate <- annotate(mytext,Maxent_Word_Token_Annotator(),sent.annotate)
  POStag <- annotate(mytext,Maxent_POS_Tag_Annotator(),word.annotate)
  myrange <- (1 + length(sent.annotate)):length(word.annotate)
  my.POStag <- unlist(POStag$features[myrange])
  my.POStag
}  
#말뭉치 텍스트 데이터를 불러오기
my.text.location <- "D:/data/ymbaek_papers"
mypaper  <- VCorpus(DirSource(my.text.location))
#예를 들어 첫번째 논문초록의 경우 
mypaper1.POStag <- my.POStag.func(mypaper[[1]]$content)
mypaper1.POStag

#첫번째 논문초록에서 사용된 명사+대명사 개수는?
sum(str_detect("NN{1,}",mypaper1.POStag))

#전체 24개의 영문논문 초록에서 등장한 명사+대명사의 비율을 계산해 보자. 
compare.noun <- rep(NA,24)
for (i in 1:24){
  my.NN <- sum(str_detect("NN{1,}",my.POStag.func(mypaper[[i]]$content)))
  all.POS <- sum(table(my.POStag.func(mypaper[[i]]$content)))
  compare.noun[i] <- my.NN/all.POS
}
round(compare.noun,2)
#최고비율의, 최저비율의 명사+대명사 비율 논문초록은?
prop.noun <- data.frame(1:24,compare.noun)
colnames(prop.noun) <- c('abstract.no','prop.noun')
prop.noun[order(prop.noun$prop.noun),]


#한국어 텍스트의 POS분석
#다른 라이브러리에 앞서 KoNLP 라이브러리를 먼저 실행할 것!
library ('KoNLP')
library ('tm')
library ('stringr')
#말뭉치 텍스트 데이터를 불러오기
my.text.location <- "D:/data/ymbaek_논문"
mypaper  <- VCorpus(DirSource(my.text.location))
mytext <- mypaper[[19]]$content
mytext

#품사구분 9개
mypaper19.pos09 <- SimplePos09(mytext)
mypaper19.pos09

#품사구분 22개
mypaper19.pos22 <- SimplePos22(mytext)
mypaper19.pos22

#아래와 같은 방식을 사용해 22개 품사구분에서 보통명사(NC)만 추출하자.
mypaper19.pos22.pp <- mypaper19.pos22
mytextlength <- length(mypaper19.pos22)
for (i in 1:mytextlength) {
  mylocation <- regexpr(pattern ='/NC+',mypaper19.pos22[i])
  mypaper19.pos22.pp[i] <- substr(mypaper19.pos22[i], 1, mylocation[[1]][1]-1)
  mypaper19.pos22.pp[i] <- gsub("[[:alnum:]]/[[:upper:]]{1,}\\+","",mypaper19.pos22.pp[i])
}
mypaper19.pos22.pp

mypaper19.pos22.pp <- unlist(mypaper19.pos22.pp)
mypaper19.pos22.pp <- mypaper19.pos22.pp[nchar(mypaper19.pos22.pp)>0]
mypaper19.pos22.pp

sum(table(mypaper19.pos22.pp))

#전체 논문초록에서 등장한 보통명사의 수를 계산해 보자. 
#위의 과정을 개인맞춤형 함수로 설정하자. 
my.NC.func <- function(mytext) {
  myobject <- SimplePos22(mytext)
  new.myobject <- mytext
  mytextlength <- length(myobject)
  mylocation <- regexpr(pattern ='/NC+',myobject)
  for (i in 1:mytextlength) {
    mylocation <- regexpr(pattern ='/NC+',myobject[i])
    new.myobject[i] <- substr(myobject[i], 1, mylocation[[1]][1]-1)
    new.myobject[i] <- gsub("[[:alnum:]]/[[:upper:]]{1,}\\+","",new.myobject[i])
  }
  new.myobject <- unlist(new.myobject)
  new.myobject <- new.myobject[nchar(new.myobject)>0]
  new.myobject
}  

my.NC.func(mypaper[[1]]$content)
sum(table(my.NC.func(mypaper[[1]]$content)))

#전체 국문논문 초록에 대해서 적용해 보자. 
size.noun <- rep(NA,19)
for (j in 1:19){
  size.noun[j] <- sum(table(my.NC.func(mypaper[[j]]$content)))
}
size.noun

#최고빈도의, 최저빈도의 보통명사 등장수를 갖는논문초록은?
size.noun <- data.frame(1:19,size.noun)
colnames(size.noun) <- c('abstract.no','no.noun')
size.noun[order(size.noun$no.noun),]
