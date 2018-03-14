#################################################
#제 4부 마무리###################################
#01 R을 활용한 온라인 데이터 수집(scraping) 소개#
#################################################

#selectorGadget.com에 방문하여 해당 프로그램을 설치하면 스크레이핑을 
#매우 효율적으로 할 수 있다. 
#rvest 라이브러리를 구동
library('rvest')
library('stringr')
#아래는 해당 웹페이지의 정보를 불러들여 paper2016이란 이름으로 저장한 것이다.
paper2016 <- read_html("http://www.dbpia.co.kr/Journal/ArticleDetail/NODE07079332")
#논문의 국문제목
K.title <- paper2016 %>% html_node("h3") %>% html_text()
K.title
#논문의 영문제목
E.title <- paper2016 %>% html_node(".h3_sub_scr") %>% html_text()
E.title
#논문의 저자정보 
author <- paper2016 %>% html_node(".writeInfo") %>% html_text()
author
author <- str_replace_all(author,"\r\n[[:space:]]+","")
author
#논문의 권, 호, 페이지 범위
refinfo <- paper2016 %>% html_node("dt") %>% html_text()
refinfo
journal <- str_extract(str_extract(refinfo,
          "[[:alpha:]]{1,}[[:space:]]{0,}[[:alpha:]]{0,}[[:space:]]{1}제"),
          "[[:alpha:]]{1,}[[:space:]]{0,}[[:alpha:]]{0,}")
journal
year <- str_extract(str_extract(refinfo,"[[:digit:]]{1,}년"),"[[:digit:]]{1,}")
year
volume <- str_extract(str_extract(refinfo,"제[[:digit:]]{1,}권"),"[[:digit:]]{1,}")
volume
issue <- str_extract(str_extract(refinfo,"제[[:digit:]]{1,}호"),"[[:digit:]]{1,}")
issue
page.s <- str_extract(str_extract(refinfo,"[[:digit:]]{1,}-"),"[[:digit:]]{1,}")
page.s
page.e <- str_extract(str_extract(refinfo,"-[[:digit:]]{1,}"),"[[:digit:]]{1,}")
page.e
#논문의 초록 
abstract <- paper2016 %>% html_node(".con_txt") %>% html_text()
abstract
abstract.divide <- str_split(str_replace(abstract,"^\r\n[[:space:]]+",""),"\r\n")
abstract.divide
K.abstract <- abstract.divide[[1]][2]
E.abstract <- abstract.divide[[1]][3]
#논문의 키워드 
kw0 <- paper2016 %>% html_node("#keyword_box") %>% html_text()
kw0
#불필요한 부분 삭제
kw1 <- str_replace_all(kw0,"\r\n[[:space:]]+","")
kw2 <- str_replace(kw1,"논문의 주요 키워드를 제공합니다. 키워드를 클릭하여 관련 논문을 확인해 보세요!","")
kw2
keywords <- unlist(str_split(kw2,"#"))[-1]
keywords

summary.paper2016 <- list(K.title,E.title,journal,author,year,volume,issue,page.s,page.e,K.abstract,E.abstract,keywords)
summary.paper2016

#위의 과정을 개인맞춤형 함수로 설정해 보자. 
myscraper <- function(myurl){
  K.title <- myurl %>% html_node("h3") %>% html_text()
  E.title <- myurl %>% html_node(".h3_sub_scr") %>% html_text()
  author <- myurl %>% html_node(".writeInfo") %>% html_text()
  author <- str_replace_all(author,"\r\n[[:space:]]+","")
  refinfo <- myurl %>% html_node("dt") %>% html_text()
  journal <- str_extract(str_extract(refinfo,
            "[[:alpha:]]{1,}[[:space:]]{0,}[[:alpha:]]{0,}[[:space:]]{1}제"),
            "[[:alpha:]]{1,}[[:space:]]{0}[[:alpha:]]{0,}")
  year <- str_extract(str_extract(refinfo,"[[:digit:]]{1,}년"),"[[:digit:]]{1,}")
  volume <- str_extract(str_extract(refinfo,"제[[:digit:]]{1,}권"),"[[:digit:]]{1,}")
  issue <- str_extract(str_extract(refinfo,"제[[:digit:]]{1,}호"),"[[:digit:]]{1,}")
  page.s <- str_extract(str_extract(refinfo,"[[:digit:]]{1,}-"),"[[:digit:]]{1,}")
  page.e <- str_extract(str_extract(refinfo,"-[[:digit:]]{1,}"),"[[:digit:]]{1,}")
  abstract <- myurl %>% html_node(".con_txt") %>% html_text()
  abstract.divide <- str_split(str_replace(abstract,"^\r\n[[:space:]]+",""),"\r\n")
  K.abstract <- abstract.divide[[1]][2]
  E.abstract <- abstract.divide[[1]][3]
  kw0 <- myurl %>% html_node("#keyword_box") %>% html_text()
  kw1 <- str_replace_all(kw0,"\r\n[[:space:]]+","")
  kw2 <- str_replace(kw1,"논문의 주요 키워드를 제공합니다. 키워드를 클릭하여 관련 논문을 확인해 보세요!","")
  keywords <- unlist(str_split(kw2,"#"))[-1]
  scraped.result <- list(K.title,E.title,journal,author,year,volume,issue,page.s,page.e,K.abstract,E.abstract,keywords)
  scraped.result
}
#위와 동일한 결과를 얻을 수 있다. 
myscraper(paper2016)

#또다른 저자의 논문 웹페이지를 스크레이핑해보자. 
paper2012 <- read_html("http://www.dbpia.co.kr/Journal/ArticleDetail/NODE01836753")
myscraper(paper2012)

