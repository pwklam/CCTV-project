library(tidyverse)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(jiebaR)
library(sysfonts)
font_add_google("Noto Sans TC", "Noto Sans TC")
library(showtext)
showtext_auto()

kwong1<- read.csv("c:/Users/user/Documents/video001-040.csv",header= TRUE)
kwong2<- read.csv("c:/Users/user/Documents/video041-070.csv",header= TRUE)
kwong3<- read.csv("c:/Users/user/Documents/video071-100.csv",header= TRUE)

kwong2<- kwong2[,-c(4,5,6,7,8)]
kwong2

combined<- rbind(kwong1, kwong2, kwong3)

corpus_kwong_r <- corpus(combined, text_field = "Content")

tokens_kwong_r <- tokens(corpus_kwong_r,
                          remove_punct = TRUE,
                          remove_numbers = TRUE,
                          remove_url = TRUE,
                          remove_symbols = TRUE,
                          verbose = TRUE)

dfm_kwong_r <- dfm(tokens_kwong_r)

features <- topfeatures(dfm_kwong_r, 50)
data.frame(list(term = names(features), frequency = unname(features))) %>% 
  ggplot(aes(x = reorder(term,-frequency), y = frequency)) +
  geom_point() +
  theme_bw() +
  labs(x = "Term", y = "Frequency") +
  theme(axis.text.x=element_text(angle=90, hjust=1))

customstopwords <- c(stopwords_list, "與", "年", "月", "日","係","佢","冇","其實","點", "speaker", "you",
                     "嘅","個","哋","唔","啲","喺", "嚟", "咗","埋", "咁","會","一個", "為", "話",
                     "做", "樣", "嗰", "家", "中","講","度", "新", "想", "台","酒", "街", "㗎", "之後",
                     "仲", "時", "星期", "認為","邊", "過", "視", "非常", "出", "今日", "先生",
                     "事", "號", "請", "特別", "地方", "響", "聽", "將", "嘢", "咪", 
                      "買", "美", "市", "落", "公司", "青", "think", "次", "無", "來", "開", "應該",  
                      "現在", "三", "該", "know community", "乜", "時候", "幾", "俾", "中華","組", "兩個",       
                      "公所", "生", "問", "好似", "面", "繼續", "覺得", "開始", "五", "裏面", 
                      "知道", "接受", "表示", "成", "inaudible", "偉", "new", "間", "曖", "收", "康", "得到",      
                      "關係", "文", "畀", "果", "真", "幫", "入", "需要", "可能", "國", "一次",      
                      "一个", "這個", "第二", "更", "最近", "種", "陳", "同時", "學", "意見", "兩", "can",         
                      "喇", "well","食", "just", "社", "場", "區", "舉行", "件", "清", "十", "返", "一定", "搵",
                      "不過" , "普通",  "將會", "法" , "從","利", "根據", 
                     "啫", "見", "並", "飲", "架", "歡迎", "同埋", "s", "me", "t", "re", "like", "希望", "因為", "方面",
                     "能夠", "點樣", "咩", "對於", "目前", "多謝", "going", "曾經", "一直", "發生", "相信", "時間", "問題",
                     "已經", "好多", "觀眾", "英文", "know community", "李", "e", "啱", "送到 ", "o", "r", "攞", "see") 
                             
stopwords_list<- read.table(file = "c:/Users/user/Documents//stopwords.txt", header = TRUE)
                             
                             
                             
                             


dfm_kwong_r <- dfm_remove(dfm_kwong_r, c(stopwords('chinese', source = "misc"), stopwords('english'), stopwords_list, customstopwords))


topfeatures(dfm_kwong_r, 100)

textplot_wordcloud(dfm_kwong_r)

library(jiebaR)
raw_texts <- as.character(corpus_kwong_r)
tokenised_texts <- purrr::map(raw_texts, segment, tokeniser)
token_kwong_r <- tokens(corpus_kwong_r,
                     remove_punct = TRUE,
                     remove_numbers = TRUE,
                     remove_url = TRUE,
                     remove_symbols = TRUE,
                     verbose = TRUE)
token_kwong_r
dfm_kwong_r <- dfm(token_kwong_r)
dfm_kwong_r <- dfm_remove(dfm_kwong_r, c(stopwords('chinese', source = "misc"), stopwords('english'), customstopwords))

topfeatures(dfm_kwong_r, 50)
textplot_wordcloud(dfm_kwong_r)
####### 


stopwords_list <- scan("stopWords.txt", character(), quote = "")

tokeniser <- worker()
raw_texts <- as.character(corpus_kwong_r)
tokenised_texts <- purrr::map(raw_texts, segment, tokeniser)
tokens_kwong_r <- tokens(tokenised_texts,
                      remove_punct = TRUE, 
                      remove_numbers = TRUE, 
                      remove_url = TRUE,
                      remove_symbols = TRUE,
                      verbose = TRUE)
dfm_kwong_r <- dfm(tokens_kwong_r)
customstopwords <- c()
dfm_kwong_r <- dfm_remove(dfm_kwong_r, c(stopwords('chinese', source = "misc"), stopwords('english'), customstopwords))
topfeatures(dfm_kwong_r, 100)
textplot_wordcloud(dfm_kwong_r)



#######
text_token_hk <- tokens(corpus_kwong_r, remove_punct = TRUE) %>%
  tokens_select(pattern = customstopwords, selection = "remove" )

text_col_hk <- textstat_collocations(text_token_hk, size = 6, min_count = 20)
knitr::kable(top_n(text_col_hk, 40, wt = count))
