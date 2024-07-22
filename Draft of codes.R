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


###topic modeling####

install.packages("topicmodels")
install.packages("quanteda")
install.packages("tidyverse")
install.packages("tidytext")
install.packages("lubridate")
install.packages("sysfonts")
install.packages("showtext")
install.packages("jiebaR")
install.packages("servr")
install.packages("ldatuning")
install.packages("doParallel")
install.packages("reshape2")
install.packages("caret")


## Creating DFMs  

library(topicmodels)
library(quanteda)
library(tidyverse)
library(lubridate)
library(sysfonts)
font_add_google("Noto Sans TC", "Noto Sans TC")
library(showtext)
showtext_auto()
library(jiebaR)
library(tidytext)

docvars(dfm_kwong_r, "docname") <- docnames(dfm_kwong_r)
dfm_trimmed <- dfm_trim(dfm_kwong_r, min_docfreq = 5, min_count = 10)
dfm_trimmed

row_sum <- apply(dfm_trimmed , 1, sum)
dfm_trimmed <- dfm_trimmed[row_sum> 0, ]

lda_data <- convert(dfm_trimmed, to = "topicmodels")
lda_data

install.packages("ldatuning")

library(ldatuning)

ldatuning.result <- FindTopicsNumber(
  lda_data,
  topics = seq(from = 10, to = 50, by = 10),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010","Deveaud2014"), # There are 4 possible metrics: Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"
  method = "Gibbs",
  control = list(seed = 4321),
  verbose = TRUE
)
FindTopicsNumber_plot(ldatuning.result)


install.packages("doParallel")

library(doParallel)
library(dplyr)
library(reshape2)
library(tidyr)
library(ggplot2)



cluster <- makeCluster(detectCores(logical = TRUE)-1, outfile = "Log.txt")
registerDoParallel(cluster)
clusterEvalQ(cluster, {
  library(topicmodels)
})


n <- nrow(lda_data)
burnin <- 1000
iter <- 1000
keep <- 50
folds <- 5
splitfolds <- sample(1:folds, n, replace = TRUE)
candidate_k <- c(10, 20, 30, 40, 50)


clusterExport(cluster, c("lda_data", "burnin", "iter", "keep", "splitfolds", "folds", "candidate_k"))
                 
system.time({
  results <- foreach(j = 1:length(candidate_k), .combine = rbind) %dopar%{
    k <- candidate_k[j]
    results_1k <- matrix(0, nrow = folds, ncol = 2)
    colnames(results_1k) <- c("k", "perplexity")
    for(i in 1:folds){
      train_set <- lda_data[splitfolds != i , ]
      valid_set <- lda_data[splitfolds == i, ]
      
      fitted <- LDA(train_set, k = k, method = "Gibbs",
                    control = list(burnin = burnin, iter = iter, keep = keep) )
      results_1k[i,] <- c(k, perplexity(fitted, newdata = valid_set))
    }
    print(k)
    return(results_1k)
  }
})
stopCluster(cluster)

results_df <- as.data.frame(results)
results_df$istest <- "test"
avg_perplexity <- results_df %>% group_by(k) %>% summarise(perplexity = mean(perplexity))
avg_perplexity$istest <- "avg"
plot_df <- rbind(results_df, avg_perplexity)

ggplot(plot_df, aes(x = k, y = perplexity, group = istest)) +
  geom_point(aes(colour = factor(istest))) +
  geom_line(data = subset(plot_df, istest %in% "avg"), color = "red") +
  ggtitle("5-fold Cross-validation of Topic Modelling") +
  labs(x = "Candidate k", y = "Perplexity") +
  scale_x_discrete(limits = candidate_k) +
  scale_color_discrete(name="Test\\Average",
                       breaks=c("test", "avg"),
                       labels=c("Test", "Average")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

lda_model <- LDA(lda_data, 20, method="Gibbs")  

get_terms(lda_model, k=20)

install.packages("tidytext")
library(tidytext)
library(tidyr)

topics <- tidy(lda_model, matrix = "beta")

topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>% 
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

install.packages("LDAvis")

library(topicmodels)
library(dplyr)
library(stringi)
library(quanteda)
library(LDAvis)

visdfm <- dfm_subset(dfm_trimmed, docname %in% rownames(lda_data))



topicmodels_json_ldavis <- function(fitted, dfm, dtm){
  # Find required quantities
  phi <- posterior(fitted)$terms %>% as.matrix
  theta <- posterior(fitted)$topics %>% as.matrix
  vocab <- colnames(phi)
  doc_length <- ntoken(dfm)
  
  temp_frequency <- as.matrix(dtm)
  freq_matrix <- data.frame(ST = colnames(temp_frequency),
                            Freq = colSums(temp_frequency))
  rm(temp_frequency)
  # Convert to json
  json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                 vocab = vocab,
                                 doc.length = doc_length,
                                 term.frequency = freq_matrix$Freq)
  return(json_lda)
}

json_lda <- topicmodels_json_ldavis(lda_model, visdfm, lda_data)

serVis(json_lda, out.dir = "LDAvis", open.browser = TRUE)


doc_gamma <- tidy(lda_model, matrix = "gamma")


doc_gamma %>%
  filter(document %in% c("text1","text2","text3","text4","text5","text6")) %>% 
  ggplot(aes(factor(topic), gamma, fill = factor(topic))) +
  geom_col() +
  facet_wrap(~ document) +
  labs(x = "topic", y = expression(gamma))



topic_df <- docvars(corpus_kwong_r)
topic_df$doc_name <- docnames(corpus_kwong_r)
topic_df$text <- as.character(corpus_kwong_r)

lda_df <- data.frame(topic = get_topics(lda_model), 
                     doc_name = lda_model@documents)
topic_df <- left_join(topic_df, lda_df, by = "doc_name")
topic_df$date <- as.Date(topic_df$Post.Created.Date)
topic_df$topic <- as.factor(topic_df$topic)


install.packages("oolong")
library(oolong)
oolong_test <- wi(lda_model)
oolong_test$do_word_intrusion_test()
oolong_test$lock(force = TRUE)
oolong_test

table(topic_df$topic)
topic_df %>% 
  group_by(topic) %>% 
  count() %>% 
  ggplot(aes(topic, n, fill = topic)) +
  geom_col() +
  coord_flip()


topic_df %>% 
  mutate(date = floor_date(date, "week")) %>% 
  group_by(date, topic) %>% 
  count() %>% 
  ggplot(aes(date, n, color = topic)) +
  geom_line() +
  facet_wrap(vars(topic))

install.packages("igraph")
install.packages("ggnewscale")
install.packages("ggnetwork")
install.packages("RColorBrewer")

library(igraph)
library(ggnewscale)
library(ggnetwork)
library(RColorBrewer)

beta_matrix <- lda_model@beta
beta_matrix <- t(beta_matrix)

cor_matrix <- cor(beta_matrix, method = "pearson")
colnames(cor_matrix) <- 1:10
rownames(cor_matrix) <- 1:10
diag(cor_matrix) <- 0
quantiles <- quantile(as.vector(cor_matrix), c(.8, .9, .95, .99))


cor_network <- graph_from_adjacency_matrix(cor_matrix, mode = "upper", weighted = TRUE)
cor_network <- igraph::delete.edges(cor_network, which(E(cor_network)$weight <= quantiles[1]))
V(cor_network)$degree <- igraph::degree(cor_network)


top99 <- function(x) { x[ x$weight >= quantiles[4], ] }
top95 <- function(x) { x[ x$weight >= quantiles[3] & x$weight < quantiles[4], ] }
top90 <- function(x) { x[ x$weight >= quantiles[2] & x$weight < quantiles[3], ] }
top80 <- function(x) { x[ x$weight < quantiles[2], ] }



ggplot(ggnetwork(cor_network), aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(aes(size = 0.5),  color = brewer.pal(n = 5, name = "Greys")[2], 
             curvature = 0.15, alpha = 0.4, show.legend = FALSE,
             data = top80) +
  geom_edges(aes(size = 0.75),  color = brewer.pal(n = 5, name = "Greys")[3], 
             curvature = 0.15, alpha = 0.4, show.legend = FALSE,
             data = top90) +
  geom_edges(aes(size = 1), color = "brown4",
             curvature = 0.15, alpha = 0.4, show.legend = FALSE,
             data = top95) +
  geom_edges(aes(size = 1), color = "brown2",
             curvature = 0.15, alpha = 0.7, show.legend = FALSE,
             data = top99) +
  new_scale_color() +
  geom_nodes(aes(x, y, size = (degree + 1)), alpha = 0.9)+
  scale_size_area("degree", max_size = 20) +
  geom_nodelabel_repel(aes(label = name), size = 6, alpha = 0.8, segment.size = 5) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.text=element_text(size=16),
        legend.title=element_text(size=16),
        legend.position = "bottom",
        legend.background = element_rect(fill="white", size=0.5, linetype="solid")) +
  guides(size=FALSE, alpha=FALSE, fill=FALSE)

