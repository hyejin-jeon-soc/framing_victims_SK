library(readr)
library(stringr)
library(stm)
library(tm)
library(Cairo)
library(extrafont)
library(xlsx)
library(tidyverse)
library(tidytext)
library(quanteda)
library(igraph)
library(readr)
library(openxlsx)


########################################
Remark1 = "
  https://bookdown.org/ahn_media/bookdown-demo/anal3topic.html 참조했음
  https://cran.r-project.org/web/packages/stm/vignettes/stmVignette.pdf 참조할것
  ^ 
"

setwd("~/Python Scripts/04_sex crime") #워킹디렉토리

# data is pre-processed 
r_data <- as.data.frame(read_csv("processed_2002.csv"))

r_data$press <- as.factor(r_data$press)
r_data$date <- as.factor(r_data$dindex)

data = subset(r_data, select = c(pos, press, date, article))

# DTM, 문서-단어행렬, stem and Non-stemmed
corpus <- corpus(data$pos)
docvars(corpus, field='press') <- data$press
docvars(corpus, field='month') <- as.integer(data$date)

###
#dfm <- dfm(dfm(corpus,
#               tolower=F,
#               stem=F)) 
#계속 에러나서 코드 변경함
# Warning messages:
#  1: 'dfm.corpus()' is deprecated. Use 'tokens()' first. 
#  2: 'stem' is deprecated; use dfm_wordstem() instead 
#
###

# dfm revise
tokens <- tokens(corpus)
dfm <- dfm(tokens, tolower = FALSE)
dfm <- dfm_wordstem(dfm)
# dfm_wordstem() 사용안함, 이미 stem되어 있기 때문
stmdfm <- convert(dfm, to = "stm", docvars = docvars(corpus))

# 제거할수 있는 단어와 문서의 수를 plotRemoved()함수로 확인
plotRemoved(stmdfm$documents, lower.thresh = seq(1, 100, by = 10))

# what is the mean of lower.thresh =15 based on the code above?
out <- prepDocuments(stmdfm$documents, stmdfm$vocab, stmdfm$meta, lower.thresh = 15)

docs <- out$documents # out_result saving for further analysis 
vocabs <- out$vocab
meta <- out$meta

# Find Topic N 
## Topic 95, Seed 4392/4392L ##########

stmmodel <- stm(out$documents, out$vocab, K = 95, prevalence =~ s(month) + press, data = out$meta, init.type = "Spectral", seed = 4392L)

### K = 0
### seed = sample(1:10000, 1)
### stmmodel <- stm(out$documents, out$vocab, K = K, prevalence =~ s(month) + press, data = out$meta, init.type = "Spectral", seed = seed)
 
# K = 0 with init.type = "Spectral", automatically find optimal # of K
# STM에서 영향을 끼치는 요인 변수 넣기: prevalence =~ s(month) + press
################

save.image(file=paste0(4392L, "stm95_s4392L_HJ", 95, ".rdata"))


# 저장된 R데이터 활용하는 경우
summary(stmmodel) %>% glimpse()
summary(stmmodel)# -> topic95 확인

#short = strtrim(r_data$article, 300)
short_95 = strtrim(data$article, 300)

# summary topic 95L to excel
topicproportion = colMeans(stmmodel$theta[,seq(1, 95, 1)]) # 원 코드는 95대신 K
labels = labelTopics(stmmodel, c(1:95), 95)

tp95L_df = data.frame(matrix(nrow=101), stringsAsFactors = F)[-1] #summary to df
for(i in 1:95){
  topic = as.data.frame(
    cbind(labels$prob[i, ][1:100], 
          labels$frex[i, ][1:100],
          labels$lift[i, ][1:100],
          labels$score[i, ][1:100]),
    stringsAsFactors = F)
  names(topic) = paste(i, c('Prob', 'FREX', 'LIFT', 'Score'))
  topic = rbind(c(topicproportion[i], NA, NA, NA), topic)
  tp95L_df = cbind(tp95L_df, topic)
}

write.xlsx(tp95L_df, paste0(4392L, 'Topics_95L ', 95, '.xlsx'))
write_csv(tp95L_df[-1,], 'Topics95_4392L.csv')

# extract original text from stm topic
findThoughts(stmmodel, texts=short_95, n=30, topics=33)$docs[[1]] # find original text

# 토픽유형 1. Report Type
topicnames_RT = c('Unc/ Editorial Column', 'Unc/ Upcoming Movies', 'RT/ Provocative and Sensational',
               'RT/ Neutral Info', 'RT/ Provocative and Sensational', 'Unc/ Hollywood #MeToo',
               'RT/ Narratives', 'RT/ Neutral Info', 'Unc/ TV-Entertainment',
               'Unc/ World News', 'RT/ Provocative and Sensational', 'RT/ Neutral Info',
               'Unc/ World News(US election)', 'RT/ Narratives', 'RT/ Neutral Info',
               'Unc/ World News ', 'RT/ Narratives', 'RT/ Neutral Info',
               'RT/ Narratives', 'RT/ Neutral Info', 'RT/ Neutral Info',
               'RT/ Neutral Info', 'RT/ Provocative and Sensational', 'RT/ Provocative and Sensational',
               'Unc/ Michael Jackson ', 'RT/ Provocative and Sensational', 'RT/ Narratives',
               'Unc/ "Women-Only Transportation', 'RT/ Provocative and Sensational', 'Unc/ Announcements (Personnel)',
               'RT/ Provocative and Sensational', 'Unc/ Announcements (Events) ', 'RT/ Neutral Info',
               'RT/ Provocative and Sensational', 'RT/ Neutral Info', 'RT/ Neutral Info',
               'RT/ Neutral Info', 'RT/ Neutral Info', 'Unc/ Announcements (Ad) ',
               'RT/ Neutral Info', 'RT/ Provocative and Sensational', 'RT/ Neutral Info',
               'RT/ Neutral Info', 'RT/ Narratives', 'RT/ Provocative and Sensational',
               'RT/ Neutral Info', 'RT/ Neutral Info', 'RT/ Provocative and Sensational',
               'RT/ Narratives', 'RT/ Provocative and Sensational', 'RT/ Provocative and Sensational',
               'RT/ Neutral Info', 'RT/ Neutral Info', 'RT/ Neutral Info',
               'RT/ Neutral Info', 'RT/ Neutral Info', 'RT/ Provocative and Sensational',
               'RT/ Neutral Info', 'RT/ Provocative and Sensational', 'Unc/dipolomacy ',
               'RT/ Narratives', 'Unc/Social Movement ', 'RT/ Narratives',
               'RT/ Neutral Info', 'Unc/Policy and Politics', 'Unc/World News',
               'RT/ Neutral Info', 'RT/ Neutral Info', 'RT/ Neutral Info',
               'RT/Provocative and Sensational', 'Unc/ Announcements (Ads) ', 'RT/Provocative and Sensational',
               'Unc/ Abortion and Pregnancy ', 'Unc/ Announcements (Personnel)', 'RT/ Narratives',
               'RT/ Narratives', 'Unc/North Korea ', 'Unc/ Announcements (Ads) ',
               'RT/Provocative and Sensational', 'RT/Provocative and Sensational', 'Unc/ Election ',
               'RT/ Neutral Info', 'Unc/ Economy ', 'Unc/Editorial Column',
               'RT/Provocative and Sensational', 'RT/ Neutral Info', 'Unc/ Announcements (Ad) ',
               'RT/ Neutral Info', 'RT/ Neutral Info', 'RT/ Narratives',
               'RT/ Neutral Info', 'RT/ Narratives', 'RT/ Neutral Info',
               'RT/ Neutral Info', 'Unc/ World News')

# write topic distribution table, #(paste0(seed, 'propbydoc', K, '.csv'))
propbydoc = make.dt(stmmodel)
write.csv(propbydoc, paste0(4392L, 'propbydoc', 95, '.csv'), row.names=F) 

par("mar")
par(mar = c(5.1, 4.1, 4.1, 2.1))
par(oma = c(0, 0, 0, 0))

# size of topics in total documents

cairo_pdf(paste0(4392L, "try1_HJ", 95, ".pdf"), 
          family="Malgun Gothic", width=6, height=4.5)
customlabels = character() #Korean words from each topic
for(i in 1:95){
  customlabels = c(customlabels, labels$prob[i,][1:5] %>% paste0(collapse=', '))
}

# exclude unclassified RT
topicnums= c(3,4,5,7,8,11,12,14,15,17,
             18,19,20,21,22,23,24,26,27,29,
             31,33,34,35,36,37,38,40,41,42,
             43,44,45,46,47,48,49,50,51,52,
             53,54,55,56,57,58,59,61,63,64,
             67,68,69,70,72,75,76,79,80,82,
             85,86,88,89,90,91,92,93,94)
par(mai=c(1.02,0.82,0.82,0.42))
plot(stmmodel, type="summary", text.cex=0.7, 
     topics = topicnums,
     xlim=c(0, 0.06), main = "Topics by Reporting types", xlab = "Expected Topic Proportions",
     custom.labels = paste(topicnames_RT[topicnums])) 
#"""
# 각 토픽별 단어까지 넣고 싶으면 위 코드(plot)에 
#custom.labels = paste(topicnames_RT[topicnums], customlabels[topicnums], sep = ' : ' )추가
#"""
dev.off()

mod.out.corr <- topicCorr(stmmodel)
adjmatrix = mod.out.corr$poscor*10


