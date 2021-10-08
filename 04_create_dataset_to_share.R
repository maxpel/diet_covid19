library(ggplot2)

library(data.table)

setDTthreads(4)

library(fst)

setwd("/home/maxpe/Documents/diet/")

liwcnames <- c("anger_liwc", "anxiety_liwc", "sadness_liwc", "posemo_liwc", "negemo_liwc", "social_liwc")

twnames <- c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'love', 'optimism', 'pessimism', 'sadness', 'surprise', 'trust')

bintwnames <- paste0(twnames,"_bin")

# groupdata_usersubset_melt <- read_fst("groupdata_usersubset_melt.fst",as.data.table = T)

groupdata_usersubset <- read_fst("groupdata_usersubset.fst",as.data.table = T)

dataset_toshare <- groupdata_usersubset[,c(list(number_tweets=.N),lapply(.SD,mean)),list(period,group,gender_script,userid),.SDcols=c(liwcnames,bintwnames)]

fwrite(dataset_toshare,file="anobp_prepostcovid_gender_liwc_tweeteval.tsv",sep="\t")
