# library(R.utils)

library(data.table)

library(fst)

setwd("/home/maxpe/Documents/diet/")


Sys.setlocale("LC_ALL", 'en_GB.UTF-8')

twnames <- c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'love', 'optimism', 'pessimism', 'sadness', 'surprise', 'trust')

bintwnames <- paste0(twnames,"_bin")

liwcnames <- c("anger_liwc", "anxiety_liwc", "sadness_liwc", "posemo_liwc", "negemo_liwc", "social_liwc")


# Group History David

t <- fread("historic_tweets_1.tsv",quote="",select=c(1,2,3,17,18,20,13),col.names = c("text_raw","id","created_at","userid","name","followers","retweeted"),nrows=Inf)

t[,index:=1:nrow(t)]

# text filtering here still necessary for legacy reasons
t <- t[!grepl("^RT",text_raw)]

t[,text:= gsub("\\t|\\n","",text_raw)]

# we don't need both text columns anymore, already written them out, otherwise comment them out
t[,text_raw:=NULL]

t[,text:=NULL]

# create date objects
t[,datetime:=as.POSIXct(created_at,format="%a %b %d %H:%M:%S +0000 %Y",tz="UTC")]

t[,date:=as.Date(datetime)]

t[,created_at:=NULL]

# define periods
t[date >= as.Date("2020-03-15") & date <= as.Date("2021-03-15"),period:="post"]

t[date >= as.Date("2019-03-15") & date < as.Date("2020-03-15"),period:="pre"]

# a few retweets are still in there
# we remove them further down
# > sum(!is.na(t$retweeted))
# [1] 4979

# fwrite(t[,.(text)],file=paste0("historic_tweets_1.tsv_text"),col.names = F,quote=FALSE)
#
# fwrite(t[,.(id)],file=paste0("historic_tweets_1.tsv_id"),col.names = F,quote=FALSE)
#
# fwrite(t[,.(userid)],file=paste0("historic_tweets_1.tsv_userid"),col.names = F,quote=FALSE)
#
# fwrite(t[,.(followers)],file=paste0("historic_tweets_1.tsv_followers"),col.names = F,quote=FALSE)
#
# fwrite(t[,.(as.integer(datetime))],file=paste0("historic_tweets_1.tsv_timestamp"),col.names = F,quote=FALSE)


# read tweeteval and liwc

tw <- fread("historic_tweets_1.tsv_text_tweeteval",col.names=twnames)

tw[,(bintwnames):=lapply(.SD,function(x) as.integer(x>0.9)),.SDcols=twnames]

tw[,(twnames):=NULL]

tl <- fread("liwc/tokenized_historic_tweets_1.tsv_text_liwc",select=c("34","33","35","31","32","40","number_tokens"),col.names=c(liwcnames,"number_tokens"))

for(i in liwcnames){       
  set(tl,j =i, value = tl[[i]]/tl[["number_tokens"]])
}

# around 5 percent of tweets are empty after tokenization
# remove or crude tokenizer?
# no content remaining, cleans up the signal
# > sum(tl$number_tokens==0)/nrow(tl)
# [1] 0.04850198

ttw <- cbind(t,tw)

rm(t,tw)

first_dataset <- cbind(ttw,tl)

rm(ttw,tl)

gc()

# subset for the few remaining retweets
# and remove empty tweets (empty after tokenization)

first_dataset <- first_dataset[period == "pre"|period == "post"]

first_dataset <- first_dataset[is.na(retweeted)]

first_dataset[,retweeted:=NULL]

first_dataset <- first_dataset[number_tokens>0]

first_dataset[,number_tokens:=NULL]


# Put them in their groups

# read in file comes from
# david/david_analysis.R

groupdt <- rbind(fread("david/ano_users_min2exclusive",col.names="userid")[,group:="ano"],fread("david/bp_users_min2exclusive",col.names="userid")[,group:="bp"])

first_dataset <- merge(first_dataset,groupdt,by="userid")

# Add Gender (now script only)

g_script <- fread("unique_screenname_history1_gender",sep="\t",encoding = "UTF-8",quote="",fill=T,select=c(2,4),col.names=c("name","gender_script"))

g_script[gender_script=="f",gender_script:="female"]

g_script[gender_script=="m",gender_script:="male"]

g_script[gender_script=="u",gender_script:="unknown"]

# we lose one user here
# > g_script[gender_script=="ff"]
# name gender_script
# 1: World Association of Girl Guides and Girl Scouts            ff

g_script <- g_script[gender_script!="ff" & gender_script!=""]

first_dataset <- merge(first_dataset,g_script,by="name",all.x=T)

# Stats for first dataset
# > first_dataset[,unique(userid),gender_script][,.N,gender_script]
# gender_script    N
# 1:          <NA>    7
# 2:       unknown 9446
# 3:          male 3138
# 4:        female 6906

write_fst(first_dataset,"first_dataset.fst")

fwrite(first_dataset,"first_dataset.tsv")

fwrite(first_dataset[,.(index)],"first_dataset_index",col.names = F)

# write out text, to be subset with awk using the index file
fwrite(fread("historic_tweets_1.tsv",quote="",select=c(1),col.names = c("text_raw"),nrows=Inf),"first_dataset_fulltext",col.names=F)


# first_dataset <- read_fst("first_dataset.fst")


# Group History Yelena

# load adds = 2nd batch

a <- cbind(
  rbindlist(lapply(sprintf("history_adds/tweets%02d.tsv_timestamp",5:11),function(x) fread(x,nrows=Inf,col.names="created_at"))),
  rbindlist(lapply(sprintf("history_adds/tweets%02d.tsv_id",5:11),function(x) fread(x,nrows=Inf,col.names="id"))),
  rbindlist(lapply(sprintf("history_adds/tweets%02d.tsv_userid",5:11),function(x) fread(x,nrows=Inf,col.names="userid"))),
  rbindlist(lapply(sprintf("history_adds/tweets%02d.tsv_followers",5:11),function(x) fread(x,nrows=Inf,col.names="followers"))),
  rbindlist(lapply(sprintf("history_adds/tweets%02d.tsv_text_tweeteval",5:11),function(x) fread(x,nrows=Inf,col.names=twnames))),
  rbindlist(lapply(sprintf("history_adds/tweets%02d.tsv_name",5:11),function(x) fread(x,nrows=Inf,col.names="name",sep=NULL,header=F))),
  rbindlist(lapply(sprintf("history_adds/tweets%02d.tsv_retweeted_retweeted_boolean",5:11),function(x) fread(x,nrows=Inf,col.names="retweeted",sep=NULL,header=F)))[,retweeted:=as.logical(retweeted)],
  rbindlist(lapply(sprintf("liwc/tokenized_tweets%02d.tsv_text_liwc",5:11),function(x) fread(x,nrows=Inf,select=c("34","33","35","31","32","40","number_tokens"),col.names=c(liwcnames,"number_tokens"))))
  )

a[,index:=1:nrow(a)]

# 31	posemo (Positive Emotions)
# 32	negemo (Negative Emotions)
# 33	anx (Anx)
# 34	anger (Anger)
# 35	sad (Sad)
# 40	social (Social)

for(i in liwcnames){       
  set(a,j =i, value = a[[i]]/a[["number_tokens"]])
}

# around 5 percent of tweets are empty after tokenization
# remove or crude tokenizer?
# no content remaining, cleans up the signal
# > sum(a$number_tokens==0)/nrow(a)
# [1] 0.05589204

# Remove last remaining retweets
a <- a[retweeted==FALSE]

a[,retweeted:=NULL]

a <- a[number_tokens>0]

a[,number_tokens:=NULL]

# create date objects
a[,datetime:=as.POSIXct(created_at,origin="1970-01-01")]

a[,date:=as.Date(datetime)]

a[,created_at:=NULL]


# same periods as before
a[date >= as.Date("2020-03-15") & date <= as.Date("2021-03-15"),period:="post"]

a[date >= as.Date("2019-03-15") & date < as.Date("2020-03-15"),period:="pre"]


second_dataset <- a[period == "pre"|period == "post"]

rm(a)

gc()

# add gender from gender script (https://sites.google.com/site/yelenamejova/resources)
a_gender <- fread("history_adds/unique_names_id_gender",sep="\t",encoding = "UTF-8",quote="",fill=T,select=c(2,4),col.names=c("name","gender_script"))

a_gender[gender_script=="f",gender_script:="female"]

a_gender[gender_script=="m",gender_script:="male"]

a_gender[gender_script=="u",gender_script:="unknown"]


# We loose three users here
a_gender <- a_gender[gender_script!="ff"]


second_dataset <- merge(second_dataset,a_gender,by="name",all.x=T)

# # Some genders not detected, "ff" or "empty" because no name exists?
# # 225 users are affected
# # length(a[is.na(gender_script),unique(userid)])


# add group (ano or bp) information
agroup <- rbind(fread("david/ano_users_adds_all",col.names="userid")[,group:="ano"],fread("david/bp_users_adds_all",col.names="userid")[,group:="bp"])

# a[,c("gender_script.x","group.x","gender_script.y","group.y") := NULL]

second_dataset <- merge(second_dataset,agroup,by="userid")


second_dataset[,(bintwnames):=lapply(.SD,function(x) as.integer(x>0.9)),.SDcols=twnames]

second_dataset[,(twnames):=NULL]



write_fst(second_dataset,"second_dataset.fst")

fwrite(second_dataset,"second_dataset.tsv")


fwrite(second_dataset[,.(index)],"second_dataset_index",col.names = F)

# write out text, to be subset with awk using the index file
# TO DO! must be somewhere on nvcluster?
fwrite(rbindlist(lapply(sprintf("history_adds/tweets%02d.tsv_text",5:11),function(x) fread(x,nrows=Inf,col.names="text"))),"second_dataset_fulltext",col.names=F)



# History Control Group Second (not selecting highly active users but a random sample)
# This group is too large, do the user subset before

library(data.table)

twnames_bin <- paste0(c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'love', 'optimism', 'pessimism', 'sadness', 'surprise', 'trust'),"_bin")

liwcnames <- c("anger_liwc", "anxiety_liwc", "sadness_liwc", "posemo_liwc", "negemo_liwc", "social_liwc")


ctrl_raw <- cbind(fread("ctrl_second/diet_ctrl_second.tsv_timestamp",col.names=c("timestamp"),sep=NULL,quote=""),
      fread("ctrl_second/diet_ctrl_second.tsv_userid",col.names=c("userid"),sep=NULL,quote=""),
      fread("ctrl_second/diet_ctrl_second.tsv_id",col.names=c("id"),sep=NULL,quote=""),
      fread("ctrl_second/diet_ctrl_second.tsv_name",col.names=c("name"),sep=NULL,quote="",header=F),
      fread("ctrl_second/diet_ctrl_second.tsv_followers",col.names=c("followers"),sep=NULL,quote=""))

ctrl_raw[,index:=1:nrow(ctrl_raw)]

# 11emo binarised with awk script in folder

ctrl_retweeted <- fread("ctrl_second/diet_ctrl_second.tsv_retweeted",col.names=c("retweeted"),sep=NULL,quote="")

ctrl_retweeted[,retweeted:=as.logical(retweeted)]

ctrl_sa <- cbind(fread("ctrl_second/diet_ctrl_second.tsv_text_liwc",select=c(34,33,35,31,32,40,74),col.names=c(liwcnames,"number_tokens"),sep="\t",quote="",header=T),
fread("ctrl_second/diet_ctrl_second.tsv_text_tweeteval_bin",col.names=twnames_bin,sep="\t",quote="",header=F))

ctrl_sa <- ctrl_sa[ctrl_retweeted$retweeted]

# 31	posemo (Positive Emotions)
# 32	negemo (Negative Emotions)
# 33	anx (Anx)
# 34	anger (Anger)
# 35	sad (Sad)
# 40	social (Social)

for(i in liwcnames){       
  set(ctrl_sa,j =i, value = ctrl_sa[[i]]/ctrl_sa[["number_tokens"]])
}

# ctrl_sa[,idx:=1:nrow(ctrl_sa)]
# 
# ctrl_raw[,idx:=1:nrow(ctrl_raw)]

# ctrl_raw <- ctrl_raw[is.na(retweeted)]
# we check that before already
# in the R extraction script
# t <- t[is.na(retweeted)]
# ctrl_raw[,retweeted:=NULL]

# install.packages("fasttime")

ctrl_raw[,datetime:=as.POSIXct(timestamp, origin="1970-01-01")]

ctrl_raw[,date:=as.Date(datetime)]

ctrl_raw[,timestamp:=NULL]


# Add Gender (script only)

gctrl <- fread("ctrl_second/diet_ctrl_second.tsv_uniquenames_gender",sep="\t",encoding = "UTF-8",quote="",fill=T,select=c(2,4),col.names=c("name","gender_script"))

gctrl[gender_script=="f",gender_script:="female"]

gctrl[gender_script=="m",gender_script:="male"]

gctrl[gender_script=="u",gender_script:="unknown"]

# we lose 155 users here?
# > g_script_ctrl[gender_script!="female" & gender_script!="male" & gender_script!="unknown"]

gctrl <- gctrl[gender_script=="female" | gender_script=="male" | gender_script=="unknown"]

ctrl_raw <- merge(ctrl_raw,gctrl,by="name",all.x=T)

ctrl <- cbind(ctrl_raw,ctrl_sa)

rm(ctrl_raw,ctrl_sa,ctrl_retweeted)

gc()

ctrl <- ctrl[number_tokens>0]

ctrl[,number_tokens:=NULL]

ctrl[date >= as.Date("2020-03-15") & date <= as.Date("2021-03-15"),period:="post"]

ctrl[date >= as.Date("2019-03-15") & date < as.Date("2020-03-15"),period:="pre"]

ctrl <- ctrl[period == "pre"|period == "post"]

ctrl <- ctrl[,group:="control"]


write_fst(ctrl,"ctrl_group.fst")

fwrite(ctrl,"ctrl_group.tsv")


fwrite(ctrl[,.(index)],"ctrl_group_index",col.names = F)

# write out text, to be subset with awk using the index file
# TO DO! must be somewhere on nvcluster?
fwrite(fread("ctrl_second/diet_ctrl_second.tsv_text",col.names="text"),"ctrl_group_fulltext",col.names=F)

