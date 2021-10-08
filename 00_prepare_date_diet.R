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



# History Control Group
# This group is too large, do the user subset before

tctrl <- rbind(fread(paste("ctrl_history/baseline_hist_tweets_fm.tsv"),quote="",select=c(2,3,13,17,18,20),col.names = c("id","created_at","retweeted","userid","name","followers")),
               fread(paste("ctrl_history/baseline_hist_tweets_u.tsv"),quote="",select=c(2,3,13,17,18,20),col.names = c("id","created_at","retweeted","userid","name","followers")))

tctrl[,idx:=1:nrow(tctrl)]

tctrl <- tctrl[is.na(retweeted)]

tctrl[,retweeted:=NULL]

tctrl[,datetime:=as.POSIXct(created_at,format="%a %b %d %H:%M:%S +0000 %Y",tz="UTC")]

tctrl[,date:=as.Date(datetime)]

tctrl[,created_at:=NULL]


# Add Gender (script only)

g_script_ctrl <- fread("ctrl_names_gender",sep="\t",encoding = "UTF-8",quote="",fill=T,select=c(2,4),col.names=c("name","gender_script"))

g_script_ctrl[gender_script=="f",gender_script:="female"]

g_script_ctrl[gender_script=="m",gender_script:="male"]

g_script_ctrl[gender_script=="u",gender_script:="unknown"]

# we lose 155 users here?
# > g_script_ctrl[gender_script!="female" & gender_script!="male" & gender_script!="unknown"]

g_script_ctrl <- g_script_ctrl[gender_script=="female" | gender_script=="male" | gender_script=="unknown"]

tctrl <- merge(tctrl,g_script_ctrl,by="name",all.x=T)


# define periods
tctrl[date >= as.Date("2020-03-15") & date <= as.Date("2021-03-15"),period:="post"]

tctrl[date >= as.Date("2019-03-15") & date < as.Date("2020-03-15"),period:="pre"]

tctrl[date < as.Date("2019-03-15") | date > as.Date("2021-03-15"),period:="outside"]


userperiod_ctrl <- tctrl[,.N,c("userid","period")]

# update, filled with 0s
userperiod_ctrl_wide <- dcast(userperiod_ctrl,userid~period,value.var="N",fill=0)


# too strict criterion for the control group? surprising...
usersenough_ctrl_and <- userperiod_ctrl_wide[pre>=10 & post >= 10, userid]
# maybe just an or?
usersenough_ctrl_or <- userperiod_ctrl_wide[pre>=10 | post >= 10, userid]

# get account creation date

userid_createdt <- rbind(fread("ctrl_history/baseline_hist_tweets_u.tsv",select=c(17,27),quote="",col.names = c("userid","account_created_at"))[,.(account_created_at=unique(account_created_at)),userid],
          fread("ctrl_history/baseline_hist_tweets_u.tsv",select=c(17,27),quote="",col.names = c("userid","account_created_at"))[,.(account_created_at=unique(account_created_at)),userid])[,.(account_created_at=unique(account_created_at)),userid]

# unique create_dates for the accounts
# sum(userid_createdt[,.N,userid]$N>1)

userid_createdt[,account_created_at:=as.POSIXct(account_created_at,format="%a %b %d %H:%M:%S +0000 %Y",tz="UTC")]

# get those userids for the academic api (full history) that have their account created before the first period and have a maximum of 500 posts in the post period not to get absolute heavy and exhaust academic API limits
usersenough_stilltoget <- userperiod_ctrl_wide[userid %in% userid_createdt[account_created_at<"2019-03-15",userid] & pre<=10 & post >= 10 & post <=1000, .(userid,post,pre)][order(post),userid]

usersenough_stilltoget_ordering <- userperiod_ctrl_wide[userid %in% userid_createdt[account_created_at<"2019-03-15",userid] & pre<=10 & post >= 10 & post <=1000, .(userid,post,pre)][order(post)]

# to save quote, get id of the earliest tweet we have for each of these users
# how to keep order?
earliestid_usersenough_stilltoget <- tctrl[tctrl[,.(minidx=.I[which.min(date)]),userid][userid %in% usersenough_stilltoget]$minidx,.(userid,datetime=gsub(" ","T",datetime))]

# combine this with the earliest date in the observation period "2019-03-15"
fwrite(merge(earliestid_usersenough_stilltoget,usersenough_stilltoget_ordering,by="userid")[order(post),.(userid,datetime)],"ctrl_archive/userstoget",col.names = F,sep=" ")

# what's happening here? we so few users with the strict two periods requirement?
rcctrl <- tctrl[date>=as.Date("2019-03-15") & date <= as.Date("2021-03-15")][,.(userrange=as.integer(max(date)-min(date))),by=userid]

# control users only have a very short range
hist(rcctrl$userrange,breaks=100)

# maybe a number of maximum posts issue?
# probably yes:
# > length(userperiod_ctrl_wide[pre>=10, userid])
# [1] 417
# > length(userperiod_ctrl_wide[post>=10, userid])
# [1] 16555

noposts_inrange <-  tctrl[date>=as.Date("2019-03-15") & date <= as.Date("2021-03-15")][,.N,by=userid]

noposts_outrange <-  tctrl[,.N,by=userid]

hist(noposts_inrange$N,breaks=100)

hist(noposts_outrange$N,breaks=100)


tctrls <- tctrl[userid %in% usersenough_ctrl & period %in% c("pre","post")]

rm(tctrl)

twctrl <- rbind(fread("ctrl_history/baseline_hist_tweets_fm.tsv.gz_text_tweeteval",col.names=twnames),
                fread("ctrl_history/baseline_hist_tweets_u.tsv.gz_text_tweeteval",col.names=twnames))[tctrls$idx]

# lapply(1:length(twnames),function(x){
#   tctrl[,twnames[x]:=twctrl[,get(twnames[x])]]
#   # twctrl[,get(x):=NULL]
# })

# rm(twctrl)



tlctrl <- rbind(fread("liwc/tokenized_baseline_hist_tweets_fm.tsv.gz_text_liwc",select=c("34","33","35","31","32","40"),col.names=c("anger_liwc", "anxiety_liwc", "sadness_liwc", "posemo_liwc", "negemo_liwc", "social_liwc")),
             fread("liwc/tokenized_baseline_hist_tweets_fm.tsv.gz_text_liwc",select=c("34","33","35","31","32","40"),col.names=c("anger_liwc", "anxiety_liwc", "sadness_liwc", "posemo_liwc", "negemo_liwc", "social_liwc")))[tctrls$idx]


ttwlctrls <- cbind(tctrls,twctrl,tlctrl)

rm(tctrls,twctrl,tlctrl)

# add group (should be all control anyway just a check)

ctrlgroup <- rbind(fread("control_history",col.names="userid")[,group:="ctrl"],fread("control_history_unknown",col.names="userid")[,group:="ctrl"])

# a[,c("gender_script.x","group.x","gender_script.y","group.y") := NULL]

ttwlctrls <- merge(ttwlctrls,ctrlgroup,by="userid")

write_feather(ttwlctrls,"ttwlctrls.feather")


# NEW: get second batch control


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


# add gender from gender script (https://sites.google.com/site/yelenamejova/resources)
a_gender <- fread("history_adds/unique_names_id_gender",sep="\t",encoding = "UTF-8",quote="",fill=T,select=c(2,4),col.names=c("name","gender_script"))

a_gender[gender_script=="f",gender_script:="female"]

a_gender[gender_script=="m",gender_script:="male"]

a_gender[gender_script=="u",gender_script:="unknown"]


# We loose three users here
a_gender <- a_gender[gender_script!="ff"]


a <- merge(a,a_gender,by="name",all.x=T)

# # Some genders not detected, "ff" or "empty" because no name exists?
# # 225 users are affected
# # length(a[is.na(gender_script),unique(userid)])


# add group (ano or bp) information
agroup <- rbind(fread("david/ano_users_adds_all",col.names="userid")[,group:="ano"],fread("david/bp_users_adds_all",col.names="userid")[,group:="bp"])

# a[,c("gender_script.x","group.x","gender_script.y","group.y") := NULL]

a <- merge(a,agroup,by="userid")

write_feather(a,"a.feather")


# History Control Group Second (not selecting highly active users but a random sample)
# This group is too large, do the user subset before

library(data.table)

twnames <- c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'love', 'optimism', 'pessimism', 'sadness', 'surprise', 'trust')

liwcnames <- c("anger_liwc", "anxiety_liwc", "sadness_liwc", "posemo_liwc", "negemo_liwc", "social_liwc")


ctrl_raw <- cbind(fread("ctrl_second/diet_ctrl_second.tsv_timestamp",col.names=c("timestamp"),sep=NULL,quote=""),
      fread("ctrl_second/diet_ctrl_second.tsv_userid",col.names=c("userid"),sep=NULL,quote=""),
      fread("ctrl_second/diet_ctrl_second.tsv_id",col.names=c("tweetid"),sep=NULL,quote=""),
      fread("ctrl_second/diet_ctrl_second.tsv_name",col.names=c("name"),sep=NULL,quote="",header=F),
      fread("ctrl_second/diet_ctrl_second.tsv_followers",col.names=c("followers"),sep=NULL,quote=""))

# 11emo binarised with awk script in folder

ctrl_sa <- cbind(fread("ctrl_second/diet_ctrl_second.tsv_text_liwc",select=c(34,33,35,31,32,40,74),col.names=c(liwcnames,"number_tokens"),sep="\t",quote="",header=T),
fread("ctrl_second/diet_ctrl_second.tsv_text_tweeteval_bin",col.names=twnames,sep="\t",quote="",header=F))

# 31	posemo (Positive Emotions)
# 32	negemo (Negative Emotions)
# 33	anx (Anx)
# 34	anger (Anger)
# 35	sad (Sad)
# 40	social (Social)

for(i in liwcnames){       
  set(ctrl_sa,j =i, value = ctrl_sa[[i]]/ctrl_sa[["number_tokens"]])
}

ctrl_sa[,idx:=1:nrow(ctrl_sa)]

ctrl_raw[,idx:=1:nrow(ctrl_raw)]

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

write_feather(ctrl,"ctrl_group_raw.feather")
