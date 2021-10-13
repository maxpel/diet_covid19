library(ggplot2)

library(data.table)

setDTthreads(4)

library(fst)

setwd("/home/maxpe/Documents/diet/")

liwcnames <- c("anger_liwc", "anxiety_liwc", "sadness_liwc", "posemo_liwc", "negemo_liwc", "social_liwc")

twnames <- c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'love', 'optimism', 'pessimism', 'sadness', 'surprise', 'trust')

bintwnames <- paste0(twnames,"_bin")

# Subset seperately and combine subsets
# Update: possible together

# different number of columns?
# update: solved
# setdiff(colnames(as.data.table(read_feather("a.feather"))),colnames(as.data.table(read_feather("ttw_full.feather"))))

# obsolete
# c <- rbind(as.data.table(read_feather("a.feather")),as.data.table(read_feather("ttw_full.feather")))

# f <- read_fst("first_dataset.fst")
# s <- read_fst("second_dataset.fst")
# # we also have to binarise the second dataset
# setdiff(colnames(f),colnames(s))

groupdata <- rbind(read_fst("first_dataset.fst",as.data.table = T),read_fst("second_dataset.fst",as.data.table = T),read_fst("ctrl_group.fst",as.data.table = T))

# setdiff(colnames(read_fst("first_dataset.fst",as.data.table = T)),colnames(read_fst("ctrl_group.fst",as.data.table = T)))

# # define periods
# c[date >= as.Date("2020-03-15") & date <= as.Date("2021-03-15"),period:="post"]
# 
# c[date >= as.Date("2019-03-15") & date < as.Date("2020-03-15"),period:="pre"]
# 
# c[date < as.Date("2019-03-15") | date > as.Date("2021-03-15"),period:="outside"]


userperiod <- groupdata[,.N,by=list(userid,period)]

userperiod_wide <- dcast(userperiod,userid~period,value.var="N")


usersenough <- userperiod_wide[pre>=10 & post >= 10, userid]

groupdata_usersubset <- groupdata[userid %in% usersenough & period %in% c("pre","post")]

# how many users per gender and group
groupdata_usersubset[,unique(userid),list(gender_script,group)][,.N,list(gender_script,group)]

# remove those with empty names
# "" doesn't seem to be necessary, but still
groupdata_usersubset <- groupdata_usersubset[!is.na(gender_script) & gender_script!=""]

write_fst(groupdata_usersubset,"groupdata_usersubset.fst")

rm(groupdata)

gc()

# is the melt one needed? not for now, I guess

groupdata_usersubset_melt <- melt(groupdata_usersubset,id.vars=c("id","group","gender_script","period"),measure.vars=c(bintwnames,liwcnames))

write_fst(groupdata_usersubset_melt,"groupdata_usersubset_melt.fst")


# because ctrl is subsetted before, we can just rowbind it here
# solve issue with the histories first

# ttwlctrls <- read_feather("ttwlctrls.feather")
