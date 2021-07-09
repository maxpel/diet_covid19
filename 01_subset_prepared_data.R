library(ggplot2)

library(data.table)

setDTthreads(4)

library(arrow)

setwd("/home/maxpe/Documents/diet/")

liwcnames <- c("anger_liwc", "anxiety_liwc", "sadness_liwc", "posemo_liwc", "negemo_liwc", "social_liwc")

twnames <- c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'love', 'optimism', 'pessimism', 'sadness', 'surprise', 'trust')

# Subset seperately and combine subsets
# Update: possible together

# different number of columns?
# update: solved
# setdiff(colnames(as.data.table(read_feather("a.feather"))),colnames(as.data.table(read_feather("ttw_full.feather"))))

c <- rbind(as.data.table(read_feather("a.feather")),as.data.table(read_feather("ttw_full.feather")))

# define periods
c[date >= as.Date("2020-03-15") & date <= as.Date("2021-03-15"),period:="post"]

c[date >= as.Date("2019-03-15") & date < as.Date("2020-03-15"),period:="pre"]

c[date < as.Date("2019-03-15") | date > as.Date("2021-03-15"),period:="outside"]


userperiod <- c[,.N,c("userid","period")]

userperiod_wide <- dcast(userperiod,userid~period,value.var="N")


usersenough <- userperiod_wide[pre>=10 & post >= 10, userid]

cs <- c[userid %in% usersenough & period %in% c("pre","post")]

rm(c)

# remove those with empty names
cs <- cs[!is.na(gender_script) & gender_script!=""]

write_feather(cs,"cs.feather")

csmelt <- melt(cs,id.vars=c("id","group","gender_script","period"),measure.vars=c(twnames,liwcnames))

write_feather(csmelt,"csmelt.feather")


# because ctrl is subsetted before, we can just rowbind it here
# solve issue with the histories first

# ttwlctrls <- read_feather("ttwlctrls.feather")
