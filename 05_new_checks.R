library(data.table)

library(fst)

data=fread("anobpcontrol_prepostcovid_gender_liwc_tweeteval.tsv")

cor(data$anger_liwc,data$anger_bin)

cor(data$posemo_liwc,data$joy_bin)

cor(data$anxiety_liwc,data$fear_bin)

cor(data$anxiety_liwc,data$sadness_bin)

cor(data$social_liwc,data$anger_bin)


