library(data.table)

library(fst)

library(ggplot2)


theme_thesis <- function(){
  theme_bw() %+replace%
    theme(
      #line = element_line(colour="black"),
      #text = element_text(colour="black"),
      # axis.title = element_text(size = 14),
      # axis.text = element_text(colour="black", size=10),
      #strip.text = element_text(size=12),
      # legend.key=element_rect(colour=NA, fill =NA),
      panel.grid.major = element_line(colour = "grey90"),
      panel.grid.minor = element_line(colour="grey90"),
      # panel.border = element_rect(fill = NA, colour = "black", size=1),
      panel.background = element_rect(fill = "white"),
      strip.background=element_rect(fill="white")#,
      #legend.title=element_blank()
      # legend.position="none"
    )
}

theme_set(theme_thesis())


setwd("/home/maxpe/Documents/diet/")

data=fread("anobpcontrol_prepostcovid_gender_liwc_tweeteval.tsv")

cor(data$anger_liwc,data$anger_bin)

cor(data$posemo_liwc,data$joy_bin)

cor(data$anxiety_liwc,data$fear_bin)

cor(data$anxiety_liwc,data$sadness_bin)

cor(data$social_liwc,data$anger_bin)

# library(ggplot2)

pldata=fread("anobpcontrol_prepostcovid_gender_liwc_tweeteval.tsv")

liwcnames <- c("anger_liwc", "anxiety_liwc", "sadness_liwc", "posemo_liwc", "negemo_liwc", "social_liwc")

twnames <- c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'love', 'optimism', 'pessimism', 'sadness', 'surprise', 'trust')

for (i in liwcnames){
  print(ggplot(data=pldata,aes_string(y=i,x="period")) + geom_bar(stat = "summary",fun = "mean") + facet_wrap(~group))
}

for (i in paste0(twnames,"_bin")){
  print(ggplot(data=pldata,aes_string(y=i,x="period")) + geom_bar(stat = "summary",fun = "mean") + facet_wrap(~group))
}

thesispl <- ggplot(data=pldata,aes(y=anger_bin,x=period)) + geom_bar(stat = "summary",fun = "mean") + facet_wrap(~group) + xlab("COVID-19 Period") + ylab("Anger")

ggsave(file="/home/maxpe/Documents/thesis/graphics/diet_anger.pdf",plot=thesispl)
