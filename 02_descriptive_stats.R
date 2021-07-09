#' ---
#' title: Emodiversity Preliminary Report
#' author: Max
#' date: "`r Sys.Date()`"
#' output:
#'    pdf_document
#' ---

#' `r knitr::opts_chunk$set(echo=FALSE,warning=FALSE,message=FALSE,cache=FALSE)`



library(ggplot2)


theme_thesis <- function(){
  theme_bw() %+replace%
    theme(
      #line = element_line(colour="black"),
      #text = element_text(colour="black"),
      axis.title = element_text(size = 14),
      axis.text = element_text(colour="black", size=10),
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

library(data.table)

setDTthreads(4)

library(arrow)

setwd("/home/maxpe/Documents/diet/")

twnames <- c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'love', 'optimism', 'pessimism', 'sadness', 'surprise', 'trust')

liwcnames <- c("anger_liwc", "anxiety_liwc", "sadness_liwc", "posemo_liwc", "negemo_liwc", "social_liwc")

bintwnames <- paste0(twnames,"_bin")

cs <- read_feather("cs.feather")

cs[,(bintwnames):=lapply(.SD,function(x) as.integer(x>0.9)),.SDcols=twnames]

#' final number of users in the groups
cs[,.(gender_script=unique(gender_script),group=unique(group)),userid][,.N,list(group,gender_script)][order(group,gender_script)]

#' number of tweets per user
ggplot(cs[,.(number_tweets=.N),userid],aes(x=number_tweets)) + geom_histogram(bins=150,fill="white",colour="black") + ylab("count_users")

#' The minimum number of tweets
min(cs[,.(number_tweets=.N),userid]$number_tweets)

#' The maximum number of tweets
max(cs[,.(number_tweets=.N),userid]$number_tweets)

liwcdt1 <- cs[,lapply(.SD,mean),list(group),.SDcols=liwcnames]

liwcdt2 <- cs[,lapply(.SD,mean),list(group,period),.SDcols=liwcnames]

liwcdt3 <- cs[,lapply(.SD,mean),list(group,gender_script),.SDcols=liwcnames]

liwcdt4 <- cs[,lapply(.SD,mean),list(group,gender_script,period),.SDcols=liwcnames]


twdt1 <- cs[,lapply(.SD,mean),list(group),.SDcols=bintwnames]

twdt2 <- cs[,lapply(.SD,mean),list(group,period),.SDcols=bintwnames]

twdt3 <- cs[,lapply(.SD,mean),list(group,gender_script),.SDcols=bintwnames]

twdt4 <- cs[,lapply(.SD,mean),list(group,gender_script,period),.SDcols=bintwnames]



# rm(cs)

# plotting with ggplot2, too much for my laptop
# ggplot(csmelt,aes(y=value)) + geom_boxplot() + facet_grid(~group+variable)
# 
# ggplot(csmelt,aes(y=value)) + geom_boxplot() + facet_grid(group~variable+factor(period,levels=c("pre","post")))
# 
# ggplot(csmelt,aes(y=value)) + geom_boxplot() + facet_grid(group~variable+gender_script+factor(period,levels=c("pre","post")))

#' Emotions for both groups LIWC (all means)
#+ fig.height=12, fig.width=12
# ggplot(t_wide[variable %in% liwcnames], aes(x = group, ymin = `0%`, lower = `25%`, middle = `50%`, upper = `75%`, ymax = `100%`)) + geom_boxplot(stat = "identity") + facet_wrap(~variable,ncol = 2)
# ggplot(t_wide[variable %in% liwcnames], aes(x = group, y = `50%`)) + geom_point() + facet_wrap(~variable,ncol = 2)
#+ fig.height=12, fig.width=12
ggplot(melt(liwcdt1,id.vars = "group",measure.vars = liwcnames),aes(x=group,y=value)) + geom_col()  + facet_wrap(~variable,ncol = 2,scales = "free")

#' Emotions for both groups LIWC during the two period
#+ fig.height=12, fig.width=12
ggplot(melt(liwcdt2,id.vars = c("group","period"),measure.vars = liwcnames),aes(x=factor(interaction(group,period),levels=c("ano.pre","ano.post","bp.pre","bp.post")),y=value)) + geom_col()  + facet_wrap(~variable,ncol = 2,scales = "free")


#' Emotions for both groups LIWC per gender
#+ fig.height=12, fig.width=12
ggplot(melt(liwcdt3,id.vars = c("group","gender_script"),measure.vars = liwcnames),aes(x=factor(interaction(group,gender_script),levels=c("ano.female","ano.male","bp.female","bp.male","ano.unknown","bp.unknown")),y=value)) + geom_col()  + facet_wrap(~variable,ncol = 2,scales = "free")


#' Emotions for both groups LIWC per gender per period
#+ fig.height=12, fig.width=12
ggplot(melt(liwcdt4,id.vars = c("group","gender_script","period"),measure.vars = liwcnames),aes(x=factor(interaction(group,period,gender_script),levels=c("ano.pre.female","ano.pre.male","ano.post.female","ano.post.male","ano.pre.unknown","ano.post.unknown","bp.pre.female","bp.pre.male","bp.post.female","bp.post.male","bp.pre.unknown","bp.post.unknown")),y=value)) + geom_col()  + facet_wrap(~variable,ncol = 1,scales = "free")


#' Emotions for both groups SemEval (all means, binarized with a threshold of 0.9)
#+ fig.height=12, fig.width=12
ggplot(melt(twdt1,id.vars = "group",measure.vars = bintwnames),aes(x=group,y=value)) + geom_col()  + facet_wrap(~variable,ncol = 2,scales = "free")

#' Emotions for both groups SemEval during the two period
#+ fig.height=12, fig.width=12
ggplot(melt(twdt2,id.vars = c("group","period"),measure.vars = bintwnames),aes(x=factor(interaction(group,period),levels=c("ano.pre","ano.post","bp.pre","bp.post")),y=value)) + geom_col()  + facet_wrap(~variable,ncol = 2,scales = "free")


#' Emotions for both groups SemEval per gender
#+ fig.height=12, fig.width=12
ggplot(melt(twdt3,id.vars = c("group","gender_script"),measure.vars = bintwnames),aes(x=factor(interaction(group,gender_script),levels=c("ano.female","ano.male","bp.female","bp.male","ano.unknown","bp.unknown")),y=value)) + geom_col()  + facet_wrap(~variable,ncol = 2,scales = "free")


#' Emotions for both groups SemEval per gender per period
#+ fig.height=12, fig.width=12
ggplot(melt(twdt4,id.vars = c("group","gender_script","period"),measure.vars = bintwnames),aes(x=factor(interaction(group,period,gender_script),levels=c("ano.pre.female","ano.pre.male","ano.post.female","ano.post.male","ano.pre.unknown","ano.post.unknown","bp.pre.female","bp.pre.male","bp.post.female","bp.post.male","bp.pre.unknown","bp.post.unknown")),y=value)) + geom_col()  + facet_wrap(~variable,ncol = 1,scales = "free")



#' Comparison for overlapping categories of our two classifiers
#+ fig.height=12, fig.width=12

# cor(cs[,lapply(.SD,mean),userid,.SDcols=c("anger_liwc","anger_bin")][,.(anger_liwc,anger_bin)])
# 
# cor(cs[,lapply(.SD,mean),userid,.SDcols=c("sadness_liwc","sadness_bin")][,.(sadness_liwc,sadness_bin)])

# one of them is enough, see below
# cor(cs[,.(posemo_bin_semeval=as.integer((joy_bin == 1 | love_bin == 1 | optimism_bin == 1 | trust_bin == 1 | anticipati_bin == 1)),userid,posemo_liwc)][,lapply(.SD,mean),userid,.SDcols=c("posemo_bin_semeval","posemo_liwc")][,.(posemo_bin_semeval,posemo_liwc)])

cs[,posemo_bin_semeval:=as.integer(joy_bin == 1 | love_bin == 1 | optimism_bin == 1 | trust_bin == 1 | anticipation_bin == 1)]

# cor(cs[,.(posemo_bin_semeval=as.integer(( joy_bin == 1 )),userid,posemo_liwc)][,lapply(.SD,mean),userid,.SDcols=c("posemo_bin_semeval","posemo_liwc")][,.(posemo_bin_semeval,posemo_liwc)])


oc <- cs[,lapply(.SD,mean),userid,.SDcols=c("sadness_liwc","sadness_bin","anger_liwc","anger_bin","posemo_bin_semeval","posemo_liwc")][,.(sadness_liwc,sadness_bin,anger_liwc,anger_bin,posemo_bin_semeval,posemo_liwc)]

lower<-round(cor(oc),2)
lower[lower.tri(round(cor(oc),2), diag=TRUE)]<-""
lower<-as.data.frame(lower)
# lower

library(xtable)
#' I created a posemo category for the semval classifier, out of all these categories:
#' joy_bin == 1 | love_bin == 1 | optimism_bin == 1 | trust_bin == 1 | anticipation_bin == 1
#+ cortable, results='asis'
print(xtable(lower), type="latex",comment = FALSE)

# x is a matrix containing the data
# method : correlation method. "pearson"" or "spearman"" is supported
# removeTriangle : remove upper or lower triangle
# results :  if "html" or "latex"
# the results will be displayed in html or latex format
# corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
#                     result=c("none", "html", "latex")){
#   #Compute correlation matrix
#   require(Hmisc)
#   x <- as.matrix(x)
#   correlation_matrix<-rcorr(x, type=method[1])
#   R <- correlation_matrix$r # Matrix of correlation coeficients
#   p <- correlation_matrix$P # Matrix of p-value 
#   
#   ## Define notions for significance levels; spacing is important.
#   mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
#   
#   ## trunctuate the correlation matrix to two decimal
#   R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
#   
#   ## build a new matrix that includes the correlations with their apropriate stars
#   Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
#   diag(Rnew) <- paste(diag(R), " ", sep="")
#   rownames(Rnew) <- colnames(x)
#   colnames(Rnew) <- paste(colnames(x), "", sep="")
#   
#   ## remove upper triangle of correlation matrix
#   if(removeTriangle[1]=="upper"){
#     Rnew <- as.matrix(Rnew)
#     Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
#     Rnew <- as.data.frame(Rnew)
#   }
#   
#   ## remove lower triangle of correlation matrix
#   else if(removeTriangle[1]=="lower"){
#     Rnew <- as.matrix(Rnew)
#     Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
#     Rnew <- as.data.frame(Rnew)
#   }
#   
#   ## remove last column and return the correlation matrix
#   Rnew <- cbind(Rnew[1:length(Rnew)-1])
#   if (result[1]=="none") return(Rnew)
#   else{
#     if(result[1]=="html") print(xtable(Rnew), type="html")
#     else print(xtable(Rnew), type="latex") 
#   }
# } 
# 
# corstars(oc)

# Try at scattermatrix
# # Customize upper panel
# upper.panel<-function(x, y){
#   points(x,y, pch=19)
#   r <- round(cor(x, y), digits=2)
#   txt <- paste0("R = ", r)
#   usr <- par("usr"); on.exit(par(usr))
#   par(usr = c(0, 1, 0, 1))
#   text(0.5, 0.9, txt)
# }
# pairs(log(oc), lower.panel = NULL,
#       upper.panel = upper.panel)


# pairs(log(oc), pch = 19, lower.panel = NULL)

# ggplot(melt(twdt4,id.vars = c("group","gender_script","period"),measure.vars = bintwnames),aes(x=factor(interaction(group,period,gender_script),levels=c("ano.pre.female","ano.pre.male","ano.post.female","ano.post.male","ano.pre.unknown","ano.post.unknown","bp.pre.female","bp.pre.male","bp.post.female","bp.post.male","bp.pre.unknown","bp.post.unknown")),y=value)) + geom_col()  + facet_wrap(~variable,ncol = 1,scales = "free")




knitr::knit_exit()


# OLDER PART WITH RAW SCORES

csmelt <- read_feather("csmelt.feather")

#' Emotions for both groups Tweeteval
#+ fig.height=12, fig.width=12
t <- csmelt[,.(value=quantile(value,c(0, .25, .5, .75, 1))),by=c("variable","group")]

t <- cbind(t,data.table(name=c("0%","25%","50%","75%","100%")))

t_wide <- dcast(t,group+variable~name,value.var = "value")

ggplot(t_wide[variable %in% twnames], aes(x = group, ymin = `0%`, lower = `25%`, middle = `50%`, upper = `75%`, ymax = `100%`)) + geom_boxplot(stat = "identity") + facet_wrap(~variable,ncol = 2)



#' Adding before and after
#+ fig.height=12, fig.width=12
t_period <- csmelt[,.(value=quantile(value,c(0, .25, .5, .75, 1))),by=c("variable","group","period")]

t_period <- cbind(t_period,data.table(name=c("0%","25%","50%","75%","100%")))

t_period_wide <- dcast(t_period,group+variable+period~name,value.var = "value")

t_period_wide[,group.period:=factor(interaction(group,period),levels=c("ano.pre","ano.post","bp.pre","bp.post"))]

ggplot(t_period_wide[variable %in% twnames], aes(x = group.period, ymin = `0%`, lower = `25%`, middle = `50%`, upper = `75%`, ymax = `100%`)) + geom_boxplot(stat = "identity") + facet_wrap(~variable,ncol = 2)


#' Adding gender
#+ fig.height=12, fig.width=12
t_period_gender <- csmelt[,.(value=quantile(value,c(0, .25, .5, .75, 1))),by=c("variable","group","period","gender_script")]

t_period_gender <- cbind(t_period_gender,data.table(name=c("0%","25%","50%","75%","100%")))

t_period_gender_wide <- dcast(t_period_gender,group+variable+period+gender_script~name,value.var = "value")

t_period_gender_wide[,group.period.gender:=factor(interaction(group,period,gender_script),levels=c("ano.pre.female","ano.pre.male","ano.post.female","ano.post.male","ano.pre.unknown","ano.post.unknown","bp.pre.female","bp.pre.male","bp.post.female","bp.post.male","bp.pre.unknown","bp.post.unknown"))]

t_period_gender_wide[,period.gender:=factor(interaction(period,gender_script),levels=c("pre.female","pre.male","post.female","post.male","pre.unknown","post.unknown"))]

ggplot(t_period_gender_wide[group=="ano" & variable %in% twnames], aes(x = period.gender, ymin = `0%`, lower = `25%`, middle = `50%`, upper = `75%`, ymax = `100%`)) + geom_boxplot(stat = "identity") + facet_wrap(group~variable,ncol=2)

ggplot(t_period_gender_wide[group=="bp" & variable %in% twnames], aes(x = period.gender, ymin = `0%`, lower = `25%`, middle = `50%`, upper = `75%`, ymax = `100%`)) + geom_boxplot(stat = "identity") + facet_wrap(group~variable,ncol=2)

