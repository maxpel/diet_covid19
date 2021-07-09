library(data.table)

library(boot)

# bootstrap

# anxiety bp pre/post does not change




# permutation test
# empirical mean difference

diffdt <- cs[,lapply(.SD,mean),group,.SDcols=c(bintwnames,liwcnames)]

higherano <- (diffdt[2,-1]-diffdt[1,-1])


diff2 = function(d1,i){
  # d = d1;
  # d$group <- d$group[i];  # randomly re-assign groups
  d$group <- d1[i,group]
  m <- d[,.(mvar=mean(variable)),by=group]
  return(m[2,-1]-m[1,-1])
}

set.seed(1234)
b4 = boot(data = cs[,.(group,variable=sadness)], statistic = diff2, R = 10000, ncpus=4)
mean(abs(b4$t) > abs(b4$t0))


t <- cs[,.(group,variable=sadness)]
t.test(t[group=="bp",variable], t[group=="ano",variable], alternative = "two.sided", var.equal = FALSE)

# this is not significant, as expected
t.test(cs[,.SD,list(group,period),.SDcols=liwcnames][group=="bp" & period=="pre"]$anxiety, cs[,.SD,list(group,period),.SDcols=liwcnames][group=="bp" & period=="post"]$anxiety, alternative = "two.sided", var.equal = FALSE)


# example code here
# https://stats.stackexchange.com/questions/20701/computing-p-value-using-bootstrap-with-r

# diff2 = function(d1,i){
#   d = d1;
#   d$group <- d$group[i];  # randomly re-assign groups
#   Mean= tapply(X=d$time, INDEX=d$group, mean)
#   Diff = Mean[1]-Mean[2]
#   Diff
# }
# 
# set.seed(1234)
# b4 = boot(data = sleep, statistic = diff2, R = 5000)
# mean(abs(b4$t) > abs(b4$t0))