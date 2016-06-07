df = read.csv("/home/vitidn/mydata/repo_git/DataSmart/chapter9/PregnancyDuration.csv")
#prob. of pregenancy last more than 349 days
pnorm(349,mean = 266,sd = 9,lower.tail = FALSE)

periods = df[,"Birth.Duration"]
qs = quantile(periods,probs = c(0.25,0.75))
interquantile_range = qs[2] - qs[1]
inner_fences = qs + c(-1.5*interquantile_range,1.5*interquantile_range)
outer_fences = qs + c(-3.0*interquantile_range,3.0*interquantile_range)
boxplot(periods)
abline(h = inner_fences,col = "blue")
abline(h = outer_fences,col = "red")
length(which(periods < inner_fences[1] | periods > inner_fences[2]))
