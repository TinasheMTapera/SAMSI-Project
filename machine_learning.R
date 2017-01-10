#=====================================================================#
# training and testing the data
#=====================================================================#

model.all<-glm(Case~ Sex + cca_1 + cca_2 + cca_3 + cca_4 + cca_5 + cca_6 + cca_7 + cca_8 + cca_9 + cca_10 + cca_11 + cca_12 + cca_13 + cca_14 + cca_15 + cca_16 + cca_17 + cca_18 + cca_19 + cca_20 + cca_21 + cca_22 + cca_23 + cca_24 + cca_25 + cca_26 + cca_27 + cca_28 + cca_29 + cca_30+ cca_31+ cca_32 + cca_33 + cca_34 + cca_35 + cca_36+ cca_37 + cca_38 + cca_39 + cca_40 + cca_41 + cca_42 + cca_43+ cca_44 + cca_45 + cca_46 + cca_47 + cca_48 + cca_49 + cca_50 + cca_51 + cca_52 + cca_53 + cca_54 + cca_55 + cca_56 + cca_57 + cca_58 + cca_59 + cca_60 + cca_61 + cca_62 + cca_63 + cca_64 + cca_65 + cca_66 + cca_67 + cca_68 + cca_69 + cca_70 +cca_71 + cca_72+ cca_73+ cca_74 + cca_75+ cca_76 +cca_77 + cca_78+ cca_79 +cca_80 +cca_81 + cca_82+ cca_83 + cca_84+ cca_85+ cca_86 +cca_87+cca_88 +cca_89 + cca_90 +cca_91 +cca_92 + cca_93, data=train, family="binomial", control = list(maxit = 1000))

summary(model.all)
aic1 = model.all$aic
all.predict = predict.glm(model.all, newdata = test, type='response')
all.predict
table(all.predict>0.5, test$Case)

#               0  1
#  FALSE  2 10
#  TRUE   4 12

###############################
#ITERATE#

sum = NULL
num = NULL
avg = NULL

for (i in 1:100)
{
# 1. randomly select 5% of data as test data, the rest is training data
n = c(sample(1:141,size = 134))

train = fv[n,]
test = fv[-n,]

# 2.model fit with training data

model.all<-glm(Case~ Sex + cca_1 + cca_2 + cca_3 + cca_4 + cca_5 + cca_6 + cca_7 + cca_8 + cca_9 + cca_10 + cca_11 + cca_12 + cca_13 + cca_14 + cca_15 + cca_16 + cca_17 + cca_18 + cca_19 + cca_20 + cca_21 + cca_22 + cca_23 + cca_24 + cca_25 + cca_26 + cca_27 + cca_28 + cca_29 + cca_30+ cca_31+ cca_32 + cca_33 + cca_34 + cca_35 + cca_36+ cca_37 + cca_38 + cca_39 + cca_40 + cca_41 + cca_42 + cca_43+ cca_44 + cca_45 + cca_46 + cca_47 + cca_48 + cca_49 + cca_50 + cca_51 + cca_52 + cca_53 + cca_54 + cca_55 + cca_56 + cca_57 + cca_58 + cca_59 + cca_60 + cca_61 + cca_62 + cca_63 + cca_64 + cca_65 + cca_66 + cca_67 + cca_68 + cca_69 + cca_70 +cca_71 + cca_72+ cca_73+ cca_74 + cca_75+ cca_76 +cca_77 + cca_78+ cca_79 +cca_80 +cca_81 + cca_82+ cca_83 + cca_84+ cca_85+ cca_86 +cca_87+cca_88 +cca_89 + cca_90 +cca_91 +cca_92 + cca_93, data=train, family="binomial", control = list(maxit = 1000))

# 3. do test data with the trained model

all.predict = predict.glm(model.all, newdata = test, type='response')

# 4. evaluate the prediction

sum[i] = sum(table(all.predict>0.5, test$Case)[4],table(all.predict>0.5, test$Case)[1])

num[i] = table(all.predict>0.5, test$Case)[1]+table(all.predict>0.5, test$Case)[2]+table(all.predict>0.5, test$Case)[3]+table(all.predict>0.5, test$Case)[4]

avg[i] = (sum[i]/num[i])*100

}

# mean(predicton rate)

avgscore1 = mean(avg, na.rm = TRUE)
# 61.11111 %


#=============================================================================#

# three section model

# m2<-read.table("split3.all.adj.csv", header=T, sep=",")

attach(m2)

m2.train = m2[which(m2$ID %in% train.ids),]
m2.test = m2[which(m2$ID %in% test.ids),]

str(m2.train)
str(m2.test)

main.model.three.split<-glm(Case~ Sex + cca_1 + cca_2 + cca_3, data=m2, family=binomial(link="logit"))
summary(main.model.three.split)
aic2 = main.model.three.split$aic


model.three.split<-glm(Case~ Sex + cca_1 + cca_2 + cca_3, data=m2.train, family=binomial(link="logit"))
summary(model.three.split)
threesplit.predict = predict.glm(model.three.split, newdata = m2.test, type='response')
threesplit.predict
table(threesplit.predict>0.5, m2.test$Case)
 
#                 0     1
# FALSE 109 126
# TRUE   77 556

#ITERATE

sum = NULL
num = NULL
avg = NULL
for (i in 1:100)
{
# 1. randomly select 5% of data as test data, the rest is training data
n = c(sample(1:141,size = 134))

train = fv[n,]
test = fv[-n,]

# 2.model fit with training data

model.three.split<-glm(Case~ Sex + cca_1 + cca_2 + cca_3, data=m2.train, family=binomial(link="logit"))
summary(model.three.split)

# 3. do test data with the trained model

threesplit.predict = predict.glm(model.three.split, newdata = m2.test, type='response')

# 4. evaluate the prediction

sum[i] = sum(table(threesplit.predict>0.5, m2.test$Case)[4],table(threesplit.predict>0.5, m2.test$Case)[1])

num[i] = table(threesplit.predict>0.5, m2.test$Case)[1]+table(threesplit.predict>0.5, m2.test$Case)[2]+table(threesplit.predict>0.5, m2.test$Case)[3]+table(threesplit.predict>0.5, m2.test$Case)[4]

avg[i] = (sum[i]/num[i])*100

}

# mean(predicton rate)

avgscore2 = mean(avg, na.rm = TRUE)
# 77.88%
#==============================================================================#

every10.model <- glm(Case~Sex + cca_3 + cca_13 + cca_23 + cca_33 + cca_43 + cca_53 + cca_63 + cca_73 + cca_83 + cca_93, data=fv, family=binomial(link="logit"), control=list(maxit=1000))
summary(every10.model)
every.10.aic = every10.model$aic

e10.predict = predict.glm(every10.model, newdata = test, type='response')
e10.predict
table(all.predict>0.5, test$Case)

#ITERATE
sum = NULL
num = NULL
avg = NULL
for (i in 1:100)
{
# 1. randomly select 5% of data as test data, the rest is training data
n = c(sample(1:141,size = 134))

train = fv[n,]
test = fv[-n,]

# 2.model fit with training data

every10.model<-glm(Case~Sex + cca_3 + cca_13 + cca_23 + cca_33 + cca_43 + cca_53 + cca_63 + cca_73 + cca_83 + cca_93, data=train, family=binomial(link="logit"), control=list(maxit=1000))

# 3. do test data with the trained model

e10.predict = predict.glm(every10.model, newdata = test, type='response')

# 4. evaluate the prediction

sum[i] = sum(table(e10.predict>0.5, test$Case)[4],table(e10.predict>0.5, test$Case)[1])

num[i] = table(e10.predict>0.5, test$Case)[1]+table(e10.predict>0.5, test$Case)[2]+table(e10.predict>0.5, test$Case)[3]+table(e10.predict>0.5, test$Case)[4]

avg[i] = (sum[i]/num[i])*100

}

# mean(predicton rate)

avgscore3 = mean(avg, na.rm = TRUE)
# 73.5
#=================================================================#

# functional model

#================================================================#

x= DTI$cca[DTI$visit==1, ]       
 ind= rowSums(is.na(x)) > 0 

x=x[!(ind),] 
y=DTI$case[DTI$visit==1 & !rowSums(is.na(DTI$cca))]

# fit= fgam(y~lf(x), family="binomial") 
# plot(fit, xlab="FA")
# summary(fit)
# abline(h=0,col='red')


#================================================================#

attach(DTI)

x = as.data.frame(DTI$cca[DTI$visit==1,])
ind = complete.cases(x)
x = x[ind,]
x = as.matrix(x)
y = DTI$case[which(DTI$visit == 1)]
y = y[ind]

xind = attributes

# full.fun.model= fgam(y~af(x, splinepars=list(k=c(8,8) ,m=list(c(2,3), c(2,3)))), gamma=1.2, family="binomial")

full.fun.model = pffr(y~ff(x), yind, data = NULL, ydata = NULL, algorithm = NA,
  method = "REML", tensortype = c("ti", "t2"), bs.yindex = list(bs = "ps",
  k = 5, m = c(2, 1)), bs.int = list(bs = "ps", k = 20, m = c(2, 1)), ...)
fun.model
# > full.fun.model

# Family: binomial 
# Link function: logit 

# Formula:
# y ~ +te(x = x.omat, z = x.tmat, by = L.x, k = c(8, 8), bs = "ps", 
    # m = list(c(2, 3), c(2, 3)))

# Estimated degrees of freedom:
# 6.58  total = 7.58 

# UBRE score: -0.02929197     rank: 62/64
    

#==========================================#
# fitting and testing a model
#==========================================#

attach(DTI)

x = as.data.frame(DTI$cca[DTI$visit==1,])
ind = complete.cases(x)
x = x[ind,]
x = as.matrix(x)
y = DTI$case[which(DTI$visit == 1)]
y = y[ind]

n = c(sample(length(y), size = 127))

ttmat = cbind(y,as.data.frame(x))


train.x = as.matrix(ttmat[n,-1])
train.y = as.matrix(ttmat[n,1])
test.x = as.matrix(ttmat[-n,-1])
test.y = as.matrix(ttmat[-n,1])


# fun.model= fgam(train.y~lf(train.x), gamma=1.2, family="binomial")

fun.model= pfr(y~af(x, argvals = x, basistype = c("s"), s(c(x))), gamma=1.2, family="binomial")

predict(fun.model, newdata = test.x, type = "response")

#=====================================#
