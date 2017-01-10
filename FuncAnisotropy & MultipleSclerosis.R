#===================#

# This script demonstrates exploratory data mining and statistical modeling on the REFUND Diffusion Tensor Imaging data; an introduction to Fractional Anisotropy, axon tract profiles, and their relation to Multiple Sclerosis, as part of the SAMSI Undergrad Workshop Group Project

#===================#

#===================#

# Getting the data ready and exploring

#===================#
library(refund) # package with data enclosed

library(help=refund) #documentation for the package
data(DTI)
names(DTI)
hist(DTI$pasat)

summary(DTI) 

#===================#

# the variable CCA contains all of the tensor imaging measurements of concern in one list-like object. This is a slightly irregular way to store the data, so we will pull it out and reattach it to the demographic variables

#===================#

cca = data.frame(DTI[,8])
data = cbind(DTI[,1:7],cca)
str(cca)

#further exploration
summary(data)


# checking the number of visits by reverse sorting
attach(data)
sorted = data[order(-visit, ID),]
head(sorted) # there are participants with multiple measurements (visits). This is longitudinal data

#documentation: visit.time = Numeric vector of the subject-specific visit time, measured in days since first vis
 
#===================#

# Checking the distribution of the data points using plots

#===================#

#using qqnorm to check for normality in the variables

j = 1

for(i in 10:length(names(data[,-1]))){
	qqnorm(data[,i])
	qqline(data[,i], col = 'red')
	cat(c("Number of variables remaining: "))
	cat(length(names(data[,-1]))-i)
	readline(prompt = "Hold Enter or press Esc to exit.")
} #hold enter to go to the next variables

#number of unique ID numbers
length(unique(data$ID)) #142 people

# first visit patients because we don't want to deal with longitudinal data
first.visit = data[which(data$visit == 1),]
head(first.visit)
str(first.visit) #142 first visits
table(first.visit$case) #I changed the variable name from data.one.visit ---> first.visit

# there are 42 control and 100 Multiple Sclerosis (MS) Cases 

#checking for and removing NAs
table(is.na(first.visit)) # table summarises, is.na is a logical operator T/F

#========================#
#fv=first.visit[which(complete.cases(first.visit)==TRUE),] #shortened the object name from first.visit to fv. Complete cases filters for data that has no NAs

#this removed the 0's from the cases. Not what we want to happen
#========================#
fv = first.visit
str(fv) #142 participants and 100 variables including demographics

#=================================#
#plotting the data
#=================================#

 library(ggplot2)

#transposing the data frame so that each column is a participant and each row is a position

transposed = t(fv[,8:100])
transposed = as.data.frame(transposed)
str(transposed)

# adding a variable that specifies the position of the cca reading as an index

cca_position = as.vector(1:93) # a vector with the positions

plotting.data = cbind(transposed,cca_position)
head(plotting.data) #142 columns + position index column of 93 positions

#have to rename column names as they start with numbers

names = make.names(as.character(colnames(plotting.data)))
colnames(plotting.data) = names 
	
# plotting the first participant (column) against position
ggplot(data = plotting.data, aes(y = plotting.data$X2001_1, x = plotting.data$cca_position))+
geom_point()+
geom_point(y = plotting.data[,2])

#=================================#
#plotting the data 2
#=================================#

# in ggplot2 its best to adjust the data so that it is long, rather than wide, and have one response variable

df = as.vector(NULL)

for(i in 1:length(colnames(plotting.data))){
	df = c(df,plotting.data[,i])
} 
# I now have a list with all of the cca values in order by participant, a total of 13299 values (the last 13207:13299 are repeats of the cca values, can be ignored for now). I now have to add variables as extra columns with factors that will give the data meaning

fv$ID #going back to the fv dataframe)

IDs = as.vector(NULL) 

for(i in 1:143){
	temp = rep.int(fv[i,1], times = 93)
	IDs = c(IDs, temp)
}

length(IDs)
length(df) # I now have IDs for each of the cca measures. Now do the same for other variables

case = as.vector(NULL) 

for(i in 1:143){
	temp = rep.int(fv[i,5], times = 93)
	case = c(case, temp)
}

sex = as.vector(NULL) 

for(i in 1:143){
	temp = rep.int(fv[i,6], times = 93)
	sex = c(sex, temp)
}

pasat = as.vector(NULL) 

for(i in 1:143){
	temp = rep.int(fv[i,7], times = 93)
	pasat = c(pasat, temp)
}

#I now have cca measures as one long vector, with 93 measures for each person, and the vectors ID, case, sex, and pasat each as one long vector with 93 repeats each. Now we combine these

plotting.data = cbind(IDs, case, sex, pasat, rep.int(c(1:93), times = 143), df)

colnames(plotting.data) = c("ID", "Case", "Sex", "PASAT", "Position", "CCA")

plotting.data = as.data.frame(plotting.data[-c(13207:13299),]) #deleting the rows I made by mistake

#recoding the case so that ggplot can categorise it

tempcase = plotting.data$Case

for(i in 1:length(tempcase)){
	
	if(tempcase[i] < 1){
		tempcase[i] = 2
	}else{
		tempcase[i] = 1
	}
}

plotting.data$Case = tempcase

colnames(plotting.data) = c("ID", "Case", "Sex", "PASAT", "Position", "CCA")
str(plotting.data)

attach(plotting.data)

palette(rainbow(3, start = 2, alpha = 0.5))

p1 = ggplot(data = plotting.data, aes(y = CCA, x = Position, group = factor(ID)))+
geom_line(alpha = 0.4, aes(colour = factor(Case)))+
guides(colour = guide_legend(title = "Case", override.aes = list(alpha = 1)))+
ggtitle("All Participants")

p2 = ggplot(data = plotting.data[which(Case == 1),], aes(y = CCA, x = Position, group = factor(ID)))+
geom_line(alpha = 0.8)+
ggtitle("Multiple Sclerosis Cases")

p3 = ggplot(data = plotting.data[which(Case == 2),], aes(y = CCA, x = Position, group = factor(ID)))+
geom_line(alpha = 0.6)+
ggtitle("Control Cases")

p4 = ggplot(data = plotting.data[which(Case == 1),], aes(y = CCA, x = Position, group = factor(ID)))+
geom_line(alpha = 0.4, aes(colour = PASAT))+
scale_colour_gradient(low = "blue", high = "red", trans = "sqrt")+
ggtitle("Multiple Sclerosis Cases w/ PASAT Scores (On SQRT Scale)")

p5 = ggplot(data = plotting.data, aes(PASAT))+
geom_histogram(colour = "black", breaks = seq(0,60, by = 2), fill = "red", alpha = 0.7)+
ggtitle("Histogram of MS Cases' PASAT Scores")

col = scale_colour_gradientn(colours =  palette())

smooth = geom_smooth(stat = "smooth", inherit.aes = FALSE, aes(x = Position, y = CCA), se = TRUE)

#============================#

# find mean of controls scores and subtract the MS cases scores

head(plotting.data)
attach(plotting.data)

d.ms = plotting.data[which(Case == 1),]
d.cont = plotting.data[which(Case == 2),]

col.means <- apply(fv, 8:100, mean)

#============================#
# ttest for difference of means between groups at each FA value #
#============================#

 #subset the data into groups
fv.control = data[which(first.visit$case == 0),]
fv.case = first.visit[which(first.visit$case == 1),]

#trial run
t1 = t.test(fv.control$cca_1, fv.case$cca_1)
t1

# t1 output has 3 points that we really need. t stat, df, and pvalue. let's create empty vectors so that the FOR LOOP puts each point into the next line in the correct vector

tstatistic = as.vector(NULL) #empty vector
df = as.vector(NULL)
pval = as.vector(NULL)

fv.control = subset(fv.control, select = c(8:100))
fv.case = subset(fv.case, select = c(8:100)) #removing the other column variables we're not testing for

n = length(colnames(fv.case)) #the number of CCA positions we are iterating through

for(i in 1:n){ #declare a loop to go through 1 to all of the CCA positions
	
	temp = t.test(fv.control[,i], fv.case[,i])
	#run a ttest at row i
	tstatistic[i] = as.numeric(temp[1])
	#place the first out put of that ttest into the originally empty tstatistic vector at row i
	df[i] = as.numeric(temp[2])
	#do the same for df and pval
	pval[i] = as.numeric(temp[3])
	
}

ttests = as.data.frame(cbind(tstatistic,df,pval)) #cbind these to create a matrix
#=============================#
w1 = wilcox.test(fv.control$cca_1, fv.case$cca_1)
w1

# t1 output has 3 points that we really need. t stat, df, and pvalue. let's create empty vectors so that the FOR LOOP puts each point into the next line in the correct vector

wstatistic = as.vector(NULL) #empty vector
df = as.vector(NULL)
pval = as.vector(NULL)

fv.control = subset(fv.control, select = c(8:100))
fv.case = subset(fv.case, select = c(8:100)) #removing the other column variables we're not testing for

n = length(colnames(fv.case)) #the number of CCA positions we are iterating through

for(i in 1:n){ #declare a loop to go through 1 to all of the CCA positions
	
	temp = wilcox.test(fv.control[,i], fv.case[,i])
	#run a ttest at row i
	wstatistic[i] = as.numeric(temp[1])
	#place the first out put of that ttest into the originally empty tstatistic vector at row i
	#do the same for df and pval
	pval[i] = as.numeric(temp[3])
	
}

wtests = as.data.frame(cbind(wstatistic,df,pval)) #cbind these to create a matrix
#=============================#

#=================================#
#plotting the data 3
#=================================#



#=============================#

# Using SPM package to create a semiparametric regression

attach(plotting.data)
ms = plotting.data[which(Case == 1),]
cont = plotting.data[which(Case == 2),] 

which(complete.cases(ms) == FALSE) # rows 1555 & 1556 have NaN
ms[1555:1556,] #participant ID 2017 position 67 & 68
ms = ms[which(complete.cases(ms) == TRUE),]

#cont[which(complete.cases(cont)==TRUE),]  #we don't need to do complete cases on this

plot(ms$Position,ms$CCA)
plot(cont$Position,cont$CCA)

fit.MS = spm(ms$CCA~f(ms$Position))
summary(fit.MS)
plot(fit.MS)

fit.cont = spm(cont$CCA~f(cont$Position), omit.missing = TRUE)
summary(fit.cont)
plot(fit.cont)

# It's not possible to get out data that I can manipulate with spm.
#=======================#

# Using a loess regression model with ggplot as opposed to spm() because we cant pull out the data

# fixing the two rows that had NAs. Going to use the mean of the participants' fa value

which(complete.cases(plotting.data) == FALSE) # rows 5461 & 5462 have NaN
plotting.data[5461:5462,] #participant ID 2017 position 67 & 68

p2017 = plotting.data[which(plotting.data$ID == 2017),]

mean = sum(p2017$CCA, na.rm = TRUE)/91

plotting.data[5461:5462,6] = mean
plotting.data[which(plotting.data$ID == 2017),]

attach(plotting.data)

p1.fit = p1 + smooth #all participants
p2.fit = p2 + smooth #mscases
p3.fit = p3 + smooth #control cases

p1build = ggplot_build(p1.fit)
head(ggplot_build(p1.fit))
temp = pg1build$data
str(temp)
p1temp = data.frame(temp[2])

p2build = ggplot_build(p2.fit)
head(ggplot_build(p2.fit))
temp = p2build$data
str(temp)
p2temp = data.frame(temp[2])

p3build = ggplot_build(p3.fit)
head(ggplot_build(p3.fit))
temp = p3build$data
str(temp)
p3temp = data.frame(temp[2])

#========================#

#plotting 3 lines

#========================#

plines = ggplot()+
geom_line(inherit.aes = FALSE,data = p1temp, aes(x = x, y = y, colour ="All Mean"))+
geom_line(inherit.aes = FALSE, data = p2temp, aes(x = x, y = y, colour = "MS Mean"))+
geom_line(inherit.aes = FALSE, data = p3temp, aes(x = x, y = y, colour = "Control Mean"))+
labs(fill = "",title = "Comparison of Means of FA Values", x = "Position Along Corpus Callosum", y = "FA Value")+
theme(legend.title = element_text(size = 0.0000001))

#========================#

# calculating and plotting differences between ms and control

#========================#

diff = as.vector(NULL)
length(p2temp[,1])

for(i in 1:80){
	
	tempcontrol = p3temp[i,2]
	tempms = p2temp[i,2]
	
	diff[i] = tempcontrol - tempms
	
}

diff.full = data.frame(cbind(diff,p2temp$x))

plot(diff.full)

pmeans = ggplot()+
geom_line(data = diff.full, aes(y = diff,x = p2temp$x, colour = "Mean Difference"))+
labs(fill = "",title = "Means Difference of FA Values Between Cases", x = "Position Along Corpus Callosum", y = "FA Value")+
theme(legend.title = element_text(size = 0.0000001))


# there are interesting spikes in this data where the difference between means is greatest. Maybe these areas represent high predictive value for our logistic regression?
#=========================#

# splitting into test & train for logistic regression
fv = data.frame(first.visit)

str(fv)
table(is.na(fv))
fv = data.frame(cbind(fv[,1],fv[,5],fv[,6],fv[,8:100]))
table(is.na(fv))
summary(fv)

ind = complete.cases(fv)
fv = fv[ind,]

colnames(fv)[1:3] = c("ID", "Case", "Sex")

length(fv$ID) #141 participants

# going to split the data train:test as 95:5

0.05*141 #7

n = sample(length(fv$ID), 7, replace = FALSE)

train = fv[-n,]
train.ids = train$ID
test = fv[n,]
test.ids = test$ID
# ========================== #

