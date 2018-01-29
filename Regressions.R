#####################################
# Rouault*, Seow*, Gillan and Fleming. (2018) Biological Psychiatry
# Psychiatric symptom dimensions are associated with dissociable shifts in metacognition but not task performance.

# Figures for regression data and factor analysis in Experiment 2
#####################################

# clear all
rm(list=ls())

# loading tools
library("ggplot2")
library("gridExtra")
library("lme4")
library(plyr) # for collapse-and-mean functions like ddply
library(psych)
library(GPArotation)
library(paran)
library(reshape)
library(polycor)
library(nFactors)
library(R.matlab)
library(reshape)
library("doBy")
options(scipen = 999)

# set directory 
setwd("/Users/marion/Desktop/PDOC/WebStudyMetacog/Data/ExperimentII")

#loading data
qndata = readMat("ME_phase2_excludqnadata_all.mat")
mdata = readMat("ME_phase2_excludanalyseddat_all.mat")

#create objects
id<-matrix(0,length(mdata$analyseddata),1)
mratio<-matrix(0,length(mdata$analyseddata),1)

confmean<-matrix(0,length(mdata$analyseddata),1)
correct<-matrix(0,length(mdata$analyseddata),1)
age<-matrix(0,length(mdata$analyseddata),1)
gender<-matrix(0,length(mdata$analyseddata),1)

qnid<-matrix(0,length(qndata$allqna),1)
zung<-matrix(0,length(qndata$allqna),1)
anxiety<-matrix(0,length(qndata$allqna),1)
ocir<-matrix(0,length(qndata$allqna),1)
leb<-matrix(0,length(qndata$allqna),1)
iq<-matrix(0,length(qndata$allqna),1)
bis<-matrix(0,length(qndata$allqna),1)
schizo<-matrix(0,length(qndata$allqna),1)
eat<-matrix(0,length(qndata$allqna),1)
apathy<-matrix(0,length(qndata$allqna),1)
alcohol<-matrix(0,length(qndata$allqna),1)

#extracting data from allqna
for (i in 1:length(qndata$allqna))
{
  qnid[i] = qndata$allqna[[i]][[1]][,,1]$id
  zung[i] = qndata$allqna[[i]][[1]][,,1]$zung[,,1]$score #first brackets is subject number
  anxiety[i] = qndata$allqna[[i]][[1]][,,1]$anxiety[,,1]$score
  ocir[i] = qndata$allqna[[i]][[1]][,,1]$ocir[,,1]$score
  leb[i] = qndata$allqna[[i]][[1]][,,1]$leb[,,1]$score
  iq[i] = qndata$allqna[[i]][[1]][,,1]$iq[,,1]$score
  bis[i] = qndata$allqna[[i]][[1]][,,1]$bis[,,1]$score[,,1]$total
  schizo[i] = qndata$allqna[[i]][[1]][,,1]$schizo[,,1]$score[,,1]$total
  eat[i] = qndata$allqna[[i]][[1]][,,1]$eat[,,1]$score[,,1]$total
  apathy[i] = qndata$allqna[[i]][[1]][,,1]$apathy[,,1]$score
  alcohol[i] = qndata$allqna[[i]][[1]][,,1]$alcohol[,,1]$score
}

qnframe = data.frame(qnid,anxiety, eat, apathy, alcohol, zung, ocir, leb, iq, bis, schizo)

#extracting data from analysed data
for (i in 1:length(mdata$analyseddata))
{
  id[i]=mdata$analyseddata[[i]][[1]][,,1]$data[1,4]
  age[i] =(mdata$analyseddata[[i]][[1]][,,1]$data[1,2])
  gender[i]=mdata$analyseddata[[i]][[1]][,,1]$data[1,3]
  
  confmean[i] = mean(mdata$analyseddata[[i]][[1]][,,1]$data[,9])

  correct[i] = mean(mdata$analyseddata[[i]][[1]][,,1]$data[,6])
  
  mratio[i] = mdata$analyseddata[[i]][[1]][,,1]$mratio
}


gender <- factor(gender)

taskframe = data.frame(id,age,gender,confmean,correct,mratio)

#merge all data together
alldata =merge(taskframe, qnframe,by.x=c("id"), by.y=c("qnid"))


#scaling the regressors
alldata$age.sc = scale(alldata$age)
alldata$confmean.sc = scale(alldata$confmean)
alldata$correct.sc = scale(alldata$correct)


# scaling the questionnaire scores
alldata$zung.sc = scale(log(alldata$zung))
alldata$anxiety.sc = scale(log(alldata$anxiety))
alldata$ocir.sc = scale(log(alldata$ocir+1))
alldata$leb.sc = scale(log(alldata$leb+1))
alldata$iq.sc = scale(alldata$iq)
alldata$schizo.sc = scale(log(alldata$schizo+1))
alldata$bis.sc = scale(log(alldata$bis))
alldata$eat.sc = scale(log(alldata$eat+1))
alldata$apathy.sc = scale(log(alldata$apathy))
alldata$alcohol.sc = scale(log(alldata$alcohol+1))

mrexcluddata <- alldata[alldata$mratio>0,] 
#exclude negative mratios

mrexcluddata$mratio.sc = scale(log(mrexcluddata$mratio))



######################################################
######################################################
# REGRESSIONS 1) VARIABLES w PSYCH SCORES ############
######################################################
######################################################

## BETWEEN-SUBJECT MODELS
# Define function we can re-use for each model
generic.model <- function(DV, df) {
  # Loop over each symptom score variable in separate lm models, store symptom score coefficients
  plot1 <- matrix(NA,9,4)
  modelString = paste(DV, "~zung.sc+iq.sc+age.sc+gender")
  mod1 <- lm(modelString, df)
  plot1[1,] <- summary(mod1)$coefficients[2,1:4]
  
  modelString = paste(DV, "~anxiety.sc+iq.sc+age.sc+gender")
  mod1 <- lm(modelString, df)
  plot1[2,] <- summary(mod1)$coefficients[2,1:4]
  
  modelString = paste(DV, "~ocir.sc+iq.sc+age.sc+gender")
  mod1 <- lm(modelString, df)
  plot1[3,] <- summary(mod1)$coefficients[2,1:4]
  
  modelString = paste(DV, "~leb.sc+iq.sc+age.sc+gender")
  mod1 <- lm(modelString, df)
  plot1[4,] <- summary(mod1)$coefficients[2,1:4]
  
  modelString = paste(DV, "~bis.sc+iq.sc+age.sc+gender")
  mod1 <- lm(modelString, df)
  plot1[5,] <- summary(mod1)$coefficients[2,1:4]
  
  modelString = paste(DV, "~schizo.sc+iq.sc+age.sc+gender")
  mod1 <- lm(modelString, df)
  plot1[6,] <- summary(mod1)$coefficients[2,1:4]
  
  modelString = paste(DV, "~eat.sc+iq.sc+age.sc++gender")
  mod1 <- lm(modelString, df)
  plot1[7,] <- summary(mod1)$coefficients[2,1:4]
  
  modelString = paste(DV, "~apathy.sc+iq.sc+age.sc++gender")
  mod1 <- lm(modelString, df)
  plot1[8,] <- summary(mod1)$coefficients[2,1:4]
  
  modelString = paste(DV, "~alcohol.sc+iq.sc+age.sc+gender")
  mod1 <- lm(modelString, df)
  plot1[9,] <- summary(mod1)$coefficients[2,1:4]
  plot1 <- data.frame(plot1)
  names(plot1) <- c("Estimate", "Std..Error", "t value", "P value")
  row.names(plot1) <- c("Depression" ,"Generalised Anxiety" ,"OCD" ,"Social Anxiety", "Impulsivity" ,"Schizotypy", "Eating Disorders", "Apathy","Alcoholism")

  return(plot1)
}


colorp <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",'#8da0cb')

# Performance
perffig1 <- generic.model("correct.sc", alldata)

# Mean confidence
metafig1 <- generic.model("confmean.sc", alldata)

# Mratio
metafig2 <- generic.model("mratio.sc", mrexcluddata)



##############################################################
##############################################################
# REGRESSIONS 2) PERFORMANCE/METACOG W AGE, GENDER, IQ #######
##############################################################
##############################################################

redmodcorr= lm(correct.sc~iq.sc+age.sc+gender,alldata) 
redmodconfm= lm(confmean.sc~iq.sc+age.sc+gender,alldata) 
redmodmratio= lm(mratio.sc~iq.sc+age.sc+gender,mrexcluddata) 


## Plot reduced model to look at the other variables - iq, age, gender
facval=c('#66c2a5','#e78ac3','#8da0cb')

perffixfig1 <- data.frame(summary(redmodcorr)$coefficients[2:4,1:4])
metafixfig1 <- data.frame(summary(redmodconfm)$coefficients[2:4,1:4])
metafixfig2 <- data.frame(summary(redmodmratio)$coefficients[2:4,1:4])



##############################################################
##############################################################
# REGRESSIONS 3) HDDM VARIABLES W PSYCH SCORES ###############
# ############################################################

# load HDDM parameters

HDDM = read.csv('subjParams_2k_3chain.csv')
HDDMpara=data.frame(t(HDDM[1:nrow(HDDM),2:length(HDDM)]))

colnames(HDDMpara) <- c("a", "t", "v_inter", "v_delta")
alldata=data.frame(alldata,HDDMpara)

alldata$a.sc = scale(alldata$a)
alldata$t.sc = scale(alldata$t)
alldata$v_inter.sc = scale(alldata$v_inter)
alldata$v_delta.sc = scale(alldata$v_delta)

# t (non decision time)
perffig2 <- generic.model("t.sc", alldata)

# v delta (drift rate - dot difference)
perffig3 <- generic.model("v_delta.sc", alldata)

# a intercept (decision threshold)
perffig4 <- generic.model("a.sc", alldata)

# v intercept (drift rate - baseline)
perffig5 <- generic.model("v_inter.sc", alldata)


##############################################################
##############################################################
# REGRESSIONS 4) HDDM VARIABLES W AGE, GENDER, IQ ############
##############################################################
##############################################################

redmoda= lm(a.sc~iq.sc+age.sc+gender,alldata)
redmodt= lm(t.sc~iq.sc+age.sc+gender,alldata)
redmodvdelta= lm(v_delta.sc~iq.sc+age.sc+gender,alldata)
redmodvinter= lm(v_inter.sc~iq.sc+age.sc+gender,alldata)

perffixfig2 <- data.frame(summary(redmodt)$coefficients[2:4,1:4])
perffixfig3 <- data.frame(summary(redmodvdelta)$coefficients[2:4,1:4])
perffixfig4 <- data.frame(summary(redmoda)$coefficients[2:4,1:4])
perffixfig5 <- data.frame(summary(redmodvinter)$coefficients[2:4,1:4])



######################################################
######################################################
##### 5) FACTOR ANALYSIS #############################
######################################################
######################################################

# LOAD ALL QUESTIONAIRRE (indiviual questions) DATA
#create objects
qnindivid<-matrix(0,length(qndata$allqna),1)
zungall<-matrix(0,length(qndata$allqna),length(qndata$allqna[[1]][[1]][,,1]$zung[,,1]$raw))
anxietyall<-matrix(0,length(qndata$allqna),length(qndata$allqna[[1]][[1]][,,1]$anxiety[,,1]$raw))
ocirall<-matrix(0,length(qndata$allqna),length(qndata$allqna[[1]][[1]][,,1]$ocir[,,1]$raw))
leball<-matrix(0,length(qndata$allqna),length(qndata$allqna[[1]][[1]][,,1]$leb[,,1]$raw[,,1]$avg))
bisall<-matrix(0,length(qndata$allqna),length(qndata$allqna[[1]][[1]][,,1]$bis[,,1]$raw))
schizoall<-matrix(0,length(qndata$allqna),length(qndata$allqna[[1]][[1]][,,1]$schizo[,,1]$raw))
eatall<-matrix(0,length(qndata$allqna),length(qndata$allqna[[1]][[1]][,,1]$eat[,,1]$raw))
apathyall<-matrix(0,length(qndata$allqna),length(qndata$allqna[[1]][[1]][,,1]$apathy[,,1]$raw))
alcoholall<-matrix(0,length(qndata$allqna),length(qndata$allqna[[1]][[1]][,,1]$alcohol[,,1]$raw))

#extracting data from allqna
for (i in 1:length(qndata$allqna))
{
  qnindivid[i,]=qndata$allqna[[i]][[1]][,,1]$id
  zungall[i,] = qndata$allqna[[i]][[1]][,,1]$zung[,,1]$raw #first brackets is subject number
  anxietyall[i,] = t(qndata$allqna[[i]][[1]][,,1]$anxiety[,,1]$raw)
  ocirall[i,] = qndata$allqna[[i]][[1]][,,1]$ocir[,,1]$raw
  leball[i,] = (qndata$allqna[[i]][[1]][,,1]$leb[,,1]$raw[,,1]$avg)
  bisall[i,] = qndata$allqna[[i]][[1]][,,1]$bis[,,1]$raw
  schizoall[i,] = qndata$allqna[[i]][[1]][,,1]$schizo[,,1]$raw
  eatall[i,]=qndata$allqna[[i]][[1]][,,1]$eat[,,1]$raw
  apathyall[i,]=qndata$allqna[[i]][[1]][,,1]$apathy[,,1]$raw
  alcoholall[i,]=qndata$allqna[[i]][[1]][,,1]$alcohol[,,1]$raw
}

qns = data.frame("qnid"=qnindivid,"zung"=zungall, "anxiety"=anxietyall,"ocir"= ocirall, "leb" =leball,"bis"= bisall,"schizo"= schizoall, 'alcohol'=alcoholall,'eat'=eatall,'apathy'=apathyall)


# DO FACTOR ANALYSIS ON RAW QUESTIONAIRRE SCORES
# Produce covariance matrix using hetcor to account for both continuous and binary correlations
	
het.mat <- hetcor(qns[,2:length(qns)])$cor

fa <- fa(r = het.mat, nfactors = 3, n.obs = nrow(qns), rotate = "oblimin", fm="ml", scores="regression")
fa.scores <- factor.scores(x=qns[,2:length(qns)], f=fa)
scores = data.frame("id"=qns$qnid, fa.scores$scores)
loadings <- fa$loadings

# loadings plot m2= sa, m1 = a&d, m4 = impul, m3 = compul (THIS CAN CHANGE W THE FACTOR ANAYLSIS)
colnames(scores) <- c("id", "AD", "Compul", "SW")
factormod =merge(alldata, scores,by.x=c("id"), by.y=c("id"))



##############################################################
##############################################################
# REGRESSIONS 6) PERFORMANCE/METCOG WITH FACTOR SCORES  ######
##############################################################
##############################################################

fullmodcorr= lm(correct.sc~AD+Compul+SW+iq.sc+age.sc+gender,factormod) 
fullmodconfm= lm(confmean.sc~AD+Compul+SW+iq.sc+age.sc+gender,factormod) 

# test magnitudes of contrasts
lambda1 <- c(0,1,1,0,0,0,0)
esticon(fullmodconfm, lambda1, beta0=0)

#log mratio
mrexcludfactormod <- factormod[factormod$mratio>0,]
mrexcludfactormod$mratio.sc = scale(log(mrexcludfactormod$mratio))

fullmodmratio= lm(mratio.sc~AD+Compul+SW+iq.sc+age.sc+gender,mrexcludfactormod) 

perffacfig1 <- data.frame(summary(fullmodcorr)$coefficients[2:4,1:4])
metafacfig1 <- data.frame(summary(fullmodconfm)$coefficients[2:4,1:4])
metafacfig2 <- data.frame(summary(fullmodmratio)$coefficients[2:4,1:4])


fullmoda= lm(a.sc~AD+Compul+SW+iq.sc+age.sc+gender,factormod) 
fullmodt= lm(t.sc~AD+Compul+SW+iq.sc+age.sc+gender,factormod) 
fullmodvdelta= lm(v_delta.sc~AD+Compul+SW+iq.sc+age.sc+gender,factormod) 
fullmodvinter= lm(v_inter.sc~AD+Compul+SW+iq.sc+age.sc+gender,factormod) 

perffacfig2 <- data.frame(summary(fullmodt)$coefficients[2:4,1:4])
perffacfig3 <- data.frame(summary(fullmodvdelta)$coefficients[2:4,1:4])
perffacfig4 <- data.frame(summary(fullmoda)$coefficients[2:4,1:4])
perffacfig5 <- data.frame(summary(fullmodvinter)$coefficients[2:4,1:4])




##############################################################
##############################################################
# REGRESSIONS 7) METACOG FACTOR FULL MODELS #################
##############################################################
##############################################################

allmconf=lm(confmean.sc~AD+Compul+SW+correct.sc+a.sc+t.sc+v_delta.sc+v_inter.sc+iq.sc+age.sc+gender,factormod) 
allmratio=lm(mratio.sc~AD+Compul+SW+correct.sc+a.sc+t.sc+v_delta.sc+v_inter.sc+iq.sc+age.sc+gender,mrexcludfactormod) 

mconfallc <- data.frame(summary(allmconf)$coefficients[2:4,1:4])
mratioallc<- data.frame(summary(allmratio)$coefficients[2:4,1:4])




###############################################################
###############################################################
# REGRESSIONS 8) METACOG FULL MODELS (FOR PYSCHS AND DEMOS) ###
###############################################################
###############################################################


## BETWEEN-SUBJECT MODELS
# Define function we can re-use for each model
generic2.model <- function(DV, df) {
  # Loop over each symptom score variable in separate lm models, store symptom score coefficients
  plot1 <- matrix(NA,9,4)
  modelString = paste(DV, "~zung.sc+correct.sc+a.sc+t.sc+v_inter.sc+v_delta.sc+iq.sc+age.sc+gender")
  mod1 <- lm(modelString, df)
  plot1[1,] <- summary(mod1)$coefficients[2,1:4]
  
  modelString = paste(DV, "~anxiety.sc+correct.sc+a.sc+t.sc+v_inter.sc+v_delta.sc+iq.sc+age.sc+gender")
  mod1 <- lm(modelString, df)
  plot1[2,] <- summary(mod1)$coefficients[2,1:4]
  
  modelString = paste(DV, "~ocir.sc+correct.sc+a.sc+t.sc+v_inter.sc+v_delta.sc+iq.sc+age.sc+gender")
  mod1 <- lm(modelString, df)
  plot1[3,] <- summary(mod1)$coefficients[2,1:4]
  
  modelString = paste(DV, "~leb.sc+correct.sc+a.sc+t.sc+v_inter.sc+v_delta.sc+iq.sc+age.sc+gender")
  mod1 <- lm(modelString, df)
  plot1[4,] <- summary(mod1)$coefficients[2,1:4]
  
  modelString = paste(DV, "~bis.sc+correct.sc+a.sc+t.sc+v_inter.sc+v_delta.sc+iq.sc+age.sc+gender")
  mod1 <- lm(modelString, df)
  plot1[5,] <- summary(mod1)$coefficients[2,1:4]
  
  modelString = paste(DV, "~schizo.sc+correct.sc+a.sc+t.sc+v_inter.sc+v_delta.sc+iq.sc+age.sc+gender")
  mod1 <- lm(modelString, df)
  plot1[6,] <- summary(mod1)$coefficients[2,1:4]
  
  modelString = paste(DV, "~eat.sc+correct.sc+a.sc+t.sc+v_inter.sc+v_delta.sc+iq.sc+age.sc++gender")
  mod1 <- lm(modelString, df)
  plot1[7,] <- summary(mod1)$coefficients[2,1:4]
  
  modelString = paste(DV, "~apathy.sc+correct.sc+a.sc+t.sc+v_inter.sc+v_delta.sc+iq.sc+age.sc++gender")
  mod1 <- lm(modelString, df)
  plot1[8,] <- summary(mod1)$coefficients[2,1:4]
  
  modelString = paste(DV, "~alcohol.sc+correct.sc+a.sc+t.sc+v_inter.sc+v_delta.sc+iq.sc+age.sc+gender")
  mod1 <- lm(modelString, df)
  plot1[9,] <- summary(mod1)$coefficients[2,1:4]
  plot1 <- data.frame(plot1)
  names(plot1) <- c("Estimate", "Std..Error", "t value", "P value")
  row.names(plot1) <- c("Depression" ,"Generalised Anxiety" ,"OCD" ,"Social Anxiety", "Impulsivity" ,"Schizotypy", "Eating Disorders", "Apathy","Alcoholism")
  
  return(plot1)
}

# Mean confidence
metafig3 <- generic2.model("confmean.sc", alldata)

# Mratio
metafig4 <- generic2.model("mratio.sc", mrexcludfactormod)


redmodconfm2= lm(confmean.sc~iq.sc+age.sc+gender+correct.sc+a.sc+t.sc+v_inter.sc+v_delta.sc,alldata) 
redmodmratio2= lm(mratio.sc~iq.sc+age.sc+gender+correct.sc+a.sc+t.sc+v_inter.sc+v_delta.sc,mrexcludfactormod) 

metafixfig3 <- data.frame(summary(redmodconfm2)$coefficients[2:4,1:4])
metafixfig4 <- data.frame(summary(redmodmratio2)$coefficients[2:4,1:4])



##############################################################
##############################################################
# PLOT 9) METACOG FULL MODELS ##################################
##############################################################
##############################################################

# to get trial by trial data:

fulldata <- data.frame()
for (i in 1:length(mdata$analyseddata))
{
  # Build up full single trial data frame
  subjdata <- mdata$analyseddata[[i]][[1]][,,1]$data
  fulldata <- rbind(fulldata, subjdata)
}


colnames(fulldata) <- c("subj_idx", "age", "gender", "subj", "rt", "response", "delta", "targetPos", "conf")
fulldata$subj_idx <- factor(fulldata$subj_idx)

#####################################################

mconfallc$Type<-rownames(mconfallc)
mconfallc$Label<- 'Mean Confidence'

mratioallc$Type<-rownames(mratioallc)
mratioallc$Label<- 'M Ratio'

mconfmratio<-rbind(mconfallc,mratioallc)
mconfmratio$Label<-factor(mconfmratio$Label, levels=c('Mean Confidence','M Ratio'))

mconfmratio$Type[mconfmratio$Type=="AD"]<-"Anxious-Depression"
mconfmratio$Type[mconfmratio$Type=="Compul"]<-"Compulsivity"
mconfmratio$Type[mconfmratio$Type=="SW"]<-"Social Withdrawal"

(a = ggplot(data = mconfmratio, aes(x = Label, y = Estimate, group=Type)) +       
  geom_bar(aes(fill = Type), colour="black",size=1.2,stat="identity", position = "dodge",width=0.8))
(a = a +theme_classic() + labs(title=" ", x=" ", y = "Regression Coefficient") +
  geom_errorbar(aes(ymin=mconfmratio$Estimate-mconfmratio$Std..Error, ymax=mconfmratio$Estimate+mconfmratio$Std..Error),colour="black", width=0.3, size=1.2, position=position_dodge(.8)))
a<- a+ geom_hline(yintercept=0,size=1)
a <- a + theme(axis.title.y = element_text(size = rel(2.5), angle = 90))  + theme(axis.title.y=element_text(margin=margin(0,20,0,0)))
a <- a + theme(legend.text = element_text(size = 20))+ theme(legend.title = element_blank()) 
a <- a + theme(axis.title.x = element_text(size = rel(3), angle = 00)) + theme(axis.title.x=element_text(margin=margin(20,0,0,0)))
a <- a + theme(plot.title = element_text(size = rel(3), angle = 00))
a <- a + theme(axis.text.x = element_text(angle = 00, size=25))
a <- a + theme(axis.text.y = element_text(angle = 00, size=25))+ scale_x_discrete(expand = c(0,0.5))
a <- a +  theme(axis.line.x = element_line(color="black", size = 1.2),axis.line.y = element_line(color="black", size = 1.2))
a<- a+ theme(axis.ticks.y=element_line(size=(1.5)))+ theme(axis.ticks.x=element_line(size=(1.5))) +theme(axis.ticks.length=unit(0.4, "cm"))
a <- a + scale_fill_manual(values=c("#8dd3c7", "#ffffbc","#bebada")) + theme(legend.position="none")
a<-a+ylim(-0.3,0.4)
fig103<-a



##############################################################
##############################################################
# PLOT 10) PERFORMANCE w METACOG WITH FIXED EFFECTS ##########
##############################################################
##############################################################

perffixfig1$Label<- 'Accuracy'
perffixfig1$Type<-rownames(perffixfig1)


###ADDING IN METACOG
metafixfig1$Type<-rownames(metafixfig1)
metafixfig1$Label<- 'Mean Confidence'
metafixfig2$Type<-rownames(metafixfig2)
metafixfig2$Label<- 'M Ratio'
perffixfig<-rbind(perffixfig1,metafixfig1,metafixfig2)

perffixfig <- perffixfig[perffixfig$Type!='gender1',]
perffixfig$Type[perffixfig$Type=='iq.sc']<-'IQ'
perffixfig$Type[perffixfig$Type=='age.sc']<-'Age'
perffixfig$Label<-factor(perffixfig$Label, levels=c("Accuracy",'Mean Confidence','M Ratio'))

(a = ggplot(data = perffixfig, aes(x = Label, y = Estimate, group=Type)) +       
  geom_bar(aes(fill = Type), color="black",size=1.2,stat="identity", position = "dodge",width=0.6))
(a = a +theme_classic() + labs(title=" ", x=" ", y = "Regression Coefficient") +
  geom_errorbar(aes(ymin=perffixfig$Estimate-perffixfig$Std..Error, ymax=perffixfig$Estimate+perffixfig$Std..Error),colour="black", width=.3, size=1.2, position=position_dodge(.6)))
a<- a+ geom_hline(yintercept=0,size=1)
a <- a + theme(axis.title.y = element_text(size = rel(2.5), angle = 90))  + theme(axis.title.y=element_text(margin=margin(0,20,0,0)))
a <- a + theme(legend.text = element_text(size = 20))+ theme(legend.title = element_blank()) 
a <- a + theme(axis.title.x = element_text(size = rel(3), angle = 00)) + theme(axis.title.x=element_text(margin=margin(20,0,0,0)))
a <- a + theme(plot.title = element_text(size = rel(3), angle = 00))
a <- a + theme(axis.text.x = element_text(angle = 00, size=25))
a <- a + theme(axis.text.y = element_text(angle = 00, size=25))  
a <- a +  theme(axis.line.x = element_line(color="black", size = 1.2),axis.line.y = element_line(color="black", size = 1.2))
a<- a+ theme(axis.ticks.y=element_line(size=(1.5)))+ theme(axis.ticks.x=element_line(size=(1.5))) +theme(axis.ticks.length=unit(0.4, "cm"))
a <- a + scale_fill_manual(values=c("#ffffff", "#555555"))+theme(legend.position="none")
a<-a+ylim(-0.3,0.4)
fig1<-a


##############################################################
##############################################################
# PLOT 11) PERFORMANCE W METACOG WITH PYSCH SCORES ###########
##############################################################
##############################################################
perffig1$Label<- 'Accuracy'
perffig1$Type<-rownames(perffig1)

#METACOG
metafig1$Type<-rownames(metafig1)
metafig1$Label<- 'Mean Confidence'


metafig2$Type<-rownames(metafig2)
metafig2$Label<- 'M Ratio'

perffig<-rbind(perffig1,metafig1,metafig2)

perffig$Label<-factor(perffig$Label, levels=c("Accuracy",'Mean Confidence','M Ratio'))

perffig$Type<-factor(perffig$Type, levels=c("Apathy","Social Anxiety","Generalised Anxiety","Impulsivity",'Depression',"Alcoholism","Schizotypy","OCD",'Eating Disorders'))

(a = ggplot(data = perffig, aes(x = Label, y = Estimate, group=Type)) +       
  geom_bar(aes(fill = Type), colour="black",size=1.2,stat="identity", position = "dodge",width=0.8))
(a = a +theme_classic() + labs(title=" ", x=" ", y = "Regression Coefficient") +
  geom_errorbar(aes(ymin=perffig$Estimate-perffig$Std..Error, ymax=perffig$Estimate+perffig$Std..Error),colour="black", width=0.3, size=1.2, position=position_dodge(.8)))
a<- a+ geom_hline(yintercept=0,size=1)
a <- a + theme(axis.title.y = element_text(size = rel(2.5), angle = 90))  + theme(axis.title.y=element_text(margin=margin(0,20,0,0)))
a <- a + theme(legend.text = element_text(size = 20))+ theme(legend.title = element_blank()) 
a <- a + theme(axis.title.x = element_text(size = rel(3), angle = 00)) + theme(axis.title.x=element_text(margin=margin(20,0,0,0)))
a <- a + theme(plot.title = element_text(size = rel(3), angle = 00))
a <- a + theme(axis.text.x = element_text(angle = 00, size=25))
a <- a + theme(axis.text.y = element_text(angle = 00, size=25))+ scale_x_discrete(expand = c(0,0.5))
a <- a +  theme(axis.line.x = element_line(color="black", size = 1.5),axis.line.y = element_line(color="black", size = 1.5))
a<- a+ theme(axis.ticks.y=element_line(size=(1.5)))+ theme(axis.ticks.x=element_line(size=(1.5))) +theme(axis.ticks.length=unit(0.4, "cm"))
a <- a + scale_fill_manual(values=c("#999999", "#377db8","#e31a1c","#984ea3","#4daf4a","#f781bf",'#ffff33','#ff7f00','#a65628'))+ theme(legend.position="none")
a<-a+ylim(-0.3,0.4)
fig2<-a

# e31a1c--red, anixety
# 377db8--blue, social anixety
# 4daf4a--green, depression
# 984ea3--purple, impulsivity
# ff7f00--orange, ocd
# ffff33--yellow, schizotypy
# a65628--brown, eating
# f781bf--pink, alcohol
# 999999--grey, apathy




##############################################################
##############################################################
# PLOT 12) PERFORMANCE/META COG WITH FACTOR SCORES ###########
##############################################################
##############################################################
# PLOTTING ReGRESSIONS FOR PERF
perffacfig1$Label<- 'Accuracy'
perffacfig1$Type<-rownames(perffacfig1)
perffacfig2$Label<- 't'
perffacfig2$Type<-rownames(perffacfig2)
perffacfig3$Label<- 'v delta'
perffacfig3$Type<-rownames(perffacfig3)
perffacfig4$Label<- 'a'
perffacfig4$Type<-rownames(perffacfig4)
perffacfig5$Label<- 'v intercept'
perffacfig5$Type<-rownames(perffacfig5)

# PLOTTING ReGRESSIONS FOR METACOG
metafacfig1$Type<-rownames(metafacfig1)
metafacfig1$Label<- 'Mean Confidence'

metafacfig2$Type<-rownames(metafacfig2)
metafacfig2$Label<- 'M Ratio'


facfig<-rbind(perffacfig1,perffacfig2,perffacfig5,perffacfig3,perffacfig4,metafacfig1,metafacfig2)

facfig$Label<-factor(facfig$Label, levels=c("Accuracy","t","v intercept", "v delta",'a','Mean Confidence','M Ratio'))

facfig$Type[facfig$Type=="AD"]<-"Anxious-Depression"
facfig$Type[facfig$Type=="Compul"]<-"Compulsivity"
facfig$Type[facfig$Type=="SW"]<-"Social Withdrawal"

(a = ggplot(data = facfig, aes(x = Label, y = Estimate, group=Type)) +       
  geom_bar(aes(fill = Type), colour="black",size=1.2,stat="identity", position = "dodge",width=0.8))
(a = a +theme_classic() + labs(title=" ", x=" ", y = "Regression Coefficient") +
  geom_errorbar(aes(ymin=facfig$Estimate-facfig$Std..Error, ymax=facfig$Estimate+facfig$Std..Error),colour="black", width=0.3, size=1.2, position=position_dodge(.8)))
a<- a+ geom_hline(yintercept=0,size=1)
a <- a + theme(axis.title.y = element_text(size = rel(2.5), angle = 90))  + theme(axis.title.y=element_text(margin=margin(0,20,0,0)))
a <- a + theme(legend.text = element_text(size = 20))+ theme(legend.title = element_blank()) 
a <- a + theme(axis.title.x = element_text(size = rel(3), angle = 00)) + theme(axis.title.x=element_text(margin=margin(20,0,0,0)))
a <- a + theme(plot.title = element_text(size = rel(3), angle = 00))
a <- a + theme(axis.text.x = element_text(angle = 00, size=25))
a <- a + theme(axis.text.y = element_text(angle = 00, size=25))+ scale_x_discrete(expand = c(0,0.5))
a <- a +  theme(axis.line.x = element_line(color="black", size = 1.2),axis.line.y = element_line(color="black", size = 1.2))
a<- a+ theme(axis.ticks.y=element_line(size=(1.5)))+ theme(axis.ticks.x=element_line(size=(1.5))) +theme(axis.ticks.length=unit(0.4, "cm"))
a <- a + scale_fill_manual(values=c("#8DD3C7", "#FFFFBC")) + theme(legend.position="none")
a <- a + scale_fill_manual(values=c("#8dd3c7", "#ffffbc","#bebada")) + theme(legend.position="none")
a<-a+ylim(-0.3,0.3)
#a<-a+ylim(-0.3,0.4)
fig7<-a


