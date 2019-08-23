library(dplyr)
library(stats)
library(survey)
library(psych)

####FUNCTIONS TO PROCESS OUTPUTS####
defineDes <- function(dataset){
  design <- svydesign(
    id = ~VEREP,
    strata = ~VESTR,
    data = dataset,
    weights = ~ANALWT_C,
    nest = TRUE
  )
  return(design)
}

confIntToDf <- function(model, varName){
  new<- data.frame(exp(cbind(coef(model), confint(model))))
  new['variable'] <- varName
  return(new)
}

mergeCiDfs <- function(bigDf, model, varName){
  newDf <- confIntToDf(model, varName)
  bigDf <- rbind(bigDf, newDf)
  return(bigDf)
}

buildUnivariateDf <- function(meanby, var){
  ci <- data.frame(confint(meanby))
  
  df <- data.frame(meanby)
  if(ncol(df) == 2){
    names(df) <- c('mean', 'SE')
    df['sex'] <- 'all'
  }else{
    names(df) <- c('sex', 'mean', 'SE')
  }
  
  df <- cbind(df, data.frame(ci))
  df['variable'] <- var
  print(df)
  return(df)
}

stackUnivariateDf <- function(df, meanby, var){
  new <- buildUnivariateDf(meanby, var)
  df <- rbind(df, new)
  print(df)
  return(df)
}

summaryToDf <- function(model, outcome){
  results <- summary(model)
  df <- data.frame(results$coefficients)
  df['variable'] <- outcome
  colnames(df) = c('Estimate', 'Std. Error', 't value', 'Pr(t)', 'outcome')
  return(df)
}

mergeSummaryDfs <- function(bigDf, model, outcome){
  new <- summaryToDf(model, outcome)
  bigDf <- rbind(bigDf, new)
  return(bigDf)
}

####LOAD CLEANED DATASET####
#NOTE: if you're interested in cleaning the raw dataset as we did...
#... please see opioids_dataset_cleaning.R
directory <- getwd()
subfolder <- '\\opioids'
directory <- paste(directory,subfolder, '\\nsduh2017', sep='')

file_loc <- paste(directory, '\\opioids_cleaned_data_v2.csv', sep = '')
df1 <- read.csv(file_loc, header = TRUE)
df1$newSex[df1$IRSEX == 1] <- 1
df1$newSex[df1$IRSEX == 2] <- 0
df1$sexF <- factor(df1$newSex, levels = c(0, 1), labels = c('Women', 'Men'))

####DATA ANALYSIS####
####MULTIVARIATE####
nsduh_design <-
  svydesign(
    id = ~VEREP,
    strata = ~VESTR,
    data = df1,
    weights = ~ANALWT_C,
    nest = TRUE
  )


design1 <- update(
  nsduh_design,
  
  sex = sexF,
  health = healthStatus,
  employment = employment,
  race = raceRecode,
  marstat = marStat,
  ed = education,
  metro = metro,
  income = incomeF,
  hasInsurance = hasInsurance,
  insureType = insureTypeF,
  marStat = marStat,
  age = ageCourseF,
  anyUse = anyPRUse,
  anyMisUse = anyPRMisuse,
  pyUse = usePY,
  pyMisuse = misusePY,
  oneDoc = oneDoc,
  multDocs = multDocs,
  stoleDoc = stoleDoc,
  gotFrRel = gotFrRel,
  boughtFrRel = boughtFrRel,
  tookFrRel = tookFrRel,
  drugDeal = drugDeal,
  otherSrc = otherSrc
)



lifetime_use <- svyglm(anyUse~sex+employment+ed+race+age+hasInsurance+marStat+metro+incomeF+health, 
                       design=design1,
                        family=quasibinomial())
summary(lifetime_use)
confIntDf <- confIntToDf(lifetime_use, 'lifetime_use')
summaryDf <- summaryToDf(lifetime_use, 'lifetime_use')


lifetime_misuse <- svyglm(anyMisUse~sex+employment+ed+race+age+hasInsurance+marStat+metro+incomeF+health, design=design1,
                          family=quasibinomial())

confIntDf <- mergeCiDfs(confIntDf,lifetime_misuse, 'lifetime_misuse')
summaryDf <- mergeSummaryDfs(summaryDf, lifetime_misuse, 'lifetime_misuse')



pastyr_use <- svyglm(pyUse~sex+employment+ed+race+age+hasInsurance+marStat+metro+incomeF+health, design=design1,
                       family=quasibinomial())
confIntDf <- mergeCiDfs(confIntDf, pastyr_use, 'pastyr_use')
summaryDf <- mergeSummaryDfs(summaryDf, pastyr_use, 'pastyr_use')

pastyr_misuse <- svyglm(pyMisuse~sex+employment+ed+race+age+hasInsurance+marStat+metro+incomeF+health, design=design1,
                          family=quasibinomial())
confIntDf <- mergeCiDfs(confIntDf, pastyr_misuse, 'pastyr_misuse')
summaryDf <- mergeSummaryDfs(summaryDf, pastyr_misuse, 'pastyr_misuse')

temp <- filter(df1, usePY == 1)
nsduh_design <-
  svydesign(
    id = ~VEREP,
    strata = ~VESTR,
    data = temp,
    weights = ~ANALWT_C,
    nest = TRUE
  )

design1 <- update(
  nsduh_design,
  employment = employment,
  health = healthStatus,
  metro = metro,
  income = incomeF,
  race = raceRecode,
  ed = education,
  hasInsurance = hasInsurance,
  insureType = insureTypeF,
  marStat = marStat,
  age = ageCourseF,
  sex = sexF,
  depend = dependPY
  
)

opDepend <- svyglm(depend~sex+employment+ed+race+age+hasInsurance+marStat+metro+incomeF+health, design=design1,
                   family=quasibinomial())
confIntDf <- mergeCiDfs(confIntDf, opDepend, 'dependence')
summaryDf <- mergeSummaryDfs(summaryDf, opDepend, 'dependence')
summary(opDepend)
#Filter to include only those who responded to detailed POMU questions
temp <- filter(df1, misusePY == 1)
nsduh_design <-
  svydesign(
    id = ~VEREP,
    strata = ~VESTR,
    data = temp,
    weights = ~ANALWT_C,
    nest = TRUE
  )

design1 <- update(
  nsduh_design,
  health = healthStatus,
  sex = sexF,
  employment = employment,
  race = raceRecode,
  ed = education,
  hasInsurance = hasInsurance,
  insureType = insureTypeF,
  income = incomeF,
  metro = metro,
  marStat = marStat,
  age = ageCourseF,
  anyUse = anyPRUse,
  anyMisUse = anyPRMisuse,
  pyUse = usePY,
  pyMisuse = misusePY,
  oneDoc = oneDoc,
  multDocs = multDocs,
  stoleDoc = stoleDoc,
  gotFrRel = gotFrRel,
  boughtFrRel = boughtFrRel,
  tookFrRel = tookFrRel,
  drugDeal = drugDeal,
  otherSrc = otherSrc,
  depend = dependPY,
  mainPain = mainPain,
  mainEmot = mainEmotions,
  mainHigh = mainHigh,
  mainRelax = mainRelax,
  mainHooked = mainHooked,
  mainOther = mainOther,
  mainOtherDrug = mainOtherDrug,
  mainSleep = mainSleep,
  mainExp = mainExperiment
)


srcOneDoc <- svyglm(oneDoc~sex+employment+ed+race+age+hasInsurance+marStat+metro+incomeF+health, design=design1,
                        family=quasibinomial())
confIntDf <- mergeCiDfs(confIntDf, srcOneDoc, 'srcOneDoc')
summaryDf <- mergeSummaryDfs(summaryDf, srcOneDoc, 'srcOneDoc')

srcMultDocs <- svyglm(multDocs~sex+employment+ed+race+age+hasInsurance+marStat+metro+incomeF+health, design=design1,
                      family=quasibinomial())
confIntDf <- mergeCiDfs(confIntDf, srcMultDocs, 'srcMultDocs')
summaryDf <- mergeSummaryDfs(summaryDf, srcMultDocs, 'srcMultDocs')

srcDrugDeal <- svyglm(drugDeal~sex+employment+ed+race+age+hasInsurance+marStat+metro+incomeF+health, design=design1,
                      family=quasibinomial())
confIntDf <- mergeCiDfs(confIntDf, srcDrugDeal, 'srcDrugDeal')
summaryDf <- mergeSummaryDfs(summaryDf, srcDrugDeal, 'srcDrugDeal')

srcBoughtFrRel <- svyglm(boughtFrRel~sex+employment+ed+race+age+hasInsurance+marStat+metro+incomeF+health, design=design1,
                      family=quasibinomial())
confIntDf <- mergeCiDfs(confIntDf, srcBoughtFrRel, 'srcBoughtFrRel')
summaryDf <- mergeSummaryDfs(summaryDf, srcBoughtFrRel, 'srcBoughtFrRel')

srcTookFrRel <- svyglm(tookFrRel~sex+employment+ed+race+age+hasInsurance+marStat+metro+incomeF+health, design=design1,
                         family=quasibinomial())
confIntDf <- mergeCiDfs(confIntDf, srcTookFrRel, 'srcTookFrRel')
summaryDf <- mergeSummaryDfs(summaryDf, srcTookFrRel, 'srcTookFrRel')

srcGotFrRel <- svyglm(gotFrRel~sex+employment+ed+race+age+hasInsurance+marStat+metro+incomeF+health, design=design1,
                         family=quasibinomial())
confIntDf <- mergeCiDfs(confIntDf, srcGotFrRel, 'gotFrRel')
summaryDf <- mergeSummaryDfs(summaryDf, srcGotFrRel, 'gotFrRel')



rsnPain <- svyglm(mainPain~sex+employment+ed+race+age+hasInsurance+marStat+metro+incomeF+health, design=design1,
                  family=quasibinomial())
confIntDf <- mergeCiDfs(confIntDf, rsnPain, 'rsnPain')
summaryDf <- mergeSummaryDfs(summaryDf, rsnPain, 'rsnPain')

rsnEmot <- svyglm(mainEmot~sex+employment+ed+race+age+hasInsurance+marStat+metro+incomeF+health, design=design1,
                  family=quasibinomial())
confIntDf <- mergeCiDfs(confIntDf,  rsnEmot, 'rsnEmot')
summaryDf <- mergeSummaryDfs(summaryDf, rsnEmot, 'rsnEmot')

rsnHigh <- svyglm(mainHigh~sex+employment+ed+race+age+hasInsurance+marStat+metro+incomeF+health, design=design1,
                  family=quasibinomial())
confIntDf <- mergeCiDfs(confIntDf,  rsnHigh, 'rsnHigh')
summaryDf <- mergeSummaryDfs(summaryDf, rsnHigh, 'rsnHigh')

rsnRelax <- svyglm(mainRelax~sex+employment+ed+race+age+hasInsurance+marStat+metro+incomeF+health, design=design1,
                  family=quasibinomial())
confIntDf <- mergeCiDfs(confIntDf, rsnRelax, 'rsnRelax')
summaryDf <- mergeSummaryDfs(summaryDf, rsnRelax, 'rsnRelax')

rsnExp <- svyglm(mainExp~sex+employment+ed+race+age+hasInsurance+marStat+metro+incomeF+health, design=design1,
                  family=quasibinomial())
confIntDf <- mergeCiDfs(confIntDf, rsnExp, 'rsnExp')
summaryDf <- mergeSummaryDfs(summaryDf, rsnExp, 'rsnExp')

rsnSleep <- svyglm(mainSleep~sex+employment+ed+race+age+hasInsurance+marStat+metro+incomeF+health, design=design1,
                  family=quasibinomial())
confIntDf <- mergeCiDfs(confIntDf, rsnSleep, 'rsnSleep')
summaryDf <- mergeSummaryDfs(summaryDf, rsnSleep, 'rsnSleep')

write.csv(confIntDf, paste(directory, '\\conf_ints_log_reg_newvars_1.csv', sep=''))
write.csv(summaryDf, paste(directory, '\\summary_log_reg_newvars_1.csv', sep=''))
confIntDf['keep'] <-  FALSE
summaryDf['keep'] <- FALSE

for(i in 1:nrow(confIntDf)){
  row <- rownames(confIntDf)[i]
  #print(row)
  if(grepl('sex', row)){
    confIntDf$keep[i] <- TRUE
    summaryDf$keep[i] <- TRUE
  }
}

just_gender_ci <- filter(confIntDf, keep == TRUE)
just_gender_summary <- filter(summaryDf, keep == TRUE)
just_gender_outputs <- cbind(just_gender_ci, just_gender_summary)

write.csv(just_gender_outputs, paste(directory, '\\outputs_gender_only.csv', sep=''))

####UNIVARIATE####

nsduh_design <-
  svydesign(
    id = ~VEREP,
    strata = ~VESTR,
    data = df1,
    weights = ~ANALWT_C,
    nest = TRUE
  )

design1 <- update(
  nsduh_design,
  health <- healthStatus,
  sex = sexF,
  employment = employment,
  race = raceF,
  ed = education,
  hasInsurance = hasInsurance,
  insureType = insureTypeF,
  marStat = marStat,
  age = ageCourseF,
  anyUse = anyPRUse,
  anyMisUse = anyPRMisuse,
  pyUse = usePY,
  pyMisuse = misusePY,
  depend = dependPY,
  oneDoc = oneDoc,
  multDocs = multDocs,
  stoleDoc = stoleDoc,
  gotFrRel = gotFrRel,
  boughtFrRel = boughtFrRel,
  tookFrRel = tookFrRel,
  drugDeal = drugDeal,
  otherSrc = otherSrc
)

#DEMOGRAPHICS
employment <- svyby(~employment, ~sex, design1, svymean)
confint(employment)
employmentAll <-  svymean(~employment, design1)
result <- summary(svytable(~employment+sex, design1), statistic = 'adjWald')

married <- svyby(~marStat, ~sex, design1, svymean)
confint(married)
marriedAll <- svymean(~marStat, design1)
summary(svytable(~marStat+sex, design1), statistic = 'adjWald')

race <- svyby(~race, ~sex, design1, svymean)
raceAll <- svymean(~race, design1)
summary(svytable(~race+sex, design1), statistic = 'adjWald')

education <- svyby(~ed, ~sex, design1, svymean)
education
confint(education)
tbl <- svytable(~ed+sex, design1)
summary(tbl, statistic = 'adjWald')
edA <- svymean(~education, design1)
edA
confint(edA)

insure <- svyby(~insureType, ~sex, design1, svymean)
insure
confint(insure)
tbl<- svytable(~insureType+sex, design1)
summary(tbl, statistic = 'adjWald')
insureA <- svymean(~insureType, design1)
insureA
confint(insureA)

ageComp <- svyby(~age, ~sex, design1, svymean)
ageComp
confint(ageComp)
tbl<- svytable(~age+sex, design1)
summary(tbl, statistic = 'adjWald')
ageA <- svymean(~age, design1)
ageA
confint(ageA)

marComp <- svyby(~marStat, ~sex, design1, svymean)
marComp
svyby(~marStat, ~sex, design1, svymean, unwtd.count)
confint(marComp)
tbl <- svytable(~marStat+sex, design1)
summary(tbl, statistic = 'adjWald')
marA <- svymean(~marStat, design1)
marA
confint(marA)

metroComp <- svyby(~metro, ~sex, design1, svymean)
metroComp
confint(metroComp)
summary(svytable(~metro+sex, design1), statistic = 'adjWald')
metroAll <- svymean(~metro, design1)
metroAll
confint(metroAll)

incomeComp <- svyby(~income, ~sex, design1, svymean)
incomeComp
confint(incomeComp)
summary(svytable(~income+sex, design1), statistic = 'adjWald')
incomeAll <- svymean(~income, design1)
incomeAll
confint(incomeAll)

insuranceAny <- svyby(~hasInsurance, ~sex, design1, svymean)
insuranceAny
confint(insuranceAny)
summary(svytable(~hasInsurance+sex, design1), statistic = 'adjWald')
insuranceAll <- svymean(~hasInsurance, design1)
insuranceAll
confint(insuranceAll)

temp = filter(df1, !is.na(anyPRUse))
nsduh_design <-
  svydesign(
    id = ~VEREP,
    strata = ~VESTR,
    data = temp,
    weights = ~ANALWT_C,
    nest = TRUE
  )

design1 <- update(
  nsduh_design,
  health = healthStatus,
  sex = sexF,
  employment = employment,
  race = raceF,
  ed = education,
  hasInsurance = hasInsurance,
  insureType = insureTypeF,
  marStat = marStat,
  age = ageCourseF,
  anyUse = anyPRUse,
  anyMisUse = anyPRMisuse,
  pyUse = usePY,
  pyMisuse = misusePY,
  depend = dependPY,
  oneDoc = oneDoc,
  multDocs = multDocs,
  stoleDoc = stoleDoc,
  gotFrRel = gotFrRel,
  boughtFrRel = boughtFrRel,
  tookFrRel = tookFrRel,
  drugDeal = drugDeal,
  otherSrc = otherSrc
)



#Lifetime use
lifetimeUse <- svyby(~anyUse, ~sex, design1, svymean)
df <- buildUnivariateDf(lifetimeUse, 'lifetimeUse')
useAll <- svymean(~anyUse, design1)
df <- stackUnivariateDf(df, useAll, 'lifetimeUse')

summary(svytable(~anyUse+sex, design1), statistic = 'adjWald')


#Past year use
pastyrUse <- svyby(~pyUse, ~sex, design1, svymean)
df <- stackUnivariateDf(df, pastyrUse, 'pastyearUse')
pyUseAll <- svymean(~pyUse, design1)
df <- stackUnivariateDf(df, pyUseAll, 'pastyearUse')

summary(svytable(~pyUse+sex, design1), statistic = 'adjWald')

temp <- filter(df1, !is.na(anyPRMisuse))
nsduh_design <-
  svydesign(
    id = ~VEREP,
    strata = ~VESTR,
    data = temp,
    weights = ~ANALWT_C,
    nest = TRUE
  )

design1 <- update(
  nsduh_design,
  
  sex = sexF,
  employment = employment,
  race = raceF,
  ed = education,
  hasInsurance = hasInsurance,
  insureType = insureTypeF,
  marStat = marStat,
  age = ageCourseF,
  anyUse = anyPRUse,
  anyMisuse = anyPRMisuse,
  pyUse = usePY,
  pyMisuse = misusePY,
  oneDoc = oneDoc,
  multDocs = multDocs,
  stoleDoc = stoleDoc,
  depend = dependPY,
  gotFrRel = gotFrRel,
  boughtFrRel = boughtFrRel,
  tookFrRel = tookFrRel,
  drugDeal = drugDeal,
  otherSrc = otherSrc
)

#Lifetime misuse
lifetimeMisuse <- svyby(~anyMisuse, ~sex, design1, svymean)
df <- stackUnivariateDf(df, lifetimeMisuse, 'lifetimeMisuse')

tbl <- svytable(~anyMisuse+sex, design1)
summary(tbl, statistic = 'adjWald')
misUseAll <- svymean(~anyMisuse, design1)
df <- stackUnivariateDf(df, misUseAll, 'lifetimeMisuse')

#Past year misuse
pyMisUse <- svyby(~pyMisuse, ~sex, design1, svymean)
df <- stackUnivariateDf(df, pyMisUse, 'pastyearMisuse')

summary(svytable(~pyMisuse+sex, design1), statistic = 'adjWald')

df <- stackUnivariateDf(df, svymean(~pyMisuse, design1), 'pastYearMisuse')

#Dependence--whole population
depComp <- svyby(~depend, ~sex, design1, svymean)
df <- stackUnivariateDf(df, depComp, 'dependence_total_pop')
summary(svytable(~depend+sex, design1), statistic = 'adjWald')
df <- stackUnivariateDf(df, svymean(~depend, design1), 'dependence_total_pop')

temp <- filter(df1, usePY == 1)
nsduh_design <-
  svydesign(
    id = ~VEREP,
    strata = ~VESTR,
    data = temp,
    weights = ~ANALWT_C,
    nest = TRUE
  )

design1 <- update(
  nsduh_design,
  
  sex = sexF,
  depend = dependPY
  
)
#Dependence--among those with past-year POU
depComp <- svyby(~depend, ~sex, design1, svymean)
df <- stackUnivariateDf(df, depComp, 'dependence_py_users')
df <- stackUnivariateDf(df, svymean(~depend, design1), 'dependence_py_users')
summary(svytable(~depend+sex, design1), statistic = 'adjWald')

#Filter to include only those who responded to detailed POMU questions
temp <- filter(df1, misusePY == 1)
nsduh_design <-
  svydesign(
    id = ~VEREP,
    strata = ~VESTR,
    data = temp,
    weights = ~ANALWT_C,
    nest = TRUE
  )

design1 <- update(
  nsduh_design,
  
  sex = sexF,
  employment = employment,
  race = raceRecode,
  ed = education,
  hasInsurance = hasInsurance,
  insureType = insureTypeF,
  marStat = marStat,
  age = ageCourseF,
  anyUse = anyPRUse,
  anyMisUse = anyPRMisuse,
  pyUse = usePY,
  pyMisuse = misusePY,
  oneDoc = oneDoc,
  multDocs = multDocs,
  stoleDoc = stoleDoc,
  gotFrRel = gotFrRel,
  boughtFrRel = boughtFrRel,
  tookFrRel = tookFrRel,
  drugDeal = drugDeal,
  otherSrc = otherSrc,
  depend = dependPY,
  mainPain = mainPain,
  mainEmot = mainEmotions,
  mainHigh = mainHigh,
  mainRelax = mainRelax,
  mainHooked = mainHooked,
  mainOther = mainOther,
  mainOtherDrug = mainOtherDrug,
  mainSleep = mainSleep,
  mainExp = mainExperiment
)

oneDoc <- svyby(~oneDoc, ~sex, design1, svymean, na.rm = TRUE)
df <- stackUnivariateDf(df, oneDoc, 'srcOneDoc')
df <- stackUnivariateDf(df, svymean(~oneDoc, design1, na.rm = TRUE), 'srcOneDoc')

summary(svytable(~oneDoc+sex, design1), statistic = "adjWald")

frRelGot <- svyby(~gotFrRel, ~sex, design1, svymean, na.rm = TRUE)
df <- stackUnivariateDf(df, frRelGot, 'srcGotFrRel')
df <- stackUnivariateDf(df, svymean(~gotFrRel, design1, na.rm = TRUE), 'srcGotFrRel')

summary(svytable(~gotFrRel+sex, design1), statistic = "adjWald")

boughtFrRel <- svyby(~boughtFrRel, ~sex, design1, svymean, na.rm = TRUE)
df <- stackUnivariateDf(df, boughtFrRel, 'srcBoughtFrRel')
df <- stackUnivariateDf(df, svymean(~boughtFrRel, design1, na.rm = TRUE), 'srcBoughtFrRel')

summary(svytable(~boughtFrRel+sex, design1), statistic = "adjWald")

tookFrRel <- svyby(~tookFrRel, ~sex, design1, svymean, na.rm = TRUE)
df <- stackUnivariateDf(df, tookFrRel, 'srcTookFrRel')
df <- stackUnivariateDf(df, svymean(~tookFrRel, design1, na.rm = TRUE), 'srcTookFrRel')
summary(svytable(~tookFrRel+sex, design1), statistic = "adjWald")

multDocsComp <- svyby(~multDocs, ~sex, design1, svymean, na.rm = TRUE)
df <- stackUnivariateDf(df, multDocsComp, 'srcMultDocs')
df <- stackUnivariateDf(df, svymean(~multDocs, design1,na.rm = TRUE), 'srcMultDocs')
#write.csv(df, paste(directory, '\\test_univariate_outs.csv', sep = ""))
summary(svytable(~multDocs+sex, design1), statistic = "adjWald")

df <- stackUnivariateDf(df, svyby(~stoleDoc, ~sex, design1, svymean, na.rm = TRUE), 
                        'srcStoleDoc')
df <- stackUnivariateDf(df, svymean(~stoleDoc, design1, na.rm=TRUE), 'srcStoleDoc')
summary(svytable(~stoleDoc+sex, design1), statistic = "adjWald")

drugDealTest <- svyby(~drugDeal, ~sex, design1, svymean, na.rm = TRUE)
drugDealTest
df <- stackUnivariateDf(df, svyby(~drugDeal, ~sex, design1, svymean, na.rm = TRUE),
                        'srcDrugDeal')
df <- stackUnivariateDf(df, svymean(~drugDeal, design1, na.rm = TRUE), 'srcDrugDeal')
summary(svytable(~drugDeal+sex, design1), statistic = "adjWald")

df <- stackUnivariateDf(df, svyby(~mainPain, ~sex, design1, svymean, na.rm = TRUE),
                        'mainRsnPain')
df <- stackUnivariateDf(df, svymean(~mainPain, design1, na.rm = TRUE), 'mainRsnPain')
summary(svytable(~mainPain+sex, design1), statistic = "adjWald")

df <- stackUnivariateDf(df, svyby(~mainRelax, ~sex, design1, svymean, na.rm = TRUE),
                        'mainRsnRelax')
df <- stackUnivariateDf(df, svymean(~mainRelax, design1, na.rm = TRUE), 'mainRsnRelax')
summary(svytable(~mainRelax+sex, design1), statistic = "adjWald")

df <- stackUnivariateDf(df, svyby(~mainExperiment, ~sex, design1, svymean, na.rm = TRUE),
                        'mainRsnExperiment')
df <- stackUnivariateDf(df, svymean(~mainExperiment, design1, na.rm = TRUE),
                        'mainRsnExperiment')
summary(svytable(~mainExperiment+sex, design1), statistic = "adjWald")

df <- stackUnivariateDf(df, svyby(~mainHigh, ~sex, design1, svymean, na.rm = TRUE),
                        'mainRsnHigh') 
df <- stackUnivariateDf(df, svymean(~mainHigh, design1, na.rm = TRUE), 'mainRsnHigh')
summary(svytable(~mainHigh+sex, design1), statistic = "adjWald")

df <- stackUnivariateDf(df, svyby(~mainSleep, ~sex, design1, svymean, na.rm = TRUE),
                        'mainRsnSleep') 
df <- stackUnivariateDf(df, svymean(~mainSleep, design1, na.rm = TRUE),
                        'mainRsnSleep')
summary(svytable(~mainSleep+sex, design1), statistic = "adjWald")

df <- stackUnivariateDf(df, svyby(~mainEmot, ~sex, design1, svymean, na.rm = TRUE),
                        'mainRsnEmotions')
df <- stackUnivariateDf(df, svymean(~mainEmot, design1, na.rm = TRUE), 'mainRsnEmotions')
summary(svytable(~mainEmot+sex, design1), statistic = 'adjWald')

df <- stackUnivariateDf(df, svyby(~mainOtherDrug, ~sex, design1, svymean, na.rm = TRUE),
                        'mainRsnOtherDrug')
df <- stackUnivariateDf(df, svymean(~mainOtherDrug, design1, na.rm = TRUE), 
                        'mainRsnOtherDrug')
summary(svytable(~mainOtherDrug+sex, design1), statistic = "adjWald")

df <- stackUnivariateDf(df, svyby(~mainHooked, ~sex, design1, svymean, na.rm = TRUE),
                        'mainRsnHooked')
df <- stackUnivariateDf(df, svymean(~mainHooked, design1, na.rm = TRUE),
                        'mainRsnHooked')
summary(svytable(~mainHooked+sex, design1), statistic = "adjWald")

df <- stackUnivariateDf(df, svyby(~mainOther, ~sex, design1, svymean, na.rm=TRUE),
                        'mainRsnOther')
df <- stackUnivariateDf(df, svymean(~mainOther, design1, na.rm = TRUE),
                        'mainRsnOther')
summary(svytable(~mainOther+sex, design1), statistic = 'adjWald')


write.csv(df, 'univariate_results.csv')
