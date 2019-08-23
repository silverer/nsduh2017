library(dplyr)
library(stats)
library(ggplot2)
library(psych)
library(survey)
library(psych)


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
  bigDf <- cbind(bigDf, new)
  return(bigDf)
}

directory <- getwd()
directory <- paste(directory, '/opioids/nsduh2017', sep='')
#directory <- "C:\\Users\\ers2244\\Documents\\opioids\\nsduh2017"


#df <- read.delim("C:\\Users\\ers2244\\Documents\\opioids\\NSDUH_2017_Tab.tsv", header = TRUE, sep = '\t')

# df1 <- dplyr::select(df, IRSEX, AGE2, HEALTH2, ANALWT_C, VEREP, VESTR, EDUHIGHCAT,
#                      IRINSUR4, PNRANYLIF, PNRANYREC, PNRNMLIF, PNRNMREC,DEPENDPNR,
#                      DEPNDPYPNR,
#                      PNRNMYR, PNRANYYR, NEWRACE2, INCOME, IRWRKSTAT18, IRMARIT,
#                      PNRWYNORX, PNRWYGAMT, PNRWYOFTN, PNRWYLNGR, PNRWYOTWY,
#                      PNRRSPAIN, PNRRSRELX, PNRRSEXPT, PNRRSHIGH, PNRRSSLEP,
#                      PNRRSEMOT, PNRRSDGFX, PNRRSHOOK, PNRRSSOR, PNRRSOTRS2,
#                      PNRRSMAIN, PNRNORXFG, SRCPNRNM2, IRMCDCHP, IRMEDICR, IRCHMPUS,
#                      IRPRVHLT, IROTHHLT)
# 
#file_loc <- paste(directory, '/opioids_cleaned_data_v1.csv', sep = '')
df1 <- read.csv('opioids_cleaned_data_v1.csv', header = TRUE)
#####DATA CLEANING--NO NEED TO RUN IF LOADING THE CLEANED DATASET####
df1$misusePY <- df1$PNRNMYR
df1$usePY <- df1$PNRANYYR


df1$anyPRUse <- NA
## 1 = has used PR in lifetime, 0 = no PR use in lifetime, na = invalid Input
df1$anyPRUse[df1$PNRANYLIF == 1 |df1$PNRANYLIF == 5] <- 1
df1$anyPRUse[df1$PNRANYLIF == 2] <- 0
df1$anyPRUse[df1$PNRANYLIF == 94 | df1$PNRANYLIF == 97 | df1$PNRANYLIF == 98] <- NA

#1 if currently married, 0 if not married, NA if <14 years old
df1$marStat[df1$IRMARIT == 1] <- 1
df1$marStat[df1$IRMARIT >1 & df1$IRMARIT < 99] <- 0
df1$marStat[df1$IRMARIT == 99] <- NA

#NA if younger than 18
df1$newAge <- df1$AGE2
df1$newAge[df1$AGE2 <= 6] <- NA
df1$newAgeF <- factor(df1$newAge, levels = c(7:17), labels = c('18', '19', '20', '21',
                                                               '22-23', '24-25', '26-29',
                                                               '30-34', '35-49', '50-64',
                                                               '65+'))
#filter out people younger than 18
df1 <- filter(df1, !is.na(df1$newAgeF))

#<22, <30, <50, 50+
df1$ageCourse[df1$newAge < 11] <- 1
df1$ageCourse[df1$newAge > 10 & df1$newAge <14] <- 2
df1$ageCourse[df1$newAge >13 & df1$newAge < 16] <- 3
df1$ageCourse[df1$newAge > 15] <- 4

df1$ageCourseF <- factor(df1$ageCourse, levels = c(1:4), labels = c('18-21', '22-29', '30-49', '50+'))

df1$employment <- factor(df1$IRWRKSTAT18, levels =c(1:4), labels = c('employed full time',
                                                                     'employed part time',
                                                                     'unemployed',
                                                                     'other'))
df1$education <- factor(df1$EDUHIGHCAT, levels = c(1:4), labels = c('less than high school',
                                                                    'high school grad',
                                                                    'some col/assoc dg',
                                                                    'college grad'))
df1$hasInsurance[df1$IRINSUR4 == 1] <- 1
df1$hasInsurance[df1$IRINSUR4 == 2] <- 0

#0 = no insurance
df1$insureType[df1$hasInsurance == 0] <- 0
df1$insureType[df1$IRMCDCHP == 1] <- 1
df1$insureType[df1$IRMEDICR == 1] <- 2
df1$insureType[df1$IRCHMPUS == 1] <- 3
df1$insureType[df1$IRPRVHLT == 1] <- 4
df1$insureType[df1$IROTHHLT == 1] <- 5

df1$insureTypeF <- factor(df1$insureType, levels = c(0:5), labels = c('none', 
                                                                      'medicaid', 
                                                                      'medicare',
                                                                      'military',
                                                                      'private',
                                                                      'other'))
df1$raceF <- factor(df1$NEWRACE2, levels = c(1:7), labels = c('NHW', 'Black',
                                                              'Native Am/AK', 'pacific isl',
                                                              'asian', 'more than one',
                                                              'hispanic'))
df1$raceRecode <- df1$raceF

levels(df1$raceRecode) <- c('NHW', 'Black',
                            'Native Am/AK/pac isl', 'Native Am/AK/pac isl',
                            'asian', 'more than one',
                            'hispanic')


#1 = yes misuse, 0 = no misuse
#NA = no mis/use/invalid input
df1$anyPRMisuse <- NA
df1$anyPRMisuse[df1$PNRNMLIF == 1 | df1$PNRNMLIF == 5] <- 1
df1$anyPRMisuse[df1$PNRNMLIF == 2 | df1$PNRNMLIF == 91] <- 0
df1$anyPRMisuse[df1$PNRNMLIF >= 85 & df1$PNRNMLIF != 91] <- NA

#1 = medical source, 2 = personal network, 0 = invalid
df1$source <- NA
df1$source[df1$SRCPNRNM2 < 4] <- 1
df1$source[df1$SRCPNRNM2 > 3] <- 2
df1$source <- factor(df1$source, levels = c(1:2), labels = c('medical', 'personal'))
#df1$source[is.na(df1$SRCPNRNM2)] <- 0

df1$sourceFull <- df1$SRCPNRNM2
df1$sourceFull[is.na(df1$SRCPNRNM2)] <- NA

#1 = yes MDE, 2 = no MDE, 0 = invalid response
df1$depressed <- df1$AMDEYR
df1$depressed[is.na(df1$AMDEYR)] <- 0


#1 = last year, 2 = more than 12 mo ago
#0 = invalid input/never used
df1$mostRecentAny <- NA
df1$mostRecentAny[df1$PNRANYREC == 1] <- 1
df1$mostRecentAny[df1$PNRANYREC == 2 |df1$PNRANYREC == 9 | df1$PNRANYREC == 83] <- 2
df1$mostRecentAny[df1$PNRANYREC >= 91] <- 0

#1 = last year, 2 = more than 12 mo ago, 
#0 = invalid input/never used
df1$mostRecentMisuse <- NA
df1$mostRecentMisuse[df1$PNRNMREC < 3 | df1$PNRNMREC == 8 |df1$PNRNMREC ==11] <- 1
df1$mostRecentMisuse[df1$PNRNMREC == 3| df1$PNRNMREC == 9 | df1$PNRNMREC == 83] <- 2
df1$mostRecentMisuse[df1$PNRNMREC >= 91] <- 0

#1 = without own rx, 0 = with own rx, na = invalid input
df1$noOwnRx <- NA
df1$noOwnRx[df1$PNRWYNORX == 1] <- 1
df1$noOwnRx[df1$PNRWYNORX == 2] <- 0


#1 = yes, 0 = no, NA = invalid input/no past year misuse
df1$greaterAmnt <- NA
df1$greaterAmnt[df1$PNRWYGAMT == 1] <- 1
df1$greaterAmnt[df1$PNRWYGAMT == 2] <- 0


#1 = yes, 0 = no, NA = invalid input/no past year misuse
df1$moreOften <- NA
df1$moreOften[df1$PNRWYOFTN == 1] <- 1
df1$moreOften[df1$PNRWYOFTN == 2] <- 0


#1 = yes, 0 = no, NA = invalid input/no past year misuse
df1$longer <- NA
df1$longer[df1$PNRWYLNGR == 1] <- 1
df1$longer[df1$PNRWYLNGR == 2] <- 0


#### REASONS ####

#1 = pain, 2 = relax, 3 = experiment, 4 = feel good/high
#5 = sleep, 6 = emotions, 7 = other drug fx, 8 = hooked
#9 = other
df1$mainRsn <- NA
df1$mainRsn[df1$PNRRSMAIN == 1 | df1$PNRRSMAIN == 11] <- 1
df1$mainRsn[df1$PNRRSMAIN == 2] <- 2
df1$mainRsn[df1$PNRRSMAIN == 3 | df1$PNRRSMAIN == 13] <- 3
df1$mainRsn[df1$PNRRSMAIN == 4] <- 4
df1$mainRsn[df1$PNRRSMAIN == 5] <- 5
df1$mainRsn[df1$PNRRSMAIN == 6 | df1$PNRRSMAIN == 16] <- 6
df1$mainRsn[df1$PNRRSMAIN == 7] <- 7
df1$mainRsn[df1$PNRRSMAIN == 8] <- 8
df1$mainRsn[df1$PNRRSMAIN == 9] <- 9
#df1$mainRsn[df1$PNRRSMAIN >= 83] <- 0
temp <- filter(df1, misusePY == 1)

#1 = yes, 0 = no, NA = invalid
df1$pain <- NA
df1$pain[df1$PNRRSPAIN == 1 | df1$PNRRSPAIN == 3] <- 1
df1$pain[df1$PNRRSPAIN == 2] <- 0


df1$relax <- NA
df1$relax[df1$PNRRSRELX == 1] <- 1
df1$relax[df1$PNRRSRELX == 2] <- 0


df1$exp <- NA
df1$exp[df1$PNRRSEXPT == 1 | df1$PNRRSEXPT == 3] <- 1
df1$exp[df1$PNRRSEXPT == 2] <- 0

df1$high <- NA
df1$high[df1$PNRRSHIGH == 1| df1$PNRRSHIGH == 3] <- 1
df1$high[df1$PNRRSHIGH == 2] <- 0


df1$sleep <- NA
df1$sleep[df1$PNRRSSLEP == 1 | df1$PNRRSSLEP == 3] <- 1
df1$sleep[df1$PNRRSSLEP == 2] <- 0


df1$emot <- NA
df1$emot[df1$PNRRSEMOT == 1 | df1$PNRRSEMOT == 3] <- 1
df1$emot[df1$PNRRSEMOT == 2] <- 0

df1$otherDrug <- NA
df1$otherDrug[df1$PNRRSDGFX == 1] <- 1
df1$otherDrug[df1$PNRRSDGFX == 2] <- 0


df1$hooked <- NA
df1$hooked[df1$PNRRSHOOK == 1 | df1$PNRRSHOOK == 3] <- 1
df1$hooked[df1$PNRRSHOOK == 2] <- 0



df1$dependPYO <- df1$DEPNDPYPNR

df1$dependPY <- NA
#0 = no dependence, 1 = yes dependence, NA = not valid
df1$dependPY[df1$dependPYO == 1] <- 1
df1$dependPY[df1$dependPYO == 0] <- 0

df1$IRSEX <- factor(df1$IRSEX, labels=c("Men", "Women"))

df1 <- fastDummies::dummy_cols(df1, select_columns = "sourceFull")
colnames(df1)[names(df1) == 'sourceFull_1'] <- "oneDoc"
colnames(df1)[names(df1) == 'sourceFull_2'] <- "multDocs"
colnames(df1)[names(df1) == 'sourceFull_3'] <- "stoleDoc"
colnames(df1)[names(df1) == 'sourceFull_4'] <- "gotFrRel"
colnames(df1)[names(df1) == 'sourceFull_5'] <- "boughtFrRel"
colnames(df1)[names(df1) == 'sourceFull_6'] <- "tookFrRel"
colnames(df1)[names(df1) == 'sourceFull_7'] <- "drugDeal"
colnames(df1)[names(df1) == 'sourceFull_8'] <- "otherSrc"

df1 <- fastDummies::dummy_cols(df1, select_columns = "mainRsn")

colnames(df1)[names(df1) == 'mainRsn_1'] <- "mainPain"
colnames(df1)[names(df1) == 'mainRsn_2'] <- "mainRelax"
colnames(df1)[names(df1) == 'mainRsn_3'] <- "mainExperiment"
colnames(df1)[names(df1) == 'mainRsn_4'] <- "mainHigh"
colnames(df1)[names(df1) == 'mainRsn_5'] <- "mainSleep"
colnames(df1)[names(df1) == 'mainRsn_6'] <- "mainEmotions"
colnames(df1)[names(df1) == 'mainRsn_7'] <- "mainOtherDrug"
colnames(df1)[names(df1) == 'mainRsn_8'] <- "mainHooked"
colnames(df1)[names(df1) == 'mainRsn_9'] <- "mainOther"


write.csv(df1, paste(directory, '\\opioids_cleaned_data_v1.csv', sep=''))


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
  
  sex = IRSEX,
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
  noOwnRx = noOwnRx,
  greaterAmnt = greaterAmnt,
  longer = longer,
  moreOften = moreOften
)



lifetime_use <- svyglm(anyUse~sex+employment+ed+race+age+hasInsurance, design=design1,
                        family=quasibinomial())
summary(lifetime_use)
confIntDf <- confIntToDf(lifetime_use, 'lifetime_use')
summaryDf <- summaryToDf(lifetime_use, 'lifetime_use')


lifetime_misuse <- svyglm(anyMisUse~sex+employment+ed+race+age+hasInsurance, design=design1,
                          family=quasibinomial())

confIntDf <- mergeCiDfs(confIntDf,lifetime_misuse, 'lifetime_misuse')
summaryDf <- mergeSummaryDfs(summaryDf, lifetime_misuse, 'lifetime_misuse')



pastyr_use <- svyglm(pyUse~sex+employment+ed+race+age+hasInsurance, design=design1,
                       family=quasibinomial())
confIntDf <- mergeCiDfs(confIntDf, pastyr_use, 'pastyr_use')
summaryDf <- mergeSummaryDfs(summaryDf, pastyr_use, 'pastyr_use')

pastyr_misuse <- svyglm(pyMisuse~sex+employment+ed+race+age+hasInsurance, design=design1,
                          family=quasibinomial())
confIntDf <- mergeCiDfs(confIntDf, pastyr_misuse, 'pastyr_misuse')
summaryDf <- mergeSummaryDfs(summaryDf, pastyr_misuse, 'pastyr_misuse')

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
  
  sex = IRSEX,
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
  mainExp = mainExperiment,
  noOwnRx = noOwnRx,
  greaterAmnt = greaterAmnt,
  longer = longer,
  moreOften = moreOften
)


srcOneDoc <- svyglm(oneDoc~sex+employment+ed+race+age+hasInsurance, design=design1,
                        family=quasibinomial())
confIntDf <- mergeCiDfs(confIntDf, srcOneDoc, 'srcOneDoc')
summaryDf <- mergeSummaryDfs(summaryDf, srcOneDoc, 'srcOneDoc')

srcMultDocs <- svyglm(multDocs~sex+employment+ed+race+age+hasInsurance, design=design1,
                      family=quasibinomial())
confIntDf <- mergeCiDfs(confIntDf, srcMultDocs, 'srcMultDocs')
summaryDf <- mergeSummaryDfs(summaryDf, srcMultDocs, 'srcMultDocs')

srcDrugDeal <- svyglm(drugDeal~sex+employment+ed+race+age+hasInsurance, design=design1,
                      family=quasibinomial())
confIntDf <- mergeCiDfs(confIntDf, srcDrugDeal, 'srcDrugDeal')
summaryDf <- mergeSummaryDfs(summaryDf, srcDrugDeal, 'srcDrugDeal')

srcBoughtFrRel <- svyglm(boughtFrRel~sex+employment+ed+race+age+hasInsurance, design=design1,
                      family=quasibinomial())
confIntDf <- mergeCiDfs(confIntDf, srcBoughtFrRel, 'srcBoughtFrRel')
summaryDf <- mergeSummaryDfs(summaryDf, srcBoughtFrRel, 'srcBoughtFrRel')

srcTookFrRel <- svyglm(tookFrRel~sex+employment+ed+race+age+hasInsurance, design=design1,
                         family=quasibinomial())
confIntDf <- mergeCiDfs(confIntDf, srcTookFrRel, 'srcTookFrRel')
summaryDf <- mergeSummaryDfs(summaryDf, srcTookFrRel, 'srcTookFrRel')

srcGotFrRel <- svyglm(gotFrRel~sex+employment+ed+race+age+hasInsurance, design=design1,
                         family=quasibinomial())
confIntDf <- mergeCiDfs(confIntDf, srcGotFrRel, 'gotFrRel')
summaryDf <- mergeSummaryDfs(summaryDf, srcGotFrRel, 'gotFrRel')

opDepend <- svyglm(depend~sex+employment+ed+race+age+hasInsurance, design=design1,
                   family=quasibinomial())
confIntDf <- mergeCiDfs(confIntDf, opDepend, 'dependence')
summaryDf <- mergeSummaryDfs(summaryDf, opDepend, 'dependence')

typeNoRx <- svyglm(noOwnRx~sex+employment+ed+race+age+hasInsurance, design=design1,
                   family=quasibinomial())
confIntDf <- mergeCiDfs(confIntDf, typeNoRx, 'noOwnRx')
summaryDf <- mergeSummaryDfs(summaryDf, typeNoRx, 'noOwnRx')

typeGreaterAmnt <- svyglm(greaterAmnt~sex+employment+ed+race+age+hasInsurance, design=design1,
                          family=quasibinomial())
confIntDf <- mergeCiDfs(confIntDf, typeGreaterAmnt, 'greaterAmnt')
summaryDf <- mergeSummaryDfs(summaryDf, typeGreaterAmnt, 'greaterAmnt')

typeLonger <- svyglm(longer~sex+employment+ed+race+age+hasInsurance, design=design1,
                     family=quasibinomial())
confIntDf <- mergeCiDfs(confIntDf, typeLonger, 'longer')
summaryDf <- mergeSummaryDfs(summaryDf, typeLonger, 'longer')

typeMoreOften <- svyglm(moreOften~sex+employment+ed+race+age+hasInsurance, design=design1,
                        family=quasibinomial())
confIntDf <- mergeCiDfs(confIntDf, typeMoreOften, 'moreOften')
summaryDf <- mergeSummaryDfs(summaryDf, typeMoreOften, 'moreOften')

rsnPain <- svyglm(mainPain~sex+employment+ed+race+age+hasInsurance, design=design1,
                  family=quasibinomial())
confIntDf <- mergeCiDfs(confIntDf, rsnPain, 'rsnPain')
summaryDf <- mergeSummaryDfs(summaryDf, rsnPain, 'rsnPain')

rsnEmot <- svyglm(mainEmot~sex+employment+ed+race+age+hasInsurance, design=design1,
                  family=quasibinomial())
confIntDf <- mergeCiDfs(confIntDf,  rsnEmot, 'rsnEmot')
summaryDf <- mergeSummaryDfs(summaryDf, rsnEmot, 'rsnEmot')

rsnHigh <- svyglm(mainHigh~sex+employment+ed+race+age+hasInsurance, design=design1,
                  family=quasibinomial())
confIntDf <- mergeCiDfs(confIntDf,  rsnHigh, 'rsnHigh')
summaryDf <- mergeSummaryDfs(summaryDf, rsnHigh, 'rsnHigh')

rsnRelax <- svyglm(mainRelax~sex+employment+ed+race+age+hasInsurance, design=design1,
                  family=quasibinomial())
confIntDf <- mergeCiDfs(confIntDf, rsnRelax, 'rsnRelax')
summaryDf <- mergeSummaryDfs(summaryDf, rsnRelax, 'rsnRelax')

rsnExp <- svyglm(mainExp~sex+employment+ed+race+age+hasInsurance, design=design1,
                  family=quasibinomial())
confIntDf <- mergeCiDfs(confIntDf, rsnExp, 'rsnExp')
summaryDf <- mergeSummaryDfs(summaryDf, rsnExp, 'rsnExp')

rsnSleep <- svyglm(mainSleep~sex+employment+ed+race+age+hasInsurance, design=design1,
                  family=quasibinomial())
confIntDf <- mergeCiDfs(confIntDf, rsnSleep, 'rsnSleep')
summaryDf <- mergeSummaryDfs(summaryDf, rsnSleep, 'rsnSleep')

write.csv(confIntDf, paste(directory, '\\conf_ints_log_reg_v1.csv', sep=''))
write.csv(summaryDf, paste(directory, '\\summary_log_reg_v1.csv', sep=''))

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
  
  sex = IRSEX,
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
  otherSrc = otherSrc,
  noOwnRx = noOwnRx,
  greaterAmnt = greaterAmnt,
  longer = longer,
  moreOften = moreOften
)

#DEMOGRAPHICS
employment <- svyby(~employment, ~sex, design1, svymean)
confint(employment)
employmentAll <-  svymean(~employment, design1)
result <- summary(svytable(~employment+sex, design1), statistic = 'adjWald')

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
confint(marComp)
tbl <- svytable(~marStat+sex, design1)
summary(tbl, statistic = 'adjWald')
marA <- svymean(~marStat, design1)
marA
confint(marA)

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
  
  sex = IRSEX,
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
  otherSrc = otherSrc,
  noOwnRx = noOwnRx,
  greaterAmnt = greaterAmnt,
  longer = longer,
  moreOften = moreOften
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
  
  sex = IRSEX,
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
  otherSrc = otherSrc,
  noOwnRx = noOwnRx,
  greaterAmnt = greaterAmnt,
  longer = longer,
  moreOften = moreOften
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
  
  sex = IRSEX,
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
  
  sex = IRSEX,
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
  mainExp = mainExperiment,
  noOwnRx = noOwnRx,
  greaterAmnt = greaterAmnt,
  longer = longer,
  moreOften = moreOften
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

df <- stackUnivariateDf(df, svyby(~noOwnRx, ~sex, design1, svymean, na.rm = TRUE),
                        'noOwnRx')
df <- stackUnivariateDf(df, svymean(~noOwnRx, design1, na.rm = TRUE),
                        'noOwnRx')
summary(svytable(~noOwnRx+sex, design1), statistic = "adjWald")

df <- stackUnivariateDf(df, svyby(~moreOften, ~sex, design1, svymean, na.rm = TRUE),
                        'moreOften')
df <- stackUnivariateDf(df, svymean(~moreOften, design1, na.rm = TRUE),
                        'moreOften')
summary(svytable(~moreOften+sex, design1), statistic = "adjWald")

df <- stackUnivariateDf(df, svyby(~greaterAmnt, ~sex, design1, svymean, na.rm = TRUE),
                        'greaterAmnt')
df <- stackUnivariateDf(df, svymean(~greaterAmnt, design1, na.rm = TRUE),
                        'greaterAmnt')
summary(svytable(~greaterAmnt+sex, design1), statistic = "adjWald")

df <- stackUnivariateDf(df, svyby(~longer, ~sex, design1, svymean, na.rm = TRUE),
                        'longer')
df <- stackUnivariateDf(df, svymean(~longer, design1, na.rm = TRUE),
                        'longer')
summary(svytable(~longer+sex, design1), statistic = "adjWald")
write.csv(df, 'univariate_results.csv')
