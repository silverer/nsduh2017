library(dplyr)
library(stats)
library(ggplot2)
library(psych)
library(survey)
library(psych)
library(oddsratio)

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


directory <- "C:\\Users\\ers2244\\Documents\\opioids\\nsduh2017"


df <- read.delim("C:\\Users\\ers2244\\Documents\\opioids\\NSDUH_2017_Tab.tsv", header = TRUE, sep = '\t')

df1 <- dplyr::select(df, IRSEX, AGE2, HEALTH2, ANALWT_C, VEREP, VESTR, EDUHIGHCAT,
                     IRINSUR4, PNRANYLIF, PNRANYREC, PNRNMLIF, PNRNMREC,DEPENDPNR,
                     DEPNDPYPNR,
                     PNRNMYR, PNRANYYR, NEWRACE2, INCOME, IRWRKSTAT18, IRMARIT,
                     PNRWYNORX, PNRWYGAMT, PNRWYOFTN, PNRWYLNGR, PNRWYOTWY,
                     PNRRSPAIN, PNRRSRELX, PNRRSEXPT, PNRRSHIGH, PNRRSSLEP,
                     PNRRSEMOT, PNRRSDGFX, PNRRSHOOK, PNRRSSOR, PNRRSOTRS2,
                     PNRRSMAIN, PNRNORXFG, SRCPNRNM2, IRMCDCHP, IRMEDICR, IRCHMPUS,
                     IRPRVHLT, IROTHHLT)


#####DATA CLEANING####
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
emp <- svyby(~employment, ~sex, design1, svymean)
emp
confint(emp)
tbl <- svytable(~employment+sex, design1)
result <- summary(tbl, statistic = 'adjWald')
empA <- svymean(~employment, design1)
empA
confint(empA)

raceComp <- svyby(~race, ~sex, design1, svymean)
raceComp
confint(raceComp)
tbl<- svytable(~race+sex, design1)
summary(tbl, statistic = 'adjWald')
raceA <- svymean(~race, design1)
raceA
confint(raceA)

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

#Lifetime use
lifetimeUse <- svyby(~anyUse, ~sex, design1, svymean)
df <- buildUnivariateDf(lifetimeUse, 'lifetimeUse')
useAll <- svymean(~anyUse, design1)
df <- stackUnivariateDf(df, useAll, 'lifetimeUse')

result <- summary(svytable(~anyUse+sex, design1), statistic = 'adjWald')


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
write.csv(df, paste(directory, '\\test_univariate_outs.csv', sep = ""))



summary(svytable(~multDocs+sex, design1), statistic = "adjWald")

tbl <- svytable(~stoleDoc+IRSEX, nsduh_design)
summary(tbl, statistic = "Wald")

drugDealTest <- svyby(~drugDeal, ~sex, design1, svymean, na.rm = TRUE)
drugDealTest
confint(drugDealTest)
drugDealA <- svymean(~drugDeal, design1, na.rm = TRUE)
drugDealA
confint(drugDealTest, level = 0.95)
tbl <- svytable(~drugDeal+sex, design1)
summary(tbl, statistic = "adjWald")

tempAny <- filter(df1, anyPRUse > 0)
nsduh_design <- defineDes(tempAny)

tbl <- svytable(~anyPRUse+IRSEX, nsduh_design)
summary(tbl, statistic = "Wald")


tempRx <- filter(df1, !is.na(df1$noOwnRx))
nsduh_design <- defineDes(df1)
design1 <- update(
  nsduh_design,
  employment = employment,
  race = raceF,
  ed = education,
  hasInsurance = hasInsurance,
  insureType = insureTypeF,
  marStat = marStat,
  age = ageCourseF,
  noOwnRx = noOwnRx,
  sex = IRSEX
)



woRx <- svyby(~noOwnRx, ~sex, design1, svymean, na.rm = TRUE)
confint(woRx)

tbl <- svytable(~noOwnRx+sex, design1)
summary(tbl, statistic = "Wald")

tempType <- filter(df1, !is.na(df1$moreOften) & !is.na(df1$longer) & !is.na(df1$greaterAmnt) & !is.na(df1$noOwnRx))

nsduh_design <- defineDes(tempType)
design1 <- update(
  nsduh_design,
  greaterAmnt = greaterAmnt,
  longer = longer,
  sexIsM = ifelse(IRSEX == "Men", 1, 0),
  moreOften = moreOften,
  noOwnRx = noOwnRx,
  employment = employment,
  race = raceF,
  ed = education,
  hasInsurance = hasInsurance,
  insureType = insureTypeF,
  marStat = marStat,
  age = ageCourseF,
  sex = IRSEX
  
)



moreOftenTest <- svyby(~moreOften, ~sex, design1, svymean, na.rm = TRUE)
confint(moreOftenTest)


tbl <- svytable(~moreOften+sex, design1)
summary(tbl, statistic = "Wald")

tempAmnt <- filter(df1, greaterAmnt >0)
nsduh_design <- defineDes(tempAmnt)
design1 <- update(
  nsduh_design,
  
  greaterAmnt = ifelse(greaterAmnt == 1, 1, 0),
  sex = IRSEX
)
amnt <- svyby(~greaterAmnt, ~sex, design1, svymean, na.rm = TRUE)
confint(amnt)
tbl <- svytable(~greaterAmnt+sex, design1)
summary(tbl, statistic = "Wald")

tempLonger <- filter(df1, longer>0)
nsduh_design <- defineDes(tempLonger)
design1 <- update(
  nsduh_design,
  
  longer = ifelse(longer == 1, 1, 0),
  sex = IRSEX
)
longerTest <- svyby(~longer, ~sex, design1, svymean, na.rm = TRUE)
confint(longerTest)

tbl <- svytable(~longer+sex, design1)
summary(tbl, statistic = "Wald")

#### REASONS ####
#1 = pain, 2 = relax, 3 = experiment, 4 = feel good/high
#5 = sleep, 6 = emotions, 7 = other drug fx, 8 = hooked
#9 = other
tempMainRsn <- filter(df1, !is.na(mainRsn))
dummyRsn <- fastDummies::dummy_cols(tempMainRsn, select_columns = "mainRsn")

colnames(dummyRsn)[names(dummyRsn) == 'mainRsn_1'] <- "mainPain"
colnames(dummyRsn)[names(dummyRsn) == 'mainRsn_2'] <- "mainRelax"
colnames(dummyRsn)[names(dummyRsn) == 'mainRsn_3'] <- "mainExperiment"
colnames(dummyRsn)[names(dummyRsn) == 'mainRsn_4'] <- "mainHigh"
colnames(dummyRsn)[names(dummyRsn) == 'mainRsn_5'] <- "mainSleep"
colnames(dummyRsn)[names(dummyRsn) == 'mainRsn_6'] <- "mainEmotions"
colnames(dummyRsn)[names(dummyRsn) == 'mainRsn_7'] <- "mainOtherDrug"
colnames(dummyRsn)[names(dummyRsn) == 'mainRsn_8'] <- "mainHooked"
colnames(dummyRsn)[names(dummyRsn) == 'mainRsn_9'] <- "mainOther"

nsduh_design <- defineDes(dummyRsn)

design1 <- update(
  nsduh_design,
  
  mainPain = mainPain,
  mainRelax = mainRelax,
  mainExperiment = mainExperiment,
  mainHigh = mainHigh,
  mainSleep = mainSleep,
  mainEmotions = mainEmotions,
  mainOtherDrug = mainOtherDrug,
  mainHooked = mainHooked,
  mainOther = mainOther,
  employment = employment,
  race = raceF,
  ed = education,
  hasInsurance = hasInsurance,
  insureType = insureTypeF,
  marStat = marStat,
  age = ageCourseF,
  sexIsM = ifelse(IRSEX == "Men", 1, 0),
  sex = IRSEX
)

mainRPain <- svyby(~mainPain, ~sex, design1, svymean, na.rm = TRUE)
confint(mainRPain)
mainRPainA <- svymean(~mainPain, design1, na.rm = TRUE)
tbl <- svytable(~mainPain+sex, design1)
summary(tbl, statistic = "adjWald")



mainRRelax <- svyby(~mainRelax, ~sex, design1, svymean, na.rm = TRUE)
confint(mainRRelax)
tbl <- svytable(~mainRelax+sex, design1)
summary(tbl, statistic = "Wald")


mainRExp <- svyby(~mainExperiment, ~sex, design1, svymean, na.rm = TRUE)
confint(mainRExp)
tbl <- svytable(~mainExperiment+sex, design1)
summary(tbl, statistic = "Wald")


mainRHigh <- svyby(~mainHigh, ~sex, design1, svymean, na.rm = TRUE)
mainRHighA <- svymean(~mainHigh, design1, na.rm = TRUE)
confint(mainRHigh)
tbl <- svytable(~mainHigh+sex, design1)
summary(tbl, statistic = "adjWald")


test <-svyglm(mainHigh~sexIsM, design = design1, family = quasibinomial())
summary(test)
oddsR <- exp(cbind(OR = coef(test), confint(test, level =0.95)))
oddsR

mainRSleep <- svyby(~mainSleep, ~sex, design1, svymean, na.rm = TRUE)
confint(mainRSleep)
tbl <- svytable(~mainSleep+sex, design1)
summary(tbl, statistic = "Wald")


mainEmoAll <- svymean(~mainEmotions, design1, na.rm = TRUE)
mainEmoAll
confint(mainEmoAll)

tbl <- table(dummyRsn$IRSEX, dummyRsn$mainEmotions)
tbl


mainREmo <- svyby(~mainEmotions, ~sex, design1, svymean, na.rm = TRUE)
confint(mainREmo)
tbl <- svytable(~mainEmotions+sex, design1)
summary(tbl, statistic = "Wald")



tbl <- svytable(~mainOtherDrug+IRSEX, nsduh_design)
summary(tbl, statistic = "Wald")



mainRHook <- svyby(~mainHooked, ~sex, design1, svymean, na.rm = TRUE)
confint(mainRHook)
tbl <- svytable(~mainHooked+sex, design1)
summary(tbl, statistic = "Wald")



tbl <- svytable(~mainOther+IRSEX, nsduh_design)
summary(tbl, statistic = "Wald")


#### OLD ####
test <-svyglm(oneDoc~sexIsM+employment+hasInsurance+marStat+age+ed, design = design1, family = quasibinomial())
summary(test)
oddsR <- exp(cbind(OR = coef(test), confint(test, level =0.95)))
oddsR

tbl <- svytable(~sexIsM, design1)
summary(tbl, statistic = "adjWald")
test <-svyglm(mainHooked~sexIsM+employment+hasInsurance+age+marStat+ed, design = design1, family = quasibinomial())
summary(test)
oddsR <- exp(cbind(OR = coef(test), confint(test, level =0.95)))
oddsR

test <- svyglm(depend~sexIsM+ed+employment+age+hasInsurance+marStat, design = design1, family = quasibinomial())
summary(test)
oddsR <- exp(cbind(OR = coef(test), confint(test, level =0.95)))
oddsR

test <-svyglm(gotFrRel~sexIsM+employment+hasInsurance+marStat+age+ed, design = design1, family = quasibinomial())
summary(test)

test <-svyglm(pyMisuse~sexIsM+ed+employment+age+hasInsurance+marStat, design = design1, family = quasibinomial())
summary(test)
oddsR <- exp(cbind(OR = coef(test), confint(test, level =0.95)))
oddsR
test <-svyglm(medSource~sexIsM, design = design1, family = quasibinomial())
summary(test)
test <-svyglm(medSource~sexIsM+employment+hasInsurance+marStat+age+ed, design = design1, family = quasibinomial())
summary(test)

oddsR <- exp(cbind(OR = coef(test), confint(test, level =0.95)))
oddsR


test <-svyglm(gotFrRel~sexIsM, design = design1, family = quasibinomial())
summary(test)
oddsR <- exp(cbind(OR = coef(test), confint(test, level =0.95)))
oddsR

test <-svyglm(mainRelax~sexIsM+employment+hasInsurance+age+marStat+ed, design = design1, family = quasibinomial())
summary(test)
oddsR <- exp(cbind(OR = coef(test), confint(test, level =0.95)))
oddsR

test <-svyglm(tookFrRel~sexIsM, design = design1, family = quasibinomial())
summary(test)
test <-svyglm(tookFrRel~sexIsM+employment+hasInsurance+marStat+age+ed, design = design1, family = quasibinomial())
summary(test)

oddsR <- exp(cbind(OR = coef(test), confint(test, level =0.95)))
oddsR

test <-svyglm(pyUse~sexIsM+ed+employment+age+hasInsurance+marStat, design = design1, family = quasibinomial())
summary(test)
oddsR <- exp(cbind(OR = coef(test), confint(test, level =0.95)))
oddsR



test <-svyglm(boughtFrRel~sexIsM, design = design1, family = quasibinomial())
summary(test)
test <-svyglm(boughtFrRel~sexIsM+employment+hasInsurance+marStat+age+ed, design = design1, family = quasibinomial())
summary(test)

oddsR <- exp(cbind(OR = coef(test), confint(test, level =0.95)))
oddsR


test <-svyglm(mainOtherDrug~sexIsM+employment+hasInsurance+age+marStat+ed, design = design1, family = quasibinomial())
summary(test)
oddsR <- exp(cbind(OR = coef(test), confint(test, level =0.95)))
oddsR

test <- svyglm(anyPRUse~sexIsM+ed+employment+age+hasInsurance+marStat, design = design1, family = quasibinomial())
summary(test)
oddsR <- exp(cbind(OR = coef(test), confint(test, level =0.95)))
oddsR
w <- wald.test(b = coef(test), Sigma = vcov(test), Terms = 1)
w

test <-svyglm(mainExperiment~sexIsM+employment+hasInsurance+age+marStat+ed, design = design1, family = quasibinomial())
summary(test)
oddsR <- exp(cbind(OR = coef(test), confint(test, level =0.95)))
oddsR

test <-svyglm(mainPain~sexIsM+employment+hasInsurance+age+marStat+ed, design = design1, family = quasibinomial())
summary(test)
oddsR <- exp(cbind(OR = coef(test), confint(test, level =0.95)))
oddsR

test <-svyglm(noOwnRx~sex+employment+ed+hasInsurance+marStat+age, design = design1, family = quasibinomial())
summary(test)
oddsR <- exp(cbind(OR = coef(test), confint(test, level =0.95)))
oddsR

test <-svyglm(multDocs~sexIsM, design = design1, family = quasibinomial())
summary(test)

test <-svyglm(multDocs~sexIsM+employment+hasInsurance+marStat+age+ed, design = design1, family = quasibinomial())
summary(test)

oddsR <- exp(cbind(OR = coef(test), confint(test, level =0.95)))
oddsR
test <-svyglm(stoleDoc~sexIsM, design = design1, family = quasibinomial())
summary(test)
test <-svyglm(stoleDoc~sexIsM+employment+hasInsurance+marStat+age+ed, design = design1, family = quasibinomial())
summary(test)

oddsR <- exp(cbind(OR = coef(test), confint(test, level =0.95)))
oddsR


test <-svyglm(greaterAmnt~sexIsM+employment+ed+hasInsurance+marStat+age, design = design1, family = quasibinomial())
summary(test)
oddsR <- exp(cbind(OR = coef(test), confint(test, level =0.95)))
oddsR

test <-svyglm(longer~sexIsM+employment+ed+hasInsurance+marStat+age, design = design1, family = quasibinomial())
summary(test)
oddsR <- exp(cbind(OR = coef(test), confint(test, level =0.95)))
oddsR

test <-svyglm(moreOften~sexIsM+employment+ed+hasInsurance+marStat+age, design = design1, family = quasibinomial())
summary(test)
oddsR <- exp(cbind(OR = coef(test), confint(test, level =0.95)))


test <-svyglm(mainEmotions~sexIsM+employment+hasInsurance+age+marStat+ed, design = design1, family = quasibinomial())
summary(test)
oddsR <- exp(cbind(OR = coef(test), confint(test, level =0.95)))
oddsR

test <-svyglm(mainHigh~sexIsM+employment+hasInsurance+age+marStat+ed, design = design1, family = quasibinomial())
summary(test)
oddsR <- exp(cbind(OR = coef(test), confint(test, level =0.95)))
oddsR


test <-svyglm(anyPRMisuse~sexIsM+ed+employment+age+hasInsurance+marStat, design = design1, family = quasibinomial())
summary(test)
oddsR <- exp(cbind(OR = coef(test), confint(test, level =0.95)))
oddsR

test <-svyglm(mainSleep~sexIsM+employment+hasInsurance+age+marStat+ed, design = design1, family = quasibinomial())
summary(test)
oddsR <- exp(cbind(OR = coef(test), confint(test, level =0.95)))
oddsR

chisq <- chisq.test(tempMisuse$AGE2, tempMisuse$anyPRMisuse)
chisq

chisq <- chisq.test(tempMisuse$race, tempMisuse$anyPRMisuse)
chisq

tempAnyRec <- filter(df1, mostRecentAny > 0)
chisq <- chisq.test(tempAnyRec$IRSEX, tempAnyRec$mostRecentAny)
chisq
title <- "Most recent PR use any"
xNames <- c("within year", "not within year")
plotCrosstabs(title, xNames, tempAnyRec$IRSEX, tempAnyRec$mostRecentAny)

chisq <- chisq.test(tempSource$IRSEX, tempSource$source)
chisq
title <- "Source of Opioid for Last Misuse by Gender"
xNames <- c("Medical source", "Nonmedical source")
plotCrosstabs(title, xNames, tempSource$IRSEX, tempSource$source)

title <- "Got from one doc"
xNames <- c("No", "Yes")
plotCrosstabs(title, xNames, dummySource$IRSEX, dummySource$oneDoc)

#dummySource$mainRsnPain <- NA
#dummySource$mainRsnPain[dummySource$mainRsn == 1] <- 1
#dummySource$mainRsnPain[is.na(dummySource$mainRsnPain)] <- 0
dummySource <- filter(dummySource, pain == 1|mainRsn == 1)
chisq <- chisq.test(dummySource$oneDoc, dummySource$IRSEX)
chisq
dummySource$sexTable <- NA
dummySource$sexTable[dummySource$IRSEX == 1] <- "Men"
dummySource$sexTable[dummySource$IRSEX == 2] <- "Women"
dummySource$docTable[dummySource$oneDoc == 0] <- "Somewhere else"
dummySource$docTable[dummySource$oneDoc == 1] <- "one doctor"
counts <- table(dummySource$sexTable, dummySource$docTable)
counts
title <- "One doc vs high"
xNames <- c("pain", "not pain")
#note- women/men actually = not from 1 doc/from one doc
test <-svyglm(noOwnRx~sexIsM+employment+ed+hasInsurance+marStat+age, design = design1, family = quasibinomial())
summary(test)
oddsR <- exp(cbind(OR = coef(test), confint(test, level =0.95)))
oddsR

chisq <- chisq.test(dummySource$IRSEX, dummySource$multDocs)
chisq

chisq <- chisq.test(dummySource$IRSEX, dummySource$stoleDoc)
chisq
chisq <- chisq.test(dummySource$IRSEX, dummySource$gotFrRel)
chisq

chisq <- chisq.test(dummySource$IRSEX, dummySource$boughtFrRel)
chisq
title <- "Bought friend/rel"
xNames <- c("No", "Yes")
plotCrosstabs(title, xNames, dummySource$IRSEX, dummySource$boughtFrRel)

chisq <- chisq.test(dummySource$IRSEX, dummySource$tookFrRel)
chisq

chisq <- chisq.test(dummySource$IRSEX, dummySource$drugDeal)
chisq
title <- "Drug Deal"
xNames <- c("No", "Yes")
plotCrosstabs(title, xNames,dummySource$IRSEX, dummySource$drugDeal)

counts<- table(tempMisuse$anyPRMisuse, tempMisuse$AGE2)
title <- "Misuse by age group"
xNames <- c("12", "13", "14", "15", "16", "17", "18", "19", "20",
            "21", "22-23", "24-25", "26-29", "30-34", "35-49",
            "50-64", "65+")
barplot(counts, main = title, col = c("darkblue", "red"),
        names.arg = xNames, beside = TRUE, legend = c("Misuse", "No misuse"))
test <-svyglm(drugDeal~sexIsM, design = design1, family = quasibinomial())
summary(test)
test <-svyglm(drugDeal~sexIsM+employment+hasInsurance+marStat+age+ed, design = design1, family = quasibinomial())
summary(test)

oddsR <- exp(cbind(OR = coef(test), confint(test, level =0.95)))
oddsR



tempOnlyMisused <- filter(df1, anyPRMisuse == 1)
tempOnlyMisused <- filter(tempOnlyMisused, depressed > 0)
chisq <- chisq.test(tempOnlyMisused$IRSEX, tempOnlyMisused$depressed)
chisq
title <- "Past year major depressive episode among past year misusers"
xNames <- c("Major depressive episode", "No major depressive episode")
plotCrosstabs(title, xNames, tempOnlyMisused$IRSEX, tempOnlyMisused$depressed)


counts <- table(tempOnlyMisused$IRSEX, tempOnlyMisused$AGE2)
barplot(counts, main = "Age of Misusers by Gender", beside = TRUE,
        names.arg = xNames, legend = c("Men", "Women"))

counts <- table(tempMisuse$IRSEX, tempMisuse$anyPRMisuse)
barplot(counts, main = "Lifetime Pain Reliever Misuse by Gender",
        col = c("darkblue", "red"), names.arg = c("Yes", "No"), 
        legend = c("Men", "Women"),beside = TRUE, ylab = "Frequency")


chisq <- chisq.test(tempAny$IRSEX, tempAny$anyPRUse)
chisq
num <- nrow(tempAny)
counts<- table(tempAny$IRSEX, tempAny$anyPRUse)
counts
title <- "Lifetime Pain Reliever Use (Any) by Gender"
xNames <- c("Yes", "No")
plotCrosstabs(title, xNames, tempAny$IRSEX, tempAny$anyPRUse)

tempWhen <- filter(df1, mostRecentMisuse > 0)
chisq <- chisq.test(tempWhen$IRSEX, tempWhen$mostRecentMisuse)
chisq
title <- "Most recent pain reliever misuse by gender"
xNames <- c("Within last 12 mo.", "Before last 12 mo.")
plotCrosstabs(title, xNames, tempWhen$IRSEX, tempWhen$mostRecentMisuse)

tempPain <- filter(df1, pain >0)
chisq <- chisq.test(tempPain$IRSEX, tempPain$pain)
chisq
title <- "Misused to relieve physical pain"
xNames <- c("Yes", "No")
plotCrosstabs(title, xNames, tempPain$IRSEX, tempPain$pain)
tempPain$onlyPain <- NA
tempPain$onlyPain[tempPain$relax == 2 & tempPain$exp == 2 & tempPain$high == 2
                  & tempPain$sleep == 2 & tempPain$emot == 2 
                  & tempPain$hooked == 2 
                  & tempPain$otherDrug == 2 & tempPain$pain == 1] <- 1
tempPain$onlyPain[is.na(tempPain$onlyPain)] <- 0
chisq <- chisq.test(tempPain$IRSEX, tempPain$onlyPain)
chisq

tempRelax <- filter(df1, relax >0)
chisq <- chisq.test(tempRelax$IRSEX, tempRelax$relax)
chisq

tempExp <- filter(df1, exp >0)
chisq <- chisq.test(tempExp$IRSEX, tempExp$exp)
chisq

tempHigh <- filter(df1, high > 0)
chisq <- chisq.test(tempHigh$IRSEX, tempHigh$high)
chisq
title <- "Misused to get high"
xNames <- c("Yes", "No")
plotCrosstabs(title, xNames, tempHigh$IRSEX, tempHigh$high)
tempHigh$onlyHigh <- NA
tempHigh$onlyHigh[tempHigh$pain == 2 & tempHigh$relax == 2 & tempHigh$exp == 2
                  & tempHigh$emot == 2 & tempHigh$hooked == 2 & tempHigh$sleep == 2
                  & tempHigh$otherDrug == 2 & tempHigh$high == 1] <- 1
tempHigh$onlyHigh[is.na(tempHigh$onlyHigh)] <- 2
chisq <- chisq.test(tempHigh$IRSEX, tempHigh$onlyHigh)
chisq
title <- "Only misused for high"
xNames <- c("Misused for high only", "Misused for other reason")
plotCrosstabs(title, xNames, tempHigh$IRSEX, tempHigh$onlyHigh)

tempSleep <- filter(df1, sleep >0)
chisq<- chisq.test(tempSleep$IRSEX, tempSleep$sleep)
chisq
title <- "Misued to help with sleep"
xNames <- c("Yes", "No")
plotCrosstabs(title, xNames, tempSleep$IRSEX, tempSleep$sleep)



tempEmot <- filter(df1, emot > 0)
chisq <- chisq.test(tempEmot$IRSEX, tempEmot$emot)
chisq
title <- "Misused to help with emotions"
xNames <- c("Yes", "No")
plotCrosstabs(title, xNames, tempEmot$IRSEX, tempEmot$emot)
tempEmot$onlyEmot <- NA
tempEmot$onlyEmot[tempEmot$pain == 2 & tempEmot$relax == 2 & tempEmot$high == 2
                  & tempEmot$sleep == 2 & tempEmot$exp == 2 & tempEmot$hooked == 2
                  & tempEmot$otherDrug == 2 & tempEmot$emot == 1] <- 1
tempEmot$onlyEmot[is.na(tempEmot$onlyEmot)] <- 2
chisq <- chisq.test(tempEmot$IRSEX, tempEmot$onlyEmot)
chisq
title <- "only emot"
xNames <- c("yes", "no")
plotCrosstabs(title, xNames, tempEmot$IRSEX, tempEmot$onlyEmot)


tempOtherDrug <- filter(df1, otherDrug > 0)
chisq <- chisq.test(tempOtherDrug$IRSEX, tempOtherDrug$otherDrug)
chisq

tempHooked <- filter(df1, hooked > 0)
chisq <- chisq.test(tempHooked$IRSEX, tempHooked$hooked)
chisq


plotCrosstabs <- function(title, names, sexVar, var2){
  counts <- table(sexVar, var2)
  print(counts)
  if(names[1] == FALSE){
    widthVar <- rep(0.5, length(counts))
    barplot(counts, main = title,
            col = c("darkblue", "red"),
            legend = c("Men", "Women"),beside = TRUE, ylim = c(0, max(counts) + 100),
            ylab = "Frequency", axis.lty = 1)
    
  }else{
    par(pin = c(3, 4))
    par(mar = c(4,4,2,1)+.1)
    
    barplot(counts, main = title,
            col = c("cadetblue", "indianred1"), names.arg = names, 
            beside = TRUE, ylim = c(0, max(counts) + 100),
            ylab = "Frequency", axis.lty = 1, legend = c("Men", "Women"))
    
  }
}

#filter so that only those who would've answered misuse question are included
temp <- filter(df1, anyPRUse == 1)
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
  noOwnRx = noOwnRx,
  greaterAmnt = greaterAmnt,
  longer = longer,
  moreOften = moreOften
)
