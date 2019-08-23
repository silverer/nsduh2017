library(dplyr)

#NOTE--CHANGE PATH PER YOUR MACHINE'S FILES
pathToRawData <- "C:\\Users\\ers2244\\Documents\\opioids\\"
filename <- 'NSDUH_2017_Tab.tsv'
fileLoc <- paste(pathToRawData, filename, sep = '')

all_df <- read.delim(fileLoc, header = TRUE, sep = '\t')

df1 <- dplyr::select(all_df, IRSEX, AGE2, HEALTH2, ANALWT_C, VEREP, VESTR, EDUHIGHCAT,
                     IRINSUR4, PNRANYLIF, PNRANYREC, PNRNMLIF, PNRNMREC,DEPENDPNR,
                     DEPNDPYPNR, COUTYP4,HEALTH2,
                     PNRNMYR, PNRANYYR, NEWRACE2, INCOME, IRWRKSTAT18, IRMARIT,
                     PNRWYNORX, PNRWYGAMT, PNRWYOFTN, PNRWYLNGR, PNRWYOTWY,
                     PNRRSPAIN, PNRRSRELX, PNRRSEXPT, PNRRSHIGH, PNRRSSLEP,
                     PNRRSEMOT, PNRRSDGFX, PNRRSHOOK, PNRRSSOR, PNRRSOTRS2,
                     PNRRSMAIN, PNRNORXFG, SRCPNRNM2, IRMCDCHP, IRMEDICR, IRCHMPUS,
                     IRPRVHLT, IROTHHLT)

df1$misusePY <- df1$PNRNMYR
df1$usePY <- df1$PNRANYYR

df1$metro <- 0
df1$metro[df1$COUTYP4 == 1 | df1$COUTYP4 == 2] <- 1

df1$incomeF <- factor(df1$INCOME, levels = c(1, 2, 3, 4), labels=c('<20k', '20k-49k',
                                                                   '50k-74k', '75k+'))

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

df1$healthStatus <- factor(df1$HEALTH2, levels = c(1, 2, 3, 4),
                           labels = c('Excellent', 'Very Good', 'Good',
                                      'Fair/Poor'))

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

df1$newSex <- NA
df1$newSex[df1$IRSEX == 1] <- 2
df1$newSex[df1$IRSEX == 2] <- 1


df1$sexF <- factor(df1$newSex, levels = c(1, 2), labels=c("Women", "Men"))


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


write.csv(df1, paste(directory, '\\opioids_cleaned_data_v2.csv', sep=''))