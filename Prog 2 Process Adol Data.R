load(file = "C:\\Data\\RData\\sites.Rdata")
load(file = "C:\\Data\\RData\\AdolQ2.Rdata")


adol<-merge(adol, sites, by="SITE", all.x=T)       # Use to filter out non North American sites
adol<-subset(adol,SITE<2900) # temporarily commented out

adol$sex2<-adol$sex

adol$sex<- factor(adol$sex,
                  levels = c(2,1),
                  labels = c( "Female", "Male"))

adol$gender<-ifelse(adol$sex==1, "Male", "Female")

adol<-subset(adol, age>10 & age <18 & gender %in% c('Male','Female'))
adol$families[adol$families>7]<-7
adol$repgrade[adol$repgrade>4]<-4
adol$spcleduc[adol$spcleduc>4]<-4
adol$pregnant[adol$pregnant>4]<-4


adol$age <- ordered(adol$age,
                    levels = c(11:17),
                    labels = c("11 Yrs Old", "12 Yrs Old", "13 Yrs Old", "14 Yrs Old","15 Yrs Old","16 Yrs Old","17 Yrs Old" ))



adol$race <- factor(adol$race,
                    levels = c(1,2,3,4,5,6,7),
                    labels = c("Hispanic", "African -\nAmerican", "Caucasian", "Asian -\nAmerican", "Native -\nAmerican", "Other", "Biracial"))

adol$religion <- factor(adol$religion,
                        levels = c(1:14),
                        labels = c('Roman Catholic', 'Jewish', 'Muslim', 'Pentecostal', 'Baptist', 'Episcopal', 'Methodist', 'Lutheran', 'Mormon', 'Eastern \nOrthodox', 'Presbyterian',  'Other', 
                                   'None', 'Atheist')) 

adol$tmsraped <- ordered(adol$tmsraped,
                         levels = c(6,1,2,3,4,5),
                         labels = c('None', 'Once', '2 Times', '3-10 Times', '11-50 Times', '> 50 Times')) 

adol$educ <- ordered(adol$educ,
                     levels = c(1:5),
                     labels = c('High School\nor GED', 'Some High \nSchool', 'Junior High \nSchool', '7th or 8th \nGrade', 'Grade \nSchool'))  

adol$homohetr <- ordered(adol$homohetr,
                         levels = c(1:4),
                         labels = c('Always Straight', 'Mostly Straight', 'Mostly Gay', 'Always Gay')) 


adol$repgrade <- ordered(adol$repgrade,
                         levels = c(0:4),
                         labels = labs<-c('None', '1 Yr', '2 Yrs', '3 Yrs', '4 or more Yrs')) 

adol$spcleduc <- ordered(adol$spcleduc,
                         levels = c(0:4),
                         labels = labs<-c('None', '1 Yr', '2 Yrs', '3 Yrs', '4 or more Yrs'))  

adol$pregnant <- ordered(adol$pregnant,
                         levels = c(0:4),
                         labels = labs<-c('Never', 'Once', 'Twice', '3 times', '4 or more times')) 


adol$families <- ordered(adol$families,
                         levels = c(0:7),
                         labels = labs<-c('None', 'One', 'Two', 'Three', 'Four', 'Five', 'Six', 'Seven')) 


adol$naccused <- ordered(adol$naccused,
                         levels = c(1:2),
                         labels = labs<-c('Not Blamed, Have Touched', 'Not Blamed, Never Touched')) 


adolcsa<-subset(adol, bhv7==1)


save(adol, file = "C:\\Data\\RData\\adol2web.Rdata")
save(adolcsa, file = "C:\\Data\\RData\\adol2CSAweb.Rdata")