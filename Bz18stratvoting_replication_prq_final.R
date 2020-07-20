library(foreign)
library(survey)
library(ggplot2)
library(dplyr)
library(stargazer)
library(gridExtra)

## FORMATTING RAW DATA FOR ANALYSIS
bzsv1<-read.csv("Br18StrategicVoting.csv",stringsAsFactors = FALSE)

bzsv<-bzsv1 %>%
  select(id,weights,Region,State,Gender,SES,Age,Income,Expenses,Edu,Interest,KnowCat,
         PartyID,SupportPT,Ideology,VoteInt,PresPref,PresPred_1:PredConf,everything())

# reformat respondent characteristic variables
bzsv$X <- NULL
bzsv$PredConf<-factor(bzsv$PredConf,levels=c("Not at all","A little","Somewhat","Very"))
bzsv$Edu<-factor(bzsv$Edu,
                 levels = c("Analfabeto/ Nunca frequentou escola","Primário incompleto (até 3ª serie do ensino fundamental)",
                            "Primário completo (4ª serie do ensino fundamental)","Ginásio incompleto (até 7ª série do ensino fundamental)",
                            "Ginásio completo (8ª série do ensino fundamental)",
                            "Colegial incompleto (até 2ª série do ensino médio)","Colegial completo (3ª série do ensino médio)",
                            "Ensino universitário incompleto ou especialização (técnico após ensino médio)","Ensino universitário completo",
                            "Pós-graduação incompleto","Pós-graduação completo ou mais"))

#impute missing values for education
bzsv$Edu[is.na(bzsv$Edu) & (bzsv$SES == "A")] <- "Pós-graduação incompleto"
bzsv$Edu[is.na(bzsv$Edu) & (bzsv$SES == "B1")] <- "Ensino universitário completo"
bzsv$Edu[is.na(bzsv$Edu) & (bzsv$SES == "B2")] <- "Ensino universitário incompleto ou especialização (técnico após ensino médio)"
bzsv$Edu[is.na(bzsv$Edu) & (bzsv$SES == "C1")] <- "Colegial completo (3ª série do ensino médio)"
bzsv$Edu[is.na(bzsv$Edu) & (bzsv$SES == "C2")] <- "Colegial incompleto (até 2ª série do ensino médio)"
bzsv$Edu[is.na(bzsv$Edu) & (bzsv$SES == "D-E")] <- "Ginásio completo (8ª série do ensino fundamental)"


bzsv$EduCat<-ifelse(bzsv$Edu == "Analfabeto/ Nunca frequentou escola" | bzsv$Edu == "Primário incompleto (até 3ª serie do ensino fundamental)"| bzsv$Edu == "Primário completo (4ª serie do ensino fundamental)"|bzsv$Edu == "Ginásio incompleto (até 7ª série do ensino fundamental)" | bzsv$Edu == "Ginásio completo (8ª série do ensino fundamental)" | bzsv$Edu == "Colegial incompleto (até 2ª série do ensino médio)","Less than High School",
                    ifelse(bzsv$Edu == "Colegial completo (3ª série do ensino médio)","High School",
                           ifelse(bzsv$Edu == "Ensino universitário incompleto ou especialização (técnico após ensino médio)","Some College",
                                  ifelse(bzsv$Edu == "Ensino universitário completo" | bzsv$Edu == "Pós-graduação incompleto"|bzsv$Edu == "Pós-graduação completo ou mais","College Degree or More",NA
                                  ))))
bzsv$EduCat<-factor(bzsv$EduCat,
                    levels = c("Less than High School","High School","Some College","College Degree or More"))


bzsv$Edu_3cat <- as.character(bzsv$EduCat)
bzsv$Edu_3cat[bzsv$Edu_3cat == "College Degree or More"] <- "College or More"
bzsv$Edu_3cat[bzsv$Edu_3cat == "Some College"] <- "College or More"
bzsv$Edu_3cat <- factor(bzsv$Edu_3cat, levels = c("Less than High School","High School","College or More"))

bzsv$SupportPTf<-NA
bzsv$SupportPTf<-factor(bzsv$SupportPT,levels=c("Neutral","Don't Know","Oppose Totally",
                                                "Oppose Partially","Support Partially",
                                                "Support Totally"))
bzsv$SupportPTf[bzsv$SupportPTf=="Don't Know"]<- NA
bzsv$SupportPTf2<-as.character(bzsv$SupportPT)

bzsv$SupportPTf2[bzsv$SupportPTf == "Oppose Totally"|bzsv$SupportPTf == "Oppose Partially"] <- "Opponents"
bzsv$SupportPTf2[bzsv$SupportPTf == "Support Totally"|bzsv$SupportPTf == "Support Partially"] <- "Supporters"
bzsv$SupportPTf2[bzsv$SupportPT == "Don't Know"] <- "Neutral"
bzsv$SupportPTf2<-factor(bzsv$SupportPTf2)


#convert to numeric for analysis later
bzsv$SES_num <- as.numeric(factor(bzsv$SES, levels = rev(levels(factor(bzsv$SES)))))
bzsv$Edu_num <- as.numeric(bzsv$Edu)
bzsv$PredConf_num <- as.numeric(bzsv$PredConf)

# sv.pot - potential strategic voter = support four or below
bzsv$sv.pot<-ifelse(bzsv$PresPref == bzsv$PresPred_1,0,
                    ifelse(bzsv$PresPref == bzsv$PresPred_2,0,
                           ifelse(bzsv$PresPref == bzsv$PresPred_3,0,
                                  ifelse(is.na(bzsv$PresPref),NA,
                                         ifelse(bzsv$PresPref == "Don't Know",0,1)))))

#make variable "sv" to characterize each respondent's vote choice in terms of strategic behavior
bzsv$sv <- -99 
bzsv$sv[bzsv$PresPref == bzsv$VoteInt] <- "Sincere"
bzsv$sv[bzsv$sv.pot == 1 & (bzsv$VoteInt == bzsv$PresPred_1 | bzsv$VoteInt == bzsv$PresPred_2 | bzsv$VoteInt == bzsv$PresPred_3)] <- "Strategic" 
bzsv$sv[bzsv$VoteInt != bzsv$PresPref & bzsv$sv != "Strategic"] <- "Other Switcher"
bzsv$sv[bzsv$VoteInt == "Null/Blank"] <- "Null/Blank"
bzsv$sv[bzsv$VoteInt == "Abstain"] <- "Abstain"
bzsv$sv[bzsv$VoteInt == "Don't Know"] <- "Undecided"
bzsv$sv[is.na(bzsv$VoteInt)|is.na(bzsv$PresPref)] <- NA
bzsv$sv[bzsv$PresPref == "Don't Know" & bzsv$VoteInt == "Alvaro Dias (PODE)"] <- "No Pref Voters"
bzsv$sv[bzsv$PresPref == "Don't Know" & bzsv$VoteInt == "Cabo Daciolo (PATRI)"] <- "No Pref Voters"
bzsv$sv[bzsv$PresPref == "Don't Know" & bzsv$VoteInt == "Ciro Gomes (PDT)"] <- "No Pref Voters"
bzsv$sv[bzsv$PresPref == "Don't Know" & bzsv$VoteInt == "Fernando Haddad (PT)"] <- "No Pref Voters"
bzsv$sv[bzsv$PresPref == "Don't Know" & bzsv$VoteInt == "Geraldo Alckmin (PSDB)"] <- "No Pref Voters"
bzsv$sv[bzsv$PresPref == "Don't Know" & bzsv$VoteInt == "Guilherme Boulos (PSOL)"] <- "No Pref Voters"
bzsv$sv[bzsv$PresPref == "Don't Know" & bzsv$VoteInt == "Henrique Meirelles (MDB)"] <- "No Pref Voters"
bzsv$sv[bzsv$PresPref == "Don't Know" & bzsv$VoteInt == "Jair Bolsonaro (PSL)"] <- "No Pref Voters"
bzsv$sv[bzsv$PresPref == "Don't Know" & bzsv$VoteInt == "João Amoêdo (NOVO)"] <- "No Pref Voters"
bzsv$sv[bzsv$PresPref == "Don't Know" & bzsv$VoteInt == "Marina Silva (REDE)"] <- "No Pref Voters"

# Valid treats vote intention NA as 0.  Valid2 treats NA as NA
bzsv$Valid<-ifelse(bzsv$VoteInt == "Abstain"| bzsv$VoteInt == "Don't Know"| bzsv$VoteInt == "Null/Blank",0,
                   ifelse(is.na(bzsv$VoteInt),0,1))
bzsv$Valid2<-ifelse(bzsv$VoteInt == "Abstain"| bzsv$VoteInt == "Don't Know"| bzsv$VoteInt == "Null/Blank",0,
                    ifelse(is.na(bzsv$VoteInt),NA,1))

bzsv$SupTop3<-NA
bzsv$SupTop3<-ifelse(bzsv$PresPred_1 == bzsv$PresPref,1, 
                     ifelse(bzsv$PresPred_2 == bzsv$PresPref,1,
                            ifelse(bzsv$PresPred_3 == bzsv$PresPref,1,
                                   ifelse(is.na(bzsv$PresPref.rmdk),NA,0)))) 

bzsv$SupTop2<-NA
bzsv$SupTop2<-ifelse(bzsv$PresPred_1 == bzsv$PresPref,1, 
                     ifelse(bzsv$PresPred_2 == bzsv$PresPref,1,
                                   ifelse(is.na(bzsv$PresPref.rmdk),NA,0)))

bzsv$VotedPref<-NA
bzsv$VotedPref<-ifelse(bzsv$Valid2 == 1 & bzsv$PresPref == bzsv$VoteInt,"Sincere",
                       ifelse(bzsv$Valid2 == 1 & bzsv$PresPref.rmdk != bzsv$VoteInt,"Switcher",
                              ifelse(bzsv$Valid2 == 0, "Don'tKnow",NA)))
bzsv$VotedPref2<-ifelse(bzsv$VotedPref == "Switcher" & bzsv$sv2 == "Strategic", "Strategic",
                        ifelse(bzsv$VotedPref == "Switcher" & bzsv$sv2 == "Other Switcher", "Other Switcher",
                               bzsv$VotedPref))


bzsv$SupBHC<-NA
bzsv$SupBHC<-ifelse(bzsv$PresPref == "Jair Bolsonaro (PSL)", 1,
                    ifelse(bzsv$PresPref == "Fernando Haddad (PT)",1,
                           ifelse(bzsv$PresPref == "Ciro Gomes (PDT)",1,
                                  ifelse(bzsv$PresPref == "Don't Know",NA,
                                         ifelse(is.na(bzsv$PresPref),NA,0)))))


#sv.nsv = strategic voting vs. non-strategic-voting- indicator, 1 = strategic voting, 2 = all else
bzsv$sv.nsv <- ifelse(bzsv$sv == "Strategic",1,
                    ifelse(is.na(bzsv$sv),NA,0))
#sv.sinv = strategic voting vs. sincere voting.  Limit data to potential strategic voters.
# 1 = strategic, 0 = sincere
bzsv$sv.sinv<-ifelse(bzsv$sv == "Strategic",1,
                     ifelse(bzsv$sv.pot == 1 & bzsv$sv == "Sincere",0,NA))

#sv.pot2 - potential strategic voters those whose candidates won't finish in top 2 (not top 3)
bzsv$sv.pot2 <- ifelse(bzsv$PresPref == bzsv$PresPred_1,0,
                     ifelse(bzsv$PresPref == bzsv$PresPred_2,0,
                            ifelse(is.na(bzsv$PresPref),NA,
                                   ifelse(bzsv$PresPref == "Don't Know",0,1))))

# more permissive definition of strategic voting
bzsv$sv2 <- -99
bzsv$sv2[bzsv$PresPref == bzsv$VoteInt] <- "Sincere"
# if you support 4 or below and switch to top 3, you are strategic
bzsv$sv2[bzsv$sv.pot == 1 & (bzsv$VoteInt == bzsv$PresPred_1 | bzsv$VoteInt == bzsv$PresPred_2 | bzsv$VoteInt == bzsv$PresPred_3)] <- "Strategic"

# if you support 3 and vote for 1 or 2, you are strategic
bzsv$sv2[bzsv$PresPref == bzsv$PresPred_3 & (bzsv$VoteInt == bzsv$PresPred_1 | bzsv$VoteInt == bzsv$PresPred_2)] <- "Strategic" 

# other switchers = people who switch but don't follow strategic logic
bzsv$sv2[bzsv$PresPref != bzsv$VoteInt & bzsv$sv2 != "Strategic"] <- "Other Switcher"

bzsv$sv2[bzsv$VoteInt == "Null/Blank"] <- "Null/Blank"
bzsv$sv2[bzsv$VoteInt == "Abstain"] <- "Abstain"
bzsv$sv2[bzsv$VoteInt == "Don't Know"] <- "Undecided"
bzsv$sv2[bzsv$sv == "No Pref Voters"] <- "No Pref Voters"
bzsv$sv2[is.na(bzsv$VoteInt)|is.na(bzsv$PresPref)] <- NA


bzsv$sv.nsv2 <- ifelse(bzsv$sv2 == "Strategic",1,
                      ifelse(is.na(bzsv$sv2),NA,0))
bzsv$sv.sinv2<-ifelse(bzsv$sv2 == "Strategic",1,
                      ifelse(bzsv$sv.pot2 == 1 & bzsv$sv2 == "Sincere",0,NA))

#knowledge battery 
bzsv$KB1n <- ifelse(bzsv$KB1c == "Correct", 1, 0)
bzsv$KB2n <- ifelse(bzsv$KB2c == "Correct", 1, 0)
bzsv$KB3n <- ifelse(bzsv$KB3c == "Correct", 1, 0)
bzsv$KB4n <- ifelse(bzsv$KB4c == "Correct", 1, 0)
bzsv$KB5n <- ifelse(bzsv$KB5c == "Correct", 1, 0)
bzsv$KB6n <- ifelse(bzsv$KB6c == "Correct", 1, 0)
bzsv$KB7n <- ifelse(bzsv$KB7c == "Correct", 1, 0)
bzsv$KB8n <- ifelse(bzsv$KB8c == "Correct", 1, 0)
bzsv$KB9n <- ifelse(bzsv$KB9c == "Correct", 1, 0)
bzsv$KBsum <-
  bzsv$KB1n + bzsv$KB2n + bzsv$KB3n + bzsv$KB4n + bzsv$KB5n + bzsv$KB6n + bzsv$KB7n + bzsv$KB8n + bzsv$KB9n


### Making weights

#must put dataset into different format to account for survey weights
bzsvw <- svydesign(ids=bzsv$id, data = bzsv, weights = bzsv$weights)


#making new weights
data.svy.unweighted <- svydesign(ids=~id, data=bzsv)
pop.gen <- data.frame(Gender = c("Female", "Male"),
                      Freq = nrow(bzsv) * c(0.5103, 0.4897))
pop.reg <- data.frame(Region = c("Centro Oeste","Nordeste","Norte","Sudeste","Sul"),
                      Freq = nrow(bzsv)* c(0.0744,0.2779,0.0841,0.4206,0.1430))
pop.ses <- data.frame(SES = c("A","B1","B2","C1","C2","D-E"),
                      Freq = nrow(bzsv)* c(0.05,0.089,0.157,0.207,0.218,0.28))
pop.edu <- data.frame(Edu_3cat = c("Less than High School", "High School", "College or More"),
                      Freq = nrow(bzsv)* c(0.526,0.269,0.205))

bzsvw <- rake(
  design = data.svy.unweighted,
  sample.margins = list( ~ Gender,  ~ Region,  ~ SES, ~ Edu_3cat),
  population.margins = list(pop.gen, pop.reg, pop.ses, pop.edu)
)




############SUMMARY STATISTICS
##### TABLE -- Weighting Variables
prop.table(table(bzsv$Gender))
prop.table(table(bzsv$Region))
prop.table(table(bzsv$SES))
prop.table(table(bzsv$Edu_3cat))


#summary stats for knowledge battery
table(bzsv$KBsum)
svymean(~KBsum,design=bzsvw)
svyvar(~KBsum,design=bzsvw) 
sqrt(2.7946) #sd


## number of strategic voters in sample
#permissive
t.1a=ftable(svymean(~sv.pot,bzsvw,na.rm=T))
t.1b=ftable(svymean(~sv.nsv,bzsvw,na.rm=T))
t.1c=ftable(svymean(~sv.pot2,bzsvw,na.rm=T))
t.1d=ftable(svymean(~sv.nsv2,bzsvw,na.rm=T))
t.1<-as.data.frame(rbind(t.1a,t.1b, t.1c, t.1d))
t.1$name<-c("sv.pot","sv.sinv", "sv.pot2", "sv.sinv2")
t.1


##### TABLE [4] -- First preferences, vote intention
prop.table(table(bzsv$VoteInt[bzsv$Valid == 1]))*100
vi_weighted <- svytable(~VoteInt,design = bzsvw)
vi_weighted / sum(vi_weighted[-c(1, 5, 13, 14)]) * 100

prop.table(table(bzsv$PresPref[!bzsv$PresPref=="Don't Know"]))*100
fp_weighted <- svytable(~PresPref,design=bzsvw)
fp_weighted / sum(fp_weighted[-4]) * 100


##### TABLE [5] -- Vote switchers 
switchers_tab <- table(bzsv$PresPref[bzsv$Valid2 == 1], bzsv$VoteInt[bzsv$Valid2 == 1])[-4,]
# bottom row of table - proportions
tot_switchers <- sum(bzsv$sv2 == "Other Switcher" | bzsv$sv2 == "Strategic", na.rm = TRUE)
switchers_tab2 <- switchers_tab
diag(switchers_tab2) <- 0 
switch_col_sum <- colSums(switchers_tab2)
switch_col_sum
sort(switch_col_sum / tot_switchers, decreasing = TRUE)


#Vote switchers by switch from-switch to:
table(bzsv$PresPref[bzsv$VoteInt=="Jair Bolsonaro (PSL)"&bzsv$sv.nsv==1])
table(bzsv$PresPref[bzsv$VoteInt=="Fernando Haddad (PT)"&bzsv$sv.nsv==1])
table(bzsv$PresPref[bzsv$VoteInt=="Ciro Gomes (PDT)"&bzsv$sv.nsv==1])
table(bzsv$PresPref[bzsv$VoteInt=="Geraldo Alckmin (PSDB)"&bzsv$sv.nsv==1])



#### TABLE [6] -- Predictions shares
pred1<-svytable(~PresPred_1,design=bzsvw)
pred1<-as.data.frame(pred1)
pred1$prop<-pred1$Freq/sum(pred1$Freq)
pred2<-svytable(~PresPred_2,design=bzsvw)
pred2<-as.data.frame(pred2)
pred2$prop<-pred2$Freq/sum(pred2$Freq)
pred3<-svytable(~PresPred_3,design=bzsvw)
pred3<-as.data.frame(pred3)
pred3$prop<-pred3$Freq/sum(pred3$Freq)
pred.df<-merge(pred1,pred2,by.x="PresPred_1",by.y="PresPred_2")
pred.df<-merge(pred.df,pred3,by.x="PresPred_1",by.y="PresPred_3")
pred.df[order(-pred.df$prop.x),]



###### TABLE [7] -- Voting decisions by presidential preference
#voted for top preference, among those who support "winning" and "losing" candidates
svytable(~VotedPref+SupTop2,design=bzsvw)
svytable(~VotedPref2+SupTop2, design = bzsvw)
svytable(~VotedPref2+SupTop3, design = bzsvw)

sum(bzsv$VoteInt[bzsv$sv2 == "Strategic"] == bzsv$PresPred_1[bzsv$sv2 == "Strategic"], na.rm = TRUE)
sum(bzsv$VoteInt[bzsv$sv2 == "Strategic"] == bzsv$PresPred_2[bzsv$sv2 == "Strategic"], na.rm = TRUE)
sum(bzsv$VoteInt[bzsv$sv2 == "Strategic"] == bzsv$PresPred_3[bzsv$sv2 == "Strategic"], na.rm = TRUE)


# how many strategic voters?
svytable(~sv.sinv2, design = bzsvw)
table(bzsv$sv.sinv2)
# how many "irrational" switchers?
sum(ifelse(bzsv$sv2 == "Other Switcher" & bzsv$VoteInt != bzsv$PresPred_1 & bzsv$VoteInt != bzsv$PresPred_2 & bzsv$VoteInt != bzsv$PresPred_3, 
           1, NA), na.rm = TRUE)



###### REGRESSION ANALYSIS
# table(bzsv$sv.sinv2)
mod_logit <- svyglm(sv.sinv2 ~ SES_num + Edu_num + KBsum + PredConf_num + SupportPTf2, 
                    design = bzsvw,family=quasibinomial())
summary(mod_logit)


#how does moving from not at all to very confident increase predicted prob of s-voting?
svymean(~as.numeric(factor(SES,levels=rev(levels(factor(SES))))),design=bzsvw)
svymean(~as.numeric(Edu),design=bzsvw,na.rm=T)
svymean(~KBsum,design=bzsvw,na.rm=T)
svymean(~as.numeric(PredConf),design=bzsvw,na.rm=T)

svytable(~SupportPTf2,design=bzsvw)

prop.table(table(bzsv$sv.sinv2, bzsv$PredConf),2)
newdata.notatall = data.frame(SES_num = 2.296, Edu_num = 6.286, KBsum = 3.115, 
                              PredConf_num = 2, SupportPTf2 = "Neutral")
predict(mod_logit, newdata.notatall, type="response") # 0.036
newdata.very = data.frame(SES_num = 2.296, Edu_num = 6.286, KBsum = 3.115, 
                          PredConf_num = 4, SupportPTf2 = "Neutral")
predict(mod_logit, newdata.very, type="response")


###### FIGURE APPENDIX --- Demographic differences between potential and nonpotential strategic voters?  
svyby(~as.numeric(Edu),~I(sv.pot2==1),design=bzsvw, svymean,na.rm=T)

trw1<-svyby(~as.numeric(Edu),~I(sv.pot2==1),design=bzsvw, svymean,na.rm=T)
trw1$outcome<-trw1$`as.numeric(Edu)`
trw1$cat<-as.character(trw1$`I(sv.pot2 == 1)`)

svyttest(as.numeric(Edu) ~ sv.pot2, bzsvw)

dem.pot1<-ggplot(trw1, aes(x=cat,y=outcome)) +
  geom_point(stat='identity', size=3) +
  coord_flip() +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_errorbar(aes(ymax = outcome-se, 
                    ymin = outcome+se),width=0.2,alpha=0.8) +
  labs(x="",y="Education Level") + 
  scale_x_discrete(labels=c("Non-Potential", "Potential"))  +
  annotate("text",x=2.4,y=6.75,label="p < 0.01") +
  scale_y_continuous(limits=c(6.0,6.9))


trw2<-svyby(~as.numeric(factor(SES,levels=rev(levels(factor(SES))))),
            ~I(sv.pot2==1),design=bzsvw, svymean,na.rm=T)
trw2$outcome<-trw2$`as.numeric(factor(SES, levels = rev(levels(factor(SES)))))`
trw2$cat<-as.character(trw2$`I(sv.pot2 == 1)`)

svyttest(as.numeric(factor(SES,levels=rev(levels(factor(SES))))) ~ sv.pot2, bzsvw)

dem.pot2<-ggplot(trw2, aes(x=cat,y=outcome)) +
  geom_point(stat='identity', size=3) +
  coord_flip() +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_errorbar(aes(ymax = outcome-se, 
                    ymin = outcome+se),width=0.2,alpha=0.8) +
  labs(x="",y="SES") + 
  scale_x_discrete(labels=c("Non-Potential", "Potential"))  +
  annotate("text",x=2.4,y=2.5,label="p < 0.01") +
  scale_y_continuous(limits=c(2.1,2.6))


trw3<-svyby(~as.numeric(KBsum),~I(sv.pot2==1),design=bzsvw, svymean,na.rm=T)
trw3$outcome<-trw3$`as.numeric(KBsum)`
trw3$cat<-as.character(trw3$`I(sv.pot2 == 1)`)

svyttest(as.numeric(KBsum) ~ sv.pot2, bzsvw)

dem.pot3<-ggplot(trw3, aes(x=cat,y=outcome)) +
  geom_point(stat='identity', size=3) +
  coord_flip() +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_errorbar(aes(ymax = outcome-se, 
                    ymin = outcome+se),width=0.2,alpha=0.8) +
  labs(x="",y="Political Knowledge") + 
  scale_x_discrete(labels=c("Non-Potential", "Potential"))  +
  annotate("text",x=2.4,y=3.27,label="p = 0.24") +
  scale_y_continuous(limits=c(2.95,3.35))

trw4<-svyby(~as.numeric(PredConf),~I(sv.pot2==1),design=bzsvw, svymean,na.rm=T)
trw4$outcome<-trw4$`as.numeric(PredConf)`
trw4$cat<-as.character(trw4$`I(sv.pot2 == 1)`)

svyttest(as.numeric(PredConf) ~ sv.pot2, bzsvw)

dem.pot4<-ggplot(trw4, aes(x=cat,y=outcome)) +
  geom_point(stat='identity', size=3) +
  coord_flip() +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_errorbar(aes(ymax = outcome-se, 
                    ymin = outcome+se),width=0.2,alpha=0.8) +
  labs(x="",y="Confidence in Prediction") + 
  scale_x_discrete(labels=c("Non-Potential", "Potential"))  +
  annotate("text",x=2.4,y=3.32,label="p = 0.03") +
  scale_y_continuous(limits=c(3.0,3.4))

# n- potentials and non potentials
table(bzsv$sv.pot2)

dem_diffs_pot_nonpot <- grid.arrange(dem.pot1,dem.pot2,dem.pot3,dem.pot4, nrow = 2,
                                     bottom = "'Potentials' n = 627, 'Non-Potentials' n = 1563")


##### APPENDIX --- demographic differences between potential and actual strategic voters

svyby(~as.numeric(Edu),~I(sv.nsv2==1),design=subset(bzsvw,sv.pot2 == 1), svymean,na.rm=T)
svyttest(as.numeric(Edu) ~ sv.nsv2, subset(bzsvw,sv.pot2 == 1))
svyby(~as.numeric(factor(SES)),~I(sv.nsv2==1),design=subset(bzsvw,sv.pot2 == 1), svymean,na.rm=T)
svyttest(as.numeric(factor(SES)) ~ sv.nsv2, subset(bzsvw,sv.pot2 == 1))
svyby(~as.numeric(KnowCat),~I(sv.nsv2==1),design=subset(bzsvw,sv.pot2 == 1), svymean,na.rm=T)
svyttest(as.numeric(KnowCat) ~ sv.nsv2, subset(bzsvw,sv.pot2 == 1))
svyby(~as.numeric(PredConf),~I(sv.nsv2==1),design=subset(bzsvw,sv.pot2 == 1), svymean,na.rm=T)
svyttest(as.numeric(PredConf) ~ sv.nsv2, subset(bzsvw,sv.pot2 == 1))
svyby(~as.numeric(SupportPTf2),~I(sv.nsv2==1),design=subset(bzsvw,sv.pot2 == 1), svymean,na.rm=T)
svyttest(as.numeric(SupportPTf2) ~ sv.nsv2, subset(bzsvw,sv.pot2 == 1))


tmw1<-svyby(~as.numeric(Edu),~I(sv.nsv2==1),design=subset(bzsvw,sv.pot2 == 1), svymean,na.rm=T)
tmw1$outcome<-tmw1$`as.numeric(Edu)`
tmw1$cat<-as.character(tmw1$`I(sv.nsv2 == 1)`)
tmw1

svyttest(as.numeric(Edu) ~ sv.nsv2, subset(bzsvw,sv.pot2 == 1))


f1<-ggplot(tmw1, aes(x=cat,y=outcome))+
  geom_point(stat='identity', size=3) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_errorbar(aes(ymax = outcome-se,
                    ymin = outcome+se),width=0.2,alpha=0.8) +
  labs(x="",y="Education Level") +
  scale_x_discrete(labels=c("Potential", "Actual")) +
  annotate("text",x=2.4,y=7.4,label="p < 0.01") +
  scale_y_continuous(limits=c(6.0,7.75))


tmw2<-svyby(~as.numeric(factor(SES,levels=rev(levels(factor(SES))))),~I(sv.nsv2==1),
            design=subset(bzsvw,sv.pot2 == 1), svymean,na.rm=T)
tmw2$outcome<-tmw2$`as.numeric(factor(SES, levels = rev(levels(factor(SES)))))`
tmw2$cat<-as.character(tmw2$`I(sv.nsv2 == 1)`)


svyttest(as.numeric(factor(SES,levels=rev(levels(factor(SES))))) ~ sv.nsv2, subset(bzsvw,sv.pot2 == 1))


f2<-ggplot(tmw2, aes(x=cat,y=outcome))+
  geom_point(stat='identity', size=3) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_errorbar(aes(ymax = outcome-se,
                    ymin = outcome+se),width=0.2,alpha=0.8) +
  labs(x="",y="SES") +
  scale_x_discrete(labels=c("Potential", "Actual")) +
  scale_y_continuous(limits=c(2.2, 3.1)) +
  annotate("text",x=2.4,y=2.92,label="p = 0.03")


tmw3<-svyby(~as.numeric(KBsum),~I(sv.nsv2==1),design=subset(bzsvw,sv.pot2 == 1), svymean,na.rm=T)
tmw3$outcome<-tmw3$`as.numeric(KBsum)`
tmw3$cat<-as.character(tmw3$`I(sv.nsv2 == 1)`)

svyttest(as.numeric(KBsum) ~ sv.nsv2, subset(bzsvw,sv.pot2 == 1))

f3<-ggplot(tmw3, aes(x=cat,y=outcome))+
  geom_point(stat='identity', size=3) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_errorbar(aes(ymax = outcome-se,
                    ymin = outcome+se),width=0.2,alpha=0.8) +
  labs(x="",y="Political Knowledge") +
  scale_x_discrete(labels=c("Potential", "Actual")) +
  scale_y_continuous(limits=c(2.8,4.0)) +
  annotate("text",x=2.4,y=3.8,label="p < 0.01")



tmw4<-svyby(~as.numeric(PredConf),~I(sv.nsv2==1),design=subset(bzsvw,sv.pot2 == 1), svymean,na.rm=T)
tmw4$outcome<-tmw4$`as.numeric(PredConf)`
tmw4$cat<-as.character(tmw3$`I(sv.nsv2 == 1)`)

svyttest(as.numeric(PredConf) ~ sv.nsv2, subset(bzsvw,sv.pot2 == 1))


f4<-ggplot(tmw4, aes(x=cat,y=outcome))+
  geom_point(stat='identity', size=3) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_errorbar(aes(ymax = outcome-se,
                    ymin = outcome+se),width=0.2,alpha=0.8) +
  labs(x="",y="Confidence in Prediction") +
  scale_x_discrete(labels=c("Potential", "Actual")) +
  scale_y_continuous(limits=c(3.1,3.7)) +
  annotate("text",x=2.4,y=3.6,label="p < 0.01")

dem_diffs_pot_actual <- grid.arrange(f1, f2, f3, f4, nrow = 2,
                                     bottom = "'Actuals' n = 173, 'Potentials' n = 447")


# # n
# table(bzsv$sv.pot2, bzsv$sv.nsv2)




##### FIGURE [APPENDIX} --- strategic voting among potentials broken down by confidence level

confbypot<-as.data.frame(svytotal(~interaction(PredConf,sv.pot2),bzsvw,na.rm=T))
confbypot$sv.pot2<-as.character(c(rep(0,4),rep(1,4)))
confbypot$PredConf<-factor(rep(c("Not at all","A little","Somewhat","Very"),2),levels=c("Not at all","A little",
                                                                                        "Somewhat","Very"))
confbypot$prop<-ifelse(confbypot$sv.pot2==0,confbypot$total/(sum(confbypot$total[confbypot$sv.pot2==0])),
                       ifelse(confbypot$sv.pot2==1,confbypot$total/(sum(confbypot$total[confbypot$sv.pot2==1])),NA))
confbypot$se2<-ifelse(confbypot$sv.pot2==0,sqrt(confbypot$prop*(1-confbypot$prop))/(sqrt(sum(confbypot$total[confbypot$sv.pot2==0]))),
                      ifelse(confbypot$sv.pot2==1,sqrt(confbypot$prop*(1-confbypot$prop))/(sqrt(sum(confbypot$total[confbypot$sv.pot2==1]))),NA))


conf_by_pot <- ggplot(confbypot, aes(x=PredConf, y=prop,fill=sv.pot2)) +
  geom_bar(position="dodge",stat='identity') + 
  theme_minimal() + 
  ggtitle(" ") +
  labs(x="Confidence in Election Prediction",y="Proportion") +
  geom_errorbar(aes(ymax = prop-se2, 
                    ymin = prop+se2),position=position_dodge()) +
  scale_fill_manual(name="Proportion Among:",labels=c("Non-Potential\nSVers","Potential\nSVers"),
                    values=c("#696969", "#C0C0C0")) +
  theme(legend.position=c(0.19,0.79),legend.background = element_rect(fill="white")) 

ggsave("C:/Users/lukep/Box/School Files/Fall 2018/Brazil Field Work/Analysis/Analysis Files/Replication Files/conf_by_pot.png", plot = conf_by_pot)


##### FIGURE [APPENDIX} -- Potential and non-potential sver's by opinion of PT
ptbypot<-as.data.frame(svytotal(~interaction(SupportPTf2,sv.pot2),bzsvw,na.rm=T))
ptbypot$sv.pot2<-as.character(c(rep(0,3),rep(1,3)))
ptbypot$PT<-factor(rep(c("Neutral","Opponents","Supporters"),2),levels=c("Neutral","Opponents","Supporters"))
ptbypot$prop<-ifelse(ptbypot$sv.pot2==0,ptbypot$total/(sum(ptbypot$total[ptbypot$sv.pot2==0])),
                     ifelse(ptbypot$sv.pot2==1,ptbypot$total/(sum(ptbypot$total[ptbypot$sv.pot2==1])),NA))
ptbypot$se2<-ifelse(ptbypot$sv.pot2==0,sqrt(ptbypot$prop*(1-ptbypot$prop))/(sqrt(sum(ptbypot$total[ptbypot$sv.pot2==0]))),
                    ifelse(ptbypot$sv.pot2==1,sqrt(ptbypot$prop*(1-ptbypot$prop))/(sqrt(sum(ptbypot$total[ptbypot$sv.pot2==1]))),NA))


pt_by_pot <- ggplot(ptbypot, aes(x=PT, y=prop,fill=sv.pot2)) +
  geom_bar(position="dodge",stat='identity') + 
  theme_minimal() + 
  ggtitle(" ") +
  labs(x="PT Opinion",y="Proportion") +
  geom_errorbar(aes(ymax = prop-se2, 
                    ymin = prop+se2),position=position_dodge()) +
  scale_fill_manual(name="Proportion Among:",labels=c("Non-Potential\nSVers","Potential\nSVers"),
                    values=c("#696969", "#C0C0C0")) +
  theme(legend.position=c(0.5,0.85),legend.background = element_rect(fill="white"))
