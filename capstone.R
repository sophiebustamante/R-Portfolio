load("/Users/sophiabustamante/Downloads/temp_dataset.RData")

#overall RQ interaction by region: firth's
install.packages("logistf")
library(logistf)
#full model
firth_full_model <- logistf(contraceptive_ever ~ modern_contra_available + Education_Respondent 
                            + marital + health_care_decision + equality_HC_seeking 
                            + contraception_decision + ask_SRHS_avaialable + puberty_info + contra_info 
                            + modern_contra_available*Region + Education_Respondent*Region + marital*Region, 
                            data = temp)
summary(firth_full_model)
exp(cbind(OR = coef(firth_full_model), confint(firth_full_model)))

#reduced model
reduced_model <- logistf(contraceptive_ever ~ modern_contra_available + Education_Respondent 
                     + marital + health_care_decision + equality_HC_seeking 
                     + contraception_decision + ask_SRHS_avaialable + puberty_info 
                     + contra_info,
                     data = temp)
summary(reduced_model)
exp(cbind(OR = coef(reduced_model), confint(reduced_model)))

#compare models using LRT
anova(reduced_model, firth_full_model, test = "Chisq")
#logistftest(reduced_model, firth_full_model)

# Extract log-likelihood values
LL_reduced <- logLik(reduced_model)
LL_full <- logLik(firth_full_model)

# Perform likelihood ratio test
LR_stat <- -2 * (LL_reduced - LL_full)
p_value <- pchisq(LR_stat, df = df.residual(reduced_model) - df.residual(firth_full_model), lower.tail = FALSE)

# Print result
p_value






#full model: exposure + all potential confounders
full_model <- glm(contraceptive_ever ~ modern_contra_available + Education_Respondent 
                  + marital 
                  + health_care_decision + equality_HC_seeking + equality_HC_seeking
                  + contraception_decision + ask_SRHS_avaialable + puberty_info + contra_info
                  + modern_contra_available*Region + Education_Respondent*Region + marital*Region, 
                  data = temp, family = binomial(link = "logit"))
summary(full_model) 
exp(cbind(OR = coef(full_model), confint(full_model)))

#reduced model
reduced_model <- glm(contraceptive_ever ~ modern_contra_available + Education_Respondent 
                     + marital + health_care_decision + equality_HC_seeking 
                     + contraception_decision + ask_SRHS_avaialable + puberty_info 
                     + contra_info,
                     data = temp, family = binomial(link = "logit"))

#compare models using LRT
anova(reduced_model, full_model, test = "Chisq")

summary(full_model)






#Not educated-recoded as 1 #1 Not educated #Primary-recoded as 2 #3 Primary education #Secondary-recoded as 3 #4 Secondary education #Other-recoded as 4 #2 Informal education (e.g., adult literacy program, church, mosque) | Only 2 #5 Diploma/Technical/vocational | Only 3 #6 Degree and above | No one is in this cell table(temp$Education_Respondent, useNA = "always") temp$Education_Respondent <- ifelse(temp$Education_Respondent == 1, 1, ifelse(temp$Education_Respondent == 3, 2, ifelse(temp$Education_Respondent == 4, 3, 4))) table(Adolescent_Survey$Education_Respondent,temp$Education_Respondent, useNA = "always")#double checking to see if code worked, it did! table(temp$marital, useNA = "always") #Recode #Married-recoded as 1 #1 Married/living together 

#Single-recoded as 2 #2 Single #3 Widowed #4 Divorced #5 separated temp$marital <- ifelse(temp$marital == 1, 1, 2) table(temp$marital, useNA = "always")#double checking to see if code worked, it did! 

table(temp$modern_contra_available, useNA = "always") #6 = Not have explored contraception 
temp$modern_contra_available <- ifelse(temp$modern_contra_available == -98, 5, temp$modern_contra_available) #Limited-recoded as 1 
#1 Very limited #2 Somewhat limited 

#Adequate-recoded as 2 #3 Adequate 

#Abundant-recoded as 3 #4 Abundant 

#Haven't explored-recoded as 4 #5 Not applicable (have not explored contraception) 
temp$modern_contra_available <- ifelse(temp$modern_contra_available == 1 | temp$modern_contra_available == 2, 1, ifelse(temp$modern_contra_available == 3, 2, ifelse(temp$modern_contra_available == 4, 3, NA))) 

table(temp$modern_contra_available, useNA = "always") #double checking to see if code worked, it did! 

table(temp$health_care_decision, useNA = "always")#5 = Someone else (excliuding -> 4parents, 3respondent&partner/spouse together, 2partner/spouse, and 1respondent) 

table(temp$equality_HC_seeking, useNA = "always")#Can possibly combine to 0(1&2 agree), 1(3 neutral), 2(4&5 disagree), 5 is strongly disagree 
table(temp$equality_contra_seeking, useNA = "always")#Can possibly combine to 0(1&2 agree), 1(3 neutral), 2(4&5 disagree), 5 is strongly disagree 
temp$equality_HC_seeking <- ifelse(temp$equality_HC_seeking == 1 | temp$equality_HC_seeking == 2, 0, ifelse(temp$equality_HC_seeking == 4 | temp$equality_HC_seeking == 5, 2, 1)) 

temp$equality_contra_seeking <- ifelse(temp$equality_contra_seeking == 1 | temp$equality_contra_seeking == 2, 0, ifelse(temp$equality_contra_seeking == 4 | temp$equality_contra_seeking == 5, 2, 1)) 
#Combined to 0(1&2 agree), 1(3 neutral), 2(4&5 disagree), 3 is disagree 

#Who usually makes decisions on whether or not you should use contraception? 
table(temp$contraception_decision, useNA = "always") #can possibly combine values, 16 is wife and husband equally #Me-recoded as 1 #1 Myself  

#Partner-recoded as 2 #2 Partner/ husband #3 Boy friend 

#Family members and friends-recoded as 3 #4 Mother #5 Father #6 Siblings #7 Other family member #8 friends/neighbours 

#Health worker-recoded as 4 #9 community health worker/ vht #10 health care worker 

#Professionals at school-recoded as 5 #11 school counselor #12 someone at school 

#Religious leader-recoded as 6 #13 religious leader 

#Other-recoded as 8 #14 V/YSLA members #15 Other specify 

#Both me and my spouse-recoded as 9 #16 Wife and Husband equaly 

temp$contraception_decision <- ifelse(temp$contraception_decision == 1, 1, ifelse(temp$contraception_decision == 2 | temp$contraception_decision == 3, 2, 
                                 ifelse(temp$contraception_decision == 4 | temp$contraception_decision == 5 | temp$contraception_decision == 6 | temp$contraception_decision == 7 | temp$contraception_decision == 8, 3, 
                                        ifelse(temp$contraception_decision == 9 | temp$contraception_decision == 10, 4, ifelse(temp$contraception_decision == 11 | temp$contraception_decision == 12, 5, ifelse(temp$contraception_decision == 13, 6, 
                                                                                                                                                                                                              ifelse(temp$contraception_decision == 14 | temp$contraception_decision == 15, 8, 9))))))) 
table(temp$contraception_decision, useNA = "always") #Combined values, 9 is the reference 

table(temp$ask_SRHS_avaialable, useNA = "always") #Feeling comfortable asking sexual and repro questions, 2 = No 

table(temp$puberty_info, useNA = "always") #In the past 12 months, have you heard about/received information on Sexual health education/puberty? 2 = No 
table(temp$contraceptive_ever, useNA = "always") 
table(temp$contra_info, useNA = "always") #In the past 6 months, have you heard about/received information on contraceptives? 2= No 
library(dplyr)
temp <- temp %>% mutate(Education_Respondent = factor(Education_Respondent), modern_contra_available = factor(modern_contra_available), 
                        marital = factor(marital), health_care_decision = factor(health_care_decision), 
                        equality_HC_seeking = factor(as.character(equality_HC_seeking)), 
                         equality_contra_seeking = factor(equality_contra_seeking), contraception_decision = factor(contraception_decision), 
                        ask_SRHS_avaialable = factor(ask_SRHS_avaialable), puberty_info = factor(puberty_info), 
                        contraceptive_ever = factor(contraceptive_ever), contra_info = factor(contra_info), 
                        Region = factor(Region))
temp<-temp %>% mutate(
  Education_Respondent = relevel(Education_Respondent, ref = "2"),#Primary Education 
  modern_contra_available = relevel(modern_contra_available, ref = "3"),#Abundant 
  marital = relevel(marital, ref = "2"),#Single 
  health_care_decision = relevel(health_care_decision, ref = "5"), 
  equality_HC_seeking = relevel(equality_HC_seeking, ref = "2"), 
  equality_contra_seeking = relevel(equality_contra_seeking, ref = "2"), 
  contraception_decision = relevel(contraception_decision, ref = "9"), 
  ask_SRHS_avaialable = relevel(ask_SRHS_avaialable, ref = "0"), 
  puberty_info = relevel(puberty_info, ref = "2"), 
  contraceptive_ever = relevel(contraceptive_ever, ref = "1"), 
  contra_info = relevel(contra_info, ref = "2"), 
  Region = relevel(Region, ref = "Oromia") )

#remove modern_contra_available-98
temp$modern_contra_available[temp$modern_contra_available == -98] <- NA
table(temp$modern_contra_available)







# Chunk test for Region as potential EMM
firth_EMM_full <- logistf(contraceptive_ever ~ modern_contra_available + Education_Respondent 
                          + marital 
                          + health_care_decision + equality_HC_seeking + equality_HC_seeking
                          + contraception_decision + ask_SRHS_avaialable + puberty_info + contra_info
                          + modern_contra_available*Region + Education_Respondent*Region + marital*Region, 
                          data = temp)
summary(firth_EMM_full)

# Extracting OR and 95% CI
odds_ratios_emmf <- exp(coef(firth_EMM_full))
conf_int_emmf <- exp(confint.default(firth_EMM_full))
p_values_emmf <- summary(firth_EMM_full)$prob

# Combining results
results_emmf <- data.frame(
  Term = names(odds_ratios_emmf),
  OR = odds_ratios_emmf,
  Lower_95_CI = conf_int_emmf[, 1],
  Upper_95_CI = conf_int_emmf[, 2],
  p_value = p_values_emmf
)
print(results_emmf)

# Reduced Model
firth_model_merged <- logistf(contraceptive_ever ~ modern_contra_available + Education_Respondent 
                              + marital 
                              + health_care_decision + equality_HC_seeking + equality_HC_seeking
                              + contraception_decision + ask_SRHS_avaialable + puberty_info + contra_info, 
                              data = temp)
summary(firth_model_merged)

# Extracting OR and 95% CI
odds_ratios_merged <- exp(coef(firth_model_merged))
conf_int_merged <- exp(confint.default(firth_model_merged))
p_values_merged <- summary(firth_model_merged)$prob

# Combining results
results_merged <- data.frame(
  Term = names(odds_ratios_merged),
  OR = odds_ratios_merged,
  Lower_95_CI = conf_int_merged[, 1],
  Upper_95_CI = conf_int_merged[, 2],
  p_value = p_values_merged
)
print(results_merged)

# Moment of truth, is Region an EMM?
logLik_firth_model_merged <- firth_model_merged$loglik[2]  
logLik_firth_EMM_full <- firth_EMM_full$loglik[2]  

LRT_stat <- -2 * (logLik_firth_model_merged - logLik_firth_EMM_full)
p_value <- pchisq(LRT_stat, df = 3, lower.tail = FALSE)  # df = 3 since 3 interactions were tested

cat("LRT Statistic:", LRT_stat, "\nP-value:", p_value)













#INDIV RQ
#full model: exposure + all potential confounders
full <- glm(ask_SRHS_avaialable ~ marital + Region + religion + Education_Recode, 
            data = temp, family = binomial(link = "logit"))
summary(full) 
exp(cbind(OR = coef(full), confint(full)))

#confounding assessment marital: Region, religion, Education_Recode

#remove Region
no_Region <- glm(ask_SRHS_avaialable ~ marital + religion + Education_Recode,
                 data = temp, family = binomial(link="logit"))
summary(no_Region)
exp(cbind(OR = coef(no_Region), confint(no_Region)))

#remove religion
no_religion<-glm(ask_SRHS_avaialable~ marital + Region + Education_Recode,
                 data=temp, family=binomial(link="logit"))
summary(no_religion)
exp(cbind(OR=coef(no_religion), confint(no_religion)))

#remove Education_Recode
no_education<-glm(ask_SRHS_avaialable~ marital + Region + religion,
                  data=temp, family=binomial(link="logit"))
summary(no_education)
exp(cbind(OR=coef(no_education), confint(no_education)))


#final model:remove education, keep region and religion
final<-glm(ask_SRHS_avaialable ~ marital + Region + religion,
           data=temp, family=binomial(link="logit"))
summary(final)
exp(cbind(OR=coef(final), confint(final)))
pvals <- summary(final)$coefficients

#unadjusted
marital<-glm(ask_SRHS_avaialable ~ marital,
            data=temp, family = binomial(link="logit"))
summary(marital)
exp(cbind(OR=coef(marital), confint(marital)))








#confounding assessment Region: no confounders

#unadjusted
Region<-glm(ask_SRHS_avaialable ~ Region,
            data=temp, family = binomial(link="logit"))
summary(Region)
exp(cbind(OR=coef(Region), confint(Region)))
pvals <- summary(Region)$coefficients






#confounding assessment religion: Region

#unadjusted
religion<-glm(ask_SRHS_avaialable ~ religion,
             data=temp, family = binomial(link="logit"))
summary(religion)
exp(cbind(OR=coef(religion), confint(religion))) #final
pvals <- summary(religion)$coefficients

#full model: exposure + all potential confounders
full_religion <- glm(ask_SRHS_avaialable ~ religion + Region, 
            data = temp, family = binomial(link = "logit"))
summary(full_religion) 
exp(cbind(OR = coef(full_religion), confint(full_religion)))

#remove Region
no_Region <- glm(ask_SRHS_avaialable ~ religion,
                 data = temp, family = binomial(link="logit"))
summary(no_Region)
exp(cbind(OR = coef(no_Region), confint(no_Region)))









#Education_Recode as exposure: assess confounders for religion, Region, marital
#full model: exposure + all potential confounders
full_education <- glm(ask_SRHS_avaialable ~ Education_Recode + Region + religion + marital, 
            data = temp, family = binomial(link = "logit"))
summary(full_education) 
exp(cbind(OR = coef(full_education), confint(full_education)))
# Get p-values
pvals <- summary(full_education)$coefficients


#remove Region
no_Region <- glm(ask_SRHS_avaialable ~ Education_Recode + religion + marital,
                 data = temp, family = binomial(link="logit"))
summary(no_Region)
exp(cbind(OR = coef(no_Region), confint(no_Region)))

#remove religion
no_religion<-glm(ask_SRHS_avaialable~ Education_Recode + Region + marital,
                 data=temp, family=binomial(link="logit"))
summary(no_religion)
exp(cbind(OR=coef(no_religion), confint(no_religion)))

#remove marital
no_marital<-glm(ask_SRHS_avaialable~ Education_Recode + Region + religion,
                  data=temp, family=binomial(link="logit"))
summary(no_marital)
exp(cbind(OR=coef(no_marital), confint(no_marital)))


#final model=full model


