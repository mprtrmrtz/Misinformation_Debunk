
################################################################################ n
## Setting Up ==================================================================
################################################################################ n

rm(list = ls())
# Hypothesis 1

library(ggplot2)
library(MASS)
library(dplyr)
library(readr)

library(ggplot2)
library(gridExtra)
library(dplyr)


source("./analysis/india/debunk/clean_data.R")
source("./analysis/debunk_utils.R")

# indian_data <- read_excel("./data/india/debunk/Harvard India data file (August 15) - answers displayed as text.xlsx")

# View(indian_data)
data_master <- read_csv("data/Myanmar_56/Myanmar_56.csv")
India <- read_excel("data/india/debunk/Harvard India data file (August 15).xlsx")

data <- data_master

View(data)
View(India)

################################################################################ n
## Clean Data ==================================================================
################################################################################ n


convert_to_factor <- function(data) {
  # Function converts a few variables to factors
 # data$resp_gender <- as.factor(data$resp_gender)
 # data$QS15 <- as.factor(data$QS15)   # data$IN01INC <- as.factor(data$IN01INC)  
 # data$QSEARCH_SCREEN <- as.factor(data$QSEARCH_SCREEN)
 # data$QIND_SCREEN <- as.factor(data$QIND_SCREEN)
  
  # ordinal factors
  data$QCORR_MIMSHARE <- factor(data$QCORR_MIMSHARE,
                                levels=c("Not at all likely", "Slightly likely", "Moderately likely", "Likely", "Very likely"),
                                ordered=TRUE)
  data$QCORR_SNSSHARE <- factor(data$QCORR_SNSSHARE,
                                levels=c("Not at all likely", "Slightly likely", "Moderately likely", "Likely", "Very likely"),
                                ordered=TRUE)
  data$QMIM_USE02 <- factor(data$QMIM_USE02,
                            levels=c("Never", "Once", "A few times", "Once a day", "A couple of times a day",
                                     "Once every few hours", "Once an hour", "Multiple times an hour"),
                            ordered=TRUE)
  data$QMIM_DISCUSS_NEWS <- factor(data$QMIM_DISCUSS_NEWS,
                                   levels=c("Never", "Rarely", "Sometimes", "Frequently", "Very frequently"),
                                   ordered=TRUE)
  data$QMIM_SEE_NEWS <- factor(data$QMIM_SEE_NEWS,
                               levels=c("Never", "Rarely", "Sometimes", "Frequently", "Very frequently"),
                               ordered=TRUE)
  data$QSNS_USE02 <- factor(data$QSNS_USE02,
                            levels=c("Never", "Once", "A few times", "Once a day", "A couple of times a day",
                                     "Once every few hours", "Once an hour", "Multiple times an hour"),
                            ordered=TRUE)
  data$QSNS_DISCUSS_NEWS <- factor(data$QSNS_DISCUSS_NEWS,
                                   levels=c("Never", "Rarely", "Sometimes", "Frequently", "Very frequently"),
                                   ordered=TRUE)
  data$QSNS_SEE_NEWS <- factor(data$QSNS_SEE_NEWS,
                               levels=c("Never", "Rarely", "Sometimes", "Frequently", "Very frequently"),
                               ordered=TRUE)
  data$QPOLI_ATT <- factor(data$QPOLI_ATT,
                           levels=c("Never", "Rarely", "Sometimes", "Frequently", "Very frequently"),
                           ordered=TRUE)
  data$QPOLI_INTEREST <- factor(data$QPOLI_INTEREST,
                                levels=c("Not at all interested", "Slightly interested", "Moderately interested",
                                         "Interested", "Very interested"),
                                ordered=TRUE)
  
  data$QCORR_IN01 <- factor(data$QCORR_IN01,
                            levels=c("None at all", "A little", "A moderate amount", "A good amount", "A great deal"),
                            ordered=TRUE)
  data$QCORR_IN02 <- factor(data$QCORR_IN02,
                            levels=c("Not at all", "A little", "A moderate amount", "A good amount", "A great deal"),
                            ordered=TRUE)
  data$QCORR_IN03 <- factor(data$QCORR_IN03,
                            levels=c("Not at all interesting", "Slightly interesting", "Moderately interesting",
                                     "Interesting", "Very interesting"),
                            ordered=TRUE)
  
  data$QCORR_CRED01 <- factor(data$QCORR_CRED01,
                              levels=c("Not at all accurate", "Slightly accurate",
                                       "Moderately accurate", "Accurate", "Very accurate"),
                              ordered=TRUE)
  data$QCORR_CRED02 <- factor(data$QCORR_CRED02,
                              levels=c("Not at all authentic", "Slightly authentic",
                                       "Moderately authentic", "Authentic", "Very authentic"),
                              ordered=TRUE)
  data$QCORR_CRED03 <- factor(data$QCORR_CRED03,
                              levels=c("Not at all believable", "Slightly believable",
                                       "Moderately believable", "Believable", "Very believable"),
                              ordered=TRUE)
  data$QCORR_CRED04 <- factor(data$QCORR_CRED04,
                              levels=c("Not at all professional", "Slightly professional",
                                       "Moderately professional", "Professional", "Very professional"),
                              ordered=TRUE)
  
  # convert belief measures pre and post correction to factor
  data$QMISINFO_BELIEF01 <- factor(data$QMISINFO_BELIEF01,
                                 levels=c("Definitely accurate", "Probably accurate",
                                          "Not sure if accurate or inaccurate",
                                          "Probably inaccurate", "Definitely inaccurate"),
                                 ordered = TRUE)
  
  data$QMISINFO_BELIEF02 <- factor(data$QMISINFO_BELIEF02,
                                   levels=c("Definitely accurate", "Probably accurate",
                                            "Not sure if accurate or inaccurate",
                                            "Probably inaccurate", "Definitely inaccurate"),
                                   ordered = TRUE)
  
  # convert tie strength/agree vars to ordered factor
  for(i in c(paste0("0", 1:9), as.character(10:12))) {
    # convert to ordered factor
    data[,paste0("QTIE_STR", i)] <- factor(data[[paste0("QTIE_STR", i)]],
                                           c("Strongly disagree", "Somewhat disagree",
                                             "Neither agree nor disagree",
                                             "Somewhat agree", "Strongly agree"),
                                           ordered=TRUE)
  }
  
  data$QTIE_AGREE <- factor(data$QTIE_AGREE,
                            c("Strongly disagree", "Somewhat disagree",
                              "Neither agree nor disagree",
                              "Somewhat agree", "Strongly agree"),
                            ordered=TRUE)
  
  # convert tie description variables to ordered factors
  data$QTIE_CRED01 <- factor(data$QTIE_CRED01,
                             c("Strongly disagree", "Somewhat disagree",
                               "Neither agree nor disagree",
                               "Somewhat agree", "Strongly agree"),
                             ordered=TRUE)
  data$QTIE_CRED02 <- factor(data$QTIE_CRED02,
                             c("Strongly disagree", "Somewhat disagree",
                               "Neither agree nor disagree",
                               "Somewhat agree", "Strongly agree"),
                             ordered=TRUE)
  data$QTIE_CRED03 <- factor(data$QTIE_CRED03,
                             c("Strongly disagree", "Somewhat disagree",
                               "Neither agree nor disagree",
                               "Somewhat agree", "Strongly agree"),
                             ordered=TRUE)
  data$QTIE_POLI_FREQ <- factor(data$QTIE_POLI_FREQ,
                                c("Never", "Rarely", "Sometimes", "Frequently", "Very frequently"),
                                ordered=TRUE)
  data$QTIE_GEN <- as.factor(data$QTIE_GEN)
  
  # prevalence, belief vars
  data$QTIE_AGREE <- factor(data$QTIE_AGREE,
                            c("Strongly disagree", "Somewhat disagree",
                              "Neither agree nor disagree",
                              "Somewhat agree", "Strongly agree"),
                            ordered=TRUE)
  
  
  
  
  # used for prevelnce ----- comment out 
  for(i in c(7,8, 10, 26, 27, 32, 101:128, 201:205, 301:302, 401:402)) {   # values of the list was 1:116 before ???????????
    # convert to ordered factor
    
    
    # Prevalence experiment 
    
    belief_col <- paste0("LOOP_CLAIM_PREV_PLAT_", i, "_QCLAIM_BELIEF")
    data[,belief_col] <- factor(data[[belief_col]],
                                levels=c("Definitely accurate", "Probably accurate",
                                         "Not sure if accurate or inaccurate",
                                         "Probably inaccurate", "Definitely inaccurate"),
                                ordered=TRUE)

    data[,paste0("LOOP_CLAIM_PREV_PLAT_", i, "_QCLAIM_PREV")] <- factor(
      data[[paste0("LOOP_CLAIM_PREV_PLAT_", i, "_QCLAIM_PREV")]],
      levels=c("No", "Maybe", "Yes"), ordered=TRUE)
  }
  
  # used for prelavence ---- comment out
  
  return(data)
}











# data <- convert_to_factor(data)





add_aux_variables <- function(data) {

  
  
  data$QATT_SCREEN <- tolower(data$QATT_SCREEN)
  data$Claim <- ifelse(data$MISINFO_CLAIM_1 == 1, 1, 2)

  
  data$tie_treatment <- ifelse(data$CLOSE_QUAL_1 == 1, "Strong", "Weak")
  
  
  
  data$group_treatment <- ifelse(data$AGREE_QUAL_1 == 1, "Ingroup", "Outgroup")
 # data$group_treatment[data$AGREE_QUAL_1 == 1 ] <- "Ingroup"
  data$format_treatment <- "None"
  

  
  
  
  data$tie_group_treatment <- paste(data$tie_treatment, data$group_treatment, sep='-')
  data$tie_group_format_treatment <- paste(data$tie_treatment, data$group_treatment, data$format_treatment, sep='-')
  
  data$tie_treatment <- as.factor(data$tie_treatment)
  data$group_treatment <- as.factor(data$group_treatment)
  data$format_treatment <- as.factor(data$format_treatment)
  data$tie_group_treatment <- as.factor(data$tie_group_treatment)
  data$tie_group_format_treatment <- as.factor(data$tie_group_format_treatment)
  
  
  # Add collapsed version of intention to share  
  data$QCORR_MIMSHARE_BINARY <- (data$QCORR_MIMSHARE > 3)
  data$QCORR_SNSSHARE_BINARY <- (data$QCORR_SNSSHARE > 3)
  
  # binary correction interest variables
  data$QCORR_IN01_BINARY <- factor(ifelse(data$QCORR_IN01 >= 4, "High", "Low"),
                                   levels=c("Low", "High"), ordered=TRUE)
  data$QCORR_IN02_BINARY <- factor(ifelse(data$QCORR_IN02 >= 4, "High", "Low"),
                                   levels=c("Low", "High"), ordered=TRUE)
  data$QCORR_IN03_BINARY <- factor(ifelse(data$QCORR_IN03 >= 4, "High", "Low"),
                                   levels=c("Low", "High"), ordered=TRUE)
  # binary correction credibility variables
  data$QCORR_CRED01_BINARY <- factor(ifelse(data$QCORR_CRED01 >= 4, "High", "Low"),
                                     levels=c("Low", "High"), ordered=TRUE)
  data$QCORR_CRED02_BINARY <- factor(ifelse(data$QCORR_CRED02 >= 4, "High", "Low"),
                                     levels=c("Low", "High"), ordered=TRUE)
  data$QCORR_CRED03_BINARY <- factor(ifelse(data$QCORR_CRED03 >= 4, "High", "Low"),
                                     levels=c("Low", "High"), ordered=TRUE)
  data$QCORR_CRED04_BINARY <- factor(ifelse(data$QCORR_CRED04 >= 4, "High", "Low"),
                                     levels=c("Low", "High"), ordered=TRUE)
  
  # data$survey_duration_mins <- as.numeric(data$DataCollection_FinishTime - data$DataCollection_StartTime)
  
  # same gender tie
  data$tie_same_gender <- (data$QTIE_GEN == data$resp_gender)
  data$tie_diff_gender <- (data$QTIE_GEN != data$resp_gender)
  

  # add variables on level of attention
  data$real_tie_name <- (data$QIND_SCREEN == 1) 
  data$searched_internet <- (data$QSEARCH_SCREEN == 1) 
  
  # determine whether misinfo and correction manipulation checks were answered correctly 
  data$correct_misinfo_manip <- (data$QCORR_MANIP == 3)                      
  
  data$correct_corr_manip <- ((data$QMISINFO_MANIP == 1 & data$Claim == 1) | (data$QMISINFO_MANIP == 4 & data$Claim == 2))
  
  
  
  # determine whether color attention check was answered correctly
 
  orange_like_values <- c("ORANGE", "Orange", "??Orange", "????????")
  
  for (i in 1:nrow(data)){
    for (value in orange_like_values){
      if (data$QATT_SCREEN[i] == value){
        data$QATT_SCREEN[i] <- 'orange'
      }
    }
  }
  
  
  
  
  
  data$correct_attention_check <- (data$QATT_SCREEN == 'orange')

  
  
  # number of possible correct attention combinations
  
  data$correct_all_three_attention_checks <- (data$correct_attention_check & data$correct_misinfo_manip & data$correct_corr_manip)
  
  
  data$correct_atleast_one_attention_check <- (data$correct_attention_check | data$correct_misinfo_manip | data$correct_corr_manip)
  data$correct_atleast_two_attention_checks <- ((data$correct_attention_check & data$correct_misinfo_manip) |
     (data$correct_attention_check & data$correct_corr_manip) |
    (data$correct_misinfo_manip & data$correct_corr_manip))
  
  
  
  data$correct_only_one_attention_check <- (data$correct_atleast_one_attention_check & !data$correct_atleast_two_attention_checks)
  data$correct_color_misinfo_attention_checks <- (data$correct_attention_check & data$correct_misinfo_manip)
  data$correct_color_corr_attention_checks <- (data$correct_attention_check & data$correct_corr_manip)
  data$correct_misinfo_corr_attention_checks <- (data$correct_misinfo_manip & data$correct_corr_manip)
 #  survey took too long (99% quantile)
 # data$too_long <- (data$survey_duration_mins > quantile(data$survey_duration_mins, 0.99, na.rm=TRUE))
  
  return(data)
}




add_numeric_variables <- function(data) {
  # convert a few variable to their numeric versions 
  data$correction_mimshare_numeric <- data$QCORR_MIMSHARE
  
  data$correction_snsshare_numeric <- data$QCORR_SNSSHARE
  
  data$education_numeric <- data$QS13

  

  data$income_numeric <- sapply(as.character(data$QS15), switch,
                                "0"= 1,
                                "20000"=2,
                                "50000"=3,
                                "100000"=4,
                                "125000"=5,
                                "175000"=6,
                                "200000"=7,
                                "Prefer not to answer"=NA, NA)
  
  
  data$mim_discuss_news_numeric <- data$QMIM_DISCUSS_NEWS
  
  data$sns_discuss_news_numeric <- data$QSNS_DISCUSS_NEWS
  
  
  data$misinfo_belief_numeric <- data$QMISINFO_BELIEF02
  
  
  data$resp_age <- as.numeric(data$resp_age)
  
  
  data$politics_attention_numeric <- data$QPOLI_ATT
  
  
  data$politics_interest_numeric <- data$QPOLI_INTEREST
  
  
  
  # numeric representation of interest in message
  data$correction_interest1_numeric <- data$QCORR_IN01
  data$correction_interest2_numeric <- data$QCORR_IN02
  data$correction_interest3_numeric <- data$QCORR_IN03
  
  # numeric representation of message credibility
  data$correction_credibility1_numeric <- data$QCORR_CRED01
  data$correction_credibility2_numeric <- data$QCORR_CRED02
  data$correction_credibility3_numeric <- data$QCORR_CRED03
  data$correction_credibility4_numeric <- data$QCORR_CRED04
  
  data$misinfo_belief_numeric <- data$QMISINFO_BELIEF01
  data$misinfo_belief_1_numeric <- data$QMISINFO_BELIEF02
  
  # numeric representation of tie strength/agree measures
  for(i in c(paste0("0", 1:9), as.character(10:12))) {
    data[,paste0("tie_strength", i, "_numeric")] <- data[[paste0("QTIE_STR", i)]]
  }
  
  data$average_tie_strength <- rowMeans(data[,c(paste0("tie_strength", c(paste0("0", 1:9), as.character(10:12)), "_numeric"))])
  data$tie_agree_numeric <- data$QTIE_AGREE
  
  # Numeric version of treatment and correction variables 
  data$tie_treatment_numeric <- ifelse(data$tie_treatment == "Strong", 0, 1)
  data$group_treatment_numeric <- ifelse(data$group_treatment == "Ingroup", 0, 1)
  data$correction_interest1_binary_numeric <- ifelse(data$QCORR_IN01_BINARY == "High", 1, 0)
  data$correction_interest2_binary_numeric <- ifelse(data$QCORR_IN02_BINARY == "High", 1, 0)
  data$correction_interest3_binary_numeric <- ifelse(data$QCORR_IN03_BINARY == "High", 1, 0)
  data$correction_credibility1_binary_numeric <- ifelse(data$QCORR_CRED01_BINARY == "High", 1, 0)
  data$correction_credibility2_binary_numeric <- ifelse(data$QCORR_CRED02_BINARY == "High", 1, 0)
  data$correction_credibility3_binary_numeric <- ifelse(data$QCORR_CRED03_BINARY == "High", 1, 0)
  data$correction_credibility4_binary_numeric <- ifelse(data$QCORR_CRED04_BINARY == "High", 1, 0)
  
  # numeric variables on tie description
  data$tie_credibility1_numeric <- data$QTIE_CRED01
  data$tie_credibility2_numeric <- data$QTIE_CRED02
  data$tie_credibility3_numeric <- data$QTIE_CRED03
  data$tie_political_freq_numeric <- data$QTIE_POLI_FREQ
  data$tie_same_gender_numeric <- ifelse(data$tie_same_gender == TRUE, 1, 0)
  data$tie_diff_gender_numeric <- ifelse(data$tie_diff_gender == TRUE, 1, 0)
  
  return(data)
}



process_data <- function(data_file) {
  data <- read_csv(data_file)

  
  # get tie role
  data$tie_role <- "Friend"
  data$tie_role[data$QTIE_ROLE_2 == 1] <- "Family"
  data$tie_role[data$QTIE_ROLE_3 == 1] <- "Romantic Partner"
  data$tie_role[data$QTIE_ROLE_4 == 1] <- "Work Colleague" 
  data$tie_role[data$QTIE_ROLE_5 == 1] <- "Schoolmate"
  data$tie_role[data$QTIE_ROLE_6 == 1] <- "Neighbor" 
  data$tie_role[data$QTIE_ROLE_7 == 1] <- "Member of peer group"
  data$tie_role[data$QTIE_ROLE_8 == 1] <- "Acquaintance"
  data$tie_role[data$QTIE_ROLE_9 == 1] <- "Stranger"
  data$tie_role[data$QTIE_ROLE_10 == 1] <- "Other"
  data$tie_role[data$QTIE_ROLE_101 == 1] <- NA
  
  # data <- convert_to_factor(data)
  data <- add_aux_variables(data)
  data <- add_numeric_variables(data)
  
  return(data)
}




print_attentive_stats <- function(data) {
  percent_no_treament <- round(sum(data$format_treatment == "None") / nrow(data), 4) * 100
  percent_unreal_ind <- round(1 - (sum(data$real_tie_name) / nrow(data)), 4) * 100
  percent_searched_internet <- round(sum(data$searched_internet) / nrow(data), 4) * 100
  percent_failed_color_attention <- round(1 - sum(data$correct_attention_check) / nrow(data), 4) * 100
  percent_failed_misinfo_attention <- round(1 - sum(data$correct_misinfo_manip) / nrow(data), 4) * 100
  percent_failed_color_misinfo_attentions <- round(1 - sum(data$correct_attention_check & data$correct_misinfo_manip) / nrow(data), 4) * 100
  
 tmp_data <- data[!is.na(data$correct_corr_manip),]
 percent_failed_correction_attention <- round(1 - sum(tmp_data$correct_corr_manip) / nrow(tmp_data), 4) * 100
 percent_failed_color_correction_attentions <- round(1 - sum(tmp_data$correct_attention_check & tmp_data$correct_corr_manip) / nrow(tmp_data), 4) * 100
 percent_failed_misinfo_correction_attentions <- round(1 - sum(tmp_data$correct_misinfo_manip & tmp_data$correct_corr_manip) / nrow(tmp_data), 4) * 100
  
  percent_failed_all_three_attentions <- round(1 - sum(tmp_data$correct_atleast_one_attention_check) / nrow(tmp_data), 4) * 100
  percent_failed_atleast_two_attentions <- round(sum(!tmp_data$correct_atleast_one_attention_check | tmp_data$correct_only_one_attention_check) / nrow(tmp_data), 4) * 100
  percent_failed_atleast_one_attention <- round(1 - sum(tmp_data$correct_all_three_attention_checks) / nrow(tmp_data), 4) * 100
  # 
 # cat(paste0(percent_no_treament, '% of subjects did not receive the correction treatment (i.e. Image vs Audio vs Text).\n'))
  cat(paste0(percent_unreal_ind, '% of subjects entered an unreal name for individual contact.\n'))
  cat(paste0(percent_searched_internet, '% of subjects searched the internet during survey.\n'))
  cat(paste0(percent_failed_color_attention, '% of subjects failed the color attention check.\n'))
  cat(paste0(percent_failed_misinfo_attention, '% of subjects failed the misinfo manipulation check.\n'))
  cat(paste0(percent_failed_correction_attention, '% of subjects failed the correction manipulation check.\n'))
  cat(paste0(percent_failed_color_misinfo_attentions, '% of subjects failed either the color attention or misinfo manipulation checks.\n'))
cat(paste0(percent_failed_color_correction_attentions, '% of subjects failed either the color attention or correction manipulation checks.\n'))
cat(paste0(percent_failed_misinfo_correction_attentions, '% of subjects failed either the misinfo manipulation or correction manipulation checks.\n'))
cat(paste0(percent_failed_all_three_attentions, '% of subjects failed all three attention checks.\n'))
cat(paste0(percent_failed_atleast_two_attentions, '% of subjects failed at least two attention checks.\n'))
 cat(paste0(percent_failed_atleast_one_attention, '% of subjects failed at least one attention checks.\n'))
}




exclude_nonattentive <- function(data, no_treatment, real_tie, search_internet, attention_check, correct_misinfo, correct_correction) {
  df <- data
  if (no_treatment) {
    df <- df[df$format_treatment != "None",]
  }
  if (real_tie) {
    df <- df[df$real_tie_name,]
  }
  if (search_internet) {
    df <- df[!df$searched_internet,]
  }
  if (attention_check) {
    df <- df[df$correct_attention_check,]
  }
  if (correct_misinfo) {
    df <- df[df$correct_misinfo_manip & !is.na(df$correct_misinfo_manip),]
  }
  if (correct_correction) {
    df <- df[df$correct_corr_manip & !is.na(df$correct_corr_manip),]
  }
  # if (long_duration) {
  #   df <- df[!df$too_long,]
  # }
  return(df)
}




################################################################################ n
## Descriptive Analysis ========================================================
################################################################################ n

data <- process_data("data/Myanmar_56/Myanmar_56.csv")

print_attentive_stats(data) # Take a closer look at the output of this 





df <- exclude_nonattentive(data,
                                                no_treatment=FALSE,
                                                real_tie=FALSE,
                                                search_internet=FALSE,
                                                attention_check=TRUE,
                                                correct_misinfo=FALSE,
                                                correct_correction=FALSE)




table(df$QMISINFO_MANIP)
table(df$QCORR_MANIP)

table(df$QMISINFO_BELIEF01, useNA="ifany")
table(df$QMISINFO_BELIEF02, useNA="ifany")

prop.table(table(df$QMISINFO_BELIEF01, useNA="ifany"))
prop.table(table(df$QMISINFO_BELIEF02, useNA="ifany"))

table(df$correct_misinfo_manip, useNA="ifany")                          
table(df$correct_corr_manip, useNA="ifany")
table(df$correct_corr_manip, df$correct_misinfo_manip)

prop.table(table(df$correct_misinfo_manip, useNA="ifany"))
prop.table(table(df$correct_corr_manip, useNA="ifany"))
prop.table(table(df$correct_corr_manip, df$correct_misinfo_manip, useNA="ifany"))

# fraction of those who got the attention check correct
table(df$QATT_SCREEN == 'orange')
prop.table(table(df$QATT_SCREEN == 'orange'))

# interest in politics
table(df$QPOLI_INTEREST)
table(df$QPOLI_ATT)
table(df$QACC_NEWS)
table(df$QMIM_SEE_NEWS)
table(df$QMIM_DISCUSS_NEWS)
table(df$QSNS_SEE_NEWS)
table(df$QSNS_DISCUSS_NEWS)

prop.table(table(df$QPOLI_INTEREST))
prop.table(table(df$QPOLI_ATT))
prop.table(table(df$QACC_NEWS))
prop.table(table(df$QMIM_SEE_NEWS))
prop.table(table(df$QMIM_DISCUSS_NEWS))
prop.table(table(df$QSNS_SEE_NEWS))
prop.table(table(df$QSNS_DISCUSS_NEWS))

# attention to correction
table(df$QCORR_IN01, useNA="ifany")
table(df$QCORR_IN02, useNA="ifany")
table(df$QCORR_IN03, useNA="ifany")
table(df$QCORR_IN01, df$QCORR_IN02, useNA="ifany")

################################################################################################
# How well self-reported tie strength match against tie strength treatment? fraction of compliers
df <- add_inferred_treatment_variables(df, "median")

df$strength_complier <- (df$inferred_tie_treatment == df$tie_treatment)
round(prop.table(table(df$tie_treatment, df$inferred_tie_treatment, dnn=c("Treatment", "Self_reported")), margin=1), 3)
round(prop.table(table(df$strength_complier)), 3)

# How well self-reported tie agreement match against tie agreement treatment? fraction of compliers
df$agree_complier <- (df$inferred_group_treatment == df$group_treatment)
round(prop.table(table(df$group_treatment, df$inferred_group_treatment, dnn=c("Treatment", "Self_reported")), margin=1), 3)
round(prop.table(table(df$agree_complier)), 3)

table(df$tie_treatment, useNA="ifany")
prop.table(table(df$tie_treatment, useNA="ifany"))
table(df$tie_group_treatment, useNA="ifany")
prop.table(table(df$tie_group_treatment, useNA="ifany"))

strong_tie_df <- df[df$tie_treatment == "Strong",]
weak_tie_df <- df[df$tie_treatment == "Weak",]
strong_tie_ingroup_df <- df[df$tie_group_treatment == "Strong-Ingroup",]
strong_tie_outgroup_df <- df[df$tie_group_treatment == "Strong-Outgroup",]
weak_tie_ingroup_df <- df[df$tie_group_treatment == "Weak-Ingroup",]
weak_tie_outgroup_df <- df[df$tie_group_treatment == "Weak-Outgroup",]

# plot the Correction sharing among all ties/groups
ggplot(df, aes(x=QCORR_MIMSHARE)) + geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Correction MIM Share") + ylab("Fraction") +
  ylim(0, .5) +
  scale_x_discrete(limits=c("Not at all likely", "Slightly likely", "Moderately likely", "Likely", "Very likely"))

# plot sharing correction on MIM by the strength of the tie
p1 <- ggplot(strong_tie_df, aes(x=QCORR_MIMSHARE)) + geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Correction MIM Share") + ylab("Fraction") + ggtitle("MIMShare Strong Ties") +
  ylim(0, 0.52) +
  scale_x_discrete(limits=c("Not at all likely", "Slightly likely", "Moderately likely", "Likely", "Very likely"))
p2 <- ggplot(weak_tie_df, aes(x=QCORR_MIMSHARE)) + geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Correction MIM Share") + ylab("Fraction") + ggtitle("MIMShare Weak Ties") +
  ylim(0, 0.5) +
  scale_x_discrete(limits=c("Not at all likely", "Slightly likely", "Moderately likely", "Likely", "Very likely"))
p <- grid.arrange(p1, p2, ncol=2)
ggsave("./results/tie_strength_mimshare.png", plot <- p, device = "png",
       width = 12, height = 6, units = "in")


# plot sharing correction on MIM by the strength and group of the tie
p1 <- ggplot(strong_tie_ingroup_df, aes(x=QCORR_MIMSHARE)) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=12)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Correction MIM Share") + ylab("Fraction") + ggtitle("MIMShare Strong Ingroup Ties") +
  ylim(0, 0.6) +
  scale_x_discrete(limits=c("Not at all likely", "Slightly likely", "Moderately likely", "Likely", "Very likely"))
p2 <- ggplot(strong_tie_outgroup_df, aes(x=QCORR_MIMSHARE)) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=12)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Correction MIM Share") + ylab("Fraction") + ggtitle("MIMShare Strong Outgroup Ties") +
  ylim(0, 0.6) +
  scale_x_discrete(limits=c("Not at all likely", "Slightly likely", "Moderately likely", "Likely", "Very likely"))
p3 <- ggplot(weak_tie_ingroup_df, aes(x=QCORR_MIMSHARE)) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=12)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Correction MIM Share") + ylab("Fraction") + ggtitle("MIMShare Weak Ingroup Ties") +
  ylim(0, 0.6) +
  scale_x_discrete(limits=c("Not at all likely", "Slightly likely", "Moderately likely", "Likely", "Very likely"))
p4 <- ggplot(weak_tie_outgroup_df, aes(x=QCORR_MIMSHARE)) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=12)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Correction MIM Share") + ylab("Fraction") + ggtitle("MIMShare Weak Outgroup Ties") +
  ylim(0, 0.6) +
  scale_x_discrete(limits=c("Not at all likely", "Slightly likely", "Moderately likely", "Likely", "Very likely"))
p <- grid.arrange(p1, p2, p3, p4, nrow=2, ncol=2)
ggsave("./results/tie_strength_group_mimshare.png", plot <- p, device = "png",
       width = 12, height = 12, units = "in")


# chi-squared test of strong vs weak
table(df$QCORR_MIMSHARE, df$tie_treatment)
chisq.test(df$tie_treatment, df$QCORR_MIMSHARE)
table(df$QCORR_SNSSHARE, df$tie_treatment)
chisq.test(df$tie_treatment, df$QCORR_SNSSHARE)

# MM 

# chi-squared test of InGroup vs Outgroup
table(df$QCORR_MIMSHARE, df$group_treatment)
chisq.test(df$group_treatment, df$QCORR_MIMSHARE)
table(df$QCORR_SNSSHARE, df$group_treatment)
chisq.test(df$group_treatment, df$QCORR_SNSSHARE)





# chi-squared test of strong-ingroup vs weak-outgroup
df_test <- df[df$tie_group_treatment %in% c("Strong-Ingroup", "Weak-Outgroup"),]
#df_test <- df[df$tie_group_treatment %in% c("Strong-Ingroup", "Strong-Outgroup"),]
#df_test <- df[df$tie_group_treatment %in% c("Strong-Ingroup", "Weak-Ingroup"),]
table(df_test$QCORR_MIMSHARE, df_test$tie_group_treatment)
chisq.test(df_test$tie_group_treatment, df_test$QCORR_MIMSHARE)
table(df_test$QCORR_SNSSHARE, df_test$tie_group_treatment)
chisq.test(df_test$tie_group_treatment, df_test$QCORR_SNSSHARE)

# plot duration to finish survey between those who get attention check right or wrong
max_duration <- 120
correct_attention_check_df <- df[df$correct_attention_check==TRUE,]
wrong_attention_check_df <- df[df$correct_attention_check==FALSE,]
# correct_attention_check_df <- correct_attention_check_df[correct_attention_check_df$survey_duration_mins < max_duration,]
# wrong_attention_check_df <- wrong_attention_check_df[wrong_attention_check_df$survey_duration_mins < max_duration,]
# p1 <- ggplot(correct_attention_check_df, aes(x=survey_duration_mins)) +
#   theme(axis.text=element_text(size=10), axis.title=element_text(size=12)) + 
#   ylab("Fraction") + ggtitle("Correct Attention Check") +
#   xlim(0,max_duration) +
#   geom_histogram(aes(y = (..count..)/sum(..count..)), fill="red") +
#   geom_vline(aes(xintercept=median(survey_duration_mins)), linetype="dashed", color = "black") +
#   geom_vline(aes(xintercept=mean(survey_duration_mins)), linetype="solid", color = "black")
# 
# p2 <- ggplot(wrong_attention_check_df, aes(x=survey_duration_mins)) +
#   theme(axis.text=element_text(size=10), axis.title=element_text(size=12)) + 
#   geom_bar(aes(y = (..count..)/sum(..count..))) +
#   ylab("Fraction") + ggtitle("Wrong Attention Check") +
#   xlim(0,max_duration) +
#   geom_histogram(aes(y = (..count..)/sum(..count..)), fill="blue") +
#   geom_vline(aes(xintercept=median(survey_duration_mins)), linetype="dashed", color = "black") +
#   geom_vline(aes(xintercept=mean(survey_duration_mins)), linetype="solid", color = "black")
# p <- grid.arrange(p1, p2, nrow=1, ncol=2)
# 
# t.test(correct_attention_check_df$survey_duration_mins,
#        wrong_attention_check_df$survey_duration_mins)

# print list of numerical columns that are highly correlated with self-reported tie strength or agreement
cat(paste('correlation between assigned treatment and actual inferred treatment:',
          round(cor(df$inferred_tie_treatment_numeric, df$tie_treatment_numeric), 3)))
numerical_cols <- which(sapply(df, is.numeric))
threshold <- 0.2
for(col_id in numerical_cols) {
  col_name <- colnames(df)[col_id]
  #res <- cor(df$inferred_tie_treatment_numeric, df[,col_id])
  res <- cor(df$inferred_tie_treatment_numeric, df$tie_treatment_numeric*df[,col_id])
  if(!grepl("tie_strength", col_name) &
     !grepl("tie_agree", col_name) &
     !grepl("inferred", col_name) &
     !is.na(res) & abs(res) > threshold) {
    print(paste0(col_name, ": ", round(res[[1]], 3)))
  }
}




################################################################################

# Hypothesis 1 =================================================================

################################################################################


# which filtering to use?
df <- exclude_nonattentive(data,
                           no_treatment=FALSE,
                           real_tie=TRUE,
                           search_internet=FALSE,
                           attention_check=TRUE,
                           correct_misinfo=FALSE,
                           correct_correction=FALSE)

print_summary_table <- function(polr_fit) {
  ctable <- coef(summary(polr_fit))
  p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  ctable <- cbind(ctable, "p value" = p)
  return(round(ctable, 4))
}



############################################
# Hypothesis 1: message interest vs. format
# Using interest measure QCORR_IN01

df1 <- df[!is.na(df$QCORR_IN01),]
test_res <- chisq.test(df1$format_treatment, df1$QCORR_IN01)

summary_stats <- df1[,c("format_treatment", "QCORR_IN01")] %>% group_by(format_treatment) %>%
  mutate(treatment_count=n()) %>%
  group_by(format_treatment, QCORR_IN01) %>% 
  summarise(freq=n()/first(treatment_count))
ggplot(summary_stats, aes(x=QCORR_IN01, y=freq, fill=format_treatment)) +
  geom_bar(stat='identity', position='dodge') +
  ylab("Within treatment fraction") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(paste('Pearson Chi-squared p-val:', round(test_res$p.value, 4)))


df_test <- df1
df_test <- df1[df1$format_treatment %in% c("Text", "Image"),]
df_test <- df1[df1$format_treatment %in% c("Text", "Audio"),]

df_test <- droplevels(df_test)
df_test$is_audio <- (df_test$format_treatment == "Audio")
print_summary_table(polr(QCORR_IN01 ~ format_treatment, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_IN01 ~ is_audio, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_IN01 ~ format_treatment + QPOLI_INTEREST, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_IN01 ~ format_treatment + QPOLI_ATT, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_IN01 ~ format_treatment + QACC_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_IN01 ~ format_treatment + QMIM_SEE_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_IN01 ~ format_treatment + QMIM_DISCUSS_NEWS, df_test, Hess=TRUE))

# Binary measure
test_res <- chisq.test(df1$format_treatment, df1$QCORR_IN01_BINARY)
summary_stats <- df1[,c("format_treatment", "QCORR_IN01_BINARY")] %>% group_by(format_treatment) %>%
  mutate(treatment_count=n()) %>%
  group_by(format_treatment, QCORR_IN01_BINARY) %>% 
  summarise(freq=n()/first(treatment_count))
ggplot(summary_stats, aes(x=QCORR_IN01_BINARY, y=freq, fill=format_treatment)) +
  geom_bar(stat='identity', position='dodge') +
  ylab("Within treatment fraction") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(paste('Pearson Chi-squared p-val:', round(test_res$p.value, 4)))

df_test <- df1
df_test <- df1[df1$format_treatment %in% c("Text", "Image"),]
df_test <- df1[df1$format_treatment %in% c("Text", "Audio"),]

df_test <- droplevels(df_test)
df_test$is_audio <- (df_test$format_treatment == "Audio")
summary(glm(QCORR_IN01_BINARY ~ format_treatment, df_test, family='binomial'))
summary(glm(QCORR_IN01_BINARY ~ is_audio, df_test, family='binomial'))
summary(glm(QCORR_IN01_BINARY ~ format_treatment + QPOLI_INTEREST, df_test, family='binomial'))
summary(glm(QCORR_IN01_BINARY ~ format_treatment + QPOLI_ATT, df_test, family='binomial'))
summary(glm(QCORR_IN01_BINARY ~ format_treatment + QACC_NEWS, df_test, family='binomial'))
summary(glm(QCORR_IN01_BINARY ~ format_treatment + QMIM_SEE_NEWS, df_test, family='binomial'))
summary(glm(QCORR_IN01_BINARY ~ format_treatment + QMIM_DISCUSS_NEWS, df_test, family='binomial'))

#####
# Using interest measure QCORR_IN02
df1 <- df[!is.na(df$QCORR_IN02),]
test_res <- chisq.test(df1$format_treatment, df1$QCORR_IN02)
summary_stats <- df1[,c("format_treatment", "QCORR_IN02")] %>% group_by(format_treatment) %>%
  mutate(treatment_count=n()) %>%
  group_by(format_treatment, QCORR_IN02) %>% 
  summarise(freq=n()/first(treatment_count))
ggplot(summary_stats, aes(x=QCORR_IN02, y=freq, fill=format_treatment)) +
  geom_bar(stat='identity', position='dodge') +
  ylab("Within treatment fraction") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(paste('Pearson Chi-squared p-val:', round(test_res$p.value, 4)))

df_test <- df1
df_test <- df1[df1$format_treatment %in% c("Text", "Image"),]
df_test <- df1[df1$format_treatment %in% c("Text", "Audio"),]

df_test <- droplevels(df_test)
df_test$is_audio <- (df_test$format_treatment == "Audio")
print_summary_table(polr(QCORR_IN02 ~ format_treatment, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_IN02 ~ is_audio, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_IN02 ~ format_treatment + QPOLI_INTEREST, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_IN02 ~ format_treatment + QPOLI_ATT, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_IN02 ~ format_treatment + QACC_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_IN02 ~ format_treatment + QMIM_SEE_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_IN02 ~ format_treatment + QMIM_DISCUSS_NEWS, df_test, Hess=TRUE))

# Binary measure
test_res <- chisq.test(df1$format_treatment, df1$QCORR_IN02_BINARY)
summary_stats <- df1[,c("format_treatment", "QCORR_IN02_BINARY")] %>% group_by(format_treatment) %>%
  mutate(treatment_count=n()) %>%
  group_by(format_treatment, QCORR_IN02_BINARY) %>% 
  summarise(freq=n()/first(treatment_count))
ggplot(summary_stats, aes(x=QCORR_IN02_BINARY, y=freq, fill=format_treatment)) +
  geom_bar(stat='identity', position='dodge') +
  ylab("Within treatment fraction") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(paste('Pearson Chi-squared p-val:', round(test_res$p.value, 4)))

df_test <- df1
df_test <- df1[df1$format_treatment %in% c("Text", "Image"),]
df_test <- df1[df1$format_treatment %in% c("Text", "Audio"),]

df_test <- droplevels(df_test)
df_test$is_audio <- (df_test$format_treatment == "Audio")
summary(glm(QCORR_IN02_BINARY ~ format_treatment, df_test, family='binomial'))
summary(glm(QCORR_IN02_BINARY ~ is_audio, df_test, family='binomial'))
summary(glm(QCORR_IN02_BINARY ~ format_treatment + QPOLI_INTEREST, df_test, family='binomial'))
summary(glm(QCORR_IN02_BINARY ~ format_treatment + QPOLI_ATT, df_test, family='binomial'))
summary(glm(QCORR_IN02_BINARY ~ format_treatment + QACC_NEWS, df_test, family='binomial'))
summary(glm(QCORR_IN02_BINARY ~ format_treatment + QMIM_SEE_NEWS, df_test, family='binomial'))
summary(glm(QCORR_IN02_BINARY ~ format_treatment + QMIM_DISCUSS_NEWS, df_test, family='binomial'))


#######
# Using interest measure QCORR_IN03
df1 <- df[!is.na(df$QCORR_IN03),]
test_res <- chisq.test(df1$format_treatment, df1$QCORR_IN03)
summary_stats <- df1[,c("format_treatment", "QCORR_IN03")] %>% group_by(format_treatment) %>%
  mutate(treatment_count=n()) %>%
  group_by(format_treatment, QCORR_IN03) %>% 
  summarise(freq=n()/first(treatment_count))
ggplot(summary_stats, aes(x=QCORR_IN03, y=freq, fill=format_treatment)) +
  geom_bar(stat='identity', position='dodge') +
  ylab("Within treatment fraction") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(paste('Pearson Chi-squared p-val:', round(test_res$p.value, 4)))

df_test <- df1
df_test <- df1[df1$format_treatment %in% c("Text", "Image"),]
df_test <- df1[df1$format_treatment %in% c("Text", "Audio"),]

df_test <- droplevels(df_test)
df_test$is_audio <- (df_test$format_treatment == "Audio")
print_summary_table(polr(QCORR_IN03 ~ format_treatment, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_IN03 ~ is_audio, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_IN03 ~ format_treatment + QPOLI_INTEREST, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_IN03 ~ format_treatment + QPOLI_ATT, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_IN03 ~ format_treatment + QACC_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_IN03 ~ format_treatment + QMIM_SEE_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_IN03 ~ format_treatment + QMIM_DISCUSS_NEWS, df_test, Hess=TRUE))

# Binary measure
test_res <- chisq.test(df1$format_treatment, df1$QCORR_IN03_BINARY)
summary_stats <- df1[,c("format_treatment", "QCORR_IN03_BINARY")] %>% group_by(format_treatment) %>%
  mutate(treatment_count=n()) %>%
  group_by(format_treatment, QCORR_IN03_BINARY) %>% 
  summarise(freq=n()/first(treatment_count))
ggplot(summary_stats, aes(x=QCORR_IN03_BINARY, y=freq, fill=format_treatment)) +
  geom_bar(stat='identity', position='dodge') +
  ylab("Within treatment fraction") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(paste('Pearson Chi-squared p-val:', round(test_res$p.value, 4)))

df_test <- df1
df_test <- df1[df1$format_treatment %in% c("Text", "Image"),]
df_test <- df1[df1$format_treatment %in% c("Text", "Audio"),]

df_test <- droplevels(df_test)
df_test$is_audio <- (df_test$format_treatment == "Audio")
summary(glm(QCORR_IN03_BINARY ~ format_treatment, df_test, family='binomial'))
summary(glm(QCORR_IN03_BINARY ~ is_audio, df_test, family='binomial'))
summary(glm(QCORR_IN03_BINARY ~ format_treatment + QPOLI_INTEREST, df_test, family='binomial'))
summary(glm(QCORR_IN03_BINARY ~ format_treatment + QPOLI_ATT, df_test, family='binomial'))
summary(glm(QCORR_IN03_BINARY ~ format_treatment + QACC_NEWS, df_test, family='binomial'))
summary(glm(QCORR_IN03_BINARY ~ format_treatment + QMIM_SEE_NEWS, df_test, family='binomial'))
summary(glm(QCORR_IN03_BINARY ~ format_treatment + QMIM_DISCUSS_NEWS, df_test, family='binomial'))




################################################################################

# Logistic Regression =========================================================

################################################################################


df <- data

# those who were actually shown a correction from a tie
# df <- data[data$format_treatment != "None",]

df <- data[data$QATT_SCREEN == 'orange',]
# df <- data[data$QATT_SCREEN == 'puce' & data$format_treatment != "None",]
df <- data[data$QATT_SCREEN == 'orange' & data$QIND_SCREEN=="Yes",]
df <- data[data$QATT_SCREEN == 'orange' & data$QIND_SCREEN=="Yes" & data$QSEARCH_SCREEN=="No",]
df <- data[data$QATT_SCREEN == 'orange' & data$correct_corr_manip & !is.na(data$correct_corr_manip),]
df <- data[data$QATT_SCREEN == 'orange' & data$correct_misinfo_manip & !is.na(data$correct_misinfo_manip),]
df <- data[data$QATT_SCREEN == 'orange' & data$correct_corr_manip & !is.na(data$correct_corr_manip) &
             data$correct_misinfo_manip & !is.na(data$correct_misinfo_manip),]

df <- data[data$correct_misinfo_manip & !is.na(data$correct_misinfo_manip),]
df <- data[data$correct_misinfo_manip & !is.na(data$correct_misinfo_manip) & data$QIND_SCREEN=="Yes",]
df <- data[data$correct_misinfo_manip & !is.na(data$correct_misinfo_manip) & data$QIND_SCREEN=="Yes" & data$QSEARCH_SCREEN=="No",]

df <- data[data$correct_corr_manip & !is.na(data$correct_corr_manip),] # BEST DATA
df <- data[data$correct_corr_manip & !is.na(data$correct_corr_manip) & data$QIND_SCREEN=="Yes",] # BEST DATA
df <- data[data$correct_corr_manip & !is.na(data$correct_corr_manip) & data$QIND_SCREEN=="Yes" & data$QSEARCH_SCREEN=="No",]

df <- data[data$correct_misinfo_manip & data$correct_corr_manip &
             !is.na(data$correct_misinfo_manip) & !is.na(data$correct_corr_manip),]
df <- data[data$correct_misinfo_manip & data$correct_corr_manip & data$QIND_SCREEN=="Yes" &
             !is.na(data$correct_misinfo_manip) & !is.na(data$correct_corr_manip),]
df <- data[data$correct_misinfo_manip & data$correct_corr_manip & data$QIND_SCREEN=="Yes" & data$QSEARCH_SCREEN=="No" &
             !is.na(data$correct_misinfo_manip) & !is.na(data$correct_corr_manip),]

print_summary_table <- function(polr_fit) {
  ctable <- coef(summary(polr_fit))
  p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  ctable <- cbind(ctable, "p value" = p)
  return(round(ctable, 4))
}


df$QCORR_MIMSHARE <- as.factor(df$QCORR_MIMSHARE)



print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QPOLI_INTEREST, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QPOLI_ATT, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QACC_NEWS, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QMIM_SEE_NEWS, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QMISINFO_BELIEF02, df, Hess=TRUE))
# print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + misinfo_belief_numeric, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QMIM_DISCUSS_NEWS, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + mim_discuss_news_numeric, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QMIM_DISCUSS_NEWS + QMISINFO_BELIEF02, df, Hess=TRUE))
# print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QMIM_DISCUSS_NEWS + income_numeric, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QMIM_DISCUSS_NEWS + resp_gender, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QMIM_DISCUSS_NEWS + resp_age, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QMIM_DISCUSS_NEWS + resp_age + education_numeric, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QMIM_DISCUSS_NEWS + resp_age + QS13, df, Hess=TRUE))

print_summary_table(polr(QCORR_MIMSHARE ~ tie_group_treatment, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_group_treatment + QPOLI_INTEREST, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_group_treatment + QPOLI_ATT, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_group_treatment + QACC_NEWS, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_group_treatment + QMIM_SEE_NEWS, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_group_treatment + QMISINFO_BELIEF02, df, Hess=TRUE))
# print_summary_table(polr(QCORR_MIMSHARE ~ tie_group_treatment + misinfo_belief_numeric, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_group_treatment + QMIM_DISCUSS_NEWS, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_group_treatment + mim_discuss_news_numeric, df, Hess=TRUE))
# print_summary_table(polr(QCORR_MIMSHARE ~ tie_group_treatment + QMIM_DISCUSS_NEWS + income_numeric, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_group_treatment + QMIM_DISCUSS_NEWS + resp_gender, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_group_treatment + QMIM_DISCUSS_NEWS + resp_age, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_group_treatment + QMIM_DISCUSS_NEWS + resp_age + education_numeric, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_group_treatment + QMIM_DISCUSS_NEWS + resp_age + QS13, df, Hess=TRUE))


#################################
# Same analysis but with binary DV
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QPOLI_INTEREST, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QPOLI_ATT, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QACC_NEWS, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMIM_SEE_NEWS, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMISINFO_BELIEF02, df, family="binomial"))
# summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment + misinfo_belief_numeric, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMIM_DISCUSS_NEWS, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment + mim_discuss_news_numeric, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMIM_DISCUSS_NEWS + QMISINFO_BELIEF02, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMIM_DISCUSS_NEWS + income_numeric, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMIM_DISCUSS_NEWS + resp_gender, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMIM_DISCUSS_NEWS + resp_age, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMIM_DISCUSS_NEWS + resp_age + education_numeric, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMIM_DISCUSS_NEWS + resp_age + QS13, df, family="binomial"))

summary(glm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QPOLI_INTEREST, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QPOLI_ATT, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QACC_NEWS, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QMIM_SEE_NEWS, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QMISINFO_BELIEF02, df, family="binomial"))
# summary(glm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + misinfo_belief_numeric, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QMIM_DISCUSS_NEWS, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + mim_discuss_news_numeric, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QMIM_DISCUSS_NEWS + QMISINFO_BELIEF02, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QMIM_DISCUSS_NEWS + income_numeric, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QMIM_DISCUSS_NEWS + resp_gender, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QMIM_DISCUSS_NEWS + resp_age, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QMIM_DISCUSS_NEWS + resp_age + education_numeric, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QMIM_DISCUSS_NEWS + resp_age + QS13, df, family="binomial"))

#################################

# analysis with inferred treatment variables
df <- add_inferred_treatment_variables(df, "median")
df <- add_inferred_treatment_variables(df, "divisive")
df <- add_inferred_treatment_variables(df, "agglomorative")

# Use inferred tie strength for treatment effects
round(prop.table(table(df$inferred_tie_treatment)), 3)
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_tie_treatment, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_tie_treatment + QPOLI_INTEREST, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_tie_treatment + QPOLI_ATT, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_tie_treatment + QMIM_SEE_NEWS, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_tie_treatment + QMIM_DISCUSS_NEWS, df, Hess=TRUE))
ggplot(df, aes(inferred_tie_treatment, correction_mimshare_numeric, fill = inferred_tie_treatment)) +
  theme_bw() + theme(legend.position = "none") +
  stat_summary(geom = "bar", fun.y = mean) +
  stat_summary(geom = "errorbar", fun.data = mean_cl_boot, fun.args=list(conf.int=0.95)) +
  scale_fill_manual(values = c("red", "blue"))


# Use inferred agreement for treatment effects
round(prop.table(table(df$inferred_group_treatment)), 3)
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_group_treatment, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_group_treatment + QPOLI_INTEREST, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_group_treatment + QPOLI_ATT, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_group_treatment + QMIM_SEE_NEWS, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_group_treatment + QMIM_DISCUSS_NEWS, df, Hess=TRUE))
ggplot(df, aes(inferred_group_treatment, correction_mimshare_numeric, fill = inferred_group_treatment)) +
  theme_bw() + theme(legend.position = "none") +
  stat_summary(geom = "bar", fun.y = mean) +
  stat_summary(geom = "errorbar", fun.data = mean_cl_boot, fun.args=list(conf.int=0.95)) +
  scale_fill_manual(values = c("red", "blue"))

# Use correction Format
round(prop.table(table(df$format_treatment)), 3)
# print_summary_table(polr(QCORR_MIMSHARE ~ format_treatment, df, Hess=TRUE))
# print_summary_table(polr(QCORR_MIMSHARE ~ format_treatment + QPOLI_INTEREST, df, Hess=TRUE))
# print_summary_table(polr(QCORR_MIMSHARE ~ format_treatment + QPOLI_ATT, df, Hess=TRUE))
# print_summary_table(polr(QCORR_MIMSHARE ~ format_treatment + QMIM_SEE_NEWS, df, Hess=TRUE))
# print_summary_table(polr(QCORR_MIMSHARE ~ format_treatment + QMIM_DISCUSS_NEWS, df, Hess=TRUE))
ggplot(df, aes(format_treatment, correction_mimshare_numeric, fill = format_treatment)) +
  theme_bw() + theme(legend.position = "none") +
  stat_summary(geom = "bar", fun.y = mean) +
  stat_summary(geom = "errorbar", fun.data = mean_cl_boot, fun.args=list(conf.int=0.95)) +
  scale_fill_manual(values = c("red", "green", "blue"))


# Use inferred strength and agreement
round(prop.table(table(df$inferred_tie_group_treatment)), 3)
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_tie_group_treatment, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_tie_group_treatment + QPOLI_INTEREST, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_tie_group_treatment + QPOLI_ATT, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_tie_group_treatment + QMIM_SEE_NEWS, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_tie_group_treatment + QMIM_DISCUSS_NEWS, df, Hess=TRUE))
ggplot(df, aes(inferred_tie_group_treatment, correction_mimshare_numeric, fill = inferred_tie_group_treatment)) +
  theme_bw() + theme(legend.position = "none") +
  stat_summary(geom = "bar", fun.y = mean) +
  stat_summary(geom = "errorbar", fun.data = mean_cl_boot, fun.args=list(conf.int=0.95)) +
  scale_fill_manual(values = c("red", "orange", "green", "blue"))


# Use inferred strength and agreement and format
round(prop.table(table(df$inferred_tie_group_format_treatment)), 3)
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_tie_group_format_treatment, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_tie_group_format_treatment + QPOLI_INTEREST, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_tie_group_format_treatment + QPOLI_ATT, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_tie_group_format_treatment + QMIM_SEE_NEWS, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_tie_group_format_treatment + QMIM_DISCUSS_NEWS, df, Hess=TRUE))
ggplot(df, aes(inferred_tie_group_format_treatment, correction_mimshare_numeric, fill = inferred_tie_group_format_treatment)) +
  theme_bw() + theme(legend.position = "none", axis.text=element_text(size=6)) +
  stat_summary(geom = "bar", fun.y = mean) +
  stat_summary(geom = "errorbar", fun.data = mean_cl_boot, fun.args=list(conf.int=0.95))




################################################################################


# OLS Regression ===============================================================


################################################################################

library(ivpack)

df <- data

summary(lm(correction_mimshare_numeric ~ tie_treatment, df))
summary(lm(correction_mimshare_numeric ~ tie_treatment + QPOLI_INTEREST, df))
summary(lm(correction_mimshare_numeric ~ tie_treatment + QPOLI_ATT, df))
summary(lm(correction_mimshare_numeric ~ tie_treatment + QACC_NEWS, df))
summary(lm(correction_mimshare_numeric ~ tie_treatment + QMIM_SEE_NEWS, df))
summary(lm(correction_mimshare_numeric ~ tie_treatment + QMISINFO_BELIEF02, df))
summary(lm(correction_mimshare_numeric ~ tie_treatment + QMIM_DISCUSS_NEWS, df))
summary(lm(correction_mimshare_numeric ~ tie_treatment + QMIM_DISCUSS_NEWS + QMISINFO_BELIEF02, df))
summary(lm(correction_mimshare_numeric ~ tie_treatment + QMIM_DISCUSS_NEWS + income_numeric, df))
summary(lm(correction_mimshare_numeric ~ tie_treatment + QMIM_DISCUSS_NEWS + resp_gender, df))
summary(lm(correction_mimshare_numeric ~ tie_treatment + QMIM_DISCUSS_NEWS + resp_age, df))
summary(lm(correction_mimshare_numeric ~ tie_treatment + QMIM_DISCUSS_NEWS + resp_age + education_numeric, df))
summary(lm(correction_mimshare_numeric ~ tie_treatment + QMIM_DISCUSS_NEWS + resp_age + QS13, df))


summary(lm(correction_mimshare_numeric ~ tie_group_treatment, df))
summary(lm(correction_mimshare_numeric ~ tie_group_treatment + QPOLI_INTEREST, df))
summary(lm(correction_mimshare_numeric ~ tie_group_treatment + QPOLI_ATT, df))
summary(lm(correction_mimshare_numeric ~ tie_group_treatment + QACC_NEWS, df))
summary(lm(correction_mimshare_numeric ~ tie_group_treatment + QMIM_SEE_NEWS, df))
summary(lm(correction_mimshare_numeric ~ tie_group_treatment + QMIM_DISCUSS_NEWS, df))
summary(lm(correction_mimshare_numeric ~ tie_group_treatment + QMIM_DISCUSS_NEWS + income_numeric, df))
summary(lm(correction_mimshare_numeric ~ tie_group_treatment + QMIM_DISCUSS_NEWS + resp_gender, df))
summary(lm(correction_mimshare_numeric ~ tie_group_treatment + QMIM_DISCUSS_NEWS + resp_age, df))
summary(lm(correction_mimshare_numeric ~ tie_group_treatment + QMIM_DISCUSS_NEWS + resp_age + education_numeric, df))
summary(lm(correction_mimshare_numeric ~ tie_group_treatment + QMIM_DISCUSS_NEWS + resp_age + QS13, df))

summary(lm(correction_mimshare_numeric ~ tie_treatment + QTIE_POLI_FREQ, df))
summary(lm(correction_mimshare_numeric ~ tie_group_treatment + QTIE_POLI_FREQ, df))
summary(lm(correction_mimshare_numeric ~ tie_group_treatment + QTIE_POLI_FREQ + QMIM_DISCUSS_NEWS, df))
summary(lm(correction_mimshare_numeric ~ tie_group_treatment + QTIE_POLI_FREQ + income_numeric, df)) 
summary(lm(correction_mimshare_numeric ~ tie_group_treatment + QTIE_POLI_FREQ + resp_gender, df)) 
summary(lm(correction_mimshare_numeric ~ tie_group_treatment + QTIE_POLI_FREQ + resp_age, df)) 
summary(lm(correction_mimshare_numeric ~ tie_group_treatment + QTIE_POLI_FREQ + resp_age + education_numeric, df)) 
summary(lm(correction_mimshare_numeric ~ tie_group_treatment + QTIE_POLI_FREQ + resp_age + QS13, df))

#################################
# analysis with collapsed dependent variable into 2 categories
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_treatment, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QPOLI_INTEREST, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QPOLI_ATT, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QACC_NEWS, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMIM_SEE_NEWS, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMISINFO_BELIEF02, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMIM_DISCUSS_NEWS, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMIM_DISCUSS_NEWS + QMISINFO_BELIEF02, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMIM_DISCUSS_NEWS + income_numeric, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMIM_DISCUSS_NEWS + resp_gender, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMIM_DISCUSS_NEWS + resp_age, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMIM_DISCUSS_NEWS + resp_age + education_numeric, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMIM_DISCUSS_NEWS + resp_age + QS13, df))

summary(lm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QPOLI_INTEREST, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QPOLI_ATT, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QACC_NEWS, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QMIM_SEE_NEWS, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QMIM_DISCUSS_NEWS, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QMIM_DISCUSS_NEWS + income_numeric, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QMIM_DISCUSS_NEWS + resp_gender, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QMIM_DISCUSS_NEWS + resp_age, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QMIM_DISCUSS_NEWS + resp_age + education_numeric, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QMIM_DISCUSS_NEWS + resp_age + QS13, df))

#################################
# analysis with inferred treatment variables
df <- add_inferred_treatment_variables(df, "median")
df <- add_inferred_treatment_variables(df, "divisive")
df <- add_inferred_treatment_variables(df, "agglomorative")

# Use inferred tie strength for treatment effects
round(prop.table(table(df$inferred_tie_treatment)), 3)
summary(lm(correction_mimshare_numeric ~ inferred_tie_treatment, df))
summary(lm(correction_mimshare_numeric ~ inferred_tie_treatment + QPOLI_INTEREST, df))
summary(lm(correction_mimshare_numeric ~ inferred_tie_treatment + QPOLI_ATT, df))
summary(lm(correction_mimshare_numeric ~ inferred_tie_treatment + QMIM_SEE_NEWS, df))
summary(lm(correction_mimshare_numeric ~ inferred_tie_treatment + QMIM_DISCUSS_NEWS, df))
ggplot(df, aes(inferred_tie_treatment, correction_mimshare_numeric, fill = inferred_tie_treatment)) +
  theme_bw() + theme(legend.position = "none") +
  stat_summary(geom = "bar", fun.y = mean) +
  stat_summary(geom = "errorbar", fun.data = mean_cl_boot, fun.args=list(conf.int=0.95)) +
  scale_fill_manual(values = c("red", "blue"))


# Use inferred agreement for treatment effects
round(prop.table(table(df$inferred_group_treatment)), 3)
summary(lm(correction_mimshare_numeric ~ inferred_group_treatment, df))
summary(lm(correction_mimshare_numeric ~ inferred_group_treatment + QPOLI_INTEREST, df))
summary(lm(correction_mimshare_numeric ~ inferred_group_treatment + QPOLI_ATT, df))
summary(lm(correction_mimshare_numeric ~ inferred_group_treatment + QMIM_SEE_NEWS, df))
summary(lm(correction_mimshare_numeric ~ inferred_group_treatment + QMIM_DISCUSS_NEWS, df))
ggplot(df, aes(inferred_group_treatment, correction_mimshare_numeric, fill = inferred_group_treatment)) +
  theme_bw() + theme(legend.position = "none") +
  stat_summary(geom = "bar", fun.y = mean) +
  stat_summary(geom = "errorbar", fun.data = mean_cl_boot, fun.args=list(conf.int=0.95)) +
  scale_fill_manual(values = c("red", "blue"))

# Use correction Format
# round(prop.table(table(df$format_treatment)), 3)
# summary(lm(correction_mimshare_numeric ~ format_treatment, df))
# summary(lm(correction_mimshare_numeric ~ format_treatment + QPOLI_INTEREST, df))
# summary(lm(correction_mimshare_numeric ~ format_treatment + QPOLI_ATT, df))
# summary(lm(correction_mimshare_numeric ~ format_treatment + QMIM_SEE_NEWS, df))
# summary(lm(correction_mimshare_numeric ~ format_treatment + QMIM_DISCUSS_NEWS, df))
# ggplot(df, aes(format_treatment, correction_mimshare_numeric, fill = format_treatment)) +
#   theme_bw() + theme(legend.position = "none") +
#   stat_summary(geom = "bar", fun.y = mean) +
#   stat_summary(geom = "errorbar", fun.data = mean_cl_boot, fun.args=list(conf.int=0.95)) +
#   scale_fill_manual(values = c("red", "green", "blue"))
# 

# Use inferred strength and agreement
round(prop.table(table(df$inferred_tie_group_treatment)), 3)
summary(lm(correction_mimshare_numeric ~ inferred_tie_group_treatment, df))
summary(lm(correction_mimshare_numeric ~ inferred_tie_group_treatment + QPOLI_INTEREST, df))
summary(lm(correction_mimshare_numeric ~ inferred_tie_group_treatment + QPOLI_ATT, df))
summary(lm(correction_mimshare_numeric ~ inferred_tie_group_treatment + QMIM_SEE_NEWS, df))
summary(lm(correction_mimshare_numeric ~ inferred_tie_group_treatment + QMIM_DISCUSS_NEWS, df))
ggplot(df, aes(inferred_tie_group_treatment, correction_mimshare_numeric, fill = inferred_tie_group_treatment)) +
  theme_bw() + theme(legend.position = "none") +
  stat_summary(geom = "bar", fun.y = mean) +
  stat_summary(geom = "errorbar", fun.data = mean_cl_boot, fun.args=list(conf.int=0.95)) +
  scale_fill_manual(values = c("red", "orange", "green", "blue"))


# Use inferred strength and agreement and format
round(prop.table(table(df$inferred_tie_group_format_treatment)), 3)
summary(lm(correction_mimshare_numeric ~ inferred_tie_group_format_treatment, df))
summary(lm(correction_mimshare_numeric ~ inferred_tie_group_format_treatment + QPOLI_INTEREST, df))
summary(lm(correction_mimshare_numeric ~ inferred_tie_group_format_treatment + QPOLI_ATT, df))
summary(lm(correction_mimshare_numeric ~ inferred_tie_group_format_treatment + QMIM_SEE_NEWS, df))
summary(lm(correction_mimshare_numeric ~ inferred_tie_group_format_treatment + QMIM_DISCUSS_NEWS, df))
ggplot(df, aes(inferred_tie_agree_format, correction_mimshare_numeric, fill = inferred_tie_agree_format)) +
  theme_bw() + theme(legend.position = "none", axis.text=element_text(size=6)) +
  stat_summary(geom = "bar", fun.y = mean) +
  stat_summary(geom = "errorbar", fun.data = mean_cl_boot, fun.args=list(conf.int=0.95))


###################################################################
# encouragement design analysis 
# Weak or Strong instrument? compute the F-statistics of treatment instrument on actual treatment.
# Is F-statistic greater than 20?
df <- add_inferred_treatment_variables(df, "median")
df <- add_inferred_treatment_variables(df, "divisive")
df <- add_inferred_treatment_variables(df, "agglomorative")
summary(lm(inferred_tie_treatment_numeric ~ tie_treatment, df))
summary(lm(inferred_tie_treatment_numeric ~ tie_treatment, df))$fstatistic

ACE <- mean(df$correction_mimshare_numeric[df$tie_treatment=="Strong"]) - mean(df$correction_mimshare_numeric[df$tie_treatment=="Weak"])
p_complier <- mean(df$inferred_tie_treatment[df$tie_treatment=="Strong"] == "Strong") - mean(df$inferred_tie_treatment[df$tie_treatment=="Weak"] == "Strong")
ACE/p_complier

ivmodel <- ivreg(formula = correction_mimshare_numeric ~ inferred_tie_treatment | tie_treatment,
                 data = df, x = TRUE)
summary(ivmodel)
ivmodel <- ivreg(formula = correction_mimshare_numeric ~ inferred_group_treatment | group_treatment,
                 data = df, x = TRUE)
summary(ivmodel)
ivmodel <- ivreg(formula = correction_mimshare_numeric ~ inferred_tie_group_treatment | tie_group_treatment,
                 data = df, x = TRUE)
summary(ivmodel)
ivmodel <- ivreg(formula = correction_mimshare_numeric ~ inferred_tie_treatment + QMIM_DISCUSS_NEWS | tie_treatment + QMIM_DISCUSS_NEWS,
                 data = df, x = TRUE)
summary(ivmodel)
ivmodel <- ivreg(formula = correction_mimshare_numeric ~ inferred_group_treatment + QMIM_DISCUSS_NEWS | group_treatment + QMIM_DISCUSS_NEWS,
                 data = df, x = TRUE)
summary(ivmodel)
ivmodel <- ivreg(formula = correction_mimshare_numeric ~ inferred_tie_group_treatment + QMIM_DISCUSS_NEWS | tie_group_treatment + QMIM_DISCUSS_NEWS,
                 data = df, x = TRUE)
summary(ivmodel)

########
# IV regression where instrument is the interaction of treatment with another variable that is correlated with inferred treatment
# This leads to a conditional model where the auxiliary variable has to be included
variables = c("correction_interest1_numeric", "correction_interest2_numeric", "correction_interest3_numeric",
              "correction_credibility1_numeric", "correction_credibility2_numeric", "correction_credibility1_numeric", "correction_credibility4_numeric",
              "correction_interest1_binary_numeric", "correction_interest2_binary_numeric", "correction_interest3_binary_numeric",
              "correction_credibility1_binary_numeric", "correction_credibility2_binary_numeric", "correction_credibility1_binary_numeric", "correction_credibility4_binary_numeric",
              "tie_credibility1_numeric", "tie_credibility2_numeric", "tie_credibility3_numeric",
              "tie_political_freq_numeric", "tie_diff_gender_numeric")

# Get the f-statistic of the new IV when the interacted variable is included as control              
idx <- 4
df$new_var <- df$tie_treatment_numeric * df[[variables[idx]]]
mod1 <- lm(paste("inferred_tie_treatment_numeric ~ ", variables[[idx]]), df)
summary(mod1)
mod2 <- lm(paste("inferred_tie_treatment_numeric ~ new_var +", variables[[idx]]), df)
summary(mod2)
waldtest(mod2, mod1)
mod0 <- lm("inferred_tie_treatment_numeric ~ tie_treatment", df)
summary(mod0)

formula <- paste("correction_mimshare_numeric ~ inferred_tie_treatment +", variables[idx], "| new_var +", variables[idx])
ivmodel <- ivreg(formula = formula, data = df, x = TRUE)
summary(ivmodel)
formula <- paste("correction_mimshare_numeric ~ inferred_tie_treatment + QMIM_DISCUSS_NEWS +", variables[idx], "| new_var + QMIM_DISCUSS_NEWS +", variables[idx])
ivmodel <- ivreg(formula = formula, data = df, x = TRUE)
summary(ivmodel)

#######
# IV estimate with weak instrument
conf_level = 0.95
ivmodel <- ivreg(formula = correction_mimshare_numeric ~ inferred_tie_treatment + QMIM_DISCUSS_NEWS | tie_treatment + QMIM_DISCUSS_NEWS,
                 data = df, x = TRUE)
ivmodel <- ivreg(formula = correction_mimshare_numeric ~ inferred_tie_treatment + QMIM_DISCUSS_NEWS + QCORR_CRED01 | tie_treatment + QMIM_DISCUSS_NEWS + QCORR_CRED01,
                 data = df, x = TRUE)
summary(ivmodel)
ivmodel_coef <- ivmodel$coefficients[2]
ivmodel_se = sqrt(diag(vcovHC(ivmodel, "HC3")))[2]
coeftest(ivmodel, vcov = vcovHC(ivmodel, "HC3"))
# construct confidence interval based on model 3
ci_low = ivmodel_coef + qnorm((1-conf_level)/2) * ivmodel_se
ci_high = ivmodel_coef - qnorm((1-conf_level)/2) * ivmodel_se
ivmodel_ci = paste0("(", round(ci_low, 3), " , ", round(ci_high, 3), ")")
ivmodel_ci

# first compute the f-statistic in the first stage
first_stage = (lm(inferred_tie_treatment_numeric ~ tie_treatment, df))
first_stage$coefficients
f_stat = (first_stage$coefficients[2])^2 / vcovHC(first_stage, "HC3")[2,2]
f_stat

# create the grid based on the estimate from previous model
alpha_se = ivmodel_se
alphas = ivmodel_coef + seq(-5*alpha_se, 5*alpha_se, by=alpha_se/20)
#alphas

accepted_alphas = c()
for(alpha in alphas) {
  y = df$correction_mimshare_numeric - alpha*(df$inferred_tie_treatment_numeric)
  x1 = df$tie_treatment_numeric
  x2 = df$QMIM_DISCUSS_NEWS
  x3 = df$QCORR_CRED01
  lmfit = lm(y ~ x1 + x2 + x3)
  summary(lmfit)
  # compute the wald statistic of regressing Expropriation on the instrument and control
  wald = (lmfit$coefficients[2])^2 / vcovHC(lmfit, "HC3")[2,2]
  # check if the wald stat passes the test, if so the alpha is in the confidence set
  if(wald <= qchisq(conf_level, 1))  {
    accepted_alphas = c(accepted_alphas, alpha)
  }
}

# generate the confidence band and the estimated alpha as mid point
ci_low = min(accepted_alphas)
ci_high = max(accepted_alphas)
estimated_alpha = (ci_low + ci_high)/2
robust_iv_se = (ci_high - ci_low) / (2 * qnorm((1 - conf_level)/2, lower.tail = FALSE))
# construct the confidence interval for model4
weak_iv_ci = paste0("(", round(ci_low, 3), " , ", round(ci_high, 3), ")")
weak_iv_ci









control_vars <- c("QMIM_DISCUSS_NEWS", #"QSNS_DISCUSS_NEWS",
                  "QCORR_CRED01", "QCORR_CRED02", "QCORR_CRED03", "QCORR_CRED04",
                  "QCORR_IN01", "QCORR_IN02", "QCORR_IN03",
                  "QMISINFO_BELIEF02", "QMISINFO_BELIEF01",
                  "QPOLI_ATT", "QPOLI_INTEREST", "QACC_NEWS",
                  "QMIM_SEE_NEWS", #"QSNS_SEE_NEWS",
                  "QTIE_CRED01", "QTIE_CRED02", "QTIE_CRED03", "QTIE_CRED03",
                  "QTIE_POLI_FREQ", "QTIE_GEN")
control_vars_combs <- combn(control_vars, 2, simplify=F)
indep_var <- "inferred_group_treatment"
instrument_var <- "group_treatment"




for(real_tie in c(FALSE, TRUE)) {
  for(search_internet in c(FALSE, TRUE)) {
    for(attention_check in c(FALSE, TRUE)) {
      for(correct_misinfo in c(FALSE, TRUE)) {
        for(correct_correction in c(FALSE, TRUE)) {
          for(i in seq(1, length(control_vars_combs))) {
            for(quant in seq(0.1, 0.9, 0.05)) {
              controls <- control_vars_combs[[i]]
              df <- exclude_nonattentive(data,
                                         no_treatment=TRUE,
                                         real_tie=real_tie,
                                         search_internet=search_internet,
                                         attention_check=attention_check,
                                         correct_misinfo=correct_misinfo,
                                         correct_correction=correct_correction)
              
              capture.output(df <- add_inferred_treatment_variables(df, quant))
              formula <- paste0("correction_mimshare_numeric ~ ", indep_var, " + ", paste(controls, collapse=" + "),
                                " | ", instrument_var, " + ", paste(controls, collapse=" + "))
              ivmodel <- ivreg(formula = formula, data = df, x = TRUE)
              ivmodel_sum <- summary(ivmodel)
              ivmodel_coef <- ivmodel_sum$coefficients[2,1]
              ivmodel_pval <- ivmodel_sum$coefficients[2,4]
              if(ivmodel_coef < 0 & ivmodel_pval < 0.005) {
                cat(paste("real_tie:", real_tie, "search_internet:", search_internet,
                          "attention_check:", attention_check, "correct_misinfo", correct_misinfo,
                          "correct_correction:", correct_correction, "quantile:", quant,
                          "controls:", paste(controls, collapse=" "), "\n"))
                print(ivmodel_sum$coefficients[2,])
                cat('*********************\n')
              }
            }
          }
        }
      }
    }
  }
}


























