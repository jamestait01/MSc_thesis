#this code is identical but it only creates the forest plot for the variable of interest, not the covariates

data <- readRDS("~/Summer_project/baseline_models/model_3a/proper_HES/baseline_data.rds")

data <- subset(select=c("eid","No. of total hosp","No. of CVD hosp"),data)

changed_meds <- readRDS("~/Summer_project/baseline_models/model_3a/final_data.rds")

data <- merge(data,changed_meds,by="eid")

library(dplyr)
data <- data %>%
  rename(`Baseline systolic blood pressure` = sys_bp.0,
         `Baseline diastolic blood pressure` = dias_bp.0)

data$`Age group` <- cut(data$Age, breaks=c(40,55,65,Inf),labels=c("40-55","55-65","65+")
                        ,right = FALSE)

data$`Medication status` <- relevel(data$`Medication status`,ref="Stayed untreated")

poisson_model <- glm(`No. of total hosp` ~ Sex + `Age group` + BMI + `Smoking status` +  `Weekly alcohol intake` +
                       `Index of Multiple Deprivation` + `Number of comorbidities` +
  `Medication status`,family="poisson",data=data)

cardiac_poisson <- glm(`No. of CVD hosp` ~ Sex + `Age group` + BMI + `Smoking status` +  `Weekly alcohol intake` +
                         `Index of Multiple Deprivation` + `Number of comorbidities` +
                         `Medication status`,family="poisson",data=data)

library(tableone)
library(gtsummary)
library(dplyr)
library(bstfun)
 
poisson_hosp_tbl <- poisson_model %>%
  tbl_regression(exponentiate=T,add_estimate_to_reference_rows = TRUE)


poisson_cardiac_tbl <- cardiac_poisson %>%
tbl_regression(exponentiate=T,add_estimate_to_reference_rows=TRUE) %>%
  bold_labels()


#merge the 2 tables together
merged_poisson <- tbl_merge(list(poisson_hosp_tbl,poisson_cardiac_tbl),tab_spanner = c("**Total hospitalisations**",
                                                                         "**CVD-specific hospitalisations**"))

gt::gtsave(as_gt(merged_poisson),"merged_poisson_table.png",expand=40) 

library(writexl)
#save as excel file to make latex table
merged_poisson %>%
  gtsummary::as_tibble() %>% 
  writexl::write_xlsx(., "example_gtsummary1.xlsx")
  
#visualise results
  
  
#plot_models(poisson_model,cardiac_poisson,m.labels = c("Overall hospitalisations","CVD-specific hospitalisations"),vline.color="black",
 #             title="Incidence rate ratio of hospitalisations",
  #            legend.title="Model",prefix.labels=c("varname"),
   #           axis.lim=c(0.5,4)) + scale_y_continuous(limits = c(0.5,4))
  

#look for those on stable meds, compare among categories
stable_meds <- data[data$`Medication status`=="Stable medications",]

#remove untreated factor level 
stable_meds$`Medication combination` <- droplevels(stable_meds$`Medication combination`)

levels(stable_meds$`Medication combination`) <- c("ACE inhibitor/ARB","ACE inhibitor/ARB + CCB",
                                                    "ACE inhibitor/ARB + CCB + Diuretic",
                                                    "ACE inhibitor/ARB + Diuretic",
                                                    "CCB", "CCB + Diuretic")

#relevel medication combination 
stable_meds$`Medication combination` <- relevel(stable_meds$`Medication combination`,ref="ACE inhibitor/ARB")


poisson_meds <- glm(`No. of total hosp` ~ Sex + `Age group` + BMI + `Smoking status` +  `Weekly alcohol intake` +
                      `Index of Multiple Deprivation` + `Number of comorbidities` +
                       `Medication combination`,family="poisson",data=stable_meds)

cardiac_meds <- glm(`No. of CVD hosp` ~ Sex + `Age group` + BMI + `Smoking status` +  `Weekly alcohol intake` +
                      `Index of Multiple Deprivation` + `Number of comorbidities` +
                      `Medication combination`,family="poisson",data=stable_meds)

poisson_meds_tbl <- poisson_meds %>%
  tbl_regression(exponentiate=T) %>%
  bold_labels()


log_cardiac_tbl <- cardiac_meds %>%
  tbl_regression(exponentiate=T) %>%
  bold_labels()

#Poisson models with stable meds with baseline blood pressure measurements

poisson_meds_with_bp <- glm(n_hospitalisations_unique ~ Sex + `Age group` + BMI + `Smoking status` +  `Weekly alcohol intake` +
                                      `Index of Multiple Deprivation` + `Number of comorbidities` +
                          `Medication combination` + `Baseline systolic blood pressure` + 
                          `Baseline diastolic blood pressure`,family="poisson",data=stable_meds)

poisson_meds_tbl_with_bp <- poisson_meds_tbl_with_bp %>%
  tbl_regression(exponentiate=T) %>%
  bold_labels()

#merge the 2 total hospitalisation stable meds tables with and without blood pressure together
merged_total_stable_meds <- tbl_merge(list(poisson_meds_tbl,poisson_meds_tbl_with_bp),tab_spanner = c("**Model 1: Without blood pressure**",
                                                                                       "**Model 2: With blood pressure**"))

gt::gtsave(as_gt(merged_total_stable_meds),"merged_poisson_stable_meds_table.png",expand=40) 



#visualise results

#library(sjPlot)
#library(sjlabelled)
#library(sjmisc)
#library(ggplot2)

#plot_models(poisson_meds,poisson_meds_with_bp,m.labels = c("Model 1: Without blood pressure adjustment","Model 2: With blood pressure adjustment")
 #           ,vline.color="black",
  #          title="Incidence rate ratio of total hospitalisations before and after adjusting for baseline blood pressure for participants staying on consistent medications ",
   #         legend.title="Model",prefix.labels=c("varname"),
    #        axis.lim=c(0.5,2)) + scale_y_continuous(limits = c(0.5,2))


#try matthew's code
library(ggplot2)
library(ggforce)
library(dplyr)


#make list of categorical variables
categorical_variables <- c("Sex","`Age group`","`Smoking status`","`Weekly alcohol intake`","`Number of comorbidities`",
                           "`Medication status`")

#make list of reference levels for each categorical variable included in model
ref_levels <- c("Female","40-55","Never","Never","0","Stayed untreated")

#make function to extract coefficients and 95% CI.s from model
makeORTable <- function(mod, ref_levels = NULL, dp = 3, categorical_variables = NULL) {
  mod_exp <- (mod$family$family == "poisson")
  
  if (class(mod)[1] == "gam") {
    tab <- as.data.frame(summary.gam(mod)$p.table)
    tab$Lower = tab$Estimate - 1.96 * tab$`Std. Error`
    tab$Upper = tab$Estimate + 1.96 * tab$`Std. Error`
    tab <- tab %>% dplyr::select(Estimate, Lower, Upper, `Pr(>|z|)`)
    tab$Level <- rownames(tab)
    tab <- tab %>% dplyr::select(Level, everything())
    colnames(tab) <- c("Level", "OR", "Lower", "Upper", "P_value")
    
    if (!is.null(categorical_variables)) {
      for (i in seq_along(categorical_variables)) {
        ref_level_name <- ifelse(is.null(ref_levels), 
                                 paste0(categorical_variables[i], " [reference]"),
                                 paste0(ref_levels[i], " [reference]"))
        tab <- rbind(tab, c(ref_level_name, 1, NA_real_, NA_real_, NA_real_))
      }
    }
    
    tab[, 2] <- round(exp(as.numeric(tab[, 2])), dp)
    tab[, 3] <- round(exp(as.numeric(tab[, 3])), dp)
    tab[, 4] <- round(exp(as.numeric(tab[, 4])), dp)
    tab[, 5] <- round(as.numeric(tab[, 5]), 5)
  } else {
    tab <- jtools::summ(mod, exp = mod_exp, ORs = mod_exp)
    
    if (mod_exp) {
      tab <- tab$coeftable %>% as.data.frame() %>% dplyr::select(1, 2, 3, 5)
    } else {
      tab <- tab$coeftable %>% as.data.frame() %>% dplyr::select(1, 2, 4)
      tab$Lower = tab$Est. - qnorm(p = 0.975, mean = 0, sd = 1) * tab$S.E.
      tab$Upper = tab$Est. + qnorm(p = 0.975, mean = 0, sd = 1) * tab$S.E.
      tab <- tab[, c(1, 4, 5, 3)]
    }
    
    tab$Level <- rownames(tab)
    tab <- tab %>% dplyr::select(Level, everything())
    colnames(tab) <- c("Level", "OR", "Lower", "Upper", "P_value")
    
    if (!is.null(categorical_variables)) {
      for (i in seq_along(categorical_variables)) {
        ref_level_name <- ifelse(is.null(ref_levels), 
                                 paste0(categorical_variables[i], " [reference]"),
                                 paste0(ref_levels[i], " [reference]"))
        tab <- rbind(tab, c(ref_level_name, 1, NA_real_, NA_real_, NA_real_))
      }
    }
    
    tab[, 2] <- round(as.numeric(tab[, 2]), dp)
    tab[, 3] <- round(as.numeric(tab[, 3]), dp)
    tab[, 4] <- round(as.numeric(tab[, 4]), dp)
    tab[, 5] <- round(as.numeric(tab[, 5]), 5)
  }
  
  rownames(tab) <- 1:nrow(tab)
  return(tab)
}

test <- makeORTable(poisson_model,ref_levels=ref_levels, categorical_variables=categorical_variables)

#rename appropriate variables
test$Level <- c("Intercept","Male","55-65","65+","BMI","Current","Prefer not to answer","Previous","1-2 times a week",
                "1-3 times a month","3-4 times a week","Daily/almost daily","Prefer not to answer","Special occasions",
                "Index of Multiple Deprivation","1-2",">=3","Stable medications","Changed medications","Decreased dosage",
                "Ended medication","Increased dosage","Started medication","Female [reference]","40-55 [reference]","Never [reference]",
"Never [reference]","0 [reference]","Stayed untreated [reference]")

#remove intercept row
test <- test[-1,]
predictors <- c("Sex","`Age group`", "BMI","`Smoking status`", "`Weekly alcohol intake`", 
                  "`Index of Multiple Deprivation`", "`Number of comorbidities`",
                  "`Medication status`")

#add predictor names to table
test$predictor <- c("Sex","Age group","Age group", "BMI","Smoking status","Smoking status","Smoking status",
                    "Weekly alcohol intake","Weekly alcohol intake","Weekly alcohol intake","Weekly alcohol intake",
                    "Weekly alcohol intake","Weekly alcohol intake","Index of Multiple Deprivation",
                    "Number of comorbidities","Number of comorbidities","Medication status",
                    "Medication status","Medication status","Medication status","Medication status","Medication status",
                    "Sex","Age group","Smoking status","Weekly alcohol intake","Number of comorbidities","Medication status")

#rename the variables now
test$Level <- c("Male","55-65","65+","BMI","Current","Prefer not to answer","Previous","1-2 times a week",
                "1-3 times a month","3-4 times a week","Daily/almost daily","Prefer not to answer","Special occasions",
                "Index of Multiple Deprivation","1-2",">=3","Stable medications","Changed medications",
                "Decreased dosage","Ended medication","Increased dosage","Started medication","Female [reference]",
                "40-55 [reference]","Never [reference]","Never [reference]","0 [reference]","Stayed untreated [reference]")

#now let's do the same for the cardiac_poisson model and combine the 2 together

cardiac_table <- makeORTable(cardiac_poisson,ref_levels=ref_levels, categorical_variables=categorical_variables)

#remove intercept
cardiac_table <- cardiac_table[-1,]

#rename appropriate variables
cardiac_table$Level <- c("Male","55-65","65+","BMI","Current","Prefer not to answer","Previous","1-2 times a week",
                "1-3 times a month","3-4 times a week","Daily/almost daily","Prefer not to answer","Special occasions",
                "Index of Multiple Deprivation","1-2",">=3","Stable medications","Changed medications","Decreased dosage",
                "Ended medication","Increased dosage","Started medication","Female [reference]","40-55 [reference]","Never [reference]",
                "Never [reference]","0 [reference]","Stayed untreated [reference]")

#add predictor names to table
cardiac_table$predictor <- c("Sex","Age group","Age group", "BMI","Smoking status","Smoking status","Smoking status",
                    "Weekly alcohol intake","Weekly alcohol intake","Weekly alcohol intake","Weekly alcohol intake",
                    "Weekly alcohol intake","Weekly alcohol intake","Index of Multiple Deprivation",
                    "Number of comorbidities","Number of comorbidities","Medication status",
                    "Medication status","Medication status","Medication status","Medication status","Medication status",
                    "Sex","Age group","Smoking status","Weekly alcohol intake","Number of comorbidities","Medication status")


# Combine the two OR tables
combined_IRR_table <- bind_rows(
  mutate(test, model_type = "Total hospitalisations"),
  mutate(cardiac_table, model_type = "CVD-specific hospitalisations"))

voi_table <- combined_IRR_table %>%
  filter(predictor=="Medication status")

#specify order_levels for the ggplot
order_levels <- c("Sex","Age group","BMI","Smoking status",
                  "Weekly alcohol intake","Index of Multiple Deprivation",
                  "Number of comorbidities","Medication status")

# Create the forest plot with facets for each predictor and color by model type
ggplot(data = voi_table, aes(x = Level, y = OR, color = model_type)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), position = position_dodge(width = 0.5), width = 0.2) +
  coord_flip() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey60") +
  scale_y_log10() +
  labs(col = "Model", x = "", y = "Incidence Rate Ratio") +
  theme_bw() + #get rid of grey background 
  theme(strip.background =element_rect(fill="white"),legend.position="bottom")+
  theme(strip.text = element_text(colour = 'black',face="bold")) +
  theme(axis.text.y = element_text(face = "bold",colour="black")) +
  # facet_col(~ predictor, scales = "free_y", space = "free") +
  scale_color_manual(values = c("Total hospitalisations" = "blue", "CVD-specific hospitalisations" = "red")) +
  labs(x="Medication status") +
  theme(legend.margin=margin(-10, 0, 0, 0)) #move legend label closer to the x-axis for less white space


ggsave(filename="changed_meds_poisson_hosp.png",
       path="C:/Users/james/Documents/MSc/Thesis3/ggplots",
       width = 12, height = 10, dpi = 300, units = "cm")

#repeat the above code but for stable meds data

#for total hospitalisations

#make list of categorical variables
categorical_variables <- c("Sex","`Age group`","`Smoking status`","`Weekly alcohol intake`","`Number of comorbidities`",
                           "`Medication combination`")

#make list of reference levels for each categorical variable included in model
ref_levels <- c("Female","40-55","Never","Never","0","ACE inhibitor/ARB")

stable_meds_table <- makeORTable(poisson_meds,ref_levels=ref_levels, categorical_variables=categorical_variables)

#stable_meds_table_with_bp <- makeORTable(poisson_meds_with_bp,ref_levels=ref_levels, categorical_variables=categorical_variables)

stable_meds_cardiac_table <- makeORTable(cardiac_meds,ref_levels=ref_levels,categorical_variables=categorical_variables)

#remove intercept row
stable_meds_table <- stable_meds_table[-1,]
stable_meds_table_with_bp <- stable_meds_table_with_bp[-1,]
stable_meds_cardiac_table <- stable_meds_cardiac_table[-1,]

#rename appropriate variables
stable_meds_table$Level <- c("Male","55-65","65+","BMI","Current","Prefer not to answer","Previous","1-2 times a week",
                "1-3 times a month","3-4 times a week","Daily/almost daily","Prefer not to answer","Special occasions",
                "Index of Multiple Deprivation","1-2",">=3","ACE inhibitor/ARB + CCB",
                "ACE inhibitor/ARB + CCB + Diuretic","ACE inhibitor/ARB + Diuretic","CCB","CCB + Diuretic",
                "Female [reference]","40-55 [reference]","Never [reference]",
                "Never [reference]","0 [reference]","ACE inhibitor/ARB [reference]")

#stable_meds_table_with_bp$Level <- c("Male","55-65","65+","BMI","Current","Prefer not to answer","Previous","1-2 times a week",
                             "1-3 times a month","3-4 times a week","Daily/almost daily","Prefer not to answer","Special occasions",
                             "Index of Multiple Deprivation","1-2",">=3","ACE inhibitor/ARB + CCB",
                             "ACE inhibitor/ARB + CCB + Diuretic","ACE inhibitor/ARB + Diuretic","CCB","CCB + Diuretic",
                             "Baseline systolic blood pressure (mmHg)","Baseline diastolic blood pressure (mmHg)",
                             "Female [reference]","40-55 [reference]","Never [reference]",
                             "Never [reference]","0 [reference]","ACE inhibitor/ARB [reference]")

stable_meds_cardiac_table$Level <- c("Male","55-65","65+","BMI","Current","Prefer not to answer","Previous","1-2 times a week",
                             "1-3 times a month","3-4 times a week","Daily/almost daily","Prefer not to answer","Special occasions",
                             "Index of Multiple Deprivation","1-2",">=3","ACE inhibitor/ARB + CCB",
                             "ACE inhibitor/ARB + CCB + Diuretic","ACE inhibitor/ARB + Diuretic","CCB","CCB + Diuretic",
                             "Female [reference]","40-55 [reference]","Never [reference]",
                             "Never [reference]","0 [reference]","ACE inhibitor/ARB [reference]")

predictors <- c("Sex","`Age group`", "BMI","`Smoking status`", "`Weekly alcohol intake`", 
                "`Index of Multiple Deprivation`", "`Number of comorbidities`",
                "`Medication combination`")

#add predictor names to table
stable_meds_table$predictor <- c("Sex","Age group","Age group", "BMI","Smoking status","Smoking status","Smoking status",
                    "Weekly alcohol intake","Weekly alcohol intake","Weekly alcohol intake","Weekly alcohol intake",
                    "Weekly alcohol intake","Weekly alcohol intake","Index of Multiple Deprivation",
                    "Number of comorbidities","Number of comorbidities","Medication combination",
                    "Medication combination","Medication combination","Medication combination",
                    "Medication combination",
                    "Sex","Age group","Smoking status","Weekly alcohol intake","Number of comorbidities","Medication combination")

#add predictor names to table
stable_meds_cardiac_table$predictor <- c("Sex","Age group","Age group", "BMI","Smoking status","Smoking status","Smoking status",
                                 "Weekly alcohol intake","Weekly alcohol intake","Weekly alcohol intake","Weekly alcohol intake",
                                 "Weekly alcohol intake","Weekly alcohol intake","Index of Multiple Deprivation",
                                 "Number of comorbidities","Number of comorbidities","Medication combination",
                                 "Medication combination","Medication combination","Medication combination",
                                 "Medication combination",
                                 "Sex","Age group","Smoking status","Weekly alcohol intake","Number of comorbidities","Medication combination")


#stable_meds_table_with_bp$predictor <- c("Sex","Age group","Age group", "BMI","Smoking status","Smoking status","Smoking status",
                                         "Weekly alcohol intake","Weekly alcohol intake","Weekly alcohol intake","Weekly alcohol intake",
                                         "Weekly alcohol intake","Weekly alcohol intake","Index of Multiple Deprivation",
                                         "Number of comorbidities","Number of comorbidities","Medication combination",
                                         "Medication combination","Medication combination","Medication combination",
                                         "Medication combination", "Baseline systolic blood pressure (mmHg)",
                                         "Baseline diastolic blood pressure (mmHg)",
                                         "Sex","Age group","Smoking status","Weekly alcohol intake",
                                         "Number of comorbidities","Medication combination")


# Combine the two OR tables
#combined_stable_meds_table <- bind_rows(
 # mutate(stable_meds_table, model_type = "Without blood pressure"),
#  mutate(stable_meds_table_with_bp, model_type = "With blood pressure"))


#Combine the two tables
combined_stable_meds_table <- bind_rows(
  mutate(stable_meds_table, model_type = "Total hospitalisations"),
  mutate(stable_meds_cardiac_table, model_type = "CVD-specific hospitalisations"))

voi_table <- combined_stable_meds_table %>%
  filter(predictor=="Medication combination") 

# Create the forest plot with facets for each predictor and color by model type
ggplot(data = voi_table, aes(x = Level, y = OR, color = model_type)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), position = position_dodge(width = 0.5), width = 0.2) +
  coord_flip() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey60") +
  scale_y_log10() +
  labs(col = "Model", x = "", y = "Incidence Rate Ratio") +
  theme_bw() + #get rid of grey background 
  theme(strip.background =element_rect(fill="white"),legend.position="bottom")+
  theme(strip.text = element_text(colour = 'black',face="bold")) +
  theme(axis.text.y = element_text(face = "bold",colour="black")) +
  # facet_col(~ predictor, scales = "free_y", space = "free") +
  scale_color_manual(values = c("Total hospitalisations" = "blue", "CVD-specific hospitalisations" = "red")) +
  labs(x="Medication status") +
  theme(legend.margin=margin(-10, 0, 0, 0)) #move legend label closer to the x-axis for less white space


