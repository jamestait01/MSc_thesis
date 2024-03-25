data <- readRDS("~/Summer_project/baseline_models/model_3a/proper_HES/baseline_data.rds")

library(dplyr)
data <- data %>%
  rename(`Baseline systolic blood pressure` = sys_bp.0,
         `Baseline diastolic blood pressure` = dias_bp.0)

data$`Age group` <- cut(data$Age, breaks=c(40,55,65,Inf),labels=c("40-55","55-65","65+")
                        ,right = FALSE)

levels(data$`Medication combination`) <- c("Untreated","ACE inhibitor/ARB","ACE inhibitor/ARB + CCB",
                                                  "ACE inhibitor/ARB + CCB + Diuretic",
                                                  "ACE inhibitor/ARB + Diuretic",
                                                  "CCB", "CCB + Diuretic")

poisson_model <- glm(`No. of total hosp` ~ Sex + `Age group` + BMI + `Smoking status` +  `Weekly alcohol intake` +
                       `Index of Multiple Deprivation` + `Number of comorbidities` +
                       `Medication combination`,family="poisson",data=data)

cardiac_model <- glm(`No. of CVD hosp` ~ Sex + `Age group` + BMI + `Smoking status` +  `Weekly alcohol intake` +
                       `Index of Multiple Deprivation` + `Number of comorbidities` +
                       `Medication combination`,family="poisson",data=data)


library(tableone)
library(gtsummary)
library(dplyr)

poisson_hosp_tbl <- poisson_model %>%
  tbl_regression(exponentiate=T) %>%
  bold_labels()


poisson_cardiac_tbl <- cardiac_model %>%
  tbl_regression(exponentiate=T) %>%
  bold_labels()


#merge the 2 tables together
merged_poisson <- tbl_merge(list(poisson_hosp_tbl,poisson_cardiac_tbl),tab_spanner = c("**Total hospitalisations**",
                                                                                       "**CVD-specific hospitalisations**"))

setwd("/rds/general/user/jt1019/home/Summer_project/baseline_models/model_3a")
gt::gtsave(as_gt(merged_poisson),"merged_poisson_baseline_table.png",expand=40) 

#code to make forestplot

#make list of categorical variables
categorical_variables <- c("Sex","`Age group`","`Smoking status`","`Weekly alcohol intake`","`Number of comorbidities`",
                           "`Medication combination`")

#make list of reference levels for each categorical variable included in model
ref_levels <- c("Female","40-55","Never","Never","0","Untreated")

total_hosp <- makeIRRTable(poisson_model,ref_levels=ref_levels, categorical_variables=categorical_variables)

cardiac_hosp <- makeIRRTable(cardiac_model,ref_levels=ref_levels, categorical_variables=categorical_variables)

#remove intercept row
total_hosp <- total_hosp[-1,]
cardiac_hosp <- cardiac_hosp[-1,]
#rename appropriate variables
total_hosp$Level <- c("Male","55-65","65+","BMI","Current","Prefer not to answer","Previous","1-2 times a week",
                             "1-3 times a month","3-4 times a week","Daily/almost daily","Prefer not to answer","Special occasions",
                             "Index of Multiple Deprivation","1-2",">=3","ACE inhibitor/ARB","ACE inhibitor/ARB + CCB",
                             "ACE inhibitor/ARB + CCB + Diuretic","ACE inhibitor/ARB + Diuretic","CCB","CCB + Diuretic",
                             "Female [reference]","40-55 [reference]","Never [reference]",
                             "Never [reference]","0 [reference]","Untreated [reference]")

cardiac_hosp$Level <- c("Male","55-65","65+","BMI","Current","Prefer not to answer","Previous","1-2 times a week",
                      "1-3 times a month","3-4 times a week","Daily/almost daily","Prefer not to answer","Special occasions",
                      "Index of Multiple Deprivation","1-2",">=3","ACE inhibitor/ARB","ACE inhibitor/ARB + CCB",
                      "ACE inhibitor/ARB + CCB + Diuretic","ACE inhibitor/ARB + Diuretic","CCB","CCB + Diuretic",
                      "Female [reference]","40-55 [reference]","Never [reference]",
                      "Never [reference]","0 [reference]","Untreated [reference]")


predictors <- c("Sex","`Age group`", "BMI","`Smoking status`", "`Weekly alcohol intake`", 
                "`Index of Multiple Deprivation`", "`Number of comorbidities`",
                "`Medication combination`")

#add predictor names to table
total_hosp$predictor <- c("Sex","Age group","Age group", "BMI","Smoking status","Smoking status","Smoking status",
                                 "Weekly alcohol intake","Weekly alcohol intake","Weekly alcohol intake","Weekly alcohol intake",
                                 "Weekly alcohol intake","Weekly alcohol intake","Index of Multiple Deprivation",
                                 "Number of comorbidities","Number of comorbidities","Medication combination",
                                 "Medication combination","Medication combination","Medication combination",
                                 "Medication combination","Medication combination",
                                 "Sex","Age group","Smoking status","Weekly alcohol intake","Number of comorbidities","Medication combination")

cardiac_hosp$predictor <- c("Sex","Age group","Age group", "BMI","Smoking status","Smoking status","Smoking status",
                                         "Weekly alcohol intake","Weekly alcohol intake","Weekly alcohol intake","Weekly alcohol intake",
                                         "Weekly alcohol intake","Weekly alcohol intake","Index of Multiple Deprivation",
                                         "Number of comorbidities","Number of comorbidities","Medication combination",
                                         "Medication combination","Medication combination","Medication combination",
                                         "Medication combination","Medication combination",
                                         "Sex","Age group","Smoking status","Weekly alcohol intake",
                                         "Number of comorbidities","Medication combination")


# Combine the two OR tables
table <- bind_rows(
  mutate(total_hosp, model_type = "Total hospitalisations"),
  mutate(cardiac_hosp, model_type = "CVD-specific hospitalisations"))

library(ggplot2)
library(ggforce)
library(dplyr)
# Create the forest plot with facets for each predictor and color by model type
voi_table <- table %>%
  filter(predictor=="Medication combination")

# Create the forest plot with facets for each predictor and color by model type
ggplot(data = voi_table, aes(x = Level, y = OR, color = model_type)) +
  geom_point(position = position_dodge(width = 0.35)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), position = position_dodge(width = 0.35), width = 0.2) +
  coord_flip() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey60") +
  scale_y_log10() +
  labs(col = "Model", x = "", y = "Incidence Rate Ratio") +
  theme_bw() + #get rid of grey background 
  theme(strip.background =element_rect(fill="white"),legend.position="bottom",
        axis.line.x.bottom = element_line(size = 0.2))+
  theme(strip.text = element_text(colour = 'black',face="bold")) +
  theme(axis.text.y = element_text(face = "bold",colour="black")) +
  #facet_col(~ predictor, scales = "free_y", space = "free",shrink=T) +
  scale_color_manual(values = c("Total hospitalisations" = "blue", "CVD-specific hospitalisations" = "red")) +
  labs(x="Medication combination") + 
  theme(legend.margin=margin(-10, 0, 0, 0)) #move legend label closer to the x-axis for less white space

