################ Estimation of the gender wage gap in Chile (2022) ######################

library(haven)
library(dplyr)
library(summarytools)
library(ggplot2)
library(stargazer)
library(gmodels)
library(stargazer)
library(mice)


#Load data set
CASEN_2022 <- read_dta("~/Downloads/CASEN 2022.dta")
View(CASEN_2022)


### ========== Question 1: Create a sample of active individuals according to ILO) =========== ###


# Convert labelled variables to numeric
sapply(CASEN_2022[, c("o1", "o3", "o5", "o6")], class)

CASEN_2022 <- CASEN_2022 %>%
  mutate(
    o1 = as.integer(o1),
    o3 = as.integer(o3),
    o5 = as.integer(o5),
    o6 = as.integer(o6),
    edad = as.integer(edad)
  )

# Create active sample
activeindividuals <- CASEN_2022 %>%
  filter(
    (o1 == 1 | o3 == 1) |          # Employed conditions
      (o1 == 2 & o5 == 1 & o6 == 1) # Unemployed conditions
  ) %>%
  filter(edad >= 15) # Exclude individuals under 15 years old

table(activeindividuals$sexo)



### =========== Question 2 and 3: Descriptive statistics between genders ============ ###


#### Employment Variables


## Comparing labor income

descriptive_stats_wage <- activeindividuals %>%
  group_by(sexo) %>%
  summarise(
    mean_income = mean(yoprcor, na.rm = TRUE),
    median_income = median(yoprcor, na.rm = TRUE),
    min_wage = min(yoprcor, na.rm = TRUE),
    max_wage = max(yoprcor, na.rm = TRUE),
    count = n()
  )
    
print(descriptive_stats_wage)
#Comment -> Males have on average a higher wage


## Comparing Hourly Wage
activeindividuals <- activeindividuals %>%
  mutate(hourly_wage = ifelse(y1 >= 0 & y2_hrs > 0, y1 / y2_hrs, 0))  # Set hourly_wage to 0 if y2_hrs = 0

descriptive_stats_hourly_wage <- activeindividuals %>%
  group_by(sexo) %>%
  summarise(
    mean_hourly_wage = mean(hourly_wage, na.rm = TRUE),
    median_hourly_wage = median(hourly_wage, na.rm = TRUE),
    min_hourly_wage = min(hourly_wage, na.rm = TRUE),
    max_hourly_wage = max(hourly_wage, na.rm = TRUE),
    count = n()
  )

print(descriptive_stats_hourly_wage)
#Comment -> Males have on average a higher hourly wage


## Comparing part time or full time
activeindividuals <- activeindividuals %>%
  mutate(
    o20 = ifelse(o20 >= 0 & o20 <= 3, o20, NA),
    o20 = case_when(
          o20 == 1 ~ 2,    # Change 3 to take the meaning of 1
          o20 == 2 ~ 1,    # Keep 2 as it is
          o20 == 3 ~ 3,    # Change 1 to take the meaning of 3
          TRUE ~ NA_real_  # Set unexpected values to NA
        )
      )
    
tapply(activeindividuals$o20, activeindividuals$sexo, summary)

descriptive_stats_time_contract <- activeindividuals %>%
  group_by(o20, sexo) %>%
  summarise(
    count = n(),  # Count observations for each gender and `o20` value
    .groups = "drop"
  ) %>%
  group_by(o20) %>%
  mutate(
    proportion = count / sum(count) * 100  # Calculate proportion for each gender
  )

print(descriptive_stats_time_contract)
#Comment -> Women represent 2/3 of the part time workers, and 73,1% of workers doing long working hours are men. Men are also more represented among full time workers.


## Comparing Overtime
activeindividuals <- activeindividuals %>%
  mutate(
    overtime = case_when(
      y3a_preg == 1 ~ 1,  
      y3a_preg == 2 ~ 0,  
      TRUE ~ NA_real_     
    )
  )

descriptive_stats_overtime <- activeindividuals %>%
  group_by(sexo) %>%
  summarise(
    total = n(),
    overtime_yes = sum(overtime == 1, na.rm = TRUE),
    overtime_no = sum(overtime == 0, na.rm = TRUE),
    proportion_overtime = mean(overtime, na.rm = TRUE)  
  )

print(descriptive_stats_overtime)
#Comment -> Men tend to do more overtime

activeindividuals <- activeindividuals %>%
  mutate(y3a = ifelse(y3a >= 0, y3a, NA))
tapply(activeindividuals$y3a, activeindividuals$sexo, summary)
#Comment -> Men on average earn more from overtime work


## Comparing number of hours worked
activeindividuals <- activeindividuals %>%
  mutate(o10 = ifelse(o10 >= 0, o10, NA))

descriptive_stats_hours <- activeindividuals %>%
  group_by(sexo) %>%
  summarise(
    mean_hours = mean(o10, na.rm = TRUE),
    median_hours = median(o10, na.rm = TRUE),
    sd_hours = sd(o10, na.rm = TRUE),
    min_hours = min(o10, na.rm = TRUE),
    max_hours = max(o10, na.rm = TRUE),
    n = n()
  )

print(descriptive_stats_hours)
#Comment -> Men tend to work more hours than women 


## Comparing contract
activeindividuals <- activeindividuals %>%
  mutate(
    o19 = ifelse(o19 >= 0, o19, NA),
    contract_dummy = case_when(
      o19 %in% c(2, 3) ~ 0,
      o19 == 1 ~ 1,
      TRUE ~ NA_real_
    )
  )

table(activeindividuals$contract_dummy)
tapply(activeindividuals$contract_dummy,activeindividuals$sexo, summary)
#Comment -> Women tend slightly to have more informal jobs


## Comparing commuting time

# Cleaning and combining commuting variables
activeindividuals <- activeindividuals %>%
  # Replace invalid values (-8, -88) with NA
  mutate(
    o28a_hr = ifelse(o28a_hr >= 0, o28a_hr, NA),    # Hours per trip
    o28a_min = ifelse(o28a_min >= 0, o28a_min, NA), # Minutes per trip
    o28b = ifelse(o28b >= 1, o28b, NA)             # Trips per week
  ) %>%
  # Convert commute time to total minutes per trip (round trip adjustment)
  mutate(
    commute_time_per_trip = ((o28a_hr * 60) + o28a_min) * 2, # Total minutes per round trip, x2 because also counting way back home
    total_commute_minutes_per_week = commute_time_per_trip * o28b, # Total weekly minutes
    total_commute_hours_per_week = total_commute_minutes_per_week / 60 # Total weekly hours
  ) %>%
  # Classify commuting time
  mutate(
    commute_category = case_when(
      total_commute_hours_per_week < 5 ~ "Short Commute",
      total_commute_hours_per_week >= 5 & total_commute_hours_per_week < 10 ~ "Moderate Commute",
      total_commute_hours_per_week >= 10 & total_commute_hours_per_week < 15 ~ "Long Commute",
      total_commute_hours_per_week >= 15 ~ "Very Long Commute",
      TRUE ~ "Unknown" 
    )
  )

# Ensure commute_category and gender are appropriately categorized
activeindividuals <- activeindividuals %>%
  mutate(
    sexo = factor(sexo, levels = c(1, 2), labels = c("Men", "Women")), 
    commute_category = factor(
      commute_category,
      levels = c("Short Commute", "Moderate Commute", "Long Commute", "Very Long Commute", "Unknown")
    ) 
  )

# Crosstab Gender Commuting Time
CrossTable(activeindividuals$commute_category, activeindividuals$sexo, prop.chisq = FALSE, prop.r = TRUE, prop.c = TRUE, prop.t = FALSE)
#Comment -> Men are disproportionately represented in longer commuting categories, suggesting they are more likely to accept jobs requiring longer travel times, whereas women are more likely to fall into shorter commutes.


## Computing mean wage per branch of activity
activeindividuals <- activeindividuals %>%
  mutate(rama1 = ifelse(rama1 >= 0, rama1, NA))

mean_wage_per_branch <- activeindividuals %>%
  group_by(rama1, sexo) %>%  # Group by branch and gender
  summarise(
    mean_wage = mean(yoprcor, na.rm = TRUE),  # Calculate mean wage
    count = n(),                          # Count individuals in each group
    .groups = "drop"
  )

print(mean_wage_per_branch, n=50)

# Compute overall mean wage and proportion of women per branch
branch_analysis <- activeindividuals %>%
  group_by(rama1) %>%
  summarise(
    overall_mean_wage = mean(yoprcor, na.rm = TRUE),           # Mean wage per branch
    total_count = n(),                                        # Total individuals per branch
    women_count = sum(sexo == "Women", na.rm = TRUE),         # Number of women in each branch
    women_proportion = women_count / total_count              # Proportion of women
  ) %>%
  ungroup() %>%  # Remove grouping to ensure rama1 is accessible
  mutate(
    branch1_label = case_when(
      rama1 == 2 ~ "Mining",
      rama1 == 7 ~ "Wholesale and retail trade",
      rama1 == 9 ~ "Food service",
      rama1 == 16 ~ "Teaching",
      rama1 == 20 ~ "Households activities",
      rama1 == 21  ~ "Extraterritorial bodies",
      TRUE ~ NA_character_  # Leave other branches unlabeled
    )
  )

print(branch_analysis, n=50)


# Scatterplot
ggplot(branch_analysis, aes(x = overall_mean_wage, y = women_proportion)) +
  geom_point(size = 3, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  geom_text(
    aes(label = branch1_label), 
    vjust = -1, size = 3, color = "black", na.rm = TRUE
  ) +
  labs(
    title = "Proportion of Women and Mean Wage by Branch",
    x = "Overall Mean Wage (Branch)",
    y = "Proportion of Women"
  ) +
  theme_minimal()
# Comment -> Women tend to be less represented in branches with higher mean wages


## Social security affiliation
activeindividuals <- activeindividuals %>%
  mutate(
    o31 = ifelse(o31 >= 0, o31, NA))

table(activeindividuals$o31)
tapply(activeindividuals$o31, activeindividuals$sexo, summary)
#Comment -> Women are slightly more affiliated to social security


#### Education Variables


## Comparing literacy 
activeindividuals <- activeindividuals %>%
  mutate(
    literacy_dummy = case_when(
      e1 == 1 ~ 1,                  # Can read and write
      e1 %in% c(2, 3, 4) ~ 0,       # Cannot read and write
      TRUE ~ NA_real_               # Handle missing or unexpected values
    )
  )

tapply(activeindividuals$literacy_dummy, activeindividuals$sexo, summary)
#Comment -> Women on average are more literate


## Comparison of level of education attained

# Descriptive statistics for education by gender
descriptive_stats_education <- activeindividuals %>%
  group_by(sexo) %>%
  summarise(
    mean_education = mean(e6a, na.rm = TRUE),
    median_education = median(e6a, na.rm = TRUE),
    n = n()
  )

print(descriptive_stats_education)
# Comment -> Women tend to be more educated

# Recode `e6a` into meaningful categories
education_gender_percentage <- activeindividuals %>%
  mutate(
    e6a_recode = case_when(
      e6a == 1 ~ "No Formal Education",                # Never attended
      e6a %in% c(2, 3, 4) ~ "Early Education",         # Nursery, Kindergarten, Pre-Kindergarten
      e6a == 5 ~ "Special Education",                 # Special Education
      e6a %in% c(6, 7) ~ "Primary Education",         # Primary or Basic Education
      e6a %in% c(8, 9) ~ "Secondary Education",       # Humanities, Scientific Secondary Education
      e6a %in% c(10, 11) ~ "Vocational Education",    # Technical or Vocational Education
      e6a == 12 ~ "Higher Technician",               # Higher Level Technician
      e6a == 13 ~ "Professional Degree",             # Professional Careers (4+ years)
      e6a == 14 ~ "Master's Degree",                 # Master's Degree
      e6a == 15 ~ "PhD",                             # PhD
      TRUE ~ NA_character_                           # Handle unexpected or missing values
    )
  ) 

education_gender_percentage <- (activeindividuals$e6a)

# Re-create the dataset from activeindividuals
education_gender_percentage <- activeindividuals %>%
  group_by(e6a, sexo) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(e6a) %>%
  mutate(percentage = (count / sum(count)) * 100)


# Check the distribution
print(education_gender_percentage, n=25)

# Ensure `e6a` exists and is numeric for recoding
education_gender_percentage <- activeindividuals %>%
  mutate(
    e6a_numeric = case_when(
      e6a == 1 ~ 1,    # No Formal Education
      e6a %in% c(2, 3, 4) ~ 2,    # Early Education
      e6a == 5 ~ 3,    # Special Education
      e6a %in% c(6, 7) ~ 4,    # Primary Education
      e6a %in% c(8, 9) ~ 5,    # Secondary Education
      e6a %in% c(10, 11) ~ 6,    # Vocational Education
      e6a == 12 ~ 7,    # Higher Technician
      e6a == 13 ~ 8,    # Professional Degree
      e6a == 14 ~ 9,    # Master's Degree
      e6a == 15 ~ 10,   # PhD
      TRUE ~ NA_real_   # Handle unexpected or missing values
    )
  )

# Density plot
ggplot(education_gender_percentage, aes(x = e6a_numeric, color = sexo)) +
  geom_density(aes(group = sexo), size = 1.2, adjust = 5) +  
  scale_x_continuous(
    breaks = 1:10,  # Numeric values for education levels
    labels = c(
      "No Formal Education", "Early Education", "Special Education", 
      "Primary Education", "Secondary Education", "Vocational Education", 
      "Higher Technician", "Professional Degree", "Master's Degree", "PhD"
    )
  ) +
  labs(
    title = "Density Plot of Education Levels by Gender (Lines)",
    x = "Education Level",
    y = "Density",
    color = "Gender"
  ) +
  scale_color_manual(values = c("Men" = "blue", "Women" = "pink")) +  # Assign distinct colors
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )
#Comment -> Women are underrepresented compared to men in lower education levels, and same or overrepresented for higher education levels


## Comparing field of education

field_gender_percentage <- activeindividuals %>%
  group_by(cinef13_area, sexo) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(cinef13_area) %>%
  mutate(percentage = (count / sum(count)) * 100)

field_gender_percentage <- field_gender_percentage %>%
  mutate(cinef13_area = ifelse(cinef13_area <= 10, cinef13_area, NA)) 

field_gender_percentage <- field_gender_percentage %>%
  mutate(
    cinef13_area_label = case_when(
      cinef13_area == 1 ~ "Health and Wellbeing",
      cinef13_area == 2 ~ "Engineering, Industry and Construction",
      cinef13_area == 3 ~ "Education",
      cinef13_area == 4 ~ "Services",
      cinef13_area == 5 ~ "Business Administration and Law",
      cinef13_area == 6 ~ "Social Sciences, Journalism and Information",
      cinef13_area == 7 ~ "Natural Sciences, Mathematics and Statistics",
      cinef13_area == 8 ~ "Agriculture, Forestry, Fisheries and Veterinary Medicine",
      cinef13_area == 9 ~ "Information and Communication Technology (ICT)",
      cinef13_area == 10 ~ "Arts and Humanities",
      TRUE ~ NA_character_
    )
  )

field_gender_percentage <- field_gender_percentage %>%
  filter(!is.na(cinef13_area)) #remove NA

field_gender_percentage <- field_gender_percentage %>%
  mutate(
    cinef13_area_label = factor(
      cinef13_area,
      levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
      labels = c("Health and Wellbeing", "Engineering, Industry and Construction", "Education", "Services", "Business and Law", "Social Science and Journalism", "Natural Science, Maths and Stats", "Agriculture", "IT", "Arts and Humanities")
    ),
    sexo = factor(sexo) 
  )

# Plot with cinef13_area included
ggplot(field_gender_percentage, aes(x = reorder(cinef13_area_label, cinef13_area), y = percentage, fill = sexo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Percentage of Gender by Field of Study and Education Level",
    x = "Field of Study",
    y = "Percentage",
    fill = "Gender"
  ) +
  scale_fill_manual(values = c("Men" = "blue", "Women" = "pink")) +  # Set custom colors
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Comment -> Very low proportion of women in Engineering and IT branches, more inclined to be in Health and Education.


## Had to interrupt studies because of external reasons
activeindividuals <- activeindividuals %>%
  mutate(
    e5a = ifelse(e5a >= 0, e5a, NA),
    precarity_educ_dummy = case_when(
      e5a %in% c(7, 8, 9, 10) ~ 1,   # No
      e5a %in% c(1, 2, 3, 4, 5, 6, 11, 12, 13, 14, 15) ~ 0,   # Yes
      TRUE ~ NA_real_
    ))

descriptive_stats_education <- activeindividuals %>%
  group_by(sexo) %>%
  summarise(
    mean_education = mean(e6a, na.rm = TRUE),
    median_education = median(e6a, na.rm = TRUE),
    n = n()
  )

table(activeindividuals$precarity_educ_dummy)
tapply(activeindividuals$precarity_educ_dummy, activeindividuals$sexo, summary)
# Comment -> Women were more incline to interrupt their studies because of external reasons


### Health 

## Disabilities
activeindividuals <- activeindividuals %>%
  mutate(disc_wg = ifelse(disc_wg >= 0, disc_wg, NA))
table(activeindividuals$disc_wg)
tapply(activeindividuals$disc_wg, activeindividuals$sexo, summary)


## Number medical checks last 12 months
activeindividuals <- activeindividuals %>%
  mutate(s26a = ifelse(s26a >= 0, s26a, NA))
         
tapply(activeindividuals$s26a, activeindividuals$sexo, summary)
# Comment -> Women had been through more than double hospital checks


## Number of mental checks
activeindividuals <- activeindividuals %>%
  mutate(
    s22a = ifelse(s22a >= 0, s22a, NA))

table(activeindividuals$s22a)
tapply(activeindividuals$s22a, activeindividuals$sexo, summary)
#Comment -> Women had on average more mental checks in the past 12 months


## Number of emergency interventions
activeindividuals <- activeindividuals %>%
  mutate(
    s21a = ifelse(s21a >= 0, s21a, NA))
table(activeindividuals$s21a)
tapply(activeindividuals$s21a, activeindividuals$sexo, summary)
#Comment -> Women had on average more emergency interventions in the past 12 months


## Has been hospitalized last 12 months
activeindividuals <- activeindividuals %>%
  mutate(
    s27a = ifelse(s27a >= 0, s27a, NA),
    hospitalization_dummy = case_when(
    s27a == 9 ~ 0,   #No
    s27a %in% c(1, 2, 3, 4, 5, 6, 7, 8) ~ 1,   #Yes
    TRUE ~ NA_real_
    ))

table(activeindividuals$hospitalization_dummy)
tapply(activeindividuals$hospitalization_dummy, activeindividuals$sexo, summary)
#Comment -> Women were on average more hospitalized in the last 12 months


## Did not have consultation for his illness / accident
activeindividuals <- activeindividuals %>%
  mutate(
    s17 = ifelse(s17 >= 0, s17, NA),  
    no_medical_care_dummy = case_when(
      s17 == 2 ~ 0,     #No         
      s17 == 1 ~ 1,      #Yes           
      TRUE ~ NA_real_                
    )
  )

tapply(activeindividuals$no_medical_care_dummy, activeindividuals$sexo, summary)
#Comment -> Men tend to have less consultations


## Because of problems getting to surgery or hospital
activeindividuals <- activeindividuals %>%
  mutate(
    ifelse(s19a >= 0, s19a, NA),
    getting_to_hospital_dummy = case_when(
      s19a == 2 ~ 0,                  # No
      s19a == 1 ~ 1,       # Yes
      TRUE ~ NA_real_             
    )
  )

table(activeindividuals$getting_to_hospital_dummy)
tapply(activeindividuals$getting_to_hospital_dummy, activeindividuals$sexo, summary)


## Problems getting an appointment/attention
activeindividuals <- activeindividuals %>%
  mutate(
    ifelse(s19b >= 0, s19b, NA),
    problem_appointment_dummy = case_when(
      s19b == 2 ~ 0,                  # No
      s19b == 1 ~ 1,       # Yes
      TRUE ~ NA_real_             
    )
  )

tapply(activeindividuals$problem_appointment_dummy, activeindividuals$sexo, summary)
#Comment -> Women suffered more of issues getting an appointment


## Problems being attended to in the establishment
activeindividuals <- activeindividuals %>%
  mutate(
    ifelse(s19c >= 0, s19c, NA),
    problem_get_to_hospital_dummy = case_when(
      s19c == 2 ~ 0,                  # No
      s19c == 1 ~ 1,       # Yes
      TRUE ~ NA_real_             
    )
  )

tapply(activeindividuals$problem_get_to_hospital_dummy, activeindividuals$sexo, summary)


## Problems due to cost for care due to cost
activeindividuals <- activeindividuals %>%
  mutate(
    s19d = ifelse(s19d >= 0, s19d, NA),  # Ensure valid values for s19d
    s19e = ifelse(s19e >= 0, s19e, NA),  # Ensure valid values for s19e
    problem_care_cost_dummy = case_when(
      s19d == 2 & s19e == 2 ~ 0,         # No problem with care cost
      s19d == 1 | s19e == 1 ~ 1,         # Problem with care cost
      TRUE ~ NA_real_                    # Handle missing or unexpected values
    )
  )

table(activeindividuals$problem_care_cost_dummy)
tapply(activeindividuals$problem_care_cost_dummy, activeindividuals$sexo, summary)
#Comment -> Women had less access to care aid because of costs 


## Under medical treatment in the last 12 months
activeindividuals <- activeindividuals %>%
  mutate(
    s28 = ifelse(s28 >= 0, s28, NA),  
    medical_treatment_dummy = case_when(
      s28 == 22 ~ 0,  
      s28 %in% c(1:21) ~ 1,  
      TRUE ~ NA_real_  
    )
  )

tapply(activeindividuals$medical_treatment_dummy, activeindividuals$sexo, summary)
#Comment -> Women tend more to have been under medical treatment


## Having some long term condition
activeindividuals <- activeindividuals %>%
  mutate(
    permanent_health_condition = case_when(
      s31_7 == 1 ~ 0,  
      s31_7 == 0 ~ 1,  
      TRUE ~ NA_real_  
    )
  )

tapply(activeindividuals$permanent_health_condition, activeindividuals$sexo, summary)
#Comment -> Women tend more to suffer from a long term health condition


### =========================== Question 4: PCA =================================== ###


## Creation composite variables
work_composite <- activeindividuals[, c(
  "o10",     #number of hours worked
  "contract_dummy", 
  "total_commute_hours_per_week",  
  "o31"     #affiliated to social security
)]

health_composite <- activeindividuals[, c(
  "disc_wg",
  "problem_care_cost_dummy", 
  "problem_get_to_hospital_dummy", 
  "no_medical_care_dummy", 
  "problem_appointment_dummy", 
  "getting_to_hospital_dummy", 
  "hospitalization_dummy", 
  "s26a",            #number of health check ups last 12 months
  "permanent_health_condition",
  "medical_treatment_dummy"
)]

education_composite <- activeindividuals[, c(
  "e6a",
  "literacy_dummy",
  "precarity_educ_dummy"
)]

table(activeindividuals$precarity_educ_dummy)

## Centrer et réduire les données
work_composite_norm <- as.data.frame(scale(work_composite, center = TRUE, scale = TRUE))
education_composite_norm <- as.data.frame(scale(education_composite, center = TRUE, scale = TRUE))
health_composite_norm <- as.data.frame(scale(health_composite, center = TRUE, scale = TRUE))


## Use PCA 
library(FactoMineR)
library(factoextra)

out.pca_health <- PCA(health_composite_norm, ncp = 6, graph = FALSE)
get_eigenvalue(out.pca_health)

out.pca_education <- PCA(education_composite_norm, ncp = 6, graph = FALSE)
get_eigenvalue(out.pca_education)

out.pca_work <- PCA(work_composite_norm, ncp = 6, graph = FALSE)
get_eigenvalue(out.pca_work)

#Using Kaiser method: keeping dimensions with eigenvalues >1

#These 6 axes represent 53.68044% of the explained variance
fviz_eig(out.pca_health, addlabels = TRUE) #we keep first six dimensions
fviz_eig(out.pca_education, addlabels = TRUE) #we keep first six dimensions
fviz_eig(out.pca_work, addlabels = TRUE) #we keep first six dimensions


#Synthetic variable
x <- out.pca_health$ind$coord[, 1:3]
print(x)

y <- out.pca_education$ind$coord[, 1:1]
print(y)

z <- out.pca_work$ind$coord[, 1:2]
print(z)

syn_var_x = (out.pca_health$eig[1,2]/100)*out.pca_health$ind$coord[,1] + (out.pca_health$eig[2,2]/100)*out.pca_health$ind$coord[,2] + (out.pca_health$eig[3,2]/100)*out.pca_health$ind$coord[,3] 
print(syn_var_x)

syn_var_y = (out.pca_education$eig[1,2]/100)*out.pca_education$ind$coord[,1]
print(syn_var_y)

syn_var_z = (out.pca_work$eig[1,2]/100)*out.pca_work$ind$coord[,1] + (out.pca_work$eig[2,2]/100)*out.pca_work$ind$coord[,2] 
print(syn_var_z)

activeindividuals$work_composite <- syn_var_z
activeindividuals$education_composite <- syn_var_y
activeindividuals$health_composite <- syn_var_x

summary(activeindividuals$education_composite)


### ===================== Question 5: Use clustering methods ========================= ###

activeindividuals$log_income <- log(activeindividuals$ytrabajocor + 0,5)

activeindividuals <- activeindividuals %>%
  mutate(
    sexo_dummy = case_when(
      sexo == "Men" ~ 0,                 
      sexo == "Women" ~ 1,       
      TRUE ~ NA_real_             
    )
  )

clustering_data <- activeindividuals[, c("log_income", "education_composite", "work_composite", "health_composite", "edad", "sexo_dummy")]

clustering_data <- na.omit(clustering_data)

clustering_data <- as.data.frame(scale(clustering_data))

summary(clustering_data)


## Elbow Method
set.seed(123)

elb_wss <- rep(0,times=10)
for (k in 1:10) {
  clus <- kmeans(clustering_data, centers = k)
  elb_wss[k] <- clus$tot.
}

plot(1:10, elb_wss, type = "b", xlab = "Nb of clusters", ylab = "WSS")

#We will keep 3 clusters
set.seed(123)
kmeans_result <- kmeans(clustering_data, centers = 3, nstart = 25)

#Cluster Plot
fviz_cluster(kmeans_result, data = clustering_data) +
  labs(title = "K-means Clustering with 3 Clusters")


## Clustering quality
library(cluster)

# Sample the data
set.seed(123)
sample_indices <- sample(1:nrow(clustering_data), size = 20000)  # random sample size
sample_data <- clustering_data[sample_indices, ]

# Compute silhouette scores and plot it
silhouette_scores <- silhouette(kmeans_result$cluster[sample_indices], dist(sample_data))
plot(silhouette_scores, border = NA, main = "Silhouette Plot for 3 Clusters (Sampled Data)")

#Cluster stats
clustering_data$cluster <- kmeans_result$cluster
cluster_summary_stats <- clustering_data %>%
  group_by(cluster) %>%
  summarize(across(everything(), mean, na.rm = TRUE))
print(cluster_summary_stats)




### ========================= Question 6: Tobit Model ============================= ###

library(sampleSelection)

# Define the "working" variable based on o1 and o3
activeindividuals$working <- ifelse(activeindividuals$o1 == 1 | activeindividuals$o3 == 1, 1, 0)
table(activeindividuals$working)

# We exclude + 60 years individuals
activeindividuals <- activeindividuals %>%
  filter(edad <= 60)

# Tobit II model
tobit2_model <- selection(
  selection = working ~ sexo_dummy + edad + e6a,   # Selection equation
  outcome = log_income ~ sexo_dummy + edad + e6a + health_composite + o10 + rama1 + total_commute_hours_per_week + area + contract_dummy + o20,  # Outcome equation
  data = activeindividuals
)

#results
summary(tobit2_model)



### =================== Question 7: Using the Oaxaca Blinder method =================== ###

library(oaxaca)

oaxaca_data <- activeindividuals[, c("overtime","area","contract_dummy", "o20","working", "rama1", "health_composite", "work_composite", "education_composite","sexo_dummy", "edad", "e6a", "yoprcor")]

oaxaca_data <- oaxaca_data %>%
  mutate(across(c(work_composite, education_composite, health_composite), scale))

# There are NAs
summary(oaxaca_data)


# Check the proportion of missing values by gender
oaxaca_data %>%
  group_by(sexo_dummy) %>%
  summarize(
    total = n(),
    missing_income = sum(is.na(yoprcor)),
    proportion_missing = mean(is.na(yoprcor))
  )
oaxaca_data %>%
  group_by(sexo_dummy) %>%
  summarize(
    total = n(),
    missing_rama1 = sum(is.na(rama1)),
    proportion_missing = mean(is.na(rama1))
  )
#Comment -> removing rows with NA could bias results, as women are overrepresented among NA values

# Inputting income = 0 for NA
oaxaca_data <- oaxaca_data %>%
  mutate(yoprcor = ifelse(is.na(yoprcor), 0, yoprcor))

summary(oaxaca_data)

# Convert specific composite matrix columns into vectors
oaxaca_data$health_composite <- as.vector(oaxaca_data$health_composite)
oaxaca_data$work_composite <- as.vector(oaxaca_data$work_composite)
oaxaca_data$education_composite <- as.vector(oaxaca_data$education_composite)


# Inputting missing values for the rest of the variables
imputed_data <- mice(oaxaca_data, method = "pmm", m = 3)
summary(imputed_data)
oaxaca_data <- complete(imputed_data)

summary(oaxaca_data)
print(oaxaca_data)

# Oaxaca Decomposition
OB1 <- oaxaca(
  yoprcor ~ overtime + area + contract_dummy + o20 + working + rama1 + health_composite +
        edad + e6a | sexo_dummy,
  data = oaxaca_data
)

plot(OB1, components = c("endowments","coefficients"))

table(oaxaca_data$sexo_dummy)
table(activeindividuals$sexo)

# the wage gap
OB1$y

OB1$twofold$overall

# Decomposing the unexplained part following Neumark
plot(OB1, 
     decomposition = "twofold", 
     group.weight = -1,  
     unexplained.split = TRUE, 
     components = c("unexplained A", "unexplained B"), 
     component.labels = c("unexplained A" = "In Favor of Men", 
                          "unexplained B" = "Against Women"),
     variables = c("edad", "e6a", "working"), 
     variable.labels = c("edad" = "Years of Age", 
                         "e6a" = "Education Level Attained", 
                         "working" = "Working Status"))




