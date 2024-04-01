
### R script for 'Public attitudes toward multilateral climate action'



# I. Getting started ------------------------------------------------------

### 1) Disable scientific notations
options(scipen = 999)


### 2) Install and/or load packages
pacman::p_load(tidyverse,
               haven, labelled, sjlabelled, # for working with labeled .sav data
               stargazer, ggeffects, dotwhisker, # for generating regression outputs
               gt, gtExtras) # for generating tables


### 3) Read the dataset

# Read in the Global Attitudes Survey
Global.df <- read_sav("./Dataset/Global Attitudes Survey (Spring 2021).sav")

# Read in the American Trends Panel
US.df <- read_sav("./Dataset/American Trends Panel (Wave 82).sav")


### 4) (For reference) Create a data dictionary
# A look up table for the original response categories
dictionary_global <- generate_dictionary(Global.df)
dictionary_US <- generate_dictionary(US.df)

# To retrieve the look up table:
# dictionary_US$value_labels or dictionary_global$value_labels




#  II. Data wrangling -----------------------------------------------------


# a. The Global Attitudes Survey ------------------------------------------



# 1) Clean column names & filter observations by country ------------------


# Remove 1) prefixes 'D_' and 2) suffixes '_2017'
names(Global.df) <- str_remove_all(names(Global.df), "^D_|_2017$")



Global.df <-
  Global.df %>%
  # Assign descriptive variable names
  rename("EDUC_AUSTRALIA_SUPPLEMENTARY" = "EDUC_AUSTRALIA",
         "EDUC_AUSTRALIA" = "EDUC_AUSTRALIA_2017B") %>%
  # Subset to respondents from
  # Australia, Germany, Netherlands, South Korea, and Sweden:
  filter(public == 1 | public == 5 | public == 9 | public == 12 | public == 14) %>%
  # Instead of displaying numeric values for each national sample,
  # assign their descriptive value labels, to indicate respondents' nationality (as strings):
  mutate(COUNTRY = get_labels(public)[public])




# 2) Standardize the education variable across countries ------------------


# Define a custom function for cleaning up the education-related variables for
# 1) the Dutch, 2) Korean, and 3) the Swede samples:

clean_response1 <- function(x) {
  
  cleaned_NA1 = ifelse(x %in% c(98, 99), NA, # Code 'Don't know' and 'Refused' as NA
                       ifelse(x %in% c(8, 9, 10), 1, # 'BA degrees or higher' as 1
                              # If the data pertains to the [FOCAL COUNTRY] AND isn't a missing value, code as 0 ('No BA degree')
                              # Otherwise (i.e. if the data doesn't actually pertain to the [FOCAL COUNTRY]), code as NA.
                              ifelse(!is.na(x), 0, NA)))
  return(cleaned_NA1)
}




Global_edu_cleaned <-
  Global.df %>%
  mutate(
    # For the Australian sample,
    # if the highest year of primary/secondary school completion is 'Don't know' or 'Refused',
    # code as NA:
    EDUC_AUSTRALIA_TEMPORARY = ifelse(EDUC_AUSTRALIA_SUPPLEMENTARY %in% c(8, 9), NA,
                                      # If the respondent is *NOT* from Australia, also code them as NA. Otherwise, code as 0:
                                      ifelse(COUNTRY != 'Australia', NA, 0)),
    # Then, overwrite'EDUC_AUSTRALIA_TEMPORARY' over our measure of 'highest degree obtained',
    # *ONLY IF* the latter is missing a value.
    # The overwritten values will either be an a) 'NA' or b) '0' (for 'Without a BA degree'):
    EDUC_AUSTRALIA_NEW = ifelse(is.na(EDUC_AUSTRALIA), EDUC_AUSTRALIA_TEMPORARY,
                                ifelse(EDUC_AUSTRALIA %in% c(98, 99), NA, # recode the 'Don't know's and 'Refused's
                                       ifelse(EDUC_AUSTRALIA %in% c(5, 6, 7), 1, 0))), # recode non-missing values as 0 or 1s
    # Next, clean the 'EDUC_GERMANY' column:
    EDUC_GERMANY_NEW = ifelse(EDUC_GERMANY %in% c(8, 9), NA, # recode the 'Don't know's and 'Refused's
                              ifelse(EDUC_GERMANY %in% c(5, 6, 7), 1, # recode the 'BA or higher's
                                     # For the remaining observations in this column, code as 0 if the data is from a German sample.
                                     # Otherwise, code as NA (because these observations are from non-German samples):
                                     ifelse(!is.na(EDUC_GERMANY), 0, NA))),
    # For 1) the Netherlands, 2) Korea, and 3) Sweden,
    # apply the customized function, then append '_NEW' to the original column names:
    across(c(EDUC_NETHERLANDS, EDUC_SKOREA, EDUC_SWEDEN), ~ clean_response1(.x), .names = "{col}_NEW")) %>%
  # Collapse the recoded responses of all 5 countries into a single variable:
  mutate(
    EDUCATION = ifelse(COUNTRY == 'Australia', EDUC_AUSTRALIA_NEW,
                       ifelse(COUNTRY == 'Germany', EDUC_GERMANY_NEW,
                              ifelse(COUNTRY == 'Netherlands', EDUC_NETHERLANDS_NEW,
                                     ifelse(COUNTRY == 'South Korea', EDUC_SKOREA_NEW, EDUC_SWEDEN_NEW)))))



# 3) Standardize the income variable across countries ---------------------


# First, code the 'Don't know's and 'Refused' as NA,
# because calculating the median income category requires the removal of NAs 
# To do so, define a custom function that converts the missing values of income-related variables:
clean_response2 <- function(x) {
  # keep the non-missing values as-is
  cleaned_NA2 = ifelse(x %in% c(98, 99), NA, x) 
  return(cleaned_NA2)
} 


# Apply the function across all 5 countries:
Global_edu_income_cleaning <-
  Global_edu_cleaned %>%
  mutate(
    across(starts_with("INCOME_") & ends_with(c("AUSTRALIA", "GERMANY", "NETHERLANDS", "SKOREA", "SWEDEN")),
           ~ clean_response2(.x)),
    # Collapse the responses of each country into a single variable:
    INCOME_UNSTANDARDIZED = ifelse(COUNTRY == 'Australia', INCOME_AUSTRALIA,
                                   ifelse(COUNTRY == 'Germany', INCOME_GERMANY,
                                          ifelse(COUNTRY == 'Netherlands', INCOME_NETHERLANDS,
                                                 ifelse(COUNTRY == 'South Korea', INCOME_SKOREA, INCOME_SWEDEN)))))


# Next, find the median income category for each country
Global_income_median_UNSTANDARDIZED <-
  Global_edu_income_cleaning %>% 
  group_by(COUNTRY) %>% 
  summarize(median_income = median(INCOME_UNSTANDARDIZED, na.rm = TRUE))



# Create a summary table of median income, for each national sample
Global_income_median_UNSTANDARDIZED  %>%
  gt() %>% 
  tab_header(
    title = "The median income category of survey respondents",
    subtitle = "By country:") %>% 
  tab_source_note(
    source_note = md("**<ins>Note</ins>**: The income categories presented above are *unstandardized*.")) %>% 
  # rename the columns
  cols_label(
    COUNTRY = "Country/Nationality",
    median_income = "Median income category") %>% 
  # align columns to the left
  cols_align(align = "left") %>%
  gt_theme_538()



# Lastly, standardize the income variables so that:
# values above or equal to the *country* median (i.e. High income) = 1 and
# values below the *country* median (i.e. Low income) = 0

Global_edu_income_cleaned <-
  Global_edu_income_cleaning %>% 
  mutate(
    INCOME = ifelse(is.na(INCOME_UNSTANDARDIZED), NA, # Code the missing values
                    # Specify the higher income category among the Australian & German sample:
                    ifelse((COUNTRY %in% c('Australia', 'Germany') & INCOME_UNSTANDARDIZED >= 6) |
                             # Higher income category among the Dutch sample:
                             (COUNTRY == 'Netherlands' & INCOME_UNSTANDARDIZED >= 5) | 
                             # Higher income category among the South Korean sample and the Swedes:
                             # Then code as 1 if high in income, otherwise code as 0
                             (COUNTRY %in% c('South Korea', 'Sweden') &  INCOME_UNSTANDARDIZED >= 8), 1, 0)))



# 4) Standardize the responses *across* the Global and the US data --------


# 1. To perform a logistic regression, convert the dependent variable (`CLIMATE_INTLCOMMUNITY`) into a binary variable
# Rename it as `CLIMATE_CONFIDENCE`
# 2. Collapse the scale responses and convert the independent variable (`DIVERSITY_GOODBAD`) into a binary variable
# Rename it as `DIVERSITY_GOOD`
# 3. Code `sex`/`gender` as `1` = female, `0` = male/non-female, instead of `1`s and `2`s
# 4. Collapse the scale responses and recode `political ideology` as `1` = left, `2` = moderate, `3` = right
# 5. Classify the respondents' `age` (straight age) into age categories



# Create a custom function that converts the missing values in
# our measure of 1) satisfaction with democracy and 2) climate concern:

clean_response3 <- function(x) {
  # Code 'Don't know' or 'Refused' as 'NA'. Else, keep it as is.
  cleaned_NA3 = ifelse(x %in% c(8, 9), NA, x)
  return(cleaned_NA3)
}



# Clean the entire dataset:

Global.df_FINAL <- Global_edu_income_cleaned %>%
  mutate(
    # Recode 'very confident' or 'somewhat confident' = 1
    CLIMATE_CONFIDENCE = ifelse(CLIMATE_INTLCOMMUNITY %in% c(1, 2), 1,
                                # 'not too confident' or 'not at all confident' = 0
                                ifelse(CLIMATE_INTLCOMMUNITY %in% c(3, 4), 0, NA)),
    DIVERSITY_GOOD = ifelse(DIVERSITY_GOODBAD %in% c(8, 9), NA,
                            # Recode 'diversity makes a better place to live' = 1
                            # 'diversity makes a worse place to live' or 'diversity doesn't make much difference' = 0
                            ifelse(DIVERSITY_GOODBAD == 1, 1, 0)), 
    # Apply the custom function to convert NAs
    across(c(SATISFIED_DEMOCRACY, CLIMATE_CONCERN), ~ clean_response3(.x)),
    # Convert sex/gender into dummy variables
    FEMALE = ifelse(SEX == 2, 1, 0),
    AGE_CATEGORY = case_when(
      AGE == 99 ~ NA_real_,
      (AGE >= 18 & AGE <= 29) ~ 1,
      (AGE >= 30 & AGE <= 49) ~ 2,
      (AGE >= 50 & AGE <= 64) ~ 3,
      AGE >= 65 ~ 4),
    POLITICAL_ID = ifelse(POLITICAL_SCALE2 %in% c(8, 9), NA,
                          # Code 'extreme left' ~ 'left leaning' as 1
                          ifelse(POLITICAL_SCALE2 %in% c(0, 1, 2), 1, 
                                 # Code 'right leaning' ~ 'extreme right' as 3; and 'center' as 2
                                 ifelse(POLITICAL_SCALE2 %in% c(4, 5, 6), 3, 2)))) %>%
  select(
    COUNTRY,
    CLIMATE_CONFIDENCE, # Dependent variable
    SATISFIED_DEMOCRACY, DIVERSITY_GOOD, # Independent variables
    CLIMATE_CONCERN, FEMALE, AGE_CATEGORY, POLITICAL_ID, EDUCATION, INCOME) # Controls


# Export the cleaned dataset
write.csv(Global.df_FINAL, file = "./Dataset/Global Attitudes Survey_CLEANED.csv")




# b. The American Trends Panel --------------------------------------------



# 1) Clean column names ---------------------------------------------------

# Remove 1) prefixes 'GAP21' and 2) suffixes '_W82' from the column names
names(US.df) <- str_remove_all(names(US.df), "^GAP21|_W82$")




# 2) Standardize the responses *across* the Global and the US data --------


US.df_FINAL <-
  US.df %>%
  mutate(
    # Change all 'Refused's to NA
    across(c(starts_with("Q"), starts_with("F_")), ~ ifelse(.x == 99, NA, .x)),
    # Create a new variable that indicates the nationality of the sample
    COUNTRY = 'US',
    # Convert to binary variables:
    CLIMATE_CONFIDENCE = case_when(
      Q28 %in% c(1,2) ~ 1, # Code as "confident"
      Q28 %in% c(3,4) ~ 0, # as "not confident"
      TRUE ~ Q28),
    # 'Diversity makes it a worse place to live' = 0; Otherwise, keep the values as-is
    DIVERSITY_GOOD = ifelse(Q20 == 2, 0, Q20),
    FEMALE = ifelse(is.na(F_GENDER), NA,
                    ifelse(F_GENDER == 2, 1, 0)), # female = 1; non-female = 0
    EDUCATION = ifelse(F_EDUCCAT %in% c(2,3), 0, F_EDUCCAT), # No BA = 0; Otherwise, keep as-is
    # Distinguish between high- vs. low-income           
    INCOME = ifelse(is.na(F_INC_SDT1), NA,
                    ifelse(F_INC_SDT1 >= median(F_INC_SDT1, na.rm = TRUE), 1, 0)),
    POLITICAL_ID = case_when(
      F_IDEO %in% c(4,5) ~ 1, # Code 'Liberal' and 'Very liberal' as 'Left'
      F_IDEO == 3        ~ 2, # 'Moderate' as 'Center'
      F_IDEO %in% c(1,2) ~ 3, # 'Very conservative' and 'Conservative' as 'Right'
      TRUE ~ F_IDEO)) %>%
  rename(SATISFIED_DEMOCRACY = Q3, # Assign descriptive variable names
         CLIMATE_CONCERN     = Q24,
         AGE_CATEGORY        = F_AGECAT) %>% 
  select(COUNTRY,
         CLIMATE_CONFIDENCE, # Dependent variable
         SATISFIED_DEMOCRACY, DIVERSITY_GOOD, # Independent variables
         CLIMATE_CONCERN, FEMALE, AGE_CATEGORY, POLITICAL_ID, EDUCATION, INCOME) # Controls


# Export the cleaned dataset
write.csv(US.df_FINAL, file = "./Dataset/American Trends Panel_CLEANED.csv")




# c. Combine the two data sets --------------------------------------------

# Merge the Global and the US survey data sets
COMBINED_df <- rbind(Global.df_FINAL, US.df_FINAL)

# Export the compiled version of the dataset
write.csv(COMBINED_df, file = "./Dataset/Climate Attitudes Survey_FINAL.csv")

# Split the data frame according to each country
# (In order to fit 6 different regression models)
national_sample <- split(COMBINED_df, f = COMBINED_df$COUNTRY)  





# III. Data analysis ------------------------------------------------------


# 1) Print the summary statistics -----------------------------------------

# Summary statistics of the entire sample (across 6 countries)
summary(COMBINED_df)


# Summary statistics of each national sample
countries <- c("Australia", "South Korea", "Germany", "Netherlands", "Sweden", "US")


country_summary <- function(data, countries) {
  summaries <- map(countries, ~ summary(data[[.x]]))
  names(summaries) <- countries
  return(summaries)
}

country_summary(national_sample, countries)





# 2) Create 2 contingency tables for each country, ------------------------
# one for attitudes toward diversity (X1) and the other for satisfaction with democracy (X2)
# For examining the distribution of 0s and 1s of Y, across varying levels of X


variables <- c("SATISFIED_DEMOCRACY", "DIVERSITY_GOOD")

generate_cross_tabs <- function(country, X) {
  xtabs(~ CLIMATE_CONFIDENCE + get(X),
        data = national_sample[[country]])
}


for (country in countries) {
  cat("Country:", country, "\n")
  
  # Generate cross-tabulations
  
  # For attitudes toward diversity (X1):
  cat("For DIVERSITY_GOOD:\n")
  print(map(country, ~generate_cross_tabs(.x, "DIVERSITY_GOOD")))
  
  # For satisfaction with democracy (X2):
  cat("For SATISFIED_DEMOCRACY:\n")
  print(map(country, ~generate_cross_tabs(.x, "SATISFIED_DEMOCRACY")))
}





# 3) Fit a multiple logistic regression model -----------------------------



# Define the equation for the logistic regression model
logit_equation <-
  CLIMATE_CONFIDENCE ~ # Y variable
  DIVERSITY_GOOD + SATISFIED_DEMOCRACY + # X variables
  CLIMATE_CONCERN + FEMALE + AGE_CATEGORY + EDUCATION + POLITICAL_ID + INCOME # Control variables




# For each country, fit the model
models <- list()

for (country in countries) {
  
  data <- national_sample[[country]]
  
  glm_model <- glm(
    logit_equation,
    data = data,
    family = binomial("logit"))
  
  models[[country]] <- glm_model
  
}




# 4) Visualize the results of the regression analysis ---------------------



# Create a regression table -----------------------------------------------

regression_table <- stargazer(
  models,
  title = "The effect of diversity and democracy perceptions on perceived efficacy of multilateral climate action",
  column.labels = toupper(countries),
  align = TRUE,
  type = "text")



# Plot the coefficients and their 95% CI ----------------------------------


# Plot the log odds
dwplot(models) %>% 
  relabel_predictors(
    c(DIVERSITY_GOOD = "DIVERSITY IS \n BETTER",
      SATISFIED_DEMOCRACY = "SATISFACTION \n WITH DEMOCRACY \n AT HOME",
      CLIMATE_CONCERN = "CONCERNED ABOUT \n CLIMATE CHANGE",
      FEMALE = "FEMALE",
      AGE_CATEGORY = "AGE",
      EDUCATION = "EDUCATION",
      POLITICAL_ID = "POLITICAL \n IDEOLOGY",
      INCOME = "INCOME")) +
  theme_gray() +
  geom_vline(xintercept = 0, linetype = 2) + # Add a dotted line at x = 0 
  labs(
    title = "Who tends to be more optimistic of \n tackling climate action at the international level?",
    subtitle = "Positive attitudes toward social diversity and low satisfaction with the functioning of democracy
    within one's country are good predictors of one's confidence in multilateral solutions",
    x = "Coefficient Estimates \n (in log odds)",
    caption = "Source: The Pew Research Center. \n Note: Bars represent 95% Confidence Intervals.") +
  scale_color_discrete(name = "Countries",
                       labels = countries) +
  theme(plot.title =  element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1, size = 9),
        text = element_text(size = 10.5))




# 5) Calculate the predicted probabilities --------------------------------

# For each country, compute the predicted probability that an individual would be
# optimistic of the impacts of global climate action, across different levels of
# a) diversity perception and b) satisfaction with democracy

# Hold other (independent and control) variables constant at the median
# In order to draw comparisons across multiple countries,
# set the reference point to the global median (i.e. the median of all 6 countries),
# NOT the national median






# Create a list of median values, which we will pass as the parameters of ggpredict()
# Set the reference to the *global* median:
median_values <- c(
  SATISFIED_DEMOCRACY = 2,
  DIVERSITY_GOOD = 1,
  CLIMATE_CONCERN = 2,
  FEMALE = 0,
  AGE_CATEGORY = 3,
  POLITICAL_ID = 2,
  EDUCATION = 0,
  INCOME = 1)







# Store the predicted probabilities for a given model and term
generate_predicted_probs <- function(model, X_var) {
  ggpredict(
    model,
    terms = NULL,
    condition = median_values,
    pretty = TRUE)
}




# 6) Plot the predicted probabilities -------------------------------------



predicted_probs_list <- map(models, ~ generate_predicted_probs(.x))



# Predicted probabilities for diversity perceptions -----------------------


# 1. Create a predicted probabilities table that compares across individuals' social diversity beliefs
(predicted_probs_DIVERSITY <-
   predicted_probs_list %>%
   # Return it as a dataframe
   map_dfr(~ .x$DIVERSITY_GOOD, .id = "Country") %>%
   # Give a descriptive label to the 'x' column, which indicates different levels of social diversity beliefs:
   rename(DIVERSITY_GOOD = x) %>% 
   
   # 2. Plot the predicted probabilities and confidence intervals   
   ggplot(aes(
     x = factor(DIVERSITY_GOOD),
     y = predicted,
     color = Country)) +
   geom_point() +
   geom_errorbar(aes(
     ymax = conf.high, ymin = conf.low),
     alpha = 0.25,
     width = 0.5,
     color = "black") +
   scale_y_continuous(
     limits = c(0, 1),
     n.breaks = 6) +
   scale_color_discrete(
     breaks = c(0:5),
     labels = countries) +
   labs(
     title = "The predicted probabilities of the level of confidence \n in our ability to tackle global climate change via multilateralism", 
     x = "Diversity makes the world a better place", 
     y = "Predicted probability", 
     subtitle = "A cross-country comparison along diversity perceptions", 
     caption = "Source: The Pew Research Center. \n Note: Bars represent 95% Confidence Intervals.
       The graph shows the predicted probabilities while holding all other values constant at their median.") +
   theme(
     plot.title =  element_text(hjust = 0.5, face = "bold"), 
     plot.subtitle = element_text(hjust = 0.5),
     plot.caption = element_text(hjust = 1, size = 9),
     text = element_text(size = 10.5)) +
   facet_wrap(~ Country, nrow = 1))




# Predicted probabilities for democracy perceptions -----------------------



# 1. Create a predicted probabilities table that compares across different levels of satisfaction with democracy
(predicted_probs_DEMOCRACY <-
   predicted_probs_list %>%
   # Return it as a dataframe
   map_dfr(~ .x$SATISFIED_DEMOCRACY, .id = "Country") %>%
   # Give a descriptive label to the 'x' column, which refers to different levels of satisfaction with democracy:
   rename(SATISFIED_DEMOCRACY = x) %>% 
   
   
   # 2. Plot the predicted probabilities and confidence intervals  
   ggplot(aes(
     x = SATISFIED_DEMOCRACY,
     y = predicted,
     linetype = Country)) +
   geom_line() +
   geom_ribbon(aes(
     ymax = conf.high, ymin = conf.low),
     alpha = 0.25) +
   scale_y_continuous(
     limits = c(0, 1),
     n.breaks = 6) +
   scale_linetype_discrete(
     breaks = c(0:5),
     labels = countries) +
   labs(
     title = "The predicted probabilities of the level of confidence \n in our ability to tackle global climate change via multilateralism", 
     x = "Satisfaction with the way democracy functions at home", 
     y = "Predicted Probability", 
     subtitle = "A cross-country comparison along democracy perceptions", 
     caption = "Source: The Pew Research Center. \n Note: Bars represent 95% Confidence Intervals. \n The graph shows the predicted probabilities while holding all other values constant at their median.") +
   theme(
     plot.title =  element_text(hjust = 0.5, face = "bold"), 
     plot.subtitle = element_text(hjust = 0.5),
     plot.caption = element_text(hjust = 1, size = 9),
     text = element_text(size = 10.5)) +
   facet_wrap(~ Country, nrow = 1))








