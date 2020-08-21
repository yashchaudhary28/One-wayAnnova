#===========================================================================================================================#
#
#                                      One-way Annova testing using Poison Data
#                                               By - Arshan and Yash
#
#===========================================================================================================================#

##############################################################################
### 
### let us state the hypothesis for the test
###
###   Null Hypothesis,      H0: The means between groups are identical.
###   Alternate Hypothesis, Ha: At least, the mean of one group is different.
###


#importing required libraries
library(dplyr)
library(ggplot2)

#Importing data
PATH <- "https://raw.githubusercontent.com/guru99-edu/R-Programming/master/poisons.csv"
df <- read.csv(PATH) %>%
    select(-X) %>% 
    mutate(poison = factor(poison, ordered = TRUE))
glimpse(df)

# Step - 1 ==> Level of poison.
levels(df$poison)

# Step - 2 ==> Compute mean and standard deviation.
df %>%
    group_by(poison) %>%
    summarise(
        count_poison = n(),
        mean_time = mean(time, na.rm = TRUE),
        sd_time = sd(time, na.rm = TRUE)
    )

# Step 3 ==> Graphically check if there is a difference between the distribution.
ggplot(df, aes(x = poison, y = time, fill = poison)) +
    geom_boxplot() +
    geom_jitter(shape = 15,
                color = "steelblue",
                position = position_jitter(0.21)) +
    theme_classic()

# Step 4 ==> Run the one-way ANOVA test with the command aov.
anova_one_way <- aov(time~poison, data = df)
summary(anova_one_way)

#The p-value is lower than the usual threshold of 0.05. So we reject H0
#You are confident to say there is a statistical difference between the groups