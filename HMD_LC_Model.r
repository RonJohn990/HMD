rm(list = ls())


# Function to pull needed data from mortality.org -------------------
# Can be specified with country adn type of data
HMD_data <- function(coun = NULL, file = NULL){
  data <- system("HMD_webscraping/hmd_webscraping.sh", intern = TRUE, input = c(coun, file))
  data <- data[3:length(data)]
  
  # Loop tp parse each row of deaths_data - Works
  tmp_df <- data.frame()
  vec <- c()
  vec_tmp <- c()
  count_no <- 1
  
  for (count_no in 1:length(data)) {
    tmp_1 <- data[count_no]
    tmp_1 <- strsplit(x = tmp_1, split = " ")
    tmp_1 <- c(tmp_1[[1]])
    
    vec_tmp <- c()
    
    for (i in tmp_1) {
      if (i != "") {
        tmp <- c(i)    
        vec_tmp = append(vec_tmp, tmp)
        # vec_tmp <- tmp
      }
    }
    tmp_df <- rbind(tmp_df, vec_tmp)
    count_no <- count_no + 1
  }
  
  col_names <- tmp_df[1,]
  tmp_df <- tmp_df[2:dim(tmp_df)[1], ]
  colnames(tmp_df) <- col_names 
  rownames(tmp_df) <- NULL
  
  # Performing some data Preprocessing
  tmp_df['Year'] <- as.numeric(unlist(tmp_df['Year']))  
  tmp_df['Age'] <- rapply(tmp_df['Age'], function(x) ifelse(x == "110+", "110", x))
  tmp_df['Age'] <- as.numeric(unlist(tmp_df['Age']))  
  tmp_df['Female'] <- as.numeric(unlist(tmp_df['Female']))
  tmp_df['Male'] <- as.numeric(unlist(tmp_df['Male']))
  tmp_df['Total'] <- as.numeric(unlist(tmp_df['Total']))
  
  return(tmp_df)
}

# AUS FIN LVA SVN AUT FRA LTU ESP BLR DEU LUX SWE
# BEL GRC NLD CHE BGR HKG NZL TWN CAN HUN NOR GBR_NP
# CHL ISL POL USA HRV IRL PRT UKR CZE ISR KOR DNK
# ITA RUS EST JPN SVK
coun <- "JPN"

# "Deaths_1x1.txt" "Exposures_1x1.txt"
# file <- "Deaths_1x1.txt"

df_deaths <- HMD_data(coun = coun, "Deaths_1x1.txt")
head(df_deaths)
str(df_deaths)


df_exposures <- HMD_data(coun, "Exposures_1x1.txt")
str(df_exposures)
head(df_exposures)



# Expolring the Data -----------------
library(tidyverse)

# Exploring Deaths data
## Plot 1
tmp_df <- df_deaths %>%
            filter(Year >= 1950 & Year <= 2019)
ggplot(data = tmp_df, aes(x = Age, y = Female, color = as.factor(Year))) +
  geom_point(alpha = 0.6)
            

## Plot 2
tmp_df <- df_deaths %>%
            filter(Age == 50) %>%
            select(Year, Male, Female) %>%
            gather(key = 'Sex', value = 'Deaths', -Year)

ggplot(data = tmp_df, aes(x = Year, y = Deaths, color = Sex)) +
  geom_line()

# ggplot(data = tmp_df, aes(x = Year, y = Female)) +
#   geom_line(aes(y = Male), color = "darkred", alpha = 0.6) +
#   geom_line(aes(y = Female), color = "steelblue", alpha = 0.6)


## Plot 3
tmp_df <- df_deaths %>%
              filter(Year > 1946) %>%
              filter(Age >= 0 & Age <= 100) %>%
              mutate(male_to_female_ratio = Male/Female)
              # select(Year, Age, male_to_female_ratio)

tmp_df <- na.omit(tmp_df)
tmp_df <- tmp_df[!is.infinite(rowSums(tmp_df)),]

ggplot(data = tmp_df, aes(x = Age, y = male_to_female_ratio, color = as.factor(Year)))+
  geom_point(alpha = 0.6)


#Exploring Exposures Data
## Plot 4
tmp_df <- df_exposures %>%
            filter(Year > 1946) %>%
            filter(Age >= 0 & Age <= 100) %>%
            mutate(male_to_female_ratio = Male/Female)
            
tmp_df <- na.omit(tmp_df)
tmp_df <- tmp_df[!is.infinite(rowSums(tmp_df)),]

ggplot(data = tmp_df, aes(x = Age, y = male_to_female_ratio, color = as.factor(Year)))+
  geom_point(alpha = 0.6)


