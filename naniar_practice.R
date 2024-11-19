#### NANIAR Practice
## CS 201 Fall 2024
## Amber Camp

library(tidyverse)
library(naniar)
library(gtExtras)

view(airquality)
view(starwars)

#### explore missing data ####
miss_var_summary(airquality) # get a summary of missing data 

miss_var_summary(airquality) %>% 
  gt() %>% # everything from here on is just to make the table look nice
  gt_theme_guardian() %>% 
  tab_header(title = "Missingness of variables")

# plot
gg_miss_var(airquality) # basic plot

# table of observations with missing data
airquality %>% 
  filter(!complete.cases(.)) %>% # extracts only obsvs in which something is missing
  head(10) %>% # limits to first 10
  gt() %>% 
  gt_theme_guardian() %>% 
  tab_header(title = "Rows that contain missing values")

# the distribution of missing data
vis_miss(airquality)

# the relationship to one variable
airquality %>% 
  mutate(
    Missing_Ozone = factor(is.na(Ozone), 
                                levels = c("TRUE", "FALSE"),
                                labels = c("Missing","Not Missing"))) %>% 
  ggplot(aes(x = Wind, fill = Missing_Ozone)) +
  geom_histogram(position = "stack") +
  labs(title = "Distribution of Wind Speeds for Missing vs. Not Missing Data",
       subtitle = "Stacked Histogram",
       x = "Wind speed",
       y = "Ozone Observations",
       fill = "Missingness") +
  theme_bw()

# same thing, but with true & false swapped
airquality %>% 
  mutate(
    Missing_Ozone = factor(is.na(Ozone), 
                           levels = c("FALSE", "TRUE"),
                           labels = c("Not Missing","Missing"))) %>% 
  ggplot(aes(x = Wind, fill = Missing_Ozone)) +
  geom_histogram(position = "stack") +
  labs(title = "Distribution of Wind Speeds for Missing vs. Not Missing Data",
       subtitle = "Stacked Histogram",
       x = "Wind speed",
       y = "Ozone Observations",
       fill = "Missingness") +
  theme_bw()

# dot plot
# can look at, for example, are there more dots on one side vs. the other, are the dots bigger or smaller, distribution patterns? 
airquality %>% 
  select(Ozone, Solar.R, Wind, Temp) %>% 
  ggplot(aes(x = Wind,
             y = Temp, 
             size = Solar.R,
             color=is.na(Ozone))) +
  geom_point(alpha = 0.7) +
  facet_wrap(~is.na(Ozone)) +
  labs(title = "Missing Ozone Data by Wind and Temperature", 
       color = "Missing Ozone data",
       y = "Temperature",
       x = "Wind speed") +
  theme_bw()

#### dealing with missing data ####

# 1) drop missing values
# let's say we cared about mass & height. We wouldn't want to drop obsvs in pink, bc we'd lose valuable data
# we may want to drop row in blue
starwars %>% 
  select(name, mass, height, hair_color) %>% 
  head(150) %>% 
  gt() %>% 
  gt_theme_guardian() %>% 
  tab_header(title = "Starwards characters") %>% 
  gt_highlight_rows(rows = is.na(mass),
                    fill = "steelblue") %>% 
  gt_highlight_rows(rows = is.na(hair_color),
                    fill = "lightpink")

# code to remove specific rows
starwars %>% 
  select(name, mass, height, hair_color) %>% 
  drop_na(mass, height) %>% # if either mass or height has NA, will drop
  head(20) %>% 
  gt() %>% 
  gt_theme_guardian() %>% 
  tab_header(title = "Star Wars characters")

# 2) sometimes we may not want to drop NAs, but replace them instead
starwars %>% 
  select(name, hair_color, species) %>% 
  head(5) %>% 
  gt() %>% 
  gt_theme_guardian() %>% 
  tab_header(title = "Star Wars characters")

starwars %>% 
  select(name, hair_color, species) %>% 
  filter(species == "Droid") %>% 
  gt() %>% 
  gt_theme_guardian() %>% 
  tab_header(title = "Star Wars characters")
# some have none, some have NA
# all should say none (bc they have no hair), so let's replace the values

starwars %>% 
  select(name, hair_color, species) %>% 
  filter(species == "Droid") %>% 
  mutate(hair_color = case_when( # "whenever it's the case that..."
    is.na(hair_color) &
      species == "Droid" ~ "none", # ~ = then
    TRUE ~ hair_color)) %>% # whenever it is the case that hair_color is missing AND species = droid, then put in "none".
# otherwise, leave hair_color alone
  gt() %>% 
  gt_theme_guardian() %>% 
  tab_header(title = "Star Wars characters")

# 3) Imputation
# What is imputation??
starwars %>% 
  select(name, height) %>% 
  filter(is.na(height)) %>% 
  gt() %>% 
  gt_theme_guardian() %>% 
  tab_header(title = "Star Wars characters")

# replace with median
median_height_before <- median(starwars$height, na.rm = TRUE)

median_replace <- as.data.frame(starwars %>% 
  mutate(height = case_when(
    is.na(height) ~ median(starwars$height, na.rm = TRUE),
    TRUE ~ height)) %>% 
  select(name, height) %>% 
  arrange(name) %>% 
  gt() %>% 
  gt_theme_guardian() %>% 
  tab_header(title = "Star Wars characters") %>%
  gt_highlight_rows(rows = name %in% c("Arvel Crynyd", "Finn", "Rey", "Poe Dameron", "BB8", "Captain Phasma"), fill = "steelblue"))

summary(median_replace$height)
median_height_after <- median(median_replace$height)

# PAUSE: how can you check your work?


# Another way: replace with mean
mean_height_before <- mean(starwars$height, na.rm = TRUE)

mean_replace <- as.data.frame(starwars %>% 
                                  mutate(height = case_when(
                                    is.na(height) ~ mean(starwars$height, na.rm = TRUE),
                                    TRUE ~ height)) %>% 
                                  select(name, height) %>% 
                                  arrange(name) %>% 
                                  gt() %>% 
                                  gt_theme_guardian() %>% 
                                  tab_header(title = "Star Wars characters") %>%
                                  gt_highlight_rows(rows = name %in% c("Arvel Crynyd", "Finn", "Rey", "Poe Dameron", "BB8", "Captain Phasma"), fill = "steelblue"))

summary(mean_replace$height)
mean_height_after <- median(mean_replace$height)

median_replace$height <- as.numeric(median_replace$height)
mean_replace$height <- as.numeric(mean_replace$height)
summary(starwars$height)
summary(median_replace$height)
summary(mean_replace$height)


#### visualize missingness with scatterplots ####
ggplot(starwars, aes(x = mass, y = height)) +
  geom_miss_point() +
  theme_bw(base_size = 16) +
  labs(title = "Height by Mass",
       caption = paste0("Source: ggplot2::starwars"))

ggplot(starwars, aes(x = mass, y = height)) +
  geom_miss_point() +
  theme_bw(base_size = 16) +
  labs(title = "Height by Mass",
       caption = paste0("Source: ggplot2::starwars")) +
  facet_wrap(~gender)

## back to airquality
ggplot(airquality, aes(x = Ozone, y = Solar.R)) +
  geom_point()
# we get a warning about missing values. they are removed and they aren't plotted! how do we fix it?

# solution
ggplot(airquality, aes(x = Ozone, y = Solar.R)) +
  geom_miss_point()

# facet by month
p1 <-
  ggplot(data = airquality,
         aes(x = Ozone,
             y = Solar.R)) + 
  geom_miss_point() + 
  facet_wrap(~Month, ncol = 2) + 
  theme(legend.position = "bottom")

p1

## make your own scatterplot of missing data using data of your choice. see if you can facet by another variable



#### data structures ####
# https://www.rdocumentation.org/packages/naniar/versions/1.1.0
# The shadow matrix is the same dimension as the data, and consists of binary indicators of missingness of data values, where missing is represented as “NA”, and not missing is represented as “!NA”, and variable names are kept the same, with the added suffix “_NA” to the variables.

head(airquality, 10)
as_shadow(airquality)

# you can bind the shadow data to the data, to help you keep track of things. This is called "nabular"
bind_shadow(airquality) # these two do the same thing
nabular(airquality)

# using the nabular format helps you manage where missing values are in the df and make it easy to do viz where you split by missingness
airquality %>% 
  bind_shadow() %>% 
  ggplot(aes(x = Temp, fill = Ozone_NA)) +
  geom_density(alpha = 0.5)

# can also help you visualize imputations
airquality %>%
  bind_shadow() %>%
  as.data.frame() %>% 
  simputation::impute_lm(Ozone ~ Temp + Solar.R) %>%
  ggplot(aes(x = Solar.R,
             y = Ozone,
             colour = Ozone_NA)) + 
  geom_point()

# or perform an upset plot to plot of the combinations of missingness across cases
gg_miss_upset(airquality)

gg_miss_var(airquality)

gg_miss_span(airquality,
             var = Ozone,
             span_every = 10) # binwidth of your var

n_miss(airquality)

n_complete(airquality)

prop_miss(airquality)

prop_complete(airquality)

pct_miss(airquality)

pct_complete(airquality)

# numerical summaries
miss_var_summary(airquality)

miss_case_summary(airquality)

airquality %>%
  group_by(Month) %>%
  miss_var_summary() 


#### statistical tests of missingnesss ####

# MCAR = missing completely at random. Missing data is completely random, unrelated to both observed and missing values.
# MAR = missing at random. Missing data is related to observed values (e.g., age in a survey about income), but not to the missing values (e.g., income). 
# MNAR = missing not at random. Missing data is related to the missing values themselves (e.g., people with high income not reporting their income).

# null hyp in this test is that the data is MCAR, and the test statistic is a chi-squared value.
# given the high statistic value and low p, we can conclude that the data is not MCAR
# high p = do not reject H0 = data IS MCAR
# low p = do reject H0 = data is not MCAR

mcar_test(diamonds) # reject with high confidence
mcar_test(starwars) 















