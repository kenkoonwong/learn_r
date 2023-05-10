# R Installation instructions ----
## 1. Install R https://cran.r-project.org/ 
## 2. Install Rstudio https://posit.co/download/rstudio-desktop/ 

# Getting to know Rstudio -----
## Tools > Global options ->
##    1. Set working directory
##.   2. Set Appearances
## Ctrl+enter OR cmd+enter -> run the line of code
## Ctrl+shift+enter OR cmd+shift+enter -> run entire script
## Ctrl+shift+C OR cmd+shift+c -> make code as comment (hence won't run)
## tab -> load a library of available commands
## Tools -> Keyboard shortcut help

## Install packages ----
install.packages("tidyverse")





# LET'S DO IT !!! ---- 
## Load library
library(tidyverse)

## Load dataset
df <- read_csv("https://raw.githubusercontent.com/kenkoonwong/learn_r/main/id_score_card.csv")



## Probability distributions -----
runif(n = 10000, min = 0, max = 100) %>% hist()

rbinom(n = 10000, size = 100, prob = 0.5) %>% hist()

rnorm(n = 10000, mean = 50, sd = 10) %>% hist()

rt(n = 10000, df = 30) %>% hist()

### quiz: Simulate random generation of poisson distribution with these parameters, n = 100, lambda = 5




## Simulate fake data -----
{
set.seed(290)
  
touches <- rbinom(n = nrow(df), size = 20, prob = 0.5)

specialty <- sample(x = c("comedy","fiction","non-finction","creativity","rock-star","influencer","literature","physics"), size = nrow(df), replace = T)

region <- sample(x = c("main","east","west","south"), size = nrow(df), replace = T, prob = c("0.6","0.1","0.1","0.2"))

pets <- runif(n = nrow(df), min = 0, max = 5) %>% round()

moneymoneymoney <- rnorm(n = nrow(df), mean = 1000000, sd = 100000)

crypto_investment <- rnorm(n = nrow(df), mean = 401000, sd = 10000)

carpool <- sample(c("yes","no"), size = nrow(df), replace = T)

space_travel <- sample(c("yes","no"), size = nrow(df), replace = T, prob = c(0.7,0.3))

solar_panel <- space_travel
}


df1 <- 
df %>%
  add_column(touches=touches, specialty=specialty, region=region, pets=pets, salary=moneymoneymoney, crypto_investment=crypto_investment,carpool=carpool, space_travel, solar_panel=solar_panel)






## explore -----
df1 %>% glimpse()

## select, filter, group_by, mutate, summarize, mean, sd
df1 %>%
  select(touches, salary)

df1 %>%
  filter(salary >= 1000000) %>% view()

df1 %>%
  filter(specialty == "comedy") 

df1 %>%
  group_by(specialty,region) %>%
  summarize(mean = mean(salary),
            sd = sd(salary)) %>%
  arrange(desc(mean))

### quiz: 
#### From df1, select columns from name to salary and assign it to a new dataframe called df_new


#### From df_new, filter touches that are more than 12


#### What is the mean pet numbers for specialty "comedy" ?
  








## DataViz -----
df1 %>%
  ggplot(.,aes(x=name,y=touches)) +
  geom_col() +
  coord_flip()

df1 %>%
  ggplot(.,aes(x=carpool,y=crypto_investment,fill=carpool)) +
  geom_boxplot(alpha=0.5) +
  geom_jitter(alpha=0.5) +
  facet_wrap(.~specialty, ncol = 2) +
  theme_bw() +
  theme(legend.position = "none") +
  ggtitle("Crypto-investment faceted by ID Specialty")
  
### quiz: Visualize the salary on the y-axis with space_travel on the x-axis with boxplot; Faceted by number of pets





 

## chisquare -----
df1 %>% 
  select(carpool, space_travel) %>%
  table() %>%
  chisq.test(correct = F)

prop.test(df1 %>% 
            select(carpool, space_travel) %>%
            table(), correct = F)

prop.test(matrix(c(9,5,17,13),nrow = 2), correct = F)  
 

## t-test
### Is there a difference in crypto investment between people who carpooled?
carpool_yes <- df1 %>%
  filter(carpool == "yes") %>%
  pull(crypto_investment)
carpool_no <- df1 %>%
  filter(carpool == "no") %>%
  pull(crypto_investment)

t.test(x = carpool_yes,y = carpool_no)


### quiz: 
### Perform a chi-square test on space_travel and solar_panel



### Perform a t-test on salary and solar_panel 






## linear regression -----
model <- lm(salary ~ touches, data = df1)

summary(model)

predict(model, newdata = tibble(touches = 50))
predict(model, newdata = tibble(touches = 0))


### quiz:
### Conduct a linear regression analysis where the salary is the response or dependent variable, and the crypto_investment is the predictor or independent variable
 




## logistic regression
model <- glm(as.factor(carpool) ~ space_travel + pets + salary, data = df1, family = "binomial")

summary(model)

predict(model, newdata = tibble(space_travel = "yes", pets = 5, salary = 1000000))
predict(model, newdata = tibble(space_travel = "yes", pets = 5, salary = 1000000), type = "response")

### quiz:
### Conduct a multivariate logistic regression analysis where solar_panel is the dependent variable and salary is the independent variable, while controlling for specialty and region
 


