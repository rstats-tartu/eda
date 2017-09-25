
library(tidyverse)

## let's have look at the file
read_file("data/PO0211.csv")
## extract data
pop <- read_delim("data/PO0211.csv",
                  "\t", 
                  escape_double = FALSE, 
                  trim_ws = TRUE,
                  skip = 2)

## extract label
label_pop <- read_delim("data/PO0211.csv", "\t", n_max = 1, col_names = F) %>% unlist

colnames(pop)[2] <- c("Year")
vars <- which(!is.na(pop$X1))[1:2]
Sex <- rep(pop$X1[vars], each = diff(vars))
pop1 <- pop[1:length(Sex), ]

pop1$Sex <- Sex 
pop1 <- select(pop1, Sex, Year, matches("^[[:digit:]]")) %>% 
  filter(complete.cases(.))

## summarise 0 and 1-4 to 0-4 age group
pop1$`0-4` <- pop1$`0` + pop1$`1-4`
pop_long <- select(pop1, -`0`, -`1-4`) %>% 
  gather("age", "ave_pop", matches("^[[:digit:]]"))

## We need to recode also variable Sex
pop_long <- mutate(pop_long, Sex = if_else(Sex == "Males", "Men", "Women"))
pop_long$age %>% unique()
cases_long$age %>% unique()



## calculate incidence per 100'000
left_join(select(cases_long, -`Specified site`, -Site_old), pop_long) %>% 
  group_by(Site, Year, Sex) %>% 
  summarise(cases = sum(cases),
            ave_pop = sum(ave_pop)) %>% 
  mutate(incidence = cases/(ave_pop/1E5)) %>% 
  arrange(desc(incidence))

