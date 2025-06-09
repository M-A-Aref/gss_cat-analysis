library(tidyverse)
library(gganimate)
# Factors "Categorical Variables"

library(forcats)
?forcats
data()
view(gss_cat)
glimpse(gss_cat)
unique(gss_cat$race)
gss_cat %>% pull(race) %>% 
  unique()
count(gss_cat, race, sort = T)
gss_cat %>% count(race)
gss_cat %>% pull(race) %>% levels()
gss_cat %>% select(race) %>% table()

gss_cat %>% mutate(race=fct_drop(race)) %>% pull(race) %>% levels()

# Order according to another numeric value
gss_cat %>% 
  drop_na(tvhours) %>% 
  group_by(relig) %>% 
  summarise(average_tv = mean(tvhours)) %>%
  ggplot(aes(average_tv, relig))+
  geom_point(size=5)

gss_cat %>% 
  drop_na(age) %>% 
  filter(rincome != 'Not applicable') %>% 
  group_by(rincome) %>% 
  summarise(average_age = mean(age)) %>% 
  mutate(rincome = fct_rev(rincome)) %>% 
  ggplot(aes(average_age, rincome))+
  geom_point(size=5)


# Order by frequency of the variable occurance
gss_cat %>%
  ggplot(aes(marital))+
  geom_bar()

gss_cat %>% 
  mutate(marital= fct_infreq(marital)) %>% 
  mutate(marital= fct_rev(marital)) %>% 
  ggplot(aes(marital))+
  geom_bar(fill= 'steelblue', alpha=0.5)+
  theme_minimal()

# Recoding factors
gss_cat %>% 
  mutate(partyid= fct_recode(partyid, 
                             'Republican, Strong' = 'Strong republican',
                             'Republican, Weak' = 'Not str republican',
                             'Independent, Near Rep' = 'Ind,near rep',
                             'Independent, Near Dem' = 'Ind,near dem',
                             'Democrat, Strong'= 'Strong democrat',
                             'Democrat, Weak'= 'Not str democrat',
                             'Other' = 'No answer',
                             'Other'='Don\'t know',
                             'Other'='Other party')) %>% 
  count(partyid)
# Collapsing factors
gss_cat %>% 
  mutate(partyid= fct_collapse(partyid,
                               Other = c('No answer', 'Don\'t know', 'Other party'),
                               Rep = c('Strong republican', 'Not str republican', 'Ind,near rep'),
                               Dem = c('Strong democrat', 'Not str democrat', 'Ind,near dem'),
                               Ind = 'Independent')) %>% 
  count(partyid)
# Lumping factors 
gss_cat %>% 
  count(relig, sort = T)
gss_cat %>% 
  mutate(relig=fct_lump(relig, n=2)) %>% count(relig)
# Reordering factors

gss_cat %>% filter(!is.na(age)) %>% 
  filter(marital%in%c("Never married", "Married", "Widowed")) %>% count(age, marital) %>% 
  group_by(age) %>% mutate(prop = n/sum(n)) %>% 
  mutate(marital= fct_reorder2(marital, age, prop)) %>% 
  mutate(marital=fct_rev(marital)) %>% 
  ggplot(aes(age, prop, colour = marital))+ geom_line(size=1, arrow=arrow(type = 'closed', ends = 'last', length = unit(.3, 'cm'), angle = 30), na.rm = T)+
  theme_classic()+
  gganimate::transition_reveal(age)
