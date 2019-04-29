library(tidyverse)

dat <- gss_cat

View(dat)

# Only 2014

marriage <- dat %>%
  filter(year == 2014) %>%
  filter(marital != "Never married") %>%
  na.omit()

colnames(marriage)
m <- data.frame(table(marriage$marital))
m <- m[order(-m$Freq),]
m$Var1 <- factor(m$Var1, levels = m$Var1[order(m$Freq)])
m

# Now for the barplot

m %>%
  ggplot(aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity")

# Mean for each level of marriage
marriage$age <- as.factor(marriage$age)

marriage %>%
  group_by(marital) %>%
  summarise(mean(age))

# And for the graph (5)
marriage$rincome <- as.factor(marriage$rincome)

marriage %>%
  mutate(rincome = fct_recode(rincome,
                              "Other" = "Not applicable",
                              "Other" = "Refused",
                              "Other" = "Don't know")) %>%
  ggplot(aes(x = age, y = rincome, group = marital)) +
  #facet_wrap(vars(marital)) +
  geom_point(aes(color = marital)) +
  labs(title = "Income by age across marital status")
