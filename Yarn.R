library(tidyverse)
library(tidytuesdayR)
theme_set(theme_minimal())

tuesday <- tidytuesdayR::tt_load(2022, week = 41)
yarn <- tuesday$yarn

# How are NAs distributed?

mean(is.na(yarn$max_gauge))

yarn_na_counts <- yarn %>% summarize(across(everything(), 
                                       \(x) mean(is.na(x))))
View(yarn_na_counts)

# Big idea: can we impute missing NA values for
# min_gauge using grams and yardage?

# some weirdness:
summary(yarn$yardage)
weird <- filter(yarn, yardage == 0)#
View(weird)

# Let's replace Kmart Sayelle's yardage with 240

yarn[yarn$permalink == "kmart-sayelle", "yardage"] <- 240

# There are also zeros for grams.
# For brevity, we are going to just filter these

yarn_sm <- yarn %>%
  filter(grams > 0,
         yardage > 0) %>% 
  mutate(dens = grams / yardage,
         gauge_std = min_gauge / gauge_divisor)

View(yarn_sm)

# How do dens and gauge_std relate in this smaller set?

ggplot(yarn_sm, aes(x = log10(dens),
                    y = log10(gauge_std))) +
  geom_point(alpha = .1)

# Let's remove the zeros from gauge_std as well.
yarn_sm <- yarn_sm %>% filter(gauge_std > 0)
ggplot(yarn_sm, aes(x = log10(dens),
                    y = log10(gauge_std))) +
  geom_point(alpha = .1) +
  geom_smooth(method = "lm")

model <- lm(log10(gauge_std) ~ log10(dens),
            data = yarn_sm, na.action = na.pass)
summary(model)

# The model says that:
# log10(gauge_std) = .495 - .514 * log10(dens) + err
# which is equivalent to: gauge_std = 10 ^ (.495 - .514 * log10(dens) + err)

yarn_new <- yarn %>% 
  mutate(gauge_std_imputed = 10^(.495 - .514*log10(grams / yardage)))
# Note there are some problematic values here. Zeros, infinities and NAs. We should deal with those

ggplot(yarn_new, aes(x = log10(min_gauge / gauge_divisor), y = log10(gauge_std_imputed))) +
  geom_point()

  
  