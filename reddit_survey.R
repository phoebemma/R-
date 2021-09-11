
reddit_survey1 <-read.csv("reddit.csv")
#checking several parameters in data

dim(reddit_survey1)
#32754 columns and 12 rows

#visualing based on income range
library(ggplot2)
table(reddit_survey1$income.range)
reddit_survey1$income.range <-ordered(reddit_survey1$income.range, levels = c("Under $20,000", "$20,000 - $29,999","$30,000 - $39,999 ","$40,000 - $49,999" , "$50,000 - $69,999", "$70,000 - $99,999","$100,000 - $149,999 ","$150,000 or more"    ))
qplot(data = reddit_survey1, x = income.range )+
  scale_x_discrete(1:7)

table(reddit_survey1$employment.status)
qplot(data = reddit_survey1, x = employment.status)+
  scale_x_discrete(1:6)
