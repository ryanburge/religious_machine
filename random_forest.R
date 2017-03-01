install.packages("rattle")
install.packages("rpart.plot")
install.packages("RColorBrewer")
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(car)
library(dplyr)


gss_reltrad_2000_2014 <- read_dta("D:/gss_reltrad_2000_2014.dta")
gss$male <- recode(gss$sex, "1=1; else=0")
gss$white <- recode(gss$race, "1=1; else=0")
gss$coldeg <- recode(gss$educ, "16:20 =1; else=0")
gss$male <- recode(gss$sex, "1=1; else=0")
gss$white <- recode(gss$race, "1=1; else=0")
gss$coldeg <- recode(gss$educ, "16:20 =1; else=0")
gss$prayschool <- recode(gss$prayer, "1=1; else=0")
gss$literal <- recode(gss$bible, "1=1; else=0")
gss$repubid <- recode(gss$partyid, "7=3")



df <- select(gss, evangelical, abany, attend, prayschool, literal, male, white, coldeg, repubid, childs, age)

agefit <- rpart(age ~ + evangelical + abany + attend + prayschool + literal + male + white + repubid + childs, data=df[!is.na(df$age),], method = "anova")
df$age[is.na(df$age)] <- predict(agefit, df[is.na(df$age),])

attendfit <- rpart(attend ~ + evangelical + abany + age + prayschool + literal + male + white + repubid + childs, data=df[!is.na(df$attend),], method = "anova")
df$attend[is.na(df$attend)] <- predict(attendfit, df[is.na(df$attend),])

repubfit <- rpart(repubid ~ + evangelical + abany + age + prayschool + literal + male + white + attend + childs, data=df[!is.na(df$repubid),], method = "anova")
df$repubid[is.na(df$repubid)] <- predict(repubfit, df[is.na(df$repubid),])


smp_size <- floor(0.75 * nrow(df))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train <- df[train_ind, ]
test <- df[-train_ind, ]

tree <- rpart(evangelical ~., train, method = "class")
fancyRpartPlot(tree)


fit <- randomForest(as.factor(evangelical) ~ attend + prayschool + literal + male + white +
                      coldeg + repubid +  age,
                    data=train, 
                    importance=TRUE, 
                    ntree=100)


pred <- predict(fit, test, type = "class")
table(pred, test$evangelical)
conf <- table(test$evangelical, pred)
sum(diag(conf)) / sum(conf)
