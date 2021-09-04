library(rsm)
library(DoE.base)

# Design

set.seed(237)

gold <- ccd(score ~ x1 * x2, n0 = 2, alpha = 1.5, 
            coding = list(x1 ~ (int_time-4)/2, x2 ~ (coadds-20)/10))

gold$score_per_sec <- NA
gold$total_time <- with(gold, (x1 * 2 + 4) * (x2 * 10 + 20))

# write.csv(gold, file="gold_results.csv")


set.seed(238)

silver <- ccd(score ~ x1 * x2, n0 = 2, alpha = 1.5, 
              coding = list(x1 ~ (int_time-4)/2, x2 ~ (coadds-20)/10))

silver$score_per_sec <- NA
silver$total_time <- with(silver, (x1 * 2 + 4) * (x2 * 10 + 20))

# write.csv(silver, file="silver_results.csv")



# Read filled out files

gold_results <- read.csv("gold_results.csv")

gold$score <- gold_results$score

gold$score_per_sec <- gold$score / gold$total_time

gold.rsm <- rsm(score ~ SO(x1,x2), na.omit(gold))

summary(gold.rsm)

contour(gold.rsm, ~ x1 + x2, image=TRUE)



silver_results <- read.csv("silver_results.csv")

silver$score <- silver_results$score

silver$score_per_sec <- silver$score / silver$total_time

silver.rsm <- rsm(score ~ SO(x1,x2), na.omit(silver))

summary(silver.rsm)

contour(silver.rsm, ~ x1 + x2, image=TRUE, xlabs = c("Integration Time [s]", "Coadditions"))




