## lab 10 walkthrough 
library(here)
rope <- read.csv(here("data", "rope.csv"))
head(rope)
dim(rope)
str(rope)
sum(is.na(rope))
unique(rope$rope.type)
rope$rope.type <- as.factor(rope$rope.type)
str(rope)
levels(rope$rope.type)
n_obs <- nrow(rope)
n_obs
dim(rope)
n_groups <- nlevels(rope$rope.type)
n_groups
mean_cut <- mean(rope$p.cut)
mean_cut
sum_of_squares <- function(n)
{
  (sum((n - mean_cut)^2))
}
ss_tot <- sum_of_squares(rope$p.cut)
ss_tot
mean_cut

aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type), 
  FUN = mean)

agg_resids <-aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) x-mean(x))

str(agg_resids)

agg_sum_sq_resids <-aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) sum((x-mean(x))^2))

str(agg_sum_sq_resids)

ss_within <- sum(agg_sum_sq_resids$x)
ss_within

ss_among <- ss_tot - ss_within
ss_among

df_tot <- n_obs - 1
df_tot

df_within <- n_obs - n_groups
df_within

df_among <- n_groups - 1
df_among

ms_among <- ss_among/df_among
ms_within <- ss_within/df_within
ms_among
ms_within

f_ratio <- ms_among / ms_within
f_ratio

f_pval <- 1-pf(f_ratio, df1 = df_among, df_within)
f_pval

fit_1 = lm(p.cut ~ rope.type, data=rope)
anova(fit_1)

anova_fit_1 = anova(fit_1)
str(anova_fit_1)
anova_fit_1$"Sum Sq"

rope2 = droplevels(
  subset(
    rope,
    rope.type %in% c("PI", "VEL", "XTC"))
)

boxplot(
  p.cut ~ rope.type,
  data = rope2,
  las = 2,
  xlab = "",
  ylab = "Proportion Rope Cut",
  main = "Subset of Rope Data")
mtext("Rope Type", side = 1, line = 3)

fit_rope_2 = lm(p.cut ~ rope.type, data=rope2)
rope2_hsd = TukeyHSD(aov(fit_rope_2))
class(rope2_hsd)

round(rope2_hsd$rope.type, digits = 4)

# number comparison tolerance
digits_check = 5

# Build the reference model using R functions
fit_1 = lm(p.cut ~ rope.type, data=rope)
anova(fit_1)
anova_fit_1 = anova(fit_1)

# Check degrees of freedom
anova_fit_1$Df == c(df_among, df_within)

# Check sums of squares
round(anova_fit_1$`Sum Sq`, digits = digits_check) == round(c(ss_among, ss_within), digits = digits_check)

# Check mean squares
round(anova_fit_1$`Mean Sq`, digits = digits_check) == round(c(ms_among, ms_within), digits = digits_check)

# Check the F-ratio
round(anova_fit_1$`F value`[1], digits = digits_check) == round(f_ratio, digits = digits_check)

# Check the F test statistic p-value
round(anova_fit_1$`Pr(>F)`[1], digits = digits_check) == round(f_pval, digits = digits_check)


