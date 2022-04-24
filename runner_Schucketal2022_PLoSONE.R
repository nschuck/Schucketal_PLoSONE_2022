################################################
## SETUP: load packages, settings and data
################################################

## load packages
packages = c('MASS', 'car', 'sfsmisc', 'gdata', 'lme4', 'emmeans', 'pwr')
pksload = unlist(lapply(packages, require, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))
if(sum(pksload) != length(packages)) {
	warning('SOME PACKAGES NOT LOADED')
}

#load some custom functions
source('aux_functions.R')
# load data
load('taskdata_Schucketal_2022_PLoSONE.RData')
# settings for lmer
lcctrl = lmerControl(optimizer=c('bobyqa'), optCtrl = list(maxfun = 500000))


################################################
## ANALYSES EXPERIMENT 1: Standard trials
################################################

# ERRORS
clme = lmer(E ~ BLOCK*GROUP + (1+BLOCK|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
Anova(clme)

# Mean difference between age groups
clme = lmer(E ~ GROUP + (1|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

# Difference between age groups last two blocks
clme = lmer(E ~ GROUP + (1|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V1' & regular.cdf$BLOCK > 6), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

# RTs
clme = lmer(RT ~ BLOCK*GROUP + (1+BLOCK|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
Anova(clme)

# Mean difference between age groups
clme = lmer(RT ~ GROUP + (1|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

# Difference between age groups last two blocks
clme = lmer(RT ~ GROUP + (1|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V1' & regular.cdf$BLOCK > 6), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")


# EFFECT OF INSTRUCTION BLOCK
# ERRORS: test for interaction in pre/post color instruction
clme = lmer(E ~ GROUP*COND + (1 + COND|ID), data = subset(regular.cdf, regular.cdf$BLOCK > 6 & regular.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ COND | GROUP), adjust = "tukey")

# RTs: test for interaction in pre/post color instruction
clme = lmer(RT ~ GROUP*COND + (1 + COND|ID), data = subset(regular.cdf, regular.cdf$BLOCK > 6 & regular.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ COND | GROUP), adjust = "tukey")


################################################
## ANALYSES EXPERIMENT 1: Spontaneous strategy discovery and switch
################################################

##### COLOR USE

clme = lmer(COLOR ~ BLOCK*GROUP + (1+BLOCK|ID), data = subset(ambiguous.cdf, ambiguous.cdf$COND == 'LEARN' & ambiguous.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
Anova(clme)
clme = lmer(COLOR ~ GROUP + (1|ID), data = subset(ambiguous.cdf, ambiguous.cdf$COND == 'LEARN' & ambiguous.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

# Only last 2 blocks
clme = lmer(COLOR ~ GROUP + (1|ID), data = subset(ambiguous.cdf, ambiguous.cdf$COND == 'LEARN' & ambiguous.cdf$BLOCK > 6 & ambiguous.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

# Proportion of subjects who switched
tmp = tapply(ambiguous.cdf$COLOR, list(regular.cdf$ID, regular.cdf$BLOCK, regular.cdf$GROUP, regular.cdf$EXP), mean)[,,,'V1']
tmp = apply(tmp[,7:8,], c(1, 3), mean, na.rm = TRUE) > (qbinom(0.95, 64, prob = 0.5)/64)*100
ctab = apply(tmp, 2, table)
chisq.test(ctab, simulate.p.value = TRUE)

# Other thresholds (75%)
tmp = tapply(ambiguous.cdf$COLOR, list(regular.cdf$ID, regular.cdf$BLOCK, regular.cdf$GROUP, regular.cdf$EXP), mean)[,,,'V1']
tmp = apply(tmp[,7:8,], c(1, 3), mean, na.rm = TRUE) > 75
ctab = apply(tmp, 2, table)
chisq.test(ctab, simulate.p.value = TRUE)

# Other thresholds (50%)
tmp = tapply(ambiguous.cdf$COLOR, list(regular.cdf$ID, regular.cdf$BLOCK, regular.cdf$GROUP, regular.cdf$EXP), mean)[,,,'V1']
tmp = apply(tmp[,7:8,], c(1, 3), mean, na.rm = TRUE) > 50
ctab = apply(tmp, 2, table)
chisq.test(ctab, simulate.p.value = TRUE)

## Reported color rule recognition, reported color rule use, correct reports
t.test(scores.cdf$RECOG[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'CHN Exp.1'],
	scores.cdf$RECOG[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'ADLT Exp.1'])

chisq.test(table(scores.cdf$RECOG[scores.cdf$EXP == 'V1'], scores.cdf$GROUPbin[scores.cdf$EXP == 'V1']))

t.test(scores.cdf$USED[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'CHN Exp.1'],
	scores.cdf$USED[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'ADLT Exp.1'])

chisq.test(table(scores.cdf$USED[scores.cdf$EXP == 'V1'], scores.cdf$GROUPbin[scores.cdf$EXP == 'V1']))

t.test(scores.cdf$CORRECT[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'CHN Exp.1'],
	scores.cdf$CORRECT[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'ADLT Exp.1'])
chisq.test(table(scores.cdf$CORRECT[scores.cdf$EXP == 'V1'], scores.cdf$GROUPbin[scores.cdf$EXP == 'V1']))


### SWITCH POINT ALIGNED DATA

# Pairwise tests between adjacent blocks
X = tapply(switchpoint.cdf$COLOR, list(switchpoint.cdf$ID, switchpoint.cdf$SWITCHED, switchpoint.cdf$BLOCK, switchpoint.cdf$GROUP, switchpoint.cdf$EXP), mean, na.rm = TRUE)[,,,,'V1']
ps_ya = sapply(1:10, function(x) t.test(X[,'SWITCHER',x,'YA'],X[,'SWITCHER',x+1,'YA'], paired = TRUE)$p.value)
ps_kids = sapply(1:10, function(x) t.test(X[,'SWITCHER',x,'KIDS'],X[,'SWITCHER',x+1,'KIDS'], paired = TRUE)$p.value)

# p values in young adults before switch
p.adjust(ps_ya[1:5], method = 'holm')
# p values in young adults after switch
p.adjust(ps_ya[7:10], method = 'holm')

# p values in young adults at switch
ps_ya[6]


# p values in children before switch
p.adjust(ps_kids[1:5], method = 'holm')
# p values in children after switch
p.adjust(ps_kids[7:10], method = 'holm')

# p values in children at switch
ps_kids[6]


# Interaction of time and age group
clme = lmer(COLOR ~ GROUP*SWITCHED*AFTER + (1 + AFTER|ID), data = subset(switchpoint.cdf, switchpoint.cdf$EXP == 'V1'))
Anova(clme)


# Age group difference before switch
clme = lmer(COLOR ~ GROUP + (1|ID), data = subset(ambiguous.cdf, ambiguous.cdf$SWITCH == FALSE & ambiguous.cdf$EXP == 'V1'))
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

# Age group difference after switch
clme = lmer(COLOR ~ GROUP + (1|ID), data = subset(ambiguous.cdf, ambiguous.cdf$SWITCH == TRUE & ambiguous.cdf$EXP == 'V1'))
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")


################################################
## ANALYSES EXPERIMENT 1 -- Covariate Tasks
################################################

# Delayed response and NoGo trials
clme = lmer(PREKEY_late ~ BLOCK*GROUP + (1+BLOCK|ID), data = subset(conflicts.cdf, conflicts.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
Anova(clme)
clme = lmer(PREKEY_late ~ GROUP + (1|ID), data = subset(conflicts.cdf, conflicts.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

clme = lmer(PREKEY_NoGo ~ BLOCK*GROUP + (1+BLOCK|ID), data = subset(conflicts.cdf, conflicts.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

# WM
t.test(scores.cdf$WM[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'CHN Exp.1'],
	scores.cdf$WM[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'ADLT Exp.1'])


# STROOP 
t.test(scores.cdf$STROOP[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'CHN Exp.1'])
t.test(scores.cdf$STROOP[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'ADLT Exp.1'])
t.test(scores.cdf$STROOP[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'CHN Exp.1'],
	scores.cdf$STROOP[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'ADLT Exp.1'])

t.test(scores.cdf$STROOP_INTER[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'CHN Exp.1'],
	scores.cdf$STROOP_INTER[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'ADLT Exp.1'])


################################################
## ANALYSES EXPERIMENT 2: Standard Trials
################################################

# ERRORS
clme = lmer(E ~ BLOCK*GROUP + (1+BLOCK|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V4'), control = lcctrl, REML = FALSE)
Anova(clme)

clme = lmer(E ~ GROUP + (1|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$BLOCK > 1 & regular.cdf$EXP == 'V4'), control = lcctrl, REML = FALSE)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

# Difference between age groups last two blocks
clme = lmer(E ~ GROUP + (1|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V4' & regular.cdf$BLOCK > 6), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

# RTS
clme = lmer(RT ~ BLOCK*GROUP + (1+BLOCK|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V4'), control = lcctrl, REML = FALSE)
Anova(clme)

# Mean difference between age groups
clme = lmer(RT ~ GROUP + (1|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V4'), control = lcctrl, REML = FALSE)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

# Difference between age groups last two blocks
clme = lmer(RT ~ GROUP + (1|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V4' & regular.cdf$BLOCK > 6), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")


# EFFECT OF INSTRUCTION BLOCK
# ERRORS: Test for interaction in pre/post color instruction
clme = lmer(E ~ GROUP*COND + (1 + COND|ID), data = subset(regular.cdf, regular.cdf$BLOCK > 6 & regular.cdf$EXP == 'V4'), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP | COND), adjust = "tukey")

# RTs: Test for interaction in pre/post color instruction
clme = lmer(RT ~ GROUP*COND + (1 + COND|ID), data = subset(regular.cdf, regular.cdf$BLOCK > 6 & regular.cdf$EXP == 'V4'), control = lcctrl, REML = FALSE)
Anova(clme)


################################################
## ANALYSES EXPERIMENT 2: Spontaneous strategy discovery and switch
################################################

### Ambiguous trials (THIS IS DIFFERENT!!! )
clme = lmer(COLOR ~ BLOCK*GROUP + (1 + BLOCK|ID), data = subset(ambiguous.cdf, ambiguous.cdf$COND == 'LEARN' & ambiguous.cdf$EXP == 'V4'), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

#### Last 2 Blocks
clme = lmer(COLOR ~ GROUP + (1|ID), data = subset(ambiguous.cdf, ambiguous.cdf$COND == 'LEARN' & ambiguous.cdf$BLOCK > 6 & ambiguous.cdf$EXP == 'V4'), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")


# Proportion of subjects who switched
tmp = tapply(ambiguous.cdf$COLOR, list(regular.cdf$ID, regular.cdf$BLOCK, regular.cdf$GROUP, regular.cdf$EXP), mean)[,,,'V4']
tmp = apply(tmp[,7:8,], c(1, 3), mean, na.rm = TRUE) > (qbinom(0.95, 64, prob = 0.5)/64)*100
ctab = apply(tmp, 2, table)
chisq.test(ctab, simulate.p.value = TRUE)

tmp = tapply(ambiguous.cdf$COLOR, list(regular.cdf$ID, regular.cdf$BLOCK, regular.cdf$GROUP, regular.cdf$EXP), mean)[,,,'V4']
tmp = apply(tmp[,7:8,], c(1, 3), mean, na.rm = TRUE) > 75
ctab = apply(tmp, 2, table)
chisq.test(ctab, simulate.p.value = TRUE)

tmp = tapply(ambiguous.cdf$COLOR, list(regular.cdf$ID, regular.cdf$BLOCK, regular.cdf$GROUP, regular.cdf$EXP), mean)[,,,'V4']
tmp = apply(tmp[,7:8,], c(1, 3), mean, na.rm = TRUE) > 50
ctab = apply(tmp, 2, table)
chisq.test(ctab, simulate.p.value = TRUE)


## Reported color rule recognition & correct reports
t.test(scores.cdf$RECOG[scores.cdf$EXP == 'V4' & scores.cdf$GROUP == 'CHN Exp.2'],
	scores.cdf$RECOG[scores.cdf$EXP == 'V4' & scores.cdf$GROUP == 'ADLT Exp.2'])

chisq.test(table(scores.cdf$RECOG[scores.cdf$EXP == 'V4'], scores.cdf$GROUPbin[scores.cdf$EXP == 'V1']))


t.test(scores.cdf$CORRECT[scores.cdf$EXP == 'V4' & scores.cdf$GROUP == 'CHN Exp.2'],
	scores.cdf$CORRECT[scores.cdf$EXP == 'V4' & scores.cdf$GROUP == 'ADLT Exp.2'])
chisq.test(table(scores.cdf$CORRECT[scores.cdf$EXP == 'V4'], scores.cdf$GROUPbin[scores.cdf$EXP == 'V4']), simulate.p.value = TRUE)

#### Not reported in the paper: how many subjects have used it (actually more kids report to have used it)
t.test(scores.cdf$USED[scores.cdf$EXP == 'V4' & scores.cdf$GROUP == 'CHN Exp.2'],
	scores.cdf$USED[scores.cdf$EXP == 'V4' & scores.cdf$GROUP == 'ADLT Exp.2'])

chisq.test(table(scores.cdf$USED[scores.cdf$EXP == 'V4'], scores.cdf$GROUPbin[scores.cdf$EXP == 'V4']))

### SWITCH POINT ALIGNED DATA

adult_switch = expinfo$switchpoints[which(names(expinfo$switchpoints) %in% expinfo$adultids_v4)]
kid_switch = expinfo$switchpoints[which(names(expinfo$switchpoints) %in% expinfo$kidids_v4)]

cdf = subset(switchpoint.cdf, switchpoint.cdf$EXP == 'V4' & !is.na(switchpoint.cdf$COLOR))

clme = lmer(COLOR ~ GROUP*AFTER + (1+AFTER|ID), data = cdf, control = lcctrl, REML = FALSE)
emmeans(clme, list(pairwise ~ AFTER:GROUP), adjust = "tukey")[[2]][5,]
emmeans(clme, list(pairwise ~ AFTER:GROUP), adjust = "tukey")[[2]][2,]
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")


################################################
## ANALYSES EXPERIMENT 2 -- Covariate Tasks
################################################

# Delayed response and NoGo trials
clme = lmer(PREKEY_late ~ BLOCK*GROUP + (1+BLOCK|ID), data = subset(conflicts.cdf, conflicts.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V4'), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

clme = lmer(PREKEY_NoGo ~ BLOCK*GROUP + (1+BLOCK|ID), data = subset(conflicts.cdf, conflicts.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V4'), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

# WM
t.test(scores.cdf$WM[scores.cdf$EXP == 'V4' & scores.cdf$GROUP == 'CHN Exp.2'],
	scores.cdf$WM[scores.cdf$EXP == 'V4' & scores.cdf$GROUP == 'ADLT Exp.2'])

# STROOP (here is a difference!!!) need to figure out why
t.test(scores.cdf$STROOP[scores.cdf$EXP == 'V4' & scores.cdf$GROUP == 'CHN Exp.2'])
t.test(scores.cdf$STROOP[scores.cdf$EXP == 'V4' & scores.cdf$GROUP == 'ADLT Exp.2'])
t.test(scores.cdf$STROOP[scores.cdf$EXP == 'V4' & scores.cdf$GROUP == 'CHN Exp.2'],
	scores.cdf$STROOP[scores.cdf$EXP == 'V4' & scores.cdf$GROUP == 'ADLT Exp.2'])


################################################
## Combined Analysis
################################################
expinfo$nkids
expinfo$nadults

# Mean Errors last two blocks
clme = lmer(E ~ GROUP + (1|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$BLOCK %in% c(1:2)), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

# Mean RTs last two blocks
clme = lmer(RT ~ GROUP + (1|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$BLOCK %in% c(1:2)), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

# Delayed response & NoGo trials
clme = lmer(PREKEY_late ~ GROUP + (1|ID), data = subset(conflicts.cdf, conflicts.cdf$COND == 'LEARN' & conflicts.cdf$BLOCK %in% c(1:2)), control = lcctrl, REML = FALSE)
Anova(clme)

clme = lmer(PREKEY_NoGo ~ GROUP + (1|ID), data = subset(conflicts.cdf, conflicts.cdf$COND == 'LEARN' & conflicts.cdf$BLOCK %in% c(1:2)), control = lcctrl, REML = FALSE)
Anova(clme)

# STROOP
t.test(scores.cdf$STROOP[scores.cdf$GROUPbin == 'KIDS'],
	scores.cdf$STROOP[scores.cdf$GROUPbin == 'YA'])

# WM
t.test(scores.cdf$WM[scores.cdf$GROUPbin == 'KIDS'],
			scores.cdf$WM[scores.cdf$GROUPbin == 'YA'])

# Color use in last two blocks
clme = lmer(COLOR ~ GROUP + (1|ID), data = subset(ambiguous.cdf, ambiguous.cdf$COND == 'LEARN' & ambiguous.cdf$BLOCK > 6 ), control = lcctrl, REML = FALSE)
Anova(clme)

tmp = tapply(ambiguous.cdf$COLOR, list(regular.cdf$ID, regular.cdf$BLOCK, regular.cdf$GROUP, regular.cdf$EXP), mean)
tmp1 = apply(tmp[,7:8,,'V1'], c(1, 3), mean, na.rm = TRUE) > (qbinom(0.95, 64, prob = 0.5)/64)*100
tmp2 = apply(tmp[,7:8,,'V4'], c(1, 3), mean, na.rm = TRUE) > (qbinom(0.95, 32, prob = 0.5)/32)*100

tmp1[is.na(tmp1)] = tmp2[is.na(tmp1)]

ctab = apply(tmp1, 2, table)
chisq.test(ctab, simulate.p.value = TRUE)

## Switchpoints
t.test(expinfo$adult_switch, expinfo$kid_switch)


## Power analysis

expinfo$nadults
pwr.chisq.test(w = 0.3, N = expinfo$nkids + expinfo$nadults, df = 1, sig.level = .05)
pwr.chisq.test(w = 0.2, N = expinfo$nkids + expinfo$nadults, df = 1, sig.level = .05)



## Reported color rule recognition, reported color use, correct reports

t.test(scores.cdf$RECOG[scores.cdf$GROUPbin == 'KIDS'],
	scores.cdf$RECOG[scores.cdf$GROUPbin == 'YA'])

chisq.test(table(scores.cdf$RECOG, scores.cdf$GROUPbin))

t.test(scores.cdf$USED[scores.cdf$GROUPbin == 'KIDS'],
	scores.cdf$USED[scores.cdf$GROUPbin == 'YA'])

chisq.test(table(scores.cdf$USED, scores.cdf$GROUPbin))

t.test(scores.cdf$CORRECT[scores.cdf$GROUPbin == 'KIDS'],
	scores.cdf$CORRECT[scores.cdf$GROUPbin == 'YA'])
chisq.test(table(scores.cdf$CORRECT, scores.cdf$GROUPbin), simulate.p.value = TRUE)

### Make data frame
cdf = data.frame(P = c(as.matrix(scores.cdf[,c(20:25, 28:29, 32:33)])), ID = rep(scores.cdf$ID, 10), AGEGROUP = rep(scores.cdf$GROUPbin, 10), DV = as.factor(rep(1:10, each = dim(scores.cdf)[1])),
	CAT = as.factor(c(rep(1, each = dim(scores.cdf)[1]*6), rep(2, each = dim(scores.cdf)[1]*4))))

clme = lmer(P~AGEGROUP*CAT + (1 + CAT|ID), data = cdf)
Anova(clme)


scores.cdf.no.na = na.omit(scores.cdf)

cor.test(scores.cdf.no.na$PRELATE, scores.cdf.no.na$PRENO)
cor.test(scores.cdf.no.na$E, scores.cdf.no.na$RT)

# Baseline model
minmod <- lm(FOLL_logit ~ GROUPbin, data=scores.cdf.no.na) #AMBIG can also be included
summary(minmod)
AIC(minmod)

fullmod = lm(FOLL_logit ~ GROUPbin*(STROOPz+TASKz+PREz+WMz), data=scores.cdf.no.na) #AMBIG can also be included
summary(fullmod)
AIC(fullmod)

anova(fullmod, minmod)

reducedmod = stepAIC(fullmod, scope=list(upper = ~GROUPbin*(STROOPz+WMz+TASKz+PREz), lower = ~1), direction = "both")
summary(reducedmod)
AIC(reducedmod)

summary(fullmod)


lm_kids = lm(scores.cdf.no.na$FOLL_logit[scores.cdf.no.na$GROUPbin == 'KIDS']~scores.cdf.no.na$TASKz[scores.cdf.no.na$GROUPbin == 'KIDS'])
lm_ya = lm(scores.cdf.no.na$FOLL_logit[scores.cdf.no.na$GROUPbin == 'YA']~scores.cdf.no.na$TASKz[scores.cdf.no.na$GROUPbin == 'YA'])

sqrt(summary(lm_kids)$r.squared)
sqrt(summary(lm_ya)$r.squared)

lm_kids = lm(scores.cdf.no.na$FOLL_logit[scores.cdf.no.na$GROUPbin == 'KIDS']~scores.cdf.no.na$STROOPz[scores.cdf.no.na$GROUPbin == 'KIDS'])
lm_ya = lm(scores.cdf.no.na$FOLL_logit[scores.cdf.no.na$GROUPbin == 'YA']~scores.cdf.no.na$STROOPz[scores.cdf.no.na$GROUPbin == 'YA'])

sqrt(summary(lm_kids)$r.squared)
sqrt(summary(lm_ya)$r.squared)
