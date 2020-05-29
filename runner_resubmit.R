################################################
## SETUP: load packages, settings and data 
################################################

setwd('/gdrive/workbench/gitrepos/pu2dkids')

## load packages 
packages = c('car', 'sfsmisc', 'RColorBrewer', 'gdata', 'beeswarm', 'lme4', 'emmeans', 'pwr')
pksload = unlist(lapply(packages, require, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))

if(sum(pksload) != length(packages)) {
	warning('SOME PACKAGES NOT LOADED')
}

#load some custom functions 
source('/gdrive/workbench/R/tools/misc/gen_functions.R')

# load data 
#load('pu2dkids_data.Rdata')
source('get_data.R')
# note on condition coding: 
# 1 = standard
# 2 = ambiguous
# 3 = delayed Go 
# 4 = delayed noGo

# prep data 
source('prep_data.R')
source('/gdrive/workbench/R/tools/misc/gen_functions.R')

# settings for lmer
lcctrl = lmerControl(optimizer=c('bobyqa'), optCtrl = list(maxfun = 500000))

# colors for plotting 
colorset = rbind(brewer.pal(6, 'Reds')[c(3, 6)], brewer.pal(6, 'Blues')[c(3,6)]) 


################################################
## MAKE DATA FRAMES 
################################################

# regular trials 
regular.cdf = data.frame(
	E = as.vector(errors), 
	RT = as.vector(rts),
	ID = rep(rownames(rts), prod(dim(rts)[2:4])),
	BLOCK = rep(as.numeric(colnames(rts)), each = dim(rts)[1], prod(dim(rts)[3:4])),
	EXP = rep(dimnames(rts)[[3]], each = prod(dim(rts)[1:2]), dim(rts)[4]), 
	GROUP = rep(dimnames(rts)[[4]], each = prod(dim(rts)[1:3])))

regular.cdf = subset(regular.cdf, !is.na(regular.cdf$E))

regular.cdf$COND = NA
regular.cdf$COND[regular.cdf$BLOCK < 9] = 'LEARN'
regular.cdf$COND[regular.cdf$BLOCK >= 9] = 'INSTR'
regular.cdf$COND = as.factor(regular.cdf$COND)


conflicts.cdf = data.frame(
	PREKEY_late = as.vector(prekeys_late), 
	PREKEY_NoGo = as.vector(prekeys_noGo),
	AMBIG_COSTS = as.vector(ambiguity_costs),
	CONG_COSTS = as.vector(congruency_costs),
	ID = rep(rownames(prekeys_late), prod(dim(prekeys_late)[2:4])),
	BLOCK = rep(as.numeric(colnames(prekeys_late)), each = dim(prekeys_late)[1], prod(dim(prekeys_late)[3:4])),
	EXP = rep(dimnames(prekeys_late)[[3]], each = prod(dim(prekeys_late)[1:2]), dim(prekeys_late)[4]), 
	GROUP = rep(dimnames(prekeys_late)[[4]], each = prod(dim(prekeys_late)[1:3])))

conflicts.cdf = subset(conflicts.cdf, !is.na(conflicts.cdf$PREKEY_late))
conflicts.cdf$COND = NA
conflicts.cdf$COND[conflicts.cdf$BLOCK < 9] = 'LEARN'
conflicts.cdf$COND[conflicts.cdf$BLOCK >= 9] = 'INSTR'
conflicts.cdf$COND = as.factor(conflicts.cdf$COND)


cthresh = qbinom(0.95, 64, prob = 0.5)/64 
tmp = tapply(DATA$followed, list(DATA$id, DATA$cond, DATA$block>6 & DATA$block<9, DATA$respkey %in% c(77, 88, 188)), mean, na.rm = TRUE)[,2,'TRUE','TRUE']

switchids = which(tmp > cthresh)
nswitchids = which(tmp <= cthresh)
nswitchers = length(switchids)

ambiguous.cdf = data.frame(
	COLOR = as.vector(followed), 
	ID = rep(rownames(followed), prod(dim(followed)[2:4])),
	AGE = rep(ages, prod(dim(followed)[2:4])),
	SWITCH = rep(rownames(followed) %in% names(switchids), prod(dim(followed)[2:4])),
	BLOCK = rep(as.numeric(colnames(followed)), each = dim(followed)[1], prod(dim(followed)[3:4])),
	EXP = rep(dimnames(followed)[[3]], each = prod(dim(followed)[1:2]), dim(followed)[4]), 
	GROUP = rep(dimnames(followed)[[4]], each = prod(dim(followed)[1:3])))


ambiguous.cdf = subset(ambiguous.cdf, !is.na(ambiguous.cdf$COLOR))
ambiguous.cdf$COND = NA
ambiguous.cdf$COND[ambiguous.cdf$BLOCK == 1] = 'RAND'
ambiguous.cdf$COND[ambiguous.cdf$BLOCK < 9 & ambiguous.cdf$BLOCK > 1] = 'LEARN'
ambiguous.cdf$COND[ambiguous.cdf$BLOCK >= 9] = 'INSTR'
ambiguous.cdf$COND = as.factor(ambiguous.cdf$COND)


#### SWITCH POINT ALIGNED 
cthresh = qbinom(0.95, 64, prob = 0.5)/64 
tmp = tapply(DATA$followed, list(DATA$id, DATA$cond, DATA$block>6 & DATA$block<9, DATA$respkey %in% c(77, 88, 188)), mean, na.rm = TRUE)[,2,'TRUE','TRUE']

switchids = which(tmp > cthresh)
nswitchids = which(tmp <= cthresh)
nswitchers = length(switchids)
nkidswicthers = length(which(ids %in% names(switchids) & ids %in% kidids_v4))
nadultswicthers = length(which(ids %in% names(switchids) & ids %in% adultids_v4))


tmp_switch = tapply(DATA$followed, list(DATA$id, DATA$miniblock, DATA$cond, DATA$respkey %in% c(0:200)), mean, na.rm = TRUE)[,,2, 'TRUE']
tmp_switch = tmp_switch[,1:16]
tmp = t(apply(tmp_switch - matrix(rowMeans(tmp_switch, na.rm = TRUE), ncol = 16, nrow = nids), 1, FUN = function(x) cumsum(x)))

#cmins = apply(tmp, 1, FUN = function(x) {min(x, na.rm = TRUE)})
#sort(cmins[switchids])

#matplot(t(tmp[switchids,]), lty = 1, type = 'o', col = 1)

#matplot(t(tmp_switch[c('201', '102', '210', '214'),]), type = 'l')

switchpoints = apply(tmp, 1, FUN = function(x) {which.min(x)})
switchpoints[!(ids %in% names(switchids))] = sample(switchpoints[(ids %in% names(switchids))], nids- nswitchers, TRUE)

adult_switch = switchpoints[which(ids %in% names(switchids) & ids %in% adultids)]
kid_switch = switchpoints[which(ids %in% names(switchids) & ids %in% kidids)]


tmp_switch2 = cbind(NA, NA, NA, NA, NA, NA, NA, NA, tmp_switch, NA, NA, NA, NA, NA, NA, NA, NA)
switchpoints2 = switchpoints + 7
follow_aligned = sapply(1:nids, function(x) tmp_switch2[x, (switchpoints2[x]-5):(switchpoints2[x] + 7)])*100
follow_aligned_mean = follow_aligned_ses = matrix(NA, 4, 13)
follow_aligned_mean[1,] = apply(follow_aligned[,which(ids %in% names(switchids) & ids %in% kidids)], 1, mean, na.rm = TRUE)
follow_aligned_mean[2,] = apply(follow_aligned[,which(ids %in% names(nswitchids) & ids %in% kidids)], 1, mean, na.rm = TRUE)
follow_aligned_mean[3,] = apply(follow_aligned[,which(ids %in% names(switchids) & ids %in% adultids)], 1, mean, na.rm = TRUE)
follow_aligned_mean[4,] = apply(follow_aligned[,which(ids %in% names(nswitchids) & ids %in% adultids)], 1, mean, na.rm = TRUE)

follow_aligned_ses[1,] = apply(follow_aligned[,which(ids %in% names(switchids) & ids %in% kidids)], 1, std.error, na.rm = TRUE)
follow_aligned_ses[2,] = apply(follow_aligned[,which(ids %in% names(nswitchids) & ids %in% kidids)], 1, std.error, na.rm = TRUE)
follow_aligned_ses[3,] = apply(follow_aligned[,which(ids %in% names(switchids) & ids %in% adultids)], 1, std.error, na.rm = TRUE)
follow_aligned_ses[4,] = apply(follow_aligned[,which(ids %in% names(nswitchids) & ids %in% adultids)], 1, std.error, na.rm = TRUE)

follow_aligned_mean = follow_aligned_mean[,2:13]
follow_aligned_ses = follow_aligned_ses[,2:13]

follow_aligned = t(follow_aligned)
follow_aligned = follow_aligned[,2:13]
#follow_aligned[,4:9]
rownames(follow_aligned) = ids
colnames(follow_aligned) = c(-6:-1, 1:6)

switchpoint.cdf = data.frame(COLOR = as.vector(follow_aligned), 
	ID = rep(rownames(follow_aligned), prod(dim(follow_aligned)[1:2])),
	EXP = rep(rownames(follow_aligned), prod(dim(follow_aligned)[1:2])),
	BLOCK = rep(as.numeric(colnames(follow_aligned)), each = dim(follow_aligned)[1]))


switchpoint.cdf$EXP = NA 
switchpoint.cdf$EXP[switchpoint.cdf$ID %in% c(adultids_v1, kidids_v1)]  = 'V1'
switchpoint.cdf$EXP[switchpoint.cdf$ID %in% c(adultids_v4, kidids_v4)]  = 'V4'

switchpoint.cdf$GROUP = NA 
switchpoint.cdf$GROUP[switchpoint.cdf$ID %in% kidids] = 'KIDS'
switchpoint.cdf$GROUP[switchpoint.cdf$ID %in% adultids] = 'YA'
switchpoint.cdf$GROUP = as.factor(switchpoint.cdf$GROUP)

switchpoint.cdf$AFTER = NA
switchpoint.cdf$AFTER[switchpoint.cdf$BLOCK < 0] = 'BEFORE'
switchpoint.cdf$AFTER[switchpoint.cdf$BLOCK > 0] = 'AFTER'

switchpoint.cdf$SWITCHED = NA
switchpoint.cdf$SWITCHED[switchpoint.cdf$ID %in% names(switchids)] = 'SWITCHER'
switchpoint.cdf$SWITCHED[!(switchpoint.cdf$ID %in% names(switchids))] = 'NOSWITCHER'

switchpoint.cdf$BLOCK = as.factor(switchpoint.cdf$BLOCK)
switchpoint.cdf$EXP = as.factor(switchpoint.cdf$EXP)
switchpoint.cdf$AFTER = as.factor(switchpoint.cdf$AFTER)
switchpoint.cdf$SWITCHED = as.factor(switchpoint.cdf$SWITCHED)

switchpoint.cdf = subset(switchpoint.cdf, switchpoint.cdf$BLOCK %in% -3:3)
switchpoint.cdf$BLOCK = droplevels(switchpoint.cdf$BLOCK)

#### QUESTIONNAIRE 
setdiff(ids, questids)
sum(setdiff(ids, questids) %in% kidids)
sum(setdiff(ids, questids) %in% adultids)

recog = recode(Qdata$recognised, "'yes' = 1; 'no' = 0")
names(recog) = Qdata$id

used = recode(Qdata$used, "'yes' = 1; 'no' = 0")
names(used) = Qdata$id

ctab = cbind(Qdata$left.up, Qdata$right.down, Qdata$left.down, Qdata$right.up) - 2

Qdata$correctreport = NA
Qdata$correctreport[Qdata$id %% 2 == 0] = apply(ctab[Qdata$id %% 2 == 0,], 1, function(x) mean(x == c(1, 1, 2, 2)) == 1)
Qdata$correctreport[Qdata$id %% 2 == 1] = apply(ctab[Qdata$id %% 2 == 1,], 1, function(x) mean(x == c(2, 2, 1, 1)) == 1)


wmscore = Qdata$wm.score
names(wmscore) = Qdata$id
stroop_rts = tapply(Sdata$resp.rt, list(Sdata$id, Sdata$congruent, Sdata$resp.corr, is.na(Sdata$trials.thisN)), mean)[,,'1', 'FALSE']*1000 
stroop_errors = 100 - tapply(Sdata$resp.corr, list(Sdata$id, Sdata$congruent, is.na(Sdata$trials.thisN)), mean, na.rm = TRUE)[,,'FALSE']*100 
stroop2_rts = tapply(Sdata2$Stimulus.RT, list(Sdata2$id, Sdata2$cond, Sdata2$Stimulus.ACC, Sdata2$Procedure.Block.), mean)[,,'0', 'BlockProc']
stroop2_errors = tapply(Sdata2$Stimulus.ACC, list(Sdata2$id, Sdata2$cond, Sdata2$Procedure.Block.), mean)[,, 'BlockProc']*100

stroop_rt_score = c(stroop_rts[,'neutral'] - stroop_rts[,'cong'], stroop2_rts[,'neutral'] - stroop2_rts[,'cong'])
stroop_error_score = c(stroop_errors[,'neutral'] - stroop_errors[,'cong'], stroop2_errors[,'neutral'] - stroop2_errors[,'cong'])
stroop_rt_score_interfere = c(stroop_rts[,'neutral'] - stroop_rts[,'incong'], stroop2_rts[,'neutral'] - stroop2_rts[,'incong'])


scores.cdf = data.frame(
	ID = names(allerrs), 
	WM = NA, 
	STROOP = NA, 
	E = allerrs, 
	RT = allrts, 
	PRELATE = allprelate, 
	PRENO = allpreno, 
	CONG = allcongcosts, 
	AMBIG = allambigcosts, 
	FOLL = allfoll, 
	FOLLbin = allfoll > (qbinom(0.95, 64, prob = 0.5)/64)*100, 
	RECOG = NA, 
	USED = NA, 
	GROUP = group)

WMmap = sapply(1:length(scores.cdf$ID), function(x) which(scores.cdf$ID[x] == names(wmscore)))
WMmap[unlist(lapply(WMmap, function(x) length(x)==0))] = NA
WMmap = unlist(WMmap)
scores.cdf$WM = wmscore[WMmap]

Smap = sapply(1:length(scores.cdf$ID), function(x) which(scores.cdf$ID[x] == names(stroop_rt_score)))
Smap[unlist(lapply(Smap, function(x) length(x)==0))] = NA
Smap = unlist(Smap)
scores.cdf$STROOP = stroop_rt_score[Smap]
scores.cdf$STROOP_INTER = stroop_rt_score_interfere[Smap]

Rmap = sapply(1:length(scores.cdf$ID), function(x) which(scores.cdf$ID[x] == names(recog)))
Rmap[unlist(lapply(Rmap, function(x) length(x)==0))] = NA
Rmap = unlist(Rmap)
scores.cdf$RECOG = recog[Rmap]

Umap = sapply(1:length(scores.cdf$ID), function(x) which(scores.cdf$ID[x] == names(used)))
Umap[unlist(lapply(Umap, function(x) length(x)==0))] = NA
Umap = unlist(Umap)
scores.cdf$USED = used[Umap]

CORRECTmap = sapply(1:length(scores.cdf$ID), function(x) which(scores.cdf$ID[x] == Qdata$id))
CORRECTmap[unlist(lapply(CORRECTmap, function(x) length(x)==0))] = NA
CORRECTmap = unlist(CORRECTmap)
scores.cdf$CORRECT = Qdata$correctreport[CORRECTmap]


scores.cdf$GROUPbin = NA
scores.cdf$GROUPbin[scores.cdf$GROUP == 'CHN Exp.1' | scores.cdf$GROUP == 'CHN Exp.2'] = 'KIDS'
scores.cdf$GROUPbin[scores.cdf$GROUP == 'ADLT Exp.1' | scores.cdf$GROUP == 'ADLT Exp.2'] = 'YA'
scores.cdf$GROUPbin = as.factor(scores.cdf$GROUPbin)

for (cvar in names(scores.cdf)[2:13]) {
	cvarz = paste(cvar, 'z', sep = '')
	scores.cdf[cvarz] = -as.vector(scale(scores.cdf[cvar]))
}

# recode vars where lower is 'better' (sign flip to all already above)
scores.cdf$FOLLz = -scores.cdf$FOLLz
scores.cdf$FOLLbinz = -scores.cdf$FOLLbinz
scores.cdf$WMz = -scores.cdf$WMz
scores.cdf$RECOGz = -scores.cdf$RECOGz
scores.cdf$USEDz = -scores.cdf$USEDz

scores.cdf$EXP = NA

scores.cdf$EXP[scores.cdf$ID %in% adultids_v1] = 'V1'
scores.cdf$EXP[scores.cdf$ID %in% kidids_v1] = 'V1'
scores.cdf$EXP[scores.cdf$ID %in% adultids_v4] = 'V4'
scores.cdf$EXP[scores.cdf$ID %in% kidids_v4] = 'V4'


################################################
## OVERVIEW: number of subjects in different experiments 
################################################


# adults stats 
nadults_v1 
mean(tapply(DATA$age, DATA$id, mean)[adultids_v1])
range(tapply(DATA$age, DATA$id, mean)[adultids_v1])
mean(tapply(DATA$sex, DATA$id, function(x) unique(x))[adultids_v1]-1)*nadults_v1

nadults_v4 
mean(tapply(DATA$age, DATA$id, mean)[adultids_v4])
range(tapply(DATA$age, DATA$id, mean)[adultids_v4])
mean(tapply(DATA$sex, DATA$id, function(x) unique(x))[adultids_v4]-1)*nadults_v4

# all 
nadults 
mean(tapply(DATA$age, DATA$id, mean)[adultids])
range(tapply(DATA$age, DATA$id, mean)[adultids])
mean(tapply(DATA$sex, DATA$id, function(x) unique(x))[adultids]-1)*nadults

# kids 
nkids_v1
mean(tapply(DATA$age, DATA$id, mean)[kidids_v1])
range(tapply(DATA$age, DATA$id, mean)[kidids_v1])
mean(tapply(DATA$sex, DATA$id, function(x) unique(x))[kidids_v1]-1)*nkids_v1

# kids 
nkids_v4
mean(tapply(DATA$age, DATA$id, mean)[kidids_v4])
range(tapply(DATA$age, DATA$id, mean)[kidids_v4])
mean(tapply(DATA$sex, DATA$id, function(x) unique(x))[kidids_v4]-1)*nkids_v4

# kids 
nkids
mean(tapply(DATA$age, DATA$id, mean)[kidids])
range(tapply(DATA$age, DATA$id, mean)[kidids])
mean(tapply(DATA$sex, DATA$id, function(x) unique(x))[kidids]-1)*nkids

ages = tapply(DATA$age, DATA$id, mean)

################################################
## ANALYSES EXPERIMENT 1
################################################

# ERRORS 

clme = lmer(E ~ BLOCK*GROUP + (1+BLOCK|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
Anova(clme)

# mean diff between age groups 
clme = lmer(E ~ GROUP + (1|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

# diff between age groups last two blocks 
clme = lmer(E ~ GROUP + (1|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V1' & regular.cdf$BLOCK > 6), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

# RTS

clme = lmer(RT ~ BLOCK*GROUP + (1+BLOCK|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
Anova(clme)

# mean diff between age groups 
clme = lmer(RT ~ GROUP + (1|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

# diff between age groups last two blocks 
clme = lmer(RT ~ GROUP + (1|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V1' & regular.cdf$BLOCK > 6), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")


# EFFECT OF INSTRUCTION BLOCK 
# ERRORS: test for interaction in pre/post instructions 
clme = lmer(E ~ GROUP*COND + (1 + COND|ID), data = subset(regular.cdf, regular.cdf$BLOCK > 6 & regular.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ COND | GROUP), adjust = "tukey")

# RTs: test for interaction in pre/post instructions 
clme = lmer(RT ~ GROUP*COND + (1 + COND|ID), data = subset(regular.cdf, regular.cdf$BLOCK > 6 & regular.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ COND | GROUP), adjust = "tukey")


# RESPONSE INHIBITION 
clme = lmer(PREKEY_late ~ BLOCK*GROUP + (1+BLOCK|ID), data = subset(conflicts.cdf, conflicts.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
Anova(clme)
clme = lmer(PREKEY_late ~ GROUP + (1|ID), data = subset(conflicts.cdf, conflicts.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

clme = lmer(PREKEY_NoGo ~ BLOCK*GROUP + (1+BLOCK|ID), data = subset(conflicts.cdf, conflicts.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
Anova(clme)
clme = lmer(PREKEY_NoGo ~ GROUP + (1|ID), data = subset(conflicts.cdf, conflicts.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

# STROOP 
t.test(scores.cdf$STROOP[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'CHN Exp.1'])
t.test(scores.cdf$STROOP[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'ADLT Exp.1'])
t.test(scores.cdf$STROOP[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'CHN Exp.1'], 
	scores.cdf$STROOP[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'ADLT Exp.1'])

t.test(scores.cdf$STROOP_INTER[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'CHN Exp.1'], 
	scores.cdf$STROOP_INTER[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'ADLT Exp.1'])

t.test(scores.cdf$WM[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'CHN Exp.1'], 
	scores.cdf$WM[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'ADLT Exp.1'])


## FIGURE 2
axiscex = 1.1
axisline = 2


pdf('plots/Errors_main.pdf', width = 4, height = 4)
tmp = tapply(regular.cdf$E, list(regular.cdf$ID, regular.cdf$BLOCK, regular.cdf$GROUP, regular.cdf$EXP), mean)[,,,'V1']
cmeans = apply(tmp, c(2, 3), mean, na.rm = TRUE)
csds = apply(tmp, c(2, 3), std.error)

matplot(cmeans, type = 'o', col = colorset[,2], lty = 1, lwd = 2, pch = 16,
	 ylim = c(0, 50), ylab = '', xlab = '',   
	 bty = 'n', cex.axis = 1.1, cex.lab = 1.2, lab = c(10, 6, 5), 
	 xaxt = 'n')

se_bars(1:10, cmeans[,'KIDS'], csds[,'KIDS'], col = colorset[1,2])
se_bars(1:10, cmeans[,'YA'], csds[,'YA'], col = colorset[2,2])

axis(1, at = 1:10, labels = c(1:10), cex = 1.1)
text(1, -17, 'RAND', srt = 0, xpd = TRUE, pos = 3, cex = 0.8)
text(9.5, -17, 'INSTR', srt = 0, xpd = TRUE, pos = 3, cex = 0.8)
abline(v = 1.5, lty = 2, lwd = 1, col = 'darkgrey')
abline(v = 8.5, lty = 2, lwd = 1, col = 'darkgrey')
mtext(1, text = 'Block', line = 2.7, cex = 1.3)
mtext(2, text = 'Errors (%)', line = 2.5, cex = 1.3)

legend('topright', legend = c('Children', 'Adults'), 
	col = t(colorset[,2]), lwd = 2, pch = 16, cex = 0.8, bg = 'white', box.lwd = 0)

dev.off()



pdf('plots/RTs_main.pdf', width = 4, height = 4)
tmp = tapply(regular.cdf$RT, list(regular.cdf$ID, regular.cdf$BLOCK, regular.cdf$GROUP, regular.cdf$EXP), mean)[,,,'V1']
cmeans = apply(tmp, c(2, 3), mean, na.rm = TRUE)
csds = apply(tmp, c(2, 3), std.error)

matplot(cmeans, type = 'o', col = colorset[,2], lty = 1, lwd = 2, pch = 16,
	 ylim = c(400, 1450), ylab = '', xlab = '',   
	 bty = 'n', cex.axis = 1.1, cex.lab = 1.2, lab = c(10, 6, 5), 
	 xaxt = 'n')

se_bars(1:10, cmeans[,'KIDS'], csds[,'KIDS'], col = colorset[2,2])
se_bars(1:10, cmeans[,'YA'], csds[,'YA'], col = colorset[1,2])

axis(1, at = 1:10, labels = c(1:10), cex = 1.1)
text(1, 30, 'RAND', srt = 0, xpd = TRUE, pos = 3, cex = 0.8)
text(9.5, 30, 'INSTR', srt = 0, xpd = TRUE, pos = 3, cex = 0.8)
abline(v = 1.5, lty = 2, lwd = 1, col = 'darkgrey')
abline(v = 8.5, lty = 2, lwd = 1, col = 'darkgrey')
mtext(1, text = 'Block', line = 2.7, cex = 1.3)
mtext(2, text = 'RTs (ms)', line = 2.5, cex = 1.3)

legend('topright', legend = c('Children', 'Adults'), 
	col = t(colorset[2:1,2]), lwd = 2, pch = 16, cex = 0.8, bg = 'white', box.lwd = 0.5)

dev.off()

pdf('plots/Prelate_mean.pdf', width = 2.6, height = 4)

prekey = tapply(conflicts.cdf$PREKEY_late, list(conflicts.cdf$ID, conflicts.cdf$EXP), mean)[,'V1']
group = tapply(conflicts.cdf$GROUP, list(conflicts.cdf$ID, conflicts.cdf$EXP), unique)[,'V1']


boxplot(prekey ~ group, bty = 'n', cex.axis = 1.1, cex.lab = 1.2, axes = FALSE, ylab = '', xlab = '', outline = FALSE, ylim = c(0, 50), add = FALSE, lwd = 2)

k = beeswarm(prekey~group, col = 'white', bg = colorset[,2], pch = 21, corral = 'wrap',
	 bty = 'n', cex.axis = 1.1, cex.lab = 1.2, 
	 xlab = '', ylab = '', labels = NA, cex = 0.9, axes = FALSE, lwd = 1, ylim = c(0, 50), add = TRUE)

text(1, -8.5, 'Children', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)
text(2, -8.5, 'Adults', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)

mtext(2, text = 'Premature Responses (%)', line = axisline, cex = axiscex)
axis(2, at = seq(0, 50, 10), labels = seq(0, 50, 10), cex = 1.1)

dev.off()

pdf('plots/NoGo_mean.pdf', width = 2.6, height = 4)

prekey = tapply(conflicts.cdf$PREKEY_NoGo, list(conflicts.cdf$ID, conflicts.cdf$EXP), mean)[,'V1']
group = tapply(conflicts.cdf$GROUP, list(conflicts.cdf$ID, conflicts.cdf$EXP), unique)[,'V1']

boxplot(prekey ~ group, bty = 'n', cex.axis = 1.1, cex.lab = 1.2, axes = FALSE, ylab = '', xlab = '', outline = FALSE, ylim = c(0, 50), add = FALSE, lwd = 2)

k = beeswarm(prekey~group, col = 'white', bg = colorset[,2], pch = 21, corral = 'wrap',
	 bty = 'n', cex.axis = 1.1, cex.lab = 1.2, 
	 xlab = '', ylab = '', labels = NA, cex = 0.9, axes = FALSE, lwd = 1, ylim = c(0, 50), add = TRUE)

text(1, -8.5, 'Children', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)
text(2, -8.5, 'Adults', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)

mtext(2, text = 'NoGo Responses (%)', line = axisline, cex = axiscex)
axis(2, at = seq(0, 50, 10), labels = seq(0, 50, 10), cex = 1.1)

dev.off()


pdf('plots/WM_mean.pdf', width = 2.6, height = 4)
boxplot(scores.cdf$WM[scores.cdf$EXP == 'V1'] ~ scores.cdf$GROUPbin[scores.cdf$EXP == 'V1'], bty = 'n', axes = FALSE, ylab = '', xlab = '', outline = FALSE, add = FALSE, lwd = 1.5, ylim =  c(0, 15))

k = beeswarm(scores.cdf$WM[scores.cdf$EXP == 'V1'] ~ scores.cdf$GROUPbin[scores.cdf$EXP == 'V1'], col = 'white', bg = colorset[,2], pch = 21, corral = 'wrap',
	 bty = 'n', cex.axis = 1.1, cex.lab = 1.2, 
	 xlab = '', ylab = '', labels = NA, cex = 0.9, axes = FALSE, lwd = 2,  add = TRUE)


text(1, -2.5, 'Children', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)
text(2, -2.5, 'Adults', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)

mtext(2, text = 'Working Memory Score', line = axisline, cex = axiscex)
axis(2, at = seq(0, 15, 3), labels = seq(0, 15, 3), cex = 1.1)
dev.off()

pdf('plots/STROOP_mean.pdf', width = 2.6, height = 4)
boxplot(scores.cdf$STROOP[scores.cdf$EXP == 'V1'] ~ scores.cdf$GROUPbin[scores.cdf$EXP == 'V1'], bty = 'n', axes = FALSE, ylab = '', xlab = '', outline = FALSE, add = FALSE, lwd = 1.5, ylim =  c(-100, 110))

k = beeswarm(scores.cdf$STROOP[scores.cdf$EXP == 'V1'] ~ scores.cdf$GROUPbin[scores.cdf$EXP == 'V1'], col = 'white', bg = colorset[,2], pch = 21, corral = 'wrap',
	 bty = 'n', cex.axis = 1.1, cex.lab = 1.2, 
	 xlab = '', ylab = '', labels = NA, cex = 0.9, axes = FALSE, lwd = 2, add = TRUE)


text(1, -135, 'Children', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)
text(2, -135, 'Adults', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)

mtext(2, text = 'Neutral RT - Cong. RT (ms)', line = axisline, cex = axiscex)
axis(2, at = seq(-100, 110, 50), labels = seq(-100, 110, 50), cex = 1.1)
abline(h = 0)

dev.off()



##### COLOR USE 

clme = lmer(COLOR ~ BLOCK*GROUP + (1+BLOCK|ID), data = subset(ambiguous.cdf, ambiguous.cdf$COND == 'LEARN' & ambiguous.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
Anova(clme)
clme = lmer(COLOR ~ GROUP + (1|ID), data = subset(ambiguous.cdf, ambiguous.cdf$COND == 'LEARN' & ambiguous.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

clme = lmer(COLOR ~ GROUP + (1|ID), data = subset(ambiguous.cdf, ambiguous.cdf$COND == 'LEARN' & ambiguous.cdf$BLOCK > 6 & ambiguous.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

# proportion of subjects who switched
tmp = tapply(ambiguous.cdf$COLOR, list(regular.cdf$ID, regular.cdf$BLOCK, regular.cdf$GROUP, regular.cdf$EXP), mean)[,,,'V1']
tmp = apply(tmp[,7:8,], c(1, 3), mean, na.rm = TRUE) > (qbinom(0.95, 64, prob = 0.5)/64)*100
ctab = apply(tmp, 2, table)
chisq.test(ctab, simulate.p.value = TRUE)

tmp = tapply(ambiguous.cdf$COLOR, list(regular.cdf$ID, regular.cdf$BLOCK, regular.cdf$GROUP, regular.cdf$EXP), mean)[,,,'V1']
tmp = apply(tmp[,7:8,], c(1, 3), mean, na.rm = TRUE) > 75
ctab = apply(tmp, 2, table)
chisq.test(ctab, simulate.p.value = TRUE)


tmp = tapply(ambiguous.cdf$COLOR, list(regular.cdf$ID, regular.cdf$BLOCK, regular.cdf$GROUP, regular.cdf$EXP), mean)[,,,'V1']
tmp = apply(tmp[,7:8,], c(1, 3), mean, na.rm = TRUE) > 50
ctab = apply(tmp, 2, table)
chisq.test(ctab, simulate.p.value = TRUE)


## recognition, used, correct reports 

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


X = tapply(switchpoint.cdf$COLOR, list(switchpoint.cdf$ID, switchpoint.cdf$SWITCHED, switchpoint.cdf$BLOCK, switchpoint.cdf$GROUP, switchpoint.cdf$EXP), mean, na.rm = TRUE)[,,,,'V1']

ps_ya = sapply(1:5, function(x) t.test(X[,'SWITCHER',x,'YA'],X[,'SWITCHER',x+1,'YA'], paired = TRUE)$p.value)

ps_kids = sapply(1:5, function(x) t.test(X[,'SWITCHER',x,'KIDS'],X[,'SWITCHER',x+1,'KIDS'], paired = TRUE)$p.value)

p.adjust(ps_ya, method = 'holm')
p.adjust(ps_kids, method = 'holm')


cdf = subset(switchpoint.cdf, switchpoint.cdf$SWITCHED == 'SWITCHER' & switchpoint.cdf$EXP == 'V1' & !is.na(switchpoint.cdf$COLOR))

clme = lmer(COLOR ~ GROUP*AFTER + (1+AFTER|ID), data = cdf, control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ AFTER:GROUP), adjust = "tukey")


summary(subset(ambiguous.cdf, ambiguous.cdf$SWITCH == TRUE))

clme = lmer(COLOR ~ GROUP + (1|ID), data = subset(ambiguous.cdf, ambiguous.cdf$SWITCH == TRUE & ambiguous.cdf$EXP == 'V1'))
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")


clme = lmer(COLOR ~ GROUP*SWITCHED*AFTER + (1 + AFTER|ID), data = subset(switchpoint.cdf, switchpoint.cdf$EXP == 'V1'))
Anova(clme)

X = tapply(switchpoint.cdf$COLOR, list(switchpoint.cdf$ID, switchpoint.cdf$SWITCHED, switchpoint.cdf$AFTER, switchpoint.cdf$GROUP, switchpoint.cdf$EXP), mean, na.rm = TRUE)[,,,,'V1']


t.test(X[,'SWITCHER', 'BEFORE', 'YA'], X[,'SWITCHER', 'BEFORE', 'KIDS'])
t.test(X[,'SWITCHER', 'AFTER', 'YA'], X[,'SWITCHER', 'AFTER', 'KIDS'])

t.test(X[,'SWITCHER', 'AFTER', 'YA'] - X[,'SWITCHER', 'BEFORE', 'YA'], X[,'SWITCHER', 'AFTER', 'KIDS'] - X[,'SWITCHER', 'BEFORE', 'KIDS'])



adult_switch = switchpoints[which(ids %in% names(switchids) & ids %in% adultids_v1)]
kid_switch = switchpoints[which(ids %in% names(switchids) & ids %in% kidids_v1)]
t.test(kid_switch/2, adult_switch/2)



cmeans = apply(tmp, c(2, 3), mean, na.rm = TRUE)
csds = apply(tmp, c(2, 3), std.error)


## FIGURE 3
axiscex = 1.1
axisline = 2


pdf('plots/Followed_main.pdf', width = 4, height = 4)
tmp = tapply(ambiguous.cdf$COLOR, list(ambiguous.cdf$ID, ambiguous.cdf$BLOCK, ambiguous.cdf$GROUP, ambiguous.cdf$EXP), mean)[,,,'V1']


cmeans = apply(tmp, c(2, 3), mean, na.rm = TRUE)
csds = apply(tmp, c(2, 3), std.error)

matplot(cmeans, type = 'o', col = colorset[,2], lty = 1, lwd = 2, pch = 16, ylab = '', xlab = '',   
	 bty = 'n', cex.axis = 1.1, cex.lab = 1.2, lab = c(10, 6, 5), 
	 xaxt = 'n', ylim = c(20, 100))

se_bars(1:10, cmeans[,'KIDS'], csds[,'KIDS'], col = colorset[1,2])
se_bars(1:10, cmeans[,'YA'], csds[,'YA'], col = colorset[2,2])

axis(1, at = 1:10, labels = c(1:10), cex = 1.1)
text(1, -10, 'RAND', srt = 0, xpd = TRUE, pos = 3, cex = 0.8)
text(9.5, -10, 'INSTR', srt = 0, xpd = TRUE, pos = 3, cex = 0.8)
abline(v = 1.5, lty = 2, lwd = 1, col = 'darkgrey')
abline(v = 8.5, lty = 2, lwd = 1, col = 'darkgrey')
abline(h = 50, lty = 2, lwd = 1, col = 'darkgrey')
mtext(1, text = 'Block', line = 2.7, cex = 1.3)
mtext(2, text = 'Color Use (%)', line = 2.5, cex = 1.3)

legend('top', legend = c('Children', 'Adults'), 
	col = t(colorset[,2]), lwd = 2, pch = 16, cex = 0.8, bg = 'white', box.lwd = 0)
dev.off()



pdf('plots/Switch_aligned.pdf', width = 3.5, height = 4)

tmp1 = tapply(switchpoint.cdf$COLOR, list(switchpoint.cdf$ID, switchpoint.cdf$BLOCK, switchpoint.cdf$GROUP, switchpoint.cdf$SWITCHED, switchpoint.cdf$EXP), mean)[,,,'NOSWITCHER','V1']
tmp2 = tapply(switchpoint.cdf$COLOR, list(switchpoint.cdf$ID, switchpoint.cdf$BLOCK, switchpoint.cdf$GROUP, switchpoint.cdf$SWITCHED, switchpoint.cdf$EXP), mean)[,,,'SWITCHER','V1']

cmeans = cbind(apply(tmp1[,4:10,], c(2, 3), mean, na.rm = TRUE), apply(tmp2[,4:10,], c(2, 3), mean, na.rm = TRUE))
csds = cbind(apply(tmp1[,4:10,], c(2, 3), std.error), apply(tmp2[,4:10,], c(2, 3), std.error))

matplot(cmeans, type = 'o', col = colorset, lty = 1, lwd = 2, pch = 16, ylab = '', xlab = '',   
	 bty = 'n', xaxt = 'n', cex.axis = 1.1, cex.lab = 1.2, lab = c(10, 6, 5), , ylim = c(20, 100))

se_bars(1:7, cmeans[,1], csds[,1], col = colorset[1,1])
se_bars(1:7, cmeans[,2], csds[,2], col = colorset[2,1])

se_bars(1:7, cmeans[,3], csds[,3], col = colorset[1,2])
se_bars(1:7, cmeans[,4], csds[,4], col = colorset[2,2])



axis(1, at = 1:7, labels = c('-3', '-2', '-1', '+1', '+2', '+3', '+4'), cex = 1.1)
abline(v = 3.5, lty = 2, lwd = 1, col = 'darkgrey')
abline(h = 50, lty = 2, lwd = 1, col = 'darkgrey')
mtext(1, text = 'Block rel. to switch', line = 2.7, cex = 1.3)
mtext(2, text = 'Color Use (%)', line = 2.5, cex = 1.3)

dev.off()


pdf('plots/Followed_mean.pdf', width = 2.6, height = 4)

tmp = apply(tapply(ambiguous.cdf$COLOR, list(ambiguous.cdf$ID, ambiguous.cdf$BLOCK, ambiguous.cdf$EXP), mean)[,7:8,'V1'], c(1), mean)
tmp = tmp[!is.na(tmp)]

cgroup = ages[names(tmp)] > 12


boxplot(tmp ~ cgroup, bty = 'n', axes = FALSE, ylab = '', xlab = '', outline = FALSE, add = FALSE, lwd = 2, ylim = c(20, 100))

k = beeswarm(tmp ~ cgroup, col = 'white', bg = colorset[,2], pch = 21, corral = 'wrap', 
	 bty = 'n', cex.axis = 1.1, cex.lab = 1.2, 
	 xlab = '', ylab = '', labels = NA, cex = 0.9, axes = FALSE, lwd = 2, ylim = c(20, 100), add = TRUE)


text(1, 5, 'Children', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)
text(2, 5, 'Adults', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)

mtext(2, text = 'Color Use Blocks 7-8 (%)', line = axisline, cex = axiscex)
axis(2, cex = 1.1)
abline(h = cthresh*100, lty = 2)

dev.off()


pdf('plots/Followed_proportions.pdf', width = 2.6, height = 4.2)

tmp = tapply(ambiguous.cdf$COLOR, list(regular.cdf$ID, regular.cdf$BLOCK, regular.cdf$GROUP, regular.cdf$EXP), mean)[,,,'V1']
tmp = apply(tmp[,7:8,], c(1, 3), mean, na.rm = TRUE) > (qbinom(0.95, 64, prob = 0.5)/64)*100
ctab = apply(tmp, 2, table)
ctab = cbind(c(ctab[,1], 0, 0), c(0, 0, ctab[,2]))
k = barplot(ctab, border = 'white', col = t(colorset), 
	 bty = 'n', cex.axis = 1.1, cex.lab = 1.2, 
	 xlab = '', ylab = '', cex = 0.9, axes = FALSE, ylim = c(0, 22))

text(k[1], -3, 'Children', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)
text(k[2], -3, 'Adults', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)

mtext(2, text = 'Number of Participants', line = axisline, cex = axiscex)
axis(2, cex = 1.1, at = seq(0, 20, 5))
text(x = k[1]-0.3, y = 1, srt = 90, pos = 4, col = 'white', labels = 'No', cex = 0.85)
text(x = k[1], y = 1, srt = 90, pos = 4, col = 'white', labels = 'Color Use', cex = 0.85)
text(x = k[1]-0.3, y = 15.5, srt = 90, pos = 4, col = 'white', labels = 'Color', cex = 0.85)
text(x = k[1], y = 15.5, srt = 90, pos = 4, col = 'white', labels = 'Use', cex = 0.85)

text(x = k[2]-0.3, y = 1, srt = 90, pos = 4, col = 'white', labels = 'No', cex = 0.85)
text(x = k[2], y = 1, srt = 90, pos = 4, col = 'white', labels = 'Color Use', cex = 0.85)
text(x = k[2]-0.3, y = 14.5, srt = 90, pos = 4, col = 'white', labels = 'Color', cex = 0.85)
text(x = k[2], y = 14.5, srt = 90, pos = 4, col = 'white', labels = 'Use', cex = 0.85)

#legend('bottomright', legend = c('Color Use', 'No Color Use'), border = NA, fill = colorset[2,], cex = 0.9)
dev.off()


pdf('plots/Recognition_proportions.pdf', width = 2.6, height = 4.2)

ctab = table(scores.cdf$RECOG[scores.cdf$EXP == 'V1'], scores.cdf$GROUPbin[scores.cdf$EXP == 'V1'])

ctab = cbind(c(ctab[,1], 0, 0), c(0, 0, ctab[,2]))
k = barplot(ctab, border = 'white', col = t(colorset), 
	 bty = 'n', cex.axis = 1.1, cex.lab = 1.2, 
	 xlab = '', ylab = '', cex = 0.9, axes = FALSE, ylim = c(0, 22))

text(k[1], -3, 'Children', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)
text(k[2], -3, 'Adults', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)

mtext(2, text = 'Number of Participants', line = axisline, cex = axiscex)
axis(2, cex = 1.1, at = seq(0, 20, 5))
text(x = k[1]-0.3, y = 1, srt = 90, pos = 4, col = 'white', labels = 'Not', cex = 0.85)
text(x = k[1], y = 1, srt = 90, pos = 4, col = 'white', labels = 'Discovered', cex = 0.85)
text(x = k[1]-0.15, y = 12.2, srt = 90, pos = 4, col = 'white', labels = 'Discovered', cex = 0.85)
#text(x = k[1], y = 15.5, srt = 90, pos = 4, col = 'white', labels = 'Use', cex = 0.9)

text(x = k[2]-0.3, y = 1, srt = 90, pos = 4, col = 'white', labels = 'Not', cex = 0.85)
text(x = k[2], y = 1, srt = 90, pos = 4, col = 'white', labels = 'Discovered', cex = 0.85)
text(x = k[2]-0.15, y = 10.5, srt = 90, pos = 4, col = 'white', labels = 'Discovered', cex = 0.85)

#legend('bottomright', legend = c('Color Use', 'No Color Use'), border = NA, fill = colorset[2,], cex = 0.9)
dev.off()

pdf('plots/Report_proportions.pdf', width = 2.6, height = 4.2)

ctab = table(scores.cdf$CORRECT[scores.cdf$EXP == 'V1'], scores.cdf$GROUPbin[scores.cdf$EXP == 'V1'])

ctab = cbind(c(ctab[,1], 0, 0), c(0, 0, ctab[,2]))
k = barplot(ctab, border = 'white', col = t(colorset), 
	 bty = 'n', cex.axis = 1.1, cex.lab = 1.2, 
	 xlab = '', ylab = '', cex = 0.9, axes = FALSE, ylim = c(0, 22))

text(k[1], -3, 'Children', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)
text(k[2], -3, 'Adults', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)

mtext(2, text = 'Number of Participants', line = axisline, cex = axiscex)
axis(2, cex = 1.1, at = seq(0, 20, 5))
text(x = k[1]-0.3, y = 1, srt = 90, pos = 4, col = 'white', labels = 'Not', cex = 0.85)
text(x = k[1], y = 1, srt = 90, pos = 4, col = 'white', labels = 'Aware', cex = 0.85)
text(x = k[1]-0.15, y = 12.5, srt = 90, pos = 4, col = 'white', labels = 'Aware', cex = 0.85)
#text(x = k[1], y = 15.5, srt = 90, pos = 4, col = 'white', labels = 'Use', cex = 0.9)

text(x = k[2]-0.3, y = 1, srt = 90, pos = 4, col = 'white', labels = 'Not', cex = 0.85)
text(x = k[2], y = 1, srt = 90, pos = 4, col = 'white', labels = 'Aware', cex = 0.85)
text(x = k[2]-0.15, y = 13.5, srt = 90, pos = 4, col = 'white', labels = 'Aware', cex = 0.85)

#legend('bottomright', legend = c('Color Use', 'No Color Use'), border = NA, fill = colorset[2,], cex = 0.9)
dev.off()



################################################
## EXPERIMENT 2
################################################


## FIGURE 4
axiscex = 1.1
axisline = 2


pdf('plots/Errors_main_Exp2.pdf', width = 4, height = 4)
tmp = tapply(regular.cdf$E, list(regular.cdf$ID, regular.cdf$BLOCK, regular.cdf$GROUP, regular.cdf$EXP), mean)[,,,'V4']
cmeans = apply(tmp, c(2, 3), mean, na.rm = TRUE)
csds = apply(tmp, c(2, 3), std.error)

matplot(cmeans, type = 'o', col = colorset[,2], lty = 1, lwd = 2, pch = 16,
	 ylim = c(0, 50), ylab = '', xlab = '',   
	 bty = 'n', cex.axis = 1.1, cex.lab = 1.2, lab = c(9, 6, 5), 
	 xaxt = 'n')

se_bars(1:9, cmeans[,'KIDS'], csds[,'KIDS'], col = colorset[1,2])
se_bars(1:9, cmeans[,'YA'], csds[,'YA'], col = colorset[2,2])

axis(1, at = 1:9, labels = c(1:9), cex = 1.1)
text(1, -17, 'RAND', srt = 0, xpd = TRUE, pos = 3, cex = 0.8)
text(9, -17, 'INSTR', srt = 0, xpd = TRUE, pos = 3, cex = 0.8)
abline(v = 1.5, lty = 2, lwd = 1, col = 'darkgrey')
abline(v = 8.5, lty = 2, lwd = 1, col = 'darkgrey')
mtext(1, text = 'Block', line = 2.7, cex = 1.3)
mtext(2, text = 'Errors (%)', line = 2.5, cex = 1.3)

legend('topright', legend = c('Children', 'Adults'), 
	col = t(colorset[,2]), lwd = 2, pch = 16, cex = 0.8, bg = 'white', box.lwd = 0)

dev.off()

tmp = tapply(regular.cdf$E, list(regular.cdf$ID, regular.cdf$BLOCK, regular.cdf$GROUP, regular.cdf$EXP), mean)

tmp1 = apply(tmp[,7:8,,], c(1, 3, 4), mean)

t.test(tmp1[,'KIDS','V1'], tmp1[,'KIDS','V4'])

tmp2 = apply(tmp[,9,,], c(1, 2, 3), mean)

t.test(tmp2[,'KIDS','V1'], tmp2[,'KIDS','V4'])

mean(tmp2[,'KIDS','V4'], na.rm = TRUE)


pdf('plots/RTs_main_Exp2.pdf', width = 4, height = 4)
tmp = tapply(regular.cdf$RT, list(regular.cdf$ID, regular.cdf$BLOCK, regular.cdf$GROUP, regular.cdf$EXP), mean)[,,,'V4']
cmeans = apply(tmp, c(2, 3), mean, na.rm = TRUE)
csds = apply(tmp, c(2, 3), std.error)

matplot(cmeans, type = 'o', col = colorset[,2], lty = 1, lwd = 2, pch = 16,
	 ylim = c(400, 1450), ylab = '', xlab = '',   
	 bty = 'n', cex.axis = 1.1, cex.lab = 1.2, lab = c(9, 6, 5), 
	 xaxt = 'n')

se_bars(1:9, cmeans[,'KIDS'], csds[,'KIDS'], col = colorset[1,2])
se_bars(1:9, cmeans[,'YA'], csds[,'YA'], col = colorset[2,2])

axis(1, at = 1:9, labels = c(1:9), cex = 1.1)
text(1, 30, 'RAND', srt = 0, xpd = TRUE, pos = 3, cex = 0.8)
text(9, 30, 'INSTR', srt = 0, xpd = TRUE, pos = 3, cex = 0.8)
abline(v = 1.5, lty = 2, lwd = 1, col = 'darkgrey')
abline(v = 8.5, lty = 2, lwd = 1, col = 'darkgrey')
mtext(1, text = 'Block', line = 2.7, cex = 1.3)
mtext(2, text = 'RTs (ms)', line = 2.5, cex = 1.3)

legend('topright', legend = c('Children', 'Adults'), 
	col = t(colorset[2:1,2]), lwd = 2, pch = 16, cex = 0.8, bg = 'white', box.lwd = 0.5)

dev.off()

pdf('plots/Prelate_mean_Exp2.pdf', width = 2.6, height = 4)

prekey = tapply(conflicts.cdf$PREKEY_late, list(conflicts.cdf$ID, conflicts.cdf$EXP), mean)[,'V4']
group = tapply(conflicts.cdf$GROUP, list(conflicts.cdf$ID, conflicts.cdf$EXP), unique)[,'V4']


boxplot(prekey ~ group, bty = 'n', cex.axis = 1.1, cex.lab = 1.2, axes = FALSE, ylab = '', xlab = '', outline = FALSE, ylim = c(0, 30), add = FALSE, lwd = 2)

k = beeswarm(prekey~group, col = 'white', bg = colorset[,2], pch = 21, corral = 'wrap',
	 bty = 'n', cex.axis = 1.1, cex.lab = 1.2, 
	 xlab = '', ylab = '', labels = NA, cex = 0.9, axes = FALSE, lwd = 1, ylim = c(0, 30), add = TRUE)

text(1, -8.5, 'Children', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)
text(2, -8.5, 'Adults', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)

mtext(2, text = 'Premature Responses (%)', line = axisline, cex = axiscex)
axis(2, at = seq(0, 30, 10), labels = seq(0, 30, 10), cex = 1.1)

dev.off()

pdf('plots/NoGo_mean_Exp2.pdf', width = 2.6, height = 4)

prekey = tapply(conflicts.cdf$PREKEY_NoGo, list(conflicts.cdf$ID, conflicts.cdf$EXP), mean)[,'V4']
group = tapply(conflicts.cdf$GROUP, list(conflicts.cdf$ID, conflicts.cdf$EXP), unique)[,'V4']

boxplot(prekey ~ group, bty = 'n', cex.axis = 1.1, cex.lab = 1.2, axes = FALSE, ylab = '', xlab = '', outline = FALSE, ylim = c(0, 30), add = FALSE, lwd = 2)

k = beeswarm(prekey~group, col = 'white', bg = colorset[,2], pch = 21, corral = 'wrap',
	 bty = 'n', cex.axis = 1.1, cex.lab = 1.2, 
	 xlab = '', ylab = '', labels = NA, cex = 0.9, axes = FALSE, lwd = 1, ylim = c(0, 30), add = TRUE)

text(1, -8.5, 'Children', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)
text(2, -8.5, 'Adults', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)

mtext(2, text = 'NoGo Responses (%)', line = axisline, cex = axiscex)
axis(2, at = seq(0, 30, 10), labels = seq(0, 30, 10), cex = 1.1)

dev.off()


pdf('plots/WM_mean_Exp2.pdf', width = 2.6, height = 4)
boxplot(scores.cdf$WM[scores.cdf$EXP == 'V4'] ~ scores.cdf$GROUPbin[scores.cdf$EXP == 'V4'], bty = 'n', axes = FALSE, ylab = '', xlab = '', outline = FALSE, add = FALSE, lwd = 1.5, ylim =  c(0, 15))

k = beeswarm(scores.cdf$WM[scores.cdf$EXP == 'V4'] ~ scores.cdf$GROUPbin[scores.cdf$EXP == 'V4'], col = 'white', bg = colorset[,2], pch = 21, corral = 'wrap',
	 bty = 'n', cex.axis = 1.1, cex.lab = 1.2, 
	 xlab = '', ylab = '', labels = NA, cex = 0.9, axes = FALSE, lwd = 2,  add = TRUE)


text(1, -2.5, 'Children', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)
text(2, -2.5, 'Adults', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)

mtext(2, text = 'Working Memory Score', line = axisline, cex = axiscex)
axis(2, at = seq(0, 15, 3), labels = seq(0, 15, 3), cex = 1.1)
dev.off()

pdf('plots/STROOP_mean_Exp2.pdf', width = 2.6, height = 4)
boxplot(scores.cdf$STROOP[scores.cdf$EXP == 'V4'] ~ scores.cdf$GROUPbin[scores.cdf$EXP == 'V4'], bty = 'n', axes = FALSE, ylab = '', xlab = '', outline = FALSE, add = FALSE, lwd = 1.5, ylim =  c(-50, 150))

k = beeswarm(scores.cdf$STROOP[scores.cdf$EXP == 'V4'] ~ scores.cdf$GROUPbin[scores.cdf$EXP == 'V4'], col = 'white', bg = colorset[,2], pch = 21, corral = 'wrap',
	 bty = 'n', cex.axis = 1.1, cex.lab = 1.2, 
	 xlab = '', ylab = '', labels = NA, cex = 0.9, axes = FALSE, lwd = 2, add = TRUE)


text(1, -85, 'Children', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)
text(2, -85, 'Adults', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)

mtext(2, text = 'Neutral RT - Cong. RT (ms)', line = axisline, cex = axiscex)
axis(2, at = seq(-50, 150, 50), labels = seq(-50, 150, 50), cex = 1.1)
abline(h = 0)

dev.off()


###############
###### FIGURE 5
###############

axiscex = 1.1
axisline = 2


pdf('plots/Followed_main_Exp2.pdf', width = 4, height = 4)
tmp = tapply(ambiguous.cdf$COLOR, list(ambiguous.cdf$ID, ambiguous.cdf$BLOCK, ambiguous.cdf$GROUP, ambiguous.cdf$EXP), mean)[,,,'V4']


cmeans = apply(tmp, c(2, 3), mean, na.rm = TRUE)
csds = apply(tmp, c(2, 3), std.error)

matplot(cmeans, type = 'o', col = colorset[,2], lty = 1, lwd = 2, pch = 16, ylab = '', xlab = '',   
	 bty = 'n', cex.axis = 1.1, cex.lab = 1.2, lab = c(9, 6, 5), 
	 xaxt = 'n', ylim = c(20, 100))

se_bars(1:10, cmeans[,'KIDS'], csds[,'KIDS'], col = colorset[1,2])
se_bars(1:10, cmeans[,'YA'], csds[,'YA'], col = colorset[2,2])

axis(1, at = 1:9, labels = c(1:9), cex = 1.1)
text(1, -10, 'RAND', srt = 0, xpd = TRUE, pos = 3, cex = 0.8)
text(9, -10, 'INSTR', srt = 0, xpd = TRUE, pos = 3, cex = 0.8)
abline(v = 1.5, lty = 2, lwd = 1, col = 'darkgrey')
abline(v = 8.5, lty = 2, lwd = 1, col = 'darkgrey')
abline(h = 50, lty = 2, lwd = 1, col = 'darkgrey')
mtext(1, text = 'Block', line = 2.7, cex = 1.3)
mtext(2, text = 'Color Use (%)', line = 2.5, cex = 1.3)

legend('top', legend = c('Children', 'Adults'), 
	col = t(colorset[,2]), lwd = 2, pch = 16, cex = 0.8, bg = 'white', box.lwd = 0)
dev.off()



pdf('plots/Switch_aligned_Exp2.pdf', width = 3.5, height = 4)

tmp1 = tapply(switchpoint.cdf$COLOR, list(switchpoint.cdf$ID, switchpoint.cdf$BLOCK, switchpoint.cdf$GROUP, switchpoint.cdf$SWITCHED, switchpoint.cdf$EXP), mean)[,,,'NOSWITCHER','V4']
tmp2 = tapply(switchpoint.cdf$COLOR, list(switchpoint.cdf$ID, switchpoint.cdf$BLOCK, switchpoint.cdf$GROUP, switchpoint.cdf$SWITCHED, switchpoint.cdf$EXP), mean)[,,,'SWITCHER','V4']

cmeans = cbind(apply(tmp1[,,], c(2, 3), mean, na.rm = TRUE), apply(tmp2[,,], c(2, 3), mean, na.rm = TRUE))
csds = cbind(apply(tmp1[,,], c(2, 3), std.error), apply(tmp2[,,], c(2, 3), std.error))

matplot(cmeans, type = 'o', col = colorset, lty = 1, lwd = 2, pch = 16, ylab = '', xlab = '',   
	 bty = 'n', xaxt = 'n', cex.axis = 1.1, cex.lab = 1.2, lab = c(6, 6, 5), , ylim = c(20, 100))

se_bars(1:6, cmeans[,1], csds[,1], col = colorset[1,1])
se_bars(1:6, cmeans[,2], csds[,2], col = colorset[2,1])

se_bars(1:6, cmeans[,3], csds[,3], col = colorset[1,2])
se_bars(1:6, cmeans[,4], csds[,4], col = colorset[2,2])



axis(1, at = 1:6, labels = c('-3', '-2', '-1', '+1', '+2', '+3'), cex = 1.1)
abline(v = 3.5, lty = 2, lwd = 1, col = 'darkgrey')
abline(h = 50, lty = 2, lwd = 1, col = 'darkgrey')
mtext(1, text = 'Block rel. to switch', line = 2.7, cex = 1.3)
mtext(2, text = 'Color Use (%)', line = 2.5, cex = 1.3)

dev.off()


pdf('plots/Followed_mean_Exp2.pdf', width = 2.6, height = 4)

tmp = apply(tapply(ambiguous.cdf$COLOR, list(ambiguous.cdf$ID, ambiguous.cdf$BLOCK, ambiguous.cdf$EXP), mean)[,7:8,'V4'], c(1), mean)
tmp = tmp[!is.na(tmp)]

cgroup = ages[names(tmp)] > 12


boxplot(tmp ~ cgroup, bty = 'n', axes = FALSE, ylab = '', xlab = '', outline = FALSE, add = FALSE, lwd = 2, ylim = c(20, 100))

k = beeswarm(tmp ~ cgroup, col = 'white', bg = colorset[,2], pch = 21, corral = 'wrap', 
	 bty = 'n', cex.axis = 1.1, cex.lab = 1.2, 
	 xlab = '', ylab = '', labels = NA, cex = 0.9, axes = FALSE, lwd = 2, ylim = c(20, 100), add = TRUE)


text(1, 5, 'Children', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)
text(2, 5, 'Adults', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)

mtext(2, text = 'Color Use Blocks 7-8 (%)', line = axisline, cex = axiscex)
axis(2, cex = 1.1)
abline(h = cthresh*100, lty = 2)

dev.off()


pdf('plots/Followed_proportions_Exp2.pdf', width = 2.6, height = 4.2)

tmp = tapply(ambiguous.cdf$COLOR, list(regular.cdf$ID, regular.cdf$BLOCK, regular.cdf$GROUP, regular.cdf$EXP), mean)[,,,'V4']
tmp = apply(tmp[,7:8,], c(1, 3), mean, na.rm = TRUE) > (qbinom(0.95, 64, prob = 0.5)/64)*100
ctab = apply(tmp, 2, table)
ctab = cbind(c(ctab[,1], 0, 0), c(0, 0, ctab[,2]))
k = barplot(ctab, border = 'white', col = t(colorset), 
	 bty = 'n', cex.axis = 1.1, cex.lab = 1.2, 
	 xlab = '', ylab = '', cex = 0.9, axes = FALSE, ylim = c(0, 25))

text(k[1], -3, 'Children', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)
text(k[2], -3, 'Adults', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)

mtext(2, text = 'Number of Participants', line = axisline, cex = axiscex)
axis(2, cex = 1.1, at = seq(0, 20, 5))
text(x = k[1]-0.3, y = 1, srt = 90, pos = 4, col = 'white', labels = 'No', cex = 0.85)
text(x = k[1], y = 1, srt = 90, pos = 4, col = 'white', labels = 'Color Use', cex = 0.85)
text(x = k[1]-0.3, y = 17.5, srt = 90, pos = 4, col = 'white', labels = 'Color', cex = 0.85)
text(x = k[1], y = 17.5, srt = 90, pos = 4, col = 'white', labels = 'Use', cex = 0.85)

text(x = k[2]-0.3, y = 1, srt = 90, pos = 4, col = 'white', labels = 'No', cex = 0.85)
text(x = k[2], y = 1, srt = 90, pos = 4, col = 'white', labels = 'Color Use', cex = 0.85)
text(x = k[2]-0.3, y = 14.5, srt = 90, pos = 4, col = 'white', labels = 'Color', cex = 0.85)
text(x = k[2], y = 14.5, srt = 90, pos = 4, col = 'white', labels = 'Use', cex = 0.85)

#legend('bottomright', legend = c('Color Use', 'No Color Use'), border = NA, fill = colorset[2,], cex = 0.9)
dev.off()


pdf('plots/Recognition_proportions_Exp2.pdf', width = 2.6, height = 4.2)

ctab = table(scores.cdf$RECOG[scores.cdf$EXP == 'V4'], scores.cdf$GROUPbin[scores.cdf$EXP == 'V4'])

ctab = cbind(c(ctab[,1], 0, 0), c(0, 0, ctab[,2]))
k = barplot(ctab, border = 'white', col = t(colorset), 
	 bty = 'n', cex.axis = 1.1, cex.lab = 1.2, 
	 xlab = '', ylab = '', cex = 0.9, axes = FALSE, ylim = c(0, 25))

text(k[1], -3, 'Children', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)
text(k[2], -3, 'Adults', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)

mtext(2, text = 'Number of Participants', line = axisline, cex = axiscex)
axis(2, cex = 1.1, at = seq(0, 20, 5))
text(x = k[1]-0.3, y = 1, srt = 90, pos = 4, col = 'white', labels = 'Not', cex = 0.85)
text(x = k[1], y = 1, srt = 90, pos = 4, col = 'white', labels = 'Discovered', cex = 0.85)
text(x = k[1]-0.15, y = 13.5, srt = 90, pos = 4, col = 'white', labels = 'Discovered', cex = 0.85)
#text(x = k[1], y = 15.5, srt = 90, pos = 4, col = 'white', labels = 'Use', cex = 0.9)

text(x = k[2]-0.3, y = 1, srt = 90, pos = 4, col = 'white', labels = 'Not', cex = 0.85)
text(x = k[2], y = 1, srt = 90, pos = 4, col = 'white', labels = 'Discovered', cex = 0.85)
text(x = k[2]-0.15, y = 13.5, srt = 90, pos = 4, col = 'white', labels = 'Discovered', cex = 0.85)

#legend('bottomright', legend = c('Color Use', 'No Color Use'), border = NA, fill = colorset[2,], cex = 0.9)
dev.off()

pdf('plots/Used_proportions_Exp2.pdf', width = 2.6, height = 4.2)

ctab = table(scores.cdf$USED[scores.cdf$EXP == 'V4'], scores.cdf$GROUPbin[scores.cdf$EXP == 'V4'])

chisq.test(ctab)

ctab = cbind(c(ctab[,1], 0, 0), c(0, 0, ctab[,2]))
k = barplot(ctab, border = 'white', col = t(colorset), 
	 bty = 'n', cex.axis = 1.1, cex.lab = 1.2, 
	 xlab = '', ylab = '', cex = 0.9, axes = FALSE, ylim = c(0, 22))

text(k[1], -3, 'Children', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)
text(k[2], -3, 'Adults', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)

mtext(2, text = 'Number of Participants', line = axisline, cex = axiscex)
axis(2, cex = 1.1, at = seq(0, 20, 5))
text(x = k[1]-0.3, y = 1, srt = 90, pos = 4, col = 'white', labels = 'Not', cex = 0.85)
text(x = k[1], y = 1, srt = 90, pos = 4, col = 'white', labels = 'Aware', cex = 0.85)
text(x = k[1]-0.15, y = 12.5, srt = 90, pos = 4, col = 'white', labels = 'Aware', cex = 0.85)
#text(x = k[1], y = 15.5, srt = 90, pos = 4, col = 'white', labels = 'Use', cex = 0.9)

text(x = k[2]-0.3, y = 1, srt = 90, pos = 4, col = 'white', labels = 'Not', cex = 0.85)
text(x = k[2], y = 1, srt = 90, pos = 4, col = 'white', labels = 'Aware', cex = 0.85)
text(x = k[2]-0.15, y = 13.5, srt = 90, pos = 4, col = 'white', labels = 'Aware', cex = 0.85)

#legend('bottomright', legend = c('Color Use', 'No Color Use'), border = NA, fill = colorset[2,], cex = 0.9)
dev.off()


pdf('plots/Report_proportions_Exp2.pdf', width = 2.6, height = 4.2)

ctab = table(scores.cdf$CORRECT[scores.cdf$EXP == 'V4'], scores.cdf$GROUPbin[scores.cdf$EXP == 'V4'])

chisq.test(ctab)

ctab = cbind(c(ctab[,1], 0, 0), c(0, 0, ctab[,2]))
k = barplot(ctab, border = 'white', col = t(colorset), 
	 bty = 'n', cex.axis = 1.1, cex.lab = 1.2, 
	 xlab = '', ylab = '', cex = 0.9, axes = FALSE, ylim = c(0, 22))

text(k[1], -3, 'Children', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)
text(k[2], -3, 'Adults', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)

mtext(2, text = 'Number of Participants', line = axisline, cex = axiscex)
axis(2, cex = 1.1, at = seq(0, 20, 5))
text(x = k[1]-0.3, y = 1, srt = 90, pos = 4, col = 'white', labels = 'Not', cex = 0.85)
text(x = k[1], y = 1, srt = 90, pos = 4, col = 'white', labels = 'Aware', cex = 0.85)
text(x = k[1]-0.15, y = 12.5, srt = 90, pos = 4, col = 'white', labels = 'Aware', cex = 0.85)
#text(x = k[1], y = 15.5, srt = 90, pos = 4, col = 'white', labels = 'Use', cex = 0.9)

text(x = k[2]-0.3, y = 1, srt = 90, pos = 4, col = 'white', labels = 'Not', cex = 0.85)
text(x = k[2], y = 1, srt = 90, pos = 4, col = 'white', labels = 'Aware', cex = 0.85)
text(x = k[2]-0.15, y = 13.5, srt = 90, pos = 4, col = 'white', labels = 'Aware', cex = 0.85)

#legend('bottomright', legend = c('Color Use', 'No Color Use'), border = NA, fill = colorset[2,], cex = 0.9)
dev.off()


