################################################
## SETUP: load packages, settings and data
################################################

setwd('~/code/pu2d_kids')

## load packages
packages = c('MASS', 'car', 'sfsmisc', 'RColorBrewer', 'gdata', 'beeswarm', 'lme4', 'emmeans', 'pwr')
pksload = unlist(lapply(packages, require, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))

if(sum(pksload) != length(packages)) {
	warning('SOME PACKAGES NOT LOADED')
}

#load some custom functions
source('aux_functions.R')

################################################
## GET DATA AND MAKE DATA FRAMES
################################################

# load data
datadir = paste(path.expand("~"), '/data/pu2d_kids/', sep = '')
source('get_data.R')

# prep data
source('prep_data.R')

expinfo = list(ages, ids, stroopids, expv, nids, kidids, nkids, kidids_v1, nkids_v1, kidids_v4, nkids_v4, adultids, nadults, adultids_v1, nadults_v1, adultids_v4, nadults_v4, cthresh)
names(expinfo) = c('ages', 'ids', 'stroopids', 'expv', 'nids', 'kidids', 'nkids', 'kidids_v1', 'nkids_v1', 'kidids_v4', 'nkids_v4', 'adultids', 'nadults', 'adultids_v1', 'nadults_v1', 'adultids_v4', 'nadults_v4', 'cthresh')
save(regular.cdf, conflicts.cdf, scores.cdf, ambiguous.cdf, switchpoint.cdf, expinfo, file = 'taskdata_Schucketal_2022_PLoSONE.RData')

# settings for lmer
lcctrl = lmerControl(optimizer=c('bobyqa'), optCtrl = list(maxfun = 500000))

# colors for plotting
# colorset = rbind(brewer.pal(6, 'Reds')[c(3, 6)], brewer.pal(6, 'Blues')[c(3,6)])

#colorset = matrix(hcl.colors(n = 20)[c(1,7, 18, 20)], 2, 2, byrow = TRUE)

colorset = matrix(c('#440154', '#8302A4', '#FDE725', '#C9BC46'), 2, 2, byrow = TRUE)

axiscex = 1.1
axisline = 2

################################################
## OVERVIEW: number of subjects in different experiments
################################################


# adults stats
nadults_v1
mean(tapply(DATA$age, DATA$id, mean)[adultids_v1])
range(tapply(DATA$age, DATA$id, mean)[adultids_v1])
sd(tapply(DATA$age, DATA$id, mean)[adultids_v1])
mean(tapply(DATA$sex, DATA$id, function(x) unique(x))[adultids_v1]-1)*nadults_v1

nadults_v4
mean(tapply(DATA$age, DATA$id, mean)[adultids_v4])
sd(tapply(DATA$age, DATA$id, mean)[adultids_v4])
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
sd(tapply(DATA$age, DATA$id, mean)[kidids_v1])
mean(tapply(DATA$sex, DATA$id, function(x) unique(x))[kidids_v1]-1)*nkids_v1

# kids
nkids_v4
mean(tapply(DATA$age, DATA$id, mean)[kidids_v4])#
sd(tapply(DATA$age, DATA$id, mean)[kidids_v4])
range(tapply(DATA$age, DATA$id, mean)[kidids_v4])
mean(tapply(DATA$sex, DATA$id, function(x) unique(x))[kidids_v4]-1)*nkids_v4

# kids
nkids
mean(tapply(DATA$age, DATA$id, mean)[kidids])
range(tapply(DATA$age, DATA$id, mean)[kidids])
mean(tapply(DATA$sex, DATA$id, function(x) unique(x))[kidids]-1)*nkids


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


pdf('plots/Fig2A_Errors_main.pdf', width = 4, height = 4)
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



pdf('plots/Fig2B_RTs_main.pdf', width = 4, height = 4)
tmp = tapply(regular.cdf$RT, list(regular.cdf$ID, regular.cdf$BLOCK, regular.cdf$GROUP, regular.cdf$EXP), mean)[,,,'V1']
cmeans = apply(tmp, c(2, 3), mean, na.rm = TRUE)
csds = apply(tmp, c(2, 3), std.error)

matplot(cmeans, type = 'o', col = colorset[,2], lty = 1, lwd = 2, pch = 16,
	 ylim = c(400, 1450), ylab = '', xlab = '',
	 bty = 'n', cex.axis = 1.1, cex.lab = 1.2, lab = c(10, 6, 5),
	 xaxt = 'n')

se_bars(1:10, cmeans[,'KIDS'], csds[,'KIDS'], col = colorset[1,2])
se_bars(1:10, cmeans[,'YA'], csds[,'YA'], col = colorset[2,2])

axis(1, at = 1:10, labels = c(1:10), cex = 1.1)
text(1, 30, 'RAND', srt = 0, xpd = TRUE, pos = 3, cex = 0.8)
text(9.5, 30, 'INSTR', srt = 0, xpd = TRUE, pos = 3, cex = 0.8)
abline(v = 1.5, lty = 2, lwd = 1, col = 'darkgrey')
abline(v = 8.5, lty = 2, lwd = 1, col = 'darkgrey')
mtext(1, text = 'Block', line = 2.7, cex = 1.3)
mtext(2, text = 'RTs (ms)', line = 2.5, cex = 1.3)

legend('topright', legend = c('Children', 'Adults'),
	col = t(colorset[1:2,2]), lwd = 2, pch = 16, cex = 0.8, bg = 'white', box.lwd = 0.5)

dev.off()






pdf('plots/Fig2C_Followed_main.pdf', width = 4, height = 4)
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


pdf('plots/Fig2D_Followed_mean.pdf', width = 2.6, height = 4)

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

pdf('plots/Fig2E_Followed_proportions.pdf', width = 2.6, height = 4.2)

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


pdf('plots/Fig2F_Recognition_proportions.pdf', width = 2.6, height = 4.2)

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

pdf('plots/Fig2G_Report_proportions.pdf', width = 2.6, height = 4.2)

ctab = table(scores.cdf$CORRECT[scores.cdf$EXP == 'V1'], scores.cdf$GROUPbin[scores.cdf$EXP == 'V1'])

ctab = cbind(c(ctab[,1], 0, 0), c(0, 0, ctab[,2]))
k = barplot(ctab, border = 'white', col = t(colorset),
	 bty = 'n', cex.axis = 1.1, cex.lab = 1.2,
	 xlab = '', ylab = '', cex = 0.9, axes = FALSE, ylim = c(0, 22))

text(k[1], -3, 'Children', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)
text(k[2], -3, 'Adults', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)

mtext(2, text = 'Number of Participants', line = axisline, cex = axiscex)
axis(2, cex = 1.1, at = seq(0, 20, 5))
text(x = k[1]-0.3, y = 1, srt = 90, pos = 4, col = 'white', labels = 'Wrong', cex = 0.85)
text(x = k[1], y = 1, srt = 90, pos = 4, col = 'white', labels = 'Recall', cex = 0.85)
text(x = k[1]-0.15, y = 9.5, srt = 90, pos = 4, col = 'white', labels = 'Correct Recall', cex = 0.85)
#text(x = k[1], y = 15.5, srt = 90, pos = 4, col = 'white', labels = 'Use', cex = 0.9)

text(x = k[2]-0.3, y = 1, srt = 90, pos = 4, col = 'white', labels = 'Wrong', cex = 0.85)
text(x = k[2], y = 1, srt = 90, pos = 4, col = 'white', labels = 'Recall', cex = 0.85)
text(x = k[2]-0.15, y = 7.5, srt = 90, pos = 4, col = 'white', labels = 'Correct Recall', cex = 0.85)

#legend('bottomright', legend = c('Color Use', 'No Color Use'), border = NA, fill = colorset[2,], cex = 0.9)
dev.off()




pdf('plots/Fig2H_Switch_aligned.pdf', width = 3.5, height = 4)

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




pdf('plots/Fig3A_Prelate_mean.pdf', width = 2.6, height = 4)

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

pdf('plots/Fig3B_NoGo_mean.pdf', width = 2.6, height = 4)

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


pdf('plots/Fig3C_WM_mean.pdf', width = 2.6, height = 4)
boxplot(scores.cdf$WM[scores.cdf$EXP == 'V1'] ~ scores.cdf$GROUPbin[scores.cdf$EXP == 'V1'], bty = 'n', axes = FALSE, ylab = '', xlab = '', outline = FALSE, add = FALSE, lwd = 1.5, ylim =  c(0, 15))

k = beeswarm(scores.cdf$WM[scores.cdf$EXP == 'V1'] ~ scores.cdf$GROUPbin[scores.cdf$EXP == 'V1'], col = 'white', bg = colorset[,2], pch = 21, corral = 'wrap',
	 bty = 'n', cex.axis = 1.1, cex.lab = 1.2,
	 xlab = '', ylab = '', labels = NA, cex = 0.9, axes = FALSE, lwd = 2,  add = TRUE)


text(1, -2.5, 'Children', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)
text(2, -2.5, 'Adults', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)

mtext(2, text = 'Working Memory Score', line = axisline, cex = axiscex)
axis(2, at = seq(0, 15, 3), labels = seq(0, 15, 3), cex = 1.1)
dev.off()

pdf('plots/Fig3D_STROOP_mean.pdf', width = 2.6, height = 4)
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




################################################
## ANALYSES EXPERIMENT 2
################################################

# ERRORS

clme = lmer(E ~ BLOCK*GROUP + (1+BLOCK|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V4'), control = lcctrl, REML = FALSE)
Anova(clme)


clme = lmer(E ~ GROUP + (1|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$BLOCK > 1 & regular.cdf$EXP == 'V4'), control = lcctrl, REML = FALSE)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

# diff between age groups last two blocks
clme = lmer(E ~ GROUP + (1|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V4' & regular.cdf$BLOCK > 6), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

# RTS

clme = lmer(RT ~ BLOCK*GROUP + (1+BLOCK|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V4'), control = lcctrl, REML = FALSE)
Anova(clme)

# mean diff between age groups
clme = lmer(RT ~ GROUP + (1|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V4'), control = lcctrl, REML = FALSE)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

# diff between age groups last two blocks
clme = lmer(RT ~ GROUP + (1|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V4' & regular.cdf$BLOCK > 6), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")



# EFFECT OF INSTRUCTION BLOCK
# ERRORS: test for interaction in pre/post instructions
clme = lmer(E ~ GROUP*COND + (1 + COND|ID), data = subset(regular.cdf, regular.cdf$BLOCK > 6 & regular.cdf$EXP == 'V4'), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ COND | GROUP), adjust = "tukey")
emmeans(clme, list(pairwise ~ GROUP | COND), adjust = "tukey")

# RTs: test for interaction in pre/post instructions
clme = lmer(RT ~ GROUP*COND + (1 + COND|ID), data = subset(regular.cdf, regular.cdf$BLOCK > 6 & regular.cdf$EXP == 'V4'), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ COND | GROUP), adjust = "tukey")


lcctrl = lmerControl(optimizer=c('bobyqa'), optCtrl = list(maxfun = 500000), calc.derivs = FALSE)

### Ambiguous trials
clme = lmer(COLOR ~ BLOCK*GROUP + (1 + BLOCK|ID), data = subset(ambiguous.cdf, ambiguous.cdf$COND == 'LEARN' & ambiguous.cdf$EXP == 'V4'), control = lcctrl, REML = FALSE)
Anova(clme)
clme = lmer(COLOR ~ GROUP + (1|ID), data = subset(ambiguous.cdf, ambiguous.cdf$COND == 'LEARN' & ambiguous.cdf$EXP == 'V4'), control = lcctrl, REML = FALSE)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

clme = lmer(COLOR ~ GROUP + (1|ID), data = subset(ambiguous.cdf, ambiguous.cdf$COND == 'LEARN' & ambiguous.cdf$BLOCK > 6 & ambiguous.cdf$EXP == 'V4'), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

clme = lmer(COLOR ~ BLOCK*GROUP + (1 + BLOCK|ID), data = subset(ambiguous.cdf, ambiguous.cdf$COND == 'LEARN'), control = lcctrl, REML = FALSE)
Anova(clme)
clme = lmer(COLOR ~ GROUP + (1|ID), data = subset(ambiguous.cdf, ambiguous.cdf$COND == 'LEARN'), control = lcctrl, REML = FALSE)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

clme = lmer(COLOR ~ GROUP + (1|ID), data = subset(ambiguous.cdf, ambiguous.cdf$COND == 'LEARN' & ambiguous.cdf$BLOCK > 6), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")


# proportion of subjects who switched
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


## recognition, used, correct reports

t.test(scores.cdf$RECOG[scores.cdf$EXP == 'V4' & scores.cdf$GROUP == 'CHN Exp.2'],
	scores.cdf$RECOG[scores.cdf$EXP == 'V4' & scores.cdf$GROUP == 'ADLT Exp.2'])

chisq.test(table(scores.cdf$RECOG[scores.cdf$EXP == 'V4'], scores.cdf$GROUPbin[scores.cdf$EXP == 'V1']))

t.test(scores.cdf$USED[scores.cdf$EXP == 'V4' & scores.cdf$GROUP == 'CHN Exp.2'],
	scores.cdf$USED[scores.cdf$EXP == 'V4' & scores.cdf$GROUP == 'ADLT Exp.2'])

chisq.test(table(scores.cdf$USED[scores.cdf$EXP == 'V4'], scores.cdf$GROUPbin[scores.cdf$EXP == 'V4']))

t.test(scores.cdf$CORRECT[scores.cdf$EXP == 'V4' & scores.cdf$GROUP == 'CHN Exp.2'],
	scores.cdf$CORRECT[scores.cdf$EXP == 'V4' & scores.cdf$GROUP == 'ADLT Exp.2'])
chisq.test(table(scores.cdf$CORRECT[scores.cdf$EXP == 'V4'], scores.cdf$GROUPbin[scores.cdf$EXP == 'V4']), simulate.p.value = TRUE)


### SWITCH POINT ALIGNED DATA


X = tapply(switchpoint.cdf$COLOR, list(switchpoint.cdf$ID, switchpoint.cdf$SWITCHED, switchpoint.cdf$BLOCK, switchpoint.cdf$GROUP, switchpoint.cdf$EXP), mean, na.rm = TRUE)[,,,,'V4']

ps_ya = sapply(1:5, function(x) t.test(X[,'SWITCHER',x,'YA'],X[,'SWITCHER',x+1,'YA'], paired = TRUE)$p.value)

ps_kids = sapply(1:5, function(x) t.test(X[,'SWITCHER',x,'KIDS'],X[,'SWITCHER',x+1,'KIDS'], paired = TRUE)$p.value)

p.adjust(ps_ya, method = 'holm')
p.adjust(ps_kids, method = 'holm')


cdf = subset(switchpoint.cdf, switchpoint.cdf$SWITCHED == 'SWITCHER' & switchpoint.cdf$EXP == 'V4' & !is.na(switchpoint.cdf$COLOR))

clme = lmer(COLOR ~ GROUP*AFTER + (1+AFTER|ID), data = cdf, control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ AFTER:GROUP), adjust = "tukey")


summary(subset(ambiguous.cdf, ambiguous.cdf$SWITCH == TRUE))

clme = lmer(COLOR ~ GROUP + (1|ID), data = subset(ambiguous.cdf, ambiguous.cdf$SWITCH == TRUE & ambiguous.cdf$EXP == 'V4'))
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")


clme = lmer(COLOR ~ GROUP*SWITCHED*AFTER + (1 + AFTER|ID), data = subset(switchpoint.cdf, switchpoint.cdf$EXP == 'V4'))
Anova(clme)

X = tapply(switchpoint.cdf$COLOR, list(switchpoint.cdf$ID, switchpoint.cdf$SWITCHED, switchpoint.cdf$AFTER, switchpoint.cdf$GROUP, switchpoint.cdf$EXP), mean, na.rm = TRUE)[,,,,'V4']


t.test(X[,'SWITCHER', 'BEFORE', 'YA'], X[,'SWITCHER', 'BEFORE', 'KIDS'])
t.test(X[,'SWITCHER', 'AFTER', 'YA'], X[,'SWITCHER', 'AFTER', 'KIDS'])

t.test(X[,'SWITCHER', 'AFTER', 'YA'] - X[,'SWITCHER', 'BEFORE', 'YA'], X[,'SWITCHER', 'AFTER', 'KIDS'] - X[,'SWITCHER', 'BEFORE', 'KIDS'])



adult_switch = switchpoints[which(ids %in% names(switchids) & ids %in% adultids_v4)]
kid_switch = switchpoints[which(ids %in% names(switchids) & ids %in% kidids_v4)]
t.test(kid_switch/2, adult_switch/2)



cmeans = apply(tmp, c(2, 3), mean, na.rm = TRUE)
csds = apply(tmp, c(2, 3), std.error)




################################################
## FIGURES EXPERIMENT 2
################################################


## FIGURE 4
axiscex = 1.1
axisline = 2


pdf('plots/Fig4A_Errors_main_Exp2.pdf', width = 4, height = 4)
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


pdf('plots/Fig4B_RTs_main_Exp2.pdf', width = 4, height = 4)
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
	col = t(colorset[2:1,2]), lwd = 2, pch = 16, cex = 0.8, bg = 'white', box.lwd = 0)

dev.off()


pdf('plots/Fig4C_Followed_main_Exp2.pdf', width = 4, height = 4)
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
	col = t(colorset[,2]), lwd = 2, pch = 16, cex = 0.8, bg = 'white', bty = 'n', box.lwd = 0)
dev.off()


pdf('plots/Fig4D_Followed_mean_Exp2.pdf', width = 2.6, height = 4)

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


pdf('plots/Fig4E_Followed_proportions_Exp2.pdf', width = 2.6, height = 4.2)

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
text(x = k[2]-0.3, y = 14.25, srt = 90, pos = 4, col = 'white', labels = 'Color', cex = 0.85)
text(x = k[2], y = 14.25, srt = 90, pos = 4, col = 'white', labels = 'Use', cex = 0.85)

#legend('bottomright', legend = c('Color Use', 'No Color Use'), border = NA, fill = colorset[2,], cex = 0.9)
dev.off()


pdf('plots/Fig4F_Recognition_proportions_Exp2.pdf', width = 2.6, height = 4.2)

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
text(x = k[2]-0.15, y = 13.25, srt = 90, pos = 4, col = 'white', labels = 'Discov.', cex = 0.85)
#text(x = k[2], y = 13.25, srt = 90, pos = 4, col = 'white', labels = 'covered', cex = 0.85)

#legend('bottomright', legend = c('Color Use', 'No Color Use'), border = NA, fill = colorset[2,], cex = 0.9)
dev.off()


pdf('plots/Fig4G_Report_proportions_Exp2.pdf', width = 2.6, height = 4.2)


ctab = table(scores.cdf$CORRECT[scores.cdf$EXP == 'V4'], scores.cdf$GROUPbin[scores.cdf$EXP == 'V4'])

chisq.test(ctab, simulate.p.value = TRUE)

ctab = cbind(c(ctab[,1], 0, 0), c(0, 0, ctab[,2]))
k = barplot(ctab, border = 'white', col = t(colorset),
	 bty = 'n', cex.axis = 1.1, cex.lab = 1.2,
	 xlab = '', ylab = '', cex = 0.9, axes = FALSE, ylim = c(0, 25))

text(k[1], -3, 'Children', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)
text(k[2], -3, 'Adults', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)

mtext(2, text = 'Number of Participants', line = axisline, cex = axiscex)
axis(2, cex = 1.1, at = seq(0, 20, 5))
text(x = k[1]-0.3, y = 1, srt = 90, pos = 4, col = 'white', labels = 'Wrong', cex = 0.85)
text(x = k[1], y = 1, srt = 90, pos = 4, col = 'white', labels = 'Recall', cex = 0.85)
text(x = k[1]-0.15, y = 12.5, srt = 90, pos = 4, col = 'white', labels = 'Correct Recall', cex = 0.85)
#text(x = k[1], y = 15.5, srt = 90, pos = 4, col = 'white', labels = 'Use', cex = 0.9)

text(x = k[2]-0.3, y = 1, srt = 90, pos = 4, col = 'white', labels = 'Wrong', cex = 0.85)
text(x = k[2], y = 1, srt = 90, pos = 4, col = 'white', labels = 'Recall', cex = 0.85)
text(x = k[2]-0.15, y = 7.5, srt = 90, pos = 4, col = 'white', labels = 'Correct Recall', cex = 0.85)

#legend('bottomright', legend = c('Color Use', 'No Color Use'), border = NA, fill = colorset[2,], cex = 0.9)
dev.off()



pdf('plots/Fig4H_Switch_aligned_Exp2.pdf', width = 3.5, height = 4)

tmp1 = tapply(switchpoint.cdf$COLOR, list(switchpoint.cdf$ID, switchpoint.cdf$BLOCK, switchpoint.cdf$GROUP, switchpoint.cdf$SWITCHED, switchpoint.cdf$EXP), mean)[,,,'NOSWITCHER','V4']
tmp2 = tapply(switchpoint.cdf$COLOR, list(switchpoint.cdf$ID, switchpoint.cdf$BLOCK, switchpoint.cdf$GROUP, switchpoint.cdf$SWITCHED, switchpoint.cdf$EXP), mean)[,,,'SWITCHER','V4']

cmeans = cbind(apply(tmp1[,4:9,], c(2, 3), mean, na.rm = TRUE), apply(tmp2[,4:9,], c(2, 3), mean, na.rm = TRUE))
csds = cbind(apply(tmp1[,4:9,], c(2, 3), std.error), apply(tmp2[,4:9,], c(2, 3), std.error))

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


pdf('plots/Fig5A_Prelate_mean_Exp2.pdf', width = 2.6, height = 4)

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

pdf('plots/Fig5B_NoGo_mean_Exp2.pdf', width = 2.6, height = 4)

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


pdf('plots/Fig5C_WM_mean_Exp2.pdf', width = 2.6, height = 4)
boxplot(scores.cdf$WM[scores.cdf$EXP == 'V4'] ~ scores.cdf$GROUPbin[scores.cdf$EXP == 'V4'], bty = 'n', axes = FALSE, ylab = '', xlab = '', outline = FALSE, add = FALSE, lwd = 1.5, ylim =  c(0, 15))

k = beeswarm(scores.cdf$WM[scores.cdf$EXP == 'V4'] ~ scores.cdf$GROUPbin[scores.cdf$EXP == 'V4'], col = 'white', bg = colorset[,2], pch = 21, corral = 'wrap',
	 bty = 'n', cex.axis = 1.1, cex.lab = 1.2,
	 xlab = '', ylab = '', labels = NA, cex = 0.9, axes = FALSE, lwd = 2,  add = TRUE)


text(1, -2.5, 'Children', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)
text(2, -2.5, 'Adults', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)

mtext(2, text = 'Working Memory Score', line = axisline, cex = axiscex)
axis(2, at = seq(0, 15, 3), labels = seq(0, 15, 3), cex = 1.1)
dev.off()

pdf('plots/Fig5D_STROOP_mean_Exp2.pdf', width = 2.6, height = 4)
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


###### JOINT ANALYSES
require(pwr)
nkids
nadults
pwr.chisq.test(w = 0.3, N = nkids + nadults, df = 1, sig.level = .05)
pwr.chisq.test(w = 0.2, N = nkids + nadults, df = 1, sig.level = .05)
pwr.chisq.test(w = 0.1, N = nkids + nadults, df = 1, sig.level = .05)

# Mean Errors last two blocks
clme = lmer(E ~ GROUP + (1|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$BLOCK %in% c(1:2)), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

clme = lmer(RT ~ GROUP + (1|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$BLOCK %in% c(1:2)), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

clme = lmer(PREKEY_late ~ GROUP + (1|ID), data = subset(conflicts.cdf, conflicts.cdf$COND == 'LEARN' & conflicts.cdf$BLOCK %in% c(1:2)), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

clme = lmer(PREKEY_NoGo ~ GROUP + (1|ID), data = subset(conflicts.cdf, conflicts.cdf$COND == 'LEARN' & conflicts.cdf$BLOCK %in% c(1:2)), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")


# STROOP
t.test(scores.cdf$STROOP[scores.cdf$GROUPbin == 'KIDS'],
	scores.cdf$STROOP[scores.cdf$GROUPbin == 'YA'])

t.test(scores.cdf$STROOP_INTER[scores.cdf$GROUPbin == 'KIDS'],
		scores.cdf$STROOP_INTER[scores.cdf$GROUPbin == 'YA'])

# WM
t.test(scores.cdf$WM[scores.cdf$GROUPbin == 'KIDS'],
			scores.cdf$WM[scores.cdf$GROUPbin == 'YA'])



# color use in last two blocks
clme = lmer(COLOR ~ GROUP + (1|ID), data = subset(ambiguous.cdf, ambiguous.cdf$COND == 'LEARN' & ambiguous.cdf$BLOCK > 6 ), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

### conflict measures


# proportion of subjects who switched
tmp = tapply(ambiguous.cdf$COLOR, list(regular.cdf$ID, regular.cdf$BLOCK, regular.cdf$GROUP, regular.cdf$EXP), mean)
tmp1 = apply(tmp[,7:8,,'V1'], c(1, 3), mean, na.rm = TRUE) > (qbinom(0.95, 64, prob = 0.5)/64)*100
tmp2 = apply(tmp[,7:8,,'V4'], c(1, 3), mean, na.rm = TRUE) > (qbinom(0.95, 32, prob = 0.5)/32)*100

tmp1[is.na(tmp1)] = tmp2[is.na(tmp1)]

ctab = apply(tmp1, 2, table)
chisq.test(ctab, simulate.p.value = TRUE)

## switchpoints
t.test(adult_switch, kid_switch)

## recognition, used, correct reports

t.test(scores.cdf$RECOG[scores.cdf$GROUPbin == 'KIDS'],
	scores.cdf$RECOG[scores.cdf$GROUPbin == 'YA'])

chisq.test(table(scores.cdf$RECOG, scores.cdf$GROUPbin))

t.test(scores.cdf$USED[scores.cdf$GROUPbin == 'KIDS'],
	scores.cdf$USED[scores.cdf$GROUPbin == 'YA'])

chisq.test(table(scores.cdf$USED, scores.cdf$GROUPbin))

t.test(scores.cdf$CORRECT[scores.cdf$GROUPbin == 'KIDS'],
	scores.cdf$CORRECT[scores.cdf$GROUPbin == 'YA'])
chisq.test(table(scores.cdf$CORRECT, scores.cdf$GROUPbin), simulate.p.value = TRUE)

### make plot
cdf = data.frame(P = c(as.matrix(scores.cdf[,c(20:25, 28:29, 32:33)])), ID = rep(scores.cdf$ID, 10), AGEGROUP = rep(scores.cdf$GROUPbin, 10), DV = as.factor(rep(1:10, each = dim(scores.cdf)[1])),
	CAT = as.factor(c(rep(1, each = dim(scores.cdf)[1]*6), rep(2, each = dim(scores.cdf)[1]*4))))

clme = lmer(P~AGEGROUP*CAT + (1 + CAT|ID), data = cdf)
summary(clme)
Anova(clme)

summary(scores.cdf[,c(20:25, 28:29, 32:33)])


ctab = aggregate(scores.cdf[,c(20:25, 28:29, 32:33)], list(scores.cdf$GROUPbin), mean, na.rm = TRUE)
ctab_ses = aggregate(scores.cdf[,c(20:25, 28:29, 32:33)], list(scores.cdf$GROUPbin), std.error)


pdf('plots/Fig6A_Overview.pdf', width = 4.25, height = 4)
par(mar=c(5.1,4.1,4.1,0.5))

k = barplot(as.matrix(ctab[,2:11]), beside = TRUE, col = colorset[,2], ylim = c(-1, 1), border = NA, names.arg = rep('', 10),
space = c(rep(c(0.75, 0.1), 6), 2, 0.1, rep(c(0.75, 0.1), 3)), cex.axis = 1)
abline(h = 0, lty = 1)
abline(v = k[2,6]+1.5, lty = 2)
se_bars(k, as.matrix(ctab[,2:11]), as.matrix(ctab_ses[,2:11]),
	colorset[,1], horiz = FALSE)

mtext(2, text = 'z Score ', line = 2.7, cex = 1.25)

text(k[1,1]+1.1, - 1,'Working memory', cex = 0.9, xpd = TRUE, pos = 2, srt = 45)
text(k[1,2]+1.1, - 1,'Stroop costs', cex = 0.9, xpd = TRUE, pos = 2, srt = 45)
text(k[1,3]+1.1, - 1,'Errors', cex = 0.9, xpd = TRUE, pos = 2, srt = 45)
text(k[1,4]+1.1, - 1,'RTs', cex = 0.9, xpd = TRUE, pos = 2, srt = 45)
text(k[1,5]+1.1, - 1,'False alarms [delay]', cex = 0.9, xpd = TRUE, pos = 2, srt = 45)
text(k[1,6]+1.1, - 1,'False alarms [noGo]', cex = 0.9, xpd = TRUE, pos = 2, srt = 45)
text(k[1,7]+1.1, - 1,'Color use', cex = 0.9, xpd = TRUE, pos = 2, srt = 45)
text(k[1,8]+1.1, - 1,'% Switched', cex = 0.9, xpd = TRUE, pos = 2, srt = 45)
text(k[1,9]+1.1, - 1,'% Rule report', cex = 0.9, xpd = TRUE, pos = 2, srt = 45)
text(k[1,10] + 1.1, - 1, '% Usage report', cex = 0.9, xpd = TRUE, pos = 2, srt = 45)

dev.off()

# PREDICTING COLOR USE

# test for colinearity
cor.test(scores.cdf.no.na$PRELATE, scores.cdf.no.na$PRENO)
cor.test(scores.cdf.no.na$E, scores.cdf.no.na$RT)

minmod <- lm(FOLL_logit ~ GROUPbin, data=scores.cdf.no.na) #AMBIG can also be included
summary(minmod)
AIC(minmod)

fullmod = lm(FOLL_logit ~ GROUPbin*(STROOPz+TASKz+PREz+WMz), data=scores.cdf.no.na) #AMBIG can also be included
summary(fullmod)
AIC(fullmod)

fullmod_robust = rlm(FOLL_logit ~ GROUPbin*(STROOPz+TASKz+PREz+WMz), data=scores.cdf.no.na) #AMBIG can also be included
summary(fullmod_robust)
f.robftest(fullmod_robust, var = 'GROUPbinYA:TASKz')

anova(fullmod, minmod)


# backwards
reducedmod = stepAIC(fullmod, scope=list(upper = ~GROUPbin*(STROOPz+WMz+TASKz+PREz), lower = ~1), direction = "both")
summary(reducedmod)
AIC(reducedmod)

Anova(tmp)


pdf('plots/Fig6B_Predict.pdf', width = 8, height = 2.5)
layout(matrix(1:3, 1, 3))

par(mar=c(4,4,1,1))

X = predict(fullmod)
Y = scores.cdf.no.na$FOLL_logit*1
lm_kids = lm(Y[scores.cdf.no.na$GROUPbin == 'KIDS']~X[scores.cdf.no.na$GROUPbin == 'KIDS'])
lm_ya = lm(Y[scores.cdf.no.na$GROUPbin == 'YA']~X[scores.cdf.no.na$GROUPbin == 'YA'])
summary(lm_kids)
summary(lm_ya)

#ccols = c(brewer.pal(6, 'Reds')[6], brewer.pal(6, 'Blues')[6])[as.numeric(scores.cdf.no.na$GROUPbin)]
ccols = c(colorset[,2])[as.numeric(scores.cdf.no.na$GROUPbin)]
plot(X, Y,type = 'p', col = ccols,
	lwd = 2, xlim = c(-1, 5), ylim = c(-1, 5), ylab = '', xlab = '',
	bty = 'n', cex.axis = 1.1, cex.lab = 1.2,
	xaxt = 'n', yaxt = 'n', pch = 16)
abline(lm_ya, lwd = 2, lty = 1, col = colorset[1,2])
abline(lm_kids, lwd = 2, lty = 2, col = colorset[2,2])

axis(1, at = seq(-1, 5, length.out = 5), cex.axis = 1.3)
mtext(1, text = 'Predicted Color Use ', line = 2.7, cex = 1.2)

axis(2, at = seq(-1, 5, length.out = 5), cex.axis = 1.3)
mtext(2, text = 'Color Use (logit)', line = 2.7, cex = 1.2)


lm_kids = lm(scores.cdf.no.na$FOLL_logit[scores.cdf.no.na$GROUPbin == 'KIDS']~scores.cdf.no.na$TASKz[scores.cdf.no.na$GROUPbin == 'KIDS'])
lm_ya = lm(scores.cdf.no.na$FOLL_logit[scores.cdf.no.na$GROUPbin == 'YA']~scores.cdf.no.na$TASKz[scores.cdf.no.na$GROUPbin == 'YA'])

sqrt(summary(lm_kids)$r.squared)
sqrt(summary(lm_ya)$r.squared)



plot(scores.cdf.no.na$TASKz[scores.cdf.no.na$GROUPbin == 'YA'], scores.cdf.no.na$FOLL_logit[scores.cdf.no.na$GROUPbin == 'YA']*1,
	type = 'p', col = colorset[2,2],
	lwd = 2, xlim = round(range(scores.cdf.no.na$TASKz), 1), ylim = c(-1, 5), ylab = '', xlab = '',
	bty = 'n', cex.axis = 1.1, cex.lab = 1.2,
	xaxt = 'n', yaxt = 'n', pch = 16)
points(scores.cdf.no.na$TASKz[scores.cdf.no.na$GROUPbin == 'KIDS'], scores.cdf.no.na$FOLL_logit[scores.cdf.no.na$GROUPbin == 'KIDS']*1,
		type = 'p', col = colorset[1,2],
		lwd = 2, pch = 16)
abline(lm_ya, lwd = 2, col = colorset[2,2])
abline(lm_kids, lwd = 2, col = colorset[1,2])

axis(1, at = seq(round(range(scores.cdf.no.na$TASKz)[1], 1), round(range(scores.cdf.no.na$TASKz)[2], 1), length.out = 5), cex.axis = 1.3)
mtext(1, text = 'Task Performance (z) ', line = 2.7, cex = 1.2)

axis(2, at = seq(-1, 5, length.out = 5), cex.axis = 1.3)
mtext(2, text = 'Color Use (logit)', line = 2.7, cex = 1.2)



lm_kids = lm(scores.cdf.no.na$FOLL_logit[scores.cdf.no.na$GROUPbin == 'KIDS']~scores.cdf.no.na$STROOPz[scores.cdf.no.na$GROUPbin == 'KIDS'])
lm_ya = lm(scores.cdf.no.na$FOLL_logit[scores.cdf.no.na$GROUPbin == 'YA']~scores.cdf.no.na$STROOPz[scores.cdf.no.na$GROUPbin == 'YA'])

sqrt(summary(lm_kids)$r.squared)
sqrt(summary(lm_ya)$r.squared)


plot(scores.cdf.no.na$STROOPz[scores.cdf.no.na$GROUPbin == 'YA'], scores.cdf.no.na$FOLL_logit[scores.cdf.no.na$GROUPbin == 'YA']*1,
	type = 'p', col = colorset[2,2],
	lwd = 2, xlim = round(range(scores.cdf.no.na$STROOPz), 1), ylim = c(-1, 5), ylab = '', xlab = '',
	bty = 'n', cex.axis = 1.1, cex.lab = 1.2,
	xaxt = 'n', yaxt = 'n', pch = 16)
points(scores.cdf.no.na$STROOPz[scores.cdf.no.na$GROUPbin == 'KIDS'], scores.cdf.no.na$FOLL_logit[scores.cdf.no.na$GROUPbin == 'KIDS']*1,
		type = 'p', col = colorset[1,2],
		lwd = 2, pch = 16)
abline(lm_ya, lwd = 2, col = colorset[2,2])
abline(lm_kids, lwd = 2, col = colorset[1,2])

axis(1, at = seq(round(range(scores.cdf.no.na$STROOPz)[1], 1), round(range(scores.cdf.no.na$STROOPz)[2], 1), length.out = 5), cex.axis = 1.3)
mtext(1, text = 'Stroop Effect (z) ', line = 2.7, cex = 1.2)

axis(2, at = seq(-1, 5, length.out = 5), cex.axis = 1.3)
mtext(2, text = 'Color Use (logit)', line = 2.7, cex = 1.2)


dev.off()


table(is.na(scores.cdf$RECOG), scores.cdf$GROUP)
table(is.na(scores.cdf$STROOP), scores.cdf$GROUP)
table(is.na(scores.cdf$WM), scores.cdf$GROUP)


tapply(scores.cdf.no.na$FOLL_logit, scores.cdf.no.na$GROUPbin, mean)
tapply(scores.cdf.no.na$STROOPz, scores.cdf.no.na$GROUPbin, mean)
tapply(scores.cdf.no.na$PREz, scores.cdf.no.na$GROUPbin, mean)
tapply(scores.cdf.no.na$TASKz, scores.cdf.no.na$GROUPbin, mean)

cor.test(scores.cdf.no.na$FOLL_logit[scores.cdf.no.na$GROUPbin == 'YA']*1, scores.cdf.no.na$STROOPz[scores.cdf.no.na$GROUPbin == 'YA'], method = 'spearman')
cor.test(scores.cdf.no.na$FOLL_logit[scores.cdf.no.na$GROUPbin == 'KIDS']*1, scores.cdf.no.na$STROOPz[scores.cdf.no.na$GROUPbin == 'KIDS'], method = 'spearman')

cor.test(scores.cdf.no.na$FOLL_logit[scores.cdf.no.na$GROUPbin == 'YA']*1, scores.cdf.no.na$PREz[scores.cdf.no.na$GROUPbin == 'YA'])
cor.test(scores.cdf.no.na$FOLL_logit[scores.cdf.no.na$GROUPbin == 'KIDS']*1, scores.cdf.no.na$PREz[scores.cdf.no.na$GROUPbin == 'KIDS'])

cor.test(scores.cdf.no.na$FOLL_logit[scores.cdf.no.na$GROUPbin == 'YA']*1, scores.cdf.no.na$WMz[scores.cdf.no.na$GROUPbin == 'YA'])
cor.test(scores.cdf.no.na$FOLL_logit[scores.cdf.no.na$GROUPbin == 'KIDS']*1, scores.cdf.no.na$WMz[scores.cdf.no.na$GROUPbin == 'KIDS'])

cor.test(scores.cdf.no.na$FOLL_logit[scores.cdf.no.na$GROUPbin == 'YA']*1, scores.cdf.no.na$TASKz[scores.cdf.no.na$GROUPbin == 'YA'])
cor.test(scores.cdf.no.na$FOLL_logit[scores.cdf.no.na$GROUPbin == 'KIDS']*1, scores.cdf.no.na$TASKz[scores.cdf.no.na$GROUPbin == 'KIDS'])

cor.test(scores.cdf.no.na$FOLL_logit[scores.cdf.no.na$GROUPbin == 'YA']*1, scores.cdf.no.na$TASKz[scores.cdf.no.na$GROUPbin == 'YA'], method = 'pearson')
cor.test(scores.cdf.no.na$FOLL_logit[scores.cdf.no.na$GROUPbin == 'YA']*1, scores.cdf.no.na$TASKz[scores.cdf.no.na$GROUPbin == 'YA'], method = 'spearman')
cor.test(scores.cdf.no.na$FOLL_logit[scores.cdf.no.na$GROUPbin == 'KIDS']*1, scores.cdf.no.na$TASKz[scores.cdf.no.na$GROUPbin == 'KIDS'], method = 'pearson')
cor.test(scores.cdf.no.na$FOLL_logit[scores.cdf.no.na$GROUPbin == 'KIDS']*1, scores.cdf.no.na$TASKz[scores.cdf.no.na$GROUPbin == 'KIDS'], method = 'spearman')

scores.cdf.no.na$FOLLbin*1

binmod <- glm(cbind(FOLL_hits, FOLL_weights - FOLL_hits) ~ GROUPbin*(STROOPz+WMz+TASKz+PREz), data=scores.cdf.no.na, family = binomial) #AMBIG can also be included
summary(binmod)


# backwards
tmp = stepAIC(binmod, scope=list(upper = ~GROUPbin*(STROOPz+WMz+TASKz+PREz), lower = ~GROUPbin), direction = "both")
summary(tmp)




scores_cond.df = data.frame(
	Z = c(scores.df$WMz, scores.df$STROOPz, scores.df$Ez, scores.df$RTz, scores.df$PRELATEz, scores.df$PRENOz, scores.df$CONGz, scores.df$AMBIGz, scores.df$FOLLz, scores.df$FOLLbinz, scores.df$RECOGz, scores.df$USEDz),
	ID = rep(scores.df$ID, 12),
	GROUP = rep(scores.df$GROUPbin, 12),
	COND = as.factor(rep(colnames(ctab)[2:13], each = nids)))

scores_cond.df = subset(scores_cond.df, scores_cond.df$COND != 'AMBIGz' & scores_cond.df$COND != 'CONGz')

clme = lmer(Z ~ COND*GROUP + (1|ID), data = scores_cond.df)
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP|COND), adjust = "tukey")
