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

# load data
load('taskdata_Schucketal_2022_PLoSONE.RData')


# settings for lmer
lcctrl = lmerControl(optimizer=c('bobyqa'), optCtrl = list(maxfun = 500000))

# colors for plotting
colorset = matrix(c('#440154', '#8302A4', '#FDE725', '#C9BC46'), 2, 2, byrow = TRUE)

# settings for plotting
axiscex = 1.1
axisline = 2


################################################
## ANALYSES EXPERIMENT 1: Standard trials
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


################################################
## PLOTS EXPERIMENT 1 (Fig 2A-B): Standard trials
################################################


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


################################################
## ANALYSES EXPERIMENT 1: Spontaneous strategy discovery and switch
################################################


##### COLOR USE

clme = lmer(COLOR ~ BLOCK*GROUP + (1+BLOCK|ID), data = subset(ambiguous.cdf, ambiguous.cdf$COND == 'LEARN' & ambiguous.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
Anova(clme)
clme = lmer(COLOR ~ GROUP + (1|ID), data = subset(ambiguous.cdf, ambiguous.cdf$COND == 'LEARN' & ambiguous.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

# only last 2 blocks
clme = lmer(COLOR ~ GROUP + (1|ID), data = subset(ambiguous.cdf, ambiguous.cdf$COND == 'LEARN' & ambiguous.cdf$BLOCK > 6 & ambiguous.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

# proportion of subjects who switched
tmp = tapply(ambiguous.cdf$COLOR, list(regular.cdf$ID, regular.cdf$BLOCK, regular.cdf$GROUP, regular.cdf$EXP), mean)[,,,'V1']
tmp = apply(tmp[,7:8,], c(1, 3), mean, na.rm = TRUE) > (qbinom(0.95, 64, prob = 0.5)/64)*100
ctab = apply(tmp, 2, table)
chisq.test(ctab, simulate.p.value = TRUE)

# other thresholds (75%)
tmp = tapply(ambiguous.cdf$COLOR, list(regular.cdf$ID, regular.cdf$BLOCK, regular.cdf$GROUP, regular.cdf$EXP), mean)[,,,'V1']
tmp = apply(tmp[,7:8,], c(1, 3), mean, na.rm = TRUE) > 75
ctab = apply(tmp, 2, table)
chisq.test(ctab, simulate.p.value = TRUE)

# other thresholds (50%)
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


# pairwise tests between adjacent blocks
X = tapply(switchpoint.cdf$COLOR, list(switchpoint.cdf$ID, switchpoint.cdf$SWITCHED, switchpoint.cdf$BLOCK, switchpoint.cdf$GROUP, switchpoint.cdf$EXP), mean, na.rm = TRUE)[,,,,'V1']
ps_ya = sapply(1:10, function(x) t.test(X[,'SWITCHER',x,'YA'],X[,'SWITCHER',x+1,'YA'], paired = TRUE)$p.value)
ps_kids = sapply(1:10, function(x) t.test(X[,'SWITCHER',x,'KIDS'],X[,'SWITCHER',x+1,'KIDS'], paired = TRUE)$p.value)

# ps in young adluts before
p.adjust(ps_ya[1:5], method = 'holm')
# ps in young adluts after
p.adjust(ps_ya[7:10], method = 'holm')

# p in young adluts at switch
ps_ya[6]


# ps in young adluts before
p.adjust(ps_kids[1:5], method = 'holm')
# ps in young adluts after
p.adjust(ps_kids[7:10], method = 'holm')

# p in young adluts at switch
ps_kids[6]


# interaction twime and agegroup
clme = lmer(COLOR ~ GROUP*SWITCHED*AFTER + (1 + AFTER|ID), data = subset(switchpoint.cdf, switchpoint.cdf$EXP == 'V1'))
Anova(clme)


# age group diff before
clme = lmer(COLOR ~ GROUP + (1|ID), data = subset(ambiguous.cdf, ambiguous.cdf$SWITCH == FALSE & ambiguous.cdf$EXP == 'V1'))
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

# age group diff after
clme = lmer(COLOR ~ GROUP + (1|ID), data = subset(ambiguous.cdf, ambiguous.cdf$SWITCH == TRUE & ambiguous.cdf$EXP == 'V1'))
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")


################################################
## PLOTS EXPERIMENT 1 (Fig2C-H): Spontaneous strategy discovery and switch
################################################




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
cgroup = expinfo$ages[names(tmp)] > 12


boxplot(tmp ~ cgroup, bty = 'n', axes = FALSE, ylab = '', xlab = '', outline = FALSE, add = FALSE, lwd = 2, ylim = c(20, 100))

k = beeswarm(tmp ~ cgroup, col = 'white', bg = colorset[,2], pch = 21, corral = 'wrap',
	 bty = 'n', cex.axis = 1.1, cex.lab = 1.2,
	 xlab = '', ylab = '', labels = NA, cex = 0.9, axes = FALSE, lwd = 2, ylim = c(20, 100), add = TRUE)


text(1, 5, 'Children', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)
text(2, 5, 'Adults', srt = 0, xpd = TRUE, pos = 3, cex = 1.05)

mtext(2, text = 'Color Use Blocks 7-8 (%)', line = axisline, cex = axiscex)
axis(2, cex = 1.1)
abline(h = expinfo$cthresh*100, lty = 2)

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
