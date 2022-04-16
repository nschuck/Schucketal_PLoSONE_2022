packages = c('RColorBrewer',  'beeswarm')
pksload = unlist(lapply(packages, require, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))
if(sum(pksload) != length(packages)) {
	warning('SOME PACKAGES NOT LOADED')
}


#load some custom functions
source('aux_functions.R')
# load data
load('taskdata_Schucketal_2022_PLoSONE.RData')
# colors for plotting
colorset = matrix(c('#440154', '#8302A4', '#FDE725', '#C9BC46'), 2, 2, byrow = TRUE)
# settings for plotting
axiscex = 1.1
axisline = 2

################################################
## FIGURE 4A-B (PLOTS EXPERIMENT 2): Standard trials
################################################


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

################################################
## FIGURE 4C-H: PLOTS EXPERIMENT 2: Spontaneous strategy discovery and switch
################################################


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
cgroup = expinfo$ages[names(tmp)] > 12
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
text(x = k[2]-0.3, y = 1, srt = 90, pos = 4, col = 'white', labels = 'Not', cex = 0.85)
text(x = k[2], y = 1, srt = 90, pos = 4, col = 'white', labels = 'Discovered', cex = 0.85)
text(x = k[2]-0.15, y = 13.25, srt = 90, pos = 4, col = 'white', labels = 'Discov.', cex = 0.85)

dev.off()


pdf('plots/Fig4G_Report_proportions_Exp2.pdf', width = 2.6, height = 4.2)

ctab = table(scores.cdf$CORRECT[scores.cdf$EXP == 'V4'], scores.cdf$GROUPbin[scores.cdf$EXP == 'V4'])
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
text(x = k[2]-0.3, y = 1, srt = 90, pos = 4, col = 'white', labels = 'Wrong', cex = 0.85)
text(x = k[2], y = 1, srt = 90, pos = 4, col = 'white', labels = 'Recall', cex = 0.85)
text(x = k[2]-0.15, y = 7.5, srt = 90, pos = 4, col = 'white', labels = 'Correct Recall', cex = 0.85)

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
