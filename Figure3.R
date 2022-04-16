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
## FIGURE 3A-D: PLOTS EXPERIMENT 1 -- Covariate Tasks
################################################



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
