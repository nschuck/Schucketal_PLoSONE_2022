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
## Figure 6 A
################################################


ctab = aggregate(scores.cdf[,c(20:25, 28:29, 32:33)], list(scores.cdf$GROUPbin), mean, na.rm = TRUE)
ctab_ses = aggregate(scores.cdf[,c(20:25, 28:29, 32:33)], list(scores.cdf$GROUPbin), std.error)

scores.cdf.no.na = na.omit(scores.cdf)

fullmod = lm(FOLL_logit ~ GROUPbin*(STROOPz+TASKz+PREz+WMz), data=scores.cdf.no.na) #AMBIG can also be included


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

################################################
## Figure 6 B
################################################


pdf('plots/Fig6B_Predict.pdf', width = 8, height = 2.5)

layout(matrix(1:3, 1, 3))
par(mar=c(4,4,1,1))

X = predict(fullmod)
Y = scores.cdf.no.na$FOLL_logit*1
lm_kids = lm(Y[scores.cdf.no.na$GROUPbin == 'KIDS']~X[scores.cdf.no.na$GROUPbin == 'KIDS'])
lm_ya = lm(Y[scores.cdf.no.na$GROUPbin == 'YA']~X[scores.cdf.no.na$GROUPbin == 'YA'])

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
