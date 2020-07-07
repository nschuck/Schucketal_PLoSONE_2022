source('/gdrive/workbench/R/tools/misc/gen_functions.R')

# exclude participants:
# FIRST those that don't switch under instruction conditions
errthresh = 1 - qbinom(0.95, 160, prob = 0.5)/160
follthresh1 = qbinom(0.95, 64, prob = 0.5)/64
follthresh2 = qbinom(0.95, 32, prob = 0.5)/32


tmp1 = tapply(DATA$followed, list(DATA$id, DATA$block, DATA$cond, DATA$taskV, DATA$respkey %in% c(77, 88, 188)), mean, na.rm = TRUE)[,9:10,2,'V1','TRUE']
exclids1 = which(rowMeans(tmp1) < follthresh1)

tmp2 = tapply(DATA$followed, list(DATA$id, DATA$block, DATA$cond, DATA$taskV, DATA$respkey %in% c(77, 88, 188)), mean, na.rm = TRUE)[,9,2,'V4','TRUE']
exclids2 = which(tmp2 < follthresh2)

groupids = tapply(DATA$id, DATA$agegroup, unique)

n_kids1 = sum(names(exclids1) %in% groupids$KIDS)
n_ya1 = sum(names(exclids1) %in% groupids$YA)

n_kids2 = sum(names(exclids2) %in% groupids$KIDS)
n_ya2 = sum(names(exclids2) %in% groupids$YA)

exclidsall = unique(names(c(exclids1, exclids2)))

DATA = subset(DATA, !(as.double(DATA$id) %in% as.double(exclidsall)))
DATA = subset(DATA, DATA$respkey %in% c(0, 77, 88, 188))
LEARN = subset(DATA, DATA$error==0)

#### get all ids in data set and clean up


ids = as.integer(levels(as.factor(LEARN$id)))
nids = length(ids)
kidids = tapply(DATA$id, DATA$agegroup, unique)$KIDS
nkids = length(kidids)
kidids_v1 = tapply(DATA$id, list(DATA$taskV, DATA$agegroup), unique)[['V1', 'KIDS']]
nkids_v1 = length(kidids_v1)
kidids_v4 = tapply(DATA$id, list(DATA$taskV, DATA$agegroup), unique)[['V4', 'KIDS']]
nkids_v4 = length(kidids_v4)
adultids = tapply(DATA$id, DATA$agegroup, unique)$YA
nadults = length(adultids)
adultids_v1 = tapply(DATA$id, list(DATA$taskV, DATA$agegroup), unique)[['V1', 'YA']]
nadults_v1 = length(adultids_v1)
adultids_v4 = tapply(DATA$id, list(DATA$taskV, DATA$agegroup), unique)[['V4', 'YA']]
nadults_v4 = length(adultids_v4)

trialspercond = apply(tapply(DATA$error, list(DATA$id, DATA$block, DATA$cond), length), c(1, 3), mean, na.rm = TRUE)

Qdata = subset(Qdata, Qdata$id %in% ids)
Sdata = subset(Sdata, Sdata$id %in% ids)
Sdata2 = subset(Sdata2, Sdata2$id %in% ids)
ages = tapply(DATA$age, DATA$id, mean)
expv = tapply(DATA$taskV, DATA$id, unique)

kidids2 = c(kidids, kidids_v1)


stroopids = c(tapply(Sdata2$id, Sdata2$id, function(x) unique(x)), tapply(Sdata$id, Sdata$id, function(x) unique(x)))
questids = tapply(Qdata$id, Qdata$id, function(x) unique(x))


#### calcualte some sueful tables

followed = tapply(DATA$followed, list(DATA$id, DATA$block, DATA$taskV, DATA$agegroup, DATA$cond, DATA$respkey %in% c(77, 88, 188)), mean, na.rm = TRUE)[,,,,2, 'TRUE']*100

foll_weights = tapply(DATA$followed, list(DATA$id, DATA$block, DATA$taskV, DATA$agegroup, DATA$cond, DATA$respkey %in% c(77, 88, 188)), length)[,,,,2, 'TRUE']
foll_hits = tapply(DATA$followed, list(DATA$id, DATA$block, DATA$taskV, DATA$agegroup, DATA$cond, DATA$respkey %in% c(77, 88, 188)), sum)[,,,,2, 'TRUE']

followed_split = tapply(DATA$followed, list(DATA$id, DATA$block, DATA$agegroup, DATA$btrial>90, DATA$cond, DATA$respkey %in% c(77, 88, 188)), mean, na.rm = TRUE)[,,,,2, 'TRUE']*100

errors = tapply(DATA$error>0, list(DATA$id, DATA$block, DATA$taskV, DATA$agegroup, DATA$cond), mean, na.rm = TRUE)[,,,,1]*100
errors_split = tapply(DATA$error>0, list(DATA$id, DATA$block, DATA$agegroup, DATA$btrial>90, DATA$cond), mean, na.rm = TRUE)[,,,,1]*100
rts = tapply(LEARN$rt, list(LEARN$id, LEARN$block, LEARN$taskV, LEARN$agegroup, LEARN$cond), median, na.rm = TRUE)[,,,,1]
prekeys_late = tapply(DATA$prekey1>0, list(DATA$id, DATA$block, DATA$taskV, DATA$agegroup, DATA$cond), mean, na.rm = TRUE)[,,,,3]*100
prekeys_late_split = tapply(DATA$prekey1>0, list(DATA$id, DATA$block, DATA$agegroup, DATA$btrial>90, DATA$cond), mean, na.rm = TRUE)[,,,,3]*100

prekeys_noGo = tapply(DATA$prekey1>0, list(DATA$id, DATA$block, DATA$taskV, DATA$agegroup, DATA$cond), mean, na.rm = TRUE)[,,,,4]*100
ambiguity_costs =
tapply(LEARN$rt, list(LEARN$id, LEARN$block, LEARN$taskV, LEARN$agegroup, LEARN$cond, LEARN$respkey != 0 ), median, na.rm = TRUE)[,,,,2,'TRUE'] -
tapply(LEARN$rt, list(LEARN$id, LEARN$block, LEARN$taskV, LEARN$agegroup, LEARN$cond), median, na.rm = TRUE)[,,,,1]
congruency_costs = tapply(LEARN$rt, list(LEARN$id, LEARN$block, LEARN$taskV, LEARN$agegroup, LEARN$dimA, LEARN$cond), median, na.rm = TRUE)[,,,,,1]
congruency_costs = (congruency_costs[,,,,1] + congruency_costs[,,,,3])/2 - (congruency_costs[,,,,2] + congruency_costs[,,,,4])/2

allfoll = as.vector(na.omit(as.vector(apply(followed[c(kidids_v1, kidids_v4, adultids_v1, adultids_v4),7:8,,], c(1, 3, 4), mean))))
allerrs = as.vector(na.omit(as.vector(apply(errors[c(kidids_v1, kidids_v4, adultids_v1, adultids_v4),7:8,,], c(1, 3, 4), mean))))
allrts = as.vector(na.omit(as.vector(apply(rts[c(kidids_v1, kidids_v4, adultids_v1, adultids_v4),7:8,,], c(1, 3, 4), mean))))
allprelate = as.vector(na.omit(as.vector(apply(prekeys_late[c(kidids_v1, kidids_v4, adultids_v1, adultids_v4),7:8,,], c(1, 3, 4), mean))))
allpreno = as.vector(na.omit(as.vector(apply(prekeys_noGo[c(kidids_v1, kidids_v4, adultids_v1, adultids_v4),7:8,,], c(1, 3, 4), mean))))

allambigcosts = as.vector(na.omit(as.vector(apply(ambiguity_costs[c(kidids_v1, kidids_v4, adultids_v1, adultids_v4),7:8,,], c(1, 3, 4), mean))))
allcongcosts = as.vector(na.omit(as.vector(apply(congruency_costs[c(kidids_v1, kidids_v4, adultids_v1, adultids_v4),7:8,,], c(1, 3, 4), mean))))

names(allfoll) = names(allrts) = names(allerrs) = names(allprelate) = names(allpreno) = names(allambigcosts) = names(allcongcosts) = c(kidids_v1, kidids_v4, adultids_v1, adultids_v4)
group =   c(rep('CHN Exp.1', nkids_v1), rep('CHN Exp.2', nkids_v4), rep('ADLT Exp.1', nadults_v1), rep('ADLT Exp.2', nadults_v4))



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


tmp_switch = tapply(DATA$followed, list(DATA$id, DATA$miniblock, DATA$cond, DATA$respkey %in% c(77, 88, 188)), mean, na.rm = TRUE)[,,2, 'TRUE']
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

Qdata$correctreport = 0
Qdata$correctreport[Qdata$id %% 2 == 0] = apply(ctab[Qdata$id %% 2 == 0,], 1, function(x) mean(x == c(1, 1, 2, 2)) == 1)
Qdata$correctreport[Qdata$id %% 2 == 1] = apply(ctab[Qdata$id %% 2 == 1,], 1, function(x) mean(x == c(2, 2, 1, 1)) == 1)

# participnats with NAs didn't write anything, i.e. they did not know
Qdata$correctreport[which(is.na(Qdata$correctreport))] = 0

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
  FOLL_hits = NA,
  FOLL_weights = NA,
	RECOG = NA,
	USED = NA,
	GROUP = group)

tmp = apply(apply(foll_hits[,7:8,,], c(1, 3, 4), sum), c(1), sum, na.rm = TRUE)
tmp2 = apply(apply(foll_weights[,7:8,,], c(1, 3, 4), sum), c(1), sum, na.rm = TRUE)

scores.cdf$FOLL_hits = tmp[scores.cdf$ID]
scores.cdf$FOLL_weights = tmp2[scores.cdf$ID]

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

for (cvar in names(scores.cdf)[2:15]) {
	cvarz = paste(cvar, 'z', sep = '')
	scores.cdf[cvarz] = -as.vector(scale(scores.cdf[cvar]))
}

# recode vars where lower is 'better' (sign flip to all already above)
scores.cdf$STROOPz = -scores.cdf$STROOPz



scores.cdf$EXP = NA

scores.cdf$EXP[scores.cdf$ID %in% adultids_v1] = 'V1'
scores.cdf$EXP[scores.cdf$ID %in% kidids_v1] = 'V1'
scores.cdf$EXP[scores.cdf$ID %in% adultids_v4] = 'V4'
scores.cdf$EXP[scores.cdf$ID %in% kidids_v4] = 'V4'

scores.cdf$FOLL2=scores.cdf$FOLL/100
scores.cdf$FOLLbin = scores.cdf$FOLLbin*1
scores.cdf$FOLL_logit = log(scores.cdf$FOLL2/(1-scores.cdf$FOLL2))
scores.cdf$PREz = (scores.cdf$PRENOz + scores.cdf$PRELATEz)/2
scores.cdf$TASKz = (scores.cdf$Ez + scores.cdf$RTz)/2

scores.cdf.no.na <- na.omit(scores.cdf)

ages = tapply(DATA$age, DATA$id, mean)
