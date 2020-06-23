
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

# statistics on how many subjs were excluded in each age group b/c of this
#table(tmp<follthresh, tapply(DATA$agegroup, list(DATA$id), unique))
#tapply(DATA$id, list(DATA$agegroup, DATA$taskV), function(x) length(unique(x)))
#apply(tapply(DATA$agegroup, list(DATA$id, DATA$taskV), unique), 2, function(x) table(tmp[x != '']<follthresh, x[x != '']))

# now exclude subjs who made too many errors in main task at end of experiment
#tmp = tapply(DATA$error>0, list(DATA$id, DATA$block, DATA$cond), mean, na.rm = TRUE)[,7:8,1]
#tmp = apply(tmp, 1, mean)
#exclids2 = which(tmp > errthresh)

# statistics on how many subjs were excluded in each age group b/c of this
#table(tmp<errthresh, tapply(DATA$agegroup, list(DATA$id), unique))
#tapply(DATA$id, list(DATA$agegroup, DATA$taskV), function(x) length(unique(x)))
#apply(tapply(DATA$agegroup, list(DATA$id, DATA$taskV), unique), 2, function(x) table(tmp[x != '']<errthresh, x[x != '']))

#length(exclids2)
#length(exclids)
#length(intersect(exclids2, exclids))
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
