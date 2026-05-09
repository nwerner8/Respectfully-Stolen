rm(list = ls())
library(dplyr)
library(glmnetUtils)

#read in the data
rs_pbp = data.table::fread("C:/Users/natew/Documents/R/baseball/mlb/data/2025plays/2025plays.csv")%>%
  select(gid:pithand, sb2:csh, br1_pre:br3_pre, f2, outs_pre, gametype, bunt, sh)

#do a lil feature engineering for the model's sake
rs_pbp = rs_pbp%>%
  mutate(score_b = ifelse(top_bot, score_h, score_v),
         score_p = ifelse(top_bot, score_v, score_h),
         score_diff = score_b-score_p,
         on1 = br1_pre != '',
         on2 = br2_pre != '',
         on3 = br3_pre != '',
         block1 = on1 & on2,
         block2 = on2 & on3,
         bunt = bunt | sh,
         runners_on = on1|on2|on3,
         sb_att = sb2|sb3|sbh|cs2|cs3|csh, #|(bunt & runners_on),
         sb = sb2|sb3|sbh,
         cs = cs2|cs3|csh,
         lead_runner = case_when(on3 ~ 3,
                                  on2 ~ 2,
                                  on1 ~ 1,
                                  T ~ 0),
         pb_same = case_when(bathand == 'B' ~ ifelse(pithand=='L','R','L'),
                             T ~ bathand) == pithand,
         is_extras = inning > 9,
         inn_extra = as.factor(ifelse(inning>9, 'extras', inning)))

#filter down to only times when a steal might occur
sb_opps = rs_pbp%>%
  filter((on1 & !block1) | (on2 & !block2) | (bunt & runners_on))%>%
  mutate(across(
    c(inn_extra, top_bot, lp, pithand, outs_pre, lead_runner),
    as.factor))

# run the cross validated lasso
sb_lasso_cv = cv.glmnet(sb_att ~ inn_extra + top_bot + lp + pithand + pb_same + outs_pre +
                          score_diff + score_diff*inning + lead_runner, 
                        data = sb_opps,
                        family = 'binomial',
                        alpha = 1,
                        nfolds = 10)

# run the lasso using the error minimizing lambda from the CV
sb_lasso = glmnet(sb_att ~ inn_extra + top_bot + lp + pithand + pb_same + outs_pre +
                    score_diff + score_diff*inning + lead_runner, 
                  data = sb_opps,
                  family = 'binomial',
                  alpha = 1,
                  lambda = sb_lasso_cv$lambda.min)
# write out the coefficients of the lasso
coef(sb_lasso)%>%
  as.matrix()%>%
  write.table(.,'clipboard', sep = '\t')

# write the estimated SB probabilities
sb_opps$sb_prob = predict(sb_lasso, sb_opps, type = 'response')

#validate that the estimations line up with reality
sum(as.logical(as.character(sb_opps$sb_att)))
sum(sb_opps$sb_prob)

#read in the Retro Sheet IDs
rs_bios = data.table::fread('../mlb/data/biodata/biofile0.csv')%>%
  mutate(name = paste(usename, lastname))%>%
  select(id,name)

#aggregate up to the catcher's stats
catcher_stats = sb_opps%>%
  group_by(f2)%>%
  summarise(sb_atts = sum(as.logical(as.character(sb_att))),
            sb = sum(sb),
            cs = sum(cs),
            sb_probs = sum(sb_prob),
            tot_opps = n())%>%
  ungroup()%>%
  mutate(cs_rate = cs/sb_atts,
         adj_att = pmax(sb_atts, sb_probs),
         adj_cs_rate = 1-(sb/adj_att),
         runner_respect = sb_probs - sb_atts,
         rr_opp = runner_respect/tot_opps,
         binom_test_lower = round(pbinom(sb_atts, tot_opps, sb_probs/tot_opps),3),
         binom_test_upper = round(pbinom(sb_atts, tot_opps, sb_probs/tot_opps, F),3))%>%
  left_join(., rs_bios, c('f2' = 'id'))%>%
  arrange(desc(runner_respect))

#write catcher stats
write.table(catcher_stats%>%filter(tot_opps>median(tot_opps)), 'clipboard',sep='\t', row.names = F)

#get the percentiles of the Pirates Catchers
pctile = ecdf(catcher_stats$runner_respect)
pctile(catcher_stats$runner_respect[catcher_stats$name=='Henry Davis'])
pctile(catcher_stats$runner_respect[catcher_stats$name=='Joey Bart'])
pctile(catcher_stats$runner_respect[catcher_stats$name=='Endy Rodriguez'])

catcher_leaders = catcher_stats%>%
   filter(tot_opps >= median(tot_opps))

pirates = catcher_stats%>%
  filter(name %in% c('Henry Davis', 'Joey Bart', 'Endy Rodriguez'))%>%
  select(name, cs_rate, adj_cs_rate, runner_respect, binom_test_lower, binom_test_upper)%>%
  mutate(percentile = pctile(runner_respect))%>%
  mutate(across(cs_rate:percentile, ~round(.x,3)))

write.table(pirates, 'clipboard', sep='\t', row.names = F)


#get distribution to sim over
endy_opps = sb_opps$sb_prob[sb_opps$f2 == rs_bios$id[rs_bios$name == 'Endy Rodriguez']]
bart_opps = sb_opps$sb_prob[sb_opps$f2 == rs_bios$id[rs_bios$name == 'Joey Bart']]
davis_opps = sb_opps$sb_prob[sb_opps$f2 == rs_bios$id[rs_bios$name == 'Henry Davis']]

#MC sim for each catcher
endy_sim = sapply(1:10000, function(x) sum((runif(length(endy_opps))<endy_opps)))
bart_sim = sapply(1:10000, function(x) sum((runif(length(bart_opps))<bart_opps)))
davis_sim = sapply(1:10000, function(x) sum((runif(length(davis_opps))<davis_opps)))

#calculate the probabilities
mean(endy_sim>=catcher_stats$sb_atts[catcher_stats$name == 'Endy Rodriguez'])
mean(bart_sim>=catcher_stats$sb_atts[catcher_stats$name == 'Joey Bart'])
mean(davis_sim<=catcher_stats$sb_atts[catcher_stats$name == 'Henry Davis'])


#plot endy's sim
data.frame(sim = endy_sim, actual_val = endy_sim == 9)%>%
  rename(`Actual Value` = actual_val)%>%
  ggplot()+
  geom_histogram(aes(endy_sim, fill = `Actual Value`), binwidth = .5)+
  scale_fill_manual(values = c('FALSE'='grey','TRUE'='red'))+
  geom_vline(xintercept = mean(endy_sim))+
  theme_minimal()+
  xlab('SB Attempts')+
  ylab('Sim Counts')+
  ggtitle('Endy Rodriguez Simulated SB Attempts',
          '10,000 Simulations Based on Modeled Probabilities')

# plot Barts sim
data.frame(sim = bart_sim, actual_val = bart_sim == catcher_stats$sb_atts[catcher_stats$name == 'Joey Bart'])%>%
  rename(`Actual Value` = actual_val)%>%
  ggplot()+
  geom_histogram(aes(sim, fill = `Actual Value`), binwidth = .5)+
  scale_fill_manual(values = c('FALSE'='grey','TRUE'='red'))+
  geom_vline(xintercept = mean(bart_sim))+
  theme_minimal()+
  xlab('SB Attempts')+
  ylab('Sim Counts')+
  ggtitle('Joey Bart Simulated SB Attempts',
          '10,000 Simulations Based on Modeled Probabilities')

# plot Davis sim
data.frame(sim = davis_sim, actual_val = davis_sim == catcher_stats$sb_atts[catcher_stats$name == 'Henry Davis'])%>%
  rename(`Actual Value` = actual_val)%>%
  ggplot()+
  geom_histogram(aes(sim, fill = `Actual Value`), binwidth = .5)+
  scale_fill_manual(values = c('FALSE'='grey','TRUE'='red'))+
  geom_vline(xintercept = mean(davis_sim))+
  theme_minimal()+
  xlab('SB Attempts')+
  ylab('Sim Counts')+
  ggtitle('Henry Davis Simulated SB Attempts',
          '10,000 Simulations Based on Modeled Probabilities')
