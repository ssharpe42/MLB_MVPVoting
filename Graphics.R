library(ggplot2)
library(ggthemes)
library(scales)

#Load Fused Lasso LP and data
source('MVP_FusedLassoLP.R')

#Final Model Stats
bat_stats = c('HRAA','RSAA','RBIAA','AVGplus','OBPplus','BClutch','B2HP','BatWAR','TWperc','Postseason')
sp_stats =c('WAA', 'SPERAplus','SPKAA','SPWHIPplus', 'SPClutch','SP2HP', 'SPWAR', 'SPTWperc','SPPostseason')
rp_stats = c('SVAA', 'RPERAplus','RPKAA','RPWHIPplus','RPClutch','RP2HP', 'RPWAR', 'RPTWperc','RPPostseason')

#Get 2017 Winners with best Winner Model
results =solve_lp_fused_lasso(AllMVP, end = 2016, 
                              n_seasons = 43,  
                              predict_next=T,
                              bat_stats = bat_stats, 
                              sp_stats = sp_stats, 
                              rp_stats= rp_stats,
                              lambda1 =0.148650889,
                              lambda2 =0.5946036)

#Get Weights for best ordering model (top 3/5)
result =solve_lp_fused_lasso(AllMVP, end = 2016, n_seasons = 43,
                             test_next = F,
                             bat_stats = bat_stats, 
                             sp_stats = sp_stats, 
                             rp_stats= rp_stats,
                             lambda1 = 0.105112052,
                             lambda2 =4.7568285)

#Convert stat names
convert_stat <- Vectorize(function(x) {
    x <- as.character(x)
    switch(x,
           'HRAA'='HR',
           'RSAA'='R',
           'RBIAA'='RBI',
           'AVGplus'='AVG',
           'OBPplus'='OBP',
           'BClutch'='Clutch',
           'BatWAR'='WAR',
           'TWperc'='Team Win %',
           'B2HP' = '2nd Half\nProduction',
           'Postseason'='Playoff\nBerth',
           'WAA'='W', 
           'SPERAplus'='ERA',
           'SPKAA'="K",
           'SPWHIPplus'='WHIP', 
           'SPClutch'='Clutch',
           'SP2HP'='2nd Half \nProduction', 
           'SPWAR'='WAR', 
           'SPTWperc'='Team Win %',
           'SPPostseason'='Playoff\nBerth',
           'SVAA'='SV', 
           'RPERAplus'='ERA',
           'RPKAA'="K",
           'RPWHIPplus'='WHIP', 
           'RPClutch'='Clutch',
           'RP2HP'='2nd Half\nProduction', 
           'RPWAR'='WAR', 
           'RPTWperc'='Team Win %',
           'RPPostseason'='Playoff\nBerth',
           NA)
})

YlMg = colorRampPalette(c('#fff7f3',
    '#fde0dd',
    '#fcc5c0',
    '#fa9fb5',
    '#f768a1',
    '#dd3497',
    '#ae017e',
    '#7a0177',
    '#49006a'))

#Get weights for each stat and year
Weights = result$stat_weights %>% gather(Stat, Value,-Season) %>%
    mutate(Stat = factor(Stat , levels = unique(Stat), ordered = T),
           Position = ifelse(Stat%in% bat_stats, 'Batter',
                             ifelse(Stat %in% sp_stats, 'Starting Pitcher','Relief Pitcher')),
           Anecdotal = grepl('TWperc|Postseason|2HP|Clutch', Stat))

#Normalize by position and season to sum to 1 for visualizations
NormWeights = group_by(Weights, Season, Position)%>%
    mutate(Value = Value/sum(Value),
           Stat = convert_stat(Stat))


#Position Player Weights Tile Graph
NormWeights%>% filter(Position=='Batter') %>%
    group_by(Stat)%>% 
    mutate(Pre2000 = mean(Value[Season<2000]), Post2000 =mean(Value[Season>=2000]), 
           AvgValue = mean(Value))%>%
    ungroup %>%
    arrange( Pre2000-Post2000 )  %>% mutate(Stat = factor(Stat, levels = Stat, ordered = T)) %>%
    ggplot()+geom_tile(aes(y = Season, x = Stat, fill = Value)) +
    scale_fill_gradientn(colors = YlMg(100),label = percent, name = "Percent of Voter \nConsideration")+
    theme_fivethirtyeight()+
    scale_y_continuous(name = 'Season\n')+
    theme(legend.key.width = unit(1, "cm"),
          axis.text = element_text(size = 12),
          plot.subtitle = element_text(size = 14),
          legend.text = element_text(size = 12),
          axis.title  = element_text(size = 14), 
          axis.title.x = element_blank())+
    ggtitle("What Do MLB MVP Voters Care About?", subtitle = 'Percent of MVP Vote Attributed Each Factor (Position Players)')
    
    
#Pitcher Weights Graph
PitcherWeights = NormWeights%>%filter(Position!='Batter')
levels = sort(unique(PitcherWeights$Stat))
levels = c('W','SV', levels[!levels %in% c('W','SV')])    

PitcherWeights %>% mutate(Stat = factor(Stat, levels = levels, ordered = T)) %>%ggplot()+geom_tile(aes(y = Season, x = Stat, fill = Value)) +
    scale_fill_gradientn(colors = YlMg(100),label = percent, name = "Percent of Voter \nConsideration")+facet_wrap(~Position, ncol = 1, scales = 'free_x')+
    theme_fivethirtyeight()+
    scale_y_continuous(name = 'Season\n')+
    theme(legend.key.width = unit(1, "cm"),
          axis.text = element_text(size = 12),
          plot.subtitle = element_text(size = 14),
          legend.text = element_text(size = 12),
          axis.title  = element_text(size = 14), 
          axis.title.x = element_blank(),
          strip.text = element_text(size = 14))+
    ggtitle("What Do MLB MVP Voters Care About?", subtitle = 'Percent of MVP Vote Attributed Each Factor (Pitchers)')

#All About WAR Graph
ggplot()+ 
    geom_line(data = filter(NormWeights, Position=='Batter', Stat=='WAR'), aes(x = Season, y = Value, group = Stat, alpha = 'WAR'),  size = 1.5)+
    geom_line(data = filter(NormWeights, Position=='Batter', Stat!='WAR'), aes(x = Season, y = Value, group = Stat, alpha = 'Other Stats'),colour = 'black', size = 1)+
    theme_fivethirtyeight()+
    scale_y_continuous(label = percent, name = 'Percent of Voter Consideration\n')+
    scale_x_continuous(name = '\nSeason')+
    scale_alpha_manual(values = c('WAR'=1, 'Other Stats'=.2), name = '')+
    theme(legend.key.width = unit(1, "cm"),
          axis.text = element_text(size = 12),
          plot.subtitle = element_text(size = 14),
          axis.title  = element_text(size = 14),
          legend.title = element_blank(),
          legend.text = element_text(size = 12))+
    ggtitle("It's All About WAR")  

#Anecdote Graph
ggplot()+ 
    geom_line(data = filter(NormWeights, Position=='Batter', Anecdotal), aes(x = Season, y = Value, group = Stat, colour = Stat),  size = 1.5)+
    geom_line(data = filter(NormWeights, Position=='Batter'), aes(x = Season, y = Value, group = Stat),alpha = .2, colour = 'black', size = 1)+
    theme_fivethirtyeight()+
    scale_y_continuous(label = percent, name = 'Percent of Voter Consideration\n')+
    scale_x_continuous(name = '\nSeason')+
    scale_alpha_manual(values = c('WAR'=1, 'Other Stats'=.2), name = '')+
    theme(legend.key.width = unit(1, "cm"),
          axis.text = element_text(size = 12),
          plot.subtitle = element_text(size = 14),
          axis.title  = element_text(size = 14),
          legend.title = element_blank(),
          legend.text = element_text(size = 12))+
    ggtitle("Anecdotes: Team, Clutch, Postseason, and Recency Bias")

#Runs and RBI graph
ggplot()+ 
    geom_line(data = filter(NormWeights, Position=='Batter', Stat %in% c('RBI','R')), aes(x = Season, y = Value, group = Stat, colour = Stat),  size = 1.5)+
    geom_line(data = filter(NormWeights, Position=='Batter', !Stat %in% c('RBI','R')), aes(x = Season, y = Value, group = Stat),alpha = .2, colour = 'black', size = 1)+
    theme_fivethirtyeight()+
    scale_y_continuous(label = percent, name = 'Percent of Voter Consideration\n')+
    scale_x_continuous(name = '\nSeason')+
    scale_alpha_manual(values = c('WAR'=1, 'Other Stats'=.2), name = '')+
    theme(legend.key.width = unit(1, "cm"),
          axis.text = element_text(size = 12),
          plot.subtitle = element_text(size = 14),
          axis.title  = element_text(size = 14),
          legend.title = element_blank(),
          legend.text = element_text(size = 12))+
    ggtitle("Runs and RBI Aren't What They Used to Be")

#HR, Avg, OBP Graph
ggplot()+ 
    geom_line(data = filter(NormWeights, Position=='Batter', Stat %in% c('OBP','AVG','HR')), aes(x = Season, y = Value, group = Stat, colour = Stat),  size = 1.5)+
    geom_line(data = filter(NormWeights, Position=='Batter', !Stat %in% c('OBP','AVG','HR')), aes(x = Season, y = Value, group = Stat),alpha = .2, colour = 'black', size = 1)+
    theme_fivethirtyeight()+
    scale_y_continuous(label = percent, name = 'Percent of Voter Consideration\n')+
    scale_x_continuous(name = '\nSeason')+
    scale_alpha_manual(values = c('WAR'=1, 'Other Stats'=.2), name = '')+
    theme(legend.key.width = unit(1, "cm"),
          axis.text = element_text(size = 12),
          plot.subtitle = element_text(size = 14),
          axis.title  = element_text(size = 14),
          legend.title = element_blank(),
          legend.text = element_text(size = 12))+
    ggtitle("Classics Stay Strong, OBP Gets a Nod")





