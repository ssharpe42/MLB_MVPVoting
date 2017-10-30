library(dplyr)
library(tidyr)
library(Lahman)
options(stringsAsFactors  = FALSE)

#Read team crosswalk
TeamCross = read.csv('data/TeamCross.csv')
Team_League = read.csv('data/Team_Leagues.csv')

MVPVotes  = readRDS('data/MVPVotes.RDS') %>% filter(Season >=1974)
#One name difference in FanGraphs
MVPVotes$Name[MVPVotes$Name=='Jim Wynn'] = 'Jimmy Wynn'
MVPVotes$Name[MVPVotes$Name=='Ken Griffey'& MVPVotes$Season>1988] = 'Ken Griffey Jr.'

#Read current year Bref info
CurrentBref = read.csv('data/BRef2017.csv') %>% 
    select(-Pos)%>%
    mutate(Season = 2017, Rank = 1, `Vote Pts` = 1)
CurrentBref  = left_join(CurrentBref, TeamCross, by = c('Tm'='BRef.Team')) %>% 
    left_join(Team_League, by = c('FG.Team'='Team','Season')) %>% 
    select(-FG.Team, -Lahman.Team) %>%
    filter(!duplicated(Name))

#Append to past votes
MVPVotes = bind_rows(MVPVotes, CurrentBref)
MVPVotes = mutate(MVPVotes, ID = paste(Name, Season))

#Read full season statistics from FanGraphs (FG)
scrape_data = function(url){
    #scrape table
    df = read_html(url) %>% 
        html_node('.rgMasterTable') %>% 
        html_table()
    #column names
    cols = as.character(df[2,-1])
    #get rid of non used rows
    df = df[-1:-3,-1]
    #name columns
    names(df) = cols
    row.names(df)=1:nrow(df)
    complete_row = df[complete.cases(df),][1,]
    #percent columns
    perc_cols = which(grepl('%',complete_row))
    #make percent into decimal
    df[, perc_cols] = lapply(df[,perc_cols], function(x) as.numeric(gsub(' %','',x))/100)
    #numeric columns as numeric
    df[,!names(df)%in%c('Name','Team','Season')] = lapply(df[,!names(df)%in%c('Name','Team','Season')], as.numeric)
    #df[, grepl('[[:digit:]]',complete_row)] = lapply(df[,grepl('[[:digit:]]',complete_row)], as.numeric)
    #take out punct from names
    names(df) = gsub('\\%','perc',names(df))
    names(df) = gsub('\\+','plus',names(df))
    names(df) = gsub('\\-','minus',names(df))
    
    return(df)
    
}
#https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=y&type=c,4,6,5,11,12,13,21,-1,34,35,40,41,-1,23,37,38,50,61,-1,111,-1,203,199,58,51,71&season=2016&month=0&season1=1974&ind=1&team=0&rost=0&age=0&filter=&players=0
FS_B = read.csv('data/Full Season Batters.csv') %>% mutate(ID = paste(Name, Season))
#https://www.fangraphs.com/leaders.aspx?pos=all&stats=sta&lg=all&qual=y&type=c,4,5,11,7,8,13,-1,36,37,40,43,44,48,51,-1,6,45,62,-1,59,74,42&season=2016&month=0&season1=1974&ind=1&team=0&rost=0&age=0&filter=&players=0
FS_SP = read.csv('data/Full Season SP.csv') %>% mutate(ID = paste(Name, Season)) %>%
    left_join(Team_League )
#https://www.fangraphs.com/leaders.aspx?pos=all&stats=rel&lg=all&qual=y&type=c,4,5,11,7,8,13,-1,36,37,40,43,44,48,51,-1,6,45,62,-1,59,74,42&season=2016&month=0&season1=1974&ind=1&team=0&rost=0&age=0&filter=&players=0
FS_RP = read.csv('data/Full Season RP.csv') %>% mutate(ID = paste(Name, Season))%>%
    left_join(Team_League )

#Read full season statistics split by Team from FG
FS_B_TeamSplit = read.csv('data/Full Season Batters_Team Split.csv')%>%
    mutate(ID = paste(Name, Season)) %>%
    left_join(Team_League)
FS_SP_TeamSplit = read.csv('data/Full Season SP_Team Split.csv')%>%
    mutate(ID = paste(Name, Season)) %>%
    left_join(Team_League)
FS_RP_TeamSplit = read.csv('data/Full Season RP_Team Split.csv')%>%
    mutate(ID = paste(Name, Season)) %>%
    left_join(Team_League)

#Get Second Half Batter Stats
# Half2_Batters = data.frame()
# for(season in 1974:2017){
#     print(season)
#     url = sprintf('http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=50&type=c,4,6,11,12,13,21,-1,34,35,40,41,-1,23,37,38,50,61,-1,111,-1,203,199,58,51&season=%d&month=31&season1=%d&ind=0&team=0&rost=0&age=0&filter=&players=&page=1_10000',season, season)       
#     current_season = scrape_data(url) %>% mutate(Season = season)
#     Half2_Batters = bind_rows(Half2_Batters, current_season)
# }
# saveRDS(Half2_Batters, 'data/Half2_Batters.RDS')
# 
# Half2_Pitchers = data.frame()
# for(season in 1974:2017){
#     print(season)
#     url = sprintf('http://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=0&type=8&season=%d&month=31&season1=%d&ind=0&team=0&rost=0&age=0&filter=&players=0&page=1_10000',season, season)       
#     current_season = scrape_data(url) %>% mutate(Season = season)
#     Half2_Pitchers = bind_rows(Half2_Pitchers, current_season)
# }
# saveRDS(Half2_Pitchers, 'data/Half2_Pitchers.RDS')

Half2_Batters = readRDS('data/Half2_Batters.RDS')
Half2_Pitchers = readRDS('data/Half2_Pitchers.RDS')

#Read league statistics from FG
League_B = bind_rows(read.csv('data/Full Season AL Batting.csv') %>%mutate(League = 'AL'),
                    read.csv('data/Full Season NL Batting.csv') %>%mutate(League = 'NL')) %>%
    mutate(lgHRpPA = HR/PA, lgRpPA = R/PA, lgRBIpPA = RBI/PA) %>%
    select(Season, League, lgHRpPA, lgRpPA, lgRBIpPA, lgAVG=AVG, lgOBP = OBP, lgSLG = SLG ) 

League_SP = bind_rows(read.csv('data/Full Season AL SP.csv')%>%mutate(League = 'AL'),
                      read.csv('data/Full Season NL SP.csv') %>%mutate(League = 'NL')) %>%
    select(Season, League, lgERA = ERA, lgFIP = FIP ,lgK.9 = K.9,lgWHIP = WHIP ) 

League_RP = bind_rows(read.csv('data/Full Season AL RP.csv')%>%mutate(League = 'AL'),
                      read.csv('data/Full Season NL RP.csv') %>%mutate(League = 'NL')) %>%
    select(Season, League, lgERA = ERA, lgFIP = FIP ,lgK.9 = K.9, lgWHIP = WHIP ) 


###################  Join MVP Batter Stats #################
MVP_B = filter(MVPVotes, ID %in% FS_B$ID) %>%
    mutate(Pos = 'B')

#Calculate normalized statistics for batters with multiple teams in one season
#Batters may switch leagues, so I have to control for the different environment
SplitBatterSeasons = filter(MVP_B, Tm=='TOT')$ID
MultTeam_B = filter(FS_B_TeamSplit, ID %in% SplitBatterSeasons) %>%
    group_by(ID, Season, Name) %>%
    arrange(desc(PA)) %>%
    left_join(League_B) %>%
    mutate(HRAA = PA*(HR/PA - lgHRpPA),
           RSAA = PA*(R/PA - lgRpPA),
           RBIAA = PA*(RBI/PA - lgRBIpPA),
           AVGplus = AVG/lgAVG, 
           OBPplus = OBP/lgOBP,
           SLGplus = SLG/lgSLG) %>%
    summarise( Team = first(Team), League = first(League) , PA=sum(PA), HR=sum(HR), R=sum(R), RBI=sum(RBI), 
               AVGplus = sum(AVGplus*AB)/sum(AB),OBPplus = sum(OBPplus*(AB+BB+HBP+SF))/sum(AB+BB+HBP+SF),
                    SLGplus = sum(SLGplus*AB)/sum(AB),HRAA=sum(HRAA), RSAA=sum(RSAA), RBIAA=sum(RBIAA))%>% 
    ungroup %>%
    left_join(TeamCross, by = c('Team'='FG.Team'))

#Missing Leagues for Current Year
B_MissingL = MVP_B[is.na(MVP_B$League),]

#Change team to one with most PA
MVP_B[match(MultTeam_B$ID,MVP_B$ID ),'Tm'] = MultTeam_B$BRef.Team

#Fill Empty Leagues
MVP_B[match(B_MissingL$ID,MVP_B$ID),'League'] =  MultTeam_B[match(B_MissingL$ID, MultTeam_B$ID),'League']

#Join full season statistics
MVP_B = left_join(MVP_B, select(FS_B , Season , Name, PA, HR, R, AVG, OBP, SLG, RBI, BsR, Def, wRAA, WAR, Clutch)) %>%
    left_join(League_B) %>%
    mutate(HRAA = PA*(HR/PA - lgHRpPA),
           RSAA = PA*(R/PA - lgRpPA),
           RBIAA = PA*(RBI/PA - lgRBIpPA),
           AVGplus = AVG/lgAVG,
           OBPplus = OBP/lgOBP,
           SLGplus = SLG/lgSLG) %>%
    select(-PA, -HR, -RBI, -R, -AVG, -matches('^lg'))

#Replace adjusted stats for players with multiple teams
MVP_B[match(MultTeam_B$ID,MVP_B$ID ), c('HRAA','RSAA','RBIAA','AVGplus','OBPplus','SLGplus')] = MultTeam_B[, c('HRAA','RSAA','RBIAA','AVGplus','OBPplus','SLGplus')]

#Get percent of production in second half
MVP_B = left_join(MVP_B, Half2_Batters %>% select(Season , Name, WAR2 = WAR)) %>%
    mutate(B2HP = pmax(0, WAR2)/WAR, BatWAR = (bWAR + WAR)/2 ) %>%
    select(-WAR2,-bWAR,-WAR) 



###################  Join MVP SP Stats #################
MVP_SP = filter(MVPVotes, ID %in% FS_SP$ID)%>%
    mutate(Pos = 'SP')
SplitSPSeasons = filter(MVP_SP, Tm=='TOT')$ID

#Calculate normalized statistics for batters with multiple teams in one season
#Batters may switch leagues, so I have to control for the different environment
MultTeam_SP = filter(FS_SP_TeamSplit, ID %in% SplitSPSeasons) %>%
    group_by(ID, Season, Name) %>%
    arrange(desc(IP)) %>%
    left_join(League_SP) %>%
    mutate(KAA = IP*(K.9 - lgK.9)/9,
           WHIPplus = lgWHIP/WHIP, 
           ERAplus = lgERA/ERA,
           FIPplus = lgFIP/FIP) %>%
    summarise( Team = first(Team), League = first(League) ,KAA = sum(KAA), ERAplus = sum(ERAplus*IP)/sum(IP), WHIPplus = sum(WHIPplus*IP)/sum(IP),
               FIPplus = sum(FIPplus*IP)/sum(IP),  IP=sum(IP))%>% 
    ungroup %>%
    left_join(TeamCross, by = c('Team'='FG.Team'))

#Missing Leagues for Current Year
SP_MissingL = MVP_SP[is.na(MVP_SP$League),]

#Change team to one with most PA
MVP_SP[match(MultTeam_SP$ID,MVP_SP$ID ),'Tm'] = MultTeam_SP$BRef.Team

#Fill Empty Leagues
MVP_SP[match(SP_MissingL$ID,MVP_SP$ID),'League'] =  MultTeam_SP[match(SP_MissingL$ID, MultTeam_SP$ID),'League']

#Join full season statistics
MVP_SP = left_join(MVP_SP, select(FS_SP , Season ,League,  Name, W, IP, GS ,ERA, FIP, K.9, WHIP, Clutch, WAR) %>%
                       group_by(Season, League) %>% 
                       mutate(WAA = GS*(W/GS - sum(W)/sum(GS))) %>% ungroup%>%
                       select(-League))%>%
    left_join(League_SP) %>%
    mutate(KAA = IP*(K.9 - lgK.9)/9,
           ERAplus = lgERA/ERA,
           WHIPplus = lgWHIP/WHIP, 
           FIPplus = lgFIP/FIP) %>%
    select( -IP, -W, -GS)

#Replace adjusted count stats for players with multiple teams
MVP_SP[match(MultTeam_SP$ID,MVP_SP$ID ), c('KAA','ERAplus','FIPplus')] = MultTeam_SP[, c('KAA','ERAplus','FIPplus')]

#Get percent of production in second half
MVP_SP = left_join(MVP_SP, Half2_Pitchers %>% select(Season , Name, WAR2=WAR)) %>%
    mutate( SP2HP = pmin(1,pmax(WAR2,0)/WAR), SPWAR = (bWAR + WAR)/2 ) %>%
    select(-bWAR,-WAR) 



######  Join Relief Pitchers Stats ################
MVP_RP = filter(MVPVotes, ID %in% FS_RP$ID)%>%
    mutate(Pos = 'RP')
RPlitRPSeasons = filter(MVP_RP, Tm=='TOT')$ID

#No split league RP
# MultTeam_RP = filter(FS_RP_TeamRPlit, ID %in% RPlitRPSeasons) %>%
#     group_by(ID, Season, Name) %>%
#     arrange(desc(IP)) %>%
#     left_join(League_RP) %>%
#     mutate(KAA = IP*(K.9 - lgK.9)/9,
#            ERAplus = lgERA/ERA,
#            FIPplus = lgFIP/FIP) %>%
#     summarise( Team = first(Team), KAA = sum(KAA), ERAplus = sum(ERAplus*IP)/sum(IP), FIPplus = sum(FIPplus*IP)/sum(IP), FIPRAA  = sum(FIPRAA ), IP=sum(IP))%>% 
#     ungroup %>%
#     left_join(TeamCross, by = c('Team'='FG.Team'))

#Change team to one with most PA
# MVP_RP[match(MultTeam_RP$ID,MVP_RP$ID ),'Tm'] = MultTeam_RP$BRef.Team

#Join full season statistics
MVP_RP = left_join(MVP_RP, select(FS_RP , Season ,League,  Name, SV, IP, G ,ERA, WHIP, FIP, K.9, Clutch, WAR) %>%
                       group_by(Season, League) %>% 
                       mutate(SVAA = G*(SV/G - sum(SV)/sum(G))) %>% ungroup%>%
                       select(-League))%>%
    left_join(League_RP) %>%
    mutate(KAA = IP*(K.9 - lgK.9)/9,
           ERAplus = lgERA/ERA,
           WHIPplus = lgWHIP/WHIP, 
           FIPplus = lgFIP/FIP) %>%
    select( -IP, -SV, -G)

#Replace adjusted count stats for players with multiple teams
#MVP_RP[match(MultTeam_RP$ID,MVP_RP$ID ), c('KAA','ERAplus','FIPplus')] = MultTeam_RP[, c('KAA','ERAplus','FIPplus')]

#Get percent of production in second half
MVP_RP = left_join(MVP_RP, Half2_Pitchers %>% select(Season , Name, WAR2=WAR)) %>%
    mutate( RP2HP = pmin(1,pmax(WAR2,0)/WAR), RPWAR = (bWAR + WAR)/2 ) %>%
    select(-bWAR,-WAR) 


################ Combine All MVP Players and Stats (Clean & Rename) ###############

Keep = c('Rank','Name','Tm','Vote Pts','League','Season', 'Pos')
BatKeep = c('HRAA','RSAA','RBIAA','AVGplus','OBPplus', 'SLGplus','BsR','Def','wRAA','Clutch','B2HP','BatWAR')
SPKeep = c('WAA', 'ERAplus','KAA','FIPplus','WHIPplus', 'Clutch','SP2HP', 'SPWAR')
RPKeep = c('SVAA', 'ERAplus','KAA','FIPplus','WHIPplus','Clutch','RP2HP','RPWAR')

names(MVP_B)[names(MVP_B) %in% intersect(BatKeep, SPKeep)] = paste0('B', names(MVP_B)[names(MVP_B) %in% intersect(BatKeep, SPKeep)])
BatKeep[BatKeep %in% intersect(BatKeep, SPKeep)] = paste0('B', BatKeep[BatKeep %in% intersect(BatKeep, SPKeep)])

names(MVP_SP)[names(MVP_SP) %in% intersect(RPKeep, SPKeep)] = paste0('SP', names(MVP_SP)[names(MVP_SP) %in% intersect(RPKeep, SPKeep)])
names(MVP_RP)[names(MVP_RP) %in% intersect(RPKeep, SPKeep)] = paste0('RP', names(MVP_RP)[names(MVP_RP) %in% intersect(RPKeep, SPKeep)])
spmatch = which(SPKeep %in% intersect(SPKeep, RPKeep))
rpmatch = which(RPKeep %in% intersect(SPKeep, RPKeep))
SPKeep[spmatch] = paste0('SP', SPKeep[spmatch])
RPKeep[rpmatch] = paste0('RP', RPKeep[rpmatch])


AllMVP = bind_rows(MVP_B[, c(Keep ,BatKeep)],
                   MVP_SP[, c(Keep, SPKeep)],
                   MVP_RP[,c(Keep, RPKeep)])

########Join Team Data###############

#Load current year not in Lahman database
CurrentLahman = read.csv('data/Lahman2017.csv')

#Join all team info to player stats
AllMVP = left_join(AllMVP, left_join( TeamCross, 
                                      bind_rows(CurrentLahman, 
                                                Lahman::Teams %>% select(Season = yearID, Lahman.Team = teamID, W, L, DivWin, WCWin)) %>%
                                 mutate(TWperc = W/(W+L), 
                                        Postseason = ifelse((DivWin=='Y'& !is.na(DivWin))| (WCWin=='Y' & !is.na(WCWin)),1,0))%>%
                                 select(-W, -L, -DivWin,-WCWin)) %>%
                       group_by(BRef.Team, Season)%>%
                       filter(row_number()==1), 
                   by = c('Tm'='BRef.Team', 'Season'))%>%
    select(-FG.Team, -Lahman.Team)%>%
    mutate(RPTWperc = TWperc, RPPostseason = Postseason, 
           SPTWperc = TWperc, SPPostseason = Postseason)

#Replace team stats with NA for stats not corresponding to player type
AllMVP[AllMVP$Pos!='B',c('TWperc','Postseason')] = NA
AllMVP[AllMVP$Pos!='SP',c('SPTWperc','SPPostseason')] = NA
AllMVP[AllMVP$Pos!='RP',c('RPTWperc','RPPostseason')] = NA

#save to file
saveRDS(AllMVP, 'data/AllMVP3.RDS')





 