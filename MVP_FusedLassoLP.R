library(lpSolve)
library(dplyr)
library(tidyr)
library(Matrix)
library(foreach)
library(doParallel)

################### LP Fused Lasso Function ####################

solve_lp_fused_lasso = function(df, 
                          n_seasons = 10, 
                          end = 2015, 
                          only_batters = FALSE,
                          top_n = 10, 
                          weighted = FALSE,
                          test_next = TRUE, 
                          predict_next = FALSE, 
                          lambda1 = 0.10511205,
                          lambda2 = 4.7568285,
                          bat_stats = c('HRAA','RSAA','RBIAA','AVGplus','OBPplus','SLGplus','BsR','Def','wRAA','BClutch','B2HP','BatWAR','TWperc','Postseason'), 
                          sp_stats =c('WAA', 'SPERAplus','SPKAA','SPFIPplus','SPWHIPplus', 'SPClutch','SP2HP', 'SPWAR', 'SPTWperc','SPPostseason'),
                          rp_stats = c('SVAA', 'RPERAplus','RPKAA','RPFIPplus','RPWHIPplus','RPClutch','RP2HP', 'RPWAR', 'RPTWperc','RPPostseason')){
    
    #Seasons
    seasons = sort(unique(df$Season))
    season_indx = which(seasons==end)
    
    #Subset positions
    if(only_batters){
        df = filter(df, Pos=='B')
        pos = 1
        #Included stats
        stats = c(bat_stats)
    }else{
        pos=3
        #Included stats
        stats = c(bat_stats, sp_stats, rp_stats)
    }
    
    #Number of Stats
    n_stats = length(stats)
    
    #Subset Statistics
    df2 =df[, c(names(df)[1:7], stats)]
    
    #Subset to season
    df_train = df2 %>%
        filter( between(Season, seasons[season_indx - n_seasons+1], end))%>%
        group_by(Season, League) %>%
        arrange(Rank)%>%
        filter(row_number()<=top_n) %>%
        ungroup
    
    cols = ncol(df_train)
    stats_cols = 8:cols
    #Scale Statistics (X - mean)/sd
    col_avg = apply(df_train[, stats_cols],2,  function(x) mean(x, na.rm = TRUE))
    col_sd = apply(df_train[, stats_cols],2,  function(x) sd(x, na.rm = TRUE))
    #avoid NaN
    col_sd[is.na(col_sd)]=1
    #apply mean and sd
    df_train[, stats_cols]= sweep(df_train[, stats_cols], 2, col_avg,"-")
    df_train[, stats_cols]= sweep(df_train[, stats_cols], 2, col_sd, "/")
    
    #Replace NA with 0
    df_train[is.na(df_train)]=0
    
    if(test_next){
        
        #test data
        if(predict_next){
            #dont filter to top_n
            #want to allow all players to be scored
            df_test = df2 %>%
                filter(Season == seasons[season_indx +1])%>%
                group_by(League) %>%
                arrange(Rank)
            
        }else{
            df_test = df2 %>%
                filter(Season == seasons[season_indx +1])%>%
                arrange(League, Rank) 
        }
        
        
        df_test[, stats_cols] = sweep(df_test[, stats_cols] , 2, col_avg,"-")
        df_test[, stats_cols] = sweep(df_test[, stats_cols] , 2, col_sd, "/")
        
        #Replace NA with 0
        df_test[is.na(df_test)]=0
        
    }
    
    #Sort by Season, League, and Rank to ensure constraints are correct
    df_train = arrange(df_train, Season, League, Rank)
    
    #Set Parameters
    #number of ordering constraints
    order_const = group_by(df_train, Season, League)%>% 
        summarise(N = n()-1)%>%
        `$`('N')
    n_order = sum(order_const)
    #Sets of weight variables (# of seasons)
    #Number of weight variables (# of seasons * # of stats)
    n_weights = n_seasons*n_stats
    #Year to year stat difference constraints
    n_diff = 2*(n_seasons-1)*n_stats
    #coef regularization constraints
    n_reg = 2*n_weights
    #total constraints 
    const = n_order+n_diff+n_reg
    #Number of abs weight diff values
    z = (n_seasons-1)*n_stats
    #Number of abs weight regularization values y = n_weights
    
    #number of variables = # of stats + error terms for each order constraint + stat differences + regularization weights
    nvar = n_weights+ n_order + (n_seasons-1)*n_stats +n_weights
    #non negative variabls
    nn_var =  n_order 
    
    #Create Constraints & Coefficients
    StatDiff = group_by(df_train, Season, League) %>%
        arrange(Rank) %>%
        mutate_each(funs(lag(.)-.), stats_cols) %>%
        filter(row_number()>1)%>%
        ungroup%>%
        arrange(Season, League, Rank) 
    
    train_ranks = StatDiff$Rank-1
    
    StatDiff = StatDiff%>%
        select(stats_cols)
    
    
    ObjCoef =  group_by(df_train, Season, League) %>%
        arrange(Rank) %>%
        mutate(ObjCoef = lag(`Vote Pts`)-`Vote Pts`) %>%
        filter(row_number()>1)%>%
        ungroup%>%
        arrange(Season, League, Rank) %>%
        `$`('ObjCoef')
    
    #Do the same for test data
    if(test_next){
        #Create Constraints & Coefficients
        StatDiffTest = group_by(df_test, League) %>%
            arrange(Rank) %>%
            mutate_each(funs(lag(.)-.), stats_cols) %>%
            mutate(PrevPos = lag(Pos))%>%
            filter(row_number()>1)%>%
            ungroup%>%
            arrange(Season, League, Rank) 
        
        #ranks and positions of test contraints
        test_ranks = StatDiffTest$Rank-1
        
        StatDiffTest = StatDiffTest%>%
            select(stats_cols)
        
        #Objective coefficients
        ObjCoefTest =  group_by(df_test, League) %>%
            arrange(Rank) %>%
            mutate(ObjCoef = lag(`Vote Pts`)-`Vote Pts`) %>%
            filter(row_number()>1)%>%
            ungroup%>%
            arrange(Season, League, Rank) %>%
            `$`('ObjCoef')
    }
    
    A = matrix(0, nrow = const +nn_var,ncol = nvar )
    
    order_season_indx = c(0, cumsum(order_const)[(1:length(order_const))[1:length(order_const)%%2==0]])
    order_season_const=lapply(2:length(order_season_indx), function(i) as.matrix(StatDiff)[(order_season_indx[i-1]+1):order_season_indx[i], ])
    
    #Score difference between ranks: stats_ylrs %*% weights_ys - stats_ylr+1s %*% weights_ys 
    A[1:n_order, 1:n_weights] = as.matrix(bdiag(order_season_const))
    
    #Error terms: stats_ylrs %*% weights_ys - stats_ylr+1s %*% weights_ys + error term_ylr >=0
    A[1:n_order, (n_weights+1):(n_weights + n_order)] = diag(1, nrow =n_order, ncol =n_order )
    
    #Year to year weight difference constraints: 
    #  weights_ys+1 - weights_ys - e_ys <=0
    #  -weights_ys+1 + weights_ys - e_ys<=0
    row = n_order
    for(season in 1:(n_seasons-1)){
        for(stat in 1:n_stats){
            row = row+1
            cols = c(season*n_stats+stat, (season-1)*n_stats+stat, n_weights+n_order+(season-1)*n_stats +stat)
            A[row, cols] = c(1,-1,-1)
            row = row+1
            A[row, cols] = c(-1,1,-1)
        }
    }
    
    #Regularize all stat coefficients
    R =  kronecker(diag(1, n_weights), matrix(c(1,-1), nrow = 2))
    #Error value coeff in obj for regularization
    Y =  kronecker(diag(1, n_weights), matrix(c(-1,-1), nrow = 2))
    #Variables not involved
    Empty = matrix(0, nrow = n_reg, ncol = n_order + (n_seasons-1)*n_stats)
    
    A[(n_order+n_diff+1):(n_order+n_diff+n_reg),]  = cbind(R, Empty, Y)
    
    #All non negative var
    A[(const+1):(const+nn_var),(n_weights+1):(n_weights+ n_order)] = diag(1, nrow = nn_var, ncol = nn_var)
    
    #Right hand side 
    b = c(rep(.001, n_order), rep(0, n_diff), rep(0, n_reg), rep(0, nn_var))
    
    #const direction
    eq = c(rep('>=',n_order), rep('<=', n_diff), rep('<=', n_reg), rep('>=',nn_var))
    #objective
    if(weighted ){
        obj = c(rep(0,n_weights),ObjCoef, rep(lambda2, z), rep(lambda1, n_weights))
    }else{
        obj = c(rep(0,n_weights),rep(1, length(ObjCoef)), rep(lambda2, z), rep(lambda1, n_weights))
    }
    #solve lp
    sol = lp(direction ='min', 
             objective.in =obj, 
             const.mat = A, 
             const.dir = eq, 
             const.rhs = b)
    
    #lp solution: stat weights
    solution = sol$solution
    objective = sol$objective
    stat_weights = matrix(solution[1:n_weights], nrow = n_seasons,byrow = TRUE)
    stat_weights_df = data.frame(stat_weights)
    names(stat_weights_df) =  stats
    stat_weights_df = mutate(stat_weights_df, Season = seasons[(season_indx - n_seasons+1):season_indx])
    
    #calculate train error (weighted, and winner unweighted)
    error_indx = (n_weights+1):(n_weights+n_order)
    error = ObjCoef*solution[error_indx]
    train_error = mean(error)
    w_indx = which(train_ranks==1)
    train_winner_error = mean(error[w_indx])
    train_winner_accuracy =mean(error[w_indx]==0)
    
    #if testing next year calculate error and return
    if(test_next){
        stat_weights_test = as.vector(stat_weights[n_seasons, ])
        
        if(predict_next){
            Scores = data.frame(as.matrix(df_test[,stats])%*%stat_weights_test)
            names(Scores)='Score'
            Scores = mutate(Scores, Name = df_test$Name, 
                            League = df_test$League, Pos = df_test$Pos)
            
            return(Scores)
        }else{
            
            Scores = data.frame(as.matrix(df_test[,stats])%*%stat_weights_test)
            names(Scores)='Score'
            Scores = mutate(Scores, Name = df_test$Name, 
                            League = df_test$League, Pos = df_test$Pos, 
                            Rank = df_test$Rank,
                            Votes = df_test$`Vote Pts`) %>%
                group_by(League) %>%
                mutate(PredRank = rank(-Score, ties.method = 'average'))
            #test ordinal error of top 3
            test_error_3 = with(Scores %>%filter(Rank<=3), mean(abs(PredRank-Rank)))
            #test ordinal error of top 5
            test_error_5 = with(Scores %>%filter(Rank<=5), mean(abs(PredRank-Rank)))
            #accuracy on predicting the winner
            test_winner_accuracy = mean(Scores[Scores$Rank==1,'PredRank']==1)
            #pitcher accuracy
            SP = filter(Scores, Pos=='SP')
            if(nrow(SP)>0){
                sp_error = with(SP, mean(abs(PredRank-Rank)))
            }else{
                sp_error = 0
            }
            #RP accuracy
            RP = filter(Scores, Pos=='RP')
            if(nrow(RP)>0){
                rp_error = with(RP, mean(abs(PredRank-Rank)))
            }else{
                rp_error = 0
            }
            return(list(sol = sol, stat_weights = stat_weights_df, 
                        results = data.frame(Season = seasons[season_indx+1],  train_error = train_error,
                                             train_winner_error = train_winner_error,
                                             train_winner_accuracy = train_winner_accuracy,
                                             test_error_3 = test_error_3,
                                             test_error5 = test_error_5,
                                             test_winner_accuracy = test_winner_accuracy, 
                                             N = length(ObjCoefTest),
                                             sp_error=sp_error,
                                             n_sp = nrow(SP),
                                             rp_error = rp_error, 
                                             n_rp = nrow(RP))))
            
        }
        
    }else{
        #Just return train error
        return(list(sol = sol, stat_weights = stat_weights_df, 
                    results = data.frame(Season = seasons[season_indx],  train_error = train_error,train_winner_error = train_winner_error,
                                         train_winner_accuracy = train_winner_accuracy)))
    }
    
}


# Load MVP Historical data
AllMVP = readRDS('data/AllMVP3.RDS') 

#######################################
# Parallelize Model Train/Testing#
######################################

# cl <- makeCluster(7,  outfile = "")
# registerDoParallel(cl)
# pb <- txtProgressBar(min = 1, max = nrow(TuneParams), style = 3)
# TuneParams = expand.grid(lambda1 = 2^seq(-5,-1,.25),lambda2 =2^seq(-1,5,.25), top_n =c(5, 7, 10), weighted = c(T,F))
# newdf = foreach(i=1:nrow(TuneParams), .combine=rbind, .packages = c('dplyr','lpSolve','Matrix')) %dopar% {
# 
#     setTxtProgressBar(pb, i)
#     bat_stats = c('HRAA','RSAA','RBIAA','AVGplus','OBPplus','BClutch','B2HP','BatWAR','TWperc','Postseason')
#     sp_stats =c('WAA', 'SPERAplus','SPKAA','SPWHIPplus', 'SPClutch','SP2HP', 'SPWAR', 'SPTWperc','SPPostseason')
#     rp_stats = c('SVAA', 'RPERAplus','RPKAA','RPWHIPplus','RPClutch','RP2HP', 'RPWAR', 'RPTWperc','RPPostseason')
# 
#     n_seasons = 10
#     current_result = data.frame()
#     start_season = 1973+ n_seasons
#     for(s in start_season:2015){
#         result =solve_lp_fused_lasso(AllMVP, end = s, n_seasons = s-1973,
#                                top_n = TuneParams$top_n[i],
#                                weighted = TuneParams$weighted[i],
#                                bat_stats = bat_stats,
#                                sp_stats = sp_stats,
#                                rp_stats= rp_stats,
#                                only_batters = F,
#                                lambda1 = TuneParams$lambda1[i],
#                                lambda2 = TuneParams$lambda2[i],
#                                pos_penalty = T, 
#                                bl1 =TuneParams$bl1[i] ,
#                                spl1 = TuneParams$spl1[i],
#                                rpl1 = TuneParams$rpl1[i]
#         )$results
#         current_result = rbind(current_result, result)
#     }
# 
#     result = current_result %>% select(-Season)
#     all_error = select(result, -matches('winner|5|3')) %>%
#         summarise_each(funs(sum(.*N)/sum(N)), -N)
#     pos_error =result %>%
#         summarise(sp_error = sum(sp_error*n_sp)/sum(n_sp),
#                   rp_error = sum(rp_error*n_rp)/sum(n_rp))
#     winner_error = select(result, matches('winner|5|3')) %>%
#         summarise_each(funs(mean))
#     last5 = filter(result, row_number()>=n()-5) %>% select(matches('test_winner|5|3')) %>%
#         summarise_each(funs(mean)) %>%
#         setNames(., paste0('Last5_', names(.)))
#     bind_cols(all_error, winner_error, pos_error, last5)
# }
# e = Sys.time()-t
# Results = bind_cols(TuneParams, newdf)
# View(Results)


#######################################
# Parallelize Single Parameter Tests#
######################################

# bat_stats = c('HRAA','RSAA','RBIAA','AVGplus','OBPplus','BClutch','B2HP','BatWAR','TWperc','Postseason')
# sp_stats =c('WAA', 'SPERAplus','SPKAA','SPWHIPplus', 'SPClutch','SP2HP', 'SPWAR', 'SPTWperc','SPPostseason')
# rp_stats = c('SVAA', 'RPERAplus','RPKAA','RPWHIPplus','RPClutch','RP2HP', 'RPWAR', 'RPTWperc','RPPostseason')
# 
# n_seasons = 10
# start_season = 1973+ n_seasons
# cl <- makeCluster(6,  outfile = "")
# registerDoParallel(cl)
# pb <- txtProgressBar(min = start_season, max = 2015, style = 3)
# 
# current_result = foreach(s=start_season:2015, .combine=rbind, .packages = c('dplyr','lpSolve','Matrix')) %dopar% {
#     setTxtProgressBar(pb, s)
#     solve_lp_fused_lasso(AllMVP, end = s, n_seasons = s-1973,
#                                  top_n = 10,
#                                  weighted = F,
#                                  bat_stats = bat_stats,
#                                  sp_stats = sp_stats,
#                                  rp_stats= rp_stats,
#                                  only_batters = F,
#                                  lambda1 =.1, #0.105112052,
#                                  lambda2 = 6, #4.7568285,
#                                  pos_penalty = F, 
#                                  bl1 =0.105112052,
#                                  spl1 = 0.08,
#                                  rpl1 = 0.2
#     )$results
# 
# }
# 
# result = current_result %>% select(-Season)
# all_error = select(result, -matches('winner|5|3')) %>%
#     summarise_each(funs(sum(.*N)/sum(N)), -N)
# pos_error =result %>%
#     summarise(sp_error = sum(sp_error*n_sp)/sum(n_sp),
#                    rp_error = sum(rp_error*n_rp)/sum(n_rp))
# winner_error = select(result, matches('winner|5|3')) %>%
#     summarise_each(funs(mean))
# last5 = filter(result, row_number()>=n()-5) %>% select(matches('test_winner|5|3')) %>%
#     summarise_each(funs(mean)) %>%
#     setNames(., paste0('Last5_', names(.)))
# bind_cols(all_error, winner_error, pos_error, last5)
