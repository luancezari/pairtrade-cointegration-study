# Result analysis

# Sera relevante avaliar:
#  - Os trades individualmente
#  - As series individuais de trade
#  - O portfolio formado pelas series de trades individuais

# Metricas de desempenho a avaliar
## Para trades individuais:
#
## Para series de trades e portfolio:
# Retorno anualizado
# Volatilidade
# Sharpe
# Drawdown
# Duração do drawdown
# E[MDD]

# Avaliar resultando controlando pelas variaveis:
# Correlacao no momento do trade
# Tempo de meia vida estimado
# n de lookback periods cointegrados
# Par pertence ao mesmo supersetor?
# Regime de mercado (?)

# Hipoteses a serem avaliadas:
# Trades com maior correlacao tem taxas de acerto maiores?

###
##### DATA WRANGLING -----
###

# Isola dados dos trades individuais


extract_trades_info <- function(result){
  
  all_trades_output <- NULL
  
  for(pair in result){
    
    if(is.null(pair$Globals$n_signif)) next
    
    returns_ <- tibble(n_signif = pair$Globals$n_signif,
                       r2 = pair$Globals$r2,
                       half_life = pair$Globals$half_life,
                       best_period = pair$Globals$best_period) %>%
      mutate(pair = paste(pair$instrument[1], pair$instrument[2], sep = '-'), .before = 1) %>%
      na.omit()
    
    if(length(unique(pair$journal$timestamp))%%2==0){
      returns_ <- returns_ %>%
        cbind(start_date = unique(pair$journal$timestamp)[seq(1, length(pair$journal$timestamp)/2, 2)],
              end_date = unique(pair$journal$timestamp)[seq(2, length(pair$journal$timestamp)/2, 2)]) %>%
        mutate(trade_days = end_date - start_date)
    } else{
      returns_ <- returns_ %>%
        cbind(start_date = unique(pair$journal$timestamp)[seq(1, length(pair$journal$timestamp)/2, 2)],
              end_date = c(unique(pair$journal$timestamp)[seq(2, length(pair$journal$timestamp)/2, 2)], 
                           last(pair$timestamp))) %>%
        mutate(trade_days = end_date - start_date)
    }
    
    wealth <- tibble(
      date = pair$timestamp,
      wealth = pair$wealth
    )
    
    returns_ <- returns_ %>%
      left_join(wealth, by = c('start_date' = 'date')) %>%
      rename(start_wealth = wealth) %>%
      left_join(wealth, by = c('end_date' = 'date')) %>%
      rename(end_wealth = wealth) %>%
      mutate(return = end_wealth/start_wealth-1)
    
    all_trades_output <- rbind(all_trades_output, returns_)
    
  } 
  
  return(tibble(all_trades_output))
  
}

all_trades_output <- extract_trades_info(result)

total_return <- NULL
for(i in 1:240){
  trade_info <- tibble(pair = paste(result[[i]]$instrument[1], result[[i]]$instrument[2], sep = '-'),
                       return = last(result[[i]]$wealth)/10^6-1)
  
  total_return <- rbind(total_return, trade_info)
}

###
##### EDA -----
###

all_trades_output %>%
  mutate(r2 = arules::discretize(r2, method = 'interval', breaks = 21, labels = seq(0, 100, 5)/100)) %>%
  ggplot(aes(r2, return)) +
  geom_boxplot()