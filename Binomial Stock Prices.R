Binomial_Stock_Option <- function(PV, Volatility, Delta_T, Time_Lapse, Risk_Free_i, type = "Call"/"Put") {
    
    
    tree = matrix(0, nrow=Time_Lapse+1, ncol=Time_Lapse+1)
    
    u = exp(Volatility*sqrt(1/Delta_T))
    d = exp(-Volatility*sqrt(1/Delta_T))
    
    for (i in 1:(Time_Lapse+1)) {
        for (j in 1:i) {
            tree[i,j] = PV * u^(j-1) * d^((i-1)-(j-1))
        }
    }
    
    q_prob = (exp(Risk_Free_i*(1/Delta_T)) - d)/(u-d)
    
    
    option_tree = matrix(0, nrow=nrow(tree), ncol=ncol(tree))
    if(type == 'Put') {
        option_tree[nrow(option_tree),] = pmax(PV - tree[nrow(tree),], 0)
    } else {
        option_tree[nrow(option_tree),] = pmax(tree[nrow(tree),] - PV, 0)
    }
    
    for (i in (nrow(tree)-1):1) {
        for(j in 1:i) {
            option_tree[i, j] = ((1-q_prob)*option_tree[i+1,j] + q_prob*option_tree[i+1,j+1])/exp(Risk_Free_i*(1/Delta_T))
        }
    }
    delta = (option_tree[2,2]-option_tree[2,1])/(tree[2,2]-tree[2,1])
    return(list(Risk.Neutral.No.Arbitrage.Probability = q_prob, Stock.Tree = t(tree), Option.Tree = t(option_tree), Option.Price = option_tree[1,1], Delta = delta))
}
  
  
  