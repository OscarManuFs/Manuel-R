Blackscholes <- function(PV, Strike_Price, Risk_Free_i, Delta_T, Volatility) {
  values <- c(2)
  
  d1 <- (log(PV/Strike_Price)+(Risk_Free_i+(Volatility^2)/2)*(1/Delta_T))/(Volatility*sqrt(1/Delta_T))
  d2 <- d1 - Volatility * sqrt(1/Delta_T)
  
  values[1] <- PV*pnorm(d1) - Strike_Price*exp(-Risk_Free_i*(1/Delta_T))*pnorm(d2)
  values[2] <- Strike_Price*exp(-Risk_Free_i*(1/Delta_T)) * pnorm(-d2) - PV*pnorm(-d1)
  
  do.call(rbind,list(c("Call", "Put"), values))
}