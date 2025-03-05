library("qrmtools")
library("tidyverse")
library('ggthemes')
library('viridis')

reportPath <- "."
pdf(NULL)

changeInD2E <- 3 #days
sigma <- 0.15

strikeDf <- data.frame(STRIKE = 0.0, TYPE = "", D2E = 0, 
                       P1 = 0.0, P2 = 0.0, 
                       DELTA1 = 0.0, DELTA2 = 0.0,
                       THETA1 = 0.0, THETA2 = 0.0, 
                       RHO1 = 0.0, RHO2 = 0.0, 
                       VEGA1 = 0.0, VEGA2 = 0.0, 
                       GAMMA1 = 0.0, GAMMA2 = 0.0)

for(d in seq(-10, 10, by=2)){
  strike <- 100 + d
  for(o in seq(10, 20, by=1)){
    pc1 <- Black_Scholes(0, 100, 0, sigma, strike, o/250, "call")
    pc2 <- Black_Scholes(0, 100, 0, sigma, strike, (o-changeInD2E)/250, "call")
    
    gc1 <- Black_Scholes_Greeks(0, 100, 0, sigma, strike, o/250, "call")
    gc2 <- Black_Scholes_Greeks(0, 100, 0, sigma, strike, (o-changeInD2E)/250, "call")
    
    strikeDf <- rbind(strikeDf, c(strike, "CALL", o, 
                                  pc1, pc2,
                                  gc1[['delta']],gc2[['delta']],
                                  gc1[['theta']],gc2[['theta']],
                                  gc1[['rho']],gc2[['rho']],
                                  gc1[['vega']],gc2[['vega']],
                                  gc1[['gamma']],gc2[['gamma']]))
    

    
    pp1 <- Black_Scholes(0, 100, 0, sigma, strike, o/250, "put")
    pp2 <- Black_Scholes(0, 100, 0, sigma, strike, (o-changeInD2E)/250, "put")
    
    gc1 <- Black_Scholes_Greeks(0, 100, 0, sigma, strike, o/250, "put")
    gc2 <- Black_Scholes_Greeks(0, 100, 0, sigma, strike, (o-changeInD2E)/250, "put")
    
    strikeDf <- rbind(strikeDf, c(strike, "PUT", o, 
                                  pp1, pp2,
                                  gc1[['delta']],gc2[['delta']],
                                  gc1[['theta']],gc2[['theta']],
                                  gc1[['rho']],gc2[['rho']],
                                  gc1[['vega']],gc2[['vega']],
                                  gc1[['gamma']],gc2[['gamma']]))
  }
}

strikeDf <- strikeDf[-1,]
strikeDf <- strikeDf |> mutate(across(-c(TYPE), as.numeric))

ggplot(strikeDf |> filter(TYPE == 'CALL'), aes(x=STRIKE - 100, y = 100*(P2/P1-1), color=as.factor(D2E))) +
  theme_economist() +
  theme(legend.position = "right") +
  geom_line() +
  scale_color_viridis_d() +
  labs(x='strike', y = '% change in premium', color = 'orig. d2e',
       title = 'Change in Call Premium',
       subtitle = 'moneyness vs. days to expiry')

ggsave(sprintf("%s/call-premium.png", reportPath), width = 10, height = 5, units = "in")

ggplot(strikeDf |> filter(TYPE == 'PUT'), aes(x=STRIKE - 100, y = 100*(P2/P1-1), color=as.factor(D2E))) +
  theme_economist() +
  theme(legend.position = "right") +
  geom_line() +
  scale_color_viridis_d() +
  labs(x='strike', y = '% change in premium', color = 'orig. d2e',
       title = 'Change in Put Premium',
       subtitle = 'moneyness vs. days to expiry')

ggsave(sprintf("%s/put-premium.png", reportPath), width = 10, height = 5, units = "in")

###############

ggplot(strikeDf |> filter(TYPE == 'CALL'), aes(x=STRIKE - 100, y = 100*(DELTA2/DELTA1-1), color=as.factor(D2E))) +
  theme_economist() +
  theme(legend.position = "right") +
  geom_line() +
  scale_color_viridis_d() +
  labs(x='strike', y = '% change in delta', color = 'orig. d2e',
       title = 'Change in Call delta',
       subtitle = 'moneyness vs. days to expiry')

ggsave(sprintf("%s/call-delta.png", reportPath), width = 10, height = 5, units = "in")

ggplot(strikeDf |> filter(TYPE == 'PUT'), aes(x=STRIKE - 100, y = 100*(DELTA2/DELTA1-1), color=as.factor(D2E))) +
  theme_economist() +
  theme(legend.position = "right") +
  geom_line() +
  scale_color_viridis_d() +
  labs(x='strike', y = '% change in delta', color = 'orig. d2e',
       title = 'Change in Put delta',
       subtitle = 'moneyness vs. days to expiry')

ggsave(sprintf("%s/put-delta.png", reportPath), width = 10, height = 5, units = "in")

ggplot(strikeDf |> filter(TYPE == 'CALL'), aes(x=STRIKE - 100, y = 100*(THETA2/THETA1-1), color=as.factor(D2E))) +
  theme_economist() +
  theme(legend.position = "right") +
  geom_line() +
  scale_color_viridis_d() +
  labs(x='strike', y = '% change in theta', color = 'orig. d2e',
       title = 'Change in Call theta',
       subtitle = 'moneyness vs. days to expiry')

ggsave(sprintf("%s/call-theta.png", reportPath), width = 10, height = 5, units = "in")

ggplot(strikeDf |> filter(TYPE == 'PUT'), aes(x=STRIKE - 100, y = 100*(THETA2/THETA1-1), color=as.factor(D2E))) +
  theme_economist() +
  theme(legend.position = "right") +
  geom_line() +
  scale_color_viridis_d() +
  labs(x='strike', y = '% change in theta', color = 'orig. d2e',
       title = 'Change in Put theta',
       subtitle = 'moneyness vs. days to expiry')

ggsave(sprintf("%s/put-theta.png", reportPath), width = 10, height = 5, units = "in")

ggplot(strikeDf |> filter(TYPE == 'CALL'), aes(x=STRIKE - 100, y = 100*(GAMMA2/GAMMA1-1), color=as.factor(D2E))) +
  theme_economist() +
  theme(legend.position = "right") +
  geom_line() +
  scale_color_viridis_d() +
  labs(x='strike', y = '% change in gamma', color = 'orig. d2e',
       title = 'Change in Call gamma',
       subtitle = 'moneyness vs. days to expiry')

ggsave(sprintf("%s/call-gamma.png", reportPath), width = 10, height = 5, units = "in")

ggplot(strikeDf |> filter(TYPE == 'PUT'), aes(x=STRIKE - 100, y = 100*(GAMMA2/GAMMA1-1), color=as.factor(D2E))) +
  theme_economist() +
  theme(legend.position = "right") +
  geom_line() +
  scale_color_viridis_d() +
  labs(x='strike', y = '% change in gamma', color = 'orig. d2e',
       title = 'Change in Put gamma',
       subtitle = 'moneyness vs. days to expiry')

ggsave(sprintf("%s/put-gamma.png", reportPath), width = 10, height = 5, units = "in")
