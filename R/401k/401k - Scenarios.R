source("efficientTaxRate.R")

GROSS.INCOME = 115000
EXPENSES = 5500
INITIAL.401K = 12000
INFLATION = 0.015
MAX.401k = 18000
PERIOD.401k = 5
PENSION.LOWER.K = 0.9

RUSSIA.RATE = 0.13
LTCG.RATE = 0.3

r = 0.07 - INFLATION

AGE = 33
AGE.MAX = 70

PENALTY = 0.1
PENALTY.BOUND = 59.5

MATCH = GROSS.INCOME / 12 * 0.05 * (0.6 + 0.4 * 0.5)


TAX.RATE.NONRESIDENT = 0.3
TAX.RATE.RESIDENT = 
N = length(seq(from = AGE, to = AGE.MAX, by = 1/12))


cashOut401k = function(s, i, gross)
{
  if(i > PENALTY.BOUND) {
    return( s * (1 - taxRate(gross + s / PERIOD.401k)) )
  }
  else
  {
    return( s * (1 - taxRate(gross + s / PERIOD.401k, lowest = T) - PENALTY) )
  }
}

cashOut401k_NonResident = function(s, i, gross)
{
  if(i > PENALTY.BOUND) {
    return( s * (1 - TAX.RATE.NONRESIDENT) )
  }
  else
  {
    return( s * (1 - TAX.RATE.NONRESIDENT - PENALTY) )
  }
}


#--------------------------------------------------------------------------------------
#       No 401k scenario
#--------------------------------------------------------------------------------------
scenario.no401k = NULL
scenario.no401k$p = rep(0, time = N)
scenario.no401k$i = rep(0, time = N)
scenario.no401k$n = rep(0, time = N)

a401k = INITIAL.401K
invest = 0
cost = 0

k = 1

for(i in seq(from = AGE, to = AGE.MAX, by = 1/12))
{
    free.cash = GROSS.INCOME * (1 - taxRate(GROSS.INCOME)) / 12 - EXPENSES
    
    if(i < PENALTY.BOUND)
    { 
      invest = invest * (1 + r / 12) + free.cash
      a401k =  a401k * (1 + r / 12)
      scenario.no401k$n[k] = invest * (1 - LTCG.RATE) + cashOut401k(a401k, i, GROSS.INCOME) - (taxRate(a401k/ PERIOD.401k + GROSS.INCOME) - taxRate(GROSS.INCOME) ) * GROSS.INCOME
    } else {
      invest = invest * (1 + r / 12) 
      a401k =  a401k * (1 + r / 12) 
      cost = cost  - EXPENSES * PENSION.LOWER.K
      scenario.no401k$n[k] = invest * (1 - LTCG.RATE) + cashOut401k(a401k, i, 0) + cost
    }
    
    scenario.no401k$p[k] = a401k
    scenario.no401k$i[k] = invest
    
    k = k + 1
}

#--------------------------------------------------------------------------------------
#       401k minimum scenario
#--------------------------------------------------------------------------------------
scenario.min401k = NULL
scenario.min401k$p = rep(0, time = N)
scenario.min401k$i = rep(0, time = N)
scenario.min401k$n = rep(0, time = N)

a401k = INITIAL.401K
invest = 0
cost = 0

k = 1

for(i in seq(from = AGE, to = AGE.MAX, by = 1/12))
{
  if(i < PENALTY.BOUND)
  {
   free.cash = (GROSS.INCOME * (1 - 0.05)) / 12  * (1 - taxRate(GROSS.INCOME) * (1 - 0.05)) - EXPENSES
  
   invest = invest * (1 + r / 12) + free.cash
   a401k =  a401k * (1 + r / 12) + MATCH + GROSS.INCOME / 12 * 0.05
   scenario.min401k$p[k] = a401k
   scenario.min401k$i[k] = invest
  
   scenario.min401k$n[k] = invest * (1 - LTCG.RATE) + cashOut401k(a401k, i, GROSS.INCOME * (1 - 0.05))  - (taxRate(a401k/ PERIOD.401k + GROSS.INCOME* (1 - 0.05)) - taxRate(GROSS.INCOME* (1 - 0.05)) ) * GROSS.INCOME* (1 - 0.05)
  }
  else
  {
    free.cash = 0
    invest = invest * (1 + r / 12) + free.cash
    a401k =  a401k * (1 + r / 12) 
    scenario.min401k$p[k] = a401k
    scenario.min401k$i[k] = invest
    cost = cost  - EXPENSES * PENSION.LOWER.K
    scenario.min401k$n[k] = invest * (1 - LTCG.RATE) + cashOut401k(a401k, i, 0) + cost
    
  }
  
  k = k + 1
}

#--------------------------------------------------------------------------------------
#       401k 18000 scenario
#--------------------------------------------------------------------------------------
scenario.max401k = NULL
scenario.max401k$p = rep(0, time = N)
scenario.max401k$i = rep(0, time = N)
scenario.max401k$n = rep(0, time = N)

a401k = INITIAL.401K
invest = 0
cost = 0

k = 1

for(i in seq(from = AGE, to = AGE.MAX, by = 1/12))
{
  if(i < PENALTY.BOUND)
  {
    free.cash = (GROSS.INCOME - MAX.401k) / 12  * (1 - taxRate(GROSS.INCOME - MAX.401k)) - EXPENSES
    
    invest = invest * (1 + r / 12) + free.cash
    a401k =  a401k * (1 + r / 12) + MATCH + MAX.401k / 12
    scenario.max401k$p[k] = a401k
    scenario.max401k$i[k] = invest
    
    scenario.max401k$n[k] = invest * (1 - LTCG.RATE) + cashOut401k(a401k, i, GROSS.INCOME - MAX.401k)  - (taxRate(a401k/ PERIOD.401k + (GROSS.INCOME - MAX.401k)) - taxRate((GROSS.INCOME - MAX.401k)) ) * (GROSS.INCOME - MAX.401k)
  }
  else
  {
    free.cash = 0
    invest = invest * (1 + r / 12) + free.cash
    a401k =  a401k * (1 + r / 12) 
    scenario.max401k$p[k] = a401k
    scenario.max401k$i[k] = invest
    cost = cost  - EXPENSES * PENSION.LOWER.K    
    scenario.max401k$n[k] = invest * (1 - LTCG.RATE) + cashOut401k(a401k, i, 0) + cost

  }
  k = k + 1
}

#--------------------------------------------------------------------------------------
#       401k NON-RESIDENT scenario
#--------------------------------------------------------------------------------------
scenario.rus401k = NULL
scenario.rus401k$p = rep(0, time = N)
scenario.rus401k$i = rep(0, time = N)
scenario.rus401k$n = rep(0, time = N)

a401k = INITIAL.401K
invest = 0
cost = 0

k = 1

for(i in seq(from = AGE, to = AGE.MAX, by = 1/12))
{
  if(i < PENALTY.BOUND)
  {
    free.cash = (GROSS.INCOME - MAX.401k) / 12  * (1 - taxRate(GROSS.INCOME - MAX.401k)) - EXPENSES
    
    invest = invest * (1 + r / 12) + free.cash
    a401k =  a401k * (1 + r / 12) + MATCH + MAX.401k / 12
    scenario.rus401k$p[k] = a401k
    scenario.rus401k$i[k] = invest
    
    scenario.rus401k$n[k] = invest * (1 - LTCG.RATE) + cashOut401k_NonResident(a401k, i, GROSS.INCOME - MAX.401k)  - (taxRate(a401k/ PERIOD.401k + (GROSS.INCOME - MAX.401k)) - taxRate((GROSS.INCOME - MAX.401k)) ) * (GROSS.INCOME - MAX.401k)
  } else {
    
    free.cash = 0
    invest = invest * (1 + r / 12) + free.cash
    a401k =  a401k * (1 + r / 12) 
    scenario.rus401k$p[k] = a401k
    scenario.rus401k$i[k] = invest
    cost = cost  - EXPENSES * PENSION.LOWER.K
    scenario.rus401k$n[k] = invest * (1 - LTCG.RATE) + cashOut401k_NonResident(a401k, i, GROSS.INCOME - MAX.401k) + cost
    
  }
    
  k = k + 1
}


# x = seq(1, 1500000, by = 100)
# #y = sapply(x, getFederalEfficientTaxRate)
# y = sapply(x, getStateEfficientTaxRate)
# 
# plot(x, y)

x = seq(from = AGE, to = AGE.MAX, by = 1/12)

plot(x, scenario.rus401k$n, type = "l")
lines(x, scenario.min401k$n, col = "green")
lines(x, scenario.max401k$n, col = "red")
lines(x, scenario.no401k$n, col = "blue")