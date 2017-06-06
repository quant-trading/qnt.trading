getFederalEfficientTaxRate = function(gross) 
{
  s = 0
  
  if(gross < 18451) {
  return( 0)
    }
  
  if(gross >= 18451 && gross < 74900) {
    s = s + (gross - 18451) * 0.15
    return(s / gross)
  }
  else
    s = s + (74900 - 18451)  * 0.15
 
  if(gross >= 74901 && gross < 151200)
  {
    s = s + (gross - 74901) * 0.25
    return(s / gross)
  }
  else 
    s = s + (151200 - 74901)  * 0.25
  
  if(gross >= 151201 && gross < 230450)
  {
    s = s + (gross - 151201) * 0.28
    return(s / gross)
  }
  else
    s = s + (230450 - 151201) * 0.28
  
  if(gross >= 230451 && gross < 411500)
  {
    s = s + (gross - 230451) * 0.33
    return(s / gross)
  }
  else
    s = s + (411500 - 230451)  * 0.33
  
  
  if(gross >= 411501 && gross < 464850)
  {
    s = s + (gross - 411501) * 0.35
    return(s / gross)
  }
  else
    s = s + (464850 - 411501) * 0.35
  
  if(gross >= 464851) {
    s = s + (gross - 464851) * 0.396
  }
    
    
  return(s / gross)
}


getStateEfficientTaxRate = function(gross) 
{
  
  # California Taxable Income	Rate
  # $0 - $15,700	1.00%
  # $15,700 - $37,220	2.00%
  # $37,220 - $58,744	4.00%
  # $58,744 - $81,546	6.00%
  # $81,546 - $103,060	8.00%
  # $103,060 - $526,444	9.30%
  # $526,444 - $631,732	10.30%
  # $631,732 - $1,052,886	11.30%
  # $1,052,886+	12.30%
  
  s = 0
  
  
  b1 = 0  
  b2 = 15700	
  r = 1.00 / 100
  if(gross >= b1 && gross < b2) {
    s = s + (gross - b1) * r
    return(s / gross)
  }
  else
    s = s + (b2 - b1)  * r

  b1 = 15700 
  b2 = 37220	
  r = 2.00 / 100  
  if(gross >= b1 && gross < b2) {
    s = s + (gross - b1) * r
    return(s / gross)
  }
  else
    s = s + (b2 - b1)  * r
  
  
  b1 = 37220
  b2 = 58744	
  r = 4.00 / 100
  if(gross >= b1 && gross < b2) {
    s = s + (gross - b1) * r
    return(s / gross)
  }
  else
    s = s + (b2 - b1)  * r
  
  b1 = 58744
  b2 = 81546	
  r = 6.00 / 100  
  if(gross >= b1 && gross < b2) {
    s = s + (gross - b1) * r
    return(s / gross)
  }
  else
    s = s + (b2 - b1)  * r

  b1 = 81546 
  b2 = 103060	
  r = 8.00 / 100  
  if(gross >= b1 && gross < b2) {
    s = s + (gross - b1) * r
    return(s / gross)
  }
  else
    s = s + (b2 - b1)  * r
  
  b1 = 103060
  b2 = 526444	
  r = 9.30 / 100  
  if(gross >= b1 && gross < b2) {
    s = s + (gross - b1) * r
    return(s / gross)
  }
  else
    s = s + (b2 - b1)  * r
  
  b1 = 526444
  b2 = 631732
  r = 10.30 / 100
  if(gross >= b1 && gross < b2) {
    s = s + (gross - b1) * r
    return(s / gross)
  }
  else
    s = s + (b2 - b1)  * r
  
  b1 = 631732
  b2 = 1052886
  r = 11.30 / 100
  if(gross >= b1 && gross < b2) {
    s = s + (gross - b1) * r
    return(s / gross)
  }
  else
    s = s + (b2 - b1)  * r

  
  b1 = 1052886
  r = 12.30 / 100  
  if(gross >= b1) {
    s = s + (gross - b1) * r
  }
  
  return(s / gross)
}


taxRate = function(gross, lowest = F)
{
  FICA = 0.0765
  if(lowest == F)
  { 
     getFederalEfficientTaxRate(gross) + getStateEfficientTaxRate(gross) + FICA
  }
  else
  {
    # Florida
    getFederalEfficientTaxRate(gross) + FICA
  }
}