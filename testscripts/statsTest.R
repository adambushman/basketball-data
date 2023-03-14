##############
#Stats Testing
##############

#Binomial problem
#--------------------
#Donovan Mitchell
#Career high 3PAs: 16
#2020-21 3P%: 0.386
#--------------------

biball = function (x,n,g,p) {
  r = 0
  
  for(i in 0:n) {
    if (i <= x) {
      r = r + dbinom(i,n,p,log = FALSE)
    } #end if
    
  } #end for
  
  if (g == 1) {
    r = (1 - r)
  }
  
  return(r)
} #end function


biball(4,15,0,0.08)

.2+(.3*2)+(.5*3)

