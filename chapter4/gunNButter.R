library(lpSolve)
library(genalg)

#objective function
# maximize: 195 * machine_gun + 150 * butter
#subject to constraints:
# 1) 150 * machine_gun + 100 * butter <= 1800
# 2) 0.5 * machine_gun + 1.5 * butter <= 21

f.obj = c(195,150)
f.con = matrix(c(150,100,0.5,1.5),nrow=2,byrow=TRUE)
f.dir = c("<=","<=")
f.rhs = c(1800,21)

#solve lp
lp_solve = lp("max",f.obj,f.con,f.dir,f.rhs)
print(sprintf("maximized profit is %f$, at %f machine guns and %f tons of butter produced per month",lp_solve$objval,lp_solve$solution[1],lp_solve$solution[2]))

#but #machine_gun need to be an integer!
int.vec = c(1)
#solve realistic lp
lp_solve = lp("max",f.obj,f.con,f.dir,f.rhs,int.vec = int.vec)
print(sprintf("realistic maximized profit is %f$, at %f machine guns and %f tons of butter produced per month",lp_solve$objval,lp_solve$solution[1],lp_solve$solution[2]))

#we get 500$ bonus if we sell 5 or more machine guns
#apply "Evolutionary" algorithm

#return -1 * total_score b/c GA solve for a minimum objective value!
evalFunction <- function(x){
  machine_gun = x[1]
  butter = x[2]
  
  if(!is.integer(machine_gun)){
    machine_gun = as.integer(machine_gun)
  }
  
  #return 0 if the constrains are not hold
  if(150 * machine_gun + 100 * butter > 1800){
    return(0)
  }
  if(0.5 * machine_gun + 1.5 * butter > 21){
    return(0)
  }
  
  if(machine_gun >= 5){
    return( -1 * (195 * machine_gun + 150 * butter + 500) )
  }else{
    return( -1 * (195 * machine_gun + 150 * butter) )
  }
}

#solve for the solution
min_values = c(0,0)
max_values = c(25,25)
gen_alg = rbga(min_values,max_values,evalFunc = evalFunction,verbose = TRUE)
plot(gen_alg)
print(summary(gen_alg))
