#Question_1
arithmetic_operation <- function(k,n){
  addition <- k + n
  subtraction <- k - n
  multiplication <- k * n
  division <- k / n 
  remainder <- k %% n
  result = c(addition,subtraction,multiplication,division,remainder)
  x=round(result,digits=2)
  print("The sum,difference,product,quotient and remainder (respectively) of the numbers k and n are ")
  return(x)
}
arithmetic_operation(7,3)
arithmetic_operation(10,6)

#Question_2
quadratic_eqn <- function(a,b,c){
  
  root_1 = ((-b) + (b^2 - ((4*a)*c))^0.5) / 2*a
  root_2 = ((-b) - (b^2 - ((4*a)*c))^0.5) / 2*a
  roots = c(root_1,root_2)
  print("The roots of the quadratic equation of the real numbers a,b and c are")
  return(roots)
}
quadratic_eqn(1,5,6)
quadratic_eqn(4,3,0)
quadratic_eqn(1,-5,6)
quadratic_eqn(2,3,-5)








  
