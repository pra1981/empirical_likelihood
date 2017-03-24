set.seed(2017908)
x=rexp(100)
mean_x = mean(x)

h_lambda = function(lambda, data, theta){
  return(sum((data - theta) / (1 + lambda * (data - theta))))
}

R_n = function(theta, data){
  n = length(data)
  low_bound = (1/n - 1) / (max(data) - theta)
  high_bound = (1/n - 1) / (min(data) - theta)
  lambda = uniroot(h_lambda, c(low_bound, high_bound), tol = 1e-9, theta = theta, data=data)$root
  return(-sum(log(1 + lambda * (data - theta))))
}

theta_grid = seq(min(x)+1e-3, 6.3, 1e-3)
R_grid = numeric(length(theta_grid))
for(i in 1:length(theta_grid)){
  R_grid[i] = R_n(theta_grid[i], x)
}
plot(theta_grid, R_grid, type = 'l',xlab = 'theta', ylab = 'R')
l = min(theta_grid[-2 * R_grid < qchisq(0.95, 1)]) # lower bound of CI
h = max(theta_grid[-2 * R_grid < qchisq(0.95, 1)]) # higher bound of CI
(l+h)/2
mean_x
