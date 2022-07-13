library(Rlab)

# update S, fits for both single S vector and a matrices of S states for different patient
update.S <- function(A, S) {
  if (is.null(dim(S))) {
    eps.1 = rnorm(n = 1, mean = 0, sd = 1/4)
    eps.2 = rnorm(n = 1, mean = 0, sd = 1/4)
    new.S = c(
      (3/4)*(2*A - 1)*S[1] + (1/4)*S[1]*S[2] + eps.1,
      (3/4)*(1 - 2*A)*S[2] + (1/4)*S[1]*S[2] + eps.2
    )
  } else {
    eps.1 = rnorm(n = length(A), mean = 0, sd = 1/4)
    eps.2 = rnorm(n = length(A), mean = 0, sd = 1/4)
    new.S = matrix(nrow = length(A), ncol = 2)
    new.S[,1] = (3/4)*(2*A - 1)*S[,1] + (1/4)*S[,1]*S[,2] + eps.1
    new.S[,2] = (3/4)*(1 - 2*A)*S[,2] + (1/4)*S[,1]*S[,2] + eps.2
  }
  return(new.S)
}

# calculate the A.hat
calc.A.hat <- function(data, H) {
  n = length(data[,1])
  R.it = matrix(nrow = n, ncol = H)
  for (i in 1:H) {
    # utility function, representing long-term reward
    if (i == H) {  ########### TBC
      R.it[,i] = data[,c(3*i+1, 3*i+2)][,1]*2 + data[,c(3*i+1, 3*i+2)][,2] - (1/4)*(2*data[,3*i]-1)
    }
  }
  A.hat = matrix(nrow = n, ncol = H)
  for (j in 1:H) {
    A.hat[,j] = R.it[,j] - mean(R.it[,j])
  }
  return(R.it)
}

# policy to generate generate A_{i}^{t}
policy <- function(n, theta){
  return(rbern(n, theta))
}

# generate the data
# H: horizon, M: initial states as matrix (dim: n*2)
generate.data <- function(theta, H, M) {
  n = length(M[,1]) # n patients
  data = matrix(nrow = n, ncol = 3*(H+1))
  grads = matrix(nrow = n, ncol = H)
  cur_state = M
  data[,1] = cur_state[,1]
  data[,2] = cur_state[,2]
  data[,3] = cur_action = policy(n, theta)
  for(i in 1:H) {
    cur_state = update.S(cur_action, cur_state)
    data[,3*i+1] = cur_state[,1]
    data[,3*i+2] = cur_state[,2]
    cur_action = policy(n, theta)
    grad = (2 * cur_action - 1) / (cur_action*theta+(1-cur_action)*(1-theta))
    grads[,i] = grad
    data[,3*i+3] = cur_action
  }
  outlist = list(data=data, grads=grads)
  return(outlist)
}

# burn-in period to generate a list of states, representing initial states for n patients
burn.in <- function(n, theta) {
  # initiate states
  M = matrix(nrow = n, ncol = 2)
  for (i in 1:n) {
    M[i,] = rnorm(2)
  }
  # burn-in period
  for (iter in 1:50) {
    A.temp = policy(n, theta = theta)
    M = update.S(A.temp, M)
  }
  return(M)
}

estimate <- function(theta) {
  gamma = 0.9
  # randomize to start
  M = burn.in(100, 0.5)
  # collect the utility
  utils = rep(0, 100)
  for (p in 1:100) {
    A.temp = policy(100, theta)
    M = update.S(A.temp, M)
    r = M[,1]*2 + M[,2] - (1/4)*(2*A.temp-1)
    if (p == 100) {
      utils = utils + r*(gamma^(p-1)) ##### TBC
    }
  }
  return(mean(utils))
}

on.policy.gradient <- function(n, H, K, M) {
  theta = 0.5 # initial theta
  # apply on-policy policy gradient MC estimator
  gamma = 0.9
  alpha = 0.00001
  
  theta.vals = rep(-1, K)
  for (k in 1:K) {
    # generate data
    print(theta)
    gen = generate.data(theta = theta, H = H, M = M)
    data = gen$data
    grads = gen$grads
    A.hat = calc.A.hat(data, H)
    obj.grad = sum(grads*A.hat)
    theta = theta + alpha*obj.grad
    theta.vals[k] = theta
  }
  plot(theta.vals, type = "l")
  return(theta)
}


n = 25 # 25, 50, 100 - number of patients
H = 24 # 24, 36, 48  - horizon
M = burn.in(n, 0.5)
theta.hat = on.policy.gradient(n, H, 100000, M)
estimate(theta.hat)

