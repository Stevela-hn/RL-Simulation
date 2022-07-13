# RL-Simulation
A collection of RL algorithms applied on Luckett paper's simulation setups

Some interpretations for "not self-explanatory" variable names I used in the script:

Parameter wise
`H`: number of horizon - in the paper it is set to be 24, 36, 48. \
`n`: number of patients sampled by each time of iteration.
`K`: number of iterations for this algorithm
`theta`: it is the key parameter we're trying to opt, represent the probability of doing treatment.

RL wise
`M`: initial states for the n patients after burn-in period - it is a n*2 matrix as "state" in this setting is defined to be a 2*1 vector.
`r`/`util`: long term reward, it is weird that the setting about values is super "short-sight" here.
`gamma`: discount rate
