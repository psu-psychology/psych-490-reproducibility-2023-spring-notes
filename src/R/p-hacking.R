n_pols <- dim(combinat::combn(c('pres', 'gov', 'senate', 'house'), 1))[2] +
dim(combinat::combn(c('pres', 'gov', 'senate', 'house'), 2))[2] +
dim(combinat::combn(c('pres', 'gov', 'senate', 'house'), 3))[2] +
1

n_econ <- n_pols

n_factors <- 4

n_better_worse <- 2

n_party <- 2

n_combos <- n_party * n_better_worse * n_pols * n_econ * n_factors 



