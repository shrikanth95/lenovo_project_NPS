library(MDPtoolbox)

get_stationary_distribution <- function(p)
{
  # Computes the stationary distribution mu of a Markov chain
  # described by p (stochastic matrix, ie sum(p,2)=1).
  # Input
  # p : transition matrix associated with a policy, p(s,s’)
  # Output
  # mu : stationary distribution for each state s ( p*mu’=mu’ )

  if (mdp_check_square_stochastic(p) != "")
  {
    warning("ERROR in get_stationary_distribution: argument p must be a stochastic matrix")
    mu = NA
  }
  else
  {
    mu <- array(0, dim(p))
    n <- nrow(p)
    # mu satisfies p*mu’=mu’ 
    A <- t(p) - diag(n)
    # and and mu sums to one
    A[n,] <- 1
    b <- matrix(0, nrow=n)
    b[n] <- 1
    # solve system of equations A*mu = b
    mu = solve(A) %*% b
  }
  
  mu
}


