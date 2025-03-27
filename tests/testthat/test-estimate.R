library( rpq )
library( rstan )
library( data.table )

set.seed( 1 )

test_that("Coverage of posterior distributions and bias in medians", {
  # model test parameters
  n_train      <- 1e4
  n_test       <- 1e3
  p_true_train <- 0.5
  width        <- 0.5

  # pos and neg probability distributions
  p_pos_func <- function( n, width ) return( 1 / ( 1 + rexp( n, 1/ width ) ) )
  p_neg_func <- function( n, width ) return( 1 -  1 / ( 1 + rexp( n, 1/ width ) ) )

  # numerical parmeters for evaluating coverage
  p_sample  <- c( 0.25, 0.5, 0.75 )
  n_sample  <- 1e2

  post_probs = seq( 0.05, 0.95, 0.05 )
  posteriors = matrix( nrow = length( p_sample ) * n_sample, ncol = length( post_probs ) )
  idx = 1;

  for( sdx in 1:n_sample ) {
    # sample new training data
    train_pos <- p_pos_func( round( n_train * p_true_train), width )
    train_neg <- p_neg_func( n_train - round( n_train * p_true_train), width )

    for( ps in p_sample ) {
      # test data with prescribed prevalence
      test <- c(
        p_pos_func( round( n_test * ps ), width ),
        p_neg_func( n_test - round( n_test * ps ), width )
      )
      stan_raw <- pq( train_neg, train_pos, test )
      posteriors[ idx, ] <- quantile( extract( stan_raw )$prev,  prob = post_probs )
      idx = idx + 1
    }
  }

  # calculate the coverage and bias
  dt_res = as.data.table( posteriors )
  setnames( dt_res, 1:length( post_probs) , sprintf( "p_%02d", as.integer( post_probs * 100 ) ) )
  dt_res[ , actual := rep( p_sample, n_sample )  ]

  expect_lt( abs( dt_res[ , mean( p_50 - actual ) ] ), 5e-3 )
  expect_lt( abs( dt_res[ , mean( actual < p_75 & actual > p_25 ) ] - 0.5 ), 0.035 )
  expect_lt( abs( dt_res[ , mean( actual < p_95 & actual > p_05 ) ] - 0.9 ), 0.035 )
})
