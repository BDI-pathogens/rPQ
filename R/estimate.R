##################################################################/
# Descrption: pq
#
###################################################################/
pq = function(
  prob_train_negative,
  prob_train_positive,
  prob_test,
  n_bucket    = 10,
  stan.iter    = 1e3,
  stan.chains  = 1,
  stan.verbose = FALSE
)
{
  # define buckets from the training data probabilities
  dt_train <- data.table(
    prob  = c( prob_train_negative, prob_train_positive ),
    class = c( rep( 0, length( prob_train_negative ) ), rep( 1, length( prob_train_positive ) ) )
  )
  dt_train <- dt_train[ order( prob ) ]
  dt_train[ , bucket := floor( ( 0:(.N-1)) / .N * n_bucket ) + 1 ]

  # summarise thet training data by buckets
  dt_train_sum <- dt_train[ , .(
    n_pos = sum( class ),
    n_neg = .N - sum( class ),
    prob = min( prob )
  ), by = "bucket" ][ order( bucket ) ]

  # bucketise the test data and summarise
  dt_test <- data.table( prob = prob_test )[ order( prob_test ) ]
  dt_test <- dt_train_sum[ ,.( bucket, prob )][ dt_test, on = "prob", roll = TRUE, rollends = TRUE ]
  dt_test_sum <- dt_test[  ,.( n = .N), by = c( "bucket" ) ]
  if( dt_test_sum[ ,.N ] != n_bucket ) {
    dt_test_sum <- dt_test[ data.table( bucket = 1:n_bucket ), on = "bucket" ]
    dt_test_sum[ , n := ifelse( is.na( n ), 0, n ) ]
  }
  dt_test_sum <- dt_test_sum[ order( bucket ) ]

  # prepare data for stan
  stan_data <- list(
    n_bucket = as.integer( n_bucket ),
    train_neg = dt_train_sum[ , n_neg ],
    train_pos = dt_train_sum[ , n_pos ],
    test      = dt_test_sum[ , n ],
    posterior = TRUE
  )

  # run stan
  stan_model <- rpq:::stanmodels$pq
  if( stan.verbose ) {
    stan_raw <- sampling( stan_model, data = stan_data, iter = stan.iter, chains = stan.chains )
  } else {
    capture.output( {
      stan_raw <- sampling( stan_model, data = stan_data, iter = stan.iter, chains = stan.chains )
    } )
  }

  return( stan_raw )
}
