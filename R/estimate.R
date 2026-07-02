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
  stan_model <- rpq:::stanmodels$PQ
  if( stan.verbose ) {
    stan_raw <- rstan::sampling( stan_model, data = stan_data, iter = stan.iter, chains = stan.chains )
  } else {
    capture.output( {
      stan_raw <- rstan::sampling( stan_model, data = stan_data, iter = stan.iter, chains = stan.chains )
    } )
  }

  return( stan_raw )
}

##################################################################/
# Descrption: pq_mult
#
###################################################################/
pq_mult = function(
    prob_train,
    class_train,
    prob_test,
    n_bucket    = 10,
    stan.iter    = 1e3,
    stan.chains  = 1,
    stan.verbose = FALSE
)
{
  # check inputs
  stopifnot( is.matrix( prob_train ) )
  stopifnot( is.vector( class_train ) )
  stopifnot( is.matrix( prob_train ) )

  n_train <- nrow( prob_train )
  n_class <- ncol( prob_train )
  n_test  <- nrow( prob_test )
  stopifnot( length( class_train ) == n_train )
  stopifnot( ncol( prob_test ) == n_class )

  # class train must be full of integers and include all posibilities
  class_train <- as.integer( class_train )
  stopifnot( min( class_train ) == 1 )
  stopifnot( max( class_train ) == n_class )
  stopifnot( length( unique( class_train ) ) == n_class )

  # make sure probabilites are normalised properly
  stopifnot( min( prob_train ) >= 0 );
  prob_train <- prob_train / rowSums( prob_train )

  # put data in dt
  dt_train[ , class := class_train ]

  # get buckets using k-means clustering
  clusters <- kmeans( rbind( prob_train, prob_test ), n_bucket )

  # summarise thet training data by buckets
  cluster_cols <- sprintf( "cluster_%.0f", seq( 1, n_bucket ) )
  dt_train[ , bucket := sprintf( "cluster_%.0f", clusters$cluster[ 1:n_train ] ) ]
  dt_train_sum <- dcast.data.table( dt_train, class ~ bucket, fun.aggregate = length )[ order( class ) ]
  dt_train_sum <- dt_train_sum[ , .SD, .SDcols = cluster_cols ]
  setcolorder( dt_train_sum, cluster_cols )

  # bucketise the test data and summarise
  dt_test <- data.table( cluster = clusters$cluster[ (n_train+1):(n_train+n_test) ] )
  dt_test <- dt_test[ , .( count = .N ), by = "cluster" ][ data.table( cluster = 1:n_bucket ), on = "cluster" ][ order( cluster )]
  dt_test[ , count := ifelse( is.na( count ), 0, count ) ]

  # prepare data for stan
  stan_data <- list(
    n_class = n_class,
    n_bucket = n_bucket,
    train = as.matrix( dt_train_sum ),
    test = dt_test[, count ],
    posterior = TRUE
  )

  # run stan
  stan_model <- rpq:::stanmodels$PQ_mult
  if( stan.verbose ) {
    stan_raw <- rstan::sampling( stan_model, data = stan_data, iter = stan.iter, chains = stan.chains )
  } else {
    capture.output( {
      stan_raw <- rstan::sampling( stan_model, data = stan_data, iter = stan.iter, chains = stan.chains )
    } )
  }

  return( stan_raw )
}

