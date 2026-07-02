data {
  int<lower=2> n_bucket;
  int<lower=2> n_class;
  array[n_class,n_bucket] int<lower=0> train;
  array[n_bucket] int<lower=0> test;
  int<lower=0,upper=1> posterior;
}

transformed data{
  matrix<lower=0>[n_class,n_bucket] train_mat;
  vector<lower=0>[n_bucket] test_v;
  real n_test;

  train_mat = to_matrix( train );
  test_v    = to_vector( test );
  n_test    = sum( test );
}

parameters {
  array[n_class] simplex[n_bucket] p_class;
  simplex[ n_class ] prev_prior;
}

transformed parameters{
  matrix[n_class,n_bucket] p_class_mat;
  row_vector[n_class] prev_prior_rv = to_row_vector( prev_prior );
  for( idx in 1:n_class )
    p_class_mat[ idx, ] = to_row_vector( p_class[ idx ] );
}

model {
  if( posterior ) {
    target += sum( train_mat .* log( p_class_mat ) );
    target += log( prev_prior_rv * p_class_mat ) * test_v;
  }
}

generated quantities {
  vector[n_class] prev = rep_vector( 0, n_class );
  vector[n_class] theta;

  for( bdx in 1:n_bucket ) {
    theta =  p_class_mat[ , bdx ] .* prev_prior;
    theta = theta / sum( theta );
    prev = prev + to_vector(multinomial_rng( theta, test[ bdx ] ) );
  }
  prev = prev / n_test;
}
