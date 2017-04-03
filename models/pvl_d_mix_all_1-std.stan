# PVL-Delta in Stan

data {
  int<lower=1> n_s;                       // Total # subjects
  int<lower=0> n_s_1;                     // # subjects in group 1
  int<lower=0> n_s_2;                     // # subjects in group 2    
  int<lower=1> n_t;                       // # trials
  int<lower=0,upper=4> choice[n_s, n_t];  // # subj. x # trials matrix with 
                                          //   choices
  real<lower=-35,upper=2> net[n_s, n_t];  // Net amount of wins + losses    
                                          // (# subj. x # trials matrix) 
  vector<lower=0,upper=1>[4] mix;         // Prior mixing proportion for each z  
  int<lower=0,upper=1> human_choice[n_s, n_t];
  int<lower=0,upper=999> n_cards;  // Number of cards in a deck
}

parameters {
  // Group-level mean parameters
  vector[2] mu_pr[4];
  // Group-level standard deviation  
  vector<lower=0,upper=1.5>[4] std;
  // Individual-level paramters    
  vector[n_s] ind_pr[4];
}

transformed parameters {
  vector[2] lp_parts[4];
  // Individual-level paramters    
  vector<lower=0,upper=1>[n_s] A_ind; 
  vector<lower=0,upper=5>[n_s] w_ind; 
  vector<lower=0,upper=1>[n_s] a_ind;   
  vector<lower=0,upper=5>[n_s] c_ind;   
  
  for (s in 1:n_s) {
    A_ind[s] = Phi(ind_pr[1,s]);
    w_ind[s] = Phi(ind_pr[2,s]) * 5;
    a_ind[s] = Phi(ind_pr[3,s]);    
    c_ind[s] = Phi(ind_pr[4,s]) * 5;  
  }
  
  for (i in 1:4) {
    lp_parts[i,1] = log(mix[i]) + normal_log(tail(ind_pr[i], n_s_2),
                                               mu_pr[i,1], std[i]);
    lp_parts[i,2] = log1m(mix[i]) + normal_log(tail(ind_pr[i], n_s_2),
                                                 mu_pr[i,2], std[i]);
  }
}
model {
  // Prior on the group-level mean parameters
  // probit scale [-Inf, Inf] 
  for (i in 1:4)
    mu_pr[i] ~ normal(0, 1);
    
  // Individual-level paramters
  // Group 1  
  for (i in 1:4)  
    for (s in 1:n_s_1)
      ind_pr[i,s] ~ normal(mu_pr[i,1], std[i]);      
  // Group 2 
  for (i in 1:4)  
    increment_log_prob(log_sum_exp(lp_parts[i]));
  
  for (s in 1:n_s) {  // loop over subjects
    vector[4] p;
    vector[4] Ev;
    real theta;
    vector[4] drawCount;
    
    // Trial 1
    p = rep_vector(.25, 4);  // assign .25 to all elements of p
    Ev = rep_vector(0, 4);   // assign 0 to all elements of Ev
    drawCount = rep_vector(0, 4);
    
    theta = 3 ^ c_ind[s] - 1;     
    choice[s,1] ~ categorical(p);

    // Remaining trials
    for (t in 1:(n_t - 1)) {
      real v;
      int currentDeck;
      
      currentDeck = choice[s,t];
      drawCount[currentDeck] = drawCount[currentDeck] + 1;
     
      if (net[s,t] >= 0)
        v = net[s,t] ^ A_ind[s];
      else
        v = -w_ind[s] * fabs(net[s,t]) ^ A_ind[s];

      // If one of the decks is depleted 
      Ev[currentDeck] = if_else(drawCount[currentDeck] == n_cards, -999999,
                                 (1 - a_ind[s]) * Ev[choice[s,t]] + a_ind[s] * v);  

      if (human_choice[s,t+1] == 1)
        choice[s,t+1] ~ categorical_logit(Ev * theta); 
    }
  }
}
generated quantities {
  int<lower=0,upper=1> z[4];  // high values of z indicate group distribution 
                              // to be different
  vector<lower=0,upper=1>[2] mu_A; 
  vector<lower=0,upper=5>[2] mu_w; 
  vector<lower=0,upper=1>[2] mu_a;   
  vector<lower=0,upper=5>[2] mu_c; 
  
  for (i in 1:2) {
    mu_A[i] = Phi(mu_pr[1,i]);
    mu_w[i] = Phi(mu_pr[2,i]) * 5;
    mu_a[i] = Phi(mu_pr[3,i]);
    mu_c[i] = Phi(mu_pr[4,i]) * 5;
  }
  for (i in 1:4) {
    vector[2] prob;
    prob = softmax(lp_parts[i]);
    z[i] = bernoulli_rng(prob[2]); 
  }
}