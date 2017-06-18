/* PVL-Delta model in Stan


*/

data {
  int<lower=1> n_s;                        // Number of subjects
  int<lower=1> n_t;                        // Number of trials
  int<lower=1,upper=4> choice[n_s,n_t];    // Matrix of deck choices
  real<lower=-11.5,upper=1> net[n_s,n_t];  // Matrix of net gains   
}

parameters {
  // Individual-level paramters    
  vector<lower=0,upper=1>[n_s] A_ind; 
  vector<lower=0,upper=5>[n_s] w_ind; 
  vector<lower=0,upper=1>[n_s] a_ind;   
  vector<lower=0,upper=5>[n_s] c_ind;   
}

model {
  for (s in 1:n_s) {  // loop over subjects
    vector[4] Ev;
    real theta;
    
    theta = 3 ^ c_ind[s] - 1;     
   
    // Assign 0 to all elements of Ev
    Ev = rep_vector(0, 4);
    
    for (t in 1:(n_t - 1)) {
      real v;
     
      if (net[s,t] >= 0)
        v = net[s,t] ^ A_ind[s];
      else
        v = -w_ind[s] * fabs(net[s,t]) ^ A_ind[s];

      // If one of the decks is depleted 
      Ev[choice[s,t]] = (1 - a_ind[s]) * Ev[choice[s,t]] + a_ind[s] * v;  

      choice[s,t+1] ~ categorical_logit(Ev * theta); 
    }
  }
}
