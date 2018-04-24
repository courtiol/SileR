#include <Rcpp.h>

// The following is needed for Roxygen2 to create the namespace properly
//' @useDynLib SileR
//' @importFrom Rcpp evalCpp

// C++ Function power for vectors (not exported)
// [[Rcpp::export]]
Rcpp::NumericVector vpowC(Rcpp::NumericVector vec1, Rcpp::NumericVector vec2) {
  int n = vec1.size();
  Rcpp::NumericVector res(n);
  for(int i = 0; i < n; i++) {
    res[i] = std::pow(vec1[i], vec2[i]);
  }
  return res;
}

// C++ Function preventing probabilities to be too extreme
// [[Rcpp::export]]
Rcpp::NumericVector unboundC(Rcpp::NumericVector x) {
    int n = x.size();
    for(int i = 0; i < n; i++) {
      if(x[i] >= 1) {
        x[i] = 1 - 1e-9;
      } else if(x[i] <= 0) {
        x[i] = 0 + 1e-9;
      };
    };
    return(x);
}


//' C++ Function computing the predicted probabilities
//'
//' This function is a C++ implementation of the predictor function.
//' It is called internally by other functions.
//'
//' @param w1 The vector of the corresponding name extracted from the design matrix
//' @param b1 The vector of the corresponding name extracted from the design matrix
//' @param w2 The vector of the corresponding name extracted from the design matrix
//' @param b2 The vector of the corresponding name extracted from the design matrix
//' @param w3 The vector of the corresponding name extracted from the design matrix
//' @param w4 The vector of the corresponding name extracted from the design matrix
//' @param w5 The vector of the corresponding name extracted from the design matrix
//' @param b4 The vector of the corresponding name extracted from the design matrix
//' @param b5 The vector of the corresponding name extracted from the design matrix
//' @param age The vector of the corresponding name extracted from the data
//' @param timecap The vector of the corresponding name extracted from the data
//' @param indices_captured The vector of boolean indicated captured individuals
//'
//' @seealso \code{\link{predictor}}
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector predictorC(Rcpp::NumericVector w1,
                               Rcpp::NumericVector b1,
                               Rcpp::NumericVector w2,
                               Rcpp::NumericVector b2,
                               Rcpp::NumericVector w3,
                               Rcpp::NumericVector w4,
                               Rcpp::NumericVector w5,
                               Rcpp::NumericVector b4,
                               Rcpp::NumericVector b5,
                               Rcpp::NumericVector age,
                               Rcpp::NumericVector timecap,
                               Rcpp::NumericVector indices_captured){
                          Rcpp::NumericVector prob_base, prob;
                          prob_base = w1 * exp(-b1 * age) + w2 * exp(b2 * age) + w3;
                          prob = prob_base + indices_captured * ((w4 - 1)*exp(-w5*age) + 1) * exp(-vpowC(b4, (1 + vpowC(b5, age))) * timecap);
                          prob = unboundC(prob);
                          return(prob);
                          }


//' C++ Function computing the log-likelihood
//'
//' This function is a C++ implementation of \code{\link{compute_logLik}}.
//' It is called internally by other functions.
//'
//' @inheritParams parameters
//'
//' @seealso \code{\link{compute_logLik}}
//'
//' @export
// [[Rcpp::export]]
double compute_logLikC(Rcpp::NumericVector pred_prob, Rcpp::NumericVector surv_bin, int scale){
  double res = scale *  (sum(log(pred_prob) * (1 - surv_bin)) + sum(log(1 - pred_prob) * surv_bin));
  return(res);
}
