#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double wAUC_sorted(const NumericVector& x, const LogicalVector& y, const NumericVector& w){
  
  int n = y.length();
  int nw = sum(w);
  double latest_control = R_NegInf;
  double count_control = 0, count_control_equal = 0;
  double add, total = 0;
  double total_w = 0, count_control_w = 0, add_w = 0;
  
  for (int i = 0; i < n; i++) {

    if (y[i]) {  // case
      add = count_control;
      add_w = count_control_w;
      if (x[i] == latest_control) add -= (count_control_equal + 1) / 2;
      if (x[i] == latest_control) add_w -= (count_control_equal + 1) / 2;
      total += add;
      total_w += add_w * w[i];
    } else {     // control
      count_control++;
      count_control_w += w[i];
      if (x[i] == latest_control) {
        count_control_equal++;
      } else {
        latest_control = x[i];
        count_control_equal = 0;
      }
    }
  }

  return total_w / (count_control_w * (nw - count_control_w)) ;
  
}

// [[Rcpp::export]]
double AUC_sorted(const NumericVector& x, const LogicalVector& y){

  int n = y.length();
  double latest_control = R_NegInf;
  double count_control = 0, count_control_equal = 0;
  double add, total = 0;
  
  for(int i = 0; i < n; i++) {
    if (y[i]) {  // case
      add = count_control;
      if (x[i] == latest_control) add -= (count_control_equal + 1) / 2;
      total += add;
    } else {     // control
      count_control++;
      if (x[i] == latest_control) {
        count_control_equal++;
      } else {
        latest_control = x[i];
        count_control_equal = 0;
      }
    }
  }

  return total / (count_control * (n - count_control)) ;
  
}
