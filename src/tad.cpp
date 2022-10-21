#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export(.tad)]]
NumericVector tad(IntegerVector evid, NumericVector time, IntegerVector calc_tad) {

  // assume data is already in expanded form (no ADDL)
  bool dose_found = FALSE;
  double last_dose = NA_REAL;

  int length = evid.size();
  NumericVector tad (length, NA_REAL);

  for(int i = 0; i < length; i++) {

    // only calculate TAD for rows satisfying cond
    if(calc_tad[i] == 0) continue;

    int this_evid = evid[i];
    double this_time = time[i];

    // if this is a dose record
    if(this_evid == 1 || this_evid == 4) {

      dose_found = TRUE;
      tad[i] = 0;

      last_dose = this_time;
    }
    else {

      if(dose_found) {
        tad[i] = this_time - last_dose;
      }
      // if no prior dosing record, leave TAD as NA.
    }
  }
  return tad;
}
