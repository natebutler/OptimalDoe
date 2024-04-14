#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double d_check(NumericMatrix mat){
  Function D_crit("D_crit");
  double result = as<double>(D_crit(mat, 0));

  return result;
}

// [[Rcpp::export]]
List findMinDscore(ListOf<NumericMatrix> matrices, int order) {
  Function D_crit("D_crit");
  int num_matrices = matrices.size();
  double min_score = as<double>(D_crit(matrices[0], order)); // Initialize with starting number
  int min_index = 0;
  NumericVector scores(num_matrices);

  for (int i = 0; i < num_matrices; i++) {
    NumericMatrix mat = matrices[i];
    double d_score = as<double>(D_crit(mat, order));
    scores(i) = d_score;

    if (d_score < min_score) {
      min_score = d_score;
      min_index = i;
    }
  }

  List result;
  result["min_score"] = min_score;
  result["min_index"] = min_index;
  return result;
}

// [[Rcpp::export]]
List coordinate_exchange(int N, int K, int model_order){
  Function seq("seq");
  NumericVector X_vec = runif(N*K, -1, 1);

  X_vec.attr("dim") = Dimension(N, K);

  NumericMatrix X_init = as<NumericMatrix>(X_vec);
  NumericVector points = seq(-1,1,0.1);

  List exchange(22);
  Function D_crit("D_crit");
  int min_index = -1;
  bool improvement = true;

  NumericMatrix X(clone(X_init));
  double D_cur = as<double>(D_crit(X_init, model_order));
  int n_iter = 0;

  while (improvement == true){
    n_iter++;
    double D_iter = as<double>(D_crit(X, model_order));
    for (int row = 0; row < N; row++){
      for (int col = 0; col < K; col++){
        exchange[0] = clone(X);
        for (int i=1; i < 22; i++){
          exchange[i] = clone(X);
          NumericMatrix mat = exchange[i];
          mat(row, col) = points[i-1];
          exchange[i] = mat;
        }

        List min_info = findMinDscore(exchange, model_order);
        min_index = min_info["min_index"];
        NumericMatrix X_cur = exchange[min_index];
        D_cur = min_info["min_score"];
        X = clone(X_cur);
      }
    }
    if (D_cur == D_iter){
      improvement = false;
    }
    if (n_iter > 10000){
      improvement = false;
    }
    // return exchange;
  }

  List opt_D;
  opt_D["initial_design"] = X_init;
  opt_D["initial_d_score"] = as<double>(D_crit(X_init, model_order));
  opt_D["opt_design"] = X;
  opt_D["opt_d_score"] = D_cur;
  opt_D["n_iter"] = n_iter;

  return opt_D;
}


