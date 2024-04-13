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
  double min_score = R_PosInf; // Initialize with positive infinity
  int min_index = -1;

  for (int i = 0; i < num_matrices; i++) {
    NumericMatrix mat = matrices[i];
    double d_score = as<double>(D_crit(mat, order));

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
NumericVector coordinate_exchange(int N, int K, int order){
  Function seq("seq");
  NumericVector X_vec = runif(N*K, -1, 1);

  X_vec.attr("dim") = Dimension(N, K);

  NumericMatrix X_init = as<NumericMatrix>(X_vec);
  NumericVector points = seq(-1,1,0.1);

  List exchange(22);
  Function D_crit("D_crit");
  double min_score = R_PosInf;
  int min_index = -1;
  bool improvement = true;

  NumericMatrix X = X_init;
  double D_cur = as<double>(D_crit(X_init));
  int n_iter = 0;

  while (improvement == true){
    double D_iter = as<double>(D_crit(X));
    for (int row = 0; row < N; row++){
      for (int col = 0; col < K; col++){
        exchange[0] = X;
        for (int i=1; i < 22; i++){
          exchange[i] = X;
          NumericMatrix mat = exchange[i];
          mat(row, col) = points[i];
          exchange[i] = mat;
        }

        List min_info = findMinDscore(exchange, order);
        int min_index = min_info["min_index"];
        NumericMatrix X = exchange[min_index];
        D_cur = min_info["min_score"];
      }
    }

    if (D_cur == D_iter){
      improvement = false;
    }
    n_iter++;
  }

  List opt_D;
}





