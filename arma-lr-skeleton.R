library('Rcpp')
library('inline')

rcpp_inc <- '
using namespace Rcpp;
using namespace arma;
using namespace std;
'

src <- '
NumericMatrix Xr(Xs);
NumericVector yr(ys);

return(wrap(0));
'

flm <- cxxfunction(signature(Xs="numeric", ys="numeric"), body=src, plugin="RcppArmadillo", inc=rcpp_inc);


y <- log(trees$Volume)
X <- cbind(1, log(trees$Girth))

a <- flm(X, y)
a


b <- lm.fit(X, y)
b$coeff

