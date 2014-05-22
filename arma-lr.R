
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

int n = Xr.nrow();
int k = Xr.ncol();

mat X(Xr.begin(), n, k, false);
vec y(yr.begin(), yr.size(), false);

int df = n - k;

vec coef = solve(X, y);
vec res = y - X * coef;

double s2 = inner_product(res.begin(), res.end(), res.begin(), 0.0)/df;
vec stderr = sqrt(s2 * diagvec(pinv(trans(X) * X)));

return(List::create(Named("coeff") = coef, Named("stderr") = stderr, Named("df") = df, Named("res") = res, Named("s2") = s2));

'

flm <- cxxfunction(signature(Xs="numeric", ys="numeric"), body=src, plugin="RcppArmadillo", inc=rcpp_inc);

a <- flm(X, y)
a


b <- lm.fit(X, y)
b$coeff

cbind(a$res, b$res)

names(b)
y <- log(trees$Volume)
X <- cbind(1, log(trees$Girth))

a$s2
a1 <- t(X) %*% X
a2 <- solve(a1)

s2 <- sum(a$res * a$res)/a$df
s2
sqrt(s2 * diag(a2))
a$stderr
