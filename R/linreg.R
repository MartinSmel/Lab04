#create class
linreg <- setRefClass ( "linreg" ,
                         fields = list ( Regressions_coefficients = "numeric",
                                         The_fitted_value = "numeric",
                                         The_residuals = "numeric",
                                         The_degrees_of_freedom = "numeric",
                                         The_residual_variance = "numeric",
                                         The_variance_of_the_reg_coef = "numeric",
                                         t_values = "numeric"
                                       ) ,
                         methods = list (
                           initialize <- function(form, data)
                           {
                             #  stopifnot(is.formula(formula) == TRUE)
                             X <- model.matrix(form, data)
                             #X <- model.matrix(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
                             #as.matrix(iris[all.vars(Petal.Length~Sepal.Width+Sepal.Length)[1]])
                             y <- as.matrix(data[all.vars(formula)[1]])
                             Q <- qr.Q(qr(X), complete = TRUE)
                             R <- qr.R(qr(X), complete = TRUE)
                             m <- nrow(X)
                             n <- ncol(X)
                             ##n<m
                             R_1 <- R[c(1:n),]
                             Q_1 <- Q[,c(1:n)]
                             Q_2 <- Q[,c(n+1:m)]
                             #Regression coef.
                             beta <- R_1^(-1)%*%t(Q_1)%*%y
                             #fitted values
                             fit_val <- X%*%beta
                             #residuals
                             e <- Q_2%*%t(Q_2)%*%y
                             #degrees of freedom
                             df <- m-p
                             #res. variance
                             sigma <- (t(e)%*%e)/df
                             #variance of the reg. coef.
                             var <- sigma^2 * R^(-1) %*% (t(R))^(-1)
                             #t-values
                             t <- beta/sqrt(var)
                             my_object <- linreg(Regressions_coefficients <- beta,
                                                 The_fitted_value <- fit_val,
                                                 The_residuals <- e,
                                                 The_degrees_of_freedom <- df,
                                                 The_residual_variance <- sigma,
                                                 The_variance_of_the_reg_coef <- var,
                                                 t_values <- t)
                             
                           } ,
                           print = function () {
                             #######
                             1
                           } ,
                           plot = function () {
                             #######
                             1
                           } ,
                           resid = function () {
                             #######
                             1
                           } ,
                           pred = function () {
                             #######
                             1
                           } ,
                           coef = function () {
                             #######
                             1
                           } ,
                           summary = function () {
                             #######
                             1
                           }
                         )
)

object <- linreg$new(linreg(Sepal.Length + Sepal.Width, data=iris))

  
