#create class
linreg <- setRefClass ( "linreg",
                         fields = list ( lst = "list",
                                         Regressions_coefficients = "numeric",
                                         The_fitted_value = "matrix",
                                         The_residuals = "matrix",
                                         The_degrees_of_freedom = "numeric",
                                         The_residual_variance = "numeric",
                                         The_variance_of_the_reg_coef = "matrix",
                                         t_values = "matrix"
                                       ) ,
                         methods = list (
                           initialize <- function(object,form, data)
                           {
                             #  stopifnot(is.formula(formula) == TRUE)
                             X <- model.matrix(form, data)
                             #X <- model.matrix(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
                             #y <-as.matrix(iris[all.vars(Petal.Length~Sepal.Width+Sepal.Length)[1]])
                             y <- as.matrix(data[all.vars(form)[1]])
                             Q <- qr.Q(qr(X), complete = TRUE)
                             R <- qr.R(qr(X), complete = TRUE)
                             m <- nrow(X)
                             n <- ncol(X)
                             ##n<m
                             R_1 <- R[c(1:n),]
                             Q_1 <- Q[,c(1:n)]
                             Q_2 <- Q[,c((n+1):m)]
                             #Regression coef.
                             beta <- as.vector(solve(R_1)%*%t(Q_1)%*%y)
                             #fitted values
                             fit_val <- X%*%beta
                             #residuals
                             e <- Q_2%*%t(Q_2)%*%y
                             #degrees of freedom
                             df <- m-n
                             #res. variance
                             sigma <- as.vector((t(e)%*%e)/df)
                             #variance of the reg. coef.
                             var <- (sigma)^2 * solve(R_1) %*% solve(t(R_1))
                             #t-values
                             t <- beta/sqrt(var)
                          #   my_object <- linreg(
                             object$Regressions_coefficients <- beta
                             object$The_fitted_value <- fit_val
                             object$The_residuals <- e
                             object$The_degrees_of_freedom <- df
                             object$The_residual_variance <- sigma
                             object$The_variance_of_the_reg_coef <- var
                             object$t_values <- t
                             object
                            } 
                      #    ,
                      #     print = function () {
                      #       #######
                      #       1
                      #     } ,
                      #     plot = function () {
                      #       #######
                      #       1
                      #     } ,
                      #     resid = function () {
                      #       #######
                      #       1
                      #     } ,
                      #     pred = function () {
                      #       #######
                      #       1
                      #     } ,
                      #     coef = function () {
                      #       #######
                      #       1
                      #     } ,
                      #     summary = function () {
                      #       #######
                      #       1
                      #     }
                         )
)


object <- linreg$new(lst = list(form = Petal.Length~Sepal.Width+Sepal.Length, data = iris))
object <- initialize(object,form = Petal.Length~Sepal.Width+Sepal.Length, data = iris)

###################


