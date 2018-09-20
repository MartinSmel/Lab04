#create class
linreg <- setRefClass ( "linreg",
                         fields = list  ( #lst = "list",
                                         #form = "formula",
                                         #data = "data.frame",
                                         Regressions_coefficients = "numeric",
                                         The_fitted_value = "matrix",
                                         The_residuals = "matrix",
                                         The_degrees_of_freedom = "numeric",
                                         The_residual_variance = "numeric",
                                         The_variance_of_the_reg_coef = "matrix",
                                         t_values = "matrix"
                                       ) ,
                         methods = list (
                           initialize = function(form, data)
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
                             var <- (sigma)^2 * solve(R_1%*%t(R_1)) 
                             #%*% solve(t(R_1))
                             #t-values
                             t <- beta/sqrt(var)
                          #   my_object <- linreg(
                             .self$Regressions_coefficients <- beta
                             .self$The_fitted_value <- fit_val
                             .self$The_residuals <- e
                             .self$The_degrees_of_freedom <- df
                             .self$The_residual_variance <- sigma
                             .self$The_variance_of_the_reg_coef <- var
                             .self$t_values <- t
                             .self
                           },                       
                          #################################### 
                          print = function (...) {
                            x <-list()
                             for(i in 1:length(Regressions_coefficients))
                             x[[i]]<- Regressions_coefficients[[i]]
                           list_names <- names(x)
                             if (is.null(list_names)) 
                             {
                              list_names <- rep("", length(x))
                             }
                             print_element <- function(i)
                             {
                               if (list_names[i]=="")
                               {
                               cat("[[",i,"]]\n", sep="")
                               }
                               else
                               {
                                 cat("$", list_names[i], "\n", sep="")
                                # print(x[[i]], ...)
                               }
                                 print(x[[i]])
                               cat("\n")
                             }
                             invisible(lapply(i<-1:length(x), print_element))
                           },
                           plot = function () {
                             #Still wrong!
                             library(ggplot2)
                             p<-ggplot()+
                               layer(
                                 mapping=aes(x=The_fitted_value, y=The_residuals),
                                 geom="point", stat="identity", position="identity")
                                 p
                               
                           },
                           resid = function () {
                             #######
                            The_residuals
                           }, 
                           pred = function () {
                             #######
                             The_fitted_value
                           } ,
                           coef = function () {
                             #######
                             coef <- as.vector(Regressions_coefficients)
                            for(i in 1: length(coef))  
                             names(coef)[[i]] <- paste("coefficient", as.character(i))
                            coef
                           } ,
                          
                           summary = function () {
                             #######
                             
                             list_elements <- list(Regressions_coefficients,
                                                The_variance_of_the_reg_coef,
                                                t_values,
                                                The_residual_variance,
                                                The_degrees_of_freedom
                                                )
                             list_names <-c("coefficients","standard error","t-value","sigma", "degrees of freedom")
                             p_element <- function(i)
                             {
                               cat("$", list_names[i], "\n", sep="" )
                               print(list_elements[[i]], show_b = TRUE)
                               cat("\n")
                             }
                             invisible(lapply(i<-1:5, p_element))
                           }
                         )
)


#object = linreg$new(form = Petal.Length~Sepal.Width+Sepal.Length, data = iris)
#print.list(object$data, show_b = TRUE)
#plot(object)
#resid(object)
#pred(object)
#coef(object)
#summary(object)
#object$print.list

###################


