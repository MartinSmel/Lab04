calculateMagicRainbowStars = function(p_value) {
  if (p_value > 0.1) return(" ")
  if (p_value > 0.05) return(".")
  if (p_value > 0.01) return("*")
  if (p_value > 0.001) return("**")
  return("***")
}
cPrint = function(x, stripoff = FALSE) {
  if (is.data.frame(x)) {
    print(x, row.names = stripoff)
  }
  else {
    print(x)
  }
}

#create class
linreg <- setRefClass ( "linreg",
                         fields = list  ( #lst = "list",
                                         data_name = "character",
                                         form = "formula",
                                         data = "data.frame",
                                         Regressions_coefficients = "vector",
                                         The_fitted_value = "matrix",
                                         The_residuals = "matrix",
                                         The_degrees_of_freedom = "numeric",
                                         The_residual_variance = "numeric",
                                         The_variance_of_the_reg_coef = "matrix",
                                         t_values = "list"
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
                             beta <- as.vector((solve((t(X) %*% X)) %*% t(X) %*% y))
                             #beta <- as.vector(solve(R_1)%*%t(Q_1)%*%y)
                             names(beta) <- c("(Intercept)", "Sepal.Width", "Sepal.Length")
                             #fitted values
                             fit_val <- X%*%beta
                             #residuals
                             e <- Q_2%*%t(Q_2)%*%y
                             #degrees of freedom
                             df <- m-n
                             #res. variance
                             sigma <- as.vector((t(e)%*%e)/df)
                             #variance of the reg. coef.
                             var <- sigma * solve(t(R)%*%R) 
                             #var <- sigma * solve(t(as.matrix(X)) %*% as.matrix(X))
                             #%*% solve(t(R_1))
                             #t-values
                             t = list()
                             for(i in (1:3))
                             t[[i]] <- beta/sqrt(var[[i,i]])
                            # t <- as.vector(t)
                             .self$data_name <- deparse(substitute(data))
                          #   my_object <- linreg(
                             .self$form <- form
                             .self$data <- data
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
                      #      x <-list()
                      #       for(i in 1:length(Regressions_coefficients))
                      #       x[[i]]<- Regressions_coefficients[[i]]
                      #     list_names <- names(x)
                      #       if (is.null(list_names)) 
                      #       {
                      #        list_names <- rep("", length(x))
                      #       }
                      #       print_element <- function(i)
                      #       {
                      #         if (list_names[i]=="")
                      #         {
                      #         cat("[[",i,"]]\n", sep="")
                      #         }
                      #         else
                      #         {
                      #           cat("$", list_names[i], "\n", sep="")
                      #          # print(x[[i]], ...)
                      #         }
                      #           cat(x[[i]])
                      #         cat("\n")
                      #       }
                      #       invisible(lapply(i<-1:length(x), print_element))
                            # Formula
                            
                            cat(paste("linreg(formula = ", format(form), ", data = ", data_name, ")\n\n", sep = ""))
                            
                            # Coefficients
                            cat(paste("Coefficients:\n\n"))
                            
                            # Values
                            table = setNames(data.frame(matrix(ncol = length(Regressions_coefficients), nrow = 0)), names(Regressions_coefficients))
                            for (i in 1:length(Regressions_coefficients)) {
                              table[1,i] = round(Regressions_coefficients[i], 3)
                            }
                              cPrint(table)
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
                           #  
                          #   list_elements <- list(Regressions_coefficients,
                          #                      The_variance_of_the_reg_coef,
                          #                      t_values,
                          #                      The_residual_variance,
                          #                      The_degrees_of_freedom
                          #                      )
                          #   list_names <-c("coefficients","standard error","t-value","sigma", "degrees of freedom")
                          #   p_element <- function(i)
                          #   {
                          #     cat("$", list_names[i], "\n", sep="" )
                          #     cat(list_elements[[i]], show_b = TRUE)
                          #     cat("\n")
                             "Prints a summary of the Linear Regression"
                             
                             cat("\nCall:\n")
                             cat(paste("linreg(formula = ", (format(form)), ", data = ", data_name, ")\n\n", sep = ""))
                             
                             # Coefficients
                             cat("Coefficients:\n\n")
                             
                             # Values
                             table = data.frame(matrix(ncol = 5, nrow = 0))
                             for (i in 1:length(Regressions_coefficients)) {
                               # Beta (coefficients), std error, t values, p values
                               local_t_value = Regressions_coefficients[i]/sqrt(The_variance_of_the_reg_coef[i, i])
                               local_p_value = 2 * pt(abs(local_t_value), The_degrees_of_freedom, lower.tail = FALSE)
                               newRow = data.frame(round(Regressions_coefficients[i], 2), round(sqrt(The_variance_of_the_reg_coef[i, i]), 2), round(local_t_value, 2), formatC(local_p_value, format = "e", digits = 2), calculateMagicRainbowStars(local_p_value))
                               rownames(newRow)[1] = rownames(The_variance_of_the_reg_coef)[i]
                               table = rbind(table, newRow)
                             }
                             
                             colnames(table) = c("Estimate", "Std. Error", "t value", "Pr(>|t|)", "")
                             cPrint(table, TRUE)
                             cat(paste("\nResidual standard error:", sqrt(The_residual_variance), "on", The_degrees_of_freedom, "degrees of freedom"))
                             
                             }
                            # invisible(lapply(i<-1:5, p_element))
                           #}
                         )
)


object = linreg$new(form = Petal.Length~Sepal.Width+Sepal.Length, data = iris)
#print.list(object$data, show_b = TRUE)
#plot(object)
#resid(object)
#pred(object)
#coef(object)
#summary(object)
#object$print.list

###################


 