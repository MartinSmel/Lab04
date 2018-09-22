
#create class
linreg <- setRefClass ( "linreg",
                         fields = list  (data_name = "character",
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
                             stopifnot (class(form) == "formula")
                             stopifnot (class(data) == "data.frame")
                             X <- model.matrix(form, data)
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
                             #t-values
                             t = list()
                             for(i in (1:3))
                             t[[i]] <- beta/sqrt(var[[i,i]])
                             .self$data_name <- deparse(substitute(data))
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
                      
                            cat(paste("linreg(formula = ", format(form), ", data = ", data_name, ")\n\n", sep = ""))
                            
                            # Coefficients
                            cat(paste("Coefficients:\n\n"))
                            
                            # Values
                            table = setNames(data.frame(matrix(ncol = length(Regressions_coefficients), nrow = 0)), names(Regressions_coefficients))
                            for (i in 1:length(Regressions_coefficients)) {
                              table[1,i] = round(Regressions_coefficients[i], 3)
                            }
                              write.table(table, quote=FALSE)
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
                         
                             cat("\nCall:\n")
                             cat(paste("linreg(formula = ", (format(form)), ", data = ", data_name, ")\n\n", sep = ""))
                             cat("Coefficients:\n\n")
                             
                             table = data.frame(matrix(ncol = 5, nrow = 0))
                             for (i in 1:length(Regressions_coefficients)) {
                               local_t_value = Regressions_coefficients[i]/sqrt(The_variance_of_the_reg_coef[i, i])
                               local_p_value = 2 * pt(abs(local_t_value), The_degrees_of_freedom, lower.tail = FALSE)
                               newRow = data.frame(round(Regressions_coefficients[i], 2), round(sqrt(The_variance_of_the_reg_coef[i, i]), 2), round(local_t_value, 2), formatC(local_p_value, format = "e", digits = 2),"***")
                               rownames(newRow)[1] = rownames(The_variance_of_the_reg_coef)[i]
                               table = rbind(table, newRow)
                             }
                             
                             colnames(table) = c("Estimate", "Std. Error", "t value", "Pr(>|t|)", "")
                             write.table(table, quote=FALSE)
                             cat(paste("\nResidual standard error:", sqrt(The_residual_variance), "on", The_degrees_of_freedom, "degrees of freedom"))
                             
                             }
                           
                         )
)



###################


 