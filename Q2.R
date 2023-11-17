###Question 2


#  Create a class to represent a polynomial expression.
setClass("poly", slots = c(coefficients = "numeric", 
                           exponents = "numeric"))

## Part a

#  Define the constructor function for "poly" class.
poly_c <- function(coefficients, exponents) {
  return(new("poly", coefficients = coefficients, exponents = exponents))
}

#  Define the validator for "poly" class.
setValidity("poly", function(object){
  #  Check the validity of the coefficients and exponents
  if (!is.numeric(object@coefficients) | !is.numeric(object@exponents)) {
    stop("This is not a valid polynomial")
  }
  
  if (length(object@coefficients) != length(object@exponents)) {
    stop("The length of coefficients must equal to the length of exponents")
  }
  return(TRUE)
})

#  Define the show method.
##' @title Display a `polynomial` object
##' @param object A `polynomial` object
setMethod("show", signature = "poly", function(object) {
  terms <- sapply(1:length(object@coefficients), function(i) {
    if (object@coefficients[i] == 0) {
      return(NULL)
    }
    sign <- ifelse(object@coefficients[i] < 0, "- ", ifelse(i == 1, "", "+ "))
    paste0(sign, abs(object@coefficients[i]), "x^", object@exponents[i])
  })
  
  poly_string <- paste(na.omit(terms), collapse = " ")
  cat(poly_string, "\n")
})


#  Define the addition method.
setMethod("+", signature = c("poly", "poly"), function(e1, e2) {
  #  Ensure that the polynomials are valid
  validObject(e1)
  validObject(e2)
  
  #  Determine the maximum exponent in either polynomial
  max_exponent <- max(max(e1@exponents), max(e2@exponents))
  
  #  Create vectors to store the result coefficients
  result_coefficients <- numeric(max_exponent + 1)
  
  #  Add the coefficients for each term with the same exponent
  result_coefficients[e1@exponents + 1] <- result_coefficients[e1@exponents + 1] + e1@coefficients
  result_coefficients[e2@exponents + 1] <- result_coefficients[e2@exponents + 1] + e2@coefficients
  
  #  Create a new polynomial
  result_poly <- new("poly", coefficients = rev(result_coefficients), 
                     exponents = rev(seq_along(result_coefficients)) - 1)
  return(result_poly)
}
)

#  Define the subtraction method.
setMethod("-", signature = c("poly", "poly"), function(e1, e2) {
  #  Ensure that the polynomials are valid
  validObject(e1)
  validObject(e2)
  
  #  Determine the maximum exponent in either polynomial
  max_exponent <- max(max(e1@exponents), max(e2@exponents))
  
  #  Create vectors to store the result coefficients
  result_coefficients <- numeric(max_exponent + 1)
  
  #  Subtract the coefficients for each term with the same exponent
  result_coefficients[e1@exponents + 1] <- result_coefficients[e1@exponents + 1] + e1@coefficients
  result_coefficients[e2@exponents + 1] <- result_coefficients[e2@exponents + 1] - e2@coefficients
  
  #  Create a new polynomial
  result_poly <- new("poly", coefficients = rev(result_coefficients), 
                     exponents = rev(seq_along(result_coefficients)) - 1)
  return(result_poly)
})


## Part b

#  Run the tweaked version of the code.
p1 <- poly_c(coefficients = c(3, 2), exponents = c(2, 0))
p2 <- poly_c(coefficients = c(7, -2, -1, 17), exponents = c(3, 2, 1, 0))
p1
p2
p1 + p2
p1 - p2