
generate <- function(

  n = 1000,
  step_length = 2,
  angular_errorSd = 0.5,
  angular_errorDist = function(n) stats::rnorm(n, sd = angular_errorSd),
  linear_errorSd = 0.25,
  linear_errorDist = function(n) stats::rnorm(n, sd = linear_errorSd),

) {

  angular_errors <- angular_errorDist(n)
  linear_errors <- linear_errorDist(n)

  coords  <- complex(n + 1)
  angle   <- 0

  for (i in 1:n) {

    angle <- angle + angular_errors[i]
    length <- step_length + linear_errors[i]
    coords[i + 1] <- coords[i] + complex(modulus = length, argument = angle)

  }

  return(data.frame(x=Re(coords), y=Im(coords)))
}
