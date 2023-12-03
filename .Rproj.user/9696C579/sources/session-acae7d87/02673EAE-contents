normalize <- function(x) {x / sqrt(sum(x^2))}

euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

euc.angle <- function(a, b) acos( sum(a*b) / ( sqrt(sum(a * a)) * sqrt(sum(b * b)) ) )

rad2deg <- function(rad) { rad * 180 / pi}

deg2rad <- function(deg) { deg * pi / 180}

angleVectors <- function(pt1,pt2,pt3){

  v1 = normalize(pt2 - pt1)
  v2 = normalize(pt3 - pt2)

  tetha <- euc.angle(v1, v2)

  return (tetha)
}

directionalChange_step <- function(coords) {

  list_data <- list()

  s <- seq(1.0,length(coords$x)-3)

  for (t in s) {

    pt1 <- data.frame(x = coords$x[t],  y = coords$y[t])
    pt2 <- data.frame(x = coords$x[t+1], y = coords$y[t+1])
    pt3 <- data.frame(x = coords$x[t+2], y = coords$y[t+2])

    tetha <- angleVectors(pt1,pt2,pt3)

    list_data <- append(list_data,(rad2deg(tetha)))
  }

  # should I make the same length ¿? -> or length -1

  list_data <- append(list_data,0)
  list_data <- append(list_data,0)
  list_data <- append(list_data,0)

  return(unlist(list_data))
}

directionalChange_unit <- function(coords) {

  list_data <- list()

  s <- seq(1.0,length(coords$x)-3)

  for (t in s) {

    pt1 <- data.frame(x = coords$x[t],  y = coords$y[t])
    pt2 <- data.frame(x = coords$x[t+1], y = coords$y[t+1])

    v1 = normalize(pt2 - pt1)
    v2 = data.frame(x = 1, y = 0)

    tetha <- euc.angle(v1, v2)

    list_data <- append(list_data,(rad2deg(tetha)))
  }

  # should I make the same length ¿? -> or length -1

  return(unlist(list_data))
}

# añadir demás funciones aquí:
