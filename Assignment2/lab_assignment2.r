name = "Satya Sai Naga Jaya Koushik Pilla"
liuid = "satpi345"
#1
sheldon_game <- function(player1, player2) {
  opt <- c("paper", "rock", "spock", "lizard", "scissors")
  
  stopifnot((player2 %in% opt) , (player1 %in% opt)) 
  
  
  
  if (player1 == player2) {return("Draw!")
  }
  
  win_sits <- list(c("spock", "scissors"),c("rock", "scissors"),c("paper", "rock"),c("spock", "rock"), c("rock", "lizard"), 
                    c("paper", "spock"),  c("scissors", "lizard"), 
                   c("lizard", "spock"), c("lizard", "paper")  )
  
  p1_wins <- any(sapply(win_sits, function(o) { player1 == o[1] && player2 == o[2]}))
  
  if (p1_wins) {return("Player 1 wins!")} else { return("Player 2 wins!")}
}

#2
my_moving_median = function(x,n,...){
  stopifnot(is.numeric(x),is.atomic(n))
  
  m_m <- length(x) - n
  u<-0
  
  for(i in seq_along(1:m_m)){ u[i] <- median(x[i:(i+n)],...)
  }
  
  return (u)
  
}
#3
for_mult_table <- function(from, to) {
  # Checking for numeric scalars
  if (length(from) != 1 || length(to) != 1 || !is.numeric(from) ||  !is.numeric(to)) {
    stop() }
  
  cs <- from:to
  rs <- from:to
  t <- matrix(0, nrow = length(rs), ncol = length(cs))
 
  for (a in 1:length(rs)) {for (b in 1:length(cs)) {
    t[a, b] <- rs[a] * cs[b]
  }
  }
  
  colnames(t) <- cs
  
  rownames(t) <- rs
 
  
  return(t)
}


e <- for_mult_table(from = 2, to = 6)
f <- for_mult_table(from = 4, to = 7)
(e)
f


#4
find_cumsum <- function(x, find_sum) {
  # condition for numeric vector and numeric scalar
  stopifnot(length(find_sum) == 1,is.numeric(x), is.numeric(find_sum) )
  
  i <- 1
  cum_sum <- 0
  j<- length(x)
  
  
  while (i <= j) {
    cum_sum <- cum_sum + x[i]
    if (cum_sum > find_sum) {break  
      
    }
    i <- i + 1
  }
  
  
  return(cum_sum)
}


#5
while_mult_table <- function(from, to) {
  # Checking for numeric scalars
  if ( length(from) != 1 || !is.numeric(from) ||!is.numeric(to)  || length(to) != 1) {
    stop()
  }
  q <-to - from + 1 
  
  m<- matrix(0,  ncol = q,nrow = q)
  
  
  c_n<-  r_n <- from:to
  
  
  p <- 1
  k <- 1
  
  
  while (p <= length(r_n)) {while (k <= length(c_n)) {
    m[p, k] <- (r_n[p]*c_n[k])
    k <- k + 1
  }
    k <- 1
    p <- p + 1
  }
  
  colnames(m) <- c_n
  rownames(m) <- r_n
  
  
  return(m)}



#6
repeat_find_cumsum <- function(x, find_sum) {
  stopifnot(length(find_sum) == 1,is.numeric(x), is.numeric(find_sum) )
  l <- 1
  c_count <- 0
  e <- length(x)
  
  repeat {
    if (l > e || c_count >= find_sum) {break
    }
    c_count <- c_count + x[l]
    l <- l + 1
  }
  
  return(c_count)
}


#8
in_environment <- function(env) {
  
  
  
  k <- as.character(ls(envir = as.environment(env)))
  
  
  
  
  return(k)
}


#1.6(9)
cov <- function(X) {
  
  a <- is.data.frame(X)
  if (!a) {
    stop("Invalid!") }
  
  s <- lapply(X, function(c) {if (length(c) <= 1) 
  {
    return(NA)  
  }
    f <- sd(c)
    g <- mean(c)
    return(f / g)
    
  })
  
  # The coefficients of variation by unlisting s
  
  
  return(unlist(s))
}




data(iris)


cv_r <- cov(iris[1:4])
print(cv_r)
#1.7(10)
moment <- function(i) {
  if (length(i) != 1 || !is.numeric(i) || i < 0) {
    
    stop("invalid i.")
  }
  
  # returning  function to compute the ith central moment
  return(
    function(y) {
      
      sum((y - mean(y))^i) / length(y)
    })
}
# Example:

cm_2 <- moment(9)


#7
repeat_my_moving_median = function(x,n,...){
  stopifnot(is.numeric(x),is.atomic(n))
  
  z<-0
  
  
  r_med <- length(x) - n
  p=1
  
  repeat{
    z[p] <- median(x[p:(p+n)],...)
    
    p<-p+1
    
    if(p>r_med){
      break
      
    }
    
  }
  
  return (z)
  
}