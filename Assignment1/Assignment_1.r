#1
name = "Satya_Sai_Naga_Jaya_Koushik_Pilla"
liuid ="satpi345"
my_num_vector = function(){
  t<-(log(11,10))
  n<- ( cos(pi/5))
  p <- exp(pi/3)
  q<- (1173 %% 7)/19
  f = c(t,n,p,q)
  return(f)
  
} 


my_num_vector()

#print (my_num_vector()) 


#2
filter_my_vector <- function(x,leq)
{
  d <- replace(x,x>=leq,NA)
  return(d)
  
}

#print(filter_my_vector(x = c(2, 9, 2, 4, 102), leq = 4))
#3
dot_prod = function(a=vector("logical"), b= vector("logical")){
  
  d <- a%*%b
  k <- as.numeric(d)
  return (k)
}
#dot_prod(a = c(3,1,12,2,4), b = c(1,2,3,4,5))
#dot_prod(a = c(-1,3), b = c(-3,-1))
#4
rec = function(x){
  if (x <= 1){
    return (1)
  } 
  else {
    return ( x*rec(x-1))
  }
}
approx_e = function(N){
  t <- 0
  for (i in 0:N){
    current = 1/rec(i)
    t = t+ current
  }
  return( t)
}
#print(approx_e(9))to get 5 decimal accuracy.


# mat= matrix(c(1,2,3,4,5,6),2,3)
# new_mat <- cbind(mat, mat)
# calculate_elements(A = new_mat)
# #print(calculate_elements(A))
# g <-cbind(c(4,3,8),c(9,5,1),c(8,1,6))
# calculate_elements(A = g)
# new_mat2 <- cbind(g, g)
# calculate_elements(A = new_mat2)
# #print(mat[2][2])
#5
my_magic_matrix=function(){
  g <- matrix(c(4, 9, 2, 3, 5, 7, 8, 1, 6), nrow = 3, byrow = TRUE)
  return (g)
  
}
#the sum of the elements is 15, for diagnol elements and rows and columns.
#6

calculate_elements = function(A){
  q<- ncol(A)
  e <- nrow(A)
  return( q*e)
  
}

#print(my_magic_matrix())
#print(nrow(my_magic_matrix()))
#7

  
row_to_zero <- function(A, i) {
  if (!is.matrix(A) || !is.numeric(A)) {
    stop(" ")
  }
  
 
  
  A[i, ] <- 0  # Set the specified row to all zeros
  
  return(A)
}
#8
mat <-  my_magic_matrix()
add_elements_to_matrix <- function(A, x, i, j) {
  for (row_ in i) {
    for (col_ in j) {
      A[row_, col_] <- A[row_, col_]+x
    }
  }
  return(A)
}
mat <- my_magic_matrix()
add_elements_to_matrix(A = mat, x = 10, i = 2, j = 3)


#9
my_magic_list = function(){
  j <- list(info="my own list",my_num_vector(),my_magic_matrix() )
  return(j)
  
}
my_magic_list()

#10
change_info = function(x,text="Some new info"){
  x$info <-text
  return (x)
  
}
#11
add_note= function(x , note = "This is a magic list!"){
  #alist1 = my_magic_list()
  x$note<-note
  return(x)
  
  
}
#12
sum_numeric_parts=function(x){
  
  y=as.numeric(unlist(x))
  y=sum(y,na.rm = TRUE)
  return(y)
  }

#row_to_zero(g,i=3)
#13
my_data.frame = function(){
  id <- c(1:3)
  name <- c("John","Lisa","Azra")
  income <- c(7.30,0.00,15.21)
  rich <- c(FALSE,FALSE,TRUE)
  my_data.frame <- data.frame(id,name,income,stringsAsFactors = FALSE,rich)
  return(my_data.frame)
  
}

my_data.frame
#14
#data(iris)

sort_head = function(df , var.name ,n){
  # g <- data.frame(df, var.name,n)
  result <- df[order(df[[var.name]], decreasing = TRUE), ]
  re1 <- head(result,n)
  return(re1)
  
}
#print(sort_head())
#15
add_median_variable =function(df, j){
  df1 <- data.frame(df)
  
  df1$compared_to_median<-with(df1,ifelse(median(df1[,j]) >df1[,j],'Smaller',
                                        ifelse(median(df1[,j]) < df1[,j], 'Greater','Median')))
  
  
  return(df1)
  
}
data(faithful)
tail(add_median_variable(df = faithful, 1))

#16
analyze_columns<-function(df,j=vector("logical")){
  vecj_2 <- df[,j[2]]
  vecj_1 <- df[,j[1]]
  
  j_2_m <-c(mean=mean(vecj_2),
            median=median(vecj_2),
            sd=sd(vecj_2))
  j_1_m <- c(mean=mean(vecj_1),
             median=median(vecj_1),
             sd=sd(vecj_1))
  
  
  j_2_vec_<-names(df)[j[2]]
  j_1_vec_<- names(df)[j[1]]
  c_mat<-cor(df[,j])
  
  fresult<-list(
    setNames(j_1_m,names(j_1_m)),
    
    setNames(j_2_m,names(j_2_m)),
    
    "correlation_matrix" =c_mat
  )
  names(fresult)<-c(j_1_vec_,j_2_vec_,"correlation_matrix")
  return(fresult)
}



