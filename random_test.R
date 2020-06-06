random_test <-function(data1, data2,HA_direction){
  
  # Get the length of both the data
  data1_len <- length(data1)
  data2_len <- length(data2)
  
  # Get the mean of the both the data
  data1_mean <- mean(data1)
  data2_mean <- mean(data2)
  
  # Number of repetitions 
  reps <- 10000
  
  
  # Our observed R value
  r <- data1_mean - data2_mean
  
  
  # Create an empty array of the number of repetitions
  results <- numeric(reps)
  
  # Create a column vector that combines the two data sets
  x <- c(data1, data2)
  
  
  for (i in 1:reps){
    # Filler is going to be a rample sample of the number of elements in our x column vector
    filler <- sample(x)
    # Divids the two groups based on the original length of the two datasets  
    group_A <- filler[1:data1_len]
    group_B <- filler[data1_len+1:data2_len]
    # Places the difference of the means of the two groups into our results column vector
    results[i] <- mean(group_A)-mean(group_B)
  }
  
  # Calculates p-value if our Ha is '<'
  if(HA_direction == -1)
    
    p_value <- sum(results < r)/reps
  
  # Calculates p-value if our Ha is '≠'
  if(HA_direction == 0)
    p_value <- sum(results != r)/reps
  
  # Calculates p-value if our Ha is '>'
  if(HA_direction == 1)
    p_value <- sum(results > r)/reps
  
  
  print(p_value)
  print(r)
  
  
  hist(results)
}



