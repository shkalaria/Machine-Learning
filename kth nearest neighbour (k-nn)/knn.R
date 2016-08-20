# k-nn classification using cosine similarity function
# Author : Shreyash Kalaria
# June, 2015


# k-nn function for classifying testing data with training data (with data class as 
# last column) and with given k value.

knn = function(training, testing, k)
{
  for(row in 1:nrow(testing))
  {
    total_training_data = nrow(training)
    total = 0;
    for(col in 1:ncol(testing))
    {
      total = total + (testing[row, col] * testing[row, col]);       
    }
    total = sqrt(total);
    
    upper = c(rep(0, total_training_data));
    lower = c(rep(0, total_training_data));
    
    sim = c(rep(0, total_training_data));
    
    for(i in 1:total_training_data)
    {
      for(j in 1:ncol(testing))
      {
        upper[i] = upper[i] + training[i, j] * testing[row, j];     
        lower[i] = lower[i] + training[i, j] * training[i, j];
      }
      
      lower[i] = sqrt(lower[i]);
      sim[i] = upper[i] / (lower[i] * total);
    }
    
    similarity = data.frame(unlist(sim), class = c(training[, ncol(training)]));
    similarity = similarity[order(similarity[, 1], decreasing = TRUE),];
    
    total_class = unique(training[c(ncol(training))])
    total_no_class = length(total_class[,1])
    
    count_class = c(rep(0, total_no_class))
    dist_weighted_count = c(rep(0, total_no_class))
    
    for(i in 1:k)
    {
      count_class[c(total_class[similarity[i,2],])] = count_class[c(total_class[similarity[i,2],])] + 1;
      dist_weighted_count[c(total_class[similarity[i,2],])] = dist_weighted_count[c(total_class[similarity[i,2],])] + similarity[i,1];
    }
    
    cat(sprintf("%s-nn classification for test data %s : %s", k, row, total_class[match(max(count_class), count_class),1]));
    cat("\n");
    cat(sprintf("distance weighted %s-nn classification for test data %s : %s", k, row, total_class[match(max(dist_weighted_count), dist_weighted_count),1]));
    cat("\n");
  }
}


# read data from text files
training_data = read.table(header = T,"knn_training_data.txt");
testing_data = read.table(header = F,"testing_data.txt");

# construct new training and testing data with only required fields.
training_data = training_data[,-1]
testing_data = testing_data[,-1]

# take the input value of k from user to find kth nearest neighbour. 
k = readline(prompt="Enter value of k : ")

# call the knn function with required parameters.
knn(training_data, testing_data, k)