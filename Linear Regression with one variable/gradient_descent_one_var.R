# Gradient Descent with one variable
# Author : Shreyash Kalaria
# July, 2015


# function that take input dependent variable(y), independent variable(x), 
# initial theta, alpha and number of iteration. returns theta and hypothesis.
gradient_one = function(x, y, theta, alpha, iterations)
{
        m = length(y);
        j = rep(0, iterations);
        
        for(i in 1:iterations)
        {
                h = theta[1] + theta[2] * x;
                
                theta[1] = theta[1] - (alpha*(1/m)*(sum(h-y)));
                theta[2] = theta[2] - (alpha*(1/m)*(sum((h-y)*x)));
                
                J = (1/(2*m)) * ( sum ((h-y)*(h-y)));
                j[i] = J;
        }
        plot(x, h);  
        abline(theta[1], theta[2]);
        
        plot(j);
        
        return(theta);
        return(h);
        
}

# function returns predicted dependent variable(y) from set of independent variable(x) and 
# theta(which is calculated with gradient function).
predict=function(x, theta)
{
        y = theta[1] + theta[2]*x;
        return(y);
}

data = read.csv("data_GD_one_var.txt"); # read data from csv file and store it in data 
 
# separate dependent variable and independent variable.
x_data = data[,1];
y_data = data[,2];

# prepare treaning set and testing set.
len_x = length(x_data);
treaning_data_index = as.integer(len_x*0.8);

x = x_data[1:treaning_data_index];
y = y_data[1:treaning_data_index];

# plot treaning data
plot(x,y);
abline(lm(y~x));

# define initial thata, alpha and iteration
theta = c(1,3);
alpha = 0.003;
iterations = 10000;

# calculate new thetas with use of 
theta = gradient_one(x, y, theta, alpha, iterations);

# predict data of testing part
y_new = predict(x_data[treaning_data_index+1:len_x], theta);

# calculate mean absoluted error and mean absolute percentage error
mae = sum(abs(y_data[treaning_data_index+1:len_x] - y_new)[1:(len_x-treaning_data_index)])/(a-treaning_data_index);
print(mae);
mape = sum(abs((y_data[treaning_data_index+1:len_x]-y_new)[1:(len_x-treaning_data_index)] / y_data[treaning_data_index+1:len_x][1:(len_x-treaning_data_index)]))/(len_x-treaning_data_index);
print(mape);