# Time-Decay Attribution Model.
# This model calculates the contribution score of each email based on the probablistic approach.

# The data consists of the e-mails that were send to the "users" along with binary conversion (Yes/No) field information # 
cust_data <- read.csv("cust_data.csv", header = T)

# Finding unique emails.
unique_email=as.character(unique(cust_data$E.mail))   

# Initializing NULL vectors
p=c(); n=c(); prob=c()    

# Calculating total number of "Yes" and "No" for each email in order to calculate the probability of conversion.
for(i in 1:length(unique_email)) 
{
  for (j in 1:nrow(cust_data))
  {
    if((unique_email[i]==as.character(cust_data$E.mail[j]))&&cust_data$Conversion[j]==1){
      
      p[j]=1;
    }
    else if((unique_email[i]==as.character(cust_data$E.mail[j]))&&cust_data$Conversion[j]==0)
    {
      n[j]=1;
    }
    else{
      p[j]=0; n[j]=0
    }
  }
  prob[i] = round(sum(p) / (sum(n)+sum(p)),2) #Probability of each email to get converted into sale("Yes").
}

# Initializing zero matricies.
con_pos = matrix(0, nrow = length(unique_email), ncol = length(unique_email), byrow = T,
                 dimnames =list(c(unique_email), c(unique_email)))
con_neg = matrix(0, nrow = length(unique_email), ncol = length(unique_email), byrow = T,
                 dimnames =list(c(unique_email), c(unique_email)))

# Calculating pair-wise total number of "Yes" and "No" in order to calculate pair-wise probability of conversion
for (i in 1:length(unique_email))
{
  for (j in 1:length(unique_email))
  {
    if(unique_email[i] != unique_email[j])
    {
      for (k in 1:nrow(cust_data))
      {
        for (p in 1:nrow(cust_data))
        {
          if (as.character(cust_data$E.mail[k])==unique_email[i] && as.character(cust_data$E.mail[p])==unique_email[j])
          {
            if (cust_data$Conversion[k]==1 && cust_data$Conversion[p]==1)
            {
              con_pos[i,j] = con_pos[i,j]+1;
            }
            if (cust_data$Conversion[k]==0 && cust_data$Conversion[p]==0)
            {
              con_neg[i,j] = con_neg[i,j]+1;
            }
          }
        }
      }
    }
  }
}          

# Initializing zero matrix for pair-wise conditional probability.
con_prob = matrix(0, nrow = length(unique_email), ncol = length(unique_email), byrow = T,
                  dimnames =list(c(unique_email), c(unique_email)))

# Calculating pair-wise conditional probability.
for (i in 1:length(unique_email)){
  for (j in 1:length(unique_email)) {
    con_prob[i,j] = round(con_pos[i,j] / (con_pos[i,j] + con_neg[i,j]),2)
  }
}

Contribution = c()
N = as.numeric((length(unique_email)) - 1) #Total number of unique e-mails
inter = c() #Initializing NULL vector for the second order interaction term.

# Finally calculating the contribution of each email in triggering the sale ("Yes").
for (i in 1:length(unique_email)){
  for (j in 1:length(unique_email)){
    if (unique_email[i] != unique_email[j]){
      inter[i] = sum(con_prob[i,j] - prob[i] - prob[j])
      Contribution[i] = prob[i] + (1/2*N) * inter[i]
    }
  }
}

plot(Contribution, xlab = "unique emails", type = "o")
