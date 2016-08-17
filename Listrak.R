cust_data <- read.csv("cust_data.csv", header = T)

unique_email=as.character(unique(cust_data$E.mail))

p=c(); n=c(); prob=c()

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
  prob[i] = round(sum(p) / (sum(n)+sum(p)),2)
}

con_pos = matrix(0, nrow = length(unique_email), ncol = length(unique_email), byrow = T,
                 dimnames =list(c(unique_email), c(unique_email)))
con_neg = matrix(0, nrow = length(unique_email), ncol = length(unique_email), byrow = T,
                 dimnames =list(c(unique_email), c(unique_email)))


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

con_prob = matrix(0, nrow = length(unique_email), ncol = length(unique_email), byrow = T,
                 dimnames =list(c(unique_email), c(unique_email)))

for (i in 1:length(unique_email)){
  for (j in 1:length(unique_email)) {
    con_prob[i,j] = round(con_pos[i,j] / (con_pos[i,j] + con_neg[i,j]),2)
  }
}

Contribution = c()
N = as.numeric((length(unique_email)) - 1) 
inter = c()

for (i in 1:length(unique_email)){
  for (j in 1:length(unique_email)){
    if (unique_email[i] != unique_email[j]){
      inter[i] = sum(con_prob[i,j] - prob[i] - prob[j])
      Contribution[i] = prob[i] + (1/2*N) * inter[i]
    }
  }
}


