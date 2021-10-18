# Generate 1000 random samples from the Bernoulli (p) Distribution

bernoulli_prob<- 0.5
random_samples<- 1000


gen_samples<- function(prob, samples)
{
 x<- numeric()
 for(i in 1: samples)
 {
   if(runif(1) <= prob)
   {
     x[i]<- 1
   }else{
     x[i]<- 0
   }
 }
 return(x)
}

random_sample_vec<- gen_samples(bernoulli_prob, random_samples)
random_sample_vec
