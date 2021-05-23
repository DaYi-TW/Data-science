KM=function(x,n)
{
   output<-kmeans(x, n, iter.max=500, nstart=1);
   #output<-kmeans(x, n, iter.max=500, nstart=1, method="euclidean");
   return(output$cluster);
}