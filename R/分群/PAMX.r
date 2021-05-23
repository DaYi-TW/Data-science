PAMX=function(x,n)
{
   output<-pam(x, n);
   return(output$cluster);
}