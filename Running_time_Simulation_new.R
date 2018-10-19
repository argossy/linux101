naiveBH = function(x=pvalues,alpha=0.10){
m=length(x);
      pvalues.sorted = sort(x)
      kmax = which((pvalues.sorted < alpha * c(1:m)/m) == TRUE); 
     if (length(kmax)>0) {kmax =max(kmax);
                                                   km = c(1:kmax); return(sig.pvalues=pvalues.sorted[km])};
   }

FastBH = function(x=pvalues,alpha=0.10){
       m=length(x);
       kmax.r = m; 
     kmax.rb = kmax.r+1;
repeat{
       kmax = which((pvalues < alpha * kmax.r/m) == TRUE);
       pvalues = pvalues[kmax];
       kmax.r = length(kmax); 
      if(kmax.r==kmax.rb){break}
     else{kmax.rb = kmax.r};
}
     return(sig.pvalues=pvalues[kmax]);
   }

set.seed(1234);
pvalues = runif(10^4); # uniform p-values;
t0=proc.time() ;
res1=naiveBH(x=pvalues,alpha=0.10);
t1=proc.time()-t0 ;
t0=proc.time() ;
res2=FastBH(x=pvalues,alpha=0.10);
t2=proc.time()-t0 ;

set.seed(1234);
pvalues = runif(10^4); pvalues = pvalues/(10^rbinom(length(pvalues),1,.8)); #.8 of the p-values are U[0,0.1] to make significant results;
t0=proc.time() ;
res3=naiveBH(x=pvalues,alpha=0.10);
t3=proc.time()-t0 ;
t0=proc.time() ;
res4=FastBH(x=pvalues,alpha=0.10);
t4=proc.time()-t0 ;
             
set.seed(1234);
pvalues = runif(10^4)/10; #all p-values are set to be U[0,0.1] - to be all significant;
t0=proc.time() ;
res5=naiveBH(x=pvalues,alpha=0.10);
t5=proc.time()-t0 ;
t0=proc.time() ;
res6=FastBH(x=pvalues,alpha=0.10);
t6=proc.time()-t0 ;

#10E4;
t1;t2;t3;t4;t5;t6;
length(res1);
length(res2);
length(res3);
length(res4);
length(res5);
length(res6);

set.seed(1234);
pvalues = runif(10^5/2); # uniform p-values;
t0=proc.time() ;
res1=naiveBH(x=pvalues,alpha=0.10);
t1=proc.time()-t0 ;
t0=proc.time() ;
res2=FastBH(x=pvalues,alpha=0.10);
t2=proc.time()-t0 ;

set.seed(1234);
pvalues = runif(10^5/2); pvalues = pvalues/(10^rbinom(length(pvalues),1,.8)); #.8 of the p-values are U[0,0.1] to make significant results;
t0=proc.time() ;
res3=naiveBH(x=pvalues,alpha=0.10);
t3=proc.time()-t0 ;
t0=proc.time() ;
res4=FastBH(x=pvalues,alpha=0.10);
t4=proc.time()-t0 ;
             
set.seed(1234);
pvalues = runif(10^5/2)/10; #all p-values are set to be U[0,0.1] - to be all significant;
t0=proc.time() ;
res5=naiveBH(x=pvalues,alpha=0.10);
t5=proc.time()-t0 ;
t0=proc.time() ;
res6=FastBH(x=pvalues,alpha=0.10);
t6=proc.time()-t0 ;

#5x10E4;
t1;t2;t3;t4;t5;t6;
length(res1);
length(res2);
length(res3);
length(res4);
length(res5);
length(res6);

set.seed(1234);
pvalues = runif(10^5); # uniform p-values;
t0=proc.time() ;
res1=naiveBH(x=pvalues,alpha=0.10);
t1=proc.time()-t0 ;
t0=proc.time() ;
res2=FastBH(x=pvalues,alpha=0.10);
t2=proc.time()-t0 ;

set.seed(1234);
pvalues = runif(10^5); pvalues = pvalues/(10^rbinom(length(pvalues),1,.8)); #.8 of the p-values are U[0,0.1] to make significant results;
t0=proc.time() ;
res3=naiveBH(x=pvalues,alpha=0.10);
t3=proc.time()-t0 ;
t0=proc.time() ;
res4=FastBH(x=pvalues,alpha=0.10);
t4=proc.time()-t0 ;
             
set.seed(1234);
pvalues = runif(10^5)/10; #all p-values are set to be U[0,0.1] - to be all significant;
t0=proc.time() ;
res5=naiveBH(x=pvalues,alpha=0.10);
t5=proc.time()-t0 ;
t0=proc.time() ;
res6=FastBH(x=pvalues,alpha=0.10);
t6=proc.time()-t0 ;

#10E5;
t1;t2;t3;t4;t5;t6;
length(res1);
length(res2);
length(res3);
length(res4);
length(res5);
length(res6);


set.seed(1234);
pvalues = runif(10^6/2); # uniform p-values;
t0=proc.time() ;
res1=naiveBH(x=pvalues,alpha=0.10);
t1=proc.time()-t0 ;
t0=proc.time() ;
res2=FastBH(x=pvalues,alpha=0.10);
t2=proc.time()-t0 ;

set.seed(1234);
pvalues = runif(10^6/2); pvalues = pvalues/(10^rbinom(length(pvalues),1,.8)); #.8 of the p-values are U[0,0.1] to make significant results;
t0=proc.time() ;
res3=naiveBH(x=pvalues,alpha=0.10);
t3=proc.time()-t0 ;
t0=proc.time() ;
res4=FastBH(x=pvalues,alpha=0.10);
t4=proc.time()-t0 ;
             
set.seed(1234);
pvalues = runif(10^6/2)/10; #all p-values are set to be U[0,0.1] - to be all significant;
t0=proc.time() ;
res5=naiveBH(x=pvalues,alpha=0.10);
t5=proc.time()-t0 ;
t0=proc.time() ;
res6=FastBH(x=pvalues,alpha=0.10);
t6=proc.time()-t0 ;

#5x10E5;
t1;t2;t3;t4;t5;t6;
length(res1);
length(res2);
length(res3);
length(res4);
length(res5);
length(res6);


set.seed(1234);
pvalues = runif(10^6); # uniform p-values;
t0=proc.time() ;
res1=naiveBH(x=pvalues,alpha=0.10);
t1=proc.time()-t0 ;
t0=proc.time() ;
res2=FastBH(x=pvalues,alpha=0.10);
t2=proc.time()-t0 ;

set.seed(1234);
pvalues = runif(10^6); pvalues = pvalues/(10^rbinom(length(pvalues),1,.8)); #.8 of the p-values are U[0,0.1] to make significant results;
t0=proc.time() ;
res3=naiveBH(x=pvalues,alpha=0.10);
t3=proc.time()-t0 ;
t0=proc.time() ;
res4=FastBH(x=pvalues,alpha=0.10);
t4=proc.time()-t0 ;
             
set.seed(1234);
pvalues = runif(10^6)/10; #all p-values are set to be U[0,0.1] - to be all significant;
t0=proc.time() ;
res5=naiveBH(x=pvalues,alpha=0.10);
t5=proc.time()-t0 ;
t0=proc.time() ;
res6=FastBH(x=pvalues,alpha=0.10);
t6=proc.time()-t0 ;

#10E6;
t1;t2;t3;t4;t5;t6;
length(res1);
length(res2);
length(res3);
length(res4);
length(res5);
length(res6);

set.seed(1234);
pvalues = runif(10^7/2); # uniform p-values;
t0=proc.time() ;
res1=naiveBH(x=pvalues,alpha=0.10);
t1=proc.time()-t0 ;
t0=proc.time() ;
res2=FastBH(x=pvalues,alpha=0.10);
t2=proc.time()-t0 ;

set.seed(1234);
pvalues = runif(10^7/2); pvalues = pvalues/(10^rbinom(length(pvalues),1,.8)); #.8 of the p-values are U[0,0.1] to make significant results;
t0=proc.time() ;
res3=naiveBH(x=pvalues,alpha=0.10);
t3=proc.time()-t0 ;
t0=proc.time() ;
res4=FastBH(x=pvalues,alpha=0.10);
t4=proc.time()-t0 ;
             
set.seed(1234);
pvalues = runif(10^7/2)/10; #all p-values are set to be U[0,0.1] - to be all significant;
t0=proc.time() ;
res5=naiveBH(x=pvalues,alpha=0.10);
t5=proc.time()-t0 ;
t0=proc.time() ;
res6=FastBH(x=pvalues,alpha=0.10);
t6=proc.time()-t0 ;

#5x10E6;
t1;t2;t3;t4;t5;t6;
length(res1);
length(res2);
length(res3);
length(res4);
length(res5);
length(res6);


set.seed(1234);
pvalues = runif(10^7/2); # uniform p-values;
t0=proc.time() ;
res1=naiveBH(x=pvalues,alpha=0.10);
t1=proc.time()-t0 ;
t0=proc.time() ;
res2=FastBH(x=pvalues,alpha=0.10);
t2=proc.time()-t0 ;

set.seed(1234);
pvalues = runif(10^7/2); pvalues = pvalues/(10^rbinom(length(pvalues),1,.8)); #.8 of the p-values are U[0,0.1] to make significant results;
t0=proc.time() ;
res3=naiveBH(x=pvalues,alpha=0.10);
t3=proc.time()-t0 ;
t0=proc.time() ;
res4=FastBH(x=pvalues,alpha=0.10);
t4=proc.time()-t0 ;
             
set.seed(1234);
pvalues = runif(10^7/2)/10; #all p-values are set to be U[0,0.1] - to be all significant;
t0=proc.time() ;
res5=naiveBH(x=pvalues,alpha=0.10);
t5=proc.time()-t0 ;
t0=proc.time() ;
res6=FastBH(x=pvalues,alpha=0.10);
t6=proc.time()-t0 ;

#5x10E6;
t1;t2;t3;t4;t5;t6;
length(res1);
length(res2);
length(res3);
length(res4);
length(res5);
length(res6);

set.seed(1234);
pvalues = runif(10^7); # uniform p-values;
t0=proc.time() ;
res1=naiveBH(x=pvalues,alpha=0.10);
t1=proc.time()-t0 ;
t0=proc.time() ;
res2=FastBH(x=pvalues,alpha=0.10);
t2=proc.time()-t0 ;

set.seed(1234);
pvalues = runif(10^7); pvalues = pvalues/(10^rbinom(length(pvalues),1,.8)); #.8 of the p-values are U[0,0.1] to make significant results;
t0=proc.time() ;
res3=naiveBH(x=pvalues,alpha=0.10);
t3=proc.time()-t0 ;
t0=proc.time() ;
res4=FastBH(x=pvalues,alpha=0.10);
t4=proc.time()-t0 ;
             
set.seed(1234);
pvalues = runif(10^7)/10; #all p-values are set to be U[0,0.1] - to be all significant;
t0=proc.time() ;
res5=naiveBH(x=pvalues,alpha=0.10);
t5=proc.time()-t0 ;
t0=proc.time() ;
res6=FastBH(x=pvalues,alpha=0.10);
t6=proc.time()-t0 ;

#10E7;
t1;t2;t3;t4;t5;t6;
length(res1);
length(res2);
length(res3);
length(res4);
length(res5);
length(res6);

set.seed(1234);
pvalues = runif(10^8/2); # uniform p-values;
t0=proc.time() ;
res1=naiveBH(x=pvalues,alpha=0.10);
t1=proc.time()-t0 ;
t0=proc.time() ;
res2=FastBH(x=pvalues,alpha=0.10);
t2=proc.time()-t0 ;

set.seed(1234);
pvalues = runif(10^8/2); pvalues = pvalues/(10^rbinom(length(pvalues),1,.8)); #.8 of the p-values are U[0,0.1] to make significant results;
t0=proc.time() ;
res3=naiveBH(x=pvalues,alpha=0.10);
t3=proc.time()-t0 ;
t0=proc.time() ;
res4=FastBH(x=pvalues,alpha=0.10);
t4=proc.time()-t0 ;
             
set.seed(1234);
pvalues = runif(10^8/2)/10; #all p-values are set to be U[0,0.1] - to be all significant;
t0=proc.time() ;
res5=naiveBH(x=pvalues,alpha=0.10);
t5=proc.time()-t0 ;
t0=proc.time() ;
res6=FastBH(x=pvalues,alpha=0.10);
t6=proc.time()-t0 ;

#5x10E7;
t1;t2;t3;t4;t5;t6;
length(res1);
length(res2);
length(res3);
length(res4);
length(res5);
length(res6);

set.seed(1234);
pvalues = runif(10^8); # uniform p-values;
t0=proc.time() ;
res1=naiveBH(x=pvalues,alpha=0.10);
t1=proc.time()-t0 ;
t0=proc.time() ;
res2=FastBH(x=pvalues,alpha=0.10);
t2=proc.time()-t0 ;

set.seed(1234);
pvalues = runif(10^8); pvalues = pvalues/(10^rbinom(length(pvalues),1,.8)); #.8 of the p-values are U[0,0.1] to make significant results;
t0=proc.time() ;
res3=naiveBH(x=pvalues,alpha=0.10);
t3=proc.time()-t0 ;
t0=proc.time() ;
res4=FastBH(x=pvalues,alpha=0.10);
t4=proc.time()-t0 ;
             
set.seed(1234);
pvalues = runif(10^8)/10; #all p-values are set to be U[0,0.1] - to be all significant;
t0=proc.time() ;
res5=naiveBH(x=pvalues,alpha=0.10);
t5=proc.time()-t0 ;
t0=proc.time() ;
res6=FastBH(x=pvalues,alpha=0.10);
t6=proc.time()-t0 ;

#10E8;
t1;t2;t3;t4;t5;t6;
length(res1);
length(res2);
length(res3);
length(res4);
length(res5);
length(res6);


set.seed(1234);
pvalues = runif(10^9/5); # uniform p-values;
t0=proc.time() ;
res1=naiveBH(x=pvalues,alpha=0.10);
t1=proc.time()-t0 ;
t0=proc.time() ;
res2=FastBH(x=pvalues,alpha=0.10);
t2=proc.time()-t0 ;

set.seed(1234);
pvalues = runif(10^9/5); pvalues = pvalues/(10^rbinom(length(pvalues),1,.8)); #.8 of the p-values are U[0,0.1] to make significant results;
t0=proc.time() ;
res3=naiveBH(x=pvalues,alpha=0.10);
t3=proc.time()-t0 ;
t0=proc.time() ;
res4=FastBH(x=pvalues,alpha=0.10);
t4=proc.time()-t0 ;
             
set.seed(1234);
pvalues = runif(10^9/5)/10; #all p-values are set to be U[0,0.1] - to be all significant;
t0=proc.time() ;
res5=naiveBH(x=pvalues,alpha=0.10);
t5=proc.time()-t0 ;
t0=proc.time() ;
res6=FastBH(x=pvalues,alpha=0.10);
t6=proc.time()-t0 ;

#2x10E8;
t1;t2;t3;t4;t5;t6;
length(res1);
length(res2);
length(res3);
length(res4);
length(res5);
length(res6);

   
set.seed(1234);
pvalues = runif(10^9/2); # uniform p-values;
t0=proc.time() ;
res1=naiveBH(x=pvalues,alpha=0.10);
t1=proc.time()-t0 ;
t0=proc.time() ;
res2=FastBH(x=pvalues,alpha=0.10);
t2=proc.time()-t0 ;

set.seed(1234);
pvalues = runif(10^9/2); pvalues = pvalues/(10^rbinom(length(pvalues),1,.8)); #.8 of the p-values are U[0,0.1] to make significant results;
t0=proc.time() ;
res3=naiveBH(x=pvalues,alpha=0.10);
t3=proc.time()-t0 ;
t0=proc.time() ;
res4=FastBH(x=pvalues,alpha=0.10);
t4=proc.time()-t0 ;
             
set.seed(1234);
pvalues = runif(10^9/2)/10; #all p-values are set to be U[0,0.1] - to be all significant;
t0=proc.time() ;
res5=naiveBH(x=pvalues,alpha=0.10);
t5=proc.time()-t0 ;
t0=proc.time() ;
res6=FastBH(x=pvalues,alpha=0.10);
t6=proc.time()-t0 ;

#5x10E8;
t1;t2;t3;t4;t5;t6;
length(res1);
length(res2);
length(res3);
length(res4);
length(res5);
length(res6);


