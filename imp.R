#Importing datasets
user.df<-read.csv(paste("user.csv"), sep=',')
View(user.df)

rating.df<-read.csv(paste("Rating.csv"), sep=',')
View(rating.df)
dim(rating.df)

item.df<-read.csv(paste("item.csv"), sep=',')
View(item.df)

rating.df<-subset(rating.df[,1:3])
item.df<-subset(item.df[,-4]) 

library(reshape)
data.df<-cast(rating.df, user_id~movie_id)
data.df[is.na(data.df)] <- 0
View(data.df)
write.csv(data.df[,2:1300],'result')

#sampling train and test data
sample <- sample.int(n = nrow(rating.df), size = 70000, replace = F)
train <- rating.df[sample, ]
View(train)

library(reshape)
train.df<-cast(train, user_id~movie_id)
train.df[is.na(train.df)] <- 0
View(train.df)
write.csv(train.df,'train.csv')
dim(train.df)

train.df<-as.data.frame(train[,2:1683])
View(train.df)

#test data
test  <- rating.df[-sample, ]
View(test)
test.df<-cast(test, user_id~movie_id)
test.df[is.na(test.df)] <- 0
write.csv(test.df,'test.csv')
dim(train.df)
dim(test.df)

#Filling up holes
tr<-matrix(nrow=943,ncol=1683)
tr[1:943,1]=c(1:943)
colnames(tr)=c(0:1682)
View(tr)

v=c(colnames(train.df))
v<-array(v)
print(v)
write.csv(as.matrix(v),'vector.csv')
d<-dim(v)
print(d)


for (j in 2:d){
  val=as.integer(v[j])
  m<-(val+1)
  tr[,m]=train.df[,j]
}
View(tr)

w=c(1:1682)
w<-array(w)
w<-setdiff(w,v)
print(w)
w<-array(w)
d1<-dim(w)
print(d1)

t<-c(colnames(test.df))
t<-array(t)
tab<-as.data.frame(t)
View(tab)

n<-nrow(tab)
print(n)
for(i in 1:d1){
  val=as.integer(w[i])
  for(j in 2:n){
    if(val==tab[j,1]){
      tr[,val+1]=test.df[,j]
    }
  }
}
write.csv(tr,'tr.csv')
dim(tr)          

te<-matrix(nrow=943, ncol=1683)
te[1:943,1]=c(1:943)
colnames(te)=c(0:1682)
View(te)

l=c(colnames(test.df))
l<-array(l)
print(l)
write.csv(as.matrix(l),'testvector.csv')
d2<-dim(l)
print(d2)


for (j in 2:d2){
  val=as.integer(l[j])
  m<-(val+1)
  te[,m]=test.df[,j]
}
te[is.na(te)] <- 0
View(te)

for(i in 1:943){
  for(j in 2:1683){
    if(tr[i,j]!=0)
      te[i,j]=0
  }
}
View(tr)
View(te)
View(tr.df)
View(data.df)
#Preprocessing over
#Train test split over

#Checking
check<-matrix(nrow=943, ncol=1682)
check[,1]=c(1:943)
View(check)
colnames(check)<-colnames(data.df)

t<-as.matrix(tr[1:943,2:1683])
tt<-as.matrix(te[1:943,2:1683])
View(t)
for(i in 1:943){
  for(j in 1:1682){
    if(t[i,j]>0)
      check[i,j]=t[i,j]
    
    else if(tt[i,j]==t[i,j])
      check[i,j]=tt[i,j]
    
    else
      check[i,j]=tt[i,j]
  }
}
View(check)
View(d)
d<-as.matrix(data.df[1:943,2:1683])
identical(check,d)
all.equal(check,d)
write.csv(check,'c1.csv')
write.csv(d,'c2.csv')

c=0
for(i in 1:943){
  for(j in 1:1682){
    if (d[i,j]!=check[i,j]){
      print(i)
      print(j)
      c=c+1
    }
  }
}
print(c)

#finding similarity 
#cosine similarity
itemsim.df<-matrix(nrow=1682,ncol=1682)
library(lsa)
v1<-(tr[,2])
v2<-(tr[,3])
co<-cosine(v1,v2)
print(co)

t<-tr[,2:1683]
itemsim.df<-cosine(t)
View(itemsim.df)
write.csv(itemsim.df,'cossim.csv')
#loops to calculate the adjusted cosine similarity
#loops to get row avergaes as the last column
dd<-as.matrix(tr)
dim(dd)
D<-matrix(nrow=943,ncol=1684)
D[,1]=c(1:943)
for ( i in 1:943){
  c=0
  sum=0
  for( j in 2: 1683){
    D[i,j]=dd[i,j]
    if (dd[i,j] !=0){
      sum=sum+dd[i,j]
      c=c+1
    }}
  D[i,1684]=sum/c
}
View(D)   
write.csv(D,'check.csv')

D<-D[,2:1684]
View(D)
N<-matrix(nrow=1682,ncol=1682)
for ( i in 1:1681 ){
  n=i+1
  for ( j in n:1682){
    
    p=0
    q=0
    r=0
    x=0
    y=0
    r=0
    z=0
    for ( k in 1:943 ){
      if (D[k,i]!=0 && D[k,j]!=0 && i!=j){
        p=(D[k,i]-D[k,1683])
        q=(D[k,j]-D[k,1683])
        r=r+p*q
        x=x+p*p
        y=y+q*q
        z=x^0.5 * y^0.5
    }
    }
    s=r/z
    N[i,j]<-s
  }
}
N[is.na(N)] <- -1000
View(N)
View(D)
write.csv(N, 'adjcos.csv')

#calculating the adjusted correlation similarity
M<-matrix(nrow=1682,ncol=1682)
for ( i in 1:1681 ){
  n=i+1
  for ( j in n:1682){
    avgi=0
    avgj=0
    sumi=0
    sumj=0
    c=0
    for ( k in 1:943 ){
      if (D[k,i]!=0 && D[k,j]!=0){
        sumi=sumi+D[k,i]
        sumj=sumj+D[k,j]
        c=c+1
      }}
    avgi=sumi/c
    avgj=sumj/c
    p=0
    q=0
    r=0
    x=0
    y=0
    r=0
    z=0
    for ( k in 1:943 ){
      if (D[k,i]!=0 && D[k,j]!=0){
        p=(D[k,i]-avgi)
        q=(D[k,j]-avgj)
        r=r+p*q
        x=x+p*p
        y=y+q*q
        z=x^0.5 * y^0.5
      }}
    s=r/z
    M[i,j]<-s
  }
}

M[is.na(M)] <- -1000
View(M)
write.csv(M, 'corrsim.csv')


#Calculating the neighbourhoods of all the items
#Neighbourhood of cosine similarity with k=30
nbd1<-matrix(nrow=1682, ncol=150)
I<-itemsim.df
for(k in 1:150){
  for( i in 1:1682){
    big=0
    val=0
    for (j in 1:1682){
      if(I[i,j]>big && i!=j){
        big=I[i,j]
        val=j
      }
      nbd1[i,k]<-val 
      I[i,val]<- -1000
    }
  }
}
View(nbd1)
write.csv(nbd1, 'neighbourhood1.csv')

#Neighbourhood of adjusted cosine similarity with k=30
nbd2<-matrix(nrow=1682, ncol=200)
I<-N
for(k in 1:200){
  for( i in 1:1682){
    big=0
    val=0
    for (j in 1:1682){
      if(I[i,j]>big & i!=j ){
        big=I[i,j]
        val=j
      }
      nbd2[i,k]<-val 
      I[i,val]<- -1000
    }
  }
}
View(nbd2)
nbd2[is.na(nbd2)]<-0


nb2<-matrix(nrow=1682, ncol=30)
nb2[,1]<-c(1:1682)
I<-N
for(k in 2:30){
  for( i in 1:1682){
    big=-100
    val=0
    for (j in 1:1682){
      if(I[i,j]>big  ){
        big=I[i,j]
        val=j
      }
      nb2[i,k]<-val 
      I[i,val]<- -1000
    }
  }
}
View(nb2)
write.csv(nbd2,'neighbourhood2.csv')

#Neighbourhood of adjusted correlation similarity with k=30
nbd3<-matrix(nrow=1682, ncol=200)
I<-M
for(k in 1:200){
  for( i in 1:1682){
    big=-100
    val=0
    for (j in 1:1682){
      if(I[i,j]>big && i!=j ){
        big=I[i,j]
        val=j
      }
      nbd3[i,k]<-val 
      I[i,val]<- -1000
    }
  }
}
View(nbd3)

nb3<-matrix(nrow=1682, ncol=30)
I<-M
nb3[,1]<-c(1:1682)
for(k in 2:30){
  for( i in 1:1682){
    big=-100
    val=0
    for (j in 1:1682){
      if(I[i,j]>big ){
        big=I[i,j]
        val=j
      }
      nb3[i,k]<-val 
      I[i,val]<- -1000
    }
  }
}
View(nbd3)
write.csv(nbd3,'neighbourhood3.csv')


#Prediction with the weighted sum method
#Prediction for cosine similarity
P1<-matrix(nrow=943,ncol=1682)
for (j in 1:1682){
  for (i in 1:943){
    val=0
    s=0
    num=0
    den=0
    for(k in 1:30){
      val=nbd1[j,k]
      if(val!=0){
        s=itemsim.df[j,val]
        num=num+((abs(s))*tr[i,val+1])
        #print(num)
        if(tr[i,val+1]!=0)
          den=den+ abs(s)
      }
    }
    P1[i,j]<-(num/den)
    
  }
}
View(P1)
write.csv(P1, 'pred1.csv')

#prediction of adjusted cosine similarity
View(nbd2)
View(N)
P2<-matrix(nrow=943,ncol=1682)
for (i in 1:943){
  for (j in 1:1682){
    val=0
    s=0
    num=0
    den=0
    
    for(k in 1:50){
      c=0
      val=nbd2[j,k]
      if(val!= 0){
        s=N[j,val]
        num=num+(abs(s)*tr[i,val+1])
        if(tr[i,val+1]!=0)
          den=den+abs(s)
      }
      P2[i,j]<-(num/den)
    }
    
  }
}
View(P2)
write.csv(P2, 'pred2.csv')

#prediction of correlation similarity
P3<-matrix(nrow=943,ncol=1682)
for (i in 1:943){
  for (j in 1:1682){
    val=0
    s=0
    num=0
    den=0
    
    for(k in 1:50){
      c=0
      val=nbd3[j,k]
      if(val!= 0){
        s=M[j,val]
        num=num+(s*tr[i,val+1])
        if(tr[i,val+1]!=0)
          den=den+abs(s)
      }
      
    }
    P3[i,j]<-(num/den)
  }
}
View(P3)
write.csv(P3, 'pred3.csv')

#Finding MAEs
P1[is.na(P1)] <- 0
View(P1)
dim(P1)
P1[is.infinite(P1)] <- 0
cc=0
for(i in 1:943){
  for(j in 1:1682){
    if(P1[i,j]>5)
      P1[i,j]=5
  }
}


mae1=0
c=0
df=0
dim(te)
dim(P1)
for(i in 1:943){
  for(j in 2:1683){
    if(te[i,j]!=0){
      df= df+ abs(P1[i,j-1] - te[i,j])
      c=c+1
      }
    }
}
print(df)
print(c)
mae1=df/c
print(mae1)



P2[is.na(P2)] <- 0
View(P2)
P1[is.infinite(P2)] <- 0
for(i in 1:943){
  for(j in 1:1682){
    if(P2[i,j]>5)
      P2[i,j]=5
  }
}

mae2=0
diff=0
c=0
for(i in 1:943){
  for(j in 1:1682){
    if(te[i,j+1]!=0){
      diff=diff+abs(P2[i,j]-te[i,j])
      c=c+1
      }
    }
  }

print(diff)
print(c)
mae2=diff/c
print(mae2)


P3[is.na(P3)] <- 0
View(P3)
P3[is.infinite(P2)] <- 0
for(i in 1:943){
  for(j in 1:1682){
    if(P3[i,j]>5)
      P3[i,j]=5
  }
}

mae3=0
df=0
c=0
for(i in 1:943){
  for(j in 1:1682){
    if(te[i,j+1]!=0){
      df=df+abs(P3[i,j]-te[i,j])
      c=c+1
      }
    }
  }

print(df)
print(c)
mae3=df/c
print(mae3)

plot(c(mae2,mae1,mae3))
plot(c(mae2,mae1,mae3), xlab='Relative Performance of similarity measure', ylab='MAE')
library(calibrate)
textxy(c('adjusted cosine','cosine','correlation'),'')

#Regression prediction
alpha1<-matrix(nrow=1682, ncol=30)
beta1<-matrix(nrow=1682, ncol=30)
epsilon1<-matrix(nrow=1682, ncol=30)

#testing code
m<-lm(as.vector(tr[,51])~as.vector(tr[,2]))
summary(m)
tv<-(summary(m)$coef[1,'Estimate'])
tv<-coef(summary(m))[, "Std. Error"]
tv<-sqrt(deviance(m)/df.residual(m))
print(tv)

#asigning the various coefficient values to different matrices
for(i in 1:1682){
  for(j in 1:30){
    val=nbd1[i,j]
    v1=as.vector(tr[,(val+1)])
    v2=as.vector(tr[,(i+1)])
    model<-lm(v1~v2)
    beta1[i,j]<-summary(model)$coef[1,'Estimate']
    alpha1[i,j]<-summary(model)$coef[2,'Estimate']
    epsilon1[i,j]<-sqrt(deviance(model)/df.residual(model))
  }
}
View(alpha1)
View(beta1)
View(epsilon1)
dim(nbd1)

R1<-matrix(nrow=943, ncol=1682)
for(i in 1:943){
  for(j in 1:1682){
    for(k in 1:30){
      val=nbd1[j,k]
      if(tr[i,j+1]!=0)
        R1[i,val]=alpha1[j,k]*tr[i,j+1] +beta1[j,k]+epsilon1[j,k]
      else
        R1[i,val]=0
    }
  }
}
View(R1)
R1[is.na(R1)] <- 0

Pr1<-matrix(nrow=943,ncol=1682)
for (j in 1:1682){
  for (i in 1:943){
    val=0
    s=0
    num=0
    den=0
    for(k in 1:30){
      val=nbd1[j,k]
      if(val!=0){
        s=itemsim.df[j,val]
        num=num+((abs(s))*R1[i,val])
        #print(num)
        if(R1[i,val]!=0)
          den=den+ abs(s)
      }
    }
    Pr1[i,j]<-(num/den)
    
  }
}
View(Pr1)
write.csv(Pr1, 'Rpred1.csv')

Pr1[is.na(Pr1)] <- 0
dim(Pr1)
Pr1[is.infinite(Pr1)] <- 0
for(i in 1:943){
  for( j in 1:1682){
    if(Pr1[i,j]>5)
      Pr1[i,j]=5
  }
}

mae1=0
c=0
df=0
dim(te)
dim(P1)
for(i in 1:943){
  for(j in 2:1683){
    if(te[i,j]!=0){
      df= df+ abs(Pr1[i,j-1] - te[i,j])
      c=c+1
    }
  }
}
print(df)
print(c)
mae1=df/c
print(mae1)


#Regression with adjusted cosine 
alpha2<-matrix(nrow=1682, ncol=30)
beta2<-matrix(nrow=1682, ncol=30)
epsilon2<-matrix(nrow=1682, ncol=30)

#asigning the various coefficient values to different matrices
for(i in 1:1682){
  for(j in 1:30){
    val=nbd2[i,j]
    v1=as.vector(tr[,(val+1)])
    v2=as.vector(tr[,(i+1)])
    model<-lm(v1~v2)
    beta2[i,j]<-summary(model)$coef[1,'Estimate']
    alpha2[i,j]<-summary(model)$coef[2,'Estimate']
    epsilon2[i,j]<-sqrt(deviance(model)/df.residual(model))
  }
}
View(alpha2)
View(beta2)
View(epsilon2)
dim(nbd2)

R2<-matrix(nrow=943, ncol=1682)
for(i in 1:943){
  for(j in 1:1682){
    for(k in 1:30){
      val=nbd2[j,k]
      R2[i,val]=alpha2[j,k]*tr[i,j+1] +beta2[j,k]+epsilon2[j,k]
    }
  }
}
View(R2)
R2[is.na(R2)] <- 0

Pr2<-matrix(nrow=943,ncol=1682)
for (j in 1:1682){
  for (i in 1:943){
    val=0
    s=0
    num=0
    den=0
    for(k in 1:10){
      val=nbd2[j,k]
      if(val!=0){
        s=N[j,val]
        num=num+((abs(s))*R2[i,val])
        #if(R2[i,val]!=0)
        den=den+ abs(s)
      }
    }
    Pr2[i,j]<-(num/den)
    
  }
}
View(Pr2)
write.csv(Pr2, 'Rpred2.csv')

Pr2[is.na(Pr2)] <- 0
dim(Pr2)
Pr2[is.infinite(Pr2)] <- 0
for(i in 1:943){
  for(j in 1:1682){
    if(Pr2[i,j]>5)
      Pr2[i,j]=5
  }
}

View(te)
mae2=0
c=0
df=0
dim(te)
dim(P2)
for(i in 1:943){
  for(j in 1:1682){
    if(te[i,j+1]!=0){
      df= df + abs(Pr2[i,j] - te[i,j+1])
      c=c+1
    }
  }
}
print(df)
print(c)
mae2=df/c
print(mae2)

#Regression with adjusted cosine 
alpha3<-matrix(nrow=1682, ncol=30)
beta3<-matrix(nrow=1682, ncol=30)
epsilon3<-matrix(nrow=1682, ncol=30)

#asigning the various coefficient values to different matrices
for(i in 1:1682){
  for(j in 1:30){
    val=nbd2[i,j]
    v1=as.vector(tr[,(val+1)])
    v2=as.vector(tr[,(i+1)])
    model<-lm(v1~v2)
    beta3[i,j]<-summary(model)$coef[1,'Estimate']
    alpha3[i,j]<-summary(model)$coef[2,'Estimate']
    epsilon3[i,j]<-sqrt(deviance(model)/df.residual(model))
  }
}
View(alpha3)
View(beta3)
View(epsilon3)
dim(nbd3)

R3<-matrix(nrow=943, ncol=1682)
for(i in 1:943){
  for(j in 1:1682){
    for(k in 1:30){
      val=nbd3[j,k]
      R3[i,val]=alpha3[j,k]*tr[i,j+1] +beta3[j,k]+epsilon3[j,k]
    }
  }
}
View(R3)
R3[is.na(R3)] <- 0

Pr3<-matrix(nrow=943,ncol=1682)
for (j in 1:1682){
  for (i in 1:943){
    val=0
    s=0
    num=0
    den=0
    for(k in 1:10){
      val=nbd3[j,k]
      if(val!=0){
        s=M[j,val]
        num=num+((abs(s))*R3[i,val])
        if(R3[i,val]!=0)
          den=den+ abs(s)
      }
    }
    Pr3[i,j]<-(num/den)
    
  }
}
View(Pr3)
write.csv(Pr3, 'Rpred3.csv')

Pr3[is.na(Pr3)] <- 0
dim(Pr3)
Pr3[is.infinite(Pr3)] <- 0
for(i in 1:943){
  for(j in 1:1682){
    if(Pr3[i,j]>5)
      Pr3[i,j]=5
  }
}

View(te)
mae3=0
c=0
df=0
dim(te)
for(i in 1:943){
  for(j in 1:1682){
    if(te[i,j+1]!=0){
      df= df + abs(Pr3[i,j] - te[i,j+1])
      c=c+1
    }
  }
}
print(df)
print(c)
mae3=df/c
print(mae3)


#Benchmark user-user process
U<-matrix(nrow=943, ncol=943)
dim(D)
for(i in 1:942){
  n=i+1
  for(j in n:943){
    p=0
    q=0
    r=0
    x=0
    y=0
    z=0
    for(k in 2:1683){
     if(tr[i,k]!=0 && tr[j,k]!=0){
        p=(tr[i,k]-D[i,1683])
        q=(tr[j,k]-D[j,1683])
        r=r+p*q
        x=x+p*p
        y=y+q*q
        z=x^0.5 * y^0.5
      }}
    s=r/z
    U[i,j]=s
      }
    }
    
View(U)
U[is.na(U)] <- -10000
write.csv(U, 'user-similarity.csv')

#Neighbourhood calc
NBD<- matrix (nrow=943, ncol=30)
I<-U
View(I)
for(k in 1:30){
  for(i in 1:943){
    big=0
    val=0
    for(j in 1:943){
      if(I[i,j]>big && i!=j){
        big=I[i,j]
        val=j
      }
      NBD[i,k]<-val
      I[i,val]<- -1000
    }
  }
}
View(NBD)
NBD[is.na(NBD)] <- 0

write.csv(NBD, 'user-neighbourhood.csv')

#finding super user
su=0
big=0
for(i in 1:943){
  c=0
  for(j in 2:1683){
    if(tr[i,j]!=0)
      c=c+1
  }
  if(c>big){
    big=c
    su=i
  }
}
print(su)
print(big)

sup<-as.vector(tr[405,])
print(sup)


#Predicting ratings
dim(D)
PU1<-matrix(nrow=943, ncol=1682)
for(i in 1:943){
  for(j in 2:1683){
    avg=D[i,1683]
    s=0
    z=0
    for(k in 1:30){
      val=NBD[i,k]
      r=tr[val,j]-(D[val,1683]*U[i,val])
      for(l in 1:30){
        v=NBD[i,l]
        s=s+U[i,val]
      }
      z=z+r/s
    }
    p=avg+z
    PU1[i,j-1]=p
    
  }
}

View(PU1)
PU1[is.na(PU1)]<-0

umae=0
d=0
c=0
for(i in 1:943){
  for(j in 2:1683){
    if(te[i,j]!=0){
      d=d+abs(te[i,j]-PU[i,j-1])
      c=c+1
    }
  }
}
umae=d/c
print(c)
print(umae)

PU2<-matrix(nrow=943, ncol=1682)
for(i in 1:943){
  for(j in 2:1683){
    avg=D[i,1683]
    s=0
    r=0
    z=0
    for(k in 1:30){
      val=NBD[i,k]
      r=r+(tr[val,j]-(D[val,1683]*U[i,val]))
    }
    for(l in 1:30){
      v=NBD[i,l]+
      s=s+U[i,val]
    }
    z=z+r/s
    p=avg+z
    PU2[i,j-1]=p
    
  }
}

View(PU2)
PU2[is.na(PU2)]<-0