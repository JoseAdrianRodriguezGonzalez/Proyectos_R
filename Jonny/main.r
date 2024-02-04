 data<-read.csv("Jonny.csv")
 id_cls <- data$Class
 (id_c <- data$Class=="pos")
 n_cp <- sum(id_c)
 n_cn <- sum(!id_c)
 nd <- ncol(data)
 nu <- matrix(0, 1, nd-2)
 for (i1 in 2:(nd-1)) {
   du <- unique(data[,i1])
   ndi <- length(du)
   nu[i1-1] <- ndi
   mC <- matrix(0, ndi, 3)
   for (i2 in seq_along(du)) {
     id1 <- data[,i1]==du[i2]
     id2 <- id1&id_c
     id3 <- id1&!id_c
     mC[i2, 1] <- sum(id2)
     mC[i2, 2] <- sum(id3)
   }
   mC[,3] <- mC[,1]/n_cp-mC[,2]/n_cn
   lt_n <- list(du, mC)
   if(i1==2)
     lt_c <- lt_n
   else
     lt_c <- c(lt_c, lt_n)
  
 }
 
 nt <- sum(nu)
 #####Valores de porcentajes teoricos
 ProbalShape<-c(lt_c[[2]][(1:3),3]+0.5)
 ProbalSize<-c(lt_c[[4]][(1:2),3]+0.5)
 ProbalColor<-c(lt_c[[6]][(1:3),3]+0.5)
 ProbalSizeI<-c(lt_c[[8]][1:2,3]+0.5)
 ProbalColorI<-c(lt_c[[10]][1:3,3]+0.5)
 MultProb<- outer(ProbalShape,ProbalSize,"*")
 arr<-c(t(MultProb))
 ###colores
 multprob2<-outer(arr,ProbalColor,"*")
 arr1<-c(t(multprob2))
 print(arr1)
 ###formas internas
 ##posbile
 multprob3<-outer(arr1,ProbalSizeI,"*")
 arr2<-c(t(multprob3))
 ##real
 ##ultimo dato
 multprob3<-outer(arr2,ProbalColorI,"*")
 arr3<-c(t(multprob3))
 for(i1 in(1:3)){
   for(i2 in(1:2)){
     for( i3 in (1:3)){
       for(i4 in (1:2)){
         for(i5 in(1:3)){
           combiG3<-(which(data$Shape==(lt_c[[1]][i1])&data$Crust.Size==(lt_c[[3]][i2])&data$Crust.Shade==(lt_c[[5]][i3])&data$Filling.Size==(lt_c[[7]][i4])&data$Filling.Shade==(lt_c[[9]][i5])))<=6
           combiNG3<-(which(data$Shape==(lt_c[[1]][i1])&data$Crust.Size==(lt_c[[3]][i2])&data$Crust.Shade==(lt_c[[5]][i3])&data$Filling.Size==(lt_c[[7]][i4])&data$Filling.Shade==(lt_c[[9]][i5])))>6
           ver3<-sum(sum(combiG3)+sum(combiNG3))
           Probc3=(sum(combiG3))/n_cp-(sum(combiNG3))/n_cp
           if(ver3!=0){
             Probc3<-Probc3+0.5
           }
           lt_r3<-list(Probc3)
           if(i2==1&i1==1&i3==1&i4==1&i5==1){
             lt_reale<-lt_r3
           }else{
             lt_reale<-c(lt_reale,lt_r3)
           }
         }
       }
     }
   }
 }
 # Crear un dataframe con las probabilidades teóricas y observadas
 df_probabilidades3 <- data.frame(
   Shape = rep(lt_c[[1]], each = 36),
   Crust.Size = rep(lt_c[[3]], each =18),
   Crust.Shade = rep(lt_c[[5]], each = 6),
   Filling.size = rep(lt_c[[7]], each = 3),
   Filling.Shade=rep(lt_c[[9]], times = length(lt_c[[9]])),
   ProbTeorica = arr3,
   ProbObservada = unlist(lt_reale)
 )
 print(df_probabilidades3)
 combinacion_maxima3 <- df_probabilidades3[df_probabilidades3$ProbObservada == max(df_probabilidades3$ProbObservada), ]
 combinamax_te<-combinacion_maxima3[combinacion_maxima3$ProbTeorica == max(combinacion_maxima3$ProbTeorica), ]
 # Imprimir la combinación con la mayor probabilidad observada
 print("Combinación con la mayor probabilidad observada:")
 combinacion_maxima3$ProbObservada<-(combinacion_maxima3$ProbObservada-0.5)*100
 combinacion_maxima3$ProbTeorica<-(combinacion_maxima3$ProbTeorica-0.5)*100
 print(combinacion_maxima3)
 print("Los pasteles que más le gusta a Jonny o es mejor venderle")
 combinamax_te$ProbObservada<-(combinamax_te$ProbObservada-0.5)*100
 combinamax_te$ProbTeorica<-(combinamax_te$ProbTeorica-0.5)*100
 print(combinamax_te)