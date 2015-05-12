

best<-function(state,outcome){
        
        if(!any(state==unique(outdata$State))){
                stop("invalid state")
        }
        i<- outcome==c("heart attack","heart failure", "pneumonia")
        if(!any(i)){
                stop("invalid outcome")
        }
        index=c(11,17,23) #ces chiffres correpsondent
        #aux trois causes de décès comme indices de colonnes 
        # plus précisement ce sont les taux de mortalité à 30 jours
        n=index[i] #numéro de colonne à considérer pour comparison
        
        data=outdata[which(outdata[,7]==state),]# sélection d'uniquement les hopitaux 
        # de l'état choisi
        
        idx=order(as.numeric(data[,n]),data[,2])
        
        name=data[idx,2]
       
        
        return(name[[1]])
}

