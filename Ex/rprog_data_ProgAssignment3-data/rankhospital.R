
rankhospital<-function(state,outcome,num="best"){
        
        if(!any(state==unique(outdata$State))){
                stop("invalid state")
        }
        i<- outcome==c("heart attack","heart failure", "pneumonia")
        if(!any(i)){
                stop("invalid outcome")
        }
        if(!is.numeric(num) & !num=="best" & !num=="worst"){
                stop("invalid num")
        }
        
        
        
        index=c(11,17,23) #ces chiffres correpsondent
        #aux trois causes de dÃ©cÃ¨s comme indices de colonnes 
        # plus prÃ©cisement ce sont les taux de mortalitÃ© Ã  30 jours
        n=index[i] #numÃ©ro de colonne Ã  considÃ©rer pour comparison
        
        data=outdata[which(outdata[,7]==state),]# sÃ©lection d'uniquement les hopitaux 
        # de l'Ã©tat choisi
        data=data[which(!is.na(as.numeric(data[,n]))),]
        
        idx=order(as.numeric(data[,n]),data[,2])
        
        name=data[idx,2]
        
        if(is.numeric(num)){
                if(length(name)<num){
                        return(NA)
                }
                return(name[[num]])
        }
        else if(num=="best"){
                return(name[[1]])
        }
        else{
                return(name[[length(name)]])
        }
        
}
