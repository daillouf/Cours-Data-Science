#setwd("C:/Users/DAILLOUX/Document travail/Cours R/Ex/rprog_data_ProgAssignment3-data")

hdata=read.csv("hospital-data.csv",stringsAsFactor=F)
outdata=read.csv("outcome-of-care-measures.csv",stringsAsFactor=F) 

rankall<-function(outcome,num="best"){
        
       
        i<- outcome==c("heart attack","heart failure", "pneumonia")
        if(!any(i)){
                stop("invalid outcome")
        }
        if(!is.numeric(num) & !num=="best" & !num=="worst"){
                stop("invalid num")
        }
        
        
        
        index=c(11,17,23)
        
        #ces chiffres correpsondent
        #aux trois causes de décès comme indices de colonnes 
        # plus précisement ce sont les taux de mortalité à  30 jour

        n=index[i] 
        
        data=outdata# sélection d'uniquement les hopitaux 
        # de l'État choisi
       
        #ici on obtient les "positions" des hopitaux, ordonnées d'abord dans
        #l'ordre des états (data[,7] puis dans l'ordre du critère [,n] et enfin 
        #par ordre des noms (cas d'égalités) na.last=Na enlève les hopitaux aux valeurs 
        #non saisies
        idx=order(data[,7],as.numeric(data[,n]),data[,2],na.last=NA) 
        
        #jeu de données ordonné
        Rdata=data[idx,]
        #dataframe des résultats, à remplir
        Pays=unique(outdata$State)
        O.Pays=Pays[order(Pays)]
        OutPut<-data.frame(state=O.Pays,hospital=Pays)
        
        OutPut$hospital=as.character(OutPut$hospital)
        for(state in OutPut$state){
                id=(state==Rdata[,7])
                if(!any(id)){
                        
                        
                        #c'est le cas où on a aucune donnée pour
                        #cet état
                        OutPut[OutPut$state==state,"hospital"]=NA       
                }
                else if(is.numeric(num)){
                        if(length(id)<num){
                                OutPut[OutPut$state==state,"hospital"]=NA 
                        }
                        
                        
                        OutPut[OutPut$state==state,"hospital"]=Rdata[id,2][num]
                }
                else if(num=="best"){
                        OutPut[OutPut$state==state,"hospital"]=Rdata[id,2][1]
                }else{
                        OutPut[OutPut$state==state,"hospital"]=Rdata[id,2][length(id)]
                }            
                        
        }
        
        OutPut
}
rankall("heart failure")
