rankall <- function(outcome, num = "best") {
        ## Read outcome data: COLS: HospitalName, State, HeartAttack, HearFailure, Pneumonia
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")[,c(2,7,11,17,23)]
        ## Check that state and outcome are valid
        if(! (outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia") ) {
                stop("invalid outcome")
        }
        if(class(num) == "character"){
                if (! (num == "best" || num == "worst")){
                        stop("invalid number")
                }
        }
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the (abbreviated) state name
        # Remove columns by outcome, only left HospitalName and Deaths by outcome
        if(outcome == "heart attack") {
                data = data[,c(1,2,3)]
        } else if(outcome == "heart failure") {
                data = data[,c(1,2,4)]
        } else if(outcome == "pneumonia") {
                data = data[,c(1,2,5)]
        }
        names(data)[3] = "Deaths"
        data[, 3] = suppressWarnings( as.numeric(data[, 3]) )
        # Remove rows with NA
        data = data[!is.na(data$Deaths),]
        splited = split(data, data$State)
        ans = lapply(splited, function(x){
                id=order(x$Deaths,x[[1]])
                x=x[id,]
                
                if(num=="best"){
                        return(x[[1]][1])
                }
                if(num=="worst"){
                        return(x[[1]][length(id)])
                }
                if(class(num)=="numeric"){
                        return(x[[1]][num])
                }

                
                
        })
        return(data.frame(hospital=unlist(ans),state=names(ans)))
}