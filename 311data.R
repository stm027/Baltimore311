data.load <- function(file) {
        #Loads required packages and reads in the .csv file
        require(stringr)
        data<-read.csv(file)
        
        #Keeps only reports with a CLOSED status
        closed.data<-data[data$status %in% "CLOSED",]
        
        #Correctly formats dates
        closed.data$statusDate<-as.Date(
                strptime(closed.data$statusDate, "%m/%d/%Y"))
        closed.data$createdDate<-as.Date(
                strptime(closed.data$createdDate, "%m/%d/%Y"))
        
        #Creates a variable finishDays describing days from creation to marking as CLOSED
        closed.data$finishDays<-closed.data$statusDate - closed.data$createdDate
        closed.data<<-closed.data
        print("Ready to run other functions")
}
 
data.graph.all <- function(min.reports.method = 0, min.reports.code = 0, jpg.name = "311data.jpg") {
        
        #Confirms data.load() had been run successfully
        if (exists("closed.data")==FALSE) {
                stop("Run data.load() first")
        }
               
        #Removes reporting methods and 311 codes that have not met specified reporting minimums
        method.table<-table(closed.data$methodReceived)
        code.table<-table(closed.data$code)
        method.index<- method.table > min.reports.method
        code.index<- code.table > min.reports.code
        method.names <- names(method.table[method.index])
        code.names <- names(code.table[code.index])
        final.data<-closed.data[closed.data$methodReceived %in% method.names,]
        final.data.2<-final.data[final.data$code %in% code.names,]
        
        #Calculates the mean time to CLOSED status by method received and creates a bar chart
        total.finishDay<-aggregate(finishDays~methodReceived, data = final.data.2, mean)
        total.finishDay<-total.finishDay[order(total.finishDay$finishDays),]
        jpeg(jpg.name)
        barplot(
                as.numeric(total.finishDay$finishDays), 
                main = paste("Baltimore 311 Reports \n Minimum Method Observations =", min.reports.method, "\n Minimum Code Observations =", min.reports.code, sep = " "), 
                names.arg = total.finishDay$methodReceived, 
                las = 2, 
                cex.names = .8, 
                ylab = "Average Days to Closed Status", 
                font.main=2)
        dev.off()
        print("Success!")
}

data.graph.individual <- function(min.reports.method = 0, min.reports.code = 0, CODE, jpg.name = "311data.jpg") {
        
        #Confirms data.load() had been run successfully
        if (exists("closed.data")==FALSE) {
                stop("Run data.load() first")
        }
        
        require(reshape2)
        
        #Removes reporting methods and 311 codes that have not met specified reporting minimums
        method.table<-table(closed.data$methodReceived)
        code.table<-table(closed.data$code)
        method.index<- method.table > min.reports.method
        code.index<- code.table > min.reports.code
        method.names <- names(method.table[method.index])
        code.names <- names(code.table[code.index])
        final.data<-closed.data[closed.data$methodReceived %in% method.names,]
        final.data.2<-final.data[final.data$code %in% code.names,]
        
        #Calculates the mean of finishDay for methodReceived by code
        suppressMessages(
                full.data<-dcast(
                        final.data.2, methodReceived ~ code, mean, na.rm = TRUE))
        
        #Pulls out specified code from full.data and removes NaN values
        code.data<-setNames(full.data[,CODE],full.data$methodReceived)
        code.data<-code.data[!is.nan(code.data)]
        code.data<-code.data[order(code.data)]
        
        #Creates a barplot
        jpeg(jpg.name)
        barplot(
                code.data, 
                main=paste("Baltimore 311 Reports for",  CODE, "\n Average Days to Closed Status by Method Received", sep=" "), 
                names.arg = names(code.data), 
                las = 2, 
                cex.names = .8, 
                ylab = "Average Days to Closed Status", 
                font.main=2)
        dev.off()
        print("Success!")
}

data.graph.all.median <- function(min.reports.method = 0, min.reports.code = 0, jpg.name = "311data.jpg") {
        
        #Confirms data.load() had been run successfully
        if (exists("closed.data")==FALSE) {
                stop("Run data.load() first")
        }
        
        #Removes reporting methods and 311 codes that have not met specified reporting minimums
        method.table<-table(closed.data$methodReceived)
        code.table<-table(closed.data$code)
        method.index<- method.table > min.reports.method
        code.index<- code.table > min.reports.code
        method.names <- names(method.table[method.index])
        code.names <- names(code.table[code.index])
        final.data<-closed.data[closed.data$methodReceived %in% method.names,]
        final.data.2<-final.data[final.data$code %in% code.names,]
        
        #Calculates the median time to CLOSED status by method received and creates a bar chart
        total.finishDay<-aggregate(finishDays~methodReceived, data = final.data.2, median)
        total.finishDay<-total.finishDay[order(total.finishDay$finishDays),]
        jpeg(jpg.name)
        barplot(
                as.numeric(total.finishDay$finishDays), 
                main = paste("Baltimore 311 Reports \n Minimum Method Observations =", min.reports.method, "\n Minimum Code Observations =", min.reports.code, sep = " "), 
                names.arg = total.finishDay$methodReceived, 
                las = 2, 
                cex.names = .8, 
                ylab = "Median Days to Closed Status", 
                font.main=2)
        dev.off()
        print("Success!")
}

data.graph.individual.median <- function(min.reports.method = 0, min.reports.code = 0, CODE, jpg.name = "311data.jpg") {
        
        #Confirms data.load() had been run successfully
        if (exists("closed.data")==FALSE) {
                stop("Run data.load() first")
        }
        
        require(reshape2)
        
        #Removes reporting methods and 311 codes that have not met specified reporting minimums
        method.table<-table(closed.data$methodReceived)
        code.table<-table(closed.data$code)
        method.index<- method.table > min.reports.method
        code.index<- code.table > min.reports.code
        method.names <- names(method.table[method.index])
        code.names <- names(code.table[code.index])
        final.data<-closed.data[closed.data$methodReceived %in% method.names,]
        final.data.2<-final.data[final.data$code %in% code.names,]
        
        #Calculates the median of finishDay for methodReceived by code
        suppressMessages(
                full.data<-dcast(
                        final.data.2, methodReceived ~ code, median, na.rm = TRUE))
        
        #Pulls out specified code from full.data and removes Na values
        code.data<-setNames(full.data[,CODE],full.data$methodReceived)
        code.data<-code.data[!is.na(code.data)]
        code.data<-code.data[order(code.data)]
        
        #Creates a barplot
        jpeg(jpg.name)
        barplot(
                code.data, 
                main=paste("Baltimore 311 Reports for",  CODE, "\n Median Days to Closed Status by Method Received", sep=" "), 
                names.arg = names(code.data), 
                las = 2, 
                cex.names = .8, 
                ylab = "Median Days to Closed Status", 
                font.main=2)
        dev.off()
        print("Success!")
}