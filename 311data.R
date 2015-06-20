simpleCap <- function(x) {
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1,1)), substring(s, 2),
              sep="", collapse=" ")
}

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
        
        #Creates a table of observation counts for code by methodReceived
        counts.df<-as.data.frame(
                cbind(as.character(closed.data$code),
                        as.character(closed.data$methodReceived)))
        counts.table<-table(counts.df)
        counts.table<<-counts.table
        print("Ready to run other functions")
}
 
graph.all <- function(stat.test = "median", min.method = 0, min.code = 0, 
                           png.name = paste0(stat.test,"-311-data.png")) {
        
        #Confirms data.load() had been run successfully
        if (exists("closed.data")==FALSE) {
                stop("Run data.load() first")
        }
        
        #Removes reporting methods and 311 codes that have not met specified reporting minimums
        method.table<-table(closed.data$methodReceived)
        code.table<-table(closed.data$code)
        method.index<- method.table > min.method
        code.index<- code.table > min.code
        method.names <- names(method.table[method.index])
        code.names <- names(code.table[code.index])
        final.data<-closed.data[closed.data$methodReceived %in% method.names,]
        final.data<-final.data[final.data$code %in% code.names,]
        final.counts<-counts.table[code.names,method.names]
        
        #Calculates the mean time to CLOSED status by method received and creates a bar chart
        total.day<-aggregate(finishDays~methodReceived, data = final.data, stat.test)
        total.day$counts<-colSums(final.counts)
        total.day<-total.day[order(total.day$finishDays),]
        total.day$finishDays<-as.numeric(total.day$finishDays)
        total.day$methodReceived = with(total.day, reorder(methodReceived, 1:length(methodReceived)))
        
        png(png.name)
        plot<-ggplot(total.day, 
                     aes(x = methodReceived, y=finishDays)) + 
                geom_bar(stat="identity", fill = "grey") +
                ylab("Days from Creation to Closed Status") +
                xlab("Method Received") +
                ggtitle(paste(simpleCap(stat.test),"Time from Creation to Closed Status \n by Method Received", sep=" ")) +
                theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
                geom_text(data=total.day, aes(y=0),
                          label=paste(total.day$counts,"obs"),
                          vjust=0, hjust=0, angle=90, size = 3)
        print(plot)
        dev.off()
        print("Success!")
}

graph.code <- function(report.code, stat.test= "median", min.method = 0, 
                             min.code = 0, png.name = paste0(report.code,"-",stat.test,"-data.png")) {
        
        #Confirms data.load() had been run successfully
        if (exists("closed.data")==FALSE) {
                stop("Run data.load() first")
        }
        
        require(reshape2)
        
        #Removes reporting methods and 311 codes that have not met specified reporting minimums
        method.table<-table(closed.data$methodReceived)
        code.table<-table(closed.data$code)
        method.index<- method.table > min.method
        code.index<- code.table > min.code
        method.names <- names(method.table[method.index])
        code.names <- names(code.table[code.index])
        final.data<-closed.data[closed.data$methodReceived %in% method.names,]
        final.data<-final.data[final.data$code %in% code.names,]
        final.counts<-counts.table[code.names,method.names]
        final.counts<-as.data.frame(t(final.counts))
        #Calculates the mean of finishDay for methodReceived by code
        
        suppressMessages(full.data<-dcast(
                        final.data, methodReceived ~ code, get(stat.test), na.rm=TRUE))
        
        #Pulls out specified code from full.data and removes NaN values
        code.data<-setNames(full.data[,report.code],full.data$methodReceived)
        count.data<-setNames(final.counts[,report.code],full.data$methodReceived)
        code.data<-code.data[!is.na(code.data)]
        count.data<-count.data[(count.data)!=0]
        count.data<-count.data[order(code.data)]
        code.data<-code.data[order(code.data)]
        code.data<-as.data.frame(code.data)
        count.data<-as.data.frame(count.data)
        code.data$count.data<-count.data[,1]
        code.data$methodReceived<-rownames(code.data)
        code.data$methodReceived = with(code.data, reorder(methodReceived, 1:length(methodReceived)))
        #Creates a barplot
        png(png.name)
        plot<-ggplot(code.data, 
                     aes(x = methodReceived, y=code.data)) + 
                geom_bar(stat="identity", fill = "grey") +
                ylab("Days from Creation to Closed Status") +
                xlab("Method Received") +
                ggtitle(paste("Baltimore 311 Reports for",  report.code, "\n", simpleCap(stat.test),"Days to Closed Status by Method Received", sep=" ")) +
                theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
                geom_text(data=code.data, aes(y=0),
                          label=paste(code.data$count.data,"obs"),
                          vjust=0, hjust=0, angle=90, size = 3)
        print(plot)
        dev.off()
        print("Success!")
}
