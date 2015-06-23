simpleCap <- function(x) {
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1,1)), substring(s, 2),
              sep="", collapse=" ")
}

data.load <- function(file) {
        #Loads required packages and reads in the .csv file
        require(stringr)
        require(ggplot2)
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
 
graph.all <- function(stat.test = "median", x = "methodReceived", min.x = 0, 
                           png.name = paste0(stat.test,"-", x,"-311-data.png")) {
        
        #Confirms data.load() had been run successfully
        if (exists("closed.data")==FALSE) {
                stop("Run data.load() first")
        }
        
        #Replaces blank factor names in x, if present, with "BLANK NAME"
        levels(closed.data[[x]])[nchar(levels(closed.data[[x]]))==0]<-"*BLANK NAME*"
        
        #Creates a table of observation counts for code by methodReceived
        counts.df<-as.data.frame(
                cbind(as.character(closed.data$code),
                      as.character(closed.data[[x]])))
        counts.table<-table(counts.df)
        
        #Removes reporting methods and 311 codes that have not met specified reporting minimums
        x.table<-table(closed.data[[x]])
        code.table<-table(closed.data$code)
        x.index<- x.table > min.x
        code.index<- code.table > 0       
        x.names <- names(x.table[x.index])
        code.names <- names(code.table[code.index])
        final.data<-closed.data[closed.data[[x]] %in% x.names,]
        final.data<-final.data[final.data$code %in% code.names,]
        final.counts<-counts.table[code.names,x.names]
        
        #Calculates the mean time to CLOSED status by method received and creates a bar chart
        total.day<-aggregate(finishDays~ final.data[[x]], data = final.data, stat.test)
        total.day$counts<-colSums(final.counts)
        total.day<-total.day[order(total.day$finishDays),]
        total.day$finishDays<-as.numeric(total.day$finishDays)
        names(total.day)[1]<-x
        total.day[[x]] = with(total.day, reorder(total.day[[x]], 1:length(total.day[[x]])))
        
        png(png.name, 480+3*length(levels(final.data[[x]])))
        plot<-ggplot(total.day, 
                     aes(total.day[[x]], y=finishDays), , environment = environment()) + 
                geom_bar(stat="identity", fill = "grey") +
                ylab("Days from Creation to Closed Status") +
                xlab(x) +
                ggtitle(paste(simpleCap(stat.test),"Time from Creation to Closed Status \n by", x, sep=" ")) +
                theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
                geom_text(data=total.day, aes(y=.5*max(total.day$finishDays)),
                          label=paste(total.day$counts,"obs"),
                          hjust=0, vjust=0, angle=90, size = 3) 
                
        print(plot)
        
        dev.off()
        print("Success!")
}

graph.code <- function(y.value, x = "methodReceived", y = "code", stat.test= "median", min.x = 0, 
                             png.name = paste0(y,"-",y.value,"-",x,"-",stat.test,"-data.png")) {
        
        #Confirms data.load() had been run successfully
        if (exists("closed.data")==FALSE) {
                stop("Run data.load() first")
        }
        
        require(reshape2)
        
        #Replaces blank factor names in x or y, if present, with "BLANK NAME"
        levels(closed.data[[x]])[nchar(levels(closed.data[[x]]))==0]<-"*BLANK NAME*"
        levels(closed.data[[y]])[nchar(levels(closed.data[[y]]))==0]<-"*BLANK NAME*"
        #Creates a table of observation counts for code by methodReceived
        counts.df<-as.data.frame(
                cbind(as.character(closed.data[[y]]),
                      as.character(closed.data[[x]])))
        counts.table<-table(counts.df)
        
        #Removes reporting methods and 311 codes that have not met specified reporting minimums
        x.table<-table(closed.data[[x]])
        y.table<-table(closed.data[[y]])
        x.index<- x.table > 0
        y.index<- y.table > 0
        x.names <- names(x.table[x.index])
        y.names <- names(y.table[y.index])
        final.data<-closed.data[closed.data[[x]] %in% x.names,]
        final.data<-final.data[final.data[[y]] %in% y.names,]
        final.counts<-counts.table[y.names,x.names]
        final.counts<-as.data.frame(t(final.counts))
        #Calculates the mean of finishDay for methodReceived by code
        
        suppressMessages(full.data<-dcast(
                        final.data, final.data[[x]] ~ final.data[[y]], get(stat.test), na.rm=TRUE))
        names(full.data)[1]<-x
        
        #Pulls out specified code from full.data and removes NaN values
        y.data<-setNames(full.data[,y.value],full.data[[x]])
        count.data<-setNames(final.counts[,y.value],full.data[[x]])
        y.data<-y.data[!is.na(y.data)]
        count.data<-count.data[(count.data)!=0]
        count.data<-count.data[order(y.data)]
        y.data<-y.data[order(y.data)]
        y.data<-as.data.frame(y.data)
        count.data<-as.data.frame(count.data)
        y.data$count.data<-count.data[,1]
        y.data[[x]]<-rownames(y.data)
        y.data[[x]] = reorder(y.data[[3]], 1:length(y.data[[3]]))
        names(y.data)[1]<-y
        y.data<-y.data[y.data$count.data>=min.x,]
        #Creates a barplot
        png(png.name, 500+4*length(levels(y.data[[x]])))
        plot<-ggplot(y.data, 
                     aes(y.data[[x]], y=y.data[[y]]), environment = environment()) + 
                geom_bar(stat="identity", fill = "grey") +
                ylab("Days from Creation to Closed Status") +
                xlab(x) +
                ggtitle(paste(simpleCap(stat.test),"Time from Creation to Closed Status \n", y, y.value,"by", x, sep=" ")) +
                theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
                geom_text(data=y.data, aes(y=0),
                          label=paste(y.data$count.data,"obs"),
                          hjust=0, vjust=0, angle=90, size = 3) 
        print(plot)
        dev.off()
        print("Success!")
}
code.desc <- function (code) {
    grep(code, closed.data$code)->code.name
    code.name<-code.name[1]
    paste(code,"=",as.character((closed.data$codeDescription[[code.name]]), sep = " "))
}
    
    