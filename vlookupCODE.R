#Vlookup in R
#Version 0.3 November 12, 2013
#Return senesical results if return column is a factor
#Version 0.2 November 11, 2013
#Require first column of table to be numeric if range lookup is being done
#Change defaults to larger=FALSE
#Julin Maloof

vlookup <- function(ref, #the value or values that you want to look for
                    table, #the table where you want to look for it; will look in first column
                    column, #the column that you want the return data to come from,
                    range=FALSE, #if there is not an exact match, return the closest?
                    larger=FALSE) #if doing a range lookup, should the smaller or larger key be used?)
{
  if(!is.numeric(column) & !column %in% colnames(table)) {
    stop(paste("can't find column",column,"in table"))
  }
  if(range) {
    if(!is.numeric(table[,1])) {
      stop(paste("The first column of table must be numeric when using range lookup"))
    }
    table <- table[order(table[,1]),] 
    index <- findInterval(ref,table[,1])
    if(larger) {
      index <- ifelse(ref %in% table[,1],index,index+1)
    }
    output <- table[index,column]
    output[!index <= dim(table)[1]] <- NA
    
  } else {
    output <- table[match(ref,table[,1]),column]
    output[!ref %in% table[,1]] <- NA #not needed?
  }
  dim(output) <- dim(ref)
  output
}

###Examples

#create the table of values where we want to do the lookup
key <- data.frame(mic=seq(10,1000,10),
                  pvalue=seq(0,1,length.out=100))

#look up a single value
vlookup(23,key,2) #NA because range lookup is false
vlookup(23,key,2,TRUE) 

#look up values from a vector
vlookup(c(43,50,23,2),key,2,TRUE)
vlookup(c(43,50,23,2),key,2)
vlookup(c(100,50,20,200),key,2)

#look up values from a matrix
mic.values <- matrix(runif(10*10,min=10,max=1000),ncol=10)
pvalues <- vlookup(mic.values,key,2,T)
mic.values
pvalues

#how does it scale?
mic.values <- matrix(runif(1000*1000,min=10,max=1000),ncol=100)
system.time(pvalues <- vlookup(mic.values,key,2,T)) # 1 second

mic.values <- matrix(runif(5000*5000,min=10,max=1000),ncol=5000)
system.time(pvalues <- vlookup(mic.values,key,2,T)) # 25 seconds

#mic.values <- matrix(runif(10000*10000,min=10,max=1000),ncol=100)
#system.time(pvalues <- vlookup(mic.values,key,2,T)) # 99 seconds #maybe faster with more memory


#test for factors
keyF <- data.frame(mic=seq(10,1000,10),
                   pvalue=factor(seq(0,1,length.out=100)))

vlookup(30,table=keyF,column=2,range=T)
