#Download the data
#First lets do a little side project to build an autonaming download function
#it should take the last part of the URL and make it into the filename

smartDownload <- function(address){
        name <- gsub("%[0-9]{2}","",address) #remove those weird %xx expressions
        name <- gsub("^.*/","",name)
        download.file(address, destfile = name)
}

xlsxToDT <- function(filePath){
        require(data.table)
        require(magrittr)
        sheetNames <- excel_sheets(filePath)
        Data.Tables <- lapply(sheetNames,function(x)read_xlsx(filePath,sheet = x) %>%
                                      as.data.table()) 
        names(Data.Tables) <- sheetNames
        Data.Tables
}

AddAllVarNames <- function(dictionaryDT){
        dictionaryDT[,initialOrder := 1:.N]
        setkey(dictionaryDT,initialOrder)
        varNames <- dictionaryDT[,unique(`VARIABLE NAME`)]
        #store all of the row numbers for each of the first instances of each variable
        rowstoNameBy <- dictionaryDT[!is.na(`VARIABLE NAME`),initialOrder]
        # somehow an NA value got into the varNames vector. Get rid of it
        varNames <- varNames[!is.na(varNames)]
        for(i in 1:length(rowstoNameBy)){
                dictionaryDT[initialOrder>=rowstoNameBy[i],AllVarNames:=varNames[i]]
        }
}


showSummaries <- function(summaryList,tempmax=3000){
        options(max.print = tempmax)
        print(summaryList)
        options(max.print = 1000)
}

DictionarySearch <- function(dictionary,searchterm,begginingWith = FALSE){
                                        #this takes a data.table and single element character
        dictionary[grep]
}