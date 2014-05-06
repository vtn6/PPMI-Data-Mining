trimData <- function(dat) {
  patients = unique(dat$PATNO)
  trimmedDat <<- dat
  for (i in 1:length(patients)) {
    patientDat = dat[dat$PATNO == patients[i],]
    print(patients[i])
    if (sum(is.na(patientDat$CAUDATE_R)) == dim(patientDat)[1]) {
      trimmedDat <<- trimmedDat[trimmedDat$PATNO != patients[i],]
    }
  }
  
  finalDat <<- trimmedDat

}

testTrim <- function(trimmedDat){
  finalDat <<- trimmedDat
  trimmedPats = levels(as.factor(trimmedDat$PATNO))
  print(trimmedPats)
  for (i in 4:dim(finalDat)[2]) {
    print(i)
    for (j in 1:length(trimmedPats)) {
      temp = trimmedDat[trimmedDat$PATNO == trimmedPats[j],]
      temp = temp[,i]
      print(trimmedPats[j])
      if (sum(is.na(temp) == length(temp))) {
        print('column removed')
        finalDat[,i] <<- NULL
        next
      }
    }
  }
}