`begday` <-
function(DAY)
{
    DAYDATE <- as.character(timeDate(as.character(DAY), format =
        "%m/%d/%Y"))
    timeDate(paste(DAYDATE, "00:00:00.000", sep = " "), format =
        "%m/%d/%Y %02H:%02M:%02S.%03N")
}

