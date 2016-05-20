`readindex` <-
function(FRAME)
{
    # Use the data frame FRAME resulting from the import of one of the Energy indexes
    # created from the DOW JONES data base, and create a tri-variate timeSeries with
    # components cap (for capitalization), divisor and index.
    names(FRAME) <- c("date", "nbstock", "cap", "divisor", "index")
    timeSeries(pos = makedate(FRAME[, 1]), data = FRAME[3:5])
}

