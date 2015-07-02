\name{ExtractRecSummary}
\alias{ExtractRecSummary}
\title{
Extended Function to Extract Summary Data for each Receiver in the File 
}
\description{
This function extracts summary data for each receiver in the file
}
\usage{
ExtractRecSummary(sInputFile)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sInputFile }{
a data frame containing VTrack archive data, this archive is created using the ReadInputData function
}
}

\examples{
data(crocs)
# Load the crocodile data in the VTrack archive format
#  adding 10 hours to convert from UTC 
Vcrocs <- ReadInputData(infile=crocs,
                        iHoursToAdd=10,
                        fAATAMS=FALSE,
                        fVemcoDualSensor=FALSE,
                        dateformat = NULL,
                        sVemcoFormat='1.0')

# Extract summary table of receivers in the file
ExtractRecSummary(Vcrocs)
}

\details{
duration given in days. 
}


\keyword{spatial}