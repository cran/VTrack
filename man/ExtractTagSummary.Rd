\name{ExtractTagSummary}
\alias{ExtractTagSummary}
\title{
Function to Extract Summary Data for each Transmitter in the File 
}
\description{
This function extracts summary data for each transmitter in the File 
}
\usage{
ExtractTagSummary(sInputFile)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sInputFile }{
a data frame containing VTrack archive data, this archive is created using the ReadInputData function
}
}

\examples{
# Load the crocodile data in the VTrack 1.0 archive format
#  adding 10 hours to convert from UTC 
data(crocs)
Vcrocs <- ReadInputData(infile=crocs,
                        iHoursToAdd=10,
                        fAATAMS=FALSE,
                        fVemcoDualSensor=FALSE,
                        dateformat = NULL,
                        sVemcoFormat='1.0')

# Extract summary table of transmitters in the file
ExtractTagSummary(Vcrocs)
}

\details{
distances are given in meters
}


\keyword{spatial}