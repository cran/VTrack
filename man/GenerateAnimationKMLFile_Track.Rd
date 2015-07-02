\name{GenerateAnimationKMLFile_Track}
\alias{GenerateAnimationKMLFile_Track}
\title{
Create Animation of Transmitter Track to View in Google Earth
}
\description{
This function creates a Keyhole Markup Language (KML) animation of horizontal movements that can be displayed in Google Earth. The animation shows when a transmitter was within the detection field of a receiver and when it moved between receivers or stations. Users can adjust the time slider to visualise individual time periods for display. 
}
\usage{
GenerateAnimationKMLFile_Track(sInputFile, sid, sPointsFile, 
                            sOutputFile, sTrackColour)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sInputFile }{
a data frame containing VTrack archive data, this archive is created using the ReadInputData function
}
  \item{sid }{
a string variable containing a single TRANSMITTERID
}
  \item{sPointsFile }{
a data frame containing the RECEIVERID, the coordinates and the detection RADIUS in meters. This should be in the format LOCATION, LATITUDE, LONGITUDE, RADIUS
}
  \item{sOutputFile }{
a string detailing the location and name of the output kml file to be created
}
  \item{sTrackColour }{
colour of the tracks in the output .kml
}
}
\details{
the output is a .kml that can be viewed as an animation in Google Earth
}

\seealso{
\code{\link{ReadInputData}}, \code{\link{RunResidenceExtraction}}, \code{\link{GenerateAnimationKMLFile}}
}

\author{
Ross Dwyer, Matthew Watts, Hamish Campbell
}
\examples{

\dontrun{
###GenerateAnimationKMLFile_Track example

# Note, users must download Google Earth in order to visualise the kml

# Load crocodile datset into VTrack archive
data(crocs)
Vcrocs <- ReadInputData(infile=crocs,
                        iHoursToAdd=10,
                        fAATAMS=FALSE,
                        fVemcoDualSensor=FALSE,
                        dateformat = NULL,
                        sVemcoFormat='1.0')

# Load Wenlock points file and generate circuitous distance matrix
data(PointsDirect_crocs)

# Set working directory (in this case a temporary directory)
setwd(tempdir())

(TransmitterList <- ExtractUniqueValues(Vcrocs,2)) # Extract the transmitter names
TransmitterList[1] # Let's create the track for this tag

# Run the function to generate the KML for a single transmitter
GenerateAnimationKMLFile_Track(Vcrocs,
                               TransmitterList[1],
                               XYVR2.Pointsfile,
                               "Track1.kml",
                               "cc69deb3")

# This file can be found within the tempdir() directory on your computer. 
# Double-click on the .kml file to open in Google Earth
}
}

\keyword{behaviour}
\keyword{spatial}