## check for data directory and create if necessary
if (!file.exists("data")) {
        dir.create("data")
}
## check for data file, if not found download and extract archive to data
## directory
dataFile1 = "data/Source_Classification_Code.rds"
dataFile2 = "data/summarySCC_PM25.rds"
if (!file.exists(dataFile1) || !file.exists(dataFile2)) {
        fileUrl = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip";
        download.file(fileUrl, destfile="data/data.zip", method="curl")
        unzip("data/data.zip", exdir="data")
}

# read data
sources <- readRDS(dataFile1)
emissions <- readRDS(dataFile2)
# ensure we are only dealing with PM25-PRI emissions data
emissions <- emissions[emissions$Pollutant=='PM25-PRI',]

# question 1 - have total emissions decreased?
totalEmissions <- aggregate(Emissions~year, 
                            emissions, 
                            na.rm=TRUE, 
                            FUN=sum)

plot(totalEmissions$year, 
     totalEmissions$Emissions, 
     type="n",
     xlab="Year",
     ylab="Emissions (x100,000 Tons)",
     yaxt="n",
     main="Total Emissions From All Sources") 

## add line
lines(totalEmissions$year, totalEmissions$Emissions, type="l")

## add custom y-axis scale
yLabels <- data.frame(v=totalEmissions[,"Emissions"], 
                      l=floor(totalEmissions[,"Emissions"]/100000))
axis(2, at=yLabels[c(1,3,4),]$v, labels=yLabels[c(1,3,4),]$l)

# question 2 - have total emissions decreased in Baltimore City?
totalBaltimoreEmissions <- aggregate(Emissions~year, 
                            emissions[emissions$fips=="24510",], 
                            na.rm=TRUE, 
                            FUN=sum)

plot(totalBaltimoreEmissions$year, 
     totalBaltimoreEmissions$Emissions, 
     type="n",
     xlab="Year",
     ylab="Emissions (Tons)",
     main="Total Emissions in Baltimore City") 

## add line
lines(totalBaltimoreEmissions$year, totalBaltimoreEmissions$Emissions, type="l")

cPalette <- c("#ff0000",
              "#00ff00",
              "#0000ff",
              "#ff00ff",
              "#ff8000",
              "#33ffff",
              "#aacc00")

# question 3 - emissions by type
library(ggplot2)
plot3 <- (ggplot(emissions[emissions$fips=="24510",], aes(x=year, y=Emissions)) 
          + geom_point(aes(color=type), size=8, alpha=0.2, shape=18) 
          + facet_wrap( ~ type, ncol=2)
          + geom_smooth(size=1, col="black", linetype=1, method="lm", se=FALSE)
          + labs(title="Emissions by Type in Baltimore City", 
                 x="Year",
                 y="Emissions (Tons)")
          + theme_bw()
          + theme(legend.position="none")
          + scale_colour_manual(values=cPalette)
          + scale_x_continuous(breaks=c(seq(1999, 2008, by=3))))

# question 4 - change in coal combustion-related emissions
# determine sources we are interested in
coalEmissions <- emissions[emissions$SCC %in% sources[grep("coal", sources$EI.Sector, ignore.case=TRUE), 1], ]
sourceNames <- sources[, c(1, 4)]
coalEmissions <- merge(coalEmissions, sourceNames, by.x = "SCC", by.y = "SCC")
facetLabeller <- function(var,value){
        value <- as.character(value)
        if (var=="EI.Sector") { 
                value[value=="Fuel Comb - Electric Generation - Coal"] <- "Electric Generation"
                value[value=="Fuel Comb - Comm/Institutional - Coal"]   <- "Comm./Institutional"
                value[value=="Fuel Comb - Industrial Boilers, ICEs - Coal"]   <- "Industrial Boilers, ICEs"
        }
        return(value)
}

plot4 <- (ggplot(coalEmissions, aes(x=year, y=Emissions))
          + geom_point(aes(color=EI.Sector), size=8, alpha=0.2, shape=18) 
          + facet_grid(.~EI.Sector, labeller=facetLabeller) 
          + geom_smooth(size=1, col="black", linetype=1, method="lm")
          + labs(title="Emissions from Coal-Combustion Sources",
                 x="Year",
                 y="Emissions (Tons)")
          + theme_bw()
          + theme(legend.position="none")
          + scale_x_continuous(breaks=c(seq(1999, 2008, by=3))))

# question 5 - change in vehicle emissions in Baltimore
# I'm using the dictionary definition of "motor vehicle" to include any
# "self-propelled wheeled conveyance, such as a car or truck, that does not run 
# on rails" and including both on-road and off-road vehicles

require(lattice)
require(gridExtra)

vehicleEmissions <- emissions[emissions$fips=="24510" & emissions$SCC %in% sources[grep("-road", sources$EI.Sector, ignore.case=TRUE), 1], ]
sourceNames <- sources[, c(1, 4)]
vehicleEmissions <- merge(vehicleEmissions, sourceNames, by.x = "SCC", by.y = "SCC")
facetLabeller <- function(var,value){
        value <- as.character(value)
        if (var=="EI.Sector") { 
                value[value=="Mobile - Non-Road Equipment - Diesel"] <- "Diesel"
                value[value=="Mobile - Non-Road Equipment - Gasoline"]   <- "Gasoline"
                value[value=="Mobile - Non-Road Equipment - Other"]   <- "Other"
                value[value=="Mobile - On-Road Diesel Heavy Duty Vehicles"] <- "Diesel Heavy Duty"
                value[value=="Mobile - On-Road Diesel Light Duty Vehicles"]   <- "Diesel Light Duty"
                value[value=="Mobile - On-Road Gasoline Heavy Duty Vehicles"] <- "Gasoline Heavy Duty"
                value[value=="Mobile - On-Road Gasoline Light Duty Vehicles"]   <- "Gasoline Light Duty"
        }
        return(value)
}
cPalette <- c("#ff0000",
              "#00ff00",
              "#0000ff",
              "#ff00ff",
              "#ff8000",
              "#33ffff",
              "#aacc00")
onRoad <- (ggplot(vehicleEmissions[vehicleEmissions$type=="ON-ROAD",], 
                  aes(x=year, y=Emissions))
          + geom_point(aes(color=EI.Sector), size=8, alpha=0.2, shape=18) 
          + facet_grid(.~EI.Sector, labeller=facetLabeller) 
          + geom_smooth(size=1, col="black", linetype=1, method="lm")
          + labs(title="On-Road Vehicles", xlab="", ylab="")
          + theme_bw()
          + theme(legend.position="none", 
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank())
          + scale_colour_manual(values=cPalette[1:4])
          + scale_x_continuous(breaks=c(seq(1999, 2008, by=3))))

nonRoad <- (ggplot(vehicleEmissions[vehicleEmissions$type=="NON-ROAD",], 
                   aes(x=year, y=Emissions))
           + geom_point(aes(color=EI.Sector), size=8, alpha=0.2, shape=18) 
           + facet_grid(.~EI.Sector, labeller=facetLabeller) 
           + geom_smooth(size=1, col="black", linetype=1, method="lm")
           + labs(title="Non-Road Vehicles & Equipment", xlab="", ylab="")
           + theme_bw()
           + theme(legend.position="none", 
                   axis.title.x=element_blank(),
                   axis.title.y=element_blank())
           + scale_colour_manual(values=cPalette[5:7])
           + scale_x_continuous(breaks=c(seq(1999, 2008, by=3))))

grid.arrange(onRoad, 
             nonRoad, 
             nrow=2, 
             left="Emissions (Tons)",
             sub="Year",
             main=textGrob("Motor Vehicle Emissions in Baltimore City"))

# question 6 - compare vehicle emissions in Baltimore with Los Angeles
baltimoreEmissions <- emissions[emissions$fips=="24510" & emissions$SCC %in% sources[grep("-road", sources$EI.Sector, ignore.case=TRUE), 1], ]
baltimoreEmissions$cityName <- "Baltimore City"
laEmissions <- emissions[emissions$fips=="06037" & emissions$SCC %in% sources[grep("-road", sources$EI.Sector, ignore.case=TRUE), 1], ]
laEmissions$cityName <- "Los Angeles County"
vehicleEmissions <- rbind(baltimoreEmissions, laEmissions)
sourceNames <- sources[, c(1, 4)]
vehicleEmissions <- merge(vehicleEmissions, sourceNames, by.x = "SCC", by.y = "SCC")

cPalette <- c("#ff0000",
              "#00ff00",
              "#0000ff",
              "#ff00ff",
              "#ff8000",
              "#33ffff",
              "#aacc00")

facetLabeller <- function(var,value){
        value <- as.character(value)
        if (var=="EI.Sector") { 
                value[value=="Mobile - Non-Road Equipment - Diesel"] <- "Non-Road Diesel"
                value[value=="Mobile - Non-Road Equipment - Gasoline"]   <- "Non-Road Gasoline"
                value[value=="Mobile - Non-Road Equipment - Other"]   <- "Non-Road Other"
                value[value=="Mobile - On-Road Diesel Heavy Duty Vehicles"] <- "On-Road Diesel Heavy Duty"
                value[value=="Mobile - On-Road Diesel Light Duty Vehicles"]   <- "On-Road Diesel Light Duty"
                value[value=="Mobile - On-Road Gasoline Heavy Duty Vehicles"] <- "On-Road Gasoline Heavy Duty"
                value[value=="Mobile - On-Road Gasoline Light Duty Vehicles"]   <- "On-Road Gasoline Light Duty"
        }
        return(value)
}

totalEmissions <- aggregate(Emissions~year+cityName, 
                            vehicleEmissions, 
                            na.rm=TRUE, 
                            FUN=sum)

byType <- (ggplot(vehicleEmissions, 
                  aes(x=year, y=Emissions))
           + geom_point(aes(color=EI.Sector), size=8, alpha=0.2, shape=18) 
           + facet_grid(cityName~EI.Sector, labeller=facetLabeller) 
           + geom_smooth(size=1, col="black", linetype=1, method="lm")
           + labs(title="Emissions by Motor Vehicle Type", 
                  xlab="Year", 
                  ylab="Emissions (Tons)")
           + theme_bw()
           + theme(legend.position="none")
           + scale_colour_manual(values=cPalette)
           + scale_x_continuous(breaks=c(seq(1999, 2008, by=3))))

totals <- (ggplot(totalEmissions, 
                   aes(x=year, y=Emissions, fill=cityName))
           + geom_bar(stat="identity", position=position_dodge()) 
           + labs(title="Total Motor Vehicle Emissions", 
                   xlab="Year", 
                   ylab="Emissions (Tons)")
           + theme_bw()
           + scale_fill_manual(values=c("#ff0000", "#0000ff"))
           + scale_x_continuous(breaks=c(seq(1999, 2008, by=3))))


grid.arrange(byType, 
             totals, 
             nrow=2, 
             main=textGrob("Comparison of Motor Vehicle Emissions in Baltimore City and Los Angeles"))