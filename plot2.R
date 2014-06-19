# check for data directory and create if necessary
if (!file.exists("data")) {
        dir.create("data")
}

# check for data file else download and extract archive to data directory
dataFile1 = "data/Source_Classification_Code.rds"
dataFile2 = "data/summarySCC_PM25.rds"
if (!file.exists(dataFile1) || !file.exists(dataFile2)) {
        url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip";
        download.file(url, destfile="data/data.zip", method="curl")
        unzip("data/data.zip", exdir="data")
}

# read data
src <- readRDS(dataFile1)
emi <- readRDS(dataFile2)
# ensure we are only dealing with PM25-PRI emissions data
emi <- emi[emi$Pollutant=='PM25-PRI',]

# calculate total emissions by year in Baltimore
totalBaltEmi <- aggregate(Emissions~year, 
                          emi[emi$fips=="24510",], 
                          na.rm=TRUE, 
                          FUN=sum)

# open image for writing
png("plot2.png", width=500, height=500)

plot(totalBaltEmi$year, 
     totalBaltEmi$Emissions, 
     type="n",
     xlab="Year",
     ylab="Emissions (Tons)",
     xaxt="n",
     main="Total Emissions in Baltimore City") 
lines(totalBaltEmi$year, totalBaltEmi$Emissions, type="l")

# add custom x-axis scale
x <- seq(1999, 2008, by=3)
axis(1, at=x, labels=x)

# close graphics device
dev.off()