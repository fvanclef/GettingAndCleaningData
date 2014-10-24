Instruction list
----------------

1. Make sure that the data folder is placed in the root folder. The name of the folder must be **UCI HAR Dataset**

2. Run the run_analysis.R script in Rstudio. The script required two packages
   * data.table
   * reshape2

3. The output of the script is a file named tidydata.txt which is written in the root folder

Code book
---------

Many variables have not been included into the tidy data set cfr features_info.txt file for the description

Only mean and standard deviation of measures have been kept.

The tidy data set has been generated by calculating the mean of measures grouped by activity and subject. We have prefixed the each variable name by the token **mean** to reflect that.

Here are the variables list 

activity: Human Action such as *LAYING*, *SITTING*, etc
subject: Person who made the activty
meantimebodyaccelerometermeanx
meantimebodyaccelerometermeany
meantimebodyaccelerometermeanz
meantimebodyaccelerometerstandarddeviationx
meantimebodyaccelerometerstandarddeviationy
meantimebodyaccelerometerstandarddeviationz
meantgravityaccelerometermeanx
meantgravityaccelerometermeany
meantgravityaccelerometermeanz
meantgravityaccelerometerstandarddeviationx
meantgravityaccelerometerstandarddeviationy
meantgravityaccelerometerstandarddeviationz
meantimebodyaccelerometerjerkmeanx
meantimebodyaccelerometerjerkmeany
meantimebodyaccelerometerjerkmeanz
meantimebodyaccelerometerjerkstandarddeviationx
meantimebodyaccelerometerjerkstandarddeviationy
meantimebodyaccelerometerjerkstandarddeviationz
meantimebodygyroscopemeanx
meantimebodygyroscopemeany
meantimebodygyroscopemeanz
meantimebodygyroscopestandarddeviationx
meantimebodygyroscopestandarddeviationy
meantimebodygyroscopestandarddeviationz
meantimebodygyroscopejerkmeanx
meantimebodygyroscopejerkmeany
meantimebodygyroscopejerkmeanz
meantimebodygyroscopejerkstandarddeviationx
meantimebodygyroscopejerkstandarddeviationy
meantimebodygyroscopejerkstandarddeviationz
meantimebodyaccelerometermagnitudemean
meantimebodyaccelerometermagnitudestandarddeviation
meantgravityaccelerometermagnitudemean
meantgravityaccelerometermagnitudestandarddeviation
meantimebodyaccelerometerjerkmagnitudemean
meantimebodyaccelerometerjerkmagnitudestandarddeviation
meantimebodygyroscopemagnitudemean
meantimebodygyroscopemagnitudestandarddeviation
meantimebodygyroscopejerkmagnitudemean
meantimebodygyroscopejerkmagnitudestandarddeviation
meanfrequencybodyaccelerometermeanx
meanfrequencybodyaccelerometermeany
meanfrequencybodyaccelerometermeanz
meanfrequencybodyaccelerometerstandarddeviationx
meanfrequencybodyaccelerometerstandarddeviationy
meanfrequencybodyaccelerometerstandarddeviationz
meanfrequencybodyaccelerometerjerkmeanx
meanfrequencybodyaccelerometerjerkmeany
meanfrequencybodyaccelerometerjerkmeanz
meanfrequencybodyaccelerometerjerkstandarddeviationx
meanfrequencybodyaccelerometerjerkstandarddeviationy
meanfrequencybodyaccelerometerjerkstandarddeviationz
meanfrequencybodygyroscopemeanx
meanfrequencybodygyroscopemeany
meanfrequencybodygyroscopemeanz
meanfrequencybodygyroscopestandarddeviationx
meanfrequencybodygyroscopestandarddeviationy
meanfrequencybodygyroscopestandarddeviationz
meanfrequencybodyaccelerometermagnitudemean
meanfrequencybodyaccelerometermagnitudestandarddeviation
meanfrequencybodybodyaccelerometerjerkmagnitudemean
meanfrequencybodybodyaccelerometerjerkmagnitudestandarddeviation
meanfrequencybodybodygyroscopemagnitudemean
meanfrequencybodybodygyroscopemagnitudestandarddeviation
meanfrequencybodybodygyroscopejerkmagnitudemean
meanfrequencybodybodygyroscopejerkmagnitudestandarddeviation
