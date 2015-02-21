---
title: "Script explanation"
author: "Srikanth"
date: "Saturday, February 21, 2015"
output: html_document
---

##README file
### How the file run_analysis.R works

The whole script file is divided into sections.
Each section has a comment at the beginning.
Below mentioned are the sections-
*Reading Column names from features table
*Reading Test data
*Adding the activity, set and respondent columns to the Test data
*Reading training data
*Adding the activity, set and respondent columns to the Training data
*Combining Test and Training data
*Filtering Merged data for columns containing Means and Standard Deviations only
*Describing activities with the Activity Labels
*Averages of each variable for each activity and each subject
*Writing to a text file

Few notes:
*A path variable has been defined to be used while reading the data; This path can be changed to your local address if you want to run the codes
*Column names have not been modified since they are quite well named with appropriate syntactical identifiers
*All six activity labels have been considered different; otherwise data wouldn't have been collected in this fashion 
*At each stage I have used the View() function in case the intermediate data needs to be seen

How it works:
The column names are read from the ytest file using the read.table command. Then the test data is read into Xtest using the same read.table command. The path is obtained from concatenating the path variable and the file names'. Respondent number is also read and stored in the subjectTest file. Then the columns are combined into one table using cbind(). In this the column 'set' is added to mention that the observations come from the test data.
The same process is repeated for the training data.
Then using an IF ELSE ladder, the activity numbers are renamed into the activity labels.
After this, the data is grouped by respondent and activity and averages of the rest of the variables are retained in the 'Averages'. This table is written into a text file using the write.table into the local which is also attached in the answer.
Any other query, kindly contact at srikanthguhan@gmail.com
Thank you.