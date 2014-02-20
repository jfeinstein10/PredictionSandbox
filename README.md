# PredictionSandbox
This is an SBT project to play with trend detection for later integration into Prediction.IO

## Setup
1. Install [IntelliJ](www.jetbrains.com/idea/).
2. Install the Scala and SBT plugins ([here's how to install plugins](http://www.jetbrains.com/idea/webhelp/installing-updating-and-uninstalling-repository-plugins.html)).
3. Import the PredictionSandbox folder as an SBT project.
4. Either let IntelliJ find, download, and link your dependencies or do it manually yourself using SBT.

## Running Scalding Jobs
You can run Scalding jobs in 1 of 2 ways. Either from the SBT Console or using IntelliJ's Run Configuration.

#### SBT Console
1. Open up the SBT Console (View > Tool Windows > SBT Console).
2. Start it with the little green arrow.
3. To run a job in the console type ```run-main com.twitter.scalding.Tool job.class.name --hdfs --input1 input1value --input2 input2value``` where ```job.class.name``` is the name of the job you want to run and the ```--input```'s are your input values.

#### Run Configuration
1. Follow the instructions [here](https://github.com/twitter/scalding/wiki/Run-in-Intellij-IDEA).
