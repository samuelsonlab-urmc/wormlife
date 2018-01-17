#README
##WormLife V0.4

Copyright (C) 2017  Jesse Llop (original code), Adam Cornwell (maintainer).
Samuelson Lab, University of Rochester Medical Center, Dept. Biomedical Genetics.

**License:** GNU GPL3. See LICENSE.txt for additional details.


**Software website/repository:** https://github.com/samuelsonlab-urmc/wormlife
**Lab website:** https://www.urmc.rochester.edu/labs/andrew-samuelson.aspx

###Relevant publications:
Coming soon!

###Overview:
WormLife implements functionality and tools for analysis of Replica-Set-style survival experiments in C elegans. Kaplan-Meier style analyses are also supported. Current functionality (first public release, V0.4) includes the ability to plot survival curves through a GUI for both Replica Set Method experiments and "traditional" Kaplan-Meier experiments. Functionality is also available, through scripting in R, for statistical comparisons of groups from Replica Set data.
Future plans for this software include development of a web-based interface, to provide an easier option to get started with.


###WormLife Setup/Installation:

1) Start by downloading the release zip file for the current release at https://github.com/samuelsonlab-urmc/wormlife. The most up-to-date documentation for the software can be found at the GitHub page. The following installation instructions will work for systems running Mac OS/Mac OS X and Linux. Other platforms may be suitable, but have not been extensively tested. Start by extracting the WormLife zip file to a folder.

2) WormLife is written in R, with a graphical user interface (GUI) that uses the TK window system, which requires an X Windows client.

3) Download and install R 3.3.3 from https://r-project.org. For Mac OS, the recommended file can be found at https://cloud.r-project.org/bin/macosx/R-3.3.3.pkg. Newer versions of R may not work with some of the necessary packages.

4) For OS X, installation of the Apple developer tool package XCode is necessary. Launch terminal. Go -> Utilities -> Terminal or Applications -> Utilities -> Terminal. Execute command “xcode-select –install”, which will prompt to install software if XCode is not already installed, or inform that XCode is already installed if it already present.

5) For OS X, install XQuartz 2.7.9 (newer versions may not work). This version can be found at https://www.xquartz.org/releases/XQuartz-2.7.9.html. After installing XQuartz for the first time, log out and log back in, or restart the computer. Note that the installer may not prompt to do this, but it is recommended.

6) Install the R package tkrplot. Open R, which should now be installed. A graphical interface should load, with a large console area. At the bottom of this console area, input “install.packages("tkrplot")” to install the tkrplot package. After the install process is finished, test loading by trying the command “library(tkrplot)” in the console. If successful, no error will be returned.


###Starting WormLife: 

1) If tkrplot was able to load successfully, the WormLife plotting interface should be able to load. Find the location of the “code” directory in the folder extracted from the zip file. For example, if the file was extracted on the Desktop, the path might be “/Users/UserName /Desktop/WormLife/code”.

2) Enter the following commands in the R console, substituting for the folder path just identified. Press enter or return after each entered line.
```r
setwd(“/Users/UserName/Desktop/WormLife/code”)
source(“plotGUI.R”)
openMain()
```

3) If all prerequisite software and packages were installed, the WormLife WormPlot interface should load in a new window, bringing up the default screen as shown in Figure 3A. Note that R must remain running in the background. In OS X, if used with two displays, there may be an issue with the GUI window being placed off-screen- temporarily detaching the second display resolves this.

