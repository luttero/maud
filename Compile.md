Build and compile MAUD from github

S. Merkel, with the heavy assistance of L. Lutterotti
 4 July 2024

These instructions were tested on linux on July 4, 2024. You may have to adapt them if you want to work on another operating system.

Java Development environment
MAUD is written in Java. In theory, you should be able to build MAUD with plain java and ant. In reality, MAUD is a huge program and Luca uses a development environment, the community edition of IntelliJ IDEA, which you can download freely from https://www.jetbrains.com/idea/download/other.html.

Java development kit
You will need a Java Development Kit:
- One option is the official Oracle version. It is free to use, but not free to redistribute. You can use it if you intend to compile for yourself. You can get it at https://www.oracle.com/fr/java/technologies/downloads/ 
- An open-source replacement is Zulu, which you can get at https://www.azul.com/downloads/ 
At the time of writing, version 19 was sufficient, but I used the 22 version of the java development kit.

Getting ready to work on MAUD source code
The first step is to download the source code for MAUD version 2, from the main github site at https://github.com/luttero/maud. This is typically done by
	1.	Creating a folder somewhere in your architecture
	2.	Inside this folder, type git clone https://github.com/luttero/maud.git
A second option is to download a zip archive from the github repository.

Edit local parameters
Inside the MAUD source code, there is a file called ant_maud_v2.properties: 
	-	Make a copy
	-	Edit this text file and set the following parameters
	-	installerDir folder in which the MAUD installers will be saved
	-	app_osx (only if you work on a Mac)
	-	JAVA_HOME where is java?
	-	java_version, 19 at the time of writing is the minimum
	-	openjdk location of openjdk, if you want to build versions with jdk bundled
	-	build where to build MAUD
	-	fpsmBuild some special build feature, which I did not use
	-	Save this file as .ant_maud_v2.properties in your home directory
	
Set-up the MAUD project in IntelliJ Idea
Here is the procedure, there may be elements missing as we fixed things as we went
	-	Startup IntelliJ Idea and open Maud.ipr. Many things should set themselves up.
	-	Idea will complain about missing the Ant extension, add the extension (there is a way to click somewhere to get it installed)
	-	Open File -> Project structure
	-	Open the tab for Project
	-	Specify the location of the JDK that you downloaded and installed
	-	Create a folder called build_numbers in the main source code folder
	-	Create an empty file Maud_full_build.number in this build_numbers folder you just created
	
You should be ready...

Build and compile in IntelliJ Idea
All is done from the build.xml file for ant. Enable the ant view in Idea (click on the ant icon near the right border of Idea window).
If you simply want to compile, try to run the compile_open section (right click on it in the Ant UI, which you get from the little ant icon).
If you want to build MAUD, try to run the build_full section.
This will create MAUD.jar file in the folder you defined for build in .ant_maud_v2.properties.

Run your MAUD version
The easiest way is to download a recent MAUD version from the website, and replace MAUD.jar in the lib folder with the version you just built.

