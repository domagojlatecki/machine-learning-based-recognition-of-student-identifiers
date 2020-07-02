# Machine Learning Based Recognition of Student Identifiers
This repository contains an implementation of handwritten digit recognition system using neural network in the Scala
programming language. No external libraries have been used, except for ScalaTest and ScalaMock in tests. The implemented
system supports segmentation and classification of handwritten numbers for fixed number of digits which can be specified
as a program argument.

## Importing and building the project
The project is divided into several smaller modules, which are managed using the [Mill](https://github.com/lihaoyi/mill)
build tool. Included bash script named `mill` can be used to automatically download Mill version specified in
`.mill-version` file. Once downloaded, the same script can be used to execute the downloaded version of the tool. It is
recommended to use this script instead of the tool directly since the script will always automatically use the version
specified in `.mill-version`.  

To generate project structure which can be imported into IntelliJ IDEA, run the following command: `./mill init`  
All modules can be compiled by running: `./mill compile`  
Tests can be executed with command: `./mill test`  

To build a JAR achieve for the main application, execute command: `./mill recognition.assembly` after which the JAR file
will be available in `out/recognition/assembly/dest/out.jar`. If `OPT_BUILD` environment variable is set, the JAR
achieve will be built using all the available compile-time optimizations offered by the Scala compiler. Alternatively,
if JAR achieve is not needed, script `opt.sh` can be used to compile the code using optimizations.  

For rest of the available commands, check out Mill documentation available [here](http://www.lihaoyi.com/mill/) and have
a look at the `build.sc` file.

## Dataset
The collected dataset used to train neural networks to recognize handwritten digits is available
[here](https://github.com/domagojlatecki/handwritten-number-dataset). The dataset contains around a thousand handwritten
10-digit numbers which have been divided into test and train sets.
