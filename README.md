This repository contains the QSigEx libraries, which constitute an optimised C++ statistical data analysis framework, as well as some code examples and HTML class documentation. 
The framework consists of optimised data structures, optimised histogramming tools with support for advanced binned PDF normalisation schemes, parallel processing units and minimisation tools. For information regarding the interface of QSigEx, refer to the autogenerated HTML documentation located in the htmldoc directory.

Dependencies:
-GNU sed (to generated dependency files)
-GCC (tested with GCC 9.4.0)
-GNU Make
-[ROOT](http://root.cern.ch). Tested with ROOT 6.28.02)
-libprocinfo (optional, but recommended)

To compile the library, simply call GNU make from this directory (default target). Make options are:
WITH_LIBPROCINFO: Support for libprocinfo
QSFAST: Compile without debugging support
PROFGEN: Add compilation flags to generate compiler profiles
PROFUSE: Add compilation flags to use compiler profiles (after using the libraries generated with the PROFGEN option)
The clear target must be used prior to call make with different options

To generate the HTML documentation, use the htmldoc target. 
