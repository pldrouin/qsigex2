This repository contains the QSigEx libraries, which constitute an optimised C++ statistical data analysis framework, as well as some code examples and HTML class documentation. 
The framework consists of optimised data structures, optimised histogramming tools with support for advanced binned PDF normalisation schemes, parallel processing units and minimisation tools. For information regarding the interface of QSigEx, refer to the autogenerated HTML documentation located in the htmldoc directory.

Dependencies:<br>
-GNU sed (to generated dependency files)<br>
-GCC (tested with GCC 9.4.0)<br>
-GNU Make<br>
-[ROOT](http://root.cern.ch). Tested with ROOT 6.28.02)<br>
-[libprocinfo](https://github.com/pldrouin/libprocinfo) (optional, but recommended)<br>

To compile the library, simply call GNU make from this directory (default target). Make options are:<br>
WITH_LIBPROCINFO: Support for libprocinfo<br>
QSFAST: Compile without debugging support<br>
PROFGEN: Add compilation flags to generate compiler profiles<br>
PROFUSE: Add compilation flags to use compiler profiles (after using the libraries generated with the PROFGEN option)<br>
The clear target must be used prior to call make with different options

To generate the HTML documentation, use the htmldoc target. 
