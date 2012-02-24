This utility takes an Apsim .Net DLL it attempts to generate an html documentation file outlining all the DLL�s inputs, outputs, events, event handlers and <link>s.
It is a command line program that expects to be passed either a DLL or a directory containing DLL�s. For each Apsim .net DLL it finds an html file with a corresponding name is created. This html file will contain the details of all the tagged I/O properties and functions on a per class basis.
It uses reflection to do this and it looks for Apsim specific tags such as <Input>, <Output>, <Units>, <Event>, etc.
The long term plan is to be able to "browse" through the Apsim modules to get a better idea about the various module interactions, a bit like and apsim implementation of Javadoc or doxygen.

Cheers,
Scott.

TODO:
- be able to handle old / non .net dlls
- create links between html file when reading multiple DLLs

Usage: DLLDocumenter.exe [dll|directory]
  Apsim module documention creator
  This will create html files with the format [assembly name].html in the current directory