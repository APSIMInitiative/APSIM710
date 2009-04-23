

The following was copied from a webpage, http://www.atalasoft.com/kb/Article.aspx?id=10103

nb. In order to solve ALL the licencing problems we also had to add a reference to TeeChart 
to the ApsimUI project even though TeeChart is not used by the ApsimUI project. 



Atalasoft Knowledgebase
Q10103 - INFO: What is the licenses.licx file?

The licenses.licx file is a text file located in a .NET project that notifies Visual Studio to 
compile a license file into an executable's resource.  This enables EXE applications to be 
deployed to a machine without an SDK installed.  Internally, Visual Studio uses lc.exe to 
link the license files to the resource based on the list of assemblies and types in the 
licenses.licx file.  Visual Studio will hide this file from the solution unless you choose to 
"Show All Files" in the solution explorer.  The licenses.licx should be an embedded resource.

Visual Studio will generate the licenses.licx file for you when dropping a component or 
control onto a form.  However for Console Application, classes that are not a component, 
or for manually generating the licenses.licx, this section might be userful.  Here is an 
example of a line in the licenses.licx file that Visual Studio creates automatically:

Atalasoft.Imaging.WinControls.WorkspaceViewer, 
Atalasoft.dotImage.WinControls, Version=2.1.1962.28178, 
Culture=neutral, PublicKeyToken=2b02b46f7326f73b

Visual Studio generates the Type, Assembly, Version, Culture, and 
PublicKeyToken automatically, but only the Type and Assembly is required.  
For example, if you are licensing the JPEG2000 Codec, and DotImage in a 
console application, this is what the licenses.licx file should look like:

Atalasoft.Imaging.AtalaImage, Atalasoft.dotImage
Atalasoft.Imaging.Codec.Jpeg2000.Jp2Decoder, Atalasoft.dotImage.Jpeg2000

Please note that licenses can only be embedded in EXE's.  For DLL's and 
ASP.NET applications, the licenses.licx file will be ignored when compiling.

If a license file cannot be found, there is an assembly version mismatch, a 
signature fails in the license file, or any other licensing problem, you will 
probably get an error message when compiling like this:

Could not transform licenses file 'licenses.licx' into a binary resource. (1) : 
error LC0004 : Exception occured creating type 
'Atalasoft.Imaging.AtalasoftLicenseException'

By removing the licenses.licx file (or temporarily renaming it) your project will 
compile and you will see a more relavent error message when running the application.



