DiffDiff Documentation

DiffDiff is a specilist 'diffing' tool for APSIM OUT files.  It is designed to diff two given files while allowing for 
slight errors between them.  It may also be pointed to different folders at which point it will attempt to match *.out 
files from each folder by comparing their file names characters-by-character before running a diff for each pair it 
finds.  The amount of error allowed between 2 files is called 'tolerance' and is specified either by the command line
or in the GUI.  Command line arguments are given below, to launch the GUI simply double-click the executable.

NOTE folder compare currently does not work properly from the GUI, this is only available in command-line mode

author is Ben Jolly - ben.jolly@agresearch.co.nz - please feel free to contact me with bug reports/feature requests


DiffDiff Command-Line Usage:
diffdiff.exe <input1> <input2> <additional args>

    where 'input1' and 'input2' may point to individual files or folders

    all arguments are optional, but the following are required if you DO NOT
    want a gui to appear:
        <input1>
        <input2>
        -t:*tol*
    
    if any of these are missing (or if '-gui' is specified) then the GUI will
    open and you may then run diffs from there.

    the default method of reporting from the command line is to create a txt
    file for each failed output file containing information on what failed
    (tab delimited).  This has the same name as the output with '.txt' appended

    valid arguments are:
        -console    redirect output to console instead of log files
        -f:*filter* if directories are used as inputs then a filter may be
                    specified here for getting certain files
                    defaults to '*.out' if not specified
        -gui        force the GUI to open
        -help       display this message
        -o:*out*    where *out* can be replaced with either a single file or a
                    directory.  

                        If file then all output is dumped into given file

                        If directory (MUST EXIST) then logs are created for 
                        each FAILED diff inside given dir
        -t:*tol*    set an acceptable tolerance for differences in output files
                    where *tol* is replaced with:
                    
                        *any double* to set exclusive maximum difference
                            i.e. -t:0.01

                        *any int*dp to use a given number of dec. places as
                            the tolerance. -ve ints specify number of digits at
                            end of value to ignore (if -1dp then 0.026 -> 0.02)
                            i.e. -t:2dp     -t:-1dp

                        *any double*pct to use a %age tolerance (new value must
                            be within 'x'% of original value)
                            i.e. -t:10pct
        -vsdebug    used when debugging from Visual Studio because it adds an
                    extra cmd line arg by default.  This tells diffdiff to
                    ignore the first argument

DiffDiff returns a count of the number of failed tests ('0' if successful)

Example Useage:
diffdiff C:\prjA\tests\orig C:\prjA\tests\new -t:0.01 -o:C:\prjA\diffs.txt