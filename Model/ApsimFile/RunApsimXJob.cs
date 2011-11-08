using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;
using System.IO;
using ApsimFile;

public class RunApsimXJob : RunApsimJob
{
    private string _ApsimFileName;
    private string _SimulationPath;

    public RunApsimXJob(string ApsimFileName, string SimulationPath, JobRunner JobRunner)
        : base(SimulationPath + " (" + Path.GetFileName(ApsimFileName) + ")", JobRunner)
    {
        _ApsimFileName = ApsimFileName;
        _SimulationPath = SimulationPath;
        _Executable = Configuration.RemoveMacros(Path.Combine("%apsim%", "Model", "ApsimX.exe"));
        _Arguments = _ApsimFileName;

        _SumFileName = Path.Combine(Path.GetDirectoryName(ApsimFileName),
                        Path.GetFileNameWithoutExtension(SimulationPath) + ".sum");
        _SumFile = new StreamWriter(_SumFileName);
    }
}

