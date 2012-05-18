using System.IO;
using System.Collections.Generic;
using ApsimFile;

public class ToowoombaCluster
{
    public static void RunOnCluster(List<string> FilesToRun , string FinalFolder, string ApsimVersion, bool isUnix, int simsPerJob, bool NiceUser, ApsimFile.ProgressNotifier Notifier)
    {
		CondorJob c = new CondorJob(NiceUser);
        if (isUnix)
		    c.arch = Configuration.architecture.unix;
        else
            c.arch = Configuration.architecture.win32;
        c.ApsimVersion = ApsimVersion;
        if (!Directory.Exists(FinalFolder))
            Directory.CreateDirectory(FinalFolder);
        c.DestinationFolder = FinalFolder;
        c.numberSimsPerJob = simsPerJob;

        c.Go(FilesToRun, Notifier);
	}
}