using System;
using System.Collections.Generic;
using System.IO;
using System.Net;
using System.Threading;
using System.Linq;
using System.ComponentModel;
using System.Diagnostics;
using System.Xml.Serialization;

using HttpServer;
using HttpListener = HttpServer.HttpListener;

using ICSharpCode.SharpZipLib.Zip;

#region MyRegion
#if false
How this APSIM Dropbox thing works

Dropbox (http://www.dropbox.com/) allows users to share files on the internet. We can use Dropbox to
transfer files between our local workstations and the APSRU cluster. To enable the service, we need
to share a folder from our workstation with the cluster by following these steps:

1. Create a directory in your Dropbox on your workstation. Use a name that you & others can recognise as yours.
2. Right click on it, select the "Dropbox" menu and "Share this Folder".
3. Share it with "peter.devoil@deedi.qld.gov.au" (whose name shows up as "Apsim JV")
4. Open a remote desktop to odin, find the dropbox icon in the system tray and "Launch dropbox website". This
   will be logged into dropbox as peter.devoil@deedi.qld.gov.au.
5. There will (maybe after a while) be a red notification in the sharing menu on this website; your
   invitation from step 3. "Accept" it.
6. Should you be curious, this same area can be seen as a disk share "\\odin\ExternalDropbox\My Dropbox".

Jobs (ie zipfiles created by ApsimUI's "send to cluster" button) that you put in your dropbox share will
be eventually run by the cluster. They'll have a ".out.zip" extension after being run. The same webpages
on http://odin/condor can be used to query the condor pool.

So in summary, there are two forms of jobs on the apsru cluster:
- old style ".sub last" jobs dropped into \\odin\CondorPostBox
- new style zipfiles

These zipfiles can be sent to the cluster
- by a dropbox as detailed above
- in the disk share "\\odin\ExternalDropbox\My Dropbox" linked to your dropbox (usually only needed
  for verification purposes)
- in a "non-dropbox"ed share "\\odin\CondorDropbox"
#endif
#endregion

namespace ApsimDropBox
    {
    class Program
        {
        static List<IDownLoader> downLoaders;

        static void Main(string[] args)
            {
            // Read in jobs we have submitted before 
            downLoaders = readInDownLoaders();

            // Fire up a web listener to report if asked
            HttpListener listener = HttpListener.Create(IPAddress.Any, 4020);
            listener.RequestReceived += OnHttpRequest;
            listener.Start(5);

            int errorCount = 0;
            while (errorCount < 10)
                {
                // Look for and run new jobs as found
                try
                    {
                    foreach (IDownLoader d in downLoaders)
                        if (d.scan())
                            saveDownLoaders();
                    errorCount = 0;
                    }
                catch (Exception e)
                    {
                        Console.WriteLine(e.ToString());
                        errorCount++;
                    }
                System.Threading.Thread.Sleep(10000);
                }
                Console.WriteLine("Too many continuous errors - exiting.");
            }

        static public void saveDownLoaders()
            {
            XmlSerializer serializer = new XmlSerializer(typeof(List<IDownLoader>));
            TextWriter textWriter = new StreamWriter("JobMonitor.xml");
            serializer.Serialize(textWriter, downLoaders);
            textWriter.Close();
            }

        static private List<IDownLoader> readInDownLoaders()
            {
            List<IDownLoader> downLoaders = new List<IDownLoader>();
            string filename = "JobMonitor.xml";
            if (File.Exists(filename))
                {
                XmlSerializer serializer = new XmlSerializer(typeof(List<IDownLoader>));
                TextReader textReader = new StreamReader(filename);
                downLoaders = (List<IDownLoader>)serializer.Deserialize(textReader);
                textReader.Close();
                }
            else
                {
                //downLoaders.Add(new FSDownLoader(Directory.GetCurrentDirectory() + "/diskshare"));
                //downLoaders.Add(new ZipDownLoader("d:/CondorDropBox", "d:/temp/"));
                downLoaders.Add(new FSDownLoader("d:\\CondorPostbox"));
                downLoaders.Add(new ZipDownLoader("d:\\ExternalDropBox\\My Dropbox", "d:\\Transfer"));
                downLoaders.Add(new ZipDownLoader("d:\\CondorDropBox", "d:\\Transfer"));
                }
            return downLoaders;
            }

        // Write some rough html to tell the world what we are doing
        static private void OnHttpRequest(object sender, RequestEventArgs e)
            {
            IHttpClientContext context = (IHttpClientContext)sender;
            IHttpRequest request = e.Request;
            IHttpResponse response = request.CreateResponse(context);
            StreamWriter writer = new StreamWriter(response.Body);
            writer.WriteLine("<pre>");
            writer.WriteLine("Monitored Jobs:");
            foreach (IDownLoader d in downLoaders)
                foreach (string jobName in d.jobList()) 
                  writer.WriteLine(jobName );
                
            writer.WriteLine("</pre>");
            writer.Flush();
            response.Send();
            }
        } // Program


    public class Zip
        {
        /// <summary>
        /// Zip all the specified files into the specified ZipFileName.
        /// </summary>
        public static void ZipUpFiles(List<string> FilesToZip, string filename, string Password)
            {
            ZipOutputStream Zip = new ZipOutputStream(File.Create(filename));
            if (Password != "")
                Zip.Password = Password;
            try
                {
                Zip.SetLevel(5); // 0 - store only to 9 - means best compression
                foreach (string FileName in FilesToZip)
                    {
                    FileStream fs = File.OpenRead(FileName);

                    byte[] Buffer = new byte[fs.Length];
                    fs.Read(Buffer, 0, Buffer.Length);
                    fs.Close();

                    ZipEntry Entry = new ZipEntry(Path.GetFileName(FileName));
                    Zip.PutNextEntry(Entry);
                    Zip.Write(Buffer, 0, Buffer.Length);
                    }
                }
            catch (System.Exception e)
                {
                Console.WriteLine("Error creating zipfile " + filename + "\n" + e.ToString());
                }
            finally 
                {
                Zip.Finish();
                Zip.Close();
                }
            }
        public static void UnZipFiles(string zipFileName, string DestFolder, string Password)
            {
            // ----------------------------------------
            // Unzip an archive to the specified folder
            // ----------------------------------------
            ZipInputStream Zip = new ZipInputStream(File.OpenRead(zipFileName));
            Zip.Password = Password;
            try
                {
                ZipEntry Entry;
                while ((Entry = Zip.GetNextEntry()) != null)
                {
                    if (Entry.IsDirectory)
                    {
                        string DestDirName = Path.Combine(DestFolder, Path.GetFileName(Entry.Name));
                        if (!Directory.Exists(DestDirName)) { Directory.CreateDirectory(DestDirName); }
                        Console.WriteLine("Creating directory " + DestDirName);
                    }
                    else if (Entry.IsFile)
                    {
                        string DestFileName = Path.Combine(DestFolder, Path.GetFileName(Entry.Name));
                        Console.WriteLine("Creating file " + DestFileName);
                        using (BinaryWriter FileOut = new BinaryWriter(new FileStream(DestFileName, FileMode.CreateNew)))
                        {
                            byte[] buffer = new byte[32768];
                            int size;
                            while ((size = Zip.Read(buffer, 0, buffer.Length)) > 0)
                                FileOut.Write(buffer, 0, size);
                            FileOut.Close();
                        }
                    }
                }

                }
            catch (System.Exception e)
                {
                Console.WriteLine("Error unpacking zipfile " + zipFileName + "\n" + e.ToString());
                }
            finally { Zip.Close(); }
            }
        }

    // Interface class for old & new style jobs
    public abstract class IJob
        {
        abstract public void run();           // Submit the job to condor
        abstract public void reset();         // Reset everything bar the name
        abstract public void checkState();    // Run (once) and test for completion after
        abstract public bool hasFinishedRunning();  // See if the job has terminated
        abstract public bool hasGone();             // See if the job is still present on disk
        abstract public string workingDirectory();  // Return the local working directory of this job
        abstract public string getName();           // Return the name of this job
        }

    // Interface class for "downloaders": scan a disk or network area for new jobs to be run.
    [System.Xml.Serialization.XmlInclude(typeof(oldStyleCondorApsimJob))]
    [System.Xml.Serialization.XmlInclude(typeof(FSDownLoader))]
    [System.Xml.Serialization.XmlInclude(typeof(ZipDownLoader))]
    public abstract class IDownLoader
        {
        abstract public bool scan();
        abstract public List<string> jobList();
        }

    // FS: File System. Scans a file system (usually an area shared as a network drive) for new jobs.
    public class FSItem
        {
        public IJob job;
        public DateTime timestamp { get; set; }
        public FSItem() { }
        public FSItem(string _name, DateTime _timestamp, IJob _job) { job = _job; timestamp = _timestamp; }
        }

    public class FSDownLoader : IDownLoader
        {
        public string fsroot;
        public List<FSItem> knownItems = new List<FSItem>();

        public FSDownLoader() { }
        public FSDownLoader(string dir) { fsroot = dir; }
        public override bool scan()
            {
            List<FSItem> newItems = scan(knownItems, fsroot);
            knownItems.AddRange(newItems);
            foreach (FSItem x in knownItems) { x.job.checkState(); }
            return (newItems.Count > 0 || prune());
            }

        private bool checkForExistingJob(string candidate)
            {
            DateTime newTimeStamp = File.GetLastWriteTime(candidate);
            foreach (FSItem x in knownItems)
                {
                if (x.job.getName() == candidate)
                    {
                    if (x.timestamp < newTimeStamp)
                        {
                        Console.WriteLine("Resubmitting FS job " + candidate);
                        x.job.reset();
                        x.timestamp = newTimeStamp;
                        }
                    return true;
                    }
                }
            return false;
            }
        public List<FSItem> scan(List<FSItem> current, string directory)
            {
            List<FSItem> result = new List<FSItem>();
            if (Directory.Exists(directory))
                {
                string newStyleJobName = Path.Combine(directory , "ApsimCondorJob.xml");
                if (File.Exists(newStyleJobName))
                    {
                    bool found = checkForExistingJob(newStyleJobName);

                    if (!found)
                        {
                        Console.WriteLine("Adding new style FS job " + newStyleJobName);
                        result.Add(new FSItem(newStyleJobName,
                                              File.GetLastWriteTime(newStyleJobName),
                                              new newStyleCondorApsimJob(newStyleJobName)));
                        }
                    }
                else
                    {
                    // fixme: Have seen a directory deleted here. What happened to other jobs started in the
                    // same scan cycle?
                    string [] subfiles = Directory.GetFiles(directory, "*.sub");
                    foreach (string subfile in subfiles)
                        {
                        bool found = checkForExistingJob(subfile);

                        if (!found)
                            {
                            Console.WriteLine("Adding old style FS job " + subfile);
                            result.Add(new FSItem(subfile,
                                                  File.GetLastWriteTime(subfile),
                                                  new oldStyleCondorApsimJob(subfile)));
                            }
                        }
                    }
                foreach (string ChildDirectoryName in Directory.GetDirectories(directory))
                    result.AddRange(scan(current, ChildDirectoryName));
                }
            return result;
            }
        public bool prune()
            {
            bool hasRemovedJobs = false;
            List<FSItem> current = new List<FSItem>();
            foreach (FSItem x in knownItems)
                if (File.Exists(x.job.getName())) // See if the .sub file has been deleted
                    current.Add(x);
                else
                    {
                    hasRemovedJobs = true;
                    Console.WriteLine("Removing FS job " + x.job.getName());
                    }
            knownItems = current;
            return hasRemovedJobs;
            }

        public override List<string> jobList()
            {
            List<string> current = new List<string>();
            foreach (FSItem x in knownItems)
                current.Add(x.job.getName());
            return current;
            }

        }

    // Zip jobs are the same as above, but a packed into a single zipfile. Instead of leaving the output in the same
    // place as the inputs, we monitor the job as condor runs it, and pack up the outputs when it finishes
    public class ZipItem
        {
        public IJob job;
        public string zipName;
        public ZipItem() { }
        public ZipItem(string _name, IJob _job) { job = _job; zipName = _name; }
        }

    public class ZipDownLoader : IDownLoader
        {
        // The downloader monitors a disk share for new zipfiles.
        public List<ZipItem> zipJobs = new List<ZipItem>();
        public string scanDir = "";
        public string workDir = "";
        public ZipDownLoader() { }
        public ZipDownLoader(string _scanDir, string _workDir)
            {
            scanDir = _scanDir;
            workDir = _workDir;
            }

        // See if any new jobs have appeared there
        public override bool scan()
            {
            List<ZipItem> newItems = ScanFolder(zipJobs, scanDir);
            zipJobs.AddRange(newItems);

            foreach (ZipItem x in zipJobs) { x.job.checkState(); }

            return (newItems.Count > 0 || prune());
            }

        // (Recursively) scan a folder for zipfiles we haven't seen before.
        // Each zipfile may contain a single newstyle job, or one or more .sub files.
        List<ZipItem> ScanFolder(List<ZipItem> current, string folder)
            {
            List<ZipItem> result = new List<ZipItem>();
            if (Directory.Exists(folder))
                {
                foreach (string ChildDirectoryName in Directory.GetDirectories(folder))
                    if (Path.GetFileName(ChildDirectoryName)[0] != '.')
                       result.AddRange(ScanFolder(current, ChildDirectoryName));

                foreach (string zipfile in Directory.GetFiles(folder, "*.zip"))
                    {
                    //Ignore "*.out.zip"
                    string ext2 = Path.GetExtension(Path.GetFileNameWithoutExtension(zipfile));
                    if (ext2 == ".out")
                        continue;

                    bool found = zipJobs.Find(x => x.zipName.Equals(zipfile)) != null;

                    if (!found)
                        {
                        string destination = Path.Combine(workDir , Path.GetFileNameWithoutExtension(zipfile));
                        Console.WriteLine("Found and unpacking new zipfile " + zipfile);
                        // These operations can fail as the zipfile may be locked by another process
                        try
                            {
                            // Clean out any existing files - may be a partial download that crashed previously
                            Directory.CreateDirectory(destination);
                            foreach (string file in Directory.GetFiles(destination, "*", SearchOption.AllDirectories).ToList())
                                File.Delete(file);
                            string destZipFileName = Path.Combine(destination, Path.GetFileName(zipfile));

                            File.Copy(zipfile, destZipFileName, true);

                            Zip.UnZipFiles(destZipFileName, destination, "");
                            File.Delete(destZipFileName);

                            FileInfo fInfo = new FileInfo(Path.Combine(destination , "ApsimCondorJob.xml"));
                            if (fInfo.Exists)
                                {
                                Console.WriteLine("Adding new style job from zipfile " + zipfile);
                                result.Add(new ZipItem(zipfile, new newStyleCondorApsimJob(Path.Combine(destination , "ApsimCondorJob.xml"))));
                                }
                            else
                                {
                                string[] subFiles = Directory.GetFiles(destination, "*.sub");
                                if (subFiles.Length> 1)
                                    Console.WriteLine("More than one submit file found in " + zipfile);

                                // FIXME: One day, we'll have to deal with more than 1 submit file in a zipfile. For now
                                // ignore it..
                                if (subFiles.Length > 0)
                                    {
                                    Console.WriteLine("Adding old style job from zipfile " + zipfile + " = " + subFiles[0]);
                                    result.Add(new ZipItem(zipfile, new oldStyleCondorApsimJob(subFiles[0])));
                                    }
                                }
                            }
                        catch (Exception e) { Console.WriteLine("Error unpacking and starting jobs in" + zipfile + "\n" + e.ToString()); }
                        }
                    }
                }
            return result;
            }
        public bool prune()
            // Scan for jobs that have finished. Upload (pack them into a zip) when they're finished.
            // Remove them from our list of monitored files when the zipfile has been deleted
            {
            foreach (ZipItem x in zipJobs)
                {
                if (x.job.hasFinishedRunning() && !x.job.hasGone())
                    {
                    string outZip = Path.Combine(Path.GetDirectoryName(x.zipName), (Path.GetFileNameWithoutExtension(x.zipName) + ".out.zip"));
                    upload(outZip, x.job.workingDirectory());
                    }
                }
            List<ZipItem> currentJobs = new List<ZipItem>();
            bool zipsHaveGone = false;
            foreach (ZipItem x in zipJobs)
                {
                if (File.Exists(x.zipName))
                    currentJobs.Add(x);
                else
                    {
                    zipsHaveGone = true;
                    Console.WriteLine("Zip job " + x.zipName + " has been deleted");
                    }
                }
            zipJobs = currentJobs;

            return zipsHaveGone;
            }

        public void upload(string destinationZip, string workingDir)
            {
            string wd = Directory.GetCurrentDirectory();
            Console.WriteLine("Zipping up  " +  workingDir + " to " + destinationZip);
            try
                {
                if (Directory.Exists(workingDir))
                    {
                    Directory.SetCurrentDirectory(workingDir);
                    File.Delete(destinationZip);
                    List<string>allFiles = Directory.GetFiles(".", "*", SearchOption.AllDirectories).ToList();
                    Zip.ZipUpFiles(allFiles, destinationZip, "");

                    foreach (string file in allFiles)
                        File.Delete(file);

                    Directory.SetCurrentDirectory(wd);
                    Directory.Delete(workingDir);
                    }
                }
            catch (Exception e) { Console.WriteLine("ZipUpLoader: " + e.ToString()); }
            finally
                {
                Directory.SetCurrentDirectory(wd);
                }
            }

        public override List<string> jobList()
            {
            List<string> current = new List<string>();
            foreach (ZipItem x in zipJobs)
                current.Add(x.zipName);
            return current;
            }
        }

    // Start a condor job with whatever .sub files are there
    public class oldStyleCondorApsimJob : IJob
        {
        public bool hasRun = false;
        public bool hasError = false;
        public bool hasFinished = false;
        public string submitFile = "";
        public List<string> myLogFiles = new List<string>();
        public oldStyleCondorApsimJob() { }
        public oldStyleCondorApsimJob(string filename)
            {
            submitFile = filename;
            }

        public override void reset()
            {
            hasRun = false;
            hasError = false;
            hasFinished = false;
            myLogFiles = new List<string>();
            }

        public override void checkState()
            {
            if (!hasRun) run();
            }

        public override void run()
            {
            Process p = new Process();
            p.EnableRaisingEvents = false;
            p.StartInfo.RedirectStandardInput = false;
            p.StartInfo.RedirectStandardOutput = false;
            p.StartInfo.RedirectStandardError = true;
            p.StartInfo.UseShellExecute = false;
#if (LINUX)
            p.StartInfo.FileName = "/opt/condor/bin/condor_submit";
#else
            p.StartInfo.FileName = "c:/condor/bin/condor_submit.exe";
#endif
            p.StartInfo.WorkingDirectory = Path.GetDirectoryName(submitFile);
            p.StartInfo.Arguments = submitFile;
            try
                {
                p.Start();
                p.WaitForExit();
                if (p.ExitCode != 0) { throw new ApplicationException("Condor returned a non-zero exit code"); }
                }
            catch (Exception e)
                {
                Console.WriteLine("Error submitting " + submitFile);
                hasError = true;
                try
                    {
                    StreamWriter writer = new StreamWriter(File.Open(Path.Combine(Path.GetDirectoryName(submitFile) , "CondorSubmitError.txt"), FileMode.Create));
                    writer.WriteLine("Error submitting " + submitFile);
                    writer.WriteLine(e.ToString());
                    writer.Write(p.StandardError.ReadToEnd());
                    writer.Close();
                    }
                catch (Exception) {/* Nothing - dont care*/}
                }
            if (!hasError) Console.WriteLine("Submitted " + submitFile);
            hasRun = true;
            }

        public override bool hasFinishedRunning()
            {
            if (!hasRun) { return false; }
            if (hasError) { return true; }
            if (hasFinished) { return true; }

            if (myLogFiles.Count == 0)
               scanSubmitFile();

            foreach (string f in myLogFiles)
                {
                try
                    {
                    string logFile = Path.Combine(Path.GetDirectoryName(submitFile), f);
                    int jobCount = 0;
                    string line;
                    StreamReader fp = new StreamReader(logFile);
                    while ((line = fp.ReadLine()) != null)
                        {
                        string code = line.Substring(0, 3);
                        switch (code)
                            {
                            case "000": jobCount++; break; // Waiting
                            case "001": break; // Executing
                            case "005": jobCount--; break; // Terminated
                            case "006": break; // Updating
                            case "007": jobCount--; break; // Shadow Exception
                            case "009": jobCount--; break; // Aborted
                            }
                        }
                    fp.Close();
                    if (jobCount > 0) return false;
                    }
                catch (IOException) { return false; /* Condor is writing to the file */ }
                }

            hasFinished = true;
            return true;
            }

        private void scanSubmitFile()
            // Look for any .log files in a submit file. Later we may test this log file for presence and completion
            {
            if (File.Exists(submitFile))
                {
                StreamReader fp = new StreamReader(submitFile);
                string line;
                while ((line = fp.ReadLine()) != null)
                    {
                    string[] nv = line.Split('=');
                    if (nv.Length > 1 && nv[0].Trim() == "log")
                        {
                        myLogFiles.Add(nv[1].Trim());
                        }
                    }
                fp.Close();
                }
            else
                Console.WriteLine("oldStyleCondorApsimJob: no submit file! (" + submitFile + ")");
            }

        public override bool hasGone()
            {
            return (!File.Exists(submitFile));
            }
        public override string workingDirectory()
            {
            return (Path.GetDirectoryName(submitFile));
            }
        public override string getName()
            {
            return (submitFile);
            }

        } // oldStyleCondorApsimJob

    // Start a condor job from a .xml file
    public class newStyleCondorApsimJob : IJob
        {
        //DateTime myTimeStamp;
        //string myId;

        public newStyleCondorApsimJob(string dirName)
            {
            }
        public override bool hasFinishedRunning() { return true; }

        public override void checkState()
            {
            //            if (isFinished())
            //                myUploader.upload(this);
            }

        // Submit job to collector
        public override void run()
            {
            }
        public bool isFinished()
            {
            return true;
            }
        public override bool hasGone()
            {
            return true;
            }
        public override string workingDirectory()
            {
            return (Path.GetDirectoryName("/"));
            }
        public override string getName()
            {
            return ("...");
            }
        public override void reset()
            {
            }
        }

}
