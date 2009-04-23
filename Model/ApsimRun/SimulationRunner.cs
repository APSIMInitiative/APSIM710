using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Diagnostics;
using CSGeneral;
using System.Windows.Forms;
using System.ComponentModel;
using System.Threading;
using ApsimFile;
using System.Runtime.InteropServices;

namespace ApsimRun
   {
   public class Detail
         {
      public string Name;
      public string FileName;
      public bool HasErrors = false;
      public bool HasWarnings = false;
      public bool IsCompleted = false;
      public string SummaryFileName;
      }

   /// <summary> 
   /// An internal class for encapsulating a single run of APSIM
   /// It contains the external Runnable object, plus the summary
   /// file being written to. 
   /// </summary>
   class SingleRun
      {
      public Runnable SimulationFile;
      public StreamWriter SummaryFile;
      public Detail Details = new Detail();
      public int PercentComplete = 0;
      public SingleRun(Runnable SimulationFile, string Name)
         {
         this.SimulationFile = SimulationFile;
         Details.Name = Name;
         Details.FileName = SimulationFile.FileName;
         }
      public string SimFileName;
      internal string PrepareToRun()
         {
         string Messages;
         SimulationFile.WriteSimFile(Details.Name, out SimFileName, out Messages);

         Details.SummaryFileName = SimFileName.Replace(".sim", ".sum");
         if (File.Exists(Details.SummaryFileName))
            File.Delete(Details.SummaryFileName);
         SummaryFile = new StreamWriter(Details.SummaryFileName);

         return Messages;
         }
      internal void IsCompleted()
         {
         Details.IsCompleted = true;
         PercentComplete = 100;
         if (SimulationFile.DeleteSimOnceRunCompleted && SimFileName != null && File.Exists(SimFileName))
            File.Delete(SimFileName);
         }
      internal void Close()
         {
         if (SimulationFile.DeleteSimOnceRunCompleted && File.Exists(SimFileName))
            File.Delete(SimFileName);
         }
      internal void WriteToSummaryFile(string Line)
         {
         try
            {
            SummaryFile.WriteLine(Line);
            }
         catch (Exception)
            {
            // This is bad. It means that the associated apsim.exe has been closed but
            // we're still being passed stdout stuff.
            try
               {
               SummaryFile = new StreamWriter(Details.SummaryFileName, true);
               SummaryFile.WriteLine(Line);
               SummaryFile.Close();
               }
            catch (Exception err)
               {
               }
            }
         }
      }

   /// <summary>
   /// This is the main simulation runner class. It takes runnable objects, adds
   /// them to the run queue, and schedules them to execute when it can. It is
   /// multi-CPU aware and is capable of running several in parallel.
   /// </summary>
   public class SimulationRunner
      {
      [Flags]
      public enum ThreadAccess : int
         {
         TERMINATE = (0x0001),
         SUSPEND_RESUME = (0x0002),
         GET_CONTEXT = (0x0008),
         SET_CONTEXT = (0x0010),
         SET_INFORMATION = (0x0020),
         QUERY_INFORMATION = (0x0040),
         SET_THREAD_TOKEN = (0x0080),
         IMPERSONATE = (0x0100),
         DIRECT_IMPERSONATION = (0x0200)
         }

      [DllImport("kernel32.dll")]
      static extern IntPtr OpenThread(ThreadAccess dwDesiredAccess, bool bInheritHandle, uint dwThreadId);
      [DllImport("kernel32.dll")]
      static extern uint SuspendThread(IntPtr hThread);
      [DllImport("kernel32.dll")]
      static extern int ResumeThread(IntPtr hThread);


      private int NumCPUsToUse = 1;
      private List<SingleRun> Simulations = new List<SingleRun>();
      private List<SingleRun> SimulationsRunning = new List<SingleRun>();
      private int NextIndex = -1;
      private int NumCompleted = 0;
      private ISynchronizeInvoke MainThread;
      private bool Stopped = false;
      private bool KillThread = false;
      private Thread WorkerThread;
      private object LockObject = new object();
      private int NumApsimsRunning = 0;
      private double _PercentageComplete = 0;
      private bool IsPaused = false;

      public delegate void WriteDelegate(Detail Simulation, string Line);
      public event WriteDelegate StdOutWritten;
      public event WriteDelegate StdErrWritten;

      /// <summary>
      /// Constructor.
      /// </summary>
      /// <param name="MainThread">An object implementing the
      /// ISynchronizeInvoke interface.  All events will be delivered
      /// through this object, ensuring that they are delivered to the
      /// correct thread.</param>
      public SimulationRunner(ISynchronizeInvoke MainThread)
         {
         this.MainThread = MainThread;
         string NumberOfProcesses = Environment.GetEnvironmentVariable("NUMBER_OF_PROCESSORS");
         if (NumberOfProcesses != null && NumberOfProcesses != "")
            NumCPUsToUse = Convert.ToInt32(NumberOfProcesses);
         NumCPUsToUse = Math.Max(NumCPUsToUse, 0);
         WorkerThread = new Thread(DoWork);
         WorkerThread.Start();
         }
      public void Close()
         {
         lock (LockObject)
            {
            NumApsimsRunning = 0;
            SimulationsRunning.Clear();
            KillThread = true;
            for (int i = 0; i < NextIndex; i++)
               Simulations[i].Close();
            }

         // wait for the thread to die.
         while (WorkerThread.IsAlive)
            KillThread = true;
         }

      public void Clear()
         {
         lock (LockObject)
            {
            Simulations.Clear();
            SimulationsRunning.Clear();
            Reset();
            }
         }
      public void Reset()
         {
         NumApsimsRunning = 0;
         NumCompleted = 0;
         NextIndex = -1;
         _PercentageComplete = 0;
         foreach (SingleRun Run in Simulations)
            {
            Run.Details.HasErrors = false;
            Run.Details.HasWarnings = false;
            Run.Details.IsCompleted = false;
            }
         }

      /// <summary>
      /// Adds the specified simulation set to the run queue. The object
      /// will be automatically run when a free CPU becomes available.
      /// </summary>
      /// <param name="SimulationSet"></param>
      public void Add(Runnable SimulationSet)
         {
         lock (LockObject)
            {
            foreach (string Name in SimulationSet.SimulationsToRun)
               Simulations.Add(new SingleRun(SimulationSet, Name));
            }
         }

      /// <summary>
      /// Returns a list of simulations and details that are currently in the queue.
      /// </summary>
      public List<Detail> SimulationDetails
         {
         get
            {
            List<Detail> Queue = new List<Detail>();
            foreach (SingleRun Run in Simulations)
               Queue.Add(Run.Details);
            return Queue;
            }
         }

      /// <summary>
      /// The number of simulations currently in the queue.
      /// </summary>
      public int Count
         {
         get
            {
            return Simulations.Count;
            }
         }

      /// <summary>
      /// Return the number of completed simulations.
      /// </summary>
      public int NumberCompleted
         {
         get
            {
            return NumCompleted;
            }
         }
    
      /// <summary>
      /// Return the number of simulations with errors.
      /// </summary>
      public int NumberWithErrors
         {
         get
            {
            int Count = 0;
            foreach (SingleRun Run in Simulations)
               {
               if (Run.Details.HasErrors)
                  Count++;
               }
            return Count;
            }
         }

      /// <summary>
      /// Return the number of simulations with errors.
      /// </summary>
      public int NumberWithWarnings
         {
         get
            {
            int Count = 0;
            foreach (SingleRun Run in Simulations)
               {
               if (Run.Details.HasWarnings)
                  Count++;
               }
            return Count;
            }
         }

      /// <summary>
      /// Return the percentage complete
      /// </summary>
      public int PercentageComplete
         {
         get
            {
            return (int) MathUtility.Round(_PercentageComplete, 0);
            }
         }

      /// <summary>
      /// Get or set the number of CPU's that APSIM is allowed to use.
      /// </summary>
      public int NumCPUs
         {
         get
            {
            return NumCPUsToUse;
            }
         set
            {
            NumCPUsToUse = value;
            }
         }

      /// <summary>
      /// Run the simulations in the queue
      /// </summary>
      public void Run()
         {
         lock (LockObject)
            {
            if (IsPaused)
               {
               IsPaused = false;
               foreach (Process P in Process.GetProcesses())
                  {
                  if (P.ProcessName.ToLower() == "apsim")
                     {
                     foreach (ProcessThread T in P.Threads)
                        {
                        IntPtr pOpenThread = OpenThread(ThreadAccess.SUSPEND_RESUME, false, (uint)T.Id);

                        if (pOpenThread != IntPtr.Zero)
                           ResumeThread(pOpenThread);
                        }
                     }
                  }
               }
            else
               {
               Stopped = false;
               IsPaused = false;
               }
            }
         }

      /// <summary>
      /// Stop all APSIM threads immediately.
      /// </summary>
      public void Stop()
         {
         lock (LockObject)
            {
            Stopped = true;
            IsPaused = false;

            NextIndex = Simulations.Count;
            foreach (Process P in Process.GetProcesses())
               {
               if (P.ProcessName == "Apsim")
                  {
                  P.Kill();
                  }
               }
            }
         }

      internal void Pause()
         {
         lock (LockObject)
            {
            IsPaused = true;
            foreach (Process P in Process.GetProcesses())
               {
               if (P.ProcessName.ToLower() == "apsim")
                  {
                  foreach (ProcessThread T in P.Threads)
                     {
                     IntPtr pOpenThread = OpenThread(ThreadAccess.SUSPEND_RESUME, false, (uint)T.Id);

                     if (pOpenThread != IntPtr.Zero)
                        SuspendThread(pOpenThread);
                     }
                  }
               }
            }
         }
      /// <summary> 
      /// Main worker thread for keeping APSIM busy.
      /// </summary>
      private void DoWork()
         {
         while (!KillThread)
            {
            SingleRun SimulationToRun = GetNextSimulationToRun();
            if (SimulationToRun != null)
               {
               ProcessCaller ApsimProcess = new ProcessCaller(MainThread);
               ApsimProcess.Tag = SimulationToRun;
               try
                  {
                  string Messages = SimulationToRun.PrepareToRun();

                  ApsimProcess.FileName = Path.GetDirectoryName(Application.ExecutablePath) + "\\apsim.exe";
                  ApsimProcess.Arguments = "\"" + SimulationToRun.SimFileName + "\"";
                  ApsimProcess.WorkingDirectory = Path.GetDirectoryName(SimulationToRun.SimFileName);
                  ApsimProcess.AllFinished += OnApsimExited;
                  ApsimProcess.StdOutReceived += OnStdOut;
                  ApsimProcess.StdErrReceived += OnStdError;

                  // Do something with any messages during the create .sim bit.
                  if (Messages != "")
                     {
                     OnStdError(ApsimProcess, new CSGeneral.DataReceivedEventArgs("APSIM  Fatal  Error"));
                     CSGeneral.DataReceivedEventArgs arg = new CSGeneral.DataReceivedEventArgs(Messages);
                     OnStdOut(ApsimProcess, arg);
                     }

                  lock (LockObject)
                     {
                     if (!Stopped)
                        {
                        NumApsimsRunning++;
                        SimulationsRunning.Add(SimulationToRun);
                        ApsimProcess.Start();
                        }
                     }
                  }
              
               catch (Exception ex) 
                  {
                  SimulationToRun.Details.HasErrors = true;
                  CSGeneral.DataReceivedEventArgs Arg = new CSGeneral.DataReceivedEventArgs(ex.Message);
                  OnStdError(ApsimProcess, new CSGeneral.DataReceivedEventArgs("APSIM  Fatal  Error"));
                  OnStdOut(ApsimProcess, Arg);
                  CloseJob(SimulationToRun);
                  }
               }
            Thread.Sleep(100);
            }
         }
      private SingleRun GetNextSimulationToRun()
         {
         lock (LockObject)
            {
            if (!IsPaused && NextIndex + 1 < Simulations.Count && NumApsimsRunning < NumCPUsToUse)
               {
               NextIndex++;
               return Simulations[NextIndex];
               }
            else
               return null;
            }
         }

      /// <summary>
      /// A handler for an APSIM process writting to stdout.
      /// </summary>
      private void OnStdOut(object sender, CSGeneral.DataReceivedEventArgs e)
         {
         ProcessCaller Process = (ProcessCaller)sender;
         SingleRun Simulation = (SingleRun)Process.Tag;
         Simulation.WriteToSummaryFile(e.Text);
         InvokeStdOutEvent(Simulation.Details, e.Text);
         }

      /// <summary>
      /// A handler for an APSIM process writting to stderr.
      /// </summary>
      private void OnStdError(object sender, CSGeneral.DataReceivedEventArgs e)
         {
         ProcessCaller Process = (ProcessCaller)sender;
         SingleRun Simulation = (SingleRun)Process.Tag;

         // Look for a percent complete
         if (e.Text.Length > 0)
            {
            if (e.Text[0] == '%')
               {
               int Percent = Convert.ToInt32(e.Text.Substring(1));
               if (Percent >= 0 && Percent < 100)
                  CalculatePercentComplete(Simulation, Percent);
               }
            else
               {
               if (e.Text.IndexOf("APSIM  Fatal  Error") != -1)
                  Simulation.Details.HasErrors = true;
               else if (e.Text.IndexOf("APSIM Warning Error") != -1)
                  Simulation.Details.HasWarnings = true;
               InvokeStdErrEvent(Simulation.Details, e.Text);
               }
            }
         }

      /// <summary>
      /// A handler for when an APSIM process terminates.
      /// </summary>
      private void OnApsimExited(object sender)
         {
         // APSIM has finished running, so we need to close the summary file
         // and attempt to run the next simulation

         ProcessCaller Process = (ProcessCaller)sender;
         SingleRun Simulation = (SingleRun)Process.Tag;
         CloseJob(Simulation);
         CalculatePercentComplete(Simulation, 100);
         }
      private void CloseJob(SingleRun Simulation)
         {
         try
            {
            Simulation.SummaryFile.Close();
            }
         catch (Exception )
            {
            }
         Simulation.IsCompleted();
         lock (LockObject)
            {
            if (SimulationsRunning.Contains(Simulation))
               {
               SimulationsRunning.Remove(Simulation);
               NumApsimsRunning--;
               }
            NumCompleted++;
            }

         }

      /// <summary>
      /// Invokes the event stdout callback
      /// </summary>
      private void InvokeStdOutEvent(Detail SimulationDetail, string Line)
         {
         if (StdOutWritten != null && KillThread == false)
            {
            object[] args = new object[] { SimulationDetail, Line };
            MainThread.Invoke(StdOutWritten, args);
            }
         }

      /// <summary>
      /// Invokes the event stderr callback
      /// </summary>
      private void InvokeStdErrEvent(Detail SimulationDetail, string Line)
         {
         if (StdErrWritten != null && KillThread == false)
            {
            object[] args = new object[] { SimulationDetail, Line };
            MainThread.Invoke(StdErrWritten, args);
            }
         }

      /// <summary>
      /// Calculate the percentage done.
      /// </summary>
      private void CalculatePercentComplete(SingleRun Simulation, int PercentDone)
         {
         int SimulationIndex = SimulationsRunning.IndexOf(Simulation);
         if (SimulationIndex != -1)
            SimulationsRunning[SimulationIndex].PercentComplete = PercentDone;

         if (PercentDone == 100 && SimulationsRunning.Contains(Simulation))
            CloseJob(Simulation);
         else
            {
            double PercentPerSimulation = 100.0 / Simulations.Count;
            _PercentageComplete = NumCompleted * PercentPerSimulation;
            foreach (SingleRun Sim in SimulationsRunning)
               _PercentageComplete += Sim.PercentComplete / 100.0 * PercentPerSimulation;
            _PercentageComplete = MathUtility.Round(_PercentageComplete, 0);
            if (NumCompleted == Simulations.Count)
               _PercentageComplete = 100;
            else
               _PercentageComplete = Math.Min(_PercentageComplete, 99);
            }
         }

      }
   }
