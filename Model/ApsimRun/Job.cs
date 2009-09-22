using System;
using System.Collections.Generic;
using System.Text;
using ApsimFile;
using System.IO;

public abstract class Job
   {
   private string      _Name = "";
   protected string    _StdOut = "";
   protected string    _StdErr = "";
   protected double    _PercentComplete = 0;
   protected bool      _HasErrors = false;
   protected bool      _HasWarnings = false;
   protected bool      _IsRunning = false;
   protected JobRunner _JobRunner = null;

   // A base class for all jobs.

   public string Name { get { return _Name; } }
   public bool HasErrors { get { return _HasErrors; } }
   public bool HasWarnings { get { return _HasErrors; } }
   public double PercentComplete { get { return _PercentComplete; } }
   public string StdOut { get { return ""; } }
   public string StdErr { get { return ""; } }
   public bool IsRunning { get { return _IsRunning; } }

   public Job(string Name, JobRunner JRunner)
      {
      _Name = Name;
      _JobRunner = JRunner;
      }
   public abstract void Run();
   public abstract void Stop();
   public abstract void Pause();
   public abstract void Resume();


   }



