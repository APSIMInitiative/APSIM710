using System;
using System.Threading;
using System.Linq;
using System.Collections.Generic;
using Renci.SshNet;

namespace ApsimHPC
{
    public delegate void UIMessageEventHandler(object sender, ApsimHPC.UIMessageEventArgs e);
    public class UIMessageEventArgs : EventArgs
    {
        private string text = "";

        public string MessageText
        {
            get { return text; }
            set { this.text = value; }
        }
    }

    public class cred
    {
        public string remoteHost = "";
        public string username = "";
        public string password = "";
    }
    public class sshCommandOutput
    {
        public int ExitStatus;
        public string Result = "";
        public string Error = "";
    }

    public class Server
    {
        private cred _cred = new cred();

        public Server()
        {
        }

        ~Server()
        {
            _sshClient = null;
            _scpClient = null;
        }


        public cred cred
        {
            get { return _cred; }
            set
            {
                if (_cred.username != value.username)
                    invalidateClient();
                if (_cred.password != value.password)
                    invalidateClient();
                if (_cred.remoteHost != value.remoteHost)
                    invalidateClient();
                _cred = value;
            }
        }

        private void invalidateClient()
        {
            _sshClient?.Disconnect();
            _sshClient = null;
            _scpClient?.Disconnect();
            _scpClient = null;
        }

        private ScpClient _scpClient = null;
        private ScpClient scpConnect()
        {
            if (cred.remoteHost == "" || cred.username == "" || cred.password == "")
            {
                logMessage("Please set username / password");
            }
            else
            {
                try
                {
                    if (_scpClient == null)
                    {
                        logMessage("Thread " + Thread.CurrentThread.ManagedThreadId + " opening new scp connection to " + cred.remoteHost);
                        ConnectionInfo ConnNfo = new ConnectionInfo(cred.remoteHost, cred.username,
                                                     new AuthenticationMethod[] { new PasswordAuthenticationMethod(cred.username, cred.password) });
                        _scpClient = new ScpClient(ConnNfo);
                        _scpClient.BufferSize = 512 * 1024;
                    }
                    if (!_scpClient.IsConnected)
                    {
                        _scpClient.Connect();
                        logMessage("Scp connected to " + cred.remoteHost);
                    }
                    return _scpClient;
                }
                catch (Renci.SshNet.Common.SshException e)
                {
                    logMessage(e.Message);
                }
            }
            return null;
        }

        public sshCommandOutput output;
        private SshClient _sshClient = null;

        private sshCommandOutput run(string cmd)
        {
            sshCommandOutput result = new sshCommandOutput();
            try
            {
                if (_sshClient == null)
                {
                    logMessage("Thread " + Thread.CurrentThread.ManagedThreadId + " opening new ssh connection to " + cred.remoteHost);
                    ConnectionInfo ConnNfo = new ConnectionInfo(cred.remoteHost, cred.username,
                                                 new AuthenticationMethod[] { new PasswordAuthenticationMethod(cred.username, cred.password) });
                    ConnNfo.Timeout = TimeSpan.FromSeconds(60);
                    _sshClient = new SshClient(ConnNfo);
                    //_sshClient.ErrorOccurred += ... FIXME (only for async calls)
                }
                if (!_sshClient.IsConnected)
                {
                    _sshClient.Connect(); // failure will throw
                    logMessage("Ssh connected to " + cred.remoteHost);
                }
                SshCommand o = _sshClient.RunCommand(cmd);
                result.ExitStatus = o.ExitStatus;
                result.Error = o.Error;
                result.Result = o.Result;
            }
            catch (Exception e)
            {
                logMessage(e.Message);
                result.ExitStatus = 1;
                result.Error = e.Message;
                result.Result = "";
            }
            return (result);
        }

        /// <summary>
        /// Return a list of apsim versions (in standalone singularity containers) that peter has built. 
        /// </summary>
        /// <returns>The remote versions.</returns>
        public List<string> getRemoteVersions()
        {
            List<string> result = new List<string>();
            output = run("ls /home/uqpdevo1/*.sapp");
            if (output.ExitStatus == 0)
            {
                foreach (string sapp in output.Result.Split(new[] { '\n' }, StringSplitOptions.None))
                {
                    if (sapp.IndexOf('/') >= 0)
                    {
                        string nicename = sapp.Substring(sapp.LastIndexOf('/') + 1);
                        result.Add(nicename.Substring(0, nicename.Length - 5));
                    }
                }
            }
            //Console.WriteLine ("remote=" + output.Result + "\nStatus=" + output.ExitStatus);
            return (result);
        }

        public bool runCommand(string cmd)
        {
            logMessage("Thread " + Thread.CurrentThread.ManagedThreadId + " executing command \"" + cmd + "\"");

            output = run(cmd);
            if (output.ExitStatus != 0)
                logMessage((output.ExitStatus == 0 ? "" : "Return status=" + output.ExitStatus + "\n") +
                               (output.Result != "" ? "stdout=" + output.Result : "") +
                               (output.Error != "" ? "stderr=" + output.Error : ""));
            return (output.ExitStatus == 0);
        }

        public bool upload(string dest, System.IO.FileInfo file)
        {
            ScpClient client = scpConnect();
            if (client != null)
            {
                try
                {
                    client.Upload(file, dest);
                }
                catch (Exception e)
                {
                    logMessage("Upload failed:" + e.Message);
                    return (false);
                }
            }
            return (true);
        }

        public bool download(string filename, System.IO.FileInfo file)
        {
            ScpClient client = scpConnect();
            if (client != null)
            {
                try
                {
                    client.Download(filename, file);
                }
                catch (Exception e)
                {
                    logMessage("Download failed:" + e.Message);
                    return (false);
                }
            }
            return (true);
        }

        public event UIMessageEventHandler OnAppendToLog;
        public void logMessage(string message)
        {
            UIMessageEventHandler handler = OnAppendToLog;
            if (handler != null)
                handler(this, new UIMessageEventArgs { MessageText = message });
        }
    }
}

