using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Net;
using System.Net.Sockets;

class Program
{

    /// <summary>
    /// This program instructs the JobScheduler to save it's XML Document to a file.
    /// </summary>
    /// <param name="args"></param>
    /// <returns></returns>
    static int Main(string[] args)
    {
        try
        {
            if (args.Length != 1)
                throw new Exception("Usage: JobSchedulerSendCommand command");
            TalkToJobScheduler(args[0].Replace("\"", ""));
        }
        catch (Exception err)
        {
            Console.WriteLine(err.Message);
            return 1;
        }
        return 0;
    }


    /// <summary>
    /// Talk to JobScheduler socket and tell it to save it's XML.
    /// </summary>
    private static void TalkToJobScheduler(string FileName)
    {
        // Open a socket connection to JobScheduler.
        int PortNumber = 13000;  // Some arbitary number.
        IPAddress localAddr = IPAddress.Parse("127.0.0.1");
        IPEndPoint ipe = new IPEndPoint(localAddr, PortNumber);
        Socket S = new Socket(ipe.AddressFamily, SocketType.Stream, ProtocolType.Tcp);
        S.Connect(ipe);
        if (!S.Connected)
            throw new Exception("Cannot connect to JobScheduler via socket");

        // Send our XML to JobScheduler.
        Byte[] bytes = Encoding.ASCII.GetBytes(FileName);
        S.Send(bytes);

        // Now wait for a response.
        S.Receive(bytes);
        S.Close();
    }
}

