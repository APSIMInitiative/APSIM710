using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

class Program
{
    static int Main(string[] args)
    {
        try
        {
            // Look for a response file.
            if (args.Length == 1 && args[0][0] == '@')
            {
                StreamReader In = new StreamReader(args[0].Substring(1));
                args = In.ReadToEnd().Split("\r\n".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                In.Close();
            }
            Go(args);
        }
        catch (Exception err)
        {
            Console.WriteLine(err.Message);
            return 1;
        }
        return 0;
    }

    private static void Go(string[] args)
    {
        System.Net.Mail.MailMessage mail = new System.Net.Mail.MailMessage();

        foreach (string arg in args)
        {
            if (arg.Contains("/from:"))
                mail.From = new System.Net.Mail.MailAddress(arg.Substring(6));
            else if (arg.Contains("/to:"))
                mail.To.Add(arg.Substring(4));
            else if (arg.Contains("/subject:"))
                mail.Subject = arg.Substring(9);
            else if (arg.Contains("/body:"))
            {
                if (arg.Substring(6)[0] == '@')
                {
                    StreamReader In = new StreamReader(arg.Substring(7));
                    mail.Body = In.ReadToEnd();
                    In.Close();
                }
                else
                    mail.Body = arg.Substring(6);
            }
        }

        mail.IsBodyHtml = true;
        System.Net.Mail.SmtpClient smtp = new System.Net.Mail.SmtpClient("smtp-relay.csiro.au");

        /*System.Net.NetworkCredential cred = new System.Net.NetworkCredential("apsimmailer@gmail.com", "CsiroDMZ!");
        System.Net.Mail.SmtpClient smtp = new System.Net.Mail.SmtpClient("smtp.gmail.com");
        smtp.UseDefaultCredentials = false;
        smtp.EnableSsl = true;
        smtp.Credentials = cred;
        smtp.Port = 587;*/

        smtp.Send(mail);
    }


}
