using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using ICSharpCode.SharpZipLib.Zip;

namespace UIUtility
   {
   public class Zip
      {
      /// <summary>
      /// Zip all the specified files into the specified ZipFileName.
      /// </summary>
      public static string ZipFiles(List<string> FilesToZip, string ZipFileName, string Password)
         {
         if (!File.Exists(ZipFileName))
            File.Delete(ZipFileName);

         ZipOutputStream Zip = new ZipOutputStream(File.Create(ZipFileName));
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
            Zip.Finish();
            Zip.Close();
            return ZipFileName;
            }
         catch (System.Exception)
            {
            Zip.Finish();
            Zip.Close();
            File.Delete(ZipFileName);
            throw;
            }
         }
      public static void UnZipFiles(string ZipFile, string DestFolder, string Password)
         {
         // ----------------------------------------
         // Unzip an archive to the specified folder
         // ----------------------------------------
         ZipInputStream Zip = new ZipInputStream(File.Open(ZipFile, FileMode.Open, FileAccess.Read));
         Zip.Password = Password;
         ZipEntry Entry;
         while ((Entry = Zip.GetNextEntry()) != null)
            {
            string DestFileName = DestFolder + "\\" + Path.GetFileName(Entry.Name);
            BinaryWriter FileOut = new BinaryWriter(new FileStream(DestFileName, FileMode.CreateNew));

            int size = 2048;
            byte[] data = new byte[2048];
            while (true)
               {
               size = Zip.Read(data, 0, data.Length);
               if (size > 0)
                  FileOut.Write(data, 0, size);
               else
                  break;
               }
            FileOut.Close();
            FileOut = null;
            }
         Zip.Close();
         }
      }
   }
