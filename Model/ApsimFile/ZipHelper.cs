using System;
using System.Collections.Generic;
using System.IO;
using ICSharpCode.SharpZipLib.Zip;

public class Zip
        {
        /// <summary>
        /// Zip all the specified files into the specified ZipFileName.
        /// </summary>
        public static void ZipFiles(IEnumerable<string> FilesToZip, string filename, string Password, int compressionLevel = 5)
            {
		    if (!File.Exists(filename))
                File.Delete(filename);
            ZipOutputStream Zip = new ZipOutputStream(File.Create(filename));
            if (Password != "")
                Zip.Password = Password;
            try
                {
                    Zip.SetLevel(compressionLevel); // 0 - store only to 9 - means best compression
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
                    string DestFileName = Path.Combine(DestFolder, Path.GetFileName(Entry.Name));
                    string DestDir = Path.GetDirectoryName(DestFileName);
                    if (!Directory.Exists(DestDir)) { Directory.CreateDirectory(DestDir); }
                    using (BinaryWriter FileOut = new BinaryWriter(new FileStream(DestFileName, FileMode.Create)))
                        {
                        byte[] buffer = new byte[32768];
                        int size;
                        while ((size = Zip.Read(buffer, 0, buffer.Length)) > 0)
                            FileOut.Write(buffer, 0, size);
                        FileOut.Close();
                        }
                    }
                }
            finally { Zip.Close(); }
            }
        }

