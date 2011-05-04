using System;
using System.IO;
using System.Reflection;

namespace CMPServices
{
     
    /// <summary>
    /// Provides access to utility functions associated with the operating system.
    /// </summary>
    public class TOSInterface
    {
        /// <summary>
        /// Describes what type of module.
        /// </summary>
        public enum CompilationMode {  
            /// <summary>
            /// Unknown
            /// </summary>
            Invalid, 
            /// <summary>
            /// Native Win32 code
            /// </summary>
            Native, 
            /// <summary>
            /// Common Language Runtime
            /// </summary>
            CLR, 
            /// <summary>
            /// Mixed mode
            /// </summary>
            Mixed };
        //=======================================================================
        /// <summary>
        /// Retrieve any environment variable value.
        /// </summary>
        /// <param name="name">Name of the environment variable.</param>
        /// <returns>Value of this variable.</returns>
        //=======================================================================
        static public String getEnvVariable(String name)
        {
            return Environment.GetEnvironmentVariable(name);
        }
        //=======================================================================
        /// <summary>
        /// Check if the file exists.
        /// </summary>
        /// <param name="fileName">File name.</param>
        /// <returns>True if it is found.</returns>
        //=======================================================================
        static public Boolean fileExists(String fileName)
        {
            return File.Exists(fileName);
        }
        //=======================================================================
        /// <summary>
        /// Unused
        /// </summary>
        /// <param name="diskID"></param>
        /// <returns>0</returns>
        //=======================================================================
        static private int checkDiskSpace(uint diskID)
        {
            return 0;
        }
        //=======================================================================
        /// <summary>
        /// Get the operating system version string.
        /// </summary>
        /// <returns>Operating system version.</returns>
        //=======================================================================
        static public String osVersion()
        {
            return Environment.OSVersion.ToString();
        }
        //=======================================================================
        /// <summary>
        /// Get a new unique temporary filename.
        /// </summary>
        /// <returns>Full path for the file.</returns>
        //=======================================================================
        static public String getTempFileName()
        {
            return Path.GetTempFileName();
        }
        //=======================================================================
        /// <summary>
        /// Get the path to the temporary files directory.
        /// </summary>
        /// <returns>Temp file directory with trailing \</returns>
        //=======================================================================
        static public String getTempFilesDir()
        {
            return Path.GetTempPath();
        }
        //=======================================================================
        /// <summary>
        /// Get the Common Files directory.
        /// </summary>
        /// <returns>The full path of the Common Files directory.</returns>
        //=======================================================================
        static public String getCommonFilesDir()
        {
            return Environment.GetFolderPath(Environment.SpecialFolder.CommonProgramFiles);
        }
        //=======================================================================
        /// <summary>
        /// Returns the full path name of the application including extension.
        /// </summary>
        /// <returns>Executable name.</returns>
        //=======================================================================
        static public String getApplicationPath()
        {
            String[] arguments = Environment.GetCommandLineArgs();
            return arguments[0];
        }
        //=======================================================================
        /// <summary>
        /// Gets the directory of this assembly without the trailing backslash.
        /// </summary>
        /// <returns>The path of this assembly.</returns>
        //=======================================================================
        static public String getAppDir()
        {
            return Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);
        }
        //=======================================================================
        /// <summary>
        /// Search for the file firstly in the same place as the module designated
        /// by startPath and then in the application directory and then
        /// in the Common Files directory.
        /// </summary>
        /// <param name="startPath">Use this path first to search for the file.</param>
        /// <param name="fileName">Short name of the file to find e.g. ddldb.dll</param>
        /// <returns>Path with no terminating backslash. Returns empty string if not found.</returns>
        //=======================================================================
        static public String findPathTo(String startPath, String fileName)
        {
            String appPath = "";
            String searchFile;

            Char sep = Path.DirectorySeparatorChar;

            searchFile = Path.Combine(startPath, fileName);    //look in startPath
            if (fileExists(searchFile))
                appPath = startPath;
            else
            {
                searchFile = Path.GetDirectoryName(getApplicationPath()) + sep + fileName;  //look in this app path
                if (fileExists(searchFile))
                    appPath = getApplicationPath();
                else
                {
                    searchFile = Path.Combine(getCommonFilesDir(), "ausfarm" + sep + "cmp" + sep + fileName);        //look in the CommonFiles path
                    if (fileExists(searchFile))
                        appPath = Path.Combine(getCommonFilesDir(), "ausfarm" + sep + "cmp");
                }
            }

            return appPath;
        }
        //=========================================================================
        /// <summary>
        /// Determine if the file refered to is a native win32 or a CLR assembly.
        /// Mixed mode assemblies are CLR.
        /// Visual C++ Developer Center. http://msdn2.microsoft.com/en-us/library/c91d4yzb(VS.80).aspx
        /// </summary>
        /// <param name="filename">File name of the Assembly or native dll to probe.</param>
        /// <returns>Compilation mode.</returns>
        //=========================================================================
        static public CompilationMode isManaged(string filename)
        {
            try
            {
                byte[] data = new byte[4096];
                FileInfo file = new FileInfo(filename);
                Stream fin = file.Open(FileMode.Open, FileAccess.Read, FileShare.ReadWrite);
                /*Int32 iRead =*/ fin.Read(data, 0, 4096);
                fin.Close();

                // If we are running on Linux, the executable/so will start with the string 0x7f + 'ELF'
                // If the 5 byte is 1, it's a 32-bit image (2 indicates 64-bit)
                // If the 16th byte is 3, it's a shared object; 2 means it's an executable
                // If it's a Mono/.Net assembly, we should the the "Windows" header

                // For now, if we're on Linux just see if it has an "ELF" header
                if (Path.VolumeSeparatorChar == '/' && data[0] == 0x7f && data[1] == 'E' && data[2] == 'L' && data[3] == 'F')
                    return CompilationMode.Native;

                // Verify this is a executable/dll
                if (UInt16FromBytes(data, 0) != 0x5a4d)
                    return CompilationMode.Invalid;

                uint headerOffset = UInt32FromBytes(data, 0x3c);  // This will get the address for the WinNT header

                //at the file offset specified at offset 0x3c, is a 4-byte
                //signature that identifies the file as a PE format image file. This signature is “PE\0\0”
                if (UInt32FromBytes(data, headerOffset) != 0x00004550)
                    return CompilationMode.Invalid;

                //uint machineType = UInt16FromBytes(data, headerOffset + 4); //type of machine
                uint optionalHdrBase = headerOffset + 24;
                //uint exportTableAddr = UInt32FromBytes(data, optionalHdrBase + 96);     //.edata
                uint exportTableSize = UInt32FromBytes(data, optionalHdrBase + 96 + 4); //.edata size

                Int32 iLightningAddr = (int)headerOffset + 24 + 208;    //CLR runtime header addr & size
                Int32 iSum = 0;
                Int32 iTop = iLightningAddr + 8;

                for (int i = iLightningAddr; i < iTop; ++i)
                    iSum |= data[i];

                if (iSum == 0)
                    return CompilationMode.Native;
                else
                {
                    if (exportTableSize > 0)
                        return CompilationMode.Mixed;
                    else
                        return CompilationMode.CLR;
                }
            }
            catch (Exception e)
            {
                throw (e);
            }
        }
        static private UInt32 UInt32FromBytes(byte[] p, uint offset)
        {
            return (UInt32)(p[offset + 3] << 24 | p[offset + 2] << 16 | p[offset + 1] << 8 | p[offset]);
        }
        static private UInt16 UInt16FromBytes(byte[] p, uint offset)
        {
            return (UInt16)(p[offset + 1] << 8 | p[offset]);
        }

    }
}
