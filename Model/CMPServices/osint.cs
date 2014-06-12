using System;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;

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
        //==============================================================================
        /// <summary>
        /// Get the user's documents directory without the trailing path delimiter.
        /// </summary>
        /// <returns>For Linux it will return a path equivalent to ~\Documents</returns>
        //==============================================================================
        static public String getPersonalFilesDir()
        {
            String result = "";

            if (Path.VolumeSeparatorChar == '/')
            {
                result = getEnvVariable("HOME") + "Documents";
            }
            else
            {
                result = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments);
            }
            return result;
        }
        //==============================================================================
        /// <summary>
        /// Get the user's application data files directory on the host disk without the trailing
        /// backslash.
        /// On Linux it returns ~ because the app data is usually in ~/.appname.
        /// </summary>
        /// <returns>The Application Data directory.</returns>
        //==============================================================================
        static public String getPersonalDataDir()
        {
            String result = "";
            if (Path.VolumeSeparatorChar == '/')
            {
                result = "~";
            }
            else
            {
                result = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData);
            }
            return result;
        }
        //==============================================================================
        /// <summary>
        /// Get the common application data files directory on the host disk without the trailing
        /// backslash.
        /// </summary>
        /// <returns></returns>
        //==============================================================================
        static public String getCommonDataDir()
        {
            if (Path.VolumeSeparatorChar == '/')
            {
                return "";
            }
            else
            {
                return Environment.GetFolderPath(Environment.SpecialFolder.CommonApplicationData);
            }
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

        // We will need to use a different set of libraries under Linux
        [DllImport("libdl")]
        internal static extern IntPtr dlopen(String dllname, int flag);

        [DllImport("libdl")]
        internal static extern IntPtr dlsym(IntPtr hModule, String procname);

        [DllImport("libdl")]
        internal static extern int dlclose(IntPtr hModule);

        [DllImport("kernel32.dll")]
        internal static extern IntPtr LoadLibrary(String dllname);

        [DllImport("kernel32.dll")]
        internal static extern IntPtr GetProcAddress(IntPtr hModule, String procname);

        [DllImport("kernel32.dll")]
        internal static extern int FreeLibrary(IntPtr hModule);

        //============================================================================
        // Wrappers for the routines to access native libraries, to localise
        // for platform dependencies
        //============================================================================
        /// <summary>
        /// Load a library.
        /// </summary>
        /// <param name="dllName">Path to the dll</param>
        /// <returns>The return value of either dlopen() or LoadLibrary()</returns>
        //=========================================================================
        static public IntPtr LibLoad(String dllName)
        {
            if (Path.VolumeSeparatorChar == '/')
            {
                return dlopen(dllName, 1); // 1 is usually the value for RTLD_LAZY...
            }
            else
                return LoadLibrary(dllName);
        }
        //=========================================================================
        /// <summary>
        /// Get the address of a function by name.
        /// </summary>
        /// <param name="handle">dll handle</param>
        /// <param name="entry">Name of the function</param>
        /// <returns>The return value from either dlsym() or GetProcAddress()</returns>
        //=========================================================================
        static public IntPtr LibGetAddr(IntPtr handle, String entry)
        {
            if (Path.VolumeSeparatorChar == '/')
            {
                return dlsym(handle, entry);
            }
            else
                return GetProcAddress(handle, entry);
        }
        //=========================================================================
        /// <summary>
        /// Unload the dll
        /// </summary>
        /// <param name="handle"></param>
        /// <returns>The return value from either dlclose() or FreeLibrary()</returns>
        //=========================================================================
        static public int LibUnload(IntPtr handle)
        {
            if (Path.VolumeSeparatorChar == '/')
                return dlclose(handle);
            else
                return FreeLibrary(handle);
        }
        //=========================================================================
        /// <summary>
        /// Load the dll and return the handle to the module
        /// </summary>
        /// <param name="Executable">Path to the dll to load.</param>
        /// <returns>The dll Handle</returns>
        //=========================================================================
        static public IntPtr loadDll(String Executable)
        {
            IntPtr dllHandle = IntPtr.Zero;
            if (dllHandle.Equals(IntPtr.Zero))
            {
                //change the current directory so that apsim components can find their support dll's
                string sCurrent = Directory.GetCurrentDirectory();
                string sModuleDir = Path.GetDirectoryName(Executable);
                if (sModuleDir != String.Empty)
                    Directory.SetCurrentDirectory(sModuleDir);
                try
                {
                    dllHandle = LibLoad(Executable);
                }
                finally
                {
                    //change back to original current directory
                    Directory.SetCurrentDirectory(sCurrent);
                }
            }
            return dllHandle;
        }

    }
}
