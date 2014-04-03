using System;
using System.Text;
using System.Xml;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;
using CMPServices;

namespace CMPComp
{
    //=========================================================================
    /// <summary>
    /// This class provides a standard interface for .NET components.
    /// You can create an instance of this class if you are loading this
    /// component from the .NET world.
    /// </summary>
    //=========================================================================
    [ComVisible(true)]
    public class TComponentInstance : TAPSIMHost
    {
        // Get Type from the the "Product" attribute of the assembly; use the DLL name if this is absent
        // Get Version from the assembly version number's major and minor components 
        // Get Author from the "Company" attribute of the assembly; use "APSIM Initiative" if this is absent
        private static String _STYPE = Assembly.GetExecutingAssembly().GetCustomAttributes(typeof(AssemblyProductAttribute), false).Length > 0 ?
                                       (Assembly.GetExecutingAssembly().GetCustomAttributes(typeof(AssemblyProductAttribute), false)[0] as AssemblyProductAttribute).Product :
                                       Path.GetFileNameWithoutExtension(Assembly.GetExecutingAssembly().GetName().Name);
        private static String _SVERSION = Assembly.GetExecutingAssembly().GetName().Version.Major.ToString() + "." +
                                          Assembly.GetExecutingAssembly().GetName().Version.Major.ToString();
        private static String _SAUTHOR = Assembly.GetExecutingAssembly().GetCustomAttributes(typeof(AssemblyCompanyAttribute), false).Length > 0 ?
                                          (Assembly.GetExecutingAssembly().GetCustomAttributes(typeof(AssemblyCompanyAttribute), false)[0] as AssemblyCompanyAttribute).Company :
                                          "APSIM Initiative";

        //=========================================================================
        /// <summary>
        /// Create an instance of a component here
        /// </summary>
        /// <param name="compID"></param>
        /// <param name="parentCompID"></param>
        /// <param name="messageCallback"></param>
        //=========================================================================
        public TComponentInstance(uint compID, uint parentCompID, MessageFromLogic messageCallback)
            : base(compID, parentCompID, messageCallback, _STYPE, _SVERSION, _SAUTHOR)
        {
        }
    }
    //============================================================================
    // Component developers should not need to change the code below this point.
    // It exists to provide a wrapping class which enables the component to be
    // loaded and used by native applications.
    //============================================================================

    //=========================================================================
    /// <summary>
    /// 
    /// </summary>
    //=========================================================================
    [ComVisible(true)]
    public class TGCComponent : TComponentInstance
    {
        //============================================================================
        /// <summary>
        /// This class is a thin wrapper over the TComponentInstance class, allowing
        /// the caller to provide the callback as a function pointer rather than as
        /// a delegate. This makes it relative easy to call the constructor from
        /// unmanaged code, without requiring mixed-mode dlls.
        /// </summary>
        /// <param name="compID"></param>
        /// <param name="parentCompID"></param>
        /// <param name="messageCallback"></param>
        //============================================================================
        // Note that we are assuming that an "IntPtr" can be stored within a ulong value.
        // This value is cast to an IntPtr, then mashalled to a delegate
        public TGCComponent(uint compID, uint parentCompID, ulong messageCallback)
            : base(compID, parentCompID,
            messageCallback == 0 ? null :
            (MessageFromLogic)Marshal.GetDelegateForFunctionPointer((IntPtr)messageCallback, typeof(MessageFromLogic)))
        {
            if (messageCallback != 0)
                msgNativeDestFunction = (NativeMessageFromLogic)Marshal.GetDelegateForFunctionPointer((IntPtr)messageCallback, typeof(NativeMessageFromLogic));
        }

        //[UnmanagedFunctionPointer(CallingConvention.StdCall)]
        //=========================================================================
        /// <summary>
        /// Define type for the native MessageFromLogic function
        /// </summary>
        //=========================================================================
        public delegate void NativeMessageFromLogic(ref IntPtr dummy, ref TNativeMsgHeader inMsg);
        //=========================================================================
        /// <summary>
        /// Define a delegate (function pointer) for MessageFromLogic
        /// </summary>
        //=========================================================================
        protected NativeMessageFromLogic msgNativeDestFunction = null;

        //==============================================================================
        /// <summary>
        /// Send a message up to the owning system so it can be routed throughout
        /// the simulation. Overridden to allow the message data to be converted
        /// to a native pointer.
        /// </summary>
        /// <param name="msg">Message that will be sent to the engine.</param>
        //==============================================================================
        protected override void sendMessageToEngine(TMsgHeader msg)
        {
            // Copy from TMsgHeader to TNativeMsgHeader.
            // The difference is in the nature of the data pointer.
            try
            {
                TNativeMsgHeader msgPtr = new TNativeMsgHeader();
                uint nBytes = msg.nDataBytes;
                try
                {
                    msgPtr.version = msg.version;
                    msgPtr.msgType = msg.msgType;
                    msgPtr.from = msg.from;
                    msgPtr.to = msg.to;
                    msgPtr.msgID = msg.msgID;
                    msgPtr.toAck = msg.toAck;
                    msgPtr.nDataBytes = nBytes;

                    if (nBytes == 0)
                    {
                        msgPtr.dataPtr = IntPtr.Zero;
                    }
                    else
                    {
                        /*
                        if (nBytes > memAllocSize)
                        {

                            if (memAllocSize > 0)
                                Marshal.FreeHGlobal(nativeMem);
                            nativeMem = Marshal.AllocHGlobal((int)nBytes);
                            memAllocSize = nBytes;
                        }
                        msgPtr.dataPtr = nativeMem; */
                        msgPtr.dataPtr = Marshal.AllocHGlobal((int)nBytes);
                        Marshal.Copy(msg.dataPtr, 0, msgPtr.dataPtr, (int)nBytes);
                    }
                    IntPtr dummy = (IntPtr)0;
                    msgNativeDestFunction(ref dummy, ref msgPtr);	//will take a copy
                }
                finally
                {
                    if (nBytes > 0)
                        Marshal.FreeHGlobal(msgPtr.dataPtr);
                }
            }
            catch (SEHException)
            { // This is the most likely exception to be caught here, but normally it will
            } // be handled elsewhere on the native side, and "External component has thrown an exception" is not a very helpful message
            catch (Exception e)
            {
                StringBuilder exmsg = new StringBuilder("sendMessageToEngine() failed ");
                exmsg.Append(e.Message);
                Console.WriteLine(exmsg);
            }
        }

        //private uint memAllocSize = 0;
        //private IntPtr nativeMem;

        //==============================================================================
        /// <summary>
        /// Send a message up to the owning system so it can be routed throughout
        /// the simulation. Overridden to allow the message data to be converted
        /// to a native pointer. This enables calling from embedded Microsoft Frameworks or Mono.
        /// The message itself then needs to be converted from a TNativeMsgHeader to a TMsgHeader,
        /// which includes taking a copy of the data referenced by the data pointer
        /// </summary>
        /// <param name="inVal">Message that will be sent to the engine.
        /// The value passed in, although described as a "ulong", is actually
        /// a pointer to a native TMsgHeader
        /// </param>
        //==============================================================================
        public void handleMessage(ulong inVal)
        {
            // We need to copy the "native" message point into a managed object.
            TNativeMsgHeader src = (TNativeMsgHeader)Marshal.PtrToStructure((IntPtr)inVal, typeof(TNativeMsgHeader));

            TMsgHeader msgPtr = TMessageInterpreter.NativeMsgToManagedMsg(ref src);

            handleMessage(msgPtr);  //calls the base class function
        }
    }
}