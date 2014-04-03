Imports System
Imports System.Text
Imports System.IO
Imports System.Reflection
Imports System.Runtime.InteropServices
Imports CMPServices

Namespace CMPComp
    ''''=========================================================================
    ''' <summary>
    ''' This class provides a standard interface for .NET components.
    ''' You can create an instance of this class if you are loading this
    ''' component from the .NET world.
    ''' </summary>
    '''=========================================================================
    <ComVisible(True)> _
    Public Class TComponentInstance
        Inherits TAPSIMHost
        ' Get Type from the the "Product" attribute of the assembly; use the DLL name if this is absent
        ' Get Version from the assembly version number's major and minor components 
        ' Get Author from the "Company" attribute of the assembly; use "APSIM Initiative" if this is absent
        Private Shared _STYPE As [String] = If(Assembly.GetExecutingAssembly().GetCustomAttributes(GetType(AssemblyProductAttribute), False).Length > 0, _
                                            CType(Assembly.GetExecutingAssembly().GetCustomAttributes(GetType(AssemblyProductAttribute), False)(0), AssemblyProductAttribute).Product, _
                                            Path.GetFileNameWithoutExtension(Assembly.GetExecutingAssembly().GetName().Name))
        Private Shared _SVERSION As [String] = Assembly.GetExecutingAssembly().GetName().Version.Major.ToString() + "." + _
                                               Assembly.GetExecutingAssembly().GetName().Version.Minor.ToString()
        Private Shared _SAUTHOR As [String] = If(Assembly.GetExecutingAssembly().GetCustomAttributes(GetType(AssemblyCompanyAttribute), False).Length > 0, _
                                              CType(Assembly.GetExecutingAssembly().GetCustomAttributes(GetType(AssemblyCompanyAttribute), False)(0), AssemblyCompanyAttribute).Company, _
                                              "APSIM Initiative")
        ''' =========================================================================
        ''' <summary>
        ''' Create an instance of a component here
        ''' </summary>
        ''' <param name="compID"></param>
        ''' <param name="parentCompID"></param>
        ''' <param name="messageCallback"></param>
        ''' =========================================================================
        Public Sub New(compID As UInteger, parentCompID As UInteger, messageCallback As MessageFromLogic)
            MyBase.New(compID, parentCompID, messageCallback, _STYPE, _SVERSION, _SAUTHOR)
        End Sub
    End Class

    '============================================================================
    ' Component developers should not need to change the code below this point.
    ' It exists to provide a wrapping class which enables the component to be
    ' loaded and used by native applications.
    '============================================================================

    <ComVisible(True)> _
    Public Class TGCComponent
        Inherits TComponentInstance

        ''' <summary>
        ''' This class is a thin wrapper over the TComponentInstance class, allowing
        ''' the caller to provide the callback as a function pointer rather than as
        ''' a delegate. This makes it relative easy to call the constructor from
        ''' unmanaged code, without requiring mixed-mode dlls.
        ''' </summary>
        ''' <param name="compID"></param>
        ''' <param name="parentCompID"></param>
        ''' <param name="messageCallback"></param>
        ''' <remarks>
        ''' ============================================================================
        ''' Note that we are assuming that an "IntPtr" can be stored within a ULong value. 
        ''' This value is cast to an IntPtr, then mashalled to a delegate
        ''' ============================================================================
        ''' </remarks>
        Public Sub New(compID As UInteger, parentCompID As UInteger, messageCallback As ULong)
            MyBase.New(compID, parentCompID, If(messageCallback = 0, Nothing, DirectCast(Marshal.GetDelegateForFunctionPointer(TMessageInterpreter.PtrCast(messageCallback), GetType(MessageFromLogic)), MessageFromLogic)))
            If messageCallback <> 0 Then
                msgNativeDestFunction = DirectCast(Marshal.GetDelegateForFunctionPointer(TMessageInterpreter.PtrCast(messageCallback), GetType(NativeMessageFromLogic)), NativeMessageFromLogic)
            End If
        End Sub

        '[UnmanagedFunctionPointer(CallingConvention.StdCall)]
        ''' =========================================================================
        ''' <summary>
        ''' Define type for the native MessageFromLogic function
        ''' </summary>
        ''' =========================================================================
        Public Delegate Sub NativeMessageFromLogic(ByRef dummy As IntPtr, ByRef inMsg As TNativeMsgHeader)
        ''' =========================================================================
        ''' <summary>
        ''' Define a delegate (function pointer) for MessageFromLogic
        ''' </summary>
        ''' =========================================================================
        Protected msgNativeDestFunction As NativeMessageFromLogic = Nothing

        ''' ==============================================================================
        ''' <summary>
        ''' Send a message up to the owning system so it can be routed throughout
        ''' the simulation. Overridden to allow the message data to be converted
        ''' to a native pointer.
        ''' <param name="msg">Message that will be sent to the engine.</param>
        ''' </summary>
        '''==============================================================================
        Protected Overrides Sub sendMessageToEngine(msg As TMsgHeader)
            ' Copy from TMsgHeader to TNativeMsgHeader.
            ' The difference is in the nature of the data pointer.
            Try
                Dim msgPtr As New TNativeMsgHeader()
                Dim nBytes As UInteger = msg.nDataBytes
                Try
                    msgPtr.version = msg.version
                    msgPtr.msgType = msg.msgType
                    msgPtr.from = msg.from
                    msgPtr.[to] = msg.[to]
                    msgPtr.msgID = msg.msgID
                    msgPtr.toAck = msg.toAck
                    msgPtr.nDataBytes = nBytes

                    If nBytes = 0 Then
                        msgPtr.dataPtr = IntPtr.Zero
                    Else
                        msgPtr.dataPtr = Marshal.AllocHGlobal(CInt(nBytes))
                        Marshal.Copy(msg.dataPtr, 0, msgPtr.dataPtr, CInt(nBytes))
                    End If
                    Dim dummy As IntPtr = CType(0, IntPtr)
                    'will take a copy
                    msgNativeDestFunction(dummy, msgPtr)
                Finally
                    If nBytes > 0 Then
                        Marshal.FreeHGlobal(msgPtr.dataPtr)
                    End If
                End Try
            Catch exc As SEHException
                ' This is the most likely exception to be caught here, but normally it will
                ' be handled elsewhere on the native side, and "External component has thrown an exception" is not a very helpful message
            Catch e As Exception
                Dim exmsg As New StringBuilder("sendMessageToEngine() failed ")
                exmsg.Append(e.Message)
                Console.WriteLine(exmsg)
            End Try
        End Sub

        ''' ==============================================================================
        ''' <summary>
        ''' Send a message up to the owning system so it can be routed throughout
        ''' the simulation. Overridden to allow the message data to be converted
        ''' to a native pointer. This enables calling from embedded Microsoft Frameworks or Mono.
        ''' The message itself then needs to be converted from a TNativeMsgHeader to a TMsgHeader,
        ''' which includes taking a copy of the data referenced by the data pointer
        ''' </summary>
        ''' <param name="inVal">Pointer to message that will be sent to the engine.
        ''' </param>
        ''' <remark>
        ''' The value passed in, although described as a "ULong", is actually
        ''' a pointer to a native TMsgHeader
        ''' </remark>
        ''' ==============================================================================
        Public Overloads Sub handleMessage(inVal As ULong)
            ' We need to copy the "native" message point into a managed object.
            Dim src As TNativeMsgHeader = DirectCast(Marshal.PtrToStructure(TMessageInterpreter.PtrCast(inVal), GetType(TNativeMsgHeader)), TNativeMsgHeader)

            Dim msgPtr As TMsgHeader = TMessageInterpreter.NativeMsgToManagedMsg(src)

            handleMessage(msgPtr)
            'calls the base class function
        End Sub


    End Class

End Namespace
