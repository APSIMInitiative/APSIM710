Imports System
Imports System.Text
Imports System.Reflection
Imports System.Runtime.InteropServices
Imports CMPServices

Namespace CMPComp
    ''''=========================================================================
    ''' <summary>
    ''' </summary>
    '''=========================================================================
    <ComVisible(True)> _
    Public Class TComponentInstance
        Inherits TAPSIMHost
        Private Shared _STYPE As [String] = "DDRules"
        Private Shared _SVERSION As [String] = "1.0"
        Private Shared _SAUTHOR As [String] = "APSIM"

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
        ''' Note that we are assuming that an "IntPtr" can be stored within a uint
        ''' value. That is, we're assuming the native caller is 32-bit.
        ''' This value is cast to an IntPtr, then mashalled to a delegate
        ''' ============================================================================
        ''' </remarks>
        Public Sub New(compID As UInteger, parentCompID As UInteger, messageCallback As UInteger)
            MyBase.New(compID, parentCompID, If(messageCallback = 0, Nothing, DirectCast(Marshal.GetDelegateForFunctionPointer(TMessageInterpreter.PtrCast(messageCallback), GetType(MessageFromLogic)), MessageFromLogic)))
            If messageCallback <> 0 Then
                msgNativeDestFunction = DirectCast(Marshal.GetDelegateForFunctionPointer(TMessageInterpreter.PtrCast(messageCallback), GetType(NativeMessageFromLogic)), NativeMessageFromLogic)
            End If
        End Sub

        '[UnmanagedFunctionPointer(CallingConvention.StdCall)]
        Public Delegate Sub NativeMessageFromLogic(ByRef dummy As UInteger, ByRef inMsg As TNativeMsgHeader)
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
                    Dim dummy As UInteger = 0
                    'will take a copy
                    msgNativeDestFunction(dummy, msgPtr)
                Finally
                    If nBytes > 0 Then
                        Marshal.FreeHGlobal(msgPtr.dataPtr)
                    End If
                End Try
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
        ''' The value passed in, although described as a "uint", is actually
        ''' a pointer to a native TMsgHeader
        ''' </remark>
        ''' ==============================================================================
        Public Overloads Sub handleMessage(inVal As UInteger)
            ' We need to copy the "native" message point into a managed object.
            Dim src As TNativeMsgHeader = DirectCast(Marshal.PtrToStructure(TMessageInterpreter.PtrCast(inVal), GetType(TNativeMsgHeader)), TNativeMsgHeader)

            Dim msgPtr As TMsgHeader = TMessageInterpreter.NativeMsgToManagedMsg(src)

            handleMessage(msgPtr)
            'calls the base class function
        End Sub


    End Class

End Namespace
