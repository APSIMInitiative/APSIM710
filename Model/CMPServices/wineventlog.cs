using System;
using System.Collections.Generic;
using System.Text;

namespace CMPServices
{
    /// <summary>
    /// The TWinEventLog class is a helper class for adding new events to the window's event log
    /// </summary>
    public class TWinEventLog
    {

        /// <summary>
        /// Default Event Id for Information
        /// </summary>
        public const int EVENTID_INFORMATION = 2100;

        /// <summary>
        /// Default Event Id for Error
        /// </summary>
        public const int EVENTID_ERROR = 2200;

        /// <summary>
        /// Default Event Id for Warning
        /// </summary>
        public const int EVENTID_WARNING = 2300;

        /// <summary>
        /// Default Event Id for Success
        /// </summary>
        public const int EVENTID_SUCCESS = 2400;

        /// <summary>
        /// Default Event Id for Failure
        /// </summary>
        public const int EVENTID_FAILURE = 2500;


        /// <summary>
        /// Add a window event - Information
        /// </summary>
        /// <param name="source">Application eg AusFarm</param>
        /// <param name="msg">Event Msg</param>
        public static void addWinEventInformation(string source, string msg)
        {
            addWinEventInformation(source, msg, EVENTID_INFORMATION);
        }

        /// <summary>
        /// Add a window event - Information
        /// </summary>
        /// <param name="source">Application eg AusFarm</param>
        /// <param name="msg">Event Msg</param>
        /// <param name="eventID">Event Id</param>
        public static void addWinEventInformation(string source, string msg, int eventID)
        {
            addWinEvent(source, msg, System.Diagnostics.EventLogEntryType.Information, eventID);
        }

        /// <summary>
        /// Add a window event - Error
        /// </summary>
        /// <param name="source">Application eg AusFarm</param>
        /// <param name="msg">Event Msg</param>
        public static void addWinEventError(string source, string msg)
        {
            addWinEventError(source, msg, EVENTID_ERROR);
        }

        /// <summary>
        /// Add a window event - Error
        /// </summary>
        /// <param name="source">Application eg AusFarm</param>
        /// <param name="msg">Event Msg</param>
        /// <param name="eventID">Event Id</param>
        public static void addWinEventError(string source, string msg, int eventID)
        {
            addWinEvent(source, msg, System.Diagnostics.EventLogEntryType.Error, eventID);
        }

        /// <summary>
        /// Add a window event - Warning
        /// </summary>
        /// <param name="source">Application eg AusFarm</param>
        /// <param name="msg">Event Msg</param>
        public static void addWinEventWarning(string source, string msg)
        {
            addWinEventWarning(source, msg, EVENTID_WARNING);
        }

        /// <summary>
        /// Add a window event - Warning
        /// </summary>
        /// <param name="source">Application eg AusFarm</param>
        /// <param name="msg">Event Msg</param>
        /// <param name="eventID">Event Id</param>
        public static void addWinEventWarning(string source, string msg, int eventID)
        {
            addWinEvent(source, msg, System.Diagnostics.EventLogEntryType.Warning, eventID);
        }


        /// <summary>
        /// Add a window event - Success
        /// </summary>
        /// <param name="source">Application eg AusFarm</param>
        /// <param name="msg">Event Msg</param>
        public static void addWinEventSuccess(string source, string msg)
        {
            addWinEventSuccess(source, msg, EVENTID_SUCCESS );
        }


        /// <summary>
        /// Add a window event - Success
        /// </summary>
        /// <param name="source">Application eg AusFarm</param>
        /// <param name="msg">Event Msg</param>
        /// <param name="eventID">Event Id</param>
        public static void addWinEventSuccess(string source, string msg, int eventID)
        {
            addWinEvent(source, msg, System.Diagnostics.EventLogEntryType.SuccessAudit, eventID);
        }


        /// <summary>
        /// Add a window event - failure
        /// </summary>
        /// <param name="source">Application eg AusFarm</param>
        /// <param name="msg">Event Msg</param>
        public static void addWinEventFailure(string source, string msg)
        {
            addWinEventFailure(source, msg, EVENTID_FAILURE);
        }


        /// <summary>
        /// Add a window event - failure
        /// </summary>
        /// <param name="source">Application eg AusFarm</param>
        /// <param name="msg">Event Msg</param>
        /// <param name="eventID">Event Id</param>
        public static void addWinEventFailure(string source, string msg, int eventID)
        {
            addWinEvent(source, msg, System.Diagnostics.EventLogEntryType.FailureAudit, eventID);
        }

        /// <summary>
        /// Add a window event
        /// </summary>
        /// <param name="source">Application eg AusFarm</param>
        /// <param name="msg">Event Msg</param>
        /// <param name="eventLogType">Event Log Type, eg Success, Fail, Warning</param>
        /// <param name="eventID"></param>
        public static void addWinEvent(string source, string msg, System.Diagnostics.EventLogEntryType eventLogType, int eventID )
        {
            System.Diagnostics.EventLog.WriteEntry(source, msg, eventLogType, eventID);
        }
    }
}
