// ------------------------------------------------------------------
//    executes a program and optionally waits until it has finished.
//    Returns true if program was successfully executed.  NOTE:  This
//    routine is NOT THREAD SAFE.  Wrap inside a Synchronize method call
//    if being called from a thread.
// ------------------------------------------------------------------
bool Exec(const char* Command_line,
          unsigned int Show_flag,
          bool Wait_for_finish);

