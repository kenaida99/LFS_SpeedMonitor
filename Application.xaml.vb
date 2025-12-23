Class Application

    ' Application-level events, such as Startup, Exit, and DispatcherUnhandledException
    ' can be handled in this file.
    Protected Overrides Sub OnStartup(e As StartupEventArgs)
        MyBase.OnStartup(e)

        ' Show the main overlay window
        Dim mainWin As New MainWindow()
        mainWin.Show()

        ' Show the control window
        Dim controlWin As New ControlWindow()
        controlWin.Show()
        controlWin.WindowState = WindowState.Minimized
    End Sub
End Class
