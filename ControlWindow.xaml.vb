Imports System.Runtime.InteropServices

Public Class ControlWindow

    <DllImport("user32.dll")>
    Private Shared Function SetForegroundWindow(hWnd As IntPtr) As Boolean
    End Function

    <DllImport("user32.dll")>
    Private Shared Function FindWindow(lpClassName As String, lpWindowName As String) As IntPtr
    End Function

    Private Sub Window_MouseLeftButtonDown(sender As Object, e As MouseButtonEventArgs)
        ' Allow dragging the borderless window
        Me.DragMove()
    End Sub

    Private Sub btnClose_Click(sender As Object, e As RoutedEventArgs)
        Me.Close()
    End Sub

    Private Sub btnEditMode_Click(sender As Object, e As RoutedEventArgs)
        ' Get MainWindow and make it editable
        Dim mainWin = TryCast(Application.Current.MainWindow, MainWindow)
        If mainWin IsNot Nothing Then
            mainWin.SetEditMode(True)
        End If
    End Sub

    Private Sub btnNormalMode_Click(sender As Object, e As RoutedEventArgs)
        ' Get MainWindow and make it click-through
        Dim mainWin = TryCast(Application.Current.MainWindow, MainWindow)
        If mainWin IsNot Nothing Then
            mainWin.SetEditMode(False)
        End If

        ' Switch focus to LFS.exe window
        SwitchToLfsWindow()
    End Sub


    Private Sub SwitchToLfsWindow()
        ' Try to find and activate LFS window
        Dim lfsHandle As IntPtr = FindWindow(Nothing, "LFS")

        ' If not found by title "LFS", try finding the process window
        If lfsHandle = IntPtr.Zero Then
            Try
                Dim lfsProcess = Process.GetProcessesByName("LFS").FirstOrDefault()
                If lfsProcess IsNot Nothing Then
                    lfsHandle = lfsProcess.MainWindowHandle
                End If
            Catch
                ' Ignore errors if process not accessible
            End Try
        End If

        If lfsHandle <> IntPtr.Zero Then
            SetForegroundWindow(lfsHandle)
        End If
    End Sub

    Private Sub Window_Loaded(sender As Object, e As RoutedEventArgs)
        Dim mainWin = TryCast(Application.Current.MainWindow, MainWindow)
        If mainWin IsNot Nothing Then
            chkTime.IsChecked = mainWin.GetControlVisibility("timeGroupBox")
            chkSpeed.IsChecked = mainWin.GetControlVisibility("speedGroupBox")
            chkRadar.IsChecked = mainWin.GetControlVisibility("radarGroupBox")
        End If

        ' Load saved password
        LoadSavedPassword()
    End Sub

    Private Sub chkTime_Checked(sender As Object, e As RoutedEventArgs)
        UpdateVisibility("timeGroupBox", True)
    End Sub

    Private Sub chkTime_Unchecked(sender As Object, e As RoutedEventArgs)
        UpdateVisibility("timeGroupBox", False)
    End Sub

    Private Sub chkSpeed_Checked(sender As Object, e As RoutedEventArgs)
        UpdateVisibility("speedGroupBox", True)
    End Sub

    Private Sub chkSpeed_Unchecked(sender As Object, e As RoutedEventArgs)
        UpdateVisibility("speedGroupBox", False)
    End Sub

    Private Sub chkRadar_Checked(sender As Object, e As RoutedEventArgs)
        UpdateVisibility("radarGroupBox", True)
    End Sub

    Private Sub chkRadar_Unchecked(sender As Object, e As RoutedEventArgs)
        UpdateVisibility("radarGroupBox", False)
    End Sub

    Private Sub btnReconnect_Click(sender As Object, e As RoutedEventArgs)
        Dim mainWin = TryCast(Application.Current.MainWindow, MainWindow)
        If mainWin IsNot Nothing Then
            Dim password As String = txtPassword.Password
            mainWin.Connect(password)

            ' Save password after successful connection attempt
            SavePassword(password)
        End If
    End Sub

    Private Sub UpdateVisibility(controlName As String, isVisible As Boolean)
        Dim mainWin = TryCast(Application.Current.MainWindow, MainWindow)
        If mainWin IsNot Nothing Then
            mainWin.SetControlVisibility(controlName, isVisible)
        End If
    End Sub

    ''' <summary>
    ''' Saves the password to user settings.
    ''' </summary>
    Private Sub SavePassword(password As String)
        Try
            If My.Settings.InsimPassword Is Nothing Then
                My.Settings.InsimPassword = String.Empty
            End If
            My.Settings.InsimPassword = password
            My.Settings.Save()
        Catch ex As Exception
            ' Silently handle any save errors
        End Try
    End Sub

    ''' <summary>
    ''' Loads the saved password from user settings.
    ''' </summary>
    Private Sub LoadSavedPassword()
        Try
            If My.Settings.InsimPassword IsNot Nothing Then
                txtPassword.Password = My.Settings.InsimPassword
            End If
        Catch ex As Exception
            ' Silently handle any load errors
        End Try
    End Sub
End Class