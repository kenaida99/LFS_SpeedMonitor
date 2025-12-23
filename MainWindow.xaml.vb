Imports InSimDotNet
Imports InSimDotNet.Packets
Imports System.Runtime.InteropServices
Imports System.Windows.Interop

Class MainWindow
    Private WithEvents _insim As InSim
    Private _nodeData As New Dictionary(Of Integer, NodeInfo)  ' Best lap data per node
    Private _currentLapData As New Dictionary(Of Integer, NodeInfo)  ' Current lap data per node
    Private _currentLap As Integer = 0
    Private _playerPLID As Byte = 0
    Private _localUCID As Byte = 0  ' Track local connection ID
    Private _lapStopwatch As New Stopwatch()
    Private _lfsFolder As String = String.Empty  ' LFS installation folder
    <DllImport("user32.dll")>
    Private Shared Function SetForegroundWindow(hWnd As IntPtr) As Boolean
    End Function

    <DllImport("user32.dll")>
    Private Shared Function FindWindow(lpClassName As String, lpWindowName As String) As IntPtr
    End Function
    ' Drag support
    Private _isDragging As Boolean = False
    Private _dragStart As Point
    Private _dragElement As UIElement = Nothing

    ' Click-through state
    Private _isClickThrough As Boolean = True

    ' Settings file path
    Private ReadOnly _settingsFilePath As String = System.IO.Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "ui_positions.json")
    Private ReadOnly _visibilityFilePath As String = System.IO.Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "ui_visibility.json")

#Region "Click-Through Window Interop"
    Private Const GWL_EXSTYLE As Integer = -20
    Private Const WS_EX_TRANSPARENT As Integer = &H20
    Private Const WS_EX_LAYERED As Integer = &H80000

    <DllImport("user32.dll")>
    Private Shared Function GetWindowLong(hWnd As IntPtr, nIndex As Integer) As Integer
    End Function

    <DllImport("user32.dll")>
    Private Shared Function SetWindowLong(hWnd As IntPtr, nIndex As Integer, dwNewLong As Integer) As Integer
    End Function

    Private Sub MakeWindowClickThrough()
        Dim hwnd As IntPtr = New WindowInteropHelper(Me).Handle
        Dim extendedStyle As Integer = GetWindowLong(hwnd, GWL_EXSTYLE)
        SetWindowLong(hwnd, GWL_EXSTYLE, extendedStyle Or WS_EX_TRANSPARENT Or WS_EX_LAYERED)
        _isClickThrough = True
        UpdateBorderAppearance()
    End Sub

    Private Sub MakeWindowClickable()
        Dim hwnd As IntPtr = New WindowInteropHelper(Me).Handle
        Dim extendedStyle As Integer = GetWindowLong(hwnd, GWL_EXSTYLE)
        SetWindowLong(hwnd, GWL_EXSTYLE, extendedStyle And Not WS_EX_TRANSPARENT)
        _isClickThrough = False
        UpdateBorderAppearance()
    End Sub

    Private Sub ToggleClickThrough()
        If _isClickThrough Then
            MakeWindowClickable()
        Else
            MakeWindowClickThrough()
        End If
    End Sub

    Private Sub UpdateBorderAppearance()
        ' Visual feedback: brighter borders when editable, dimmer when click-through
        Dim brush As Brush = If(_isClickThrough, Brushes.Gray, Brushes.Cyan)
        timeGroupBox.BorderBrush = brush
        speedGroupBox.BorderBrush = brush
        infoBorder.BorderBrush = brush
        radarGroupBox.BorderBrush = brush

        ' Show/hide cursor based on click-through state
        Me.Cursor = If(_isClickThrough, Cursors.None, Cursors.Arrow)

        ' Show/hide the Finished Edits button based on edit mode
        btnFinishedEdits.Visibility = If(_isClickThrough, Visibility.Collapsed, Visibility.Visible)
    End Sub
#End Region

#Region "Position Persistence"
    Private Sub SaveElementPositions()
        Dim positions As New Dictionary(Of String, Double()) From {
            {"timeGroupBox", New Double() {Canvas.GetLeft(timeGroupBox), Canvas.GetTop(timeGroupBox)}},
            {"speedGroupBox", New Double() {Canvas.GetLeft(speedGroupBox), Canvas.GetTop(speedGroupBox)}},
            {"infoBorder", New Double() {Canvas.GetLeft(infoBorder), Canvas.GetTop(infoBorder)}},
            {"radarGroupBox", New Double() {Canvas.GetLeft(radarGroupBox), Canvas.GetTop(radarGroupBox)}}
        }

        Dim json As String = Newtonsoft.Json.JsonConvert.SerializeObject(positions)
        System.IO.File.WriteAllText(_settingsFilePath, json)
    End Sub

    Private Sub LoadElementPositions()
        If Not System.IO.File.Exists(_settingsFilePath) Then Return

        Try
            Dim json As String = System.IO.File.ReadAllText(_settingsFilePath)
            Dim positions = Newtonsoft.Json.JsonConvert.DeserializeObject(Of Dictionary(Of String, Double()))(json)

            If positions.ContainsKey("timeGroupBox") Then
                Canvas.SetLeft(timeGroupBox, positions("timeGroupBox")(0))
                Canvas.SetTop(timeGroupBox, positions("timeGroupBox")(1))
            End If

            If positions.ContainsKey("speedGroupBox") Then
                Canvas.SetLeft(speedGroupBox, positions("speedGroupBox")(0))
                Canvas.SetTop(speedGroupBox, positions("speedGroupBox")(1))
            End If

            If positions.ContainsKey("infoBorder") Then
                Canvas.SetLeft(infoBorder, positions("infoBorder")(0))
                Canvas.SetTop(infoBorder, positions("infoBorder")(1))
            End If

            If positions.ContainsKey("radarGroupBox") Then
                Canvas.SetLeft(radarGroupBox, positions("radarGroupBox")(0))
                Canvas.SetTop(radarGroupBox, positions("radarGroupBox")(1))
            End If
        Catch ex As Exception
            ' If loading fails, use default positions from XAML
        End Try
    End Sub
#End Region

#Region "Visibility Persistence"
    Public Sub SetControlVisibility(controlName As String, isVisible As Boolean)
        Dim visibility As Visibility = If(isVisible, Visibility.Visible, Visibility.Collapsed)

        Select Case controlName
            Case "timeGroupBox"
                timeGroupBox.Visibility = visibility
            Case "speedGroupBox"
                speedGroupBox.Visibility = visibility
            Case "radarGroupBox"
                radarGroupBox.Visibility = visibility
        End Select

        SaveVisibilitySettings()
    End Sub

    Public Function GetControlVisibility(controlName As String) As Boolean
        Select Case controlName
            Case "timeGroupBox"
                Return timeGroupBox.Visibility = Visibility.Visible
            Case "speedGroupBox"
                Return speedGroupBox.Visibility = Visibility.Visible
            Case "radarGroupBox"
                Return radarGroupBox.Visibility = Visibility.Visible
        End Select
        Return False
    End Function

    Private Sub SaveVisibilitySettings()
        Dim settings As New Dictionary(Of String, Boolean) From {
            {"timeGroupBox", timeGroupBox.Visibility = Visibility.Visible},
            {"speedGroupBox", speedGroupBox.Visibility = Visibility.Visible},
            {"radarGroupBox", radarGroupBox.Visibility = Visibility.Visible}
        }

        Dim json As String = Newtonsoft.Json.JsonConvert.SerializeObject(settings)
        System.IO.File.WriteAllText(_visibilityFilePath, json)
    End Sub

    Private Sub LoadVisibilitySettings()
        If Not System.IO.File.Exists(_visibilityFilePath) Then Return

        Try
            Dim json As String = System.IO.File.ReadAllText(_visibilityFilePath)
            Dim settings = Newtonsoft.Json.JsonConvert.DeserializeObject(Of Dictionary(Of String, Boolean))(json)

            If settings.ContainsKey("timeGroupBox") Then
                timeGroupBox.Visibility = If(settings("timeGroupBox"), Visibility.Visible, Visibility.Collapsed)
            End If

            If settings.ContainsKey("speedGroupBox") Then
                speedGroupBox.Visibility = If(settings("speedGroupBox"), Visibility.Visible, Visibility.Collapsed)
            End If

            If settings.ContainsKey("radarGroupBox") Then
                radarGroupBox.Visibility = If(settings("radarGroupBox"), Visibility.Visible, Visibility.Collapsed)
            End If
        Catch ex As Exception
        End Try
    End Sub
#End Region

#Region "Global Hotkey (CTRL+T)"
    Private Const HOTKEY_ID As Integer = 9000
    Private Const MOD_CONTROL As UInteger = &H2
    Private Const VK_T As UInteger = &H54

    <DllImport("user32.dll")>
    Private Shared Function RegisterHotKey(hWnd As IntPtr, id As Integer, fsModifiers As UInteger, vk As UInteger) As Boolean
    End Function

    <DllImport("user32.dll")>
    Private Shared Function UnregisterHotKey(hWnd As IntPtr, id As Integer) As Boolean
    End Function

    Private _hwndSource As HwndSource

    Private Sub RegisterGlobalHotkey()
        Dim hwnd As IntPtr = New WindowInteropHelper(Me).Handle
        _hwndSource = HwndSource.FromHwnd(hwnd)
        _hwndSource.AddHook(AddressOf HwndHook)

        If Not RegisterHotKey(hwnd, HOTKEY_ID, MOD_CONTROL, VK_T) Then
            MessageBox.Show("Failed to register hotkey CTRL+T. It may be in use by another application.",
                          "Hotkey Registration", MessageBoxButton.OK, MessageBoxImage.Warning)
        End If
    End Sub

    Private Sub UnregisterGlobalHotkey()
        Dim hwnd As IntPtr = New WindowInteropHelper(Me).Handle
        UnregisterHotKey(hwnd, HOTKEY_ID)
        _hwndSource?.RemoveHook(AddressOf HwndHook)
    End Sub

    Private Function HwndHook(hwnd As IntPtr, msg As Integer, wParam As IntPtr, lParam As IntPtr, ByRef handled As Boolean) As IntPtr
        Const WM_HOTKEY As Integer = &H312
        If msg = WM_HOTKEY AndAlso wParam.ToInt32() = HOTKEY_ID Then
            ToggleClickThrough()
            handled = True
        End If
        Return IntPtr.Zero
    End Function
#End Region

    Public Sub New()
        InitializeComponent()
        Connect()
    End Sub

    Protected Overrides Sub OnSourceInitialized(e As EventArgs)
        MyBase.OnSourceInitialized(e)
        ' Load saved element positions
        LoadElementPositions()
        ' Load saved visibility settings
        LoadVisibilitySettings()
        ' Register global hotkey
        RegisterGlobalHotkey()
        ' Make window click-through after it's fully initialized
        MakeWindowClickThrough()
    End Sub

#Region "Drag Support for Group Boxes"
    Private Sub GroupBox_MouseLeftButtonDown(sender As Object, e As MouseButtonEventArgs)
        _dragElement = CType(sender, UIElement)
        _isDragging = True
        _dragStart = e.GetPosition(mainCanvas)
        _dragElement.CaptureMouse()
        e.Handled = True
    End Sub

    Private Sub GroupBox_MouseLeftButtonUp(sender As Object, e As MouseButtonEventArgs)
        If _isDragging Then
            _isDragging = False
            _dragElement?.ReleaseMouseCapture()
            _dragElement = Nothing
            ' Save positions after drag ends
            SaveElementPositions()
        End If
        e.Handled = True
    End Sub

    Private Sub GroupBox_MouseMove(sender As Object, e As MouseEventArgs)
        If _isDragging AndAlso _dragElement IsNot Nothing Then
            Dim currentPos As Point = e.GetPosition(mainCanvas)
            Dim offsetX As Double = currentPos.X - _dragStart.X
            Dim offsetY As Double = currentPos.Y - _dragStart.Y

            Dim newLeft As Double = Canvas.GetLeft(_dragElement) + offsetX
            Dim newTop As Double = Canvas.GetTop(_dragElement) + offsetY

            Canvas.SetLeft(_dragElement, newLeft)
            Canvas.SetTop(_dragElement, newTop)

            _dragStart = currentPos
        End If
    End Sub
#End Region

    Sub RequestConnections()
        If _insim IsNot Nothing AndAlso _insim.IsConnected Then
            Dim req As New IS_TINY() With {
                .ReqI = 1,
                .SubT = TinyType.TINY_NCN
            }
            _insim.SendAsync(req)

            req.SubT = TinyType.TINY_NPL
            _insim.SendAsync(req)

            req.SubT = TinyType.TINY_SST
            _insim.SendAsync(req)
        End If
    End Sub

    Private Sub btnFinishedEdits_Click(sender As Object, e As RoutedEventArgs)
        ' Save positions and revert to click-through mode
        SaveElementPositions()
        MakeWindowClickThrough()

        ' Minimize the ControlWindow
        For Each win As Window In Application.Current.Windows
            If TypeOf win Is ControlWindow Then
                win.WindowState = WindowState.Minimized
                Exit For
            End If
        Next

        ' Switch focus back to LFS.exe
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
    ''' <summary>
    ''' Sets the edit mode from external windows (like ControlWindow).
    ''' </summary>
    ''' <param name="editMode">True for edit mode (clickable), False for normal mode (click-through).</param>
    Public Sub SetEditMode(editMode As Boolean)
        If editMode Then
            MakeWindowClickable()
        Else
            MakeWindowClickThrough()
        End If
    End Sub
    Private Function GetLfsFolderFromProcess() As String
        Try
            Dim lfsProcess = Process.GetProcessesByName("LFS").FirstOrDefault()
            If lfsProcess IsNot Nothing Then
                Dim exePath As String = lfsProcess.MainModule.FileName
                Return System.IO.Path.GetDirectoryName(exePath)
            End If
        Catch ex As Exception
            ' Process may have elevated privileges or be inaccessible
        End Try
        Return String.Empty
    End Function

    Private Function GetBestLapsFolder() As String
        If String.IsNullOrEmpty(_lfsFolder) Then
            Return System.IO.Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "BestLaps")
        End If
        Return System.IO.Path.Combine(_lfsFolder, "BestLaps")
    End Function

    Public Sub Connect(Optional password As String = "")
        If _insim IsNot Nothing AndAlso _insim.IsConnected Then
            _insim.Disconnect()
        End If

        Try
            _lfsFolder = GetLfsFolderFromProcess()
            _insim = New InSim()

            AddHandler _insim.PacketReceived, AddressOf PacketReceived

            _insim.Initialize(New InSimSettings() With {
                .Host = "127.0.0.1",
                .Port = 29999,
                .Admin = password,
                .Flags = InSimFlags.ISF_MCI Or InSimFlags.ISF_LOCAL,
                .Interval = 10
            })

            RequestConnections()
        Catch ex As Exception
            MessageBox.Show("Connection failed: " & vbCrLf & vbCrLf & "1) Make sure LFS is running first." & vbCrLf & "2) Make sure you type in /insim=29999" & vbCrLf & "3) Restart this app", "InSim Connection Error", MessageBoxButton.OK, MessageBoxImage.Error)
        End Try
    End Sub

    Private Sub btnConnect_Click(sender As Object, e As RoutedEventArgs)
        Connect()
    End Sub

    Private Sub PacketReceived(sender As Object, e As PacketEventArgs)
        Select Case e.Packet.Type
            Case PacketType.ISP_NCN
                NCN_Received(CType(e.Packet, IS_NCN))
            Case PacketType.ISP_NPL
                NPL_Received(CType(e.Packet, IS_NPL))
            Case PacketType.ISP_LAP
                LAP_Received(CType(e.Packet, IS_LAP))
            Case PacketType.ISP_MCI
                MCI_Received(CType(e.Packet, IS_MCI))
            Case PacketType.ISP_STA
                STA_Received(CType(e.Packet, IS_STA))
        End Select
    End Sub

    Private Sub NCN_Received(ncn As IS_NCN)
        If Not ncn.Remote OrElse ncn.UName = "kenaida99" Then
            _localUCID = ncn.UCID
        End If
    End Sub

    Public Property CurrentTrack As String = ""

    Private Sub STA_Received(sta As IS_STA)
        If sta.Track <> CurrentTrack Then
            _nodeData.Clear()
            _currentLapData.Clear()
            _currentLap = 0
            CurrentTrack = sta.Track
            LoadBestLapData(sta.Track)
        End If
    End Sub

    Private Sub LoadBestLapData(trackName As String)
        CurrentTrack = trackName
        Dim filePath As String = System.IO.Path.Combine(GetBestLapsFolder(), trackName & "_bestlap.json")
        If Not System.IO.File.Exists(filePath) Then
            Return
        End If
        _nodeData.Clear()
        Dim json = System.IO.File.ReadAllText(filePath)
        Dim lapData = Newtonsoft.Json.JsonConvert.DeserializeObject(Of List(Of NodeInfo))(json)

        For Each nodeInfo As NodeInfo In lapData
            _nodeData(nodeInfo.Node) = nodeInfo
        Next
    End Sub

    Private Sub NPL_Received(npl As IS_NPL)
        If npl.UCID = _localUCID Then
            _playerPLID = npl.PLID
            _currentLap = 0
            RestartLap()
        End If
    End Sub

    Private Sub LAP_Received(lap As IS_LAP)
        If lap.PLID = _playerPLID Then
            SaveBestLapData()
            _currentLap = lap.LapsDone
            RestartLap()
        End If
    End Sub

    Sub RestartLap()
        _currentLapData.Clear()
        _lapStopwatch.Restart()
    End Sub

    Public Property CurrentLapData As Dictionary(Of Integer, NodeInfo)
        Get
            Return _currentLapData
        End Get
        Set(value As Dictionary(Of Integer, NodeInfo))
            _currentLapData = value
        End Set
    End Property

    Function NodeAlreadyExists(node As Integer) As Boolean
        Return _currentLapData.ContainsKey(node)
    End Function

    Private Function SetNodeData(car As CompCar, tsTime As TimeSpan) As NodeInfo
        If NodeAlreadyExists(car.Node) Then
            Return _nodeData(car.Node)
        End If

        Dim speedMps As Double = Helpers.MathHelper.SpeedToKph(car.Speed)

        Dim nodeInfo As New NodeInfo() With {
            .Node = car.Node,
            .Speed = speedMps,
            .NodeTime = tsTime
        }

        _currentLapData(car.Node) = nodeInfo
        Return nodeInfo
    End Function



#Region "Radar Processing"
    Private Const LFS_POSITION_SCALE As Double = 65536 ' 65536 = 1 metre
    Private Const LFS_HEADING_SCALE As Double = 32768 ' 32768 = 180 degrees
    Private _radarRadius As Double = 50.0 ' Radius in meters to show on radar

    ''' <summary>
    ''' Processes MCI packet data and updates the proximity radar with nearby cars.
    ''' </summary>
    ''' <param name="mci">The MCI packet containing car positions.</param>
    Private Sub UpdateRadarFromMCI(mci As IS_MCI)
        ' Find the player's car data
        Try


            Dim playerCar As CompCar = Nothing
            For Each car As CompCar In mci.Info
                If car.PLID = _playerPLID Then
                    playerCar = car
                    Exit For
                End If
            Next

            ' If player not found in this packet, skip radar update
            If playerCar Is Nothing Then
                Return
            End If

            ' Get player position in meters
            Dim playerX As Double = playerCar.X / LFS_POSITION_SCALE
            Dim playerY As Double = playerCar.Y / LFS_POSITION_SCALE

            ' Get player heading in radians (0 = world Y direction, 32768 = 180 degrees)
            Dim playerHeading As Double = playerCar.Heading / LFS_HEADING_SCALE * Math.PI

            ' Track which cars are currently visible
            Dim visibleCarIds As New HashSet(Of Integer)

            ' Process all other cars
            For Each car As CompCar In mci.Info
                If car.PLID = _playerPLID OrElse car.PLID = 0 Then
                    Continue For
                End If

                ' Get car position in meters
                Dim carX As Double = car.X / LFS_POSITION_SCALE
                Dim carY As Double = car.Y / LFS_POSITION_SCALE

                ' Calculate relative position (world coordinates)
                Dim relX As Double = carX - playerX
                Dim relY As Double = carY - playerY

                ' Calculate distance
                Dim distance As Double = Math.Sqrt(relX * relX + relY * relY)

                ' Only process cars within radar range
                If distance <= _radarRadius Then
                    ' Rotate coordinates so player's heading is "up" on radar
                    ' We rotate by the negative of player heading
                    Dim cosH As Double = Math.Cos(-playerHeading)
                    Dim sinH As Double = Math.Sin(-playerHeading)

                    ' Apply rotation: rotated position relative to player's forward direction
                    Dim rotatedX As Double = relX * cosH - relY * sinH
                    Dim rotatedY As Double = relX * sinH + relY * cosH

                    ' Calculate the other car's heading in radians
                    Dim otherCarHeading As Double = car.Heading / LFS_HEADING_SCALE * Math.PI

                    ' Calculate relative heading (player heading - other car heading)
                    ' LFS heading is CCW, WPF rotation is CW, so we invert the difference
                    Dim relativeHeading As Double = playerHeading - otherCarHeading

                    ' Determine if car is ahead in race position
                    Dim isAhead As Boolean? = Nothing
                    If car.Position > 0 AndAlso playerCar.Position > 0 Then
                        isAhead = car.Position < playerCar.Position
                    End If

                    visibleCarIds.Add(car.PLID)

                    ' Update UI on dispatcher thread
                    Dim carId As Integer = car.PLID
                    Dim finalX As Double = rotatedX
                    Dim finalY As Double = rotatedY
                    Dim finalRelativeHeading As Double = relativeHeading
                    Dim ahead As Boolean? = isAhead

                    Dispatcher.BeginInvoke(Sub()
                                               proximityRadar.UpdateCar(carId, finalX, finalY, finalRelativeHeading, ahead)
                                           End Sub)

                End If
            Next

            ' Remove cars that are no longer in range (on UI thread)
            Dispatcher.BeginInvoke(Sub()
                                       RemoveCarsNotInSet(visibleCarIds)
                                   End Sub)
        Catch ex As Exception
            Console.WriteLine("MCI: " & ex.Message)
        End Try
    End Sub



    ' Track cars currently on radar for cleanup
    Private _carsOnRadar As New HashSet(Of Integer)

    ''' <summary>
    ''' Removes cars from the radar that are no longer visible.
    ''' </summary>
    Private Sub RemoveCarsNotInSet(visibleCarIds As HashSet(Of Integer))
        Dim toRemove As New List(Of Integer)

        For Each carId In _carsOnRadar
            If Not visibleCarIds.Contains(carId) Then
                toRemove.Add(carId)
                proximityRadar.RemoveCar(carId)
            End If
        Next

        For Each carId In toRemove
            _carsOnRadar.Remove(carId)
        Next

        ' Add new cars to tracking set
        For Each carId In visibleCarIds
            _carsOnRadar.Add(carId)
        Next
    End Sub
#End Region
    Private Sub MCI_Received(mci As IS_MCI)

        ' Update radar with all car positions
        UpdateRadarFromMCI(mci)

        For Each car As CompCar In mci.Info
            If car.PLID <> _playerPLID OrElse _playerPLID = 0 Then
                Continue For
            End If

            If NodeAlreadyExists(car.Node) Then Exit Sub

            Dim node As Integer = car.Node
            Dim ts As TimeSpan = _lapStopwatch.Elapsed
            Dim ThisNode As NodeInfo = SetNodeData(car, ts)

            Dim timeDiff As Double = 0
            Dim tsTimeDiff As TimeSpan
            Dim speedDiff As Double = 0

            If _nodeData.ContainsKey(node) Then
                Dim bestNode = _nodeData(node)
                tsTimeDiff = ThisNode.NodeTime - bestNode.NodeTime
                speedDiff = ThisNode.Speed - bestNode.Speed
            End If

            Dispatcher.BeginInvoke(Sub()
                                       timeGauge.UpdateValue(tsTimeDiff.TotalMilliseconds / 1000)
                                       speedGauge.UpdateValue(speedDiff)
                                       txtCurrentNode.Text = node.ToString()
                                       txtCurrentLap.Text = (_currentLap + 1).ToString()
                                       txtCurrentSpeed.Text = ThisNode.Speed.ToString("F1") & " km/h"
                                   End Sub)
        Next
    End Sub

    Private Sub SaveBestLapData()
        If _currentLapData.Count = 0 Then Return

        If _nodeData.Count = 0 Then
            For Each kvp In _currentLapData
                _nodeData(kvp.Key) = kvp.Value
            Next
        Else
            Dim currentMaxTime = _currentLapData.Values.Max(Function(n) n.NodeTime)
            Dim bestMaxTime = _nodeData.Values.Max(Function(n) n.NodeTime)

            If currentMaxTime < bestMaxTime Then
                _nodeData.Clear()
                For Each kvp In _currentLapData
                    _nodeData(kvp.Key) = kvp.Value
                Next
            Else
                Return
            End If
        End If

        Dim folderPath As String = GetBestLapsFolder()
        If Not System.IO.Directory.Exists(folderPath) Then
            System.IO.Directory.CreateDirectory(folderPath)
        End If

        Dim filePath As String = System.IO.Path.Combine(folderPath, CurrentTrack & "_bestlap.json")
        Dim json As String = Newtonsoft.Json.JsonConvert.SerializeObject(_nodeData.Values.ToList())
        System.IO.File.WriteAllText(filePath, json)
    End Sub

    Protected Overrides Sub OnClosed(e As EventArgs)
        MyBase.OnClosed(e)
        UnregisterGlobalHotkey()
        If _insim IsNot Nothing AndAlso _insim.IsConnected Then
            _insim.Disconnect()
        End If
    End Sub

    Private Sub btnClose_Click(sender As Object, e As RoutedEventArgs)
        Me.Close()
    End Sub
End Class

Public Class NodeInfo
    Public Property Speed As Double
    Public Property NodeTime As TimeSpan
    Public Property Node As Integer
End Class