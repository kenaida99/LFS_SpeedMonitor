Imports System.Windows.Media
Imports System.Windows.Shapes
Imports InSimDotNet.Packets

Public Class RadarControl
    Private ReadOnly CarDots As New Dictionary(Of Integer, Rectangle)
    Private ReadOnly CarLabels As New Dictionary(Of Integer, TextBlock)
    Private ReadOnly DefaultCarColor As SolidColorBrush = New SolidColorBrush(Color.FromRgb(0, 0, 0)) ' white
    Private ReadOnly AheadCarColor As SolidColorBrush = New SolidColorBrush(Color.FromRgb(0, 0, 0)) ' white
    Private ReadOnly BehindCarColor As SolidColorBrush = New SolidColorBrush(Color.FromRgb(0, 0, 0)) ' white
    Private ReadOnly ProximityWarningColor As SolidColorBrush = New SolidColorBrush(Color.FromRgb(255, 0, 0)) ' red

    ''' <summary>
    ''' Standard saloon car length in meters.
    ''' </summary>
    Private Const CarLength As Double = 4.5

    ''' <summary>
    ''' Standard saloon car width in meters.
    ''' </summary>
    Private Const CarWidth As Double = 1.8

    ''' <summary>
    ''' The maximum range in meters that the radar displays.
    ''' Cars beyond this distance won't appear on the radar.
    ''' </summary>
    Public Property MaxRange As Double = 100.0

    ''' <summary>
    ''' The proximity warning radius in meters. Cars within this distance turn red.
    ''' This is measured from the closest edge of each car, not center-to-center.
    ''' </summary>
    Public Property ProximityWarningRadius As Double = 8.0

    ''' <summary>
    ''' Size of car dots on the radar.
    ''' </summary>
    Public Property CarDotSize As Double = 8.0

    ''' <summary>
    ''' Gets the radar radius in pixels.
    ''' </summary>
    Private ReadOnly Property RadarRadius As Double
        Get
            Return radarBackground.Width / 2
        End Get
    End Property

    ''' <summary>
    ''' Gets the center point of the radar canvas.
    ''' </summary>
    Private ReadOnly Property RadarCenter As Double
        Get
            Return carCanvas.Width / 2
        End Get
    End Property

    Private _warningCars As New HashSet(Of Integer)
    Private Const NormalOpacity As Double = 0.15
    Private Const WarningOpacity As Double = 1.0

    Public Sub New()
        InitializeComponent()
        UpdateOpacity()
    End Sub

    Private Sub UpdateOpacity()
        Dim newOpacity As Double = If(_warningCars.Count > 0, WarningOpacity, NormalOpacity)
        If carCanvas.Opacity <> newOpacity Then
            carCanvas.Opacity = newOpacity
            playerDot.Opacity = newOpacity
        End If
    End Sub

    ''' <summary>
    ''' Gets the four corner points of a car rectangle given its center position and heading.
    ''' </summary>
    ''' <param name="centerX">X position of car center.</param>
    ''' <param name="centerY">Y position of car center.</param>
    ''' <param name="headingRadians">Car heading in radians (0 = facing up/north).</param>
    ''' <returns>Array of 4 corner points.</returns>
    Private Function GetCarCorners(centerX As Double, centerY As Double, headingRadians As Double) As Point()
        Dim halfLength As Double = CarLength / 2
        Dim halfWidth As Double = CarWidth / 2

        ' Local corner positions (before rotation)
        Dim corners As Point() = {
            New Point(-halfWidth, halfLength),   ' Front-left
            New Point(halfWidth, halfLength),    ' Front-right
            New Point(halfWidth, -halfLength),   ' Rear-right
            New Point(-halfWidth, -halfLength)   ' Rear-left
        }

        ' Rotate and translate each corner
        Dim cosH As Double = Math.Cos(headingRadians)
        Dim sinH As Double = Math.Sin(headingRadians)

        For i As Integer = 0 To 3
            Dim x As Double = corners(i).X
            Dim y As Double = corners(i).Y
            corners(i) = New Point(
                centerX + (x * cosH - y * sinH),
                centerY + (x * sinH + y * cosH)
            )
        Next

        Return corners
    End Function

    ''' <summary>
    ''' Calculates the minimum distance from a point to a line segment.
    ''' </summary>
    Private Function PointToSegmentDistance(p As Point, a As Point, b As Point) As Double
        Dim abX As Double = b.X - a.X
        Dim abY As Double = b.Y - a.Y
        Dim apX As Double = p.X - a.X
        Dim apY As Double = p.Y - a.Y

        Dim ab2 As Double = abX * abX + abY * abY
        If ab2 = 0 Then Return Math.Sqrt(apX * apX + apY * apY)

        Dim t As Double = Math.Max(0, Math.Min(1, (apX * abX + apY * abY) / ab2))
        Dim projX As Double = a.X + t * abX
        Dim projY As Double = a.Y + t * abY

        Dim dx As Double = p.X - projX
        Dim dy As Double = p.Y - projY
        Return Math.Sqrt(dx * dx + dy * dy)
    End Function

    ''' <summary>
    ''' Checks if two line segments intersect.
    ''' </summary>
    Private Function SegmentsIntersect(a1 As Point, a2 As Point, b1 As Point, b2 As Point) As Boolean
        Dim d1 As Double = CrossProduct(b2.X - b1.X, b2.Y - b1.Y, a1.X - b1.X, a1.Y - b1.Y)
        Dim d2 As Double = CrossProduct(b2.X - b1.X, b2.Y - b1.Y, a2.X - b1.X, a2.Y - b1.Y)
        Dim d3 As Double = CrossProduct(a2.X - a1.X, a2.Y - a1.Y, b1.X - a1.X, b1.Y - a1.Y)
        Dim d4 As Double = CrossProduct(a2.X - a1.X, a2.Y - a1.Y, b2.X - a1.X, b2.Y - a1.Y)

        If ((d1 > 0 AndAlso d2 < 0) OrElse (d1 < 0 AndAlso d2 > 0)) AndAlso
           ((d3 > 0 AndAlso d4 < 0) OrElse (d3 < 0 AndAlso d4 > 0)) Then
            Return True
        End If

        Return False
    End Function

    Private Function CrossProduct(x1 As Double, y1 As Double, x2 As Double, y2 As Double) As Double
        Return x1 * y2 - y1 * x2
    End Function

    ''' <summary>
    ''' Calculates the minimum edge-to-edge distance between two rotated car rectangles.
    ''' </summary>
    ''' <param name="relativeX">X distance between car centers (meters).</param>
    ''' <param name="relativeY">Y distance between car centers (meters).</param>
    ''' <param name="relativeHeading">Heading difference in radians (other car heading - player heading).</param>
    ''' <returns>The shortest distance between the perimeters of the two cars.</returns>
    Private Function CalculateEdgeToEdgeDistance(relativeX As Double, relativeY As Double, relativeHeading As Double) As Double
        ' Player car is at origin, facing up (heading = 0)
        Dim playerCorners As Point() = GetCarCorners(0, 0, 0)

        ' Other car is at relative position with relative heading
        Dim otherCorners As Point() = GetCarCorners(relativeX, relativeY, relativeHeading)

        ' Check if any edges intersect (cars are overlapping)
        For i As Integer = 0 To 3
            Dim pA As Point = playerCorners(i)
            Dim pB As Point = playerCorners((i + 1) Mod 4)
            For j As Integer = 0 To 3
                Dim oA As Point = otherCorners(j)
                Dim oB As Point = otherCorners((j + 1) Mod 4)
                If SegmentsIntersect(pA, pB, oA, oB) Then
                    Return 0 ' Cars are overlapping
                End If
            Next
        Next

        ' Find minimum distance between all corner-to-edge combinations
        Dim minDistance As Double = Double.MaxValue

        ' Check distance from each player corner to each other car edge
        For Each pCorner In playerCorners
            For j As Integer = 0 To 3
                Dim oA As Point = otherCorners(j)
                Dim oB As Point = otherCorners((j + 1) Mod 4)
                Dim dist As Double = PointToSegmentDistance(pCorner, oA, oB)
                minDistance = Math.Min(minDistance, dist)
            Next
        Next

        ' Check distance from each other car corner to each player edge
        For Each oCorner In otherCorners
            For i As Integer = 0 To 3
                Dim pA As Point = playerCorners(i)
                Dim pB As Point = playerCorners((i + 1) Mod 4)
                Dim dist As Double = PointToSegmentDistance(oCorner, pA, pB)
                minDistance = Math.Min(minDistance, dist)
            Next
        Next

        Return minDistance
    End Function

    ''' <summary>
    ''' Updates the visibility of the player's car rectangle based on nearby cars.
    ''' </summary>
    Private Sub UpdatePlayerCarVisibility()
        ' Show player car only when other cars are within radar range
        playerDot.Visibility = If(CarDots.Count > 0, Visibility.Visible, Visibility.Collapsed)
    End Sub

    ''' <summary>
    ''' Updates or adds a car to the radar display.
    ''' </summary>
    ''' <param name="carId">Unique identifier for the car.</param>
    ''' <param name="relativeX">X position relative to player (meters, positive = right).</param>
    ''' <param name="relativeY">Y position relative to player (meters, positive = ahead).</param>
    ''' <param name="relativeHeading">Heading difference in radians (other car heading - player heading). 0 = same direction.</param>
    ''' <param name="isAhead">True if the car is ahead in race position.</param>
    Public Sub UpdateCar(carId As Integer, relativeX As Double, relativeY As Double, Optional relativeHeading As Double = 0, Optional isAhead As Boolean? = Nothing)
        ' Calculate center-to-center distance from player (used for range check)
        Dim centerDistance As Double = Math.Sqrt(relativeX * relativeX + relativeY * relativeY)

        ' Check if car is within radar range
        If centerDistance > MaxRange Then
            RemoveCar(carId)
            Return
        End If

        ' Calculate edge-to-edge distance for proximity warning (accounting for rotation)
        Dim edgeDistance As Double = CalculateEdgeToEdgeDistance(relativeX, relativeY, relativeHeading)

        ' Scale position to radar coordinates
        Dim scale As Double = RadarRadius / MaxRange

        ' Visual adjustments to make markers more visible and fix intersection asymmetry
        Dim visualWidthFactor As Double = 1.3
        Dim lateralSpacingFactor As Double = 1.3

        ' Calculate scaled car dimensions for radar display
        ' Use CarDotSize as minimum width
        Dim scaledCarWidth As Double = Math.Max(CarWidth * visualWidthFactor * scale, CarDotSize)
        Dim scaledCarLength As Double = Math.Max(CarLength * scale, 12.0) ' Minimum 12 pixels long

        ' Apply lateral spacing factor instead of fixed offset to maintain symmetry
        ' This fixes the issue where right-side cars intersected too early
        Dim radarX As Double = RadarCenter + ((relativeX * lateralSpacingFactor) * scale) - (scaledCarWidth / 2)
        Dim radarY As Double = RadarCenter - (relativeY * scale) - (scaledCarLength / 2)

        ' Get or create the car dot
        Dim dot As Rectangle
        If Not CarDots.TryGetValue(carId, dot) Then
            dot = New Rectangle() With {
                .Stroke = Brushes.White,
                .StrokeThickness = 1,
                .RenderTransformOrigin = New Point(0.5, 0.5)
            }
            CarDots(carId) = dot
            carCanvas.Children.Add(dot)
        End If

        ' Update rectangle size to match scaled car dimensions
        dot.Width = scaledCarWidth
        dot.Height = scaledCarLength

        ' Apply rotation to the dot to show car heading
        dot.RenderTransform = New RotateTransform(relativeHeading * 180 / Math.PI)

        ' Get or create the distance label
        'Dim label As TextBlock
        'If Not CarLabels.TryGetValue(carId, label) Then
        '    label = New TextBlock() With {
        '        .Foreground = Brushes.White,
        '        .FontSize = 10,
        '        .FontWeight = FontWeights.Bold
        '    }
        '    CarLabels(carId) = label
        '    carCanvas.Children.Add(label)
        'End If

        '' Update distance label text (show edge-to-edge distance)
        'label.Text = edgeDistance.ToString("F1")

        ' Set color based on edge-to-edge proximity first, then position
        If edgeDistance <= ProximityWarningRadius Then
            dot.Fill = ProximityWarningColor
            _warningCars.Add(carId)
        Else
            _warningCars.Remove(carId)
            If isAhead.HasValue Then
                dot.Fill = If(isAhead.Value, AheadCarColor, BehindCarColor)
            Else
                dot.Fill = DefaultCarColor
            End If
        End If

        UpdateOpacity()

        ' Position the dot
        Canvas.SetLeft(dot, radarX)
        Canvas.SetTop(dot, radarY)

        ' Position the label (centered above the dot)
        'Canvas.SetLeft(label, radarX + (scaledCarWidth / 2) - 8)
        'Canvas.SetTop(label, radarY - 14)

        ' Update player car visibility
        UpdatePlayerCarVisibility()
    End Sub

    ''' <summary>
    ''' Removes a car from the radar display.
    ''' </summary>
    ''' <param name="carId">Unique identifier for the car to remove.</param>
    Public Sub RemoveCar(carId As Integer)
        If CarDots.ContainsKey(carId) Then
            carCanvas.Children.Remove(CarDots(carId))
            CarDots.Remove(carId)
        End If

        If CarLabels.ContainsKey(carId) Then
            carCanvas.Children.Remove(CarLabels(carId))
            CarLabels.Remove(carId)
        End If

        _warningCars.Remove(carId)
        UpdateOpacity()

        ' Update player car visibility
        UpdatePlayerCarVisibility()
    End Sub

    ''' <summary>
    ''' Clears all cars from the radar except the player.
    ''' </summary>
    Public Sub ClearAllCars()
        For Each dot In CarDots.Values
            carCanvas.Children.Remove(dot)
        Next
        CarDots.Clear()

        For Each label In CarLabels.Values
            carCanvas.Children.Remove(label)
        Next
        CarLabels.Clear()

        _warningCars.Clear()
        UpdateOpacity()

        ' Hide player car when no other cars are present
        UpdatePlayerCarVisibility()
    End Sub

    ''' <summary>
    ''' Resets the radar to its default state.
    ''' </summary>
    Public Sub Reset()
        ClearAllCars()
    End Sub

    ' Add smoothing for direction values
    Private _smoothedDirection As Double = 0
    Private Const SmoothingFactor As Double = 1 ' Lower = smoother, slower response

    Public Function GetSmoothedDirection(rawDirection As Integer) As Double
        ' Convert raw direction to degrees (0-360)
        Dim directionDegrees As Double = (rawDirection / 32768.0) * 180.0
        Return rawDirection
        ' Apply exponential moving average
        _smoothedDirection = _smoothedDirection + SmoothingFactor * (directionDegrees - _smoothedDirection)

        Return _smoothedDirection
    End Function

    Private _lastX As Integer?
    Private _lastY As Integer?

    Public Function CalculateDirectionFromPosition(car As CompCar) As Double?
        If _lastX.HasValue AndAlso _lastY.HasValue Then
            Dim dx As Double = car.X - _lastX.Value
            Dim dy As Double = car.Y - _lastY.Value

            ' Only calculate if there's meaningful movement
            If Math.Abs(dx) > 100 OrElse Math.Abs(dy) > 100 Then
                Dim direction As Double = Math.Atan2(dx, dy) * (180.0 / Math.PI)
                If direction < 0 Then direction += 360

                _lastX = car.X
                _lastY = car.Y
                Return direction
            End If
        End If

        _lastX = car.X
        _lastY = car.Y
        Return Nothing
    End Function
End Class