Imports System.Windows.Media

Public Class DifferenceGaugeControl
        Private ReadOnly RedColor As SolidColorBrush = New SolidColorBrush(Color.FromRgb(220, 50, 50))
        Private ReadOnly GreenColor As SolidColorBrush = New SolidColorBrush(Color.FromRgb(50, 200, 50))
        Private ReadOnly TransparentColor As SolidColorBrush = Brushes.Transparent

        Private _leftIndicators As Border()
        Private _rightIndicators As Border()

        ''' <summary>
        ''' The step size for each indicator (e.g., 0.2 for ms, 2.0 for km/h)
        ''' </summary>
        Public Property StepSize As Double = 1.0

        ''' <summary>
        ''' Format string for displaying the value (e.g., "F2" for 2 decimal places)
        ''' </summary>
        Public Property ValueFormat As String = "F2"

        ''' <summary>
        ''' If True, positive values show green (faster). If False, positive values show red (slower).
        ''' </summary>
        Public Property PositiveIsGood As Boolean = True

        Public Sub New()
            InitializeComponent()
            _leftIndicators = {left1, left2, left3, left4, left5}
            _rightIndicators = {right1, right2, right3, right4, right5}
        End Sub

        ''' <summary>
        ''' Updates the gauge with the specified difference value.
        ''' </summary>
        ''' <param name="value">The difference value to display.</param>
        Public Sub UpdateValue(value As Double)
            ' Clear all indicators
            For Each indicator In _leftIndicators
                indicator.Background = TransparentColor
            Next
            For Each indicator In _rightIndicators
                indicator.Background = TransparentColor
            Next

            ' Update text display
            If value >= 0 Then
                txtValue.Text = "+" & value.ToString(ValueFormat)
            Else
                txtValue.Text = value.ToString(ValueFormat)
            End If

            ' Determine colors based on PositiveIsGood setting
            Dim isGood As Boolean = If(PositiveIsGood, value > 0, value < 0)
            txtValue.Foreground = If(value = 0, Brushes.White, If(isGood, GreenColor, RedColor))

            ' Calculate number of indicators to show
            Dim steps As Integer = CInt(Math.Floor(Math.Abs(value) / StepSize))
            steps = Math.Min(steps, 5)  ' Max 5 indicators

            ' Determine which side and color to use
            Dim showOnRight As Boolean = If(PositiveIsGood, value > 0, value < 0)
            Dim indicatorColor As SolidColorBrush = If(isGood, GreenColor, RedColor)

            If showOnRight Then
                For i As Integer = 0 To steps - 1
                    _rightIndicators(i).Background = indicatorColor
                Next
            ElseIf value <> 0 Then
                For i As Integer = 0 To steps - 1
                    _leftIndicators(i).Background = indicatorColor
                Next
            End If
        End Sub

        ''' <summary>
        ''' Resets the gauge to its default state.
        ''' </summary>
        Public Sub Reset()
            UpdateValue(0)
        End Sub
    End Class
