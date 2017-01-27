'5/31 v1.0 基本功能可用
'5/31晚 v1.1 添加预览下一步颜色功能
'6/7 v1.2 生日前最后一次调整   完成下一步生成三个随机时不会检查是否已超过或等于五个 以及 分数放入主类中
'6/20 v1.3 添加存档功能
'17/1/22 v1.4 删除出错的routinearray
Public Class Main
    Public Const libver As String = "1.3"

    Public theSizeOfArray As Integer = 7
    Public array(theSizeOfArray, theSizeOfArray) As Integer
    Public theMaxSearchStep = 70
    Public CountOfArray As Integer
    Public color(2) As Integer
    Public score As Integer

    Public foundarray As New ArrayList
    Public delarray As New ArrayList
    Public routinearray As New ArrayList
    Public nullarray As New ArrayList
    Public Sub initialize()
        '刷新数列
        Dim i As Integer, j As Integer
        For i = 0 To theSizeOfArray
            For j = 0 To theSizeOfArray
                array(i, j) = 0
            Next
        Next

        '生成初始五个
        Dim rand As New System.Random
        For k = 1 To 5
            i = rand.Next(theSizeOfArray)
            j = rand.Next(theSizeOfArray)
            If array(i, j) = 0 Then
                array(i, j) = rand.Next(5) + 1
            Else
                k -= 1
            End If
        Next

        '为防止第一次重新开始游戏时的错误 将CountOfArray重置
        CountOfArray = 0
        Dim randcolor As New System.Random
        For i = 0 To 2
            color(i) = randcolor.Next(5) + 1
        Next

        '将分数重置
        score = 0
    End Sub
    Public Function canNextStep() As Boolean
        CountOfArray = 0
        For m = 0 To theSizeOfArray
            For n = 0 To theSizeOfArray
                If array(m, n) <> 0 Then
                    CountOfArray += 1
                Else
                    nullarray.Add(m & n)
                End If
            Next
        Next
        If CountOfArray < (theSizeOfArray + 1) ^ 2 * 0.75 Then nullarray.Clear()
        If CountOfArray >= (theSizeOfArray + 1) ^ 2 - 3 Then
            Return (False)
        Else
            Return (True)
        End If
    End Function
    Public Function nextStep() As String
        Dim i As Integer, j As Integer
        'If CountOfArray >= theSizeOfArray ^ 2 - 3 Then
        'Return ("OutOfRange")
        'Else
        Dim _array As String
        If CountOfArray >= (theSizeOfArray + 1) ^ 2 * 0.75 Then
            '还存在问题   并没有想好用什么方法来解决最后时候random效率低的问题
            'Dim nullarray As New System.Collections.ArrayList
            'For i = 0 To theSizeOfArray
            '    For j = 0 To theSizeOfArray
            '        If array(i, j) = 0 Then
            '            _array(0) = i
            '            _array(1) = j
            '            nullarray.Add(_array)
            '        End If
            '    Next
            'Next
            Dim rand As New System.Random
            'Dim randcolor As New System.Random
            Dim x As Integer
            For i = 0 To 2
                'x = rand.Next(3) + 1
                x = rand.Next(nullarray.Count)
                _array = nullarray(x)
                'array(_array(0), _array(1)) = randcolor.Next(5) + 1
                array(Mid(_array, 1, 1), Mid(_array, 2, 1)) = rand.Next(5) + 1
                nullarray.RemoveAt(x)
                score += checkline(Mid(_array, 1, 1), Mid(_array, 2, 1))
            Next
        Else
            Dim randcolor As New System.Random
            For k = 0 To 2
                i = randcolor.Next(theSizeOfArray + 1)
                j = randcolor.Next(theSizeOfArray + 1)
                If array(i, j) = 0 Then
                    array(i, j) = color(k)
                Else
                    k -= 1
                End If
                score += checkline(i, j) * 2
            Next
            For i = 0 To 2
                color(i) = randcolor.Next(5) + 1
            Next
            Return ("done")
        End If
    End Function
    Public Function checkline(ByVal x As Integer, y As Integer) As Integer
        'Dim x As Integer = thePositionOfBall(0), y As Integer = thePositionOfBall(1)
        '斜时用到的新y
        Dim _y As Integer
        '斜时用到的x的定义域
        Dim theAreaOfX(1) As Integer
        '斜时用到的数组
        Dim _theSameColorPostion(theSizeOfArray, 1) As Integer
        Dim theCountOfcolor As Integer
        Dim theSameColorPostion(theSizeOfArray) As Boolean
        '列检查
        theSameColorPostion(y) = True
        theCountOfcolor += 1
        '向上检查
        For i = y - 1 To 0 Step -1
            If array(x, i) = array(x, y) Then
                theSameColorPostion(i) = True
                theCountOfcolor += 1
            Else
                Exit For
            End If
        Next
        '向下检查
        For i = y + 1 To theSizeOfArray Step 1
            If array(x, i) = array(x, y) Then
                theSameColorPostion(i) = True
                theCountOfcolor += 1
            Else
                Exit For
            End If
        Next
        '检查列上是否有5个以上并重写数组(返回所有连续颜色位置)
        If theCountOfcolor >= 5 Then
            'Dim thePostionOfSelectedColor(theSizeOfArray, 1) As Integer
            For i = 0 To theSizeOfArray
                'thePostionOfSelectedColor(i, 0) = x
                If theSameColorPostion(i) = True Then
                    'thePostionOfSelectedColor(i, 1) = 1
                    array(x, i) = 0
                Else
                    'thePostionOfSelectedColor(i, 1) = 0
                End If
            Next
            score += theCountOfcolor * 2
            Return (theCountOfcolor)
        Else
            theCountOfcolor = 0
            ReDim theSameColorPostion(theSizeOfArray)
        End If

        '行检查
        theSameColorPostion(x) = True
        theCountOfcolor += 1
        '向左检查
        For i = x - 1 To 0 Step -1
            If array(i, y) = array(x, y) Then
                theSameColorPostion(i) = True
                theCountOfcolor += 1
            Else
                Exit For
            End If
        Next
        '向右检查
        For i = x + 1 To theSizeOfArray
            If array(i, y) = array(x, y) Then
                theSameColorPostion(i) = True
                theCountOfcolor += 1
            Else
                Exit For
            End If
        Next
        '检查行上是否有5个以上并重写数组
        If theCountOfcolor >= 5 Then
            For i = 0 To theSizeOfArray
                If theSameColorPostion(i) = True Then
                    array(i, y) = 0
                End If
            Next
            score += theCountOfcolor * 2
            Return (theCountOfcolor)
        Else
            theCountOfcolor = 0
            ReDim theSameColorPostion(theSizeOfArray)
        End If

        '左斜检查 \
        _theSameColorPostion(x, 0) = x
        _theSameColorPostion(x, 1) = y
        theCountOfcolor += 1
        '求出x的定义域
        theAreaOfX(0) = x - y
        If theAreaOfX(0) < 0 Then theAreaOfX(0) = 0
        theAreaOfX(1) = x + (theSizeOfArray - y)
        If theAreaOfX(1) > theSizeOfArray Then theAreaOfX(1) = theSizeOfArray

        theSameColorPostion(x) = True
        _y = y
        '向左上检查
        For i = x - 1 To theAreaOfX(0) Step -1
            _y -= 1
            If array(i, _y) = array(x, y) Then
                theSameColorPostion(i) = True
                _theSameColorPostion(i, 0) = i
                _theSameColorPostion(i, 1) = _y
                theCountOfcolor += 1
            Else
                Exit For
            End If
        Next
        _y = y
        '向右下检查
        For i = x + 1 To theAreaOfX(1)
            _y += 1
            If array(i, _y) = array(x, y) Then
                theSameColorPostion(i) = True
                _theSameColorPostion(i, 0) = i
                _theSameColorPostion(i, 1) = _y
                theCountOfcolor += 1
            Else
                Exit For
            End If
        Next
        '检查行上是否有5个以上并重写数组
        If theCountOfcolor >= 5 Then
            For i = 0 To theSizeOfArray
                If theSameColorPostion(i) = True Then
                    array(i, _theSameColorPostion(i, 1)) = 0
                End If
            Next
            score += theCountOfcolor * 2
            Return (theCountOfcolor)
        Else
            theCountOfcolor = 0
            ReDim theSameColorPostion(theSizeOfArray)
            ReDim _theSameColorPostion(theSizeOfArray, 1)
        End If

        '右斜检查 /
        _theSameColorPostion(x, 0) = x
        _theSameColorPostion(x, 1) = y
        theCountOfcolor += 1
        '求出x的定义域
        theAreaOfX(0) = x - (theSizeOfArray - y)
        If theAreaOfX(0) < 0 Then theAreaOfX(0) = 0
        theAreaOfX(1) = x + y
        If theAreaOfX(1) > theSizeOfArray Then theAreaOfX(1) = theSizeOfArray

        theSameColorPostion(x) = True
        _y = y
        '向左下检查
        For i = x - 1 To theAreaOfX(0) Step -1
            _y += 1
            If array(i, _y) = array(x, y) Then
                theSameColorPostion(i) = True
                _theSameColorPostion(i, 0) = i
                _theSameColorPostion(i, 1) = _y
                theCountOfcolor += 1
            Else
                Exit For
            End If
        Next
        _y = y
        '向右上检查
        For i = x + 1 To theAreaOfX(1)
            _y -= 1
            If array(i, _y) = array(x, y) Then
                theSameColorPostion(i) = True
                _theSameColorPostion(i, 0) = i
                _theSameColorPostion(i, 1) = _y
                theCountOfcolor += 1
            Else
                Exit For
            End If
        Next
        '检查行上是否有5个以上并重写数组
        If theCountOfcolor >= 5 Then
            For i = 0 To theSizeOfArray
                If theSameColorPostion(i) = True Then
                    array(i, _theSameColorPostion(i, 1)) = 0
                End If
            Next
            score += theCountOfcolor * 2
            Return (theCountOfcolor)
        Else
            theCountOfcolor = 0
            ReDim theSameColorPostion(theSizeOfArray)
            ReDim _theSameColorPostion(theSizeOfArray, 1)
        End If
        Return (0)
    End Function

    Public Sub Move(ByVal x1 As Integer, ByVal y1 As Integer, ByVal x2 As Integer, ByVal y2 As Integer)
        array(x2, y2) = array(x1, y1)
        array(x1, y1) = 0
    End Sub
    Public Function canMove(ByVal x1 As Integer, ByVal y1 As Integer, ByVal x2 As Integer, ByVal y2 As Integer) As Boolean
        If array(x1, y1) <> 0 And array(x2, y2) = 0 Then

            '重新清空数组
            foundarray.Clear()
            delarray.Clear()

            Dim temparray(theSizeOfArray, theSizeOfArray) As Boolean
            Dim coordinate As String
            Dim Isfoundinanyarray As Boolean
            '父临时集合 
            Dim fatherarray As New ArrayList
            '在循环中的父临时数组 储存找到的坐标位置和由谁找到的
            Dim tmparray(3) As Integer

            Dim x As Integer, y As Integer
            For i = 0 To theSizeOfArray
                For j = 0 To theSizeOfArray
                    If array(i, j) = 0 Then temparray(i, j) = True
                Next
            Next
            foundarray.Add(intToString(x1, y1))
            For i = 0 To theMaxSearchStep

                '当跑完完整foundarray时返回False
                If foundarray.Count = 0 Then Return False

                coordinate = foundarray(0)

                If coordinate = intToString(x2, y2) Then
                    Return True
                End If
                'If coordinate = intToString(x2, y2) Then
                '    '作为之后匹配上一个结果的临时变量
                '    Dim x3 As Integer = x2, y3 As Integer = y2
                '    routinearray.Add(intToString(x2, y2))
                '    '将foundarray中的数据处理提取出routinearray
                '    For j = 0 To foundarray.Count
                '        '搜寻x2y2在foundarray中的位置并提取出来加入routinearray中
                '        Dim tempx As Integer, tempy As Integer
                '        tempx = Mid(fatherarray(i).ToString, 3, 1)
                '        tempy = Mid(fatherarray(i).ToString, 4, 1)
                '        If intToString(tempx, tempy) = intToString(x3, y3) Then
                '            routinearray.Add(intToString(Mid(fatherarray(i).ToString, 1, 1), Mid(fatherarray(i).ToString, 2, 1)))
                '            x3 = Mid(fatherarray(i).ToString, 1, 1)
                '            y3 = Mid(fatherarray(i).ToString, 2, 1)
                '        End If
                '    Next

                '    Return True
                'End If

                Isfoundinanyarray = Isinfounddelarray(coordinate)

                x = Left(coordinate, 1)
                y = Right(coordinate, 1)

                If Isfoundinanyarray = True Then
                    '向上
                    If temparray(x, ifOutArrayEdge(y - 1)) = True Then
                        If foundarray.Contains(intToString(x, ifOutArrayEdge(y - 1))) = False And delarray.Contains(intToString(x, ifOutArrayEdge(y - 1))) = False Then
                            tmparray(0) = x
                            tmparray(1) = y
                            tmparray(2) = x
                            tmparray(3) = ifOutArrayEdge(y - 1)
                            fatherarray.Add(tmparray)
                            foundarray.Add(intToString(x, ifOutArrayEdge(y - 1)))
                        End If
                    End If
                    '向右
                    If temparray(ifOutArrayEdge(x + 1), y) = True Then
                        If foundarray.Contains(intToString(ifOutArrayEdge(x + 1), y)) = False And delarray.Contains(intToString(ifOutArrayEdge(x + 1), y)) = False Then
                            tmparray(0) = x
                            tmparray(1) = y
                            tmparray(2) = ifOutArrayEdge(x + 1)
                            tmparray(3) = y
                            fatherarray.Add(tmparray)
                            foundarray.Add(intToString(ifOutArrayEdge(x + 1), y))
                        End If
                    End If
                    '向下
                    If temparray(x, ifOutArrayEdge(y + 1)) = True Then
                        If foundarray.Contains(intToString(x, ifOutArrayEdge(y + 1))) = False And delarray.Contains(intToString(x, ifOutArrayEdge(y + 1))) = False Then
                            tmparray(0) = x
                            tmparray(1) = y
                            tmparray(2) = x
                            tmparray(3) = ifOutArrayEdge(y + 1)
                            fatherarray.Add(tmparray)
                            foundarray.Add(intToString(x, ifOutArrayEdge(y + 1)))
                        End If
                    End If
                    '向左
                    If temparray(ifOutArrayEdge(x - 1), y) = True Then
                        If foundarray.Contains(intToString(ifOutArrayEdge(x - 1), y)) = False And delarray.Contains(intToString(ifOutArrayEdge(x - 1), y)) = False Then
                            tmparray(0) = x
                            tmparray(1) = y
                            tmparray(2) = ifOutArrayEdge(x - 1)
                            tmparray(3) = y
                            fatherarray.Add(tmparray)
                            foundarray.Add(intToString(ifOutArrayEdge(x - 1), y))
                        End If
                    End If
                End If

                '把foundarray中的查找过得提取出来放入delarray
                If Isindelarray(coordinate) = True Then
                    delarray.Add(coordinate)
                    foundarray.RemoveAt(0)
                End If




            Next


        End If
        Return (False)
    End Function

    Function ifOutArrayEdge(ByVal a As Integer) As Integer
        If a < 0 Then
            Return (0)
        ElseIf a > theSizeOfArray Then
            Return (theSizeOfArray)
        Else
            Return (a)
        End If
    End Function

    Function intToString(ByVal x As Integer, ByVal y As Integer) As String
        Dim re As String
        If x = 0 Then
            re = "0" & y.ToString
        Else
            re = x * 10 + y
        End If
        Return (re)
    End Function

    Function Isinfounddelarray(ByVal coordinate As String) As Boolean
        '返回true表示坐标不存在于任何数组中
        If foundarray.Count = 1 Then
            '对于第一次
            Return True
        Else
            '对于之后的
            If foundarray.Contains(coordinate) = True And delarray.Contains(coordinate) = True Then Return False
        End If
        Return True
    End Function
    Function Isindelarray(ByVal coordinate As String) As Boolean
        '返回true表示坐标不存在于delarray中
        If delarray.Count - 1 <> 0 Then
            If delarray.Contains(coordinate) = True Then Return False
            Return True
        Else
            Return True
        End If
    End Function

    Function save(ByVal section As Integer) As String
        Dim str As String = ""
        For i = 0 To theSizeOfArray
            str &= array(i, section)
        Next
        Return str
    End Function

    Sub load(ByVal str As String, ByVal section As Integer)
        For i = 0 To theSizeOfArray
            array(i, section) = Mid(str, i + 1, 1)
        Next
    End Sub

End Class
