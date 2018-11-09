Public Class Creature_Class
    Public Alive As Boolean = True
    Public DNA As String
    Public Nodes As New System.Collections.Generic.Dictionary(Of String, Node_Class)
    Public UniqueNodeNumber As Integer = 0
    Public X As Decimal
    Public Y As Decimal
    Public OldX As Decimal = X
    Public OldY As Decimal = Y
    Public Velocity As Decimal = 1
    Public Angle As Decimal = Rnd() * 5 - 2.5
    Public OldAngle As Decimal = Angle
    Public Rateofchangeofangle = 0
    Public Health As Decimal = 1000
    Public R As Integer = Rnd() * 255
    Public B As Integer = Rnd() * 255
    Public G As Integer = Rnd() * 255
    Public Colour As SolidBrush = New SolidBrush(Color.FromArgb(R, B, G))
    Public Generation As Integer = 0

    Public Sub Run()
        For Each Node In Nodes
            If Node.Value.Type <> "i" Then
                Dim Inputsum As Decimal
                For Each Link In Node.Value.Links
                    If Nodes.ContainsKey(Link.Key) Then
                        Inputsum += Link.Value.Weight * Nodes(Link.Key).Value
                    End If
                Next
                If Inputsum > Node.Value.Threshold Then
                    Node.Value.TempValue = 1 'Inputsum / 50 + 1
                Else
                    Node.Value.TempValue = -1 'Inputsum / 50 - 1
                End If
            End If
        Next

        For Each Node In Nodes
            Node.Value.Value = Node.Value.TempValue
        Next
    End Sub
    Public Sub AddNode()
        UniqueNodeNumber += 1
        Dim NewNode As New Node_Class
        NewNode.Type = "h"
        Dim Nodeintecept As Integer = 0
        Dim Newlink As New Links_Class

        Randomize()
        For i = 0 To Int(Rnd(UniqueNodeNumber - 2)) + 1 Step 1
            Do
                Randomize()
                Nodeintecept = Rnd() * Nodes.Count
                If Nodes.ContainsKey(Nodeintecept) Then
                    If Nodes(Nodeintecept).Type <> "i" Then
                        If Nodes(Nodeintecept).Links.ContainsKey(UniqueNodeNumber) = False Then
                            Exit Do
                        End If
                    End If
                End If
            Loop
            Newlink = New Links_Class
            Randomize()
            Newlink.Weight = Rnd() * 2 - 1
            Nodes(Nodeintecept).Links.Add(UniqueNodeNumber, Newlink)
        Next


        For i = 0 To Int(Rnd(UniqueNodeNumber - 2)) + 1 Step 1
            Nodeintecept = 0
            Do
                Randomize()
                Nodeintecept = Rnd() * Nodes.Count
                If Nodes.ContainsKey(Nodeintecept) Then
                    If Nodes(Nodeintecept).Type <> "o" Then
                        If NewNode.Links.ContainsKey(Nodeintecept) = False Then
                            Exit Do
                        End If
                    End If
                End If
            Loop
            Newlink = New Links_Class
            Randomize()
            Newlink.Weight = Rnd() * 2 - 1
            NewNode.Links.Add(Nodeintecept, Newlink)
        Next

        Nodes.Add(UniqueNodeNumber, NewNode)


    End Sub
    Public Sub RemoveNode(ByVal NodeID As Integer)

        If Nodes.ContainsKey(NodeID) = True Then
            Nodes.Remove(NodeID)
            For Each Node In Nodes
                If Node.Value.Links.ContainsKey(NodeID) Then
                    Node.Value.Links.Remove(NodeID)
                End If
            Next
        End If

    End Sub
    Public Sub SetNewCreature(ByVal NumberofInputs As Integer, ByVal NumberofOutputs As Integer)
        For i = 1 To NumberofInputs Step 1
            UniqueNodeNumber += 1
            Dim NewNode As New Node_Class
            NewNode.Type = "i"
            Nodes.Add(UniqueNodeNumber, NewNode)
        Next
        For o = 1 To NumberofOutputs Step 1
            UniqueNodeNumber += 1
            Dim NewNode As New Node_Class
            NewNode.Type = "o"
            Randomize()
            NewNode.Threshold = Rnd() * 2 - 1
            'For each input node, make link to output
            For Each Node In Nodes
                Dim NewLink As New Links_Class
                Randomize()
                NewLink.Weight = Rnd() * 2 - 1
                NewNode.Links.Add(Node.Key, NewLink)
            Next
            Dim SelfLink As New Links_Class
            Randomize()
            SelfLink.Weight = Rnd() * 2 - 1
            NewNode.Links.Add(UniqueNodeNumber, SelfLink)
            Nodes.Add(UniqueNodeNumber, NewNode)
        Next
    End Sub
    Public Sub SetNewFromDNA(ByVal DNA As String, ByRef C As Integer)
        Dim Highestref As Integer = 0
        Dim Startcvalue = C
        Dim Genfound As Boolean = False
        Dim Str_Gen As String = Nothing
        For C = Startcvalue To Len(DNA) Step 1
            Dim Nextchar As Char = GetChar(DNA, C)
            Select Case Nextchar
                Case "<"
                    If Genfound = True Then
                        'New Node to add
                        Dim NewNode As New Node_Class
                        Dim ID As Integer = 0
                        CreateNode(DNA, C, ID, NewNode, Highestref)
                        Nodes.Add(ID, NewNode)
                    End If

                Case ">"
                    If Genfound = False Then
                        Genfound = True
                        Generation = CInt(Str_Gen)
                    Else
                        Exit For
                    End If
                Case "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"
                    If Genfound = False Then
                        Str_Gen &= Nextchar
                    End If
            End Select
        Next
        UniqueNodeNumber = Highestref
    End Sub
    Private Sub CreateNode(ByVal DNA As String, ByRef C As Integer, ByRef ID As Integer, ByRef NewNode As Node_Class, ByRef HighRef As Integer)


        Dim IDFound As Boolean = False
        Dim Str_ID As String = Nothing

        Dim TypeFound As Boolean = False
        Dim Str_Type As String = Nothing

        Dim ThresholdFound As Boolean = False
        Dim Str_Threshold As String = Nothing

        Dim ValueFound As Boolean = False
        Dim Str_Value As String = Nothing


        Do
            C += 1
            If C >= DNA.Length Then
                Exit Do
            End If
            Dim Nextchar As Char = GetChar(DNA, C)
            Select Case Nextchar
                Case "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", ".", "-", "i", "o", "h"
                    If IDFound = False Then
                        Str_ID &= Nextchar
                    Else
                        If TypeFound = False Then
                            Str_Type &= Nextchar
                        Else
                            If ThresholdFound = False Then
                                Str_Threshold &= Nextchar
                            Else
                                If ValueFound = False Then
                                    Str_Value &= Nextchar
                                Else

                                End If
                            End If
                        End If
                    End If
                Case ","
                    If IDFound = False Then
                        IDFound = True
                        ID = Str_ID

                        If ID >= HighRef Then
                            HighRef = ID
                        End If
                    Else
                        If TypeFound = False Then
                            TypeFound = True
                            NewNode.Type = Str_Type
                        Else
                            If ThresholdFound = False Then
                                ThresholdFound = True
                                NewNode.Threshold = Str_Threshold
                            Else
                                If ValueFound = False Then
                                    ValueFound = True
                                    NewNode.Value = Str_Value
                                Else

                                End If
                            End If
                        End If
                    End If
                Case "{"
                    Dim NewLink As New Links_Class
                    Dim LocationFound As Boolean = False
                    Dim str_Location As String = Nothing
                    Dim str_Weight As String = Nothing

                    Do

                        C += 1
                        If C >= DNA.Length Then
                            Exit Do
                        End If
                        Dim Nextchar2 As Char = GetChar(DNA, C)
                        Select Case Nextchar2
                            Case ","
                                LocationFound = True
                            Case "}"
                                NewLink.Weight = CDec(str_Weight)
                                NewNode.Links.Add(str_Location, NewLink)

                                If CInt(str_Location) >= HighRef Then
                                    HighRef = CInt(str_Location)
                                End If

                                Exit Do
                            Case "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", ".", "-"
                                If LocationFound = False Then
                                    str_Location &= Nextchar2
                                Else
                                    str_Weight &= Nextchar2
                                End If
                        End Select
                    Loop
                Case ">"
                    Exit Do
            End Select
        Loop


    End Sub
    Public Sub PrintDNA()
        Dim DNAstring As String = Nothing
        DNAstring &= "<" & Generation & ">"
        For Each Node In Nodes
            DNAstring &= "<" & PrintNode(Node.Value, Node.Key) & ">"
        Next
        DNA = DNAstring
    End Sub
    Private Function PrintNode(ByVal Node As Node_Class, ByVal ID As Integer)
        Dim DNA As String = Nothing
        DNA = ID & "," & Node.Type & "," & Node.Threshold & "," & Node.Value & ","
        For Each Link In Node.Links
            DNA &= "{" & Link.Key.ToString & "," & Link.Value.Weight & "}"
        Next
        PrintNode = DNA
    End Function
    Public Sub Mutation()
        For Each Node In Nodes
            For Each Link In Node.Value.Links
                Randomize()
                If Rnd() * 100 < 5 Then
                    Randomize()
                    Link.Value.Weight *= Rnd() * 4 - 2
                End If
                Randomize()
                If Rnd() * 100 < 1 Then
                    Randomize()
                    Link.Value.Weight += Rnd() * 2 - 1
                End If
            Next
        Next
        Randomize()
        If Rnd() * 100 < 5 Then
            AddNode()
        End If
        Randomize()
        Dim Nodeindex = Rnd() * (UniqueNodeNumber)
        If Nodes.ContainsKey(Nodeindex) = True Then
            If Nodes(Nodeindex).Type = "h" Then
                Randomize()
                If Rnd() * (100 + UniqueNodeNumber * 2) < 10 + UniqueNodeNumber Then
                    RemoveNode(Nodeindex)
                End If
            End If
        End If
    End Sub


End Class
