' Converter to translate QB64 ode whxch used $NOPREFIX and used QB64/QB64PE reserved words without _ prefix
' Oaul Robinson 2024-09-25

Option _Explicit
'$Include:'L:\programming\$library\Common_Dialog_Prefix.bi'
Const UCa = "A"
Const UCz = "Z"
Const Underscore = Asc("_")
Const Quote = Chr$(34) ' "
Const FALSE = 0
Const TRUE = Not FALSE

Dim As String InFile, OutFile, ErrF, OutLine, Identifier
Dim As Integer InF, OutF, TypeCount, I, KeywordCount
Dim As Long Sz, Current

Print "This program takes a BASIC language source program which may have used"
Print "new reserved keywords or functions in QB64/QB64PE and converts them to the"
Print "correct form."
Print "Give input file"
InFile = _OpenFileDialog$("Enter name of BASIC language file to convert", "./", "*.bas|*.bm|*.bi|*.*", "BASIC Programming Langage Filess")
If InFile = "" Then Print "No file selected.": End
InF = FreeFile

ErrF = "Opening input file": On Error GoTo error1
Open InFile For Binary As #InF
Print "File "; InFile; " opened, size";
Sz = LOF(InF)
Print Sz; "bytes."
If Sz < 5 Then Print "?File empty or too small": End
ErrF = "Reading input file"
Dim FI(Sz) As _Unsigned _Byte
Get #InF, , FI()
Print "File successfully read,"; Sz; "bytes."
Print "Give name of output file:"
ErrF = "opening output file"
OutF = FreeFile
OutFile = _SaveFileDialog$("Enter name of QB64/QB64PE language file to convert", "./", "*.bas|*.bi*.bm", "QB64/QB64PE file")
If OutFile = "" Then Print "No output file selected": End

Print OutFile

' We don't close input until after output is written
' to prevent user from erasing original file

ErrF = "Creating output file"

Open OutFile For Output As #OutF
Print "Output file "; OutFile; " opened."
ErrF = "Writing output file"
OutLine = ""
Current = 0

Dim As _Unsigned _Byte C, Found, NotKeyword, Match
Dim As _Unsigned _Byte DontSkip, InIdent, InQuote, InComment
Dim As String * 1 Ch, UCh
Dim As String TheIdent, RealIdent


'Load Keyword table
Read KeywordCount

Dim Keyword(KeywordCount) As String

For I = 1 To KeywordCount
    Read Keyword(I)
Next

' Now scan the file

NotKeyword = FALSE
InIdent = FALSE
InQuote = FALSE
TheIdent = ""
DontSkip = TRUE

Dim SkipOnce As _Unsigned _Byte

For Current = 0 To Sz

    C = FI(Current): Ch = Chr$(C): UCh = UCase$(Ch)

    ' quote and comment pass through everything
    If (Not (InQuote Or InComment)) Then
        ' Identifier: 1st char A-Z and _; 2nd & subsequent chsrs: those plus 0-9 and .
        If (InIdent And ((Ch >= "0" And Ch <= "9") Or Ch = ".")) Or (UCh >= UCa And UCh <= UCz) Or C = Underscore Then
            TheIdent = TheIdent + UCh
            RealIdent = RealIdent + Ch
            InIdent = TRUE
            DontSkip = FALSE ' don't collect these yet
            ' New keywords to be fixed don't have 0-9, . or _ in them
            If (Ch >= "0" And Ch <= "9") Or Ch = "." Or C = Underscore Then
                NotKeyword = TRUE
            End If
        Else ' not part of identifier
            If InIdent Then ' We came to end of identifier
                InIdent = FALSE
                DontSkip = TRUE
                If NotKeyword Then
                    TheIdent = ""
                    DontSkip = TRUE ' We do want to collect this char
                Else 'Scan for keyword

                    For I = 1 To KeywordCount
                        If TheIdent = Keyword(I) Then
                            ' insert _ before identifier
                            RealIdent = "_" + RealIdent

                            Exit For
                        End If
                    Next
                    ' If it is a keyword, we fixed it
                    'If it was found in keywords, we put _ in front
                    'if not, we just restored it

                    OutLine = OutLine + RealIdent
                    RealIdent = ""
                    TheIdent = ""
                    DontSkip = TRUE
                    SkipOnce = TRUE
                End If
            End If
        End If
    End If
    If SkipOnce Then ' Skip this one char because we already have it
        SkipOnce = FALSE

    ElseIf DontSkip Then OutLine = OutLine + Ch ' not in ident; pass it through

    End If
    ' If we hit an open comment, bypass
    If Not InQuote And Ch = "'" Then InComment = TRUE: DontSkip = TRUE
    ' If it is " while not inside a comment, switch
    If (Not InComment) And Ch = Quote Then InQuote = Not InQuote

    If C = 10 Then ' end of line
        InComment = FALSE ' All these end at end of line
        InQuote = FALSE
        DontSkip = TRUE
        InQuote = FALSE

    End If
Next




Close
End



'While Current <= Sz
'    Print Sz; Current
'    C = FI(Current): Ch = Chr$(C)
'    NotKeyword = FALSE
'    Select Case C
'        Case 10 ' Line feed
'            Print #OutF, CurrentLine
'        Case 13: ' Carriage return is ignored
'        Case LCa To LCz, UCa To UCz, Underscore '  identifier or keyword
''        this indicates the start of an identifier
'            NotKeyword = FALSE
'            Identifier = ""
'            Do

'                If (Ch >= "0" And Ch <= "9") Or C = Underscore Then NotKeyword = TRUE
'                Identifier = Identifier + Ch
'                Current = Current + 1
'                C = FI(Current): Ch = Chr$(C)
''                 While still a letter or other valid identifier, continue
'                If (C >= UCa And C <= UCz) Or (C >= LCa And C <= UCz) or _
'                   (Ch >= "0" And Ch <= "9") Or C = Underscore Then _Continue

'                If NotKeyword Then
'                    CurrentLine = CurrentLine + Identifier
'                    Identifier = ""
'                    Current = Current - 1
'                    Exit Do '
'                End If

'                Found = 0
'                TheIdent = UCase$(Identifier)

'            Loop


'        Case Else
'            CurrentLine = CurrentLine + Chr$(FI(Current))
'    End Select
'    Current = Current + 1
'Wend
'Close
'Print "Cnversion completed."

End

Dim As Integer ER, EL
error1:
ER = Err: EL = _ErrorLine
Resume Quit



Quit:
Print "?Error"; ER; "when "; ErrF; " on line"; EL; "described as "
Print _ErrorMessage$(ER)
End

' kEYWORD LIST

' Number of keywords
Data 6


' QB64 keywords without leading _ in ALL CAPS
Data "BYTE"
Data "CONTINUE"
Data "ERRORLINE"
Data "ERRORMESSAGE"
' ...
Data "SAVEFILEDIALOG"
Data "UNSIGNED"

