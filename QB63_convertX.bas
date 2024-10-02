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

' Vars used by subs too
Dim Shared As Long Sz, Current, State
Dim Shared As _Unsigned _Byte C, Done
Dim Shared As String * 1 Ch, UCh


Dim As String InFile, OutFile, ErrF, OutLine, Identifier
Dim As Integer InF, OutF, TypeCount, I, KeywordCount


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
Dim Shared FI(Sz) As _Unsigned _Byte
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
Const Normal = 0
Const Ident1 = 1
Const Ident2 = 2
Const InComment = 3
Const Inquote = 4

Dim As _Unsigned _Byte Found, NotKeyword, Match

Dim As String TheIdent, RealIdent


'Load Keyword table
Read KeywordCount

Dim Keyword(KeywordCount) As String

For I = 1 To KeywordCount
    Read Keyword(I)
Next

' Now scan the file

NotKeyword = FALSE
TheIdent = ""


Dim SkipOnce As _Unsigned _Byte

Current = 0
Done = FALSE
Do
    If Current > Sz Then Exit Do
    NextCH
    Select Case State
        Case Normal
            Save

    End Select


    ' quote and comment pass through everything
    If (Not (Inquote Or InComment)) Then
        ' Identifier: 1st char A-Z and _; 2nd & subsequent chsrs: those plus 0-9 and .
        If (InIdent And ((Ch >= "0" And Ch <= "9") Or Ch = ".")) Or (UCh >= UCa And UCh <= UCz) Or C = Underscore Then
            TheIdent = TheIdent + UCh
            RealIdent = RealIdent + Ch
            InIdent = TRUE
            Skip = TRUE ' don't pass these on to the new file yet
            ' New keywords to be fixed don't have 0-9, . or _ in them
            If (Ch >= "0" And Ch <= "9") Or Ch = "." Or C = Underscore Then
                NotKeyword = TRUE
            End If
        Else ' not part of identifier
            If InIdent Then ' We came to end of identifier
                InIdent = FALSE
                Skip = FALSE ' We do want to collect this char

                If NotKeyword Then
                    TheIdent = "" ' We dont need upper case copy any more
                    OutLine = OutLine + RealIdent
                    SkipOnce = TRUE
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

                    SkipOnce = TRUE
                End If
            End If
        End If
    End If
    If Skip Or SkipOnce Then ' Skip this one char because we already have it
        SkipOnce = FALSE

    Else
        OutLine = OutLine + Ch ' not in an ident; pass it through

    End If
    ' If we hit an open comment, bypass till end of line
    If Not InQuote And Ch = "'" Then InComment = TRUE: Skip = FALSE
    ' If it is " while not inside a comment, switch
    If (Not InComment) And Ch = Quote Then InQuote = Not InQuote

    If C = 10 Then ' end of line
        InComment = FALSE ' All these end at end of line
        InQuote = FALSE
        Skip = FALSE
        InQuote = FALSE
    End If
Loop Until Done Or Current > Sz

Close
Print "Cnversion completed."

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

Sub NextCH
    C = FI(Current): Ch = Chr$(C): UCh = UCase$(Ch)
    Current = Current + 1
End Sub

Sub Save
   Ou
end sub
