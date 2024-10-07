' Converter to translate QB64 ode whxch used $NOPREFIX and used QB64/QB64PE reserved words without _ prefix
' Oaul Robinson 2024-09-25

Option _Explicit

Const UCa = "A"
Const UCz = "Z"
Const Digit0 = "0"
Const Digit9 = "9"
Const Underscore = "_"
Const Quote = Chr$(34) ' "
Const Squote = "'"
Const FALSE = 0
Const TRUE = Not FALSE

' Vars used by subs too
Dim Shared As Long Current
Dim Shared As _Unsigned _Byte Done, isIdent
Dim Shared As String * 1 Ch, UCh



Dim As String InFile, OutFile, ErrF, InLine
Dim As Integer InF, OutF, TypeCount, I, KeywordCount


Print "This program takes a BASIC language source program which may have used"
Print "new reserved keywords or functions in QB64/QB64PE and converts them to the"
Print "correct form."
Print "Give input file"
InFile = _OpenFileDialog$("Enter name of BASIC language file to convert", "./", "*.bas|*.bm|*.bi|*.*", "BASIC Programming Langage Filess")
If InFile = "" Then Print "No file selected.": End
InF = FreeFile

ErrF = "Opening input file": On Error GoTo error1
Open InFile For Input As #InF
Print "File "; InFile; " opened.";

Print "Give name of output file:"
ErrF = "opening output file"
OutF = FreeFile
OutFile = _SaveFileDialog$("Enter name of QB64/QB64PE language file to convert", "./", "*.bas|*.bi*.bm", "QB64/QB64PE file")
If OutFile = "" Then Print "No output file selected": End

Print "File "; OutFile; " opened for output."

' We don't close input until after output is written
' to prevent user from erasing original file

ErrF = "Creating output file"

Open OutFile For Output As #OutF
Print "Output file "; OutFile; " opened."
ErrF = "Writing output file"


'Load Keyword table
Read KeywordCount

Dim Keyword(KeywordCount) As String


For I = 1 To KeywordCount
    Read Keyword(I)
Next

' Now scan the file

Dim As String TheIdent, FileLine, OutLine, UIdent
Dim As Integer Sz, C, IDStart, StringStart

While Not EOF(InF)
    Line Input #InF, FileLine
    Sz = Len(FileLine)
    Dim As String * 1 Ch(Sz), uch(Sz)
    For I = 1 To Sz: Ch(I) = Mid$(FileLine, I, 1): uch(I) = UCase$(Ch(I)): Next
    C = 1
    OutLine = ""
    While C < Sz
        Select Case Ch(C)
            Case Squote ' Line Comment
                OutLine = OutLine + Mid$(FileLine, C, Sz)
                Exit While ' exit to read next line
            Case Quote
                StringStart = C
                C = C + 1
                While Ch(C) <> Quote And C <= Sz
                    C = C + 1
                Wend
                OutLine = OutLine + Mid$(FileLine, StringStart, C)
                C = C + 1
            Case UCa To UCz, Underscore
                isIdent = TRUE
                IDStart = C
                TheIdent = ""
                UIdent = ""
                ' Now, we keep inspecting until we reach the end of the identifier
                ' if it contains _ or 0 to 9, it can't be what we're looking for
                ' but we have to keep going so we don't misidentify a partial
                While Ch(C) = Underscore Or (Ch(C) >= Digit0 And Ch(C) <= Digit9) Or ((uch(C) >= UCa And uch(C) <= UCz)) And C <= Sz
                    TheIdent = TheIdent + Ch(C)
                    UIdent = UIdent + uch(C)
                    If Ch(C) = Underscore Or (Ch(C) >= Digit0 And Ch(C) <= Digit9) Then isIdent = FALSE
                    C = C + 1
                Wend
                If isIdent Then
                    For I = 1 To KeywordCount
                        If UIdent = Keyword(I) Then
                            TheIdent = "_" + TheIdent
                            Exit For
                        End If
                    Next
                    ' Identifier is unchanged if it didn't match. otherwise itis prefgixed by _
                End If
                OutLine = OutLine + TheIdent
                C = C + 1
            Case Else
                OutLine = OutLine + Ch(C)
                C = C + 1
        End Select
    Wend
    Print #OutF, OutLine
Wend
Close

end


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



