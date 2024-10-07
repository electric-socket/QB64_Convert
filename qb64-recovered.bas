' Converter to translate QB64 ode whxch used $NOPREFIX and used QB64/QB64PE reserved words without _ prefix
' Oaul Robinson 2024-09-25

Option _Explicit
'$Include:'L:\programming\$library\Common_Dialog_Prefix.bi'
Const UCa = "A"
Const UCz = "Z"
Const Underscore = "_"
Const Quote = Chr$(34) ' "
Const Squote = "'"
Const FALSE = 0
Const TRUE = Not FALSE

' Vars used by subs too
Dim Shared As Long Current
Dim Shared As _Unsigned _Byte C, Done, NotIdent
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

Dim TheIdent As String


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

'$Include:'L:\programming\$library\Common_Dialog_Suffix.bi'

