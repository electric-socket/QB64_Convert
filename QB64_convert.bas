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
Print "File "; InFile; " opened."

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
Dim As Integer Sz, C, IDStart, StringStart, LineCount
Print "Processing file."
While Not EOF(InF)
    Line Input #InF, FileLine
    LineCount = LineCount + 1

    FileLine = FileLine + " " ' for sanity check
    Sz = Len(FileLine)
    ReDim As String * 1 Ch(Sz), uch(Sz) ' Since I don't preserve, it's automatically cleared on each line
    For I = 1 To Sz: Ch(I) = Mid$(FileLine, I, 1): uch(I) = UCase$(Ch(I)): Next
    C = 1
    OutLine = ""
    While C < Sz
        Select Case Ch(C)
            Case " " 'space
                OutLine = OutLine + " "


                C = C + 1
            Case Squote ' Line Comment
                OutLine = OutLine + Mid$(FileLine, C, Sz)
                Exit While ' exit to read next line
            Case Quote
                StringStart = C
                C = InStr(C + 1, FileLine, Quote)
                OutLine = OutLine + Mid$(FileLine, StringStart, C - StringStart + 1)
                C = C + 1
            Case UCa To UCz, Underscore
                isIdent = TRUE
                IDStart = C
                TheIdent = ""
                UIdent = ""
                ' Now, we keep inspecting until we reach the end of the identifier
                ' if it contains _ or 0 to 9, it can't be what we're looking for
                ' but we have to keep going so we don't misidentify a partial

                While Ch(C) = Underscore Or (Ch(C) >= Digit0 And Ch(C) <= Digit9) Or ((uch(C) >= UCa And uch(C) <= UCz)) And C < Sz

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
                'Print "dbg LC="; LineCount; "C="; C; " Ch(c)="; Ch(C); " theident="; TheIdent
                'Input I
                '  C = C + 1
            Case Else
                OutLine = OutLine + Ch(C)
                C = C + 1
        End Select
    Wend
    Print #OutF, RTrim$(OutLine) ' don't add trailing spaces
Wend
Close
Print "Copy and convert completed to "; OutFile; ","
Print LineCount; "lines processed."
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
Data 256


' QB64 keywords without leading _ in ALL CAPS
Data "ACOS"
Data "ACOSH"
Data "ADLER32"
Data "ALLOWFULLSCREEN"
Data "ALPHA"
Data "ALPHA32"
Data "ANDALSO"
Data "ARCCOT"
Data "ARCCSC"
Data "ARCSEC"
Data "ASIN"
Data "ASINH"
Data "ASSERT"
Data "ATAN2"
Data "ATANH"
Data "AUTODISPLAY"
Data "AXIS"
Data "BACKGROUNDCOLOR"
Data "BIT"
Data "BIN"
Data "BLEND"
Data "BLINK"
Data "BLINK"
Data "BLUE"
Data "BLUE32"
Data "BUTTON"
Data "BUTTONCHANGE"
Data "BYTE"
Data "CAPSLOCK"
Data "CEIL"
Data "CINP"
Data "CLEARCOLOR"
Data "CLIP"
Data "CLIPBOARD$"
Data "CLIPBOARDIMAGE"
Data "COLORCHOOSERDIALOG"
Data "COMMANDCOUNT"
Data "CONNECTED"
Data "CONNECTIONADDRESS"
Data "CONSOLE"
Data "CONSOLEINPUT"
Data "CONSOLECURSOR"
Data "CONSOLEFONT"
Data "CONSOLETITLE"
Data "CONTINUE"
Data "CONTROLCHR"
Data "COPYIMAGE"
Data "COPYPALETTE"
Data "COT"
Data "COTH"
Data "COSH"
Data "CRC32"
Data "CSC"
Data "CSCH"
Data "CV"
Data "CWD$"
Data "D2G"
Data "D2R"
Data "DEFAULTCOLOR"
Data "DEFINE"
Data "DEFLATE"
Data "DELAY"
Data "DEPTHBUFFER"
Data "DESKTOPHEIGHT"
Data "DESKTOPWIDTH"
Data "DEST"
Data "DEST"
Data "DEVICE$"
Data "DEVICEINPUT"
Data "DEVICES"
Data "DIR"
Data "DIREXISTS"
Data "DISPLAY"
Data "DISPLAYORDER"
Data "DONTBLEND"
Data "DONTWAIT"
Data "DROPPEDFILE"
Data "ECHO"
Data "EMBEDDED"
Data "ENVIRONCOUNT"
Data "ERRORLINE"
Data "ERRORMESSAGE"
Data "EXIT"
Data "EXPLICIT"
Data "EXPLICITARRAY"
Data "FILEEXISTS"
Data "FILES"
Data "FINISHDROP"
Data "FLOAT"
Data "FONT"
Data "FONTHEIGHT"
Data "FONTWIDTH"
Data "FREEFONT"
Data "FREEIMAGE"
Data "FREETIMER"
Data "FULLPATH"
Data "FULLSCREEN"
Data "G2D"
Data "G2R"
Data "GLRENDER"
Data "GREEN"
Data "GREEN32"
Data "HEIGHT"
Data "HIDE"
Data "HYPOT"
Data "ICON"
Data "INCLERRORFILE"
Data "INCLERRORLINE"
Data "INFLATE"
Data "INPUTBOX"
Data "INSTRREV"
Data "INTEGER64"
Data "KEYCLEAR"
Data "KEYHIT"
Data "KEYDOWN"
Data "LASTAXIS"
Data "LASTBUTTON"
Data "LASTWHEEL"
Data "LIMIT"
Data "LOADFONT"
Data "LOADIMAGE"
Data "MAPTRIANGLE"
Data "MAPUNICODE"
Data "MD5$"
Data "MEM"
Data "MEM"
Data "MEMCOPY"
Data "MEMELEMENT"
Data "MEMEXISTS"
Data "MEMFILL"
Data "MEMFREE"
Data "MEMGET"
Data "MEMGET"
Data "MEMIMAGE"
Data "MEMNEW"
Data "MEMPUT"
Data "MEMSOUND"
Data "MESSAGEBOX"
Data "MIDDLE"
Data "MIDISOUNDBANK"
Data "MK"
Data "MOUSEBUTTON"
Data "MOUSEHIDDEN"
Data "MOUSEHIDE"
Data "MOUSEINPUT"
Data "MOUSEMOVE"
Data "MOUSEMOVEMENTX"
Data "MOUSEMOVEMENTY"
Data "MOUSESHOW"
Data "MOUSEWHEEL"
Data "MOUSEX"
Data "MOUSEY"
Data "NEGATE"
Data "NEWIMAGE"
Data "NOTIFYPOPUP"
Data "NUMLOCK"
Data "OFFSET"
Data "OPENCLIENT"
Data "OPENCONNECTION"
Data "OPENFILEDIALOG"
Data "OPENHOST"
Data "ORELSE"
Data "OS"
Data "PALETTECOLOR"
Data "PI"
Data "PIXELSIZE"
Data "PRESERVE"
Data "PRINTIMAGE"
Data "PRINTMODE"
Data "PRINTSTRING"
Data "PRINTWIDTH"
Data "PUTIMAGE"
Data "R2D"
Data "R2G"
Data "RED"
Data "RED32"
Data "READBIT"
Data "READFILE"
Data "RESETBIT"
Data "RESIZE"
Data "RESIZEHEIGHT"
Data "RESIZEWIDTH"
Data "RGB"
Data "RGB32"
Data "RGBA"
Data "RGBA32"
Data "ROL"
Data "ROR"
Data "ROUND"
Data "SAVEFILEDIALOG"
Data "SAVEIMAGE"
Data "SEC"
Data "SECH"
Data "SELECTFOLDERDIALOG"
Data "SCREENCLICK"
Data "SCREENEXISTS"
Data "SCREENICON"
Data "SCREENIMAGE"
Data "SCREENMOVE"
Data "SCREENPRINT"
Data "SCREENSHOW"
Data "SCREENHIDE"
Data "SCREENX"
Data "SCREENY"
Data "SCROLLLOCK"
Data "SETALPHA"
Data "SETBIT"
Data "SHELLHIDE"
Data "SHL"
Data "SHR"
Data "SINH"
Data "SMOOTH"
Data "SNDBAL"
Data "SNDCLOSE"
Data "SNDCOPY"
Data "SNDGETPOS"
Data "SNDLEN"
Data "SNDLIMIT"
Data "SNDLOOP"
Data "SNDNEW"
Data "SNDOPEN"
Data "SNDOPENRAW"
Data "SNDPAUSE"
Data "SNDPAUSED"
Data "SNDPLAY"
Data "SNDPLAYCOPY"
Data "SNDPLAYFILE"
Data "SNDPLAYING"
Data "SNDRATE"
Data "SNDRAW"
Data "SNDRAWDONE"
Data "SNDRAWLEN"
Data "SNDSETPOS"
Data "SNDSTOP"
Data "SNDVOL"
Data "SOURCE"
Data "STARTDIR"
Data "STATUSCODE"
Data "STRCMP"
Data "STRICMP"
Data "TANH"
Data "TITLE"
Data "TOGGLEBIT"
Data "TOTALDROPPEDFILES"
Data "TRIM"
Data "UCHARPOS"
Data "UFONTHEIGHT"
Data "ULINESPACING"
Data "UNSIGNED"
Data "UPRINTSTRING"
Data "UPRINTWIDTH"
Data "WHEEL"
Data "WIDTH"
Data "WINDOWHANDLE"
Data "WINDOWHASFOCUS"
Data "WRITEFILE"



