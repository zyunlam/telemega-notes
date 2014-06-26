!include WordFunc.nsh

; Definitions for Java Detection

!define JRE_VERSION "1.6"
!define JRE32_URL "http://javadl.sun.com/webapps/download/AutoDL?BundleId=52247&/jre-6u27-windows-i586.exe"
!define JRE64_URL "http://javadl.sun.com/webapps/download/AutoDL?BundleId=52249&/jre-6u27-windows-x64.exe"

Var JavaDownload
Var JavaBits

Function GetJRE
	${If} ${RunningX64}
	   StrCpy $JavaDownload ${JRE64_URL}
	   StrCpy $JavaBits "64"
	${Else}
	   StrCpy $JavaDownload ${JRE32_URL}
	   StrCpy $JavaBits "32"
	${EndIf}

        MessageBox MB_OK "This product uses Java ${JRE_VERSION}, \
			$JavaBits bits, it will now \
                        be downloaded and installed"

        StrCpy $2 "$TEMP\Java Runtime Environment.exe"
        nsisdl::download /TIMEOUT=30000 $JavaDownload $2
        Pop $R0 ;Get the return value
                StrCmp $R0 "success" +3
                MessageBox MB_OK "Download failed: $R0"
                Quit
        ExecWait $2
        Delete $2
FunctionEnd

Function DoDetectJRE

  DetailPrint "Desired Java version ${JRE_VERSION}"

  ; Check in HKCU for CurrentVersion

  ClearErrors
  ReadRegStr $2 HKCU "SOFTWARE\JavaSoft\Java Runtime Environment" \
             "CurrentVersion"

  IfErrors hklm_version

  DetailPrint "HKEY_CURRENT_USER Java version $2"

  ${VersionCompare} $2 ${JRE_VERSION} $3

  IntCmp $3 1 yes yes no

hklm_version:

  ; Check in HKLM for CurrentVersion

  ClearErrors
  ReadRegStr $2 HKLM "SOFTWARE\JavaSoft\Java Runtime Environment" \
             "CurrentVersion"
  
  IfErrors hkcu_any

  DetailPrint "HKEY_LOCAL_MACHINE Java version $2"

  ${VersionCompare} $2 ${JRE_VERSION} $3

  IntCmp $3 1 yes yes no

hkcu_any:

  ; Check in HKCU for any Java install

  StrCpy $0 0

hkcu_any_loop:
  EnumRegKey $1 HKCU "SOFTWARE\JavaSoft" $0

  StrCmp $1 "Java Runtime Environment" found_hkcu

  StrCmp $1 "" hklm_any
  
  IntOp $0 $0 + 1

  Goto hkcu_any_loop
  
found_hkcu:

  DetailPrint "HKEY_CURRENT_USER has SOFTWARE\JavaSoft\$1"

  Goto maybe

hklm_any:

  ; Check in HKCU for any Java install

  StrCpy $0 0

hklm_any_loop:
  EnumRegKey $1 HKLM "SOFTWARE\JavaSoft" $0

  StrCmp $1 "Java Runtime Environment" found_hklm

  StrCmp $1 "" no
  
  IntOp $0 $0 + 1

  Goto hklm_any_loop
  
found_hklm:

  DetailPrint "HKEY_CURRENT_USER has SOFTWARE\JavaSoft\$1"

  Goto maybe

yes:
  StrCpy $0 2
  Goto done

maybe:
  StrCpy $0 1
  Goto done

no:
  StrCpy $0 0
  Goto done  

done:

FunctionEnd

var dialog
var hwnd
var null

var install
var quit
var skip

Function DetectJRE

  Call DoDetectJRE

  IntCmp $0 1 ask_maybe ask_no yes

ask_no:
  StrCpy $0 "No Java detected. Download and install?"
  Goto ask

ask_maybe:
  StrCpy $0 "Cannot determine installed Java version. Download and install?"
  Goto ask

ask:
  MessageBox MB_YESNOCANCEL $0 IDYES do_java IDNO skip_java

bail:
  Abort

do_java:
  Call GetJRE


skip_java:
yes:

FunctionEnd
