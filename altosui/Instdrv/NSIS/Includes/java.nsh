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

Function DetectJRE
  ReadRegStr $2 HKLM "SOFTWARE\JavaSoft\Java Runtime Environment" \
             "CurrentVersion"

  DetailPrint "Desired Java version ${JRE_VERSION}"
  DetailPrint "Actual Java version $2"

  ${VersionCompare} $2 ${JRE_VERSION} $3

  IntCmp $3 1 done done

  Call GetJRE

done:

FunctionEnd
