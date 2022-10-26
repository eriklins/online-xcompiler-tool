@echo off
SET THEFILE=onlinexcomptool
echo Assembling %THEFILE%
C:\lazarus\fpc\bin\x86_64-win64\as.exe --64 -o C:\Users\erikl\OneDrive\Projekte\lazarus\OnlineXCompTool\lib\x86_64-win64\OnlineXCompTool.o   C:\Users\erikl\OneDrive\Projekte\lazarus\OnlineXCompTool\lib\x86_64-win64\OnlineXCompTool.s
if errorlevel 1 goto asmend
Del C:\Users\erikl\OneDrive\Projekte\lazarus\OnlineXCompTool\lib\x86_64-win64\OnlineXCompTool.s
SET THEFILE=C:\Users\erikl\OneDrive\Projekte\lazarus\OnlineXCompTool\OnlineXCompTool.exe
echo Linking %THEFILE%
C:\lazarus\fpc\bin\x86_64-win64\ld.exe -b pei-x86-64  --gc-sections   --subsystem windows --entry=_WinMainCRTStartup    -o C:\Users\erikl\OneDrive\Projekte\lazarus\OnlineXCompTool\OnlineXCompTool.exe C:\Users\erikl\OneDrive\Projekte\lazarus\OnlineXCompTool\link9624.res
if errorlevel 1 goto linkend
goto end
:asmend
echo An error occurred while assembling %THEFILE%
goto end
:linkend
echo An error occurred while linking %THEFILE%
:end
