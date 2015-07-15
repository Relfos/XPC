plex.exe delphi.l ..\XPC_PascalLexer.pas
if %errorlevel% neq 0 exit /b %errorlevel%
plex.exe delphi_preprocessor.l ..\XPC_PascalPreProcessor.pas
if %errorlevel% neq 0 exit /b %errorlevel%
pyacc.exe -d delphi.y ..\XPC_PascalParser.pas