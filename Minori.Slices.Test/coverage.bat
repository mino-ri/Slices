rd /s /q "./coveragereport"
rd /s /q "./TestResults"

set "processOutput="
for /f "delims=" %%i in ('dotnet test --collect:"XPlat Code Coverage"') do set "processOutput=%%i"
set "lastOutput=%processOutput: =%"

reportgenerator -reports:"%lastOutput%" -targetdir:"coveragereport" -reporttypes:Html

start "" "./coveragereport/index.html"

pause