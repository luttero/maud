@echo on
setlocal

set "SRC_VERSION="
for /f "tokens=2 delims==;" %%v in ('findstr /r /c:"double maud_version *=" src\it\unitn\ing\rista\util\Constants.java') do set "SRC_VERSION=%%v"
if defined SRC_VERSION set "SRC_VERSION=%SRC_VERSION: =%"
if not "%SRC_VERSION%"=="%PKG_VERSION%" (
  echo recipe version %PKG_VERSION% differs from maud_version "%SRC_VERSION%" in Constants.java; update the version in recipe\recipe.yaml 1>&2
  exit 1
)

if not exist build_numbers mkdir build_numbers
if not exist ant_home mkdir ant_home
type nul > build_numbers\Maud_full_build.number
set "SRC_FWD=%SRC_DIR:\=/%"
set "JH_FWD=%JAVA_HOME:\=/%"
(
  echo java_version=21
  echo build=%SRC_FWD%/out
  echo JAVA_HOME=%JH_FWD%
  echo openjdk=%JH_FWD%
  echo installerDir=%SRC_FWD%/inst
) > maud_build.properties
rem ant.bat splits unquoted arguments at "=", so the -D argument is quoted whole.
call ant "-Duser.home=%SRC_DIR%\ant_home" -propertyfile maud_build.properties build_full
if errorlevel 1 exit 1

if not exist "%PREFIX%\share\maud\lib" mkdir "%PREFIX%\share\maud\lib"
for %%j in (libs\current\*.jar) do (
  echo %%~nxj| findstr /b /l /c:"._" >nul || copy /y "%%j" "%PREFIX%\share\maud\lib\" >nul
)
copy /y out\Maud.jar "%PREFIX%\share\maud\lib\Maud.jar"
if errorlevel 1 exit 1
copy /y LICENSE "%PREFIX%\share\maud\LICENSE"
if errorlevel 1 exit 1
