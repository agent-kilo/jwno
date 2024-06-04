param (
    [string]$msi_file = "installer.msi",
    [string]$app_folder = "janet"
)

$log_file = "janet_installation.log"

$main_process = Start-Process -FilePath "C:\WINDOWS\system32\msiexec.exe" -ArgumentList "/quiet /passive /qn /l* $log_file /i $msi_file APPLICATIONFOLDER=`"$app_folder`"" -NoNewWindow -PassThru
$main_process.WaitForExit()

$log_process = Start-Process "powershell" "Get-Content -Path $log_file" -NoNewWindow -PassThru
$log_process.WaitForExit()

exit $main_process.ExitCode
