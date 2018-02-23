# ﻿WES PORTER
# Script Summary: Send me an email when an AnnAGNPS simulation finishes.

Read-Host -AsSecureString | ConvertFrom-SecureString | Out-File -FilePath wsp2sgis@gmail.com.securestring

$b = 1

do

    {

        $a = get-process -ID 1104

        $a.waitforexit()

        $b = 2



        $From = "wsp2sgis@gmail.com"
        $To = "wsp2sgis@gmail.com"
        $Subject = "TEST 1"
        $Body = Get-Content "E:/Wes/School/Fall_2017/GSA/Lab7/AnnAGNPS_Sims/Initial/AnnAGNPS_LOG_All.csv" | select -Last 10 | Out-String
        $SMTPServer = "smtp.gmail.com"
        $SMTPUsername = "wsp2sgis@gmail.com"
        $EncryptedPasswordFile = "wsp2sgis@gmail.com.securestring"
        $SecureStringPassword = Get-Content -Path $EncryptedPasswordFile | ConvertTo-SecureString
        $EmailCredential = New-Object -TypeName System.Management.Automation.PSCredential -ArgumentList $SMTPUsername,$SecureStringPassword
        $SMTPPort = "587"
        Send-MailMessage -From $From -to $To -Subject $Subject -Body $Body -SmtpServer $SMTPServer -port $SMTPPort -UseSsl `
                         -Credential $EmailCredential

    }

while ($b -eq 1)

cd "E:/Wes/School/Fall_2017/GSA/Lab7/AnnAGNPS_Sims/K0/"

start-process 'E:/Wes/School/Fall_2017/GSA/Lab7/AnnAGNPS_Sims/K0/AnnAGNPS.exe'

$Subject = "AnnAGNPS Update: Initializing K0"
$Body = "Beginning execution of K0."

Send-MailMessage -From $From -to $To -Subject $Subject -Body $Body -SmtpServer $SMTPServer -port $SMTPPort -UseSsl `
                         -Credential $EmailCredential

$b = 1

do

    {

        $a = get-process AnnAGNPS

        $a.waitforexit()

        $b = 2



        $Subject = "AnnAGNPS Update: K0"
        $Body = Get-Content "E:/Wes/School/Fall_2017/GSA/Lab7/AnnAGNPS_Sims/K0/AnnAGNPS_LOG_All.csv" | select -Last 10 | Out-String

        Send-MailMessage -From $From -to $To -Subject $Subject -Body $Body -SmtpServer $SMTPServer -port $SMTPPort -UseSsl `
                         -Credential $EmailCredential

    }

while ($b -eq 1)
