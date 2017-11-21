# WES PORTER
# Script Summary: Send me an email when an AnnAGNPS simulation finishes. 

Read-Host -AsSecureString | ConvertFrom-SecureString | Out-File -FilePath wsp2sgis@gmail.com.securestring

$b = 1

do

    {

        $a = get-process Calculator

        $a.waitforexit()

        $b = 2

       

        $From = "wsp2sgis@gmail.com"
        $To = "wsp2sgis@gmail.com"
        $Subject = "AnnAGNPS Update"
        $Body = "Insert body text here"
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



