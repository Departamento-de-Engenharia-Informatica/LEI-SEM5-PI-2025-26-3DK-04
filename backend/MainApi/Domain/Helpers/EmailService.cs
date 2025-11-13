using System.Net;
using System.Net.Mail;

public class EmailService
{
    private readonly string _from = "gmail";
    private readonly string _password = "password";

    public void SendActivationEmail(string toEmail, string activationLink)
    {
        using var client = new SmtpClient("smtp.gmail.com", 587)
        {
            EnableSsl = true,
            Credentials = new NetworkCredential(_from, _password)
        };

        var mail = new MailMessage(_from, toEmail)
        {
            Subject = "Activate your account",
            Body = $"Click the link to activate your account:\n\n{activationLink}",
            IsBodyHtml = false
        };
        client.Send(mail);
    }
}