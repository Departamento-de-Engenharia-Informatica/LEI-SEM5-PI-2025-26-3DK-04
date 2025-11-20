using System.Net;
using System.Net.Mail;
using Microsoft.Extensions.Configuration;

public class EmailService
{
    //private readonly string _from = "bauleluis2@gmail.com";
    //private readonly string _password = "edcw nmiw gqsf ycpm";
    private readonly IConfiguration _config;

    private readonly string _from;
    private readonly string _password;
    private readonly string _smtpHost;
    private readonly int _smtpPort;

    public EmailService(IConfiguration config)
    {
        _from = config["MailSettings:EmailFrom"];
        _password = config["MailSettings:EmailPassword"];
        _smtpHost = config["MailSettings:SmtpHost"];
        _smtpPort = int.Parse(config["MailSettings:SmtpPort"]);
    }
    
    public void SendActivationEmail(string toEmail, string activationLink)
    {
        using var client = new SmtpClient(_smtpHost, _smtpPort)
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