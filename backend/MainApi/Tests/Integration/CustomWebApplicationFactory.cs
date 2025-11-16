using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.DependencyInjection.Extensions;
using Microsoft.EntityFrameworkCore;
using Microsoft.Data.Sqlite;
using System.Data.Common;
using System.IO;
using DDDSample1.Infrastructure;
using System.Linq;
using Microsoft.AspNetCore.Authentication;
using Microsoft.Extensions.Logging;
using System.Text.Encodings.Web;
using Microsoft.AspNetCore.Http;
using System.Threading.Tasks;
using System.Security.Claims;
using Microsoft.Extensions.Options;

namespace DDDNetCore.Tests.Integration
{
    public class CustomWebApplicationFactory<TStartup> : WebApplicationFactory<TStartup> where TStartup : class
    {
        protected override void ConfigureWebHost(IWebHostBuilder builder)
        {
            // Encontrar content root corretamente
            var appAssembly = typeof(TStartup).Assembly;
            var appPath = Path.GetDirectoryName(appAssembly.Location) ?? Directory.GetCurrentDirectory();
            var directoryInfo = new DirectoryInfo(appPath);
            DirectoryInfo projectDir = null;

            while (directoryInfo != null)
            {
                if (directoryInfo.EnumerateFiles("*.csproj").Any())
                {
                    projectDir = directoryInfo;
                    break;
                }
                directoryInfo = directoryInfo.Parent;
            }

            builder.UseContentRoot(projectDir?.FullName ?? Directory.GetCurrentDirectory());

            builder.UseEnvironment("Testing");

            builder.ConfigureServices(services =>
            {
                //
                // 1) ADICIONAR AUTENTICAÇÃO FAKE PARA TESTES
                //
                services.AddAuthentication("Test")
                    .AddScheme<AuthenticationSchemeOptions, TestAuthHandler>("Test", options => { });

                //
                // 2) GARANTIR QUE O BD É CRIADA DE NOVO EM CADA TESTE
                //
                var sp = services.BuildServiceProvider();
                using (var scope = sp.CreateScope())
                {
                    var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                    if (db != null)
                    {
                        db.Database.EnsureDeleted();
                        db.Database.EnsureCreated();
                    }
                }
            });
        }
    }

    //
    // HANDLER DE AUTENTICAÇÃO FAKE
    //
    public class TestAuthHandler : AuthenticationHandler<AuthenticationSchemeOptions>
    {
        public TestAuthHandler(
            IOptionsMonitor<AuthenticationSchemeOptions> options,
            ILoggerFactory logger,
            UrlEncoder encoder,
            ISystemClock clock)
            : base(options, logger, encoder, clock)
        { }

        protected override Task<AuthenticateResult> HandleAuthenticateAsync()
        {
            var claims = new[]
            {
                new Claim("role", "Admin"), // <-- OBRIGATÓRIO para bater certo com AuthorizeRole
                new Claim(ClaimTypes.NameIdentifier, "IntegrationTestUser"),
                new Claim(ClaimTypes.Name, "Test User")
            };

            var identity = new ClaimsIdentity(claims, "Test");
            var principal = new ClaimsPrincipal(identity);
            var ticket = new AuthenticationTicket(principal, "Test");

            return Task.FromResult(AuthenticateResult.Success(ticket));
        }
    }
}
