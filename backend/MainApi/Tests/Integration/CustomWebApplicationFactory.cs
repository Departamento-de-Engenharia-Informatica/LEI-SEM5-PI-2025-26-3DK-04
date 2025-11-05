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

namespace DDDNetCore.Tests.Integration
{
    public class CustomWebApplicationFactory<TStartup> : WebApplicationFactory<TStartup> where TStartup : class
    {
        protected override void ConfigureWebHost(IWebHostBuilder builder)
        {
            // Try to locate the project content root (folder containing a .csproj) by walking
            // up from the test assembly location. This prevents the TestServer from trying
            // to use a missing content root path when tests run from bin folders.
            var appAssembly = typeof(TStartup).Assembly;
            var appPath = Path.GetDirectoryName(appAssembly.Location) ?? Directory.GetCurrentDirectory();
            var directoryInfo = new DirectoryInfo(appPath);
            DirectoryInfo projectDir = null;
            while (directoryInfo != null)
            {
                // look for any .csproj in this directory
                if (directoryInfo.EnumerateFiles("*.csproj").Any())
                {
                    projectDir = directoryInfo;
                    break;
                }
                directoryInfo = directoryInfo.Parent;
            }

            if (projectDir != null)
            {
                builder.UseContentRoot(projectDir.FullName);
            }
            else
            {
                // fallback to current directory if we couldn't find a .csproj
                builder.UseContentRoot(Directory.GetCurrentDirectory());
            }

            builder.UseEnvironment("Testing"); // use 'Testing' so Startup can detect test runs
            builder.ConfigureServices(services =>
            {
                // When running tests we rely on Startup to register the InMemory provider
                // (Startup detects the "Testing" environment). Don't add a second provider
                // here; just ensure the database is created on the service provider Startup built.
                var sp = services.BuildServiceProvider();
                using (var scope = sp.CreateScope())
                {
                    // Try to get the application's DbContext if it was registered by Startup.
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
}