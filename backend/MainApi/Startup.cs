using DDDNetCore.Domain.PortInfrastructure.StorageArea;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.EntityFrameworkCore.Storage.ValueConversion;
using DDDSample1.Infrastructure;
using DDDSample1.Infrastructure.Vessels;
using DDDSample1.Infrastructure.StaffMembers;
using DDDSample1.Infrastructure.Qualifications;
using DDDSample1.Infrastructure.Shared;
using Swashbuckle.AspNetCore.SwaggerGen;
using DDDSample1.Domain.Shared;
using System;
using DDDSample1.Domain.Authentication;
using DDDSample1.Domain.Docks;
using DDDSample1.Domain.Organizations;
using DDDSample1.Domain.PhysicalResources;
using DDDSample1.Domain.PortInfrastructure.StorageArea;
using DDDSample1.Domain.Vessels;
using DDDSample1.Domain.Vessels.VesselVisitNotification;
using DDDSample1.Domain.StaffMembers;
using DDDSample1.Domain.Qualifications;
using DDDSample1.Infrastructure.Docks;
using DDDSample1.Infrastructure.Organizations;
using DDDSample1.Infrastructure.PhysicalResources;
using DDDSample1.Infrastructure.StorageAreas;
using DDDSample1.Infrastructure.Authentication;

namespace DDDSample1
{
    public class Startup
    {
        private readonly IWebHostEnvironment _env;

        public Startup(IConfiguration configuration, IWebHostEnvironment env)
        {
            Configuration = configuration;
            _env = env;
        }

        public IConfiguration Configuration { get; }

        public void ConfigureServices(IServiceCollection services)
        {
            var connectionString = Configuration.GetConnectionString("DefaultConnection");

            if (_env != null && _env.IsEnvironment("Testing"))
            {
                services.AddDbContext<DDDSample1DbContext>(opt =>
                    opt.UseInMemoryDatabase("DDDSample1TestDB")
                        .ReplaceService<IValueConverterSelector, StronglyEntityIdValueConverterSelector>()
                        .EnableSensitiveDataLogging());
            }
            else
            {
                services.AddDbContext<DDDSample1DbContext>(opt =>
                    opt.UseNpgsql(connectionString)
                        .ReplaceService<IValueConverterSelector, StronglyEntityIdValueConverterSelector>()
                        .EnableSensitiveDataLogging());
            }

            ConfigureMyServices(services);

            // ===== Authorization =====
            // Não usamos JWT Bearer middleware
            services.AddAuthorization();

            // Swagger
            services.AddEndpointsApiExplorer();
            services.AddSwaggerGen(c =>
            {
                c.SchemaFilter<EnumSchemaFilter>();
                c.ParameterFilter<EnumParameterFilter>();
            });

            services.AddControllers().AddNewtonsoftJson(options =>
            {
                options.SerializerSettings.Converters.Add(new Newtonsoft.Json.Converters.StringEnumConverter());
            });

            services.AddCors(options =>
            {
                options.AddDefaultPolicy(builder =>
                {
                    builder.WithOrigins("http://localhost:4200")
                        .AllowAnyHeader()
                        .AllowAnyMethod();
                });
            });
        }

        public void Configure(IApplicationBuilder app, IWebHostEnvironment env)
        {
            if (env.IsDevelopment())
            {
                app.UseDeveloperExceptionPage();

                app.UseSwagger();
                app.UseSwaggerUI(c =>
                {
                    c.SwaggerEndpoint("/swagger/v1/swagger.json", "DDDNetCore API V1");
                    c.RoutePrefix = string.Empty;
                });
            }
            else
            {
                app.UseHsts();
            }

            app.UseHttpsRedirection();
            app.UseCors();

            app.UseRouting();

            // Não é necessário middleware JWT
            // app.UseAuthentication(); 
            app.UseAuthorization();

            app.UseEndpoints(endpoints =>
            {
                endpoints.MapControllers();
            });
        }

        public void ConfigureMyServices(IServiceCollection services)
        {
            services.AddTransient<IUnitOfWork, UnitOfWork>();

            services.AddTransient<IVesselVisitNotificationRepository, VesselVisitNotificationRepository>();
            services.AddTransient<VesselVisitNotificationService>();

            services.AddTransient<IVesselTypeRepository, VesselTypeRepository>();
            services.AddTransient<VesselTypeService>();

            services.AddTransient<IVesselRepository, VesselRepository>();
            services.AddTransient<VesselService>();

            services.AddTransient<IStorageAreaRepository, StorageAreaRepository>();
            services.AddTransient<StorageAreaService>();

            services.AddTransient<IStaffMemberRepository, StaffMemberRepository>();
            services.AddTransient<StaffMemberService>();

            services.AddTransient<IQualificationRepository, QualificationRepository>();
            services.AddTransient<QualificationService>();

            services.AddTransient<IDockRepository, DockRepository>();
            services.AddTransient<DockService>();

            services.AddTransient<IOrganizationRepository, OrganizationRepository>();
            services.AddTransient<OrganizationService>();

            services.AddTransient<IRepresentativeRepository, RepresentativeRepository>();
            services.AddTransient<RepresentativeService>();

            services.AddTransient<IPhysicalResourceRepository, PhysicalResourcesRepository>();
            services.AddTransient<PhysicalResourceService>();

            services.AddTransient<IUserRepository, UserRepository>();
            services.AddTransient<UserService>();

            services.AddTransient<GoogleAuthService>();

            services.AddTransient<IUserActivationRepository, UserActivationRepository>();
            services.AddTransient<EmailService>();
        }
    }
}
