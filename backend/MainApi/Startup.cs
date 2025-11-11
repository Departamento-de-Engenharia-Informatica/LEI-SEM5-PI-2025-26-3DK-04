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

        // This method gets called by the runtime. Use this method to add services to the container.
        public void ConfigureServices(IServiceCollection services)
        {
            // services.AddDbContext<DDDSample1DbContext>(opt =>
            //     opt.UseInMemoryDatabase("DDDSample1DB")
            //     .ReplaceService<IValueConverterSelector, StronglyEntityIdValueConverterSelector>());

            // Conditionally register the ReplaceService used to plug the project's
            // StronglyEntityIdValueConverterSelector. ReplaceService interacts with
            // EF Core's internal service provider and can cause conflicts when the
            // test host constructs its own provider (UseInternalServiceProvider).
            // To avoid test-time issues we skip ReplaceService when running under
            // the "Testing" environment (set by the test factory).
            var connectionString = Configuration.GetConnectionString("DefaultConnection");

            // Use InMemory provider during tests to avoid mixing relational providers
            // in the same service provider. We detect the environment via the
            // injected IWebHostEnvironment (set by the test factory with UseEnvironment).
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
            
            // Add Swagger services
            services.AddEndpointsApiExplorer();
            services.AddSwaggerGen(c =>
            {
                // Configurar Swagger para mostrar enums como strings em vez de números
                c.SchemaFilter<EnumSchemaFilter>();        // Para schemas (body)
                c.ParameterFilter<EnumParameterFilter>();  // Para parâmetros (query, path)
            });

            services.AddControllers().AddNewtonsoftJson(options =>
            {
                // Configurar serialização de enums como strings em vez de números
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

        // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
        public void Configure(IApplicationBuilder app, IWebHostEnvironment env)
        {
            if (env.IsDevelopment())
            {
                app.UseDeveloperExceptionPage();
                // Add Swagger middleware
                app.UseSwagger();
                app.UseSwaggerUI(c =>
                {
                    c.SwaggerEndpoint("/swagger/v1/swagger.json", "DDDNetCore API V1");
                    c.RoutePrefix = string.Empty;
                });
            }
            else
            {
                // The default HSTS value is 30 days. You may want to change this for production scenarios, see https://aka.ms/aspnetcore-hsts.
                app.UseHsts();
            }
            
            app.UseHttpsRedirection(); // Movido para depois do UseCors
            app.UseCors();    // <-- Mova UseCors() para ANTES
            app.UseRouting();
            app.UseAuthorization();

            app.UseEndpoints(endpoints =>
            {
                endpoints.MapControllers();
            });
        }

        public void ConfigureMyServices(IServiceCollection services)
        {
            services.AddTransient<IUnitOfWork,UnitOfWork>();
            
            services.AddTransient<IVesselVisitNotificationRepository,VesselVisitNotificationRepository>();
            services.AddTransient<VesselVisitNotificationService>();
            
            services.AddTransient<IVesselTypeRepository,VesselTypeRepository>();
            services.AddTransient<VesselTypeService>();
            
            services.AddTransient<IVesselRepository,VesselRepository>();
            services.AddTransient<VesselService>();
            
            services.AddTransient<IStorageAreaRepository,StorageAreaRepository>();
            services.AddTransient<StorageAreaService>();
            
            services.AddTransient<IStaffMemberRepository,StaffMemberRepository>();
            services.AddTransient<StaffMemberService>();
            
            services.AddTransient<IQualificationRepository,QualificationRepository>();
            services.AddTransient<QualificationService>();
            
            services.AddTransient<IDockRepository,DockRepository>();
            services.AddTransient<DockService>();
            
            services.AddTransient<IOrganizationRepository,OrganizationRepository>();
            services.AddTransient<OrganizationService>();
            
            services.AddTransient<IRepresentativeRepository,RepresentativeRepository>();
            services.AddTransient<RepresentativeService>();
            
            services.AddTransient<IPhysicalResourceRepository,PhysicalResourcesRepository>();
            services.AddTransient<PhysicalResourceService>();

            services.AddTransient<IUserRepository,UserRepository>();
            
            services.AddTransient<GoogleAuthService>();

            services.AddTransient<IUserActivationRepository, UserActivationRepository>();
            services.AddTransient<EmailService>();
        }
    }
}
