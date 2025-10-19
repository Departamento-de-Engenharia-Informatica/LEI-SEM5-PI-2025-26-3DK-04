using DDDNetCore.Domain.PortInfrastructure.StorageArea;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.EntityFrameworkCore.Storage.ValueConversion;
using DDDSample1.Infrastructure;
using DDDSample1.Infrastructure.Categories;
using DDDSample1.Infrastructure.Products;
using DDDSample1.Infrastructure.Families;
using DDDSample1.Infrastructure.Vessels;
using DDDSample1.Infrastructure.StaffMembers;
using DDDSample1.Infrastructure.Qualifications;
using DDDSample1.Infrastructure.Shared;
using Swashbuckle.AspNetCore.SwaggerGen;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Categories;
using DDDSample1.Domain.Docks;
using DDDSample1.Domain.Products;
using DDDSample1.Domain.Families;
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

namespace DDDSample1
{
    public class Startup
    {
        public Startup(IConfiguration configuration)
        {
            Configuration = configuration;
        }

        public IConfiguration Configuration { get; }

        // This method gets called by the runtime. Use this method to add services to the container.
        public void ConfigureServices(IServiceCollection services)
        {
            services.AddDbContext<DDDSample1DbContext>(opt =>
                opt.UseInMemoryDatabase("DDDSample1DB")
                .ReplaceService<IValueConverterSelector, StronglyEntityIdValueConverterSelector>());

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
        }

        // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
        public void Configure(IApplicationBuilder app, IWebHostEnvironment env)
        {
            if (env.IsDevelopment())
            {
                app.UseDeveloperExceptionPage();
                // Add Swagger middleware
                app.UseSwagger();
                app.UseSwaggerUI();
            }
            else
            {
                // The default HSTS value is 30 days. You may want to change this for production scenarios, see https://aka.ms/aspnetcore-hsts.
                app.UseHsts();
            }

            app.UseHttpsRedirection();

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

            services.AddTransient<ICategoryRepository,CategoryRepository>();
            services.AddTransient<CategoryService>();

            services.AddTransient<IProductRepository,ProductRepository>();
            services.AddTransient<ProductService>();

            services.AddTransient<IFamilyRepository,FamilyRepository>();
            services.AddTransient<FamilyService>();
            
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
        }
    }
}
