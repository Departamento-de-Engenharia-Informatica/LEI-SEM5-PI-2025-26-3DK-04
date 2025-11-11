using DDDSample1.Domain.Authentication;
using Microsoft.EntityFrameworkCore;
using DDDSample1.Domain.Organizations;
using DDDSample1.Domain.Vessels;
using DDDSample1.Domain.Vessels.VesselVisitNotification;
using DDDSample1.Domain.Docks;
using DDDSample1.Domain.PhysicalResources;
using DDDSample1.Domain.PortInfrastructure.StorageArea;
using DDDSample1.Domain.Qualifications;
using DDDSample1.Domain.StaffMembers;
using DDDSample1.Infrastructure.Authentication;
using DDDSample1.Infrastructure.Organizations;
using DDDSample1.Infrastructure.Vessels;
using DDDSample1.Infrastructure.Docks;
using DDDSample1.Infrastructure.PhysicalResources;
using DDDSample1.Infrastructure.PortInfrastructure;
using DDDSample1.Infrastructure.StorageAreas;
using DDDSample1.Infrastructure.Qualifications;
using DDDSample1.Infrastructure.StaffMembers;

namespace DDDSample1.Infrastructure
{
    public class DDDSample1DbContext : DbContext
    {
        public DbSet<VesselVisitNotification> VesselVisitNotifications { get; set; }
        public DbSet<VesselType> VesselTypes { get; set; }
        public DbSet<Vessel> Vessels { get; set; }
        public DbSet<Organization> Organizations { get; set; }
        public DbSet<Representative> Representatives { get; set; }

        public DbSet<Dock> Docks { get; set; }
        public DbSet<StorageArea> StorageAreas { get; set; }
        public DbSet<Qualification> Qualifications { get; set; }
        public DbSet<StaffMember> StaffMembers { get; set; }
        public DbSet<PhysicalResource> PhysicalResources { get; set; }
        public DbSet<User> Users { get; set; }
        public DbSet<UserActivation> UserActivations { get; set; }
        public DDDSample1DbContext(DbContextOptions options) : base(options)
        {
        }

        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            modelBuilder.ApplyConfiguration(new VesselVisitNotificationEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new VesselTypeEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new VesselEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new OrganizationEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new RepresentativeEntityTypeConfiguration()); 
            modelBuilder.ApplyConfiguration(new DockEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new StorageAreaEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new QualificationEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new StaffMemberEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new PhysicalResourceEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new UserEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new UserActivationEntityTypeConfiguration());

            // modelBuilder.Ignore<QualificationID>();
        }
    }
}
