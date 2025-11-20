using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.PortInfrastructure.StorageArea;
using DDDSample1.Domain.Docks;
using DDDSample1.Domain.Shared;
using DDDSample1.Infrastructure;

namespace DDDSample1.Infrastructure.StorageAreas
{
    internal class StorageAreaEntityTypeConfiguration : IEntityTypeConfiguration<StorageArea>
    {
        public void Configure(EntityTypeBuilder<StorageArea> builder)
        {
            builder.HasKey(sa => sa.Id);

            builder.Property(sa => sa.Id)
                   .HasConversion(id => id.AsGuid(), guid => new StorageAreaID(guid))
                   .ValueGeneratedNever();

            builder.Property(sa => sa.StorageAreaType)
                   .IsRequired();

            builder.OwnsOne(sa => sa.Location, loc =>
            {
                 loc.Property(p => p.Coordinates).IsRequired();
                 loc.Property(p => p.Description).IsRequired();
            });

            builder.Property(sa => sa.Code)
                   .IsRequired()
                   .HasMaxLength(50);

            builder.Property(sa => sa.Designation)
                   .IsRequired()
                   .HasMaxLength(100);

            builder.Property(sa => sa.MaxCapacityTEUs)
                   .IsRequired();

            builder.Property(sa => sa.CurrentOccupancyTEUs)
                   .IsRequired();

             builder.Property(sa => sa.Active)
                   .IsRequired();

             builder.OwnsMany(sa => sa.DockAssignments, da =>
             {
                 da.ToTable("StorageDockAssignments");

                 da.WithOwner().HasForeignKey("StorageAreaId");

                 da.Property(d => d.DockId)
                     .HasConversion(id => id.AsGuid(), guid => new DockID(guid))
                     .IsRequired();

                 da.Property(d => d.DistanceMeters)
                     .IsRequired();

                 da.HasKey("StorageAreaId", "DockId");
             });

            builder.HasIndex(sa => sa.Code).IsUnique();
            
            builder.Property(sa => sa.Length)
                   .IsRequired();
            builder.Property(sa => sa.Width)
                   .IsRequired();
            builder.Property(sa => sa.Heigth)
                .IsRequired();
        }
    }
}