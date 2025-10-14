using DDDSample1.Domain.Docks;
using DDDSample1.Domain.PortInfrastructure.StorageArea;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
//using DDDSample1.Domain.PortInfrastructure.StorageArea;

namespace DDDSample1.Infrastructure.StorageAreas
{
    internal class StorageAreaEntityTypeConfiguration : IEntityTypeConfiguration<StorageArea>
    {
        public void Configure(EntityTypeBuilder<StorageArea> builder)
        {
            builder.ToTable("StorageAreas", SchemaNames.DDDSample1);

            builder.HasKey(sa => sa.Id);
            builder.Property(sa => sa.Id)
                .HasConversion(id => id.AsGuid(), guid => new StorageAreaID(guid))
                .IsRequired();

            builder.Property(sa => sa.Type)
                .IsRequired()
                .HasMaxLength(50);

            builder.Property(sa => sa.Location)
                .IsRequired()
                .HasMaxLength(100);

            builder.Property(sa => sa.MaxCapacityTEUs)
                .IsRequired();

            builder.Property(sa => sa.CurrentOccupancyTEUs)
                .IsRequired();

            builder.OwnsMany(sa => sa.DockAssignments, da =>
            {
                da.ToTable("StorageDockAssignments", SchemaNames.DDDSample1);
                da.WithOwner().HasForeignKey("StorageAreaId");

                da.Property(d => d.DockId)
                    .HasConversion(id => id.AsGuid(), guid => new DockId(guid))
                    .IsRequired();

                da.Property(d => d.DistanceMeters)
                    .IsRequired();

                da.HasKey("StorageAreaId", "DockId");
            });
        }
    }
}
