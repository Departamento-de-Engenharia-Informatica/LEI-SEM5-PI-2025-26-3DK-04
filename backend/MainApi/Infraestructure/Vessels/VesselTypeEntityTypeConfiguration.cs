using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.Vessels;

namespace DDDSample1.Infrastructure.Vessels
{
    internal class VesselTypeEntityTypeConfiguration : IEntityTypeConfiguration<VesselType>
    {
        public void Configure(EntityTypeBuilder<VesselType> builder)
        {
            builder.HasKey(vt => vt.Id);

            // Convert strong-typed ID to Guid for relational providers (SQLite/Postgres)
            builder.Property(vt => vt.Id)
                .HasConversion(
                    id => id.AsGuid(),
                    guid => new VesselTypeId(guid))
                .ValueGeneratedNever();

            builder.Property(vt => vt.Name)
                .IsRequired()
                .HasMaxLength(100);

            builder.Property(vt => vt.Description)
                .IsRequired()
                .HasMaxLength(500);

            builder.Property(vt => vt.Capacity)
                .IsRequired();

            builder.Property(vt => vt.MaxRows)
                .IsRequired();

            builder.Property(vt => vt.MaxBays)
                .IsRequired();

            builder.Property(vt => vt.MaxTiers)
                .IsRequired();

            builder.Property(vt => vt.Active)
                .IsRequired();

            // Create an index on Name for better search performance
            builder.HasIndex(vt => vt.Name);
        }
    }
}
