using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.Docks;

namespace DDDSample1.Infrastructure.PortInfrastructure
{
    internal class DockEntityTypeConfiguration : IEntityTypeConfiguration<Dock>
    {
        public void Configure(EntityTypeBuilder<Dock> builder)
        {
            builder.HasKey(d => d.Id);

            // Convert strong-typed ID to Guid for relational providers (SQLite/Postgres)
            builder.Property(d => d.Id)
                .HasConversion(
                    id => id.AsGuid(),
                    guid => new DockID(guid))
                .ValueGeneratedNever();

            builder.Property(d => d.Name)
                .IsRequired()
                .HasMaxLength(100);

            builder.Property(d => d.Length)
                .IsRequired();

            builder.Property(d => d.Depth)
                .IsRequired();

            builder.Property(d => d.MaxDraft)
                .IsRequired();

            // Configure Location as a Value Object
            builder.OwnsOne(d => d.Location);

            // Configure AllowedVesselTypes as a many-to-many relationship
            builder.HasMany(d => d.AllowedVesselTypes)
                .WithMany()
                .UsingEntity("DockVesselTypes");
        }
    }
}