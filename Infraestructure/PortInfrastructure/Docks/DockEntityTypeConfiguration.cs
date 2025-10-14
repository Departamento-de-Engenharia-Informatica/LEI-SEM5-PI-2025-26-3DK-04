using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.Docks;

namespace DDDSample1.Infrastructure.PortInfrastructure
{
    internal class DockEntityTypeConfiguration : IEntityTypeConfiguration<Dock>
    {
        public void Configure(EntityTypeBuilder<Dock> builder)
        {
            builder.ToTable("Docks", SchemaNames.DDDSample1);

            builder.HasKey(d => d.Id);

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
            builder.OwnsOne(d => d.Location, loc =>
            {
                loc.Property(l => l.Coordinates)
                    .HasColumnName("Coordinates")
                    .HasMaxLength(100);

                loc.Property(l => l.Description)
                    .HasColumnName("LocationDescription")
                    .HasMaxLength(200);
            });

            // Configure AllowedVesselTypes as a many-to-many relationship
            builder.HasMany(d => d.AllowedVesselTypes)
                .WithMany()
                .UsingEntity(j => j.ToTable("DockVesselTypes", SchemaNames.DDDSample1));
        }
    }
}