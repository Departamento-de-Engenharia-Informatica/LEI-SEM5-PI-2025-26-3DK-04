using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.Vessels;

namespace DDDSample1.Infrastructure.Vessels
{
    internal class VesselEntityTypeConfiguration : IEntityTypeConfiguration<Vessel>
    {
        public void Configure(EntityTypeBuilder<Vessel> builder)
        {
            builder.HasKey(v => v.Id);

            // Convert strong-typed ID to Guid for relational providers (SQLite/Postgres)
            builder.Property(v => v.Id)
                .HasConversion(
                    id => id.AsGuid(),
                    guid => new VesselId(guid))
                .ValueGeneratedNever();

            // Configure IMO Number as a simple string
            builder.Property(v => v.ImoNumber)
                .IsRequired()
                .HasMaxLength(15); // IMO + 7 digits = 10 chars, but allowing some buffer

            builder.Property(v => v.Name)
                .IsRequired()
                .HasMaxLength(200);

            // Convert strong-typed VesselTypeId to Guid for relational providers
            builder.Property(v => v.VesselTypeId)
                .HasConversion(
                    id => id.AsGuid(),
                    guid => new VesselTypeId(guid))
                .IsRequired();

            builder.Property(v => v.Owner)
                .IsRequired()
                .HasMaxLength(200);

            builder.Property(v => v.Operator)
                .IsRequired()
                .HasMaxLength(200);

            builder.Property(v => v.Active)
                .IsRequired();

            // Configure Crew as owned entities (value objects)
            builder.OwnsMany(v => v.Crew, crew =>
            {
                // Use a shadow property as the key for the owned entity
                crew.Property<int>("Id");
                crew.HasKey("Id");

                crew.Property(c => c.Name)
                    .IsRequired()
                    .HasMaxLength(150);

                crew.Property(c => c.CitizenId)
                    .IsRequired()
                    .HasMaxLength(50);

                crew.Property(c => c.Nationality)
                    .IsRequired()
                    .HasMaxLength(50);
            });

            // Create indexes for search performance
            builder.HasIndex(v => v.Name);
            builder.HasIndex(v => v.Owner);
            builder.HasIndex(v => v.Operator);
        }
    }
}
