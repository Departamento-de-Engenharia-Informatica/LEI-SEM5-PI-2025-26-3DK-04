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

            // Configure IMO Number as a simple string
            builder.Property(v => v.ImoNumber)
                .IsRequired()
                .HasMaxLength(15); // IMO + 7 digits = 10 chars, but allowing some buffer

            builder.Property(v => v.Name)
                .IsRequired()
                .HasMaxLength(200);

            builder.Property(v => v.VesselTypeId)
                .IsRequired();

            builder.Property(v => v.Owner)
                .IsRequired()
                .HasMaxLength(200);

            builder.Property(v => v.Operator)
                .IsRequired()
                .HasMaxLength(200);

            builder.Property(v => v.Active)
                .IsRequired();

            // Create indexes for search performance
            builder.HasIndex(v => v.Name);
            builder.HasIndex(v => v.Owner);
            builder.HasIndex(v => v.Operator);
        }
    }
}
