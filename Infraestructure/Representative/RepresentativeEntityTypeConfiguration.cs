using System;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.Organizations;

namespace DDDSample1.Infrastructure.Organizations
{
    internal class RepresentativeEntityTypeConfiguration : IEntityTypeConfiguration<Representative>
    {
        public void Configure(EntityTypeBuilder<Representative> builder)
        {
            // Primary Key
            builder.HasKey(r => r.Id);

            builder.Property(r => r.Id)
                .HasConversion(
                    id => id.AsString(),
                    value => new RepresentativeId(value))
                .IsRequired()
                .ValueGeneratedNever();

            // FK para Organização
            builder.Property(r => r.OrganizationId)
                .IsRequired(false); // pode ser null antes de associar a org

            // Campos do representante
            builder.Property(r => r.Name)
                .IsRequired()
                .HasMaxLength(150);

            builder.Property(r => r.CitizenId)
                .IsRequired()
                .HasMaxLength(50);

            builder.Property(r => r.Nationality)
                .IsRequired()
                .HasMaxLength(50);

            builder.Property(r => r.Email)
                .IsRequired()
                .HasMaxLength(150);

            builder.Property(r => r.PhoneNumber)
                .IsRequired()
                .HasMaxLength(50);

            builder.Property(r => r.Status)
                .IsRequired()
                .HasConversion<string>(); // Armazena enum como string

            // Índices opcionais
            builder.HasIndex(r => r.Email).IsUnique();
        }
    }
}