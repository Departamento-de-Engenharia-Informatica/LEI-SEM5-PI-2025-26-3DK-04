using System;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.Organizations;

namespace DDDSample1.Infrastructure.Organizations
{
    internal class OrganizationEntityTypeConfiguration : IEntityTypeConfiguration<Organization>
    {
        public void Configure(EntityTypeBuilder<Organization> builder)
        {
            //builder.ToTable("Organizations", SchemaNames.DDDSample1);

            
            builder.HasKey(o => o.Id);

            builder.Property(o => o.Id)
                .HasConversion(
                    id => id.AsString(),
                    value => new OrganizationId(value))
                .IsRequired();

            
            builder.Property(o => o.LegalName)
                .IsRequired()
                .HasMaxLength(200);

            builder.Property(o => o.AlternativeName)
                .HasMaxLength(200);

            builder.Property(o => o.Address)
                .IsRequired()
                .HasMaxLength(500);

            builder.Property(o => o.TaxNumber)
                .IsRequired()
                .HasMaxLength(50);

          
            builder.OwnsMany(o => o.Representatives, rep =>
            {
                //rep.ToTable("Representatives", SchemaNames.DDDSample1);

                rep.WithOwner().HasForeignKey("OrganizationId");

                rep.Property(r => r.Id)
                    .ValueGeneratedNever();

                rep.Property(r => r.Name)
                    .IsRequired()
                    .HasMaxLength(150);

                rep.Property(r => r.CitizenId)
                    .IsRequired()
                    .HasMaxLength(50);

                rep.Property(r => r.Nationality)
                    .IsRequired()
                    .HasMaxLength(50);

                rep.Property(r => r.Email)
                    .IsRequired()
                    .HasMaxLength(150);

                rep.Property(r => r.PhoneNumber)
                    .IsRequired()
                    .HasMaxLength(50);

                rep.Property<Guid>("OrganizationId"); // FK

                rep.HasKey(r => r.Id);
            });

            builder.Metadata.FindNavigation(nameof(Organization.Representatives))
                ?.SetPropertyAccessMode(PropertyAccessMode.Field);
        }
    }
}
