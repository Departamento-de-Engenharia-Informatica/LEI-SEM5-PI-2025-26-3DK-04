using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.Organizations;

namespace DDDSample1.Infrastructure.Organizations
{
    internal class OrganizationEntityTypeConfiguration : IEntityTypeConfiguration<Organization>
    {
        public void Configure(EntityTypeBuilder<Organization> builder)
        {
            // Chave primária
            builder.HasKey(o => o.Id);

            // Id fornecido pelo usuário, máximo 10 caracteres
            builder.Property(o => o.Id)
                .HasConversion(
                    id => id.AsString(),
                    value => new OrganizationId(value))
                .HasMaxLength(10)  // limite de 10 caracteres
                .IsRequired()
                .ValueGeneratedNever();

            // Legal Name
            builder.Property(o => o.LegalName)
                .IsRequired()
                .HasMaxLength(200);

            // Alternative Name
            builder.Property(o => o.AlternativeName)
                .HasMaxLength(200);

            // Address
            builder.Property(o => o.Address)
                .IsRequired()
                .HasMaxLength(500);

            // Tax Number (obrigatório)
            builder.Property(o => o.TaxNumber)
                .IsRequired()
                .HasMaxLength(50);

            // Relação com Representatives (NÃO OWNED!)
            builder.HasMany(o => o.Representatives)
                .WithOne() // Se Representative tiver uma propriedade Organization, substitui por .WithOne(r => r.Organization)
                .HasForeignKey(r => r.OrganizationId)
                .OnDelete(DeleteBehavior.Cascade);

            // Configuração para EF Core acessar campo privado
            builder.Metadata.FindNavigation(nameof(Organization.Representatives))
                ?.SetPropertyAccessMode(PropertyAccessMode.Field);
        }
    }
}