using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.Qualifications;

namespace DDDSample1.Infrastructure.Qualifications
{
    internal class QualificationEntityTypeConfiguration : IEntityTypeConfiguration<Qualification>
    {
        public void Configure(EntityTypeBuilder<Qualification> builder)
        {
            // Chave primária
            builder.HasKey(b => b.Id);

            // Convert strong-typed ID to Guid for relational providers (SQLite/Postgres)
            builder.Property(b => b.Id)
                .HasConversion(
                    id => id.AsGuid(),
                    guid => new QualificationID(guid))
                .ValueGeneratedNever();
            
            // Propriedade Name
            builder.Property(b => b.Name)
                .IsRequired()
                .HasMaxLength(150);
        }
    }
}
