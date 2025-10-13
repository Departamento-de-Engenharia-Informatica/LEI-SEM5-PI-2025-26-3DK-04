using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.Qualifications;

namespace DDDSample1.Infrastructure.Qualifications
{
    internal class QualificationEntityTypeConfiguration : IEntityTypeConfiguration<Qualification>
    {
        public void Configure(EntityTypeBuilder<Qualification> builder)
        {
            builder.ToTable("Qualifications", SchemaNames.DDDSample1);
            
            // Chave primária
            builder.HasKey(b => b.Id);
            
            // Propriedade Name
            builder.Property(b => b.Name)
                .IsRequired()
                .HasMaxLength(200);
            
            // Índice único para Name (evitar duplicados ao nível da base de dados)
            builder.HasIndex(b => b.Name)
                .IsUnique();
            
            // Propriedade Description
            builder.Property(b => b.Description)
                .HasMaxLength(500);
        }
    }
}
