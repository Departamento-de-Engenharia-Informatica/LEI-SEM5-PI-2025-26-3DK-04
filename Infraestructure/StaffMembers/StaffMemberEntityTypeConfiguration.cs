using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.StaffMembers;

namespace DDDSample1.Infrastructure.StaffMembers
{
    internal class StaffMemberEntityTypeConfiguration : IEntityTypeConfiguration<StaffMember>
    {
        public void Configure(EntityTypeBuilder<StaffMember> builder)
        {
            builder.ToTable("StaffMembers", SchemaNames.DDDSample1);
            
            // Chave primária
            builder.HasKey(b => b.Id);
            
            // Propriedade Name
            builder.Property(b => b.Name)
                .IsRequired()
                .HasMaxLength(200);
            
            // Propriedade Email
            builder.Property(b => b.Email)
                .IsRequired()
                .HasMaxLength(100);
            
            // Propriedade PhoneNumber
            builder.Property(b => b.PhoneNumber)
                .IsRequired();
            
            // Propriedade OperationalWindow
            builder.Property(b => b.OperationalWindow)
                .HasMaxLength(100);
            
            // Propriedade Status (enum armazenado como string)
            builder.Property(b => b.Status)
                .IsRequired()
                .HasConversion<string>(); // Converte enum MemberStatus para string
            
            // Configurar relacionamento com Qualifications
        }
    }
}
