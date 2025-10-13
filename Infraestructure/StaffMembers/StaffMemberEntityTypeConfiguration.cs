using System.Collections.Generic;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.StaffMembers;
using DDDSample1.Domain.Qualifications;

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

            builder.HasMany(s => s.Qualifications)
                .WithMany()
                .UsingEntity<Dictionary<string, object>>(
                    "StaffMemberQualifications",  // Nome da tabela de junção
                    j => j
                        .HasOne<Qualification>()
                        .WithMany()
                        .HasForeignKey("QualificationId")
                        .OnDelete(DeleteBehavior.Cascade),
                    j => j
                        .HasOne<StaffMember>()
                        .WithMany()
                        .HasForeignKey("StaffMemberId")
                        .OnDelete(DeleteBehavior.Cascade),
                    j =>
                    {
                        j.ToTable("StaffMemberQualifications", SchemaNames.DDDSample1);
                        j.HasKey("StaffMemberId", "QualificationId");
                    }
                );
        }
    }
}
