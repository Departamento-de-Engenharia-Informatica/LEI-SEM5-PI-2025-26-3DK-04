using System;
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
            // Chave primária
            builder.HasKey(b => b.Id);

            // Convert strong-typed ID to Guid for relational providers (SQLite/Postgres)
            builder.Property(b => b.Id)
                .HasConversion(
                    id => id.AsGuid(),
                    guid => new StaffMemberID(guid))
                .ValueGeneratedNever();
            
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
            
            // Propriedade OperationalWindow (convert OperationalWindow value object to string)
            builder.Property(b => b.OperationalWindow)
                .HasConversion(
                    v => v == null ? null : v.ToString(),
                    s => s == null ? new DDDSample1.Domain.StaffMembers.OperationalWindow(TimeSpan.Zero, TimeSpan.Zero) : new DDDSample1.Domain.StaffMembers.OperationalWindow(s))
                .HasMaxLength(100);
            
            // Propriedade Status (enum armazenado como string)
            builder.Property(b => b.Status)
                .IsRequired()
                .HasConversion<string>(); // Converte enum MemberStatus para string

            // Configurar relacionamento Many-to-Many com Qualifications
            builder.HasMany(s => s.Qualifications)
                .WithMany()
                .UsingEntity(
                    "StaffMemberQualifications",  // Nome da tabela de junção
                    l => l.HasOne(typeof(Qualification)).WithMany().HasForeignKey("QualificationId").OnDelete(DeleteBehavior.Cascade),
                    r => r.HasOne(typeof(StaffMember)).WithMany().HasForeignKey("StaffMemberId").OnDelete(DeleteBehavior.Cascade),
                    j => j.HasKey("StaffMemberId", "QualificationId")
                );
        }
    }
}
