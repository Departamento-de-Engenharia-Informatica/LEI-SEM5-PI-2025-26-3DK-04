using System;
using System.Collections.Generic;
using System.Linq;
using DDDSample1.Domain.PhysicalResources;
using DDDSample1.Domain.Qualifications;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace DDDSample1.Infrastructure.PhysicalResources;

public class PhysicalResourceEntityTypeConfiguration : IEntityTypeConfiguration<PhysicalResource>
{
    public void Configure(EntityTypeBuilder<PhysicalResource> builder)
    {
        builder.HasKey(r => r.Id);

        // Convert strong-typed ID to Guid for relational providers (SQLite/Postgres)
        builder.Property(r => r.Id)
            .HasConversion(
                id => id.AsGuid(),
                guid => new PhysicalResourceId(guid))
            .ValueGeneratedNever();        
        
        builder.Property(r => r.Description).IsRequired();
        builder.Property(r => r.Type).IsRequired();
        builder.Property(r => r.Capacity).IsRequired();
        builder.Property(r => r.AssignedArea);
        builder.Property(r => r.SetupTime);
        builder.Property(r => r.Status).HasConversion<string>();

        builder.HasMany(r => r.Qualifications)
            .WithMany()
            .UsingEntity(
                "PhysicalResourceQualifications", // Nome da tabela de junção
                l => l.HasOne(typeof(Qualification))
                    .WithMany()
                    .HasForeignKey("QualificationId")
                    .OnDelete(DeleteBehavior.Cascade),
                r => r.HasOne(typeof(PhysicalResource))
                    .WithMany()
                    .HasForeignKey("PhysicalResourceId")
                    .OnDelete(DeleteBehavior.Cascade),
                j => j.HasKey("PhysicalResourceId", "QualificationId")
            );



    }
}
