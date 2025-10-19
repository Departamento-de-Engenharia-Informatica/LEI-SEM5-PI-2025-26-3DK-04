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
        builder.Property(r => r.Description).IsRequired();
        builder.Property(r => r.Type).IsRequired();
        builder.Property(r => r.Capacity).IsRequired();
        builder.Property(r => r.AssignedArea);
        builder.Property(r => r.SetupTime);
        builder.Property(r => r.Status).HasConversion<string>();

        builder.Property<List<QualificationID>>("_qualificationIds")
            .HasConversion(
                ids => string.Join(",", ids.Select(id => id.Value.ToString())),
                str => str.Split(",", StringSplitOptions.RemoveEmptyEntries)
                    .Select(s => new QualificationID(Guid.Parse(s)))
                    .ToList()
            );

    }
}
