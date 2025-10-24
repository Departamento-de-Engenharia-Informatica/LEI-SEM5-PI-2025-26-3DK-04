using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.Vessels.VesselVisitNotification;
using DDDSample1.Infrastructure;
using DDDSample1.Domain.Organizations;

namespace DDDSample1.Infrastructure.Vessels
{
    internal class VesselVisitNotificationEntityTypeConfiguration : IEntityTypeConfiguration<VesselVisitNotification>
    {
        public void Configure(EntityTypeBuilder<VesselVisitNotification> builder)
        {
            builder.HasKey(b => b.Id);

            // Convert strong-typed ID to Guid for relational providers (SQLite/Postgres)
            builder.Property(b => b.Id)
                .HasConversion(
                    id => id.AsGuid(),
                    guid => new VesselVisitNotificationID(guid))
                .ValueGeneratedNever();

            builder.Property(b => b.Status)
                .IsRequired()
                .HasConversion<string>(); // Armazena enum como string
            
            builder.Property(b => b.AssignedDock)
                .HasMaxLength(50);
            
            builder.Property(b => b.RejectedReason)
                .HasMaxLength(500);
            
            builder.Property(b => b.DecisionOutcome)
                .HasMaxLength(50);
            
            builder.Property(b => b.OfficerId)
                .HasMaxLength(100);
            
            builder.Property(b => b.DecisionTimeStamp);

            builder.Property(b => b.CreatedAt)
                .IsRequired();
            
            // Configure RepresentativeId as value object
            builder.Property(b => b.RepresentativeId)
                .IsRequired();

            // Configure RepresentativeId as value object (string-backed ID)
            builder.Property(b => b.RepresentativeId)
                .HasConversion(
                    id => id.AsString(),
                    str => new RepresentativeId(str))
                .IsRequired()
                .ValueGeneratedNever();
                
            
            // Configurar relacionamento com Vessel (obrigatório)
            builder.HasOne(b => b.Vessel)
                .WithMany()
                .IsRequired()
                .OnDelete(DeleteBehavior.Restrict);
            
            // Configurar LoadingCargo como Value Object (owned entity)
            builder.OwnsOne(b => b.LoadingCargo, cargo =>
            {
                cargo.Ignore(c => c.Manifests); // Ignore navigation property for now
            });
            
            // Configurar UnloadingCargo como Value Object (owned entity)
            builder.OwnsOne(b => b.UnloadingCargo, cargo =>
            {
                cargo.Ignore(c => c.Manifests); // Ignore navigation property for now
            });
        }
    }
}
