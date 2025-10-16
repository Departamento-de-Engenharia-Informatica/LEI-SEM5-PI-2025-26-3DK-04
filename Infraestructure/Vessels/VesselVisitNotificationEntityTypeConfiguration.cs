using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.Vessels.VesselVisitNotification;
using DDDSample1.Infrastructure;

namespace DDDSample1.Infrastructure.Vessels
{
    internal class VesselVisitNotificationEntityTypeConfiguration : IEntityTypeConfiguration<VesselVisitNotification>
    {
        public void Configure(EntityTypeBuilder<VesselVisitNotification> builder)
        {
            builder.HasKey(b => b.Id);
            
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
            
            // Configurar relacionamento com Vessel (obrigatório)
            builder.HasOne(b => b.Vessel)
                .WithMany()
                .IsRequired()
                .OnDelete(DeleteBehavior.Restrict);
            
            // Configurar LoadingCargo como Value Object (owned entity)
            builder.OwnsOne(b => b.LoadingCargo);
            
            // Configurar UnloadingCargo como Value Object (owned entity)
            builder.OwnsOne(b => b.UnloadingCargo);
        }
    }
}
