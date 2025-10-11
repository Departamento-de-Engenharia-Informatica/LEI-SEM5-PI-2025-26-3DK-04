using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.Vessels;

namespace DDDSample1.Infrastructure.Vessels
{
    internal class VesselVisitNotificationEntityTypeConfiguration : IEntityTypeConfiguration<VesselVisitNotification>
    {
        public void Configure(EntityTypeBuilder<VesselVisitNotification> builder)
        {
            builder.ToTable("VesselVisitNotifications", SchemaNames.DDDSample1);
            
            builder.HasKey(b => b.Id);
            
            builder.Property(b => b.State)
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
            
            // Configurar LoadingCargo como Value Object
            builder.OwnsOne(b => b.LoadingCargo, cargo =>
            {
                cargo.Property(c => c.MaterialType).HasColumnName("Type").HasMaxLength(100);
                cargo.Property(c => c.Quantity).HasColumnName("Quantity");
            });
            
            // Configurar UnloadingCargo como Value Object
            builder.OwnsOne(b => b.UnloadingCargo, cargo =>
            {
                cargo.Property(c => c.MaterialType).HasColumnName("Type").HasMaxLength(100);
                cargo.Property(c => c.Quantity).HasColumnName("Quantity");
            });
        }
    }
}
