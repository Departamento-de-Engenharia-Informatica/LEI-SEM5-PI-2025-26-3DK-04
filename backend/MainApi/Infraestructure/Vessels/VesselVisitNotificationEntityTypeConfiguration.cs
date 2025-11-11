using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.Vessels.VesselVisitNotification;
using DDDSample1.Domain.Organizations;
using DDDSample1.Domain.Vessels.VesselInformation;

namespace DDDSample1.Infrastructure.Vessels
{
    internal class VesselVisitNotificationEntityTypeConfiguration : IEntityTypeConfiguration<VesselVisitNotification>
    {
        public void Configure(EntityTypeBuilder<VesselVisitNotification> builder)
        {
            builder.HasKey(b => b.Id);

            builder.Property(b => b.Id)
                .HasConversion(
                    id => id.AsGuid(),
                    guid => new VesselVisitNotificationID(guid))
                .ValueGeneratedNever();

            builder.Property(b => b.Status)
                .IsRequired()
                .HasConversion<string>();

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

            builder.Property(b => b.RepresentativeId)
                .HasConversion(
                    id => id.AsString(),
                    str => new RepresentativeId(str))
                .IsRequired()
                .ValueGeneratedNever();
            
            // Novos campos para integração com IARTI
            builder.Property(b => b.ArrivalTime);
            
            builder.Property(b => b.DepartureTime);
            
            builder.Property(b => b.UnloadTime);
            
            builder.Property(b => b.LoadTime);
            
            builder.Property(b => b.PhysicalResourceId)
                .HasMaxLength(100);
            
            builder.Property(b => b.DockId)
                .HasMaxLength(100);
            
            // Propriedade de coleção para Staff Member IDs (armazenada como JSON ou tabela separada)
            builder.Property(b => b.StaffMemberIds)
                .HasConversion(
                    v => string.Join(',', v),
                    v => v.Split(',', StringSplitOptions.RemoveEmptyEntries).ToList())
                .HasMaxLength(1000);

            builder.HasOne(b => b.Vessel)
                .WithMany()
                .IsRequired()
                .OnDelete(DeleteBehavior.Restrict);

            // ---------------------- LOADING CARGO ----------------------
            builder.OwnsOne(b => b.LoadingCargo, cargo =>
            {
                cargo.WithOwner().HasForeignKey("VesselVisitNotificationId");

                cargo.OwnsMany(lc => lc.Manifests, manifest =>
                {
                    manifest.ToTable("LoadingCargoManifests");

                    manifest.WithOwner().HasForeignKey("VesselVisitNotificationId");

                    manifest.HasKey(m => m.Id);

                    manifest.Property(m => m.Id)
                        .HasConversion(
                            id => id.AsGuid(),
                            guid => new CargoManifestID(guid))
                        .HasColumnType("uuid")
                        .ValueGeneratedNever();

                    // ---- Containers ----
                    manifest.OwnsMany(m => m.Containers, container =>
                    {
                        container.ToTable("LoadingManifestContainers");

                        container.HasKey(c => c.Id);

                        container.Property(c => c.Id)
                            .HasConversion(
                                id => id.AsGuid(),
                                guid => new ContainerID(guid))
                            .HasColumnType("uuid")
                            .ValueGeneratedNever();

                        container.Property(c => c.PayloadWeight).IsRequired();
                        container.Property(c => c.ContentsDescription).IsRequired(false);

                        // ⚙️ Cada Container pertence a um Manifest — a FK é criada automaticamente pelo EF
                        container.WithOwner().HasForeignKey("CargoManifestId");
                    });
                });
            });

            // ---------------------- UNLOADING CARGO ----------------------
            builder.OwnsOne(b => b.UnloadingCargo, cargo =>
            {
                cargo.WithOwner().HasForeignKey("VesselVisitNotificationId");

                cargo.OwnsMany(lc => lc.Manifests, manifest =>
                {
                    manifest.ToTable("UnloadingCargoManifests");

                    manifest.WithOwner().HasForeignKey("VesselVisitNotificationId");

                    manifest.HasKey(m => m.Id);

                    manifest.Property(m => m.Id)
                        .HasConversion(
                            id => id.AsGuid(),
                            guid => new CargoManifestID(guid))
                        .HasColumnType("uuid")
                        .ValueGeneratedNever();

                    // ---- Containers ----
                    manifest.OwnsMany(m => m.Containers, container =>
                    {
                        container.ToTable("UnloadingManifestContainers");

                        container.HasKey(c => c.Id);

                        container.Property(c => c.Id)
                            .HasConversion(
                                id => id.AsGuid(),
                                guid => new ContainerID(guid))
                            .HasColumnType("uuid")
                            .ValueGeneratedNever();

                        container.Property(c => c.PayloadWeight).IsRequired();
                        container.Property(c => c.ContentsDescription).IsRequired(false);

                        // ⚙️ Relação com CargoManifest automática (sem precisar de ManifestId na entidade)
                        container.WithOwner().HasForeignKey("CargoManifestId");
                    });
                });
            });

            builder.HasIndex("VesselId");
            builder.HasIndex(vn => vn.Status);
        }
    }
}
