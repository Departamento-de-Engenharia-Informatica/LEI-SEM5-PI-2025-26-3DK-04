using System;
using System.Linq;
using DDDNetCore.Domain.PortInfrastructure.StorageArea;
using DDDSample1.Domain.Docks;
using DDDSample1.Domain.PortInfrastructure.StorageArea;
using DDDSample1.Domain.Shared;
using Xunit;
using FluentAssertions;

namespace DDDNetCore.Tests.Domain
{
    public class StorageAreaTests
    {
        [Fact]
        public void CreateStorageArea_ShouldInitializeCorrectly()
        {
            var location = new Location("41.123,-8.611", "North Terminal");

            var storageArea = new StorageArea(
                code: "SA01",
                designation: "Main Storage",
                storageAreaType: StorageAreaType.Refrigerated,
                location: location,
                maxCapacityTEUs: 500,
                length: 100,
                width: 50,
                heigth: 30
            );

            storageArea.Code.Should().Be("SA01");
            storageArea.Designation.Should().Be("Main Storage");
            storageArea.StorageAreaType.Should().Be(StorageAreaType.Refrigerated);
            storageArea.Location.Should().Be(location);
            storageArea.MaxCapacityTEUs.Should().Be(500);
            storageArea.CurrentOccupancyTEUs.Should().Be(0);
            storageArea.Active.Should().BeTrue();
        }

        [Fact]
        public void UpdateDetails_ShouldChangePropertiesCorrectly()
        {
            var location1 = new Location("41.123,-8.611", "North Terminal");
            var location2 = new Location("42.000,-9.000", "South Terminal");

            var storageArea = new StorageArea(
                code: "SA01",
                designation: "Main Storage",
                storageAreaType: StorageAreaType.Warehouse,
                location: location1,
                maxCapacityTEUs: 500,
                length: 100,
                width: 50,
                heigth: 30
            );

            storageArea.UpdateDetails(
                code: "SA02",
                designation: "Secondary Storage",
                storageAreaType: StorageAreaType.Yard,
                location: location2,
                maxCapacityTEUs: 600,
                currentOccupancyTEUs: 100,
                length: 120,
                depth: 60,
                heigth: 40
            );

            storageArea.Code.Should().Be("SA02");
            storageArea.Designation.Should().Be("Secondary Storage");
            storageArea.StorageAreaType.Should().Be(StorageAreaType.Yard);
            storageArea.Location.Should().Be(location2);
            storageArea.MaxCapacityTEUs.Should().Be(600);
            storageArea.CurrentOccupancyTEUs.Should().Be(100);
        }

        [Fact]
        public void AssignDock_ShouldAddAssignment()
        {
            var storageArea = new StorageArea(
                "SA01",
                "Main Storage",
                StorageAreaType.Refrigerated,
                new Location("41.123,-8.611", "North Terminal"),
                500,
                100,
                50,
                30
            );

            var dockId = new DockID(Guid.NewGuid());
            storageArea.AssignDock(dockId, 150.0);

            storageArea.DockAssignments.Should().ContainSingle();
            storageArea.DockAssignments.First().DockId.Should().Be(dockId);
            storageArea.DockAssignments.First().DistanceMeters.Should().Be(150.0);
        }

        [Fact]
        public void UnassignDock_ShouldRemoveAssignment()
        {
            var storageArea = new StorageArea(
                "SA01",
                "Main Storage",
                StorageAreaType.Refrigerated,
                new Location("41.123,-8.611", "North Terminal"),
                500,
                100,
                50,
                30
            );

            var dockId = new DockID(Guid.NewGuid());
            storageArea.AssignDock(dockId, 150.0);

            storageArea.UnassignDock(dockId);

            storageArea.DockAssignments.Should().BeEmpty();
        }

        [Fact]
        public void MarkAsInactive_And_MarkAsActive_ShouldToggleActiveState()
        {
            var storageArea = new StorageArea(
                "SA01",
                "Main Storage",
                StorageAreaType.Warehouse,
                new Location("41.123,-8.611", "North Terminal"),
                500,
                100,
                50,
                30
            );

            storageArea.MarkAsInactive();
            storageArea.Active.Should().BeFalse();

            storageArea.MarkAsActive();
            storageArea.Active.Should().BeTrue();
        }

        [Fact]
        public void AssignDock_Twice_ShouldThrow()
        {
            var storageArea = new StorageArea(
                "SA01",
                "Main Storage",
                StorageAreaType.Refrigerated,
                new Location("41.123,-8.611", "North Terminal"),
                500,
                100,
                50,
                30
            );

            var dockId = new DockID(Guid.NewGuid());
            storageArea.AssignDock(dockId, 100);

            Action act = () => storageArea.AssignDock(dockId, 200);
            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage($"Dock {dockId.AsString()} already assigned to this storage area.");
        }

        [Fact]
        public void UnassignDock_NotAssigned_ShouldThrow()
        {
            var storageArea = new StorageArea(
                "SA01",
                "Main Storage",
                StorageAreaType.Yard,
                new Location("41.123,-8.611", "North Terminal"),
                500,
                100,
                50,
                30
            );

            var dockId = new DockID(Guid.NewGuid());

            Action act = () => storageArea.UnassignDock(dockId);
            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage($"Dock {dockId.AsString()} is not assigned to this storage area.");
        }

        [Fact]
        public void CreateStorageArea_InvalidParameters_ShouldThrow()
        {
            var location = new Location("41.123,-8.611", "North Terminal");

            Action act1 = () => new StorageArea("", "Designation", StorageAreaType.Other, location, 100, 50, 30, 20);
            Action act2 = () => new StorageArea("Code", "", StorageAreaType.Other, location, 100, 50, 30, 20);
            Action act3 = () => new StorageArea("Code", "Designation", StorageAreaType.Other, null, 100, 50, 30, 20);
            Action act4 = () => new StorageArea("Code", "Designation", StorageAreaType.Other, location, 0, 50, 30, 20);

            act1.Should().Throw<BusinessRuleValidationException>();
            act2.Should().Throw<BusinessRuleValidationException>();
            act3.Should().Throw<BusinessRuleValidationException>();
            act4.Should().Throw<BusinessRuleValidationException>();
        }
        [Fact]
        public void UpdateDetails_NegativeCurrentOccupancy_ShouldThrow()
        {
            var location = new Location("41.123,-8.611", "North Terminal");
            var storageArea = new StorageArea("SA01", "Main Storage", StorageAreaType.Refrigerated, location, 500, 100, 50, 30);

            Action act = () => storageArea.UpdateDetails("SA01", "Main Storage", StorageAreaType.Refrigerated, location, 500, -1, 50, 30, 20);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Current occupancy cannot be negative.");
        }

        [Fact]
        public void UpdateDetails_CurrentOccupancyExceedsMax_ShouldThrow()
        {
            var location = new Location("41.123,-8.611", "North Terminal");
            var storageArea = new StorageArea("SA01", "Main Storage", StorageAreaType.Refrigerated, location, 500, 100, 50, 30);

            Action act = () => storageArea.UpdateDetails("SA01", "Main Storage", StorageAreaType.Refrigerated, location, 500, 600, 50, 30, 20);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Current occupancy cannot exceed maximum capacity.");
        }

        [Fact]
        public void AssignDock_InvalidDistance_ShouldThrow()
        {
            var storageArea = new StorageArea("SA01", "Main Storage", StorageAreaType.Warehouse, new Location("41.123,-8.611", "North Terminal"), 500, 100, 50, 30);
            var dockId = new DockID(Guid.NewGuid());

            Action act = () => storageArea.AssignDock(dockId, 0);
            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Distance must be greater than 0.");

            Action act2 = () => storageArea.AssignDock(dockId, -5);
            act2.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Distance must be greater than 0.");
        }

        [Fact]
        public void UpdateDetails_InvalidStorageAreaType_ShouldThrow()
        {
            var location = new Location("41.123,-8.611", "North Terminal");
            var storageArea = new StorageArea("SA01", "Main Storage", StorageAreaType.Yard, location, 500, 100, 50, 30);

            // Force invalid enum casting
            var invalidType = (StorageAreaType)999;

            Action act = () => storageArea.UpdateDetails("SA01", "Main Storage", invalidType, location, 500, 0, 50, 30, 20);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Invalid storage area type provided.");
        }

        [Fact]
        public void MarkAsInactive_WhenAlreadyInactive_ShouldRemainFalse()
        {
            var storageArea = new StorageArea("SA01", "Main Storage", StorageAreaType.Refrigerated, new Location("41.123,-8.611", "North Terminal"), 500, 100, 50, 30);
            storageArea.MarkAsInactive();

            storageArea.MarkAsInactive();

            storageArea.Active.Should().BeFalse();
        }

        [Fact]
        public void MarkAsActive_WhenAlreadyActive_ShouldRemainTrue()
        {
            var storageArea = new StorageArea("SA01", "Main Storage", StorageAreaType.Refrigerated, new Location("41.123,-8.611", "North Terminal"), 500, 100, 50, 30);

            storageArea.MarkAsActive();

            storageArea.Active.Should().BeTrue();
        }

        [Fact]
        public void MultipleDockAssignments_ShouldStoreAllCorrectly()
        {
            var storageArea = new StorageArea("SA01", "Main Storage", StorageAreaType.Yard, new Location("41.123,-8.611", "North Terminal"), 500, 100, 50, 30);
            var dock1 = new DockID(Guid.NewGuid());
            var dock2 = new DockID(Guid.NewGuid());

            storageArea.AssignDock(dock1, 100);
            storageArea.AssignDock(dock2, 200);

            storageArea.DockAssignments.Count.Should().Be(2);
            storageArea.DockAssignments.Should().Contain(a => a.DockId == dock1 && a.DistanceMeters == 100);
            storageArea.DockAssignments.Should().Contain(a => a.DockId == dock2 && a.DistanceMeters == 200);
        }
    }
}
