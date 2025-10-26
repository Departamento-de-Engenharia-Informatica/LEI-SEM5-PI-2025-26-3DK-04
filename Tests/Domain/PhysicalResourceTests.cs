using System;
using System.Collections.Generic;
using DDDSample1.Domain.PhysicalResources;
using DDDSample1.Domain.Qualifications;
using DDDSample1.Domain.Shared;
using FluentAssertions;
using Xunit;

namespace DDDNetCore.Tests.Domain
{
    public class PhysicalResourceTests
    {
        [Fact]
        public void CreatePhysicalResource_WithValidData_Succeeds()
        {
            // Arrange
            var qualifications = new List<Qualification>();
            var resource = new PhysicalResource(
                description: "Forklift",
                type: "Vehicle",
                capacity: 2000,
                assignedArea: "Warehouse A",
                setupTime: 15,
                status: ResourceStatus.Active,
                qualifications: qualifications
            );

            // Assert
            resource.Description.Should().Be("Forklift");
            resource.Type.Should().Be("Vehicle");
            resource.Capacity.Should().Be(2000);
            resource.AssignedArea.Should().Be("Warehouse A");
            resource.SetupTime.Should().Be(15);
            resource.Status.Should().Be(ResourceStatus.Active);
            resource.Qualifications.Should().BeEquivalentTo(qualifications);
            resource.Id.Value.Should().NotBe("Guid.Empty");
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData(" ")]
        public void CreatePhysicalResource_InvalidDescription_Throws(string invalidDescription)
        {
            // Act
            Action act = () => new PhysicalResource(
                description: invalidDescription,
                type: "Machine",
                capacity: 100,
                assignedArea: null,
                setupTime: null,
                status: ResourceStatus.Active,
                qualifications: new List<Qualification>()
            );

            // Assert
            act.Should().Throw<BusinessRuleValidationException>()
               .WithMessage("Description is required.");
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData(" ")]
        public void CreatePhysicalResource_InvalidType_Throws(string invalidType)
        {
            Action act = () => new PhysicalResource(
                description: "Crane",
                type: invalidType,
                capacity: 100,
                assignedArea: null,
                setupTime: null,
                status: ResourceStatus.Active,
                qualifications: new List<Qualification>()
            );

            act.Should().Throw<BusinessRuleValidationException>()
               .WithMessage("Type is required.");
        }

        [Fact]
        public void CreatePhysicalResource_InvalidCapacity_Throws()
        {
            Action act = () => new PhysicalResource(
                description: "Loader",
                type: "Vehicle",
                capacity: 0,
                assignedArea: null,
                setupTime: null,
                status: ResourceStatus.Active,
                qualifications: new List<Qualification>()
            );

            act.Should().Throw<BusinessRuleValidationException>()
               .WithMessage("Capacity must be positive.");
        }

        [Fact]
        public void UpdatePhysicalResource_ChangesPropertiesCorrectly()
        {
            // Arrange
            var resource = new PhysicalResource(
                "Forklift",
                "Vehicle",
                2000,
                "Warehouse A",
                15,
                ResourceStatus.Active,
                new List<Qualification>()
            );

            var newQualifications = new List<Qualification> { new Qualification("Forklift License") };

            // Act
            resource.Update(
                description: "Electric Forklift",
                capacity: 2500,
                assignedArea: "Warehouse B",
                setupTime: 20,
                qualifications: newQualifications
            );

            // Assert
            resource.Description.Should().Be("Electric Forklift");
            resource.Capacity.Should().Be(2500);
            resource.AssignedArea.Should().Be("Warehouse B");
            resource.SetupTime.Should().Be(20);
            resource.Qualifications.Should().BeEquivalentTo(newQualifications);
        }

        [Fact]
        public void ChangeStatus_ShouldUpdateStatusCorrectly()
        {
            // Arrange
            var resource = new PhysicalResource(
                "Forklift",
                "Vehicle",
                2000,
                null,
                null,
                ResourceStatus.Active,
                new List<Qualification>()
            );

            // Act & Assert
            resource.ChangeStatus(ResourceStatus.Inactive);
            resource.Status.Should().Be(ResourceStatus.Inactive);

            resource.ChangeStatus(ResourceStatus.UnderMaintenance);
            resource.Status.Should().Be(ResourceStatus.UnderMaintenance);

            resource.ChangeStatus(ResourceStatus.Active);
            resource.Status.Should().Be(ResourceStatus.Active);
        }

        [Fact]
        public void UpdatePhysicalResource_WithNullQualifications_ShouldInitializeEmptyList()
        {
            var resource = new PhysicalResource("Forklift", "Vehicle", 2000, null, null, ResourceStatus.Active, new List<Qualification>());

            resource.Update("Forklift Updated", 2000, null, null, null);

            resource.Qualifications.Should().BeEmpty();
        }

        [Fact]
        public void MultipleStatusChanges_ShouldPersistCorrectly()
        {
            var resource = new PhysicalResource("Forklift", "Vehicle", 2000, null, null, ResourceStatus.Active, new List<Qualification>());

            resource.ChangeStatus(ResourceStatus.UnderMaintenance);
            resource.ChangeStatus(ResourceStatus.Inactive);
            resource.ChangeStatus(ResourceStatus.Active);

            resource.Status.Should().Be(ResourceStatus.Active);
        }

        [Fact]
        public void PhysicalResource_CanHaveEmptyAssignedAreaAndSetupTime()
        {
            var resource = new PhysicalResource("Crane", "Equipment", 5000, null, null, ResourceStatus.Active, new List<Qualification>());
            resource.AssignedArea.Should().BeNull();
            resource.SetupTime.Should().BeNull();
        }

        [Fact]
        public void PhysicalResource_CanAddMultipleQualifications()
        {
            var qualifications = new List<Qualification>
            {
                new Qualification("Forklift License"),
                new Qualification("Safety Training")
            };

            var resource = new PhysicalResource("Forklift", "Vehicle", 2000, null, null, ResourceStatus.Active, qualifications);

            resource.Qualifications.Should().HaveCount(2);
            resource.Qualifications.Should().Contain(q => q.Name == "Forklift License");
            resource.Qualifications.Should().Contain(q => q.Name == "Safety Training");
        }
    }
}
