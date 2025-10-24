using FluentAssertions;
using Xunit;
using System;
using DDDSample1.Domain.Vessels;
using DDDSample1.Domain.Shared;

namespace DDDNetCore.Tests.Domain
{
    public class VesselTypeTests
    {
        [Fact]
        public void WhenPassingCorrectData_ThenVesselTypeIsInstantiated()
        {
            var vesselType = new VesselType("Container Ship", "Large cargo vessel", 1000, 10, 20, 8);

            vesselType.Name.Should().Be("Container Ship");
            vesselType.Description.Should().Be("Large cargo vessel");
            vesselType.Capacity.Should().Be(1000);
            vesselType.MaxRows.Should().Be(10);
            vesselType.MaxBays.Should().Be(20);
            vesselType.MaxTiers.Should().Be(8);
            vesselType.Active.Should().BeTrue();
            vesselType.Id.Should().NotBeNull();
        }

        [Theory]
        [InlineData("")]
        [InlineData("   ")]
        [InlineData(null)]
        public void WhenPassingEmptyOrNullName_ThenThrowsException(string name)
        {
            Action act = () => new VesselType(name, "Description", 1000, 10, 20, 8);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Vessel type name is required.");
        }

        [Theory]
        [InlineData("")]
        [InlineData("   ")]
        [InlineData(null)]
        public void WhenPassingEmptyOrNullDescription_ThenThrowsException(string description)
        {
            Action act = () => new VesselType("Container Ship", description, 1000, 10, 20, 8);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Vessel type description is required.");
        }

        [Theory]
        [InlineData(0)]
        [InlineData(-1)]
        [InlineData(-100)]
        public void WhenPassingInvalidCapacity_ThenThrowsException(int capacity)
        {
            Action act = () => new VesselType("Container Ship", "Description", capacity, 10, 20, 8);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Capacity must be greater than zero.");
        }

        [Theory]
        [InlineData(0)]
        [InlineData(-1)]
        [InlineData(-10)]
        public void WhenPassingInvalidMaxRows_ThenThrowsException(int maxRows)
        {
            Action act = () => new VesselType("Container Ship", "Description", 1000, maxRows, 20, 8);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Maximum number of rows must be greater than zero.");
        }

        [Theory]
        [InlineData(0)]
        [InlineData(-1)]
        [InlineData(-10)]
        public void WhenPassingInvalidMaxBays_ThenThrowsException(int maxBays)
        {
            Action act = () => new VesselType("Container Ship", "Description", 1000, 10, maxBays, 8);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Maximum number of bays must be greater than zero.");
        }

        [Theory]
        [InlineData(0)]
        [InlineData(-1)]
        [InlineData(-10)]
        public void WhenPassingInvalidMaxTiers_ThenThrowsException(int maxTiers)
        {
            Action act = () => new VesselType("Container Ship", "Description", 1000, 10, 20, maxTiers);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Maximum number of tiers must be greater than zero.");
        }

        [Fact]
        public void WhenChangingToValidName_ThenNameIsUpdated()
        {
            var vesselType = new VesselType("Container Ship", "Description", 1000, 10, 20, 8);
            string newName = "Cargo Ship";

            vesselType.ChangeName(newName);

            vesselType.Name.Should().Be(newName);
        }

        [Theory]
        [InlineData("")]
        [InlineData("   ")]
        [InlineData(null)]
        public void WhenChangingToInvalidName_ThenThrowsException(string invalidName)
        {
            var vesselType = new VesselType("Container Ship", "Description", 1000, 10, 20, 8);

            Action act = () => vesselType.ChangeName(invalidName);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Vessel type name is required.");
        }

        [Fact]
        public void WhenChangingToValidDescription_ThenDescriptionIsUpdated()
        {
            var vesselType = new VesselType("Container Ship", "Description", 1000, 10, 20, 8);
            string newDescription = "Updated cargo vessel description";

            vesselType.ChangeDescription(newDescription);

            vesselType.Description.Should().Be(newDescription);
        }

        [Theory]
        [InlineData("")]
        [InlineData("   ")]
        [InlineData(null)]
        public void WhenChangingToInvalidDescription_ThenThrowsException(string invalidDescription)
        {
            var vesselType = new VesselType("Container Ship", "Description", 1000, 10, 20, 8);

            Action act = () => vesselType.ChangeDescription(invalidDescription);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Vessel type description is required.");
        }

        [Fact]
        public void WhenChangingToValidCapacity_ThenCapacityIsUpdated()
        {
            var vesselType = new VesselType("Container Ship", "Description", 1000, 10, 20, 8);
            int newCapacity = 1500;

            vesselType.ChangeCapacity(newCapacity);

            vesselType.Capacity.Should().Be(newCapacity);
        }

        [Theory]
        [InlineData(0)]
        [InlineData(-1)]
        [InlineData(-100)]
        public void WhenChangingToInvalidCapacity_ThenThrowsException(int invalidCapacity)
        {
            var vesselType = new VesselType("Container Ship", "Description", 1000, 10, 20, 8);

            Action act = () => vesselType.ChangeCapacity(invalidCapacity);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Capacity must be greater than zero.");
        }

        [Fact]
        public void WhenChangingToValidOperationalConstraints_ThenConstraintsAreUpdated()
        {
            var vesselType = new VesselType("Container Ship", "Description", 1000, 10, 20, 8);
            int newMaxRows = 12;
            int newMaxBays = 25;
            int newMaxTiers = 10;

            vesselType.ChangeOperationalConstraints(newMaxRows, newMaxBays, newMaxTiers);

            vesselType.MaxRows.Should().Be(newMaxRows);
            vesselType.MaxBays.Should().Be(newMaxBays);
            vesselType.MaxTiers.Should().Be(newMaxTiers);
        }

        [Theory]
        [InlineData(0, 20, 8)]
        [InlineData(-1, 20, 8)]
        public void WhenChangingToInvalidMaxRows_InConstraints_ThenThrowsException(int invalidMaxRows, int maxBays, int maxTiers)
        {
            var vesselType = new VesselType("Container Ship", "Description", 1000, 10, 20, 8);

            Action act = () => vesselType.ChangeOperationalConstraints(invalidMaxRows, maxBays, maxTiers);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Maximum number of rows must be greater than zero.");
        }

        [Theory]
        [InlineData(10, 0, 8)]
        [InlineData(10, -1, 8)]
        public void WhenChangingToInvalidMaxBays_InConstraints_ThenThrowsException(int maxRows, int invalidMaxBays, int maxTiers)
        {
            var vesselType = new VesselType("Container Ship", "Description", 1000, 10, 20, 8);

            Action act = () => vesselType.ChangeOperationalConstraints(maxRows, invalidMaxBays, maxTiers);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Maximum number of bays must be greater than zero.");
        }

        [Theory]
        [InlineData(10, 20, 0)]
        [InlineData(10, 20, -1)]
        public void WhenChangingToInvalidMaxTiers_InConstraints_ThenThrowsException(int maxRows, int maxBays, int invalidMaxTiers)
        {
            var vesselType = new VesselType("Container Ship", "Description", 1000, 10, 20, 8);

            Action act = () => vesselType.ChangeOperationalConstraints(maxRows, maxBays, invalidMaxTiers);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Maximum number of tiers must be greater than zero.");
        }

        [Fact]
        public void WhenMarkingAsInactive_ThenActiveIsFalse()
        {
            var vesselType = new VesselType("Container Ship", "Description", 1000, 10, 20, 8);

            vesselType.MarkAsInactive();

            vesselType.Active.Should().BeFalse();
        }

        [Fact]
        public void WhenMarkingAsActive_ThenActiveIsTrue()
        {
            var vesselType = new VesselType("Container Ship", "Description", 1000, 10, 20, 8);
            vesselType.MarkAsInactive();

            vesselType.MarkAsActive();

            vesselType.Active.Should().BeTrue();
        }

        [Fact]
        public void WhenChangingNameOfInactiveVesselType_ThenThrowsException()
        {
            var vesselType = new VesselType("Container Ship", "Description", 1000, 10, 20, 8);
            vesselType.MarkAsInactive();

            Action act = () => vesselType.ChangeName("New Name");

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("It is not possible to change the name of an inactive vessel type.");
        }

        [Fact]
        public void WhenChangingDescriptionOfInactiveVesselType_ThenThrowsException()
        {
            var vesselType = new VesselType("Container Ship", "Description", 1000, 10, 20, 8);
            vesselType.MarkAsInactive();

            Action act = () => vesselType.ChangeDescription("New Description");

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("It is not possible to change the description of an inactive vessel type.");
        }

        [Fact]
        public void WhenChangingCapacityOfInactiveVesselType_ThenThrowsException()
        {
            var vesselType = new VesselType("Container Ship", "Description", 1000, 10, 20, 8);
            vesselType.MarkAsInactive();

            Action act = () => vesselType.ChangeCapacity(1500);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("It is not possible to change the capacity of an inactive vessel type.");
        }

        [Fact]
        public void WhenChangingOperationalConstraintsOfInactiveVesselType_ThenThrowsException()
        {
            var vesselType = new VesselType("Container Ship", "Description", 1000, 10, 20, 8);
            vesselType.MarkAsInactive();

            Action act = () => vesselType.ChangeOperationalConstraints(12, 25, 10);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("It is not possible to change the operational constraints of an inactive vessel type.");
        }

        [Fact]
        public void WhenCreatingTwoVesselTypes_ShouldHaveDifferentIds()
        {
            var vesselType1 = new VesselType("Container Ship", "Description 1", 1000, 10, 20, 8);
            var vesselType2 = new VesselType("Cargo Ship", "Description 2", 1500, 12, 25, 10);

            vesselType1.Id.Should().NotBe(vesselType2.Id);
        }
    }
}
