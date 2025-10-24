using FluentAssertions;
using Xunit;
using System;
using DDDSample1.Domain.Vessels;
using DDDSample1.Domain.Shared;

namespace DDDNetCore.Tests.Domain
{
    public class VesselTests
    {
        private VesselTypeId _validVesselTypeId = new VesselTypeId(Guid.NewGuid());

        [Fact]
        public void WhenPassingCorrectData_ThenVesselIsInstantiated()
        {
            var vessel = new Vessel("IMO1234567", "Test Vessel", _validVesselTypeId, "Test Owner", "Test Operator");

            vessel.ImoNumber.Should().Be("IMO1234567");
            vessel.Name.Should().Be("Test Vessel");
            vessel.VesselTypeId.Should().Be(_validVesselTypeId);
            vessel.Owner.Should().Be("Test Owner");
            vessel.Operator.Should().Be("Test Operator");
            vessel.Active.Should().BeTrue();
            vessel.Crew.Should().NotBeNull();
            vessel.Crew.Should().BeEmpty();
            vessel.Id.Should().NotBeNull();
        }

        [Theory]
        [InlineData("")]
        [InlineData("   ")]
        [InlineData(null)]
        public void WhenPassingEmptyOrNullImoNumber_ThenThrowsException(string imoNumber)
        {
            Action act = () => new Vessel(imoNumber, "Test Vessel", _validVesselTypeId, "Test Owner", "Test Operator");

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("IMO number is required.");
        }

        [Theory]
        [InlineData("1234567")]
        [InlineData("IMO123456")]
        [InlineData("IMO12345678")]
        [InlineData("IMOX1234567")]
        [InlineData("IM01234567")]
        public void WhenPassingInvalidImoNumberFormat_ThenThrowsException(string imoNumber)
        {
            Action act = () => new Vessel(imoNumber, "Test Vessel", _validVesselTypeId, "Test Owner", "Test Operator");

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("IMO number must be in the format: IMO1234567 (7 digits after IMO prefix).");
        }

        [Theory]
        [InlineData("IMO1234567")]
        [InlineData("IMO9876543")]
        [InlineData("IMO0000000")]
        public void WhenPassingValidImoNumberFormats_ThenVesselIsCreated(string imoNumber)
        {
            var vessel = new Vessel(imoNumber, "Test Vessel", _validVesselTypeId, "Test Owner", "Test Operator");

            vessel.ImoNumber.Should().Be(imoNumber);
        }

        [Theory]
        [InlineData("")]
        [InlineData("   ")]
        [InlineData(null)]
        public void WhenPassingEmptyOrNullName_ThenThrowsException(string name)
        {
            Action act = () => new Vessel("IMO1234567", name, _validVesselTypeId, "Test Owner", "Test Operator");

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Vessel name is required.");
        }

        [Fact]
        public void WhenPassingNullVesselTypeId_ThenThrowsException()
        {
            Action act = () => new Vessel("IMO1234567", "Test Vessel", null, "Test Owner", "Test Operator");

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Vessel type is required.");
        }

        [Theory]
        [InlineData("")]
        [InlineData("   ")]
        [InlineData(null)]
        public void WhenPassingEmptyOrNullOwner_ThenThrowsException(string owner)
        {
            Action act = () => new Vessel("IMO1234567", "Test Vessel", _validVesselTypeId, owner, "Test Operator");

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Vessel owner is required.");
        }

        [Theory]
        [InlineData("")]
        [InlineData("   ")]
        [InlineData(null)]
        public void WhenPassingEmptyOrNullOperator_ThenThrowsException(string operatorName)
        {
            Action act = () => new Vessel("IMO1234567", "Test Vessel", _validVesselTypeId, "Test Owner", operatorName);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Vessel operator is required.");
        }

        [Fact]
        public void WhenChangingToValidName_ThenNameIsUpdated()
        {
            var vessel = new Vessel("IMO1234567", "Test Vessel", _validVesselTypeId, "Test Owner", "Test Operator");
            string newName = "Updated Vessel";

            vessel.ChangeName(newName);

            vessel.Name.Should().Be(newName);
        }

        [Theory]
        [InlineData("")]
        [InlineData("   ")]
        [InlineData(null)]
        public void WhenChangingToInvalidName_ThenThrowsException(string invalidName)
        {
            var vessel = new Vessel("IMO1234567", "Test Vessel", _validVesselTypeId, "Test Owner", "Test Operator");

            Action act = () => vessel.ChangeName(invalidName);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Vessel name is required.");
        }

        [Fact]
        public void WhenChangingToValidVesselType_ThenVesselTypeIsUpdated()
        {
            var vessel = new Vessel("IMO1234567", "Test Vessel", _validVesselTypeId, "Test Owner", "Test Operator");
            var newVesselTypeId = new VesselTypeId(Guid.NewGuid());

            vessel.ChangeVesselType(newVesselTypeId);

            vessel.VesselTypeId.Should().Be(newVesselTypeId);
        }

        [Fact]
        public void WhenChangingToNullVesselType_ThenThrowsException()
        {
            var vessel = new Vessel("IMO1234567", "Test Vessel", _validVesselTypeId, "Test Owner", "Test Operator");

            Action act = () => vessel.ChangeVesselType(null);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Vessel type is required.");
        }

        [Fact]
        public void WhenChangingToValidOwner_ThenOwnerIsUpdated()
        {
            var vessel = new Vessel("IMO1234567", "Test Vessel", _validVesselTypeId, "Test Owner", "Test Operator");
            string newOwner = "New Owner";

            vessel.ChangeOwner(newOwner);

            vessel.Owner.Should().Be(newOwner);
        }

        [Theory]
        [InlineData("")]
        [InlineData("   ")]
        [InlineData(null)]
        public void WhenChangingToInvalidOwner_ThenThrowsException(string invalidOwner)
        {
            var vessel = new Vessel("IMO1234567", "Test Vessel", _validVesselTypeId, "Test Owner", "Test Operator");

            Action act = () => vessel.ChangeOwner(invalidOwner);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Vessel owner is required.");
        }

        [Fact]
        public void WhenChangingToValidOperator_ThenOperatorIsUpdated()
        {
            var vessel = new Vessel("IMO1234567", "Test Vessel", _validVesselTypeId, "Test Owner", "Test Operator");
            string newOperator = "New Operator";

            vessel.ChangeOperator(newOperator);

            vessel.Operator.Should().Be(newOperator);
        }

        [Theory]
        [InlineData("")]
        [InlineData("   ")]
        [InlineData(null)]
        public void WhenChangingToInvalidOperator_ThenThrowsException(string invalidOperator)
        {
            var vessel = new Vessel("IMO1234567", "Test Vessel", _validVesselTypeId, "Test Owner", "Test Operator");

            Action act = () => vessel.ChangeOperator(invalidOperator);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Vessel operator is required.");
        }

        [Fact]
        public void WhenMarkingAsInactive_ThenActiveIsFalse()
        {
            var vessel = new Vessel("IMO1234567", "Test Vessel", _validVesselTypeId, "Test Owner", "Test Operator");

            vessel.MarkAsInactive();

            vessel.Active.Should().BeFalse();
        }

        [Fact]
        public void WhenMarkingAsActive_ThenActiveIsTrue()
        {
            var vessel = new Vessel("IMO1234567", "Test Vessel", _validVesselTypeId, "Test Owner", "Test Operator");
            vessel.MarkAsInactive();

            vessel.MarkAsActive();

            vessel.Active.Should().BeTrue();
        }

        [Fact]
        public void WhenChangingNameOfInactiveVessel_ThenThrowsException()
        {
            var vessel = new Vessel("IMO1234567", "Test Vessel", _validVesselTypeId, "Test Owner", "Test Operator");
            vessel.MarkAsInactive();

            Action act = () => vessel.ChangeName("New Name");

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("It is not possible to change the name of an inactive vessel.");
        }

        [Fact]
        public void WhenChangingVesselTypeOfInactiveVessel_ThenThrowsException()
        {
            var vessel = new Vessel("IMO1234567", "Test Vessel", _validVesselTypeId, "Test Owner", "Test Operator");
            vessel.MarkAsInactive();
            var newVesselTypeId = new VesselTypeId(Guid.NewGuid());

            Action act = () => vessel.ChangeVesselType(newVesselTypeId);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("It is not possible to change the type of an inactive vessel.");
        }

        [Fact]
        public void WhenChangingOwnerOfInactiveVessel_ThenThrowsException()
        {
            var vessel = new Vessel("IMO1234567", "Test Vessel", _validVesselTypeId, "Test Owner", "Test Operator");
            vessel.MarkAsInactive();

            Action act = () => vessel.ChangeOwner("New Owner");

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("It is not possible to change the owner of an inactive vessel.");
        }

        [Fact]
        public void WhenChangingOperatorOfInactiveVessel_ThenThrowsException()
        {
            var vessel = new Vessel("IMO1234567", "Test Vessel", _validVesselTypeId, "Test Owner", "Test Operator");
            vessel.MarkAsInactive();

            Action act = () => vessel.ChangeOperator("New Operator");

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("It is not possible to change the operator of an inactive vessel.");
        }

        [Fact]
        public void WhenCreatingTwoVessels_ShouldHaveDifferentIds()
        {
            var vessel1 = new Vessel("IMO1234567", "Vessel One", _validVesselTypeId, "Owner One", "Operator One");
            var vessel2 = new Vessel("IMO7654321", "Vessel Two", _validVesselTypeId, "Owner Two", "Operator Two");

            vessel1.Id.Should().NotBe(vessel2.Id);
        }

        [Fact]
        public void WhenCreatingVessel_CrewShouldBeEmptyList()
        {
            var vessel = new Vessel("IMO1234567", "Test Vessel", _validVesselTypeId, "Test Owner", "Test Operator");

            vessel.Crew.Should().NotBeNull();
            vessel.Crew.Should().BeEmpty();
        }
    }
}
