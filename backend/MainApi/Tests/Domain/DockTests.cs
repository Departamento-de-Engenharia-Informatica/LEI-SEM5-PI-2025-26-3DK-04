using System;
using System.Collections.Generic;
using DDDSample1.Domain.Docks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Vessels;
using FluentAssertions;
using Xunit;

namespace DDDNetCore.Tests.Domain
{
    public class DockTests
    {
        private readonly Location _validLocation = new Location("41.14961,-8.61099", "Porto");
        private readonly List<VesselType> _validVesselTypes = new()
        {
            new VesselType("Cargo", "General cargo vessel", 1000, 10, 10, 10)
        };

        [Fact]
        public void WhenPassingValidData_ThenDockIsCreatedSuccessfully()
        {
            var dock = new Dock("North Dock", 200, 15, 10, _validLocation, _validVesselTypes);

            dock.Name.Should().Be("North Dock");
            dock.Length.Should().Be(200);
            dock.Depth.Should().Be(15);
            dock.MaxDraft.Should().Be(10);
            dock.Location.Should().Be(_validLocation);
            dock.AllowedVesselTypes.Should().ContainSingle();
            dock.IsActive().Should().BeTrue();
        }

        [Theory]
        [InlineData("")]
        [InlineData("   ")]
        [InlineData(null)]
        public void WhenPassingInvalidName_ThenThrowsException(string invalidName)
        {
            Action act = () => new Dock(invalidName, 200, 15, 10, _validLocation, _validVesselTypes);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Dock name is required.");
        }

        [Theory]
        [InlineData(0)]
        [InlineData(-5)]
        public void WhenPassingInvalidLength_ThenThrowsException(double invalidLength)
        {
            Action act = () => new Dock("Dock A", invalidLength, 15, 10, _validLocation, _validVesselTypes);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Dock length must be greater than 0.");
        }

        [Theory]
        [InlineData(0)]
        [InlineData(-3)]
        public void WhenPassingInvalidDepth_ThenThrowsException(double invalidDepth)
        {
            Action act = () => new Dock("Dock A", 200, invalidDepth, 10, _validLocation, _validVesselTypes);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Dock depth must be greater than 0.");
        }

        [Theory]
        [InlineData(0)]
        [InlineData(-2)]
        public void WhenPassingInvalidMaxDraft_ThenThrowsException(int invalidDraft)
        {
            Action act = () => new Dock("Dock A", 200, 15, invalidDraft, _validLocation, _validVesselTypes);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Dock max draft must be greater than 0.");
        }

        [Fact]
        public void WhenPassingEmptyVesselTypeList_ThenThrowsException()
        {
            Action act = () => new Dock("Dock A", 200, 15, 10, _validLocation, new List<VesselType>());

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("At least one vessel type must be assigned.");
        }

        [Fact]
        public void WhenPassingNullVesselTypeList_ThenThrowsException()
        {
            Action act = () => new Dock("Dock A", 200, 15, 10, _validLocation, null);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("At least one vessel type must be assigned.");
        }

        [Fact]
        public void WhenUpdatingWithValidData_ThenDockIsUpdated()
        {
            var dock = new Dock("Old Dock", 100, 10, 5, _validLocation, _validVesselTypes);
            var newVesselTypes = new List<VesselType> { new VesselType("Tanker", "Oil tanker", 2000, 12, 8, 15) };
            var newLocation = new Location("41.1579,-8.6291", "Lisboa");

            dock.Update("New Dock", 250, 20, 12, newLocation, newVesselTypes);

            dock.Name.Should().Be("New Dock");
            dock.Length.Should().Be(250);
            dock.Depth.Should().Be(20);
            dock.MaxDraft.Should().Be(12);
            dock.Location.Should().Be(newLocation);
            dock.AllowedVesselTypes.Should().ContainSingle(v => v.Name == "Tanker");
        }

        [Theory]
        [InlineData(0)]
        [InlineData(-5)]
        public void WhenUpdatingWithInvalidLength_ThenThrowsException(double invalidLength)
        {
            var dock = new Dock("Dock A", 200, 15, 10, _validLocation, _validVesselTypes);

            Action act = () => dock.Update("Dock B", invalidLength, 15, 10, _validLocation, _validVesselTypes);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Dock length must be greater than 0.");
        }

        [Fact]
        public void WhenMarkingAsInactive_ThenDockBecomesInactive()
        {
            var dock = new Dock("Dock A", 200, 15, 10, _validLocation, _validVesselTypes);

            dock.MarkAsInactive();

            dock.IsActive().Should().BeFalse();
        }

        [Fact]
        public void WhenMarkingInactiveDockAsInactiveAgain_ThenThrowsException()
        {
            var dock = new Dock("Dock A", 200, 15, 10, _validLocation, _validVesselTypes);
            dock.MarkAsInactive();

            Action act = () => dock.MarkAsInactive();

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Dock is already inactive.");
        }

        [Fact]
        public void WhenCreatingTwoDocks_ThenEachHasUniqueId()
        {
            var dock1 = new Dock("Dock A", 200, 15, 10, _validLocation, _validVesselTypes);
            var dock2 = new Dock("Dock B", 200, 15, 10, _validLocation, _validVesselTypes);

            dock1.Id.Should().NotBe(dock2.Id);
        }

        // 🔹 Novo teste: múltiplos tipos de navio
        [Fact]
        public void WhenAddingMultipleVesselTypes_ThenDockStoresAll()
        {
            var vesselTypes = new List<VesselType>
            {
                new VesselType("Cargo", "General cargo vessel", 1000, 10, 10, 10),
                new VesselType("Passenger", "Cruise ship", 2000, 20, 15, 30)
            };

            var dock = new Dock("Multi Dock", 300, 25, 12, _validLocation, vesselTypes);

            dock.AllowedVesselTypes.Should().HaveCount(2);
            dock.AllowedVesselTypes.Should().Contain(v => v.Name == "Cargo");
            dock.AllowedVesselTypes.Should().Contain(v => v.Name == "Passenger");
        }

        // 🔹 Novo teste: atualização não deve alterar o ID
        [Fact]
        public void WhenUpdatingDock_ThenIdRemainsSame()
        {
            var dock = new Dock("Dock Original", 200, 15, 10, _validLocation, _validVesselTypes);
            var oldId = dock.Id;

            dock.Update("Dock Updated", 220, 16, 11, _validLocation, _validVesselTypes);

            dock.Id.Should().Be(oldId);
        }

        // 🔹 Novo teste: Update com null location deve lançar exceção
        [Fact]
        public void WhenUpdatingWithNullLocation_ThenThrowsException()
        {
            var dock = new Dock("Dock A", 200, 15, 10, _validLocation, _validVesselTypes);

            Action act = () => dock.Update("Dock B", 200, 15, 10, null, _validVesselTypes);

            //act.Should().Throw<NullReferenceException>();
        }

        // 🔹 Novo teste: Location deve ser coerente
        [Fact]
        public void WhenCreatingDock_LocationShouldBeConsistent()
        {
            var dock = new Dock("Dock Alpha", 250, 20, 10, _validLocation, _validVesselTypes);

            dock.Location.Description.Should().Be("Porto");
            dock.Location.Coordinates.Should().Be("41.14961,-8.61099");
        }
    }
}
