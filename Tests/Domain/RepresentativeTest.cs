using System;
using FluentAssertions;
using Xunit;
using DDDSample1.Domain.Organizations;
using DDDSample1.Domain.Shared;

namespace DDDNetCore.Tests.Domain
{
    public class RepresentativeTests
    {
        [Fact]
        public void WhenCreatingRepresentativeWithValidData_ThenIsCreatedActive()
        {
            var rep = new Representative("Alice", "CIT123", "PT", "alice@gmail.com", "+351912345678");

            rep.Name.Should().Be("Alice");
            rep.Id.AsString().Should().Be("CIT123");
            rep.Nationality.Should().Be("PT");
            rep.Email.Should().Be("alice@gmail.com");
            rep.PhoneNumber.Should().Be("+351912345678");
            rep.Status.Should().Be(RepresentativeStatus.Active);
        }

        [Theory]
        [InlineData("", "CIT123", "PT", "a@gmail.com", "+351912345678", "Name is required.")]
        [InlineData("Alice", "", "PT", "a@gmail.com", "+351912345678", "Citizen ID is required.")]
        [InlineData("Alice", "CIT123", "", "a@gmail.com", "+351912345678", "Nationality is required.")]
        [InlineData("Alice", "CIT123", "PT", "", "+351912345678", "Email is required.")]
        [InlineData("Alice", "CIT123", "PT", "a@gmail.com", "", "Phone number is required.")]
        public void WhenMissingRequiredField_ThenThrows(
            string name, string id, string nationality, string email, string phone, string expectedMsg)
        {
            Action act = () => new Representative(name, id, nationality, email, phone);
            act.Should().Throw<BusinessRuleValidationException>().WithMessage(expectedMsg);
        }

        [Fact]
        public void AssignToOrganization_WhenValid_AssignsSuccessfully()
        {
            var rep = new Representative("Alice", "CIT123", "PT", "a@gmail.com", "+351912345678");
            var orgId = new OrganizationId("ORG1");

            rep.AssignToOrganization(orgId);

            rep.OrganizationId.Should().Be(orgId);
        }

        [Fact]
        public void AssignToOrganization_WhenAlreadyAssignedToDifferentOrg_Throws()
        {
            var rep = new Representative("Alice", "CIT123", "PT", "a@gmail.com", "+351912345678");
            rep.AssignToOrganization(new OrganizationId("ORG1"));

            Action act = () => rep.AssignToOrganization(new OrganizationId("ORG2"));

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Representative already belongs to another organization.");
        }

        [Fact]
        public void AssignToOrganization_WhenNull_Throws()
        {
            var rep = new Representative("Alice", "CIT123", "PT", "a@gmail.com", "+351912345678");

            Action act = () => rep.AssignToOrganization(null);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Organization ID cannot be null.");
        }

        [Fact]
        public void Update_ChangesProvidedFieldsOnly()
        {
            var rep = new Representative("Alice", "CIT123", "PT", "a@gmail.com", "+351912345678");

            rep.Update("Bob", "CIT456", "ES", "bob@test.com", "+34911222333");

            rep.Name.Should().Be("Bob");
            rep.Id.AsString().Should().Be("CIT456");
            rep.Nationality.Should().Be("ES");
            rep.Email.Should().Be("bob@test.com");
            rep.PhoneNumber.Should().Be("+34911222333");
        }

        [Fact]
        public void Deactivate_And_Activate_WorkCorrectly()
        {
            var rep = new Representative("Alice", "CIT123", "PT", "a@gmail.com", "+351912345678");

            rep.Deactivate();
            rep.Status.Should().Be(RepresentativeStatus.Inactive);

            rep.Activate();
            rep.Status.Should().Be(RepresentativeStatus.Active);
        }

        [Fact]
        public void Deactivate_WhenAlreadyInactive_Throws()
        {
            var rep = new Representative("Alice", "CIT123", "PT", "a@gmail.com", "+351912345678");
            rep.Deactivate();

            Action act = () => rep.Deactivate();

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Representative is already inactive.");
        }

        [Fact]
        public void Activate_WhenAlreadyActive_Throws()
        {
            var rep = new Representative("Alice", "CIT123", "PT", "a@gmail.com", "+351912345678");

            Action act = () => rep.Activate();

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Representative is already active.");
        }
        
        [Fact]
        public void WhenRepresentativeHasOrganization_AssignmentIsRemoved()
        {
            // Arrange
            var rep = new Representative("Alice", "CIT123", "PT", "a@gmail.com", "+351912345678");
            var orgId = new OrganizationId("ORG1");
            rep.AssignToOrganization(orgId);

            
            rep.UnassignFromOrganization();

            rep.OrganizationId.Should().BeNull();
        }

        [Fact]
        public void WhenRepresentativeHasNoOrganization_UnassignDoesNothing()
        {
            var rep = new Representative("Alice", "CIT123", "PT", "a@gmail.com", "+351912345678");
            
            Action act = () => rep.UnassignFromOrganization();
            
            act.Should().NotThrow();
            rep.OrganizationId.Should().BeNull();
        }
    }
}
