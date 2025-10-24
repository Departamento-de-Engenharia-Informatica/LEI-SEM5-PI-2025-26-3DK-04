using System;
using System.Linq;
using FluentAssertions;
using Xunit;
using DDDSample1.Domain.Organizations;
using DDDSample1.Domain.Shared;
using System.Collections.Generic;

namespace DDDNetCore.Tests.Domain
{
    public class OrganizationTests
    {
        private Representative CreateRep(string idSuffix = "1")
        {
            return new Representative("Alice" + idSuffix, "CIT12345" + idSuffix, "PT", $"a{idSuffix}@gmail.com", $"+35191234{idSuffix}56");
        }

        [Fact]
        public void WhenCreatingWithValidData_ThenIsCreated()
        {
            var org = new Organization("ORG1", "Legal", "Alt", "Street 1", "PT123456789");

            org.Id.AsString().Should().Be("ORG1");
            org.LegalName.Should().Be("Legal");
            org.AlternativeName.Should().Be("Alt");
            org.Address.Should().Be("Street 1");
            org.TaxNumber.Should().Be("PT123456789");
            org.Representatives.Should().BeEmpty();
        }

        [Theory]
        [InlineData("", "Legal", "Alt", "Addr", "PT123456789", "Organization identifier is required.")]
        [InlineData("ORG1", "", "Alt", "Addr", "PT123456789", "Legal name is required.")]
        [InlineData("ORG1", "Legal", "Alt", "", "PT123456789", "Address is required.")]
        public void WhenMissingRequiredField_ThenThrows(string id, string name, string alt, string addr, string tax, string expected)
        {
            Action act = () => new Organization(id, name, alt, addr, tax);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage(expected);
        }

        [Fact]
        public void AddRepresentative_AssignsSuccessfully()
        {
            var org = new Organization("ORG1", "Legal", "Alt", "Street", "PT123456789");
            var rep = CreateRep();

            org.AddRepresentative(rep);

            org.Representatives.Should().ContainSingle()
                .Which.Should().Be(rep);

            rep.OrganizationId.Should().Be(org.Id);
        }

        [Fact]
        public void AddRepresentative_WhenNull_Throws()
        {
            var org = new Organization("ORG1", "Legal", "Alt", "Street", "PT123456789");

            Action act = () => org.AddRepresentative(null);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Representative cannot be null.");
        }

        [Fact]
        public void AddRepresentative_WhenSameEmailOrPhone_Throws()
        {
            var org = new Organization("ORG1", "Legal", "Alt", "Street", "PT123456789");
            var rep1 = CreateRep("1");
            var rep2 = CreateRep("2");

            org.AddRepresentative(rep1);
            // duplicate email
            rep2 = new Representative("Bob", "CIT23451", "PT", rep1.Email, "+351900000000");

            Action actEmail = () => org.AddRepresentative(rep2);
            actEmail.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("A representative with this email already exists in this organization.");

            // duplicate phone
            var rep3 = new Representative("Bob", "CIT322456", "PT", "b@gmail.com", rep1.PhoneNumber);
            Action actPhone = () => org.AddRepresentative(rep3);
            actPhone.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("A representative with this phone number already exists in this organization.");
        }

        [Fact]
        public void AddRepresentative_WhenBelongsToAnotherOrg_Throws()
        {
            var org1 = new Organization("ORG1", "Legal", "Alt", "Street", "PT123456789");
            var org2 = new Organization("ORG2", "Legal2", "Alt2", "Street2", "PT987654321");
            var rep = CreateRep();

            rep.AssignToOrganization(org2.Id);

            Action act = () => org1.AddRepresentative(rep);
            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Representative already assigned to another organization.");
        }

        [Fact]
        public void HasRepresentative_ReturnsTrueWhenListNotEmpty()
        {
            var org = new Organization("ORG1", "Legal", "Alt", "Street", "PT123456789");
            org.HasRepresentative().Should().BeFalse();

            org.AddRepresentative(CreateRep());
            org.HasRepresentative().Should().BeTrue();
        }

        [Fact]
        public void ValidateReadyForRegistration_WithoutReps_Throws()
        {
            var org = new Organization("ORG1", "Legal", "Alt", "Street", "PT123456789");

            Action act = () => org.ValidateReadyForRegistration();

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("An organization must have at least one representative at registration.");
        }

        [Fact]
        public void RemoveRepresentative_RemovesSuccessfully()
        {
            var org = new Organization("ORG1", "Legal", "Alt", "Street", "PT123456789");
            var rep = CreateRep();

            org.AddRepresentative(rep);
            org.Representatives.Should().ContainSingle();

            org.RemoveRepresentative(rep);

            org.Representatives.Should().BeEmpty();
        }

        [Fact]
        public void RemoveRepresentative_WhenNull_DoesNothing()
        {
            var org = new Organization("ORG1", "Legal", "Alt", "Street", "PT123456789");

            Action act = () => org.RemoveRepresentative(null);

            act.Should().NotThrow();
            org.Representatives.Should().BeEmpty();
        }
    }
}
