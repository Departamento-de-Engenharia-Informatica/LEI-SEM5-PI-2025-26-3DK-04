using FluentAssertions;
using Xunit;
using System;
using DDDSample1.Domain.Qualifications;
using DDDSample1.Domain.Shared;

namespace DDDNetCore.Tests.Domain
{
    public class QualificationTests
    {
        [Theory]
        [InlineData("Qualification 1")]
        [InlineData("Qualification 2")]
        [InlineData("Qualification 3")]
        public void WhenPassingCorrectData_ThenQualificationIsInstantiated(string name)
        {
            var qual = new Qualification(name);

            qual.Name.Should().Be(name);
            qual.Id.Should().NotBeNull();
        }

        [Theory]
        [InlineData("qualification")]
        [InlineData("name")]
        [InlineData("")]
        public void WhenPassingInvalidData_ThenBusinessRuleValidationExceptionIsThrown(string name)
        {
            Action act = () => new Qualification(name);

            act.Should().Throw<BusinessRuleValidationException>();
        }
        
        [Theory]
        [InlineData("")]
        [InlineData("   ")]
        [InlineData("\r\n \t ")]
        [InlineData(null)]
        public void WhenPassingEmptyOrNullName_ThenThrowsException(string name)
        {
            Action act = () => new Qualification(name);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Qualification name is required.");
        }

        [Theory]
        [InlineData("SingleWord")]
        [InlineData("Name")]
        public void WhenPassingNameWithLessThanTwoWords_ThenThrowsException(string name)
        {
            Action act = () => new Qualification(name);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Qualification name must contain at least two words.");
        }

        [Fact]
        public void WhenPassingNameWithMoreThan150Characters_ThenThrowsException()
        {
            string longName = new string('a', 75) + " " + new string('b', 76); // 151 chars

            Action act = () => new Qualification(longName);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Qualification name must have a maximum length of 150 characters.");
        }

        [Fact]
        public void WhenChangingToValidName_ThenNameIsUpdated()
        {
            var qual = new Qualification("Qualification 1");
            string newName = "Qualification 1 2";

            qual.ChangeName(newName);

            qual.Name.Should().Be(newName);
        }

        [Theory]
        [InlineData("")]
        [InlineData("   ")]
        [InlineData(null)]
        [InlineData("SingleWord")]
        public void WhenChangingToInvalidName_ThenThrowsException(string invalidName)
        {
            var qual = new Qualification("Qualification 1");

            Action act = () => qual.ChangeName(invalidName);

            act.Should().Throw<BusinessRuleValidationException>();
        }

        [Fact]
        public void WhenChangingToNameWithMoreThan150Characters_ThenThrowsException()
        {
            var qual = new Qualification("Software Engineer");
            string longName = new string('a', 75) + " " + new string('b', 76); // 151 chars

            Action act = () => qual.ChangeName(longName);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Qualification name must have a maximum length of 150 characters.");
        }

        [Fact]
        public void WhenCreatingTwoQualifications_ShouldHaveDifferentIds()
        {
            var qual1 = new Qualification("Qualification 1");
            var qual2 = new Qualification("Qualification 2");

            qual1.Id.Should().NotBe(qual2.Id);
        }
    }
}