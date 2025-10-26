using System;
using System.Collections.Generic;
using FluentAssertions;
using Xunit;
using DDDSample1.Domain.StaffMembers;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Qualifications;

namespace DDDNetCore.Tests.Domain
{
	public class StaffMemberTests
	{
		[Fact]
		public void WhenPassingCorrectData_ThenStaffMemberIsInstantiated()
		{
			var staff = new StaffMember("John Doe","john@example.com",912345678,"08:00-17:00");

			staff.Name.Should().Be("John Doe");
			staff.Email.Should().Be("john@example.com");
			staff.PhoneNumber.Should().Be(912345678);
			staff.OperationalWindow.ToString().Should().Be("08:00-17:00");
			staff.Status.Should().Be(MemberStatus.Available);
			staff.Qualifications.Should().NotBeNull();
			staff.Qualifications.Should().BeEmpty();
			staff.Id.Should().NotBeNull();
		}

		[Theory]
		[InlineData("")]
		[InlineData("no-at-symbol")]
		public void WhenPassingInvalidEmail_ThenBusinessRuleValidationExceptionIsThrown(string email)
		{
			Action act = () => new StaffMember("John Doe", email, 912345678, "08:00-17:00");

			act.Should().Throw<BusinessRuleValidationException>();
		}

		[Theory]
		[InlineData(0)]
		[InlineData(-1)]
		[InlineData(123)]
		[InlineData(12345678)]
		[InlineData(1234567890)]
		public void WhenPassingInvalidPhoneNumber_ThenBusinessRuleValidationExceptionIsThrown(int phone)
		{
			Action act = () => new StaffMember("John Doe", "john@example.com", phone, "08:00-17:00");

			act.Should().Throw<BusinessRuleValidationException>();
		}

		[Fact]
		public void WhenChangingToValidName_ThenNameIsUpdated()
		{
			var staff = new StaffMember("John Doe","john@example.com",912345678,"08:00-17:00");

			staff.ChangeName("Jane Smith");

			staff.Name.Should().Be("Jane Smith");
		}

		[Theory]
		[InlineData("")]
		[InlineData("   ")]
		[InlineData(null)]
		public void WhenChangingToInvalidName_ThenThrowsException(string invalidName)
		{
			var staff = new StaffMember("John Doe","john@example.com",912345678,"08:00-17:00");

			Action act = () => staff.ChangeName(invalidName);

			act.Should().Throw<BusinessRuleValidationException>();
		}

		[Fact]
		public void WhenChangingToValidEmail_ThenEmailIsUpdated()
		{
			var staff = new StaffMember("John Doe","john@example.com",912345678,"08:00-17:00");

			staff.ChangeEmail("jane@domain.com");

			staff.Email.Should().Be("jane@domain.com");
		}

		[Theory]
		[InlineData("")]
		[InlineData("no-at")]
		[InlineData(null)]
		public void WhenChangingToInvalidEmail_ThenThrowsException(string invalidEmail)
		{
			var staff = new StaffMember("John Doe","john@example.com",912345678,"08:00-17:00");

			Action act = () => staff.ChangeEmail(invalidEmail);

			act.Should().Throw<BusinessRuleValidationException>();
		}

		[Fact]
		public void WhenChangingPhoneNumberToValid_ThenUpdated()
		{
			var staff = new StaffMember("John Doe","john@example.com",912345678,"08:00-17:00");

			staff.ChangePhoneNumber(999888777);

			staff.PhoneNumber.Should().Be(999888777);
		}

		[Theory]
		[InlineData(0)]
		[InlineData(-10)]
		[InlineData(12345)]
		public void WhenChangingPhoneNumberToInvalid_ThenThrows(int phone)
		{
			var staff = new StaffMember("John Doe","john@example.com",912345678,"08:00-17:00");

			Action act = () => staff.ChangePhoneNumber(phone);

			act.Should().Throw<BusinessRuleValidationException>();
		}

		[Fact]
		public void WhenAddingQualification_ThenItIsAdded()
		{
			var staff = new StaffMember("John Doe","john@example.com",912345678,"08:00-17:00");
			var qual = new Qualification("Software Engineer");

			staff.AddQualification(qual);

			staff.Qualifications.Should().ContainSingle().Which.Should().Be(qual);
		}

		[Fact]
		public void WhenAddingNullQualification_ThenThrows()
		{
			var staff = new StaffMember("John Doe","john@example.com",912345678,"08:00-17:00");

			Action act = () => staff.AddQualification(null);

			act.Should().Throw<BusinessRuleValidationException>();
		}

		[Fact]
		public void WhenAddingDuplicateQualification_ThenThrows()
		{
			var qual = new Qualification("Software Engineer");
			var staff = new StaffMember("John Doe","john@example.com",912345678,"08:00-17:00", new List<Qualification>{ qual });

			Action act = () => staff.AddQualification(qual);

			act.Should().Throw<BusinessRuleValidationException>()
				.WithMessage("Qualification already exists for this staff member.");
		}

		[Fact]
		public void WhenRemovingNonexistentQualification_ThenThrows()
		{
			var qual = new Qualification("Software Engineer");
			var staff = new StaffMember("John Doe","john@example.com",912345678,"08:00-17:00");

			Action act = () => staff.RemoveQualification(qual);

			act.Should().Throw<BusinessRuleValidationException>();
		}

		[Fact]
		public void WhenClearingQualifications_ThenListIsEmpty()
		{
			var qual = new Qualification("Software Engineer");
			var staff = new StaffMember("John Doe","john@example.com",912345678,"08:00-17:00", new List<Qualification>{ qual });

			staff.ClearQualifications();

			staff.Qualifications.Should().BeEmpty();
		}

		[Fact]
		public void WhenUpdatingQualificationsWithNull_ThenThrows()
		{
			var staff = new StaffMember("John Doe","john@example.com",912345678,"08:00-17:00");

			Action act = () => staff.UpdateQualifications(null);

			act.Should().Throw<BusinessRuleValidationException>();
		}

		[Fact]
		public void WhenUpdatingQualifications_ThenListIsReplaced()
		{
			var qual1 = new Qualification("Software Engineer");
			var qual2 = new Qualification("Project Manager");
			var staff = new StaffMember("John Doe","john@example.com",912345678,"08:00-17:00", new List<Qualification>{ qual1 });

			staff.UpdateQualifications(new List<Qualification>{ qual2 });

			staff.Qualifications.Should().ContainSingle().Which.Should().Be(qual2);
		}

		[Fact]
		public void WhenDeactivatingAndReactivating_StatusChangesAppropriately()
		{
			var staff = new StaffMember("John Doe","john@example.com",912345678,"08:00-17:00");

			staff.Deactivate();
			staff.Status.Should().Be(MemberStatus.Unavailable);

			// Deactivating again should throw
			Action act = () => staff.Deactivate();
			act.Should().Throw<BusinessRuleValidationException>();

			staff.Reactivate();
			staff.Status.Should().Be(MemberStatus.Available);

			// Reactivating again should throw
			Action act2 = () => staff.Reactivate();
			act2.Should().Throw<BusinessRuleValidationException>();
		}

		[Fact]
		public void WhenCreatingTwoStaffMembers_ShouldHaveDifferentIds()
		{
			var s1 = new StaffMember("A","a@a.com",912345678,"08:00-17:00");
			var s2 = new StaffMember("B","b@b.com",912345679,"08:00-17:00");

			s1.Id.Should().NotBe(s2.Id);
		}

		[Fact]
		public void WhenDeactivatingAlreadyUnavailable_ThenThrows()
		{
			var staff = new StaffMember("John Doe","john@example.com",912345678,"08:00-17:00");

			// Deactivate once to put in Unavailable state
			staff.Deactivate();

			Action act = () => staff.Deactivate();

			act.Should().Throw<BusinessRuleValidationException>()
				.WithMessage("Staff member is already inactive.");
		}

		[Fact]
		public void WhenReactivatingAlreadyAvailable_ThenThrows()
		{
			var staff = new StaffMember("John Doe","john@example.com",912345678,"08:00-17:00");

			// By default staff is Available, so Reactivate should throw
			Action act = () => staff.Reactivate();

			act.Should().Throw<BusinessRuleValidationException>()
				.WithMessage("Staff member is already active.");
		}
	}
}

