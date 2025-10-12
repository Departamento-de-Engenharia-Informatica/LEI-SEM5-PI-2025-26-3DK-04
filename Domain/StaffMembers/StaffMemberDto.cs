using System;
using System.Collections.Generic;

namespace DDDSample1.Domain.StaffMembers
{
    /// <summary>
    /// DTO de resposta para StaffMember
    /// </summary>
    public class StaffMemberDto
    {
        public Guid Id { get; set; }
        public string Name { get; set; }
        public string Email { get; set; }
        public int PhoneNumber { get; set; }
        public string OperationalWindow { get; set; }
        public MemberStatus Status { get; set; }
        public List<QualificationDto> Qualifications { get; set; }

        public StaffMemberDto(string name, string email, int phoneNumber, string operationalWindow)
        {
            this.Id = new StaffMemberID(Guid.NewGuid());
            this.Name = name;
            this.Email = email;
            this.PhoneNumber = phoneNumber;
            this.OperationalWindow = operationalWindow;
            this.Status = MemberStatus.Active;
            this.Qualifications = new List<Qualification>(); // Lista vazia
        }
    }
}
