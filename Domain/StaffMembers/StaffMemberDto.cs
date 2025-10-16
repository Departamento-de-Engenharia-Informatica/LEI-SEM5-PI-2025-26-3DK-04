using System;
using System.Collections.Generic;
using DDDSample1.Domain.Qualifications;

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
            this.Id = Guid.NewGuid();
            this.Name = name;
            this.Email = email;
            this.PhoneNumber = phoneNumber;
            this.OperationalWindow = operationalWindow;
            this.Status = MemberStatus.Avaliable;
            this.Qualifications = new List<QualificationDto>(); // Lista vazia
        }
    }
}
