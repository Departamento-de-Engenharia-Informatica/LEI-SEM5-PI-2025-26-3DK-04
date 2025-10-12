namespace DDDSample1.Domain.StaffMembers
{
    /// <summary>
    /// DTO para criar um novo StaffMember
    /// Usado em: POST /api/staffmembers
    /// </summary>
    public class CreateStaffMemberDto
    {
        public string Name { get; set; }
        public string Email { get; set; }
        public int PhoneNumber { get; set; }
        public string OperationalWindow { get; set; }
        public MemberStatus Status { get; set; }
        
        // Qualifications começam vazias, podem ser adicionadas depois via endpoint específico
    }
}
