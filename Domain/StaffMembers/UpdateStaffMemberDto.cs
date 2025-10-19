namespace DDDSample1.Domain.StaffMembers
{
    /// <summary>
    /// DTO para atualizar um StaffMember existente
    /// Usado em: PUT /api/staffmembers/{id}
    /// Campos nullable permitem atualização parcial - só altera os campos fornecidos
    /// Nota: Strings já são nullable por padrão
    /// O ID (número mecanográfico) NÃO pode ser modificado
    /// </summary>
    public class UpdateStaffMemberDto
    {
        public string Name { get; set; }
        public string Email { get; set; }
        public int? PhoneNumber { get; set; }
        public string OperationalWindow { get; set; }
        public MemberStatus? Status { get; set; }
        
        // Qualifications não são atualizadas por aqui, usar endpoints específicos
    }
}
