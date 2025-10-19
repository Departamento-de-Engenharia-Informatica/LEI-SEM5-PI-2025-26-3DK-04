using System;
using System.Collections.Generic;
using DDDSample1.Domain.Qualifications;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.StaffMembers
{
    public class StaffMember : Entity<StaffMemberID>, IAggregateRoot
    {
        public string Name { get; private set; }
        public string Email { get; private set; }

        public int PhoneNumber { get; private set; }

        public string OperationalWindow { get; private set; }

        public MemberStatus Status { get; private set; }

        public List<Qualification> Qualifications { get; private set; }

        // Construtor para inicializar a lista de qualificações
        public StaffMember(string name, string email, int phoneNumber, string operationalWindow)
        {
            this.Id = new StaffMemberID(Guid.NewGuid());
            this.Name = name;
            this.Email = email;
            this.PhoneNumber = phoneNumber;
            this.OperationalWindow = operationalWindow;
            this.Status = MemberStatus.Available; // Status inicia sempre como Available
            this.Qualifications = new List<Qualification>(); // Lista vazia
        }

        // Construtor que aceita uma lista de qualificações
        public StaffMember(string name, string email, int phoneNumber, string operationalWindow, List<Qualification> qualifications)
        {
            this.Id = new StaffMemberID(Guid.NewGuid());
            this.Name = name;
            this.Email = email;
            this.PhoneNumber = phoneNumber;
            this.OperationalWindow = operationalWindow;
            this.Status = MemberStatus.Available; // Status sempre inicia como Available
            this.Qualifications = qualifications;
        }

        // Parameterless constructor for ORM
        private StaffMember() { }

        public void ChangeName(string name)
        {
            if (string.IsNullOrWhiteSpace(name))
                throw new BusinessRuleValidationException("Name cannot be empty.");
            
            this.Name = name;
        }

        public void ChangeEmail(string email)
        {
            if (string.IsNullOrWhiteSpace(email))
                throw new BusinessRuleValidationException("Email cannot be empty.");
            
            // formato de email
            if (!email.Contains("@"))
                throw new BusinessRuleValidationException("Invalid email format.");
            
            this.Email = email;
        }

        public void ChangePhoneNumber(int phoneNumber)
        {
            if (phoneNumber <= 0)
                throw new BusinessRuleValidationException("Invalid phone number.");

            // formato de telefone
            if (phoneNumber.ToString().Length != 9)
                throw new BusinessRuleValidationException("Phone number needs to have 9 numbers.");    
            
            this.PhoneNumber = phoneNumber;
        }

        public void ChangeOperationalWindow(string operationalWindow)
        {
            this.OperationalWindow = operationalWindow;
        }

        public void ChangeStatus(MemberStatus status)
        {
            this.Status = status;
        }

        // Adicionar uma qualificação
        public void AddQualification(Qualification qualification)
        {
            if (qualification == null)
                throw new BusinessRuleValidationException("Qualification cannot be null.");
            
            // Validar se já existe (evitar duplicados)
            if (this.Qualifications.Exists(q => q.Id == qualification.Id))
                throw new BusinessRuleValidationException("Qualification already exists for this staff member.");
            
            this.Qualifications.Add(qualification);
        }

        // Remover uma qualificação
        public void RemoveQualification(Qualification qualification)
        {
            if (qualification == null)
                throw new BusinessRuleValidationException("Qualification cannot be null.");
            
            if (!this.Qualifications.Remove(qualification))
                throw new BusinessRuleValidationException("Qualification not found.");
        }

        // Limpar todas as qualificações
        public void ClearQualifications()
        {
            this.Qualifications.Clear();
        }

        // Substituir todas as qualificações
        public void UpdateQualifications(List<Qualification> qualifications)
        {
            if (qualifications == null)
                throw new BusinessRuleValidationException("Qualifications list cannot be null.");
            
            this.Qualifications = new List<Qualification>(qualifications); // Cópia defensiva
        }

        // Desativar staff member
        public void Deactivate()
        {
            if (this.Status == MemberStatus.Unavailable)
                throw new BusinessRuleValidationException("Staff member is already inactive.");
            
            this.Status = MemberStatus.Unavailable;
        }

        public void Reactivate()
        {
            if (this.Status == MemberStatus.Available)
                throw new BusinessRuleValidationException("Staff member is already active.");
            
            this.Status = MemberStatus.Available;
        }
    }
}