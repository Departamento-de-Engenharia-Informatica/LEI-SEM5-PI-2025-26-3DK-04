using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.StaffMembers
{
    public class StaffMemberService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IStaffMemberRepository _repo;
        
        public StaffMemberService(
            IUnitOfWork unitOfWork,
            IStaffMemberRepository repo)
        {
            _unitOfWork = unitOfWork;
            _repo = repo;
        }
        
        // Criar novo staff member
        public async Task<StaffMemberDto> CreateAsync(CreateStaffMemberDto dto)
        {
            // Criar entidade de domínio
            var staffMember = new StaffMember(
                dto.Name,
                dto.Email,
                dto.PhoneNumber,
                dto.OperationalWindow,
                dto.Status
            );
            
            // Adicionar ao repositório
            await _repo.AddAsync(staffMember);
            
            // Persistir mudanças
            await _unitOfWork.CommitAsync();
            
            return MapToDto(staffMember);
        }
        
        // Obter staff member por ID
        public async Task<StaffMemberDto> GetByIdAsync(Guid id)
        {
            var staffMember = await _repo.GetByIdAsync(new StaffMemberID(id));
            
            if (staffMember == null)
                return null;
            
            return MapToDto(staffMember);
        }
        
        // Atualizar staff member
        public async Task<StaffMemberDto> UpdateAsync(Guid id, UpdateStaffMemberDto dto)
        {
            var staffMember = await _repo.GetByIdAsync(new StaffMemberID(id));
            
            if (staffMember == null)
                throw new BusinessRuleValidationException("Staff member not found.");
            
            // Atualizar usando métodos de domínio
            if (!string.IsNullOrWhiteSpace(dto.Name))
                staffMember.ChangeName(dto.Name);
            
            if (!string.IsNullOrWhiteSpace(dto.Email))
                staffMember.ChangeEmail(dto.Email);
            
            if (dto.PhoneNumber.HasValue)
                staffMember.ChangePhoneNumber(dto.PhoneNumber.Value);
            
            if (!string.IsNullOrWhiteSpace(dto.OperationalWindow))
                staffMember.ChangeOperationalWindow(dto.OperationalWindow);
            
            if (dto.Status.HasValue)
                staffMember.ChangeStatus(dto.Status.Value);
            
            // Persistir mudanças
            await _unitOfWork.CommitAsync();
            
            return MapToDto(staffMember);
        }
        
        // Desativar staff member (soft delete)
        public async Task<StaffMemberDto> DeactivateAsync(Guid id)
        {
            var staffMember = await _repo.GetByIdAsync(new StaffMemberID(id));
            
            if (staffMember == null)
                throw new BusinessRuleValidationException("Staff member not found.");
            
            staffMember.Deactivate();
            
            await _unitOfWork.CommitAsync();
            
            return MapToDto(staffMember);
        }
        
        // Reativar staff member
        public async Task<StaffMemberDto> ReactivateAsync(Guid id)
        {
            var staffMember = await _repo.GetByIdAsync(new StaffMemberID(id));
            
            if (staffMember == null)
                throw new BusinessRuleValidationException("Staff member not found.");
            
            staffMember.Reactivate();
            
            await _unitOfWork.CommitAsync();
            
            return MapToDto(staffMember);
        }
        
        // Adicionar qualificação
        public async Task<StaffMemberDto> AddQualificationAsync(Guid staffId, Guid qualificationId)
        {
            var staffMember = await _repo.GetByIdAsync(new StaffMemberID(staffId));
            
            if (staffMember == null)
                throw new BusinessRuleValidationException("Staff member not found.");
            
            // TODO: Buscar qualificação do repositório de Qualifications
            // var qualification = await _qualificationRepo.GetByIdAsync(qualificationId);
            // if (qualification == null)
            //     throw new BusinessRuleValidationException("Qualification not found.");
            
            // staffMember.AddQualification(qualification);
            
            await _unitOfWork.CommitAsync();
            
            return MapToDto(staffMember);
        }
        
        // Remover qualificação
        public async Task<StaffMemberDto> RemoveQualificationAsync(Guid staffId, Guid qualificationId)
        {
            var staffMember = await _repo.GetByIdAsync(new StaffMemberID(staffId));
            
            if (staffMember == null)
                throw new BusinessRuleValidationException("Staff member not found.");
            
            // TODO: Buscar qualificação e remover
            // var qualification = staffMember.Qualifications.FirstOrDefault(q => q.Id == qualificationId);
            // if (qualification == null)
            //     throw new BusinessRuleValidationException("Qualification not found.");
            
            // staffMember.RemoveQualification(qualification);
            
            await _unitOfWork.CommitAsync();
            
            return MapToDto(staffMember);
        }
        
        // Listar todos os staff members ativos
        public async Task<List<StaffMemberDto>> GetAllActiveAsync()
        {
            var staffMembers = await _repo.GetActiveStaffAsync();
            return staffMembers.Select(s => MapToDto(s)).ToList();
        }
        
        // Listar todos os staff members
        public async Task<List<StaffMemberDto>> GetAllForAuditAsync()
        {
            var staffMembers = await _repo.GetAllForAuditAsync();
            return staffMembers.Select(s => MapToDto(s)).ToList();
        }
        
        // Pesquisar por nome
        public async Task<List<StaffMemberDto>> SearchByNameAsync(string name)
        {
            var staffMembers = await _repo.GetByNameAsync(name);
            return staffMembers.Select(s => MapToDto(s)).ToList();
        }
        
        // Filtrar por status
        public async Task<List<StaffMemberDto>> GetByStatusAsync(MemberStatus status)
        {
            var staffMembers = await _repo.GetByStatusAsync(status);
            return staffMembers.Select(s => MapToDto(s)).ToList();
        }
        
        // Filtrar por qualificação
        public async Task<List<StaffMemberDto>> GetByQualificationAsync(Guid qualificationId)
        {
            var staffMembers = await _repo.GetByQualificationAsync(qualificationId);
            return staffMembers.Select(s => MapToDto(s)).ToList();
        }
        
        // Pesquisa combinada (múltiplos filtros)
        public async Task<List<StaffMemberDto>> SearchAsync(
            string name = null,
            MemberStatus? status = null,
            Guid? qualificationId = null)
        {
            var staffMembers = await _repo.SearchAsync(name, status, qualificationId);
            return staffMembers.Select(s => MapToDto(s)).ToList();
        }
        
        // Mapear entidade para DTO
        private StaffMemberDto MapToDto(StaffMember staffMember)
        {
            return new StaffMemberDto
            {
                Id = staffMember.Id.AsGuid(),
                Name = staffMember.Name,
                Email = staffMember.Email,
                PhoneNumber = staffMember.PhoneNumber,
                OperationalWindow = staffMember.OperationalWindow,
                Status = staffMember.Status,
                Qualifications = staffMember.Qualifications?.Select(q => new QualificationDto
                {
                    Id = q.Id,
                    Name = q.Name,
                    Description = q.Description
                }).ToList() ?? new List<QualificationDto>()
            };
        }
    }
}
