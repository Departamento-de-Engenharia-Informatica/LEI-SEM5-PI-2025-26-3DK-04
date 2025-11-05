using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Qualifications;

namespace DDDSample1.Domain.StaffMembers
{
    public class StaffMemberService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IStaffMemberRepository _repo;
        private readonly IQualificationRepository _qualificationRepo;
        
        public StaffMemberService(
            IUnitOfWork unitOfWork,
            IStaffMemberRepository repo,
            IQualificationRepository qualificationRepo)
        {
            _unitOfWork = unitOfWork;
            _repo = repo;
            _qualificationRepo = qualificationRepo;
        }
        
        // Criar novo staff member
        public async Task<StaffMemberDto> CreateAsync(CreateStaffMemberDto dto)
        {
            // Criar entidade de domínio (sem qualificações inicialmente)
            // Deferir conversão de OperationalWindowDto para domínio: passar string ao construtor
            // para que a validação dentro do domínio (email -> phone -> operationalWindow) ocorra na ordem do construtor
            string operationalWindowString = null;
            if (dto.OperationalWindow != null)
            {
                var start = dto.OperationalWindow.StartTime?.Trim();
                var end = dto.OperationalWindow.EndTime?.Trim();
                operationalWindowString = $"{start}-{end}";
            }

            var staffMember = new StaffMember(
                dto.Name,
                dto.Email,
                dto.PhoneNumber,
                operationalWindowString
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
            
            if (dto.OperationalWindow != null)
                staffMember.ChangeOperationalWindow(dto.OperationalWindow.ToDomain());
            
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
            
            // Procurar qualificação do repositório de Qualifications
            var qualification = await _qualificationRepo.GetByIdAsync(new QualificationID(qualificationId));
            if (qualification == null)
                throw new BusinessRuleValidationException("Qualification not found.");
            
            staffMember.AddQualification(qualification);
            
            await _unitOfWork.CommitAsync();
            
            return MapToDto(staffMember);
        }
        
        // Remover qualificação
        public async Task<StaffMemberDto> RemoveQualificationAsync(Guid staffId, Guid qualificationId)
        {
            var staffMember = await _repo.GetByIdAsync(new StaffMemberID(staffId));
            
            if (staffMember == null)
                throw new BusinessRuleValidationException("Staff member not found.");
            
            // Procurar qualificação na lista do staff member
            var qualification = staffMember.Qualifications.FirstOrDefault(q => q.Id.AsGuid() == qualificationId);
            if (qualification == null)
                throw new BusinessRuleValidationException("Qualification not found in this staff member.");
            
            staffMember.RemoveQualification(qualification);
            
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
            var dto = new StaffMemberDto(
                staffMember.Name,
                staffMember.Email,
                staffMember.PhoneNumber,
                OperationalWindowDto.FromDomain(staffMember.OperationalWindow)
            )
            {
                Id = staffMember.Id.AsGuid(),
                Status = staffMember.Status,
                Qualifications = staffMember.Qualifications?.Select(q => new QualificationDto
                {
                    Id = q.Id.AsGuid(),
                    Name = q.Name
                }).ToList() ?? new List<QualificationDto>()
            };
            
            return dto;
        }   
    }
}
