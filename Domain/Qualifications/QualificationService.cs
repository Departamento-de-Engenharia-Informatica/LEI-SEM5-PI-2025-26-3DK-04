using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Qualifications
{
    public class QualificationService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IQualificationRepository _repo;
        
        public QualificationService(
            IUnitOfWork unitOfWork,
            IQualificationRepository repo)
        {
            _unitOfWork = unitOfWork;
            _repo = repo;
        }
        
        // Criar nova qualificação
        public async Task<QualificationDto> CreateAsync(CreateQualificationDto dto)
        {
            // Criar entidade de domínio (ID é único por natureza - GUID)
            var qualification = new Qualification(dto.Name);
            
            // Adicionar ao repositório
            await _repo.AddAsync(qualification);
            
            // Persistir mudanças
            await _unitOfWork.CommitAsync();
            
            return MapToDto(qualification);
        }
        
        // Obter qualificação por ID
        public async Task<QualificationDto> GetByIdAsync(Guid id)
        {
            var qualification = await _repo.GetByIdAsync(new QualificationID(id));
            
            if (qualification == null)
                return null;
            
            return MapToDto(qualification);
        }
        
        // Listar todas as qualificações
        public async Task<List<QualificationDto>> GetAllAsync()
        {
            var qualifications = await _repo.GetAllAsync();
            return qualifications.Select(q => MapToDto(q)).ToList();
        }
        
        // Atualizar qualificação
        public async Task<QualificationDto> UpdateAsync(Guid id, UpdateQualificationDto dto)
        {
            var qualification = await _repo.GetByIdAsync(new QualificationID(id));
            
            if (qualification == null)
                throw new BusinessRuleValidationException("Qualification not found.");
            
            // Atualizar usando métodos de domínio
            if (!string.IsNullOrWhiteSpace(dto.Name))
                qualification.ChangeName(dto.Name);
            
            // Persistir mudanças
            await _unitOfWork.CommitAsync();
            
            return MapToDto(qualification);
        }
        
        // Remover qualificação
        public async Task DeleteAsync(Guid id)
        {
            var qualification = await _repo.GetByIdAsync(new QualificationID(id));
            
            if (qualification == null)
                throw new BusinessRuleValidationException("Qualification not found.");
            
            // Remove da base de dados
            _repo.Remove(qualification);
            
            // Persistir mudanças
            await _unitOfWork.CommitAsync();
        }
        
        // Pesquisar por nome
        public async Task<List<QualificationDto>> SearchByNameAsync(string name)
        {
            var qualifications = await _repo.GetByNameAsync(name);
            return qualifications.Select(q => MapToDto(q)).ToList();
        }
        
        private QualificationDto MapToDto(Qualification qualification)
        {
            return new QualificationDto
            {
                Id = qualification.Id.AsGuid(),
                Name = qualification.Name
            };
        }
    }
}
