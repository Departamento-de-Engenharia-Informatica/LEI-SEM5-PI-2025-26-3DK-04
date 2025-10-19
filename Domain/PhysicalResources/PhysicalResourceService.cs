using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.PhysicalResources.DTOs;
using DDDSample1.Domain.Qualifications;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.PhysicalResources;

public class PhysicalResourceService
{
    private readonly IPhysicalResourceRepository _repo;
    private readonly IUnitOfWork _unitOfWork;

    public PhysicalResourceService(IPhysicalResourceRepository repo, IUnitOfWork unitOfWork)
    {
        _repo = repo;
        _unitOfWork = unitOfWork;
    }

    public async Task<PhysicalResourceDto> CreateAsync(CreatePhysicalResourceDto dto)
    {
        var qualificationIds = dto.QualificationIds.Select(id => new QualificationID(id)).ToList();

        var resource = new PhysicalResource(
            dto.Description, dto.Type, dto.Capacity,
            dto.AssignedArea, dto.SetupTime, dto.Status, qualificationIds);

        await _repo.AddAsync(resource);
        await _unitOfWork.CommitAsync();

        return Map(resource);
    }

    public async Task<PhysicalResourceDto> UpdateAsync(Guid id, UpdatePhysicalResourceDto dto)
    {
        var resource = await _repo.GetByIdAsync(new PhysicalResourceId(id));
        if (resource == null)
            throw new BusinessRuleValidationException("Resource not found.");

        var qualificationIds = dto.QualificationIds.Select(id => new QualificationID(id)).ToList();
        resource.Update(dto.Description, dto.Capacity, dto.AssignedArea, dto.SetupTime, qualificationIds);

        await _unitOfWork.CommitAsync();
        return Map(resource);
    }

    public async Task<PhysicalResourceDto> ChangeStatusAsync(Guid id, ResourceStatus newStatus)
    {
        var resource = await _repo.GetByIdAsync(new PhysicalResourceId(id));
        if (resource == null)
            throw new BusinessRuleValidationException("Resource not found.");

        resource.ChangeStatus(newStatus);
        await _unitOfWork.CommitAsync();

        return Map(resource);
    }

    public async Task<List<PhysicalResourceDto>> SearchAsync(string? description, string? type, ResourceStatus? status)
    {
        var results = await _repo.SearchAsync(description, type, status);
        return results.Select(Map).ToList();
    }

    private PhysicalResourceDto Map(PhysicalResource r) => new PhysicalResourceDto
    {
        Id = r.Id.AsGuid(),
        Description = r.Description,
        Type = r.Type,
        Capacity = r.Capacity,
        AssignedArea = r.AssignedArea,
        SetupTime = r.SetupTime,
        Status = r.Status,
        QualificationIds = r.QualificationIds.Select(q => q.Value).ToList()
    };
}
