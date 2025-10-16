using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDNetCore.Infraestructure.PortInfrastructure.DTOs;
using DDDSample1.Domain.Docks;
using DDDSample1.Domain.PortInfrastructure.StorageArea;
//using DDDSample1.Domain.PortInfrastructure.StorageArea;

namespace DDDNetCore.Domain.PortInfrastructure.StorageArea;

public class StorageAreaService
{
    private readonly IStorageAreaRepository _repo;

    public StorageAreaService(IStorageAreaRepository repo)
    {
        _repo = repo;
    }

    public async Task<StorageAreaDto> RegisterAsync(StorageAreaDto dto)
    {
        var area = new DDDSample1.Domain.PortInfrastructure.StorageArea.StorageArea(dto.Type, dto.Location, dto.MaxCapacityTEUs);
        area.UpdateDetails(dto.Type, dto.Location, dto.MaxCapacityTEUs, dto.CurrentOccupancyTEUs);

        foreach (var dock in dto.AssignedDocks ?? new List<DockAssignmentDto>())
        {
            area.AssignDock(new DockID(dock.DockId), dock.DistanceMeters);
        }

        await _repo.AddAsync(area);
        return ToDto(area);
    }

    public async Task<StorageAreaDto> UpdateAsync(Guid id, StorageAreaDto dto)
    {
        var area = await _repo.GetByIdAsync(new StorageAreaID(id));
        if (area == null) return null;

        area.UpdateDetails(dto.Type, dto.Location, dto.MaxCapacityTEUs, dto.CurrentOccupancyTEUs);
        return ToDto(area);
    }

    private StorageAreaDto ToDto(DDDSample1.Domain.PortInfrastructure.StorageArea.StorageArea area)
    {
        return new StorageAreaDto
        {
            Id = area.Id.AsGuid(),
            Type = area.Type,
            Location = area.Location,
            MaxCapacityTEUs = area.MaxCapacityTEUs,
            CurrentOccupancyTEUs = area.CurrentOccupancyTEUs,
            AssignedDocks = area.DockAssignments.Select(a => new DockAssignmentDto
            {
                DockId = a.DockId.AsGuid(),
                DistanceMeters = a.DistanceMeters
            }).ToList()
        };
    }

    public async Task<object> GetByIdAsync(StorageAreaID storageAreaId)
    {
        var area = await _repo.GetByIdAsync(storageAreaId);
        if (area == null) return null;
        return ToDto(area);
    }
}
