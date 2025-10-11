using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Docks;
using DDDSample1.Domain.Shared;
//using DDDSample1.Domain.VesselTypes;


namespace DDDSample1.Infrastructure.Docks;

public class DockService
{
    private readonly IDockRepository _dockRepo;
    private readonly IVesselTypeRepository _vesselTypeRepo;

    public DockService(IDockRepository dockRepo, IVesselTypeRepository vesselTypeRepo)
    {
        _dockRepo = dockRepo;
        _vesselTypeRepo = vesselTypeRepo;
    }

    public async Task<DockDto> RegisterDockAsync(DockDto dto)
    {
        var vesselTypes = new List<VesselType>();
        foreach (var id in dto.VesselTypeIds)
        {
            var vt = await _vesselTypeRepo.GetByIdAsync(new VesselTypeId(id));
            if (vt == null) throw new BusinessRuleValidationException($"Vessel type {id} not found.");
            vesselTypes.Add(vt);
        }

        var location = new Location(dto.Coordinates, dto.LocationDescription);
        var dock = new Dock(dto.Name, dto.Length, dto.Depth, dto.MaxDraft, location, vesselTypes);
        await _dockRepo.AddAsync(dock);

        return new DockDto
        {
            Id = dock.Id.AsGuid(),
            Name = dock.Name,
            Length = dock.Length,
            Depth = dock.Depth,
            MaxDraft = dock.MaxDraft,
            Coordinates = dock.Location.Coordinates,
            LocationDescription = dock.Location.Description,
            AllowedVesselTypes = dock.AllowedVesselTypes.Select(v => v.Name).ToList()
        };
    }

    public async Task<DockDto> UpdateDockAsync(Guid dockId, DockDto dto)
    {
        var dock = await _dockRepo.GetByIdAsync(new DockId(dockId));
        if (dock == null) throw new BusinessRuleValidationException("Dock not found.");

        var vesselTypes = new List<VesselType>();
        foreach (var id in dto.VesselTypeIds)
        {
            var vt = await _vesselTypeRepo.GetByIdAsync(new VesselTypeId(id));
            if (vt == null) throw new BusinessRuleValidationException($"Vessel type {id} not found.");
            vesselTypes.Add(vt);
        }

        var location = new Location(dto.Coordinates, dto.LocationDescription);
        dock.Update(dto.Name, dto.Length, dto.Depth, dto.MaxDraft, location, vesselTypes);
        await _dockRepo.UpdateAsync(dock);

        return new DockDto
        {
            Id = dock.Id.AsGuid(),
            Name = dock.Name,
            Length = dock.Length,
            Depth = dock.Depth,
            MaxDraft = dock.MaxDraft,
            Coordinates = dock.Location.Coordinates,
            LocationDescription = dock.Location.Description,
            AllowedVesselTypes = dock.AllowedVesselTypes.Select(v => v.Name).ToList()
        };
    }
}
