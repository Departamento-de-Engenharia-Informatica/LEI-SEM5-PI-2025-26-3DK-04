using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.PhysicalResources;
using Microsoft.EntityFrameworkCore;

namespace DDDSample1.Infrastructure.PhysicalResources;

public class PhysicalResourcesRepository : IPhysicalResourceRepository
{
    private readonly DDDSample1DbContext _context;

    public PhysicalResourcesRepository(DDDSample1DbContext context)
    {
        _context = context;
    }

    public async Task<List<PhysicalResource>> GetAllAsync()
    {
        return await _context.PhysicalResources.ToListAsync();
    }

    public async Task<PhysicalResource> GetByIdAsync(PhysicalResourceId id)
    {
        return await _context.PhysicalResources.FindAsync(id);
    }

    public async Task<List<PhysicalResource>> GetByIdsAsync(List<PhysicalResourceId> ids)
    {
        var guidList = ids.Select(i => i.Value).ToList();
        return await _context.PhysicalResources
            .Where(r => guidList.Contains(r.Id.Value))
            .ToListAsync();
    }

    public async Task<PhysicalResource> AddAsync(PhysicalResource obj)
    {
        var entry = await _context.PhysicalResources.AddAsync(obj);
        return entry.Entity;
    }

    public void Remove(PhysicalResource obj)
    {
        _context.PhysicalResources.Remove(obj);
    }

    public async Task UpdateAsync(PhysicalResource obj)
    {
        _context.PhysicalResources.Update(obj);
        await Task.CompletedTask;
    }

    public async Task<List<PhysicalResource>> SearchAsync(string? description, string? type, ResourceStatus? status)
    {
        var query = _context.PhysicalResources.AsQueryable();

        if (!string.IsNullOrWhiteSpace(description))
            query = query.Where(r => r.Description.Contains(description));

        if (!string.IsNullOrWhiteSpace(type))
            query = query.Where(r => r.Type == type);

        if (status.HasValue)
            query = query.Where(r => r.Status == status.Value);

        return await query.ToListAsync();
    }
}

