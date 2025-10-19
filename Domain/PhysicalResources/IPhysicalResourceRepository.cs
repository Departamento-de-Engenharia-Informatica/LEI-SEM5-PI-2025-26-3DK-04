using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.PhysicalResources;

public interface IPhysicalResourceRepository : IRepository<PhysicalResource, PhysicalResourceId>
{
    Task<List<PhysicalResource>> SearchAsync(string? description, string? type, ResourceStatus? status);
}
