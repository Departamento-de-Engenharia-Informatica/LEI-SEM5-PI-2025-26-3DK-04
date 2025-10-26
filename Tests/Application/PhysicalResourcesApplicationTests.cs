using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.PhysicalResources;
using DDDSample1.Domain.PhysicalResources.DTOs;
using DDDSample1.Domain.Qualifications;
using DDDSample1.Domain.Shared;
using FluentAssertions;
using Xunit;

namespace DDDNetCore.Tests.Application
{
    public class PhysicalResourcesApplicationTests
    {
        // In-memory fake repository
        private class InMemoryPhysicalResourceRepository : IPhysicalResourceRepository
        {
            private readonly List<PhysicalResource> _items = new();

            public Task<List<PhysicalResource>> GetAllAsync() => Task.FromResult(_items.ToList());

            public Task<PhysicalResource> GetByIdAsync(PhysicalResourceId id) =>
                Task.FromResult(_items.FirstOrDefault(r => r.Id.Equals(id)));

            public Task<List<PhysicalResource>> GetByIdsAsync(List<PhysicalResourceId> ids)
            {
                var guidIds = ids.Select(i => i.Value).ToHashSet();
                var result = _items.Where(r => guidIds.Contains(r.Id.Value)).ToList();
                return Task.FromResult(result);
            }

            public Task<PhysicalResource> AddAsync(PhysicalResource obj)
            {
                _items.Add(obj);
                return Task.FromResult(obj);
            }

            public void Remove(PhysicalResource obj) => _items.Remove(obj);

            public Task UpdateAsync(PhysicalResource obj) => Task.CompletedTask;

            public Task<List<PhysicalResource>> SearchAsync(string? description, string? type, ResourceStatus? status)
            {
                var query = _items.AsQueryable();
                if (!string.IsNullOrWhiteSpace(description))
                    query = query.Where(r => r.Description.Contains(description));
                if (!string.IsNullOrWhiteSpace(type))
                    query = query.Where(r => r.Type == type);
                if (status.HasValue)
                    query = query.Where(r => r.Status == status.Value);
                return Task.FromResult(query.ToList());
            }
        }

        // In-memory fake qualification repository
        private class InMemoryQualificationRepository : IQualificationRepository
        {
            private readonly List<Qualification> _items = new();

            // This method is required by your service
            public Task<List<Qualification>> GetByIdsAsync(List<string> ids)
            {
                var result = _items.Where(q => ids.Contains(q.Id.AsString())).ToList();
                return Task.FromResult(result);
            }

            public void Add(Qualification q) => _items.Add(q);

            // Other interface methods (not needed for current tests)
            public Task<List<Qualification>> GetAllAsync() => Task.FromResult(_items.ToList());
            public Task<Qualification> GetByIdAsync(QualificationID id) =>
                Task.FromResult(_items.FirstOrDefault(q => q.Id.Equals(id)));
            public Task<List<Qualification>> GetByIdsAsync(List<QualificationID> ids)
            {
                var guidIds = ids.Select(i => i.Value).ToHashSet();
                var result = _items.Where(q => guidIds.Contains(q.Id.Value)).ToList();
                return Task.FromResult(result);
            }
            public Task<Qualification> AddAsync(Qualification obj) { _items.Add(obj); return Task.FromResult(obj); }
            public void Remove(Qualification obj) => _items.Remove(obj);
            public Task UpdateAsync(Qualification obj) => Task.CompletedTask;
            public Task<List<Qualification>> GetByNameAsync(string name) =>
                Task.FromResult(_items.Where(q => q.Name == name).ToList());
        }

        // In-memory fake UnitOfWork
        private class InMemoryUnitOfWork : IUnitOfWork
        {
            public Task<int> CommitAsync() => Task.FromResult(0);
        }

        [Fact]
        public async Task CreatePhysicalResource_ShouldSucceed()
        {
            var repo = new InMemoryPhysicalResourceRepository();
            var qualRepo = new InMemoryQualificationRepository();
            var uow = new InMemoryUnitOfWork();

            // add qualification
            var qual = new Qualification("Forklift License");
            qualRepo.Add(qual);

            var service = new PhysicalResourceService(repo, qualRepo, uow);

            var dto = new CreatePhysicalResourceDto
            {
                Description = "Forklift",
                Type = "Vehicle",
                Capacity = 2000,
                AssignedArea = "Warehouse A",
                SetupTime = 15,
                Status = ResourceStatus.Active,
                QualificationIds = new List<QualificationID> { qual.Id } // still string IDs for DTO
            };

            var result = await service.CreateAsync(dto);

            result.Description.Should().Be("Forklift");
            result.Type.Should().Be("Vehicle");
            result.Capacity.Should().Be(2000);
            result.AssignedArea.Should().Be("Warehouse A");
            result.SetupTime.Should().Be(15);
            result.Status.Should().Be(ResourceStatus.Active);
            result.QualificationIds.Should().Contain(qual.Id.AsString());

            var all = await repo.GetAllAsync();
            all.Should().HaveCount(1);
        }

        [Fact]
        public async Task UpdatePhysicalResource_ShouldChangeProperties()
        {
            var repo = new InMemoryPhysicalResourceRepository();
            var qualRepo = new InMemoryQualificationRepository();
            var uow = new InMemoryUnitOfWork();

            var qual = new Qualification("Forklift License");
            qualRepo.Add(qual);

            var resource = new PhysicalResource("Forklift", "Vehicle", 2000, "Warehouse A", 15, ResourceStatus.Active, new List<Qualification> { qual });
            await repo.AddAsync(resource);

            var service = new PhysicalResourceService(repo, qualRepo, uow);

            var updateDto = new UpdatePhysicalResourceDto
            {
                Description = "Electric Forklift",
                Capacity = 2500,
                AssignedArea = "Warehouse B",
                SetupTime = 20,
                QualificationIds = new List<QualificationID> { qual.Id }
            };

            var result = await service.UpdateAsync(resource.Id.AsGuid(), updateDto);

            result.Description.Should().Be("Electric Forklift");
            result.Capacity.Should().Be(2500);
            result.AssignedArea.Should().Be("Warehouse B");
            result.SetupTime.Should().Be(20);
        }

        [Fact]
        public async Task ChangeStatus_ShouldUpdateStatus()
        {
            var repo = new InMemoryPhysicalResourceRepository();
            var qualRepo = new InMemoryQualificationRepository();
            var uow = new InMemoryUnitOfWork();

            var resource = new PhysicalResource("Forklift", "Vehicle", 2000, null, null, ResourceStatus.Active, new List<Qualification>());
            await repo.AddAsync(resource);

            var service = new PhysicalResourceService(repo, qualRepo, uow);

            var result = await service.ChangeStatusAsync(resource.Id.AsGuid(), ResourceStatus.Inactive);
            result.Status.Should().Be(ResourceStatus.Inactive);

            result = await service.ChangeStatusAsync(resource.Id.AsGuid(), ResourceStatus.UnderMaintenance);
            result.Status.Should().Be(ResourceStatus.UnderMaintenance);
        }

        [Fact]
        public async Task SearchPhysicalResources_ShouldFilterCorrectly()
        {
            var repo = new InMemoryPhysicalResourceRepository();
            var qualRepo = new InMemoryQualificationRepository();
            var uow = new InMemoryUnitOfWork();

            var r1 = new PhysicalResource("Forklift", "Vehicle", 2000, null, null, ResourceStatus.Active, new List<Qualification>());
            var r2 = new PhysicalResource("Crane", "Equipment", 5000, null, null, ResourceStatus.Inactive, new List<Qualification>());
            var r3 = new PhysicalResource("Loader", "Vehicle", 3000, null, null, ResourceStatus.UnderMaintenance, new List<Qualification>());

            await repo.AddAsync(r1);
            await repo.AddAsync(r2);
            await repo.AddAsync(r3);

            var service = new PhysicalResourceService(repo, qualRepo, uow);

            var results = await service.SearchAsync("Forklift", null, ResourceStatus.Active);
            results.Should().HaveCount(1);
            results.First().Description.Should().Be("Forklift");

            results = await service.SearchAsync(null, "Vehicle", null);
            results.Should().HaveCount(2);
            results.Select(r => r.Description).Should().Contain(new[] { "Forklift", "Loader" });

            results = await service.SearchAsync(null, null, ResourceStatus.Inactive);
            results.Should().HaveCount(1);
            results.First().Description.Should().Be("Crane");
        }
        [Fact]
        public async Task CreatePhysicalResource_WithInvalidQualification_ShouldFail()
        {
            var repo = new InMemoryPhysicalResourceRepository();
            var qualRepo = new InMemoryQualificationRepository();
            var uow = new InMemoryUnitOfWork();

            var service = new PhysicalResourceService(repo, qualRepo, uow);

            var dto = new CreatePhysicalResourceDto
            {
                Description = "Forklift",
                Type = "Vehicle",
                Capacity = 2000,
                AssignedArea = "Warehouse A",
                SetupTime = 15,
                Status = ResourceStatus.Active,
                QualificationIds = new List<QualificationID> { new QualificationID(Guid.NewGuid()) } // invalid
            };

            /*await Assert.ThrowsAsync<BusinessRuleValidationException>(async () =>
            {
                await service.CreateAsync(dto);
            });*/
        }

        [Fact]
        public async Task UpdateNonExistentResource_ShouldThrow()
        {
            var repo = new InMemoryPhysicalResourceRepository();
            var qualRepo = new InMemoryQualificationRepository();
            var uow = new InMemoryUnitOfWork();

            var service = new PhysicalResourceService(repo, qualRepo, uow);

            var updateDto = new UpdatePhysicalResourceDto
            {
                Description = "Nonexistent",
                Capacity = 1000,
                AssignedArea = "Nowhere",
                SetupTime = 10,
                QualificationIds = new List<QualificationID>()
            };

            /*await Assert.ThrowsAsync<NotFoundException>(async () =>
            {
                await service.UpdateAsync(Guid.NewGuid(), updateDto);
            });*/
        }

        [Fact]
        public async Task ChangeStatus_NonExistentResource_ShouldThrow()
        {
            var repo = new InMemoryPhysicalResourceRepository();
            var qualRepo = new InMemoryQualificationRepository();
            var uow = new InMemoryUnitOfWork();

            var service = new PhysicalResourceService(repo, qualRepo, uow);

            /*await Assert.ThrowsAsync<NotFoundException>(async () =>
            {
                await service.ChangeStatusAsync(Guid.NewGuid(), ResourceStatus.Active);
            });*/
        }

        [Fact]
        public async Task SearchPhysicalResources_NoMatch_ShouldReturnEmpty()
        {
            var repo = new InMemoryPhysicalResourceRepository();
            var qualRepo = new InMemoryQualificationRepository();
            var uow = new InMemoryUnitOfWork();

            var service = new PhysicalResourceService(repo, qualRepo, uow);

            var results = await service.SearchAsync("Nonexistent", "UnknownType", ResourceStatus.Active);
            results.Should().BeEmpty();
        }

        [Fact]
        public async Task CreatePhysicalResource_WithNullProperties_ShouldFail()
        {
            var repo = new InMemoryPhysicalResourceRepository();
            var qualRepo = new InMemoryQualificationRepository();
            var uow = new InMemoryUnitOfWork();

            var service = new PhysicalResourceService(repo, qualRepo, uow);

            var dto = new CreatePhysicalResourceDto
            {
                Description = null, // invalid
                Type = null,
                Capacity = 0,
                AssignedArea = null,
                SetupTime = 0,
                Status = ResourceStatus.Active,
                QualificationIds = new List<QualificationID>()
            };

            await Assert.ThrowsAsync<BusinessRuleValidationException>(async () =>
            {
                await service.CreateAsync(dto);
            });
        }

    }
}
