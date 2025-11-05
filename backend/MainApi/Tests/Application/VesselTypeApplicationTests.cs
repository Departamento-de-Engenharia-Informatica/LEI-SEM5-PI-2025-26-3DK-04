using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using FluentAssertions;
using Xunit;
using DDDSample1.Domain.Vessels;
using DDDSample1.Domain.Shared;

namespace DDDNetCore.Tests.Application
{
    public class VesselTypeApplicationTests
    {
        // Fake in-memory repository for unit tests
        private class InMemoryVesselTypeRepository : IVesselTypeRepository
        {
            private readonly List<VesselType> _items = new List<VesselType>();

            public Task<VesselType> AddAsync(VesselType obj)
            {
                _items.Add(obj);
                return Task.FromResult(obj);
            }

            public void Remove(VesselType obj)
            {
                _items.Remove(obj);
            }

            public Task UpdateAsync(VesselType obj)
            {
                // no-op for in-memory
                return Task.CompletedTask;
            }

            public Task<List<VesselType>> GetAllAsync()
            {
                return Task.FromResult(_items.ToList());
            }

            public Task<VesselType> GetByIdAsync(VesselTypeId id)
            {
                var found = _items.FirstOrDefault(x => x.Id.AsGuid() == id.AsGuid());
                return Task.FromResult(found);
            }

            public Task<List<VesselType>> GetByIdsAsync(List<VesselTypeId> ids)
            {
                var guids = ids.Select(i => i.AsGuid()).ToHashSet();
                var found = _items.Where(x => guids.Contains(x.Id.AsGuid())).ToList();
                return Task.FromResult(found);
            }

            public Task<List<VesselType>> SearchByNameAsync(string name)
            {
                if (string.IsNullOrWhiteSpace(name))
                    return Task.FromResult(new List<VesselType>());

                var lower = name.ToLowerInvariant();
                var found = _items.Where(x => x.Name != null && x.Name.ToLowerInvariant().Contains(lower)).ToList();
                return Task.FromResult(found);
            }

            public Task<List<VesselType>> SearchByDescriptionAsync(string description)
            {
                if (string.IsNullOrWhiteSpace(description))
                    return Task.FromResult(new List<VesselType>());

                var lower = description.ToLowerInvariant();
                var found = _items.Where(x => x.Description != null && x.Description.ToLowerInvariant().Contains(lower)).ToList();
                return Task.FromResult(found);
            }

            public Task<List<VesselType>> SearchByNameOrDescriptionAsync(string searchTerm)
            {
                if (string.IsNullOrWhiteSpace(searchTerm))
                    return Task.FromResult(_items.ToList());

                var lower = searchTerm.ToLowerInvariant();
                var found = _items.Where(x =>
                    (x.Name != null && x.Name.ToLowerInvariant().Contains(lower)) ||
                    (x.Description != null && x.Description.ToLowerInvariant().Contains(lower))
                ).ToList();
                return Task.FromResult(found);
            }
        }

        // Fake unit of work that counts commits
        private class InMemoryUnitOfWork : IUnitOfWork
        {
            public int CommitCallCount { get; private set; } = 0;

            public Task<int> CommitAsync()
            {
                CommitCallCount++;
                return Task.FromResult(1);
            }
        }

        [Fact]
        public async Task CreateVesselType_CreatesAndReturnsDto()
        {
            var repo = new InMemoryVesselTypeRepository();
            var uow = new InMemoryUnitOfWork();
            var service = new VesselTypeService(uow, repo);

            var dto = new CreatingVesselTypeDto
            {
                Name = "Container Ship",
                Description = "Large vessel for transporting containers",
                Capacity = 5000,
                MaxRows = 10,
                MaxBays = 20,
                MaxTiers = 8
            };

            var result = await service.AddAsync(dto);

            result.Should().NotBeNull();
            result.Name.Should().Be("Container Ship");
            result.Description.Should().Be("Large vessel for transporting containers");
            result.Capacity.Should().Be(5000);
            result.MaxRows.Should().Be(10);
            result.MaxBays.Should().Be(20);
            result.MaxTiers.Should().Be(8);
            result.Active.Should().BeTrue();

            // repository should contain one item
            var all = await repo.GetAllAsync();
            all.Should().ContainSingle().Which.Name.Should().Be("Container Ship");
            uow.CommitCallCount.Should().Be(1);
        }

        [Fact]
        public async Task CreateVesselType_EmptyName_ThrowsBusinessRule()
        {
            var repo = new InMemoryVesselTypeRepository();
            var uow = new InMemoryUnitOfWork();
            var service = new VesselTypeService(uow, repo);

            var dto = new CreatingVesselTypeDto
            {
                Name = "",
                Description = "Test description",
                Capacity = 1000,
                MaxRows = 5,
                MaxBays = 10,
                MaxTiers = 5
            };

            Func<Task> act = async () => await service.AddAsync(dto);

            await Assert.ThrowsAsync<BusinessRuleValidationException>(act);
            uow.CommitCallCount.Should().Be(0);
        }

        [Fact]
        public async Task CreateVesselType_EmptyDescription_ThrowsBusinessRule()
        {
            var repo = new InMemoryVesselTypeRepository();
            var uow = new InMemoryUnitOfWork();
            var service = new VesselTypeService(uow, repo);

            var dto = new CreatingVesselTypeDto
            {
                Name = "Test Vessel",
                Description = "",
                Capacity = 1000,
                MaxRows = 5,
                MaxBays = 10,
                MaxTiers = 5
            };

            Func<Task> act = async () => await service.AddAsync(dto);

            await Assert.ThrowsAsync<BusinessRuleValidationException>(act);
            uow.CommitCallCount.Should().Be(0);
        }

        [Fact]
        public async Task CreateVesselType_InvalidCapacity_ThrowsBusinessRule()
        {
            var repo = new InMemoryVesselTypeRepository();
            var uow = new InMemoryUnitOfWork();
            var service = new VesselTypeService(uow, repo);

            var dto = new CreatingVesselTypeDto
            {
                Name = "Test Vessel",
                Description = "Test description",
                Capacity = 0,
                MaxRows = 5,
                MaxBays = 10,
                MaxTiers = 5
            };

            Func<Task> act = async () => await service.AddAsync(dto);

            await Assert.ThrowsAsync<BusinessRuleValidationException>(act);
            uow.CommitCallCount.Should().Be(0);
        }

        [Fact]
        public async Task CreateVesselType_InvalidMaxRows_ThrowsBusinessRule()
        {
            var repo = new InMemoryVesselTypeRepository();
            var uow = new InMemoryUnitOfWork();
            var service = new VesselTypeService(uow, repo);

            var dto = new CreatingVesselTypeDto
            {
                Name = "Test Vessel",
                Description = "Test description",
                Capacity = 1000,
                MaxRows = 0,
                MaxBays = 10,
                MaxTiers = 5
            };

            Func<Task> act = async () => await service.AddAsync(dto);

            await Assert.ThrowsAsync<BusinessRuleValidationException>(act);
            uow.CommitCallCount.Should().Be(0);
        }

        [Fact]
        public async Task UpdateVesselType_ChangesAllFields()
        {
            var repo = new InMemoryVesselTypeRepository();
            var uow = new InMemoryUnitOfWork();
            var service = new VesselTypeService(uow, repo);

            var entity = new VesselType("Old Name", "Old Description", 1000, 5, 10, 5);
            await repo.AddAsync(entity);
            var id = entity.Id.AsGuid();

            var updateDto = new UpdatingVesselTypeDto
            {
                Name = "New Name",
                Description = "New Description",
                Capacity = 2000,
                MaxRows = 8,
                MaxBays = 15,
                MaxTiers = 7
            };
            var result = await service.UpdateAsync(id, updateDto);

            result.Should().NotBeNull();
            result.Id.Should().Be(id);
            result.Name.Should().Be("New Name");
            result.Description.Should().Be("New Description");
            result.Capacity.Should().Be(2000);
            result.MaxRows.Should().Be(8);
            result.MaxBays.Should().Be(15);
            result.MaxTiers.Should().Be(7);
            uow.CommitCallCount.Should().Be(1);

            var stored = await repo.GetByIdAsync(new VesselTypeId(id));
            stored.Should().NotBeNull();
            stored.Name.Should().Be("New Name");
            stored.Description.Should().Be("New Description");
        }

        [Fact]
        public async Task UpdateVesselType_NonExistentId_ReturnsNull()
        {
            var repo = new InMemoryVesselTypeRepository();
            var uow = new InMemoryUnitOfWork();
            var service = new VesselTypeService(uow, repo);

            var updateDto = new UpdatingVesselTypeDto
            {
                Name = "Test",
                Description = "Test",
                Capacity = 1000,
                MaxRows = 5,
                MaxBays = 10,
                MaxTiers = 5
            };

            var result = await service.UpdateAsync(Guid.NewGuid(), updateDto);

            result.Should().BeNull();
            uow.CommitCallCount.Should().Be(0);
        }

        [Fact]
        public async Task SearchByName_ReturnsMatches()
        {
            var repo = new InMemoryVesselTypeRepository();
            var uow = new InMemoryUnitOfWork();
            var service = new VesselTypeService(uow, repo);

            // seed
            await repo.AddAsync(new VesselType("Container Ship", "For containers", 5000, 10, 20, 8));
            await repo.AddAsync(new VesselType("Tanker Ship", "For liquid cargo", 3000, 8, 15, 6));
            await repo.AddAsync(new VesselType("Bulk Carrier", "For dry bulk", 4000, 9, 18, 7));

            var results = await service.SearchByNameAsync("container");

            results.Should().HaveCount(1);
            results.First().Name.Should().Contain("Container");
        }

        [Fact]
        public async Task SearchByDescription_ReturnsMatches()
        {
            var repo = new InMemoryVesselTypeRepository();
            var uow = new InMemoryUnitOfWork();
            var service = new VesselTypeService(uow, repo);

            // seed
            await repo.AddAsync(new VesselType("Container Ship", "For containers", 5000, 10, 20, 8));
            await repo.AddAsync(new VesselType("Tanker Ship", "For liquid cargo", 3000, 8, 15, 6));
            await repo.AddAsync(new VesselType("Bulk Carrier", "For dry bulk", 4000, 9, 18, 7));

            var results = await service.SearchByDescriptionAsync("liquid");

            results.Should().HaveCount(1);
            results.First().Description.Should().Contain("liquid");
        }

        [Fact]
        public async Task SearchByNameOrDescription_ReturnsMatches()
        {
            var repo = new InMemoryVesselTypeRepository();
            var uow = new InMemoryUnitOfWork();
            var service = new VesselTypeService(uow, repo);

            // seed
            await repo.AddAsync(new VesselType("Container Ship", "For containers", 5000, 10, 20, 8));
            await repo.AddAsync(new VesselType("Tanker Ship", "For liquid cargo", 3000, 8, 15, 6));
            await repo.AddAsync(new VesselType("Bulk Carrier", "For dry bulk", 4000, 9, 18, 7));

            var results = await service.SearchAsync("ship");

            results.Should().HaveCount(2);
        }

        [Fact]
        public async Task GetAllVesselTypes_ReturnsAll()
        {
            var repo = new InMemoryVesselTypeRepository();
            var uow = new InMemoryUnitOfWork();
            var service = new VesselTypeService(uow, repo);

            // seed
            await repo.AddAsync(new VesselType("Container Ship", "For containers", 5000, 10, 20, 8));
            await repo.AddAsync(new VesselType("Tanker Ship", "For liquid cargo", 3000, 8, 15, 6));

            var results = await service.GetAllAsync();

            results.Should().HaveCount(2);
        }

        [Fact]
        public async Task InactivateVesselType_MarksAsInactive()
        {
            var repo = new InMemoryVesselTypeRepository();
            var uow = new InMemoryUnitOfWork();
            var service = new VesselTypeService(uow, repo);

            var entity = new VesselType("Test Type", "Test Description", 1000, 5, 10, 5);
            await repo.AddAsync(entity);
            var id = entity.Id;

            var result = await service.InactivateAsync(id);

            result.Should().NotBeNull();
            result.Active.Should().BeFalse();
            uow.CommitCallCount.Should().Be(1);

            var stored = await repo.GetByIdAsync(id);
            stored.Should().NotBeNull();
            stored.Active.Should().BeFalse();
        }

        [Fact]
        public async Task UpdateVesselType_InvalidName_ThrowsBusinessRule()
        {
            var repo = new InMemoryVesselTypeRepository();
            var uow = new InMemoryUnitOfWork();
            var service = new VesselTypeService(uow, repo);

            var entity = new VesselType("Original Name", "Original Description", 1000, 5, 10, 5);
            await repo.AddAsync(entity);
            var id = entity.Id.AsGuid();

            var updateDto = new UpdatingVesselTypeDto
            {
                Name = "",
                Description = "Updated Description",
                Capacity = 2000,
                MaxRows = 6,
                MaxBays = 12,
                MaxTiers = 6
            };

            Func<Task> act = async () => await service.UpdateAsync(id, updateDto);

            await Assert.ThrowsAsync<BusinessRuleValidationException>(act);
            uow.CommitCallCount.Should().Be(0);
        }
    }
}
