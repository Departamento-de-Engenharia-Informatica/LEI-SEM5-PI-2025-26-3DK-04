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
    public class VesselApplicationTests
    {
        // Fake in-memory repository for VesselType
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

        // Fake in-memory repository for Vessel
        private class InMemoryVesselRepository : IVesselRepository
        {
            private readonly List<Vessel> _items = new List<Vessel>();

            public Task<Vessel> AddAsync(Vessel obj)
            {
                _items.Add(obj);
                return Task.FromResult(obj);
            }

            public void Remove(Vessel obj)
            {
                _items.Remove(obj);
            }

            public Task UpdateAsync(Vessel obj)
            {
                // no-op for in-memory
                return Task.CompletedTask;
            }

            public Task<List<Vessel>> GetAllAsync()
            {
                return Task.FromResult(_items.ToList());
            }

            public Task<Vessel> GetByIdAsync(VesselId id)
            {
                var found = _items.FirstOrDefault(x => x.Id.AsGuid() == id.AsGuid());
                return Task.FromResult(found);
            }

            public Task<List<Vessel>> GetByIdsAsync(List<VesselId> ids)
            {
                var guids = ids.Select(i => i.AsGuid()).ToHashSet();
                var found = _items.Where(x => guids.Contains(x.Id.AsGuid())).ToList();
                return Task.FromResult(found);
            }

            public Task<Vessel> GetByImoNumberAsync(string imoNumber)
            {
                var found = _items.FirstOrDefault(x => x.ImoNumber == imoNumber);
                return Task.FromResult(found);
            }

            public Task<List<Vessel>> SearchByNameAsync(string name)
            {
                if (string.IsNullOrWhiteSpace(name))
                    return Task.FromResult(new List<Vessel>());

                var lower = name.ToLowerInvariant();
                var found = _items.Where(x => x.Name != null && x.Name.ToLowerInvariant().Contains(lower)).ToList();
                return Task.FromResult(found);
            }

            public Task<List<Vessel>> SearchByOwnerAsync(string owner)
            {
                if (string.IsNullOrWhiteSpace(owner))
                    return Task.FromResult(new List<Vessel>());

                var lower = owner.ToLowerInvariant();
                var found = _items.Where(x => x.Owner != null && x.Owner.ToLowerInvariant().Contains(lower)).ToList();
                return Task.FromResult(found);
            }

            public Task<List<Vessel>> SearchByOperatorAsync(string operatorName)
            {
                if (string.IsNullOrWhiteSpace(operatorName))
                    return Task.FromResult(new List<Vessel>());

                var lower = operatorName.ToLowerInvariant();
                var found = _items.Where(x => x.Operator != null && x.Operator.ToLowerInvariant().Contains(lower)).ToList();
                return Task.FromResult(found);
            }

            public Task<List<Vessel>> SearchAsync(string searchTerm)
            {
                if (string.IsNullOrWhiteSpace(searchTerm))
                    return Task.FromResult(_items.ToList());

                var lower = searchTerm.ToLowerInvariant();
                var found = _items.Where(x =>
                    (x.Name != null && x.Name.ToLowerInvariant().Contains(lower)) ||
                    (x.Owner != null && x.Owner.ToLowerInvariant().Contains(lower)) ||
                    (x.Operator != null && x.Operator.ToLowerInvariant().Contains(lower)) ||
                    (x.ImoNumber != null && x.ImoNumber.ToLowerInvariant().Contains(lower))
                ).ToList();
                return Task.FromResult(found);
            }

            public Task<bool> ExistsByImoNumberAsync(string imoNumber)
            {
                var exists = _items.Any(x => x.ImoNumber == imoNumber);
                return Task.FromResult(exists);
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
        public async Task CreateVessel_CreatesAndReturnsDto()
        {
            var vesselRepo = new InMemoryVesselRepository();
            var vesselTypeRepo = new InMemoryVesselTypeRepository();
            var uow = new InMemoryUnitOfWork();
            var service = new VesselService(uow, vesselRepo, vesselTypeRepo);

            // Create a vessel type first
            var vesselType = new VesselType("Container Ship", "For containers", 5000, 10, 20, 8);
            await vesselTypeRepo.AddAsync(vesselType);

            var dto = new CreatingVesselDto
            {
                ImoNumber = "IMO1234567",
                Name = "MV Atlantic",
                VesselTypeId = vesselType.Id.AsGuid(),
                Owner = "Atlantic Shipping",
                Operator = "Ocean Lines"
            };

            var result = await service.AddAsync(dto);

            result.Should().NotBeNull();
            result.ImoNumber.Should().Be("IMO1234567");
            result.Name.Should().Be("MV Atlantic");
            result.VesselTypeId.Should().Be(vesselType.Id.AsGuid());
            result.Owner.Should().Be("Atlantic Shipping");
            result.Operator.Should().Be("Ocean Lines");
            result.Active.Should().BeTrue();

            // repository should contain one item
            var all = await vesselRepo.GetAllAsync();
            all.Should().ContainSingle().Which.Name.Should().Be("MV Atlantic");
            uow.CommitCallCount.Should().Be(1);
        }

        [Fact]
        public async Task CreateVessel_EmptyImoNumber_ThrowsBusinessRule()
        {
            var vesselRepo = new InMemoryVesselRepository();
            var vesselTypeRepo = new InMemoryVesselTypeRepository();
            var uow = new InMemoryUnitOfWork();
            var service = new VesselService(uow, vesselRepo, vesselTypeRepo);

            var vesselType = new VesselType("Container Ship", "For containers", 5000, 10, 20, 8);
            await vesselTypeRepo.AddAsync(vesselType);

            var dto = new CreatingVesselDto
            {
                ImoNumber = "",
                Name = "Test Vessel",
                VesselTypeId = vesselType.Id.AsGuid(),
                Owner = "Test Owner",
                Operator = "Test Operator"
            };

            Func<Task> act = async () => await service.AddAsync(dto);

            await Assert.ThrowsAsync<BusinessRuleValidationException>(act);
            uow.CommitCallCount.Should().Be(0);
        }

        [Fact]
        public async Task CreateVessel_InvalidImoFormat_ThrowsBusinessRule()
        {
            var vesselRepo = new InMemoryVesselRepository();
            var vesselTypeRepo = new InMemoryVesselTypeRepository();
            var uow = new InMemoryUnitOfWork();
            var service = new VesselService(uow, vesselRepo, vesselTypeRepo);

            var vesselType = new VesselType("Container Ship", "For containers", 5000, 10, 20, 8);
            await vesselTypeRepo.AddAsync(vesselType);

            var dto = new CreatingVesselDto
            {
                ImoNumber = "IMO123",  // Invalid: too short
                Name = "Test Vessel",
                VesselTypeId = vesselType.Id.AsGuid(),
                Owner = "Test Owner",
                Operator = "Test Operator"
            };

            Func<Task> act = async () => await service.AddAsync(dto);

            await Assert.ThrowsAsync<BusinessRuleValidationException>(act);
            uow.CommitCallCount.Should().Be(0);
        }

        [Fact]
        public async Task CreateVessel_DuplicateImoNumber_ThrowsBusinessRule()
        {
            var vesselRepo = new InMemoryVesselRepository();
            var vesselTypeRepo = new InMemoryVesselTypeRepository();
            var uow = new InMemoryUnitOfWork();
            var service = new VesselService(uow, vesselRepo, vesselTypeRepo);

            var vesselType = new VesselType("Container Ship", "For containers", 5000, 10, 20, 8);
            await vesselTypeRepo.AddAsync(vesselType);

            // Create first vessel
            var existingVessel = new Vessel("IMO1234567", "First Vessel", vesselType.Id, "Owner1", "Operator1");
            await vesselRepo.AddAsync(existingVessel);

            // Try to create second vessel with same IMO
            var dto = new CreatingVesselDto
            {
                ImoNumber = "IMO1234567",
                Name = "Second Vessel",
                VesselTypeId = vesselType.Id.AsGuid(),
                Owner = "Owner2",
                Operator = "Operator2"
            };

            Func<Task> act = async () => await service.AddAsync(dto);

            await Assert.ThrowsAsync<BusinessRuleValidationException>(act);
            uow.CommitCallCount.Should().Be(0);
        }

        [Fact]
        public async Task CreateVessel_NonExistentVesselType_ThrowsBusinessRule()
        {
            var vesselRepo = new InMemoryVesselRepository();
            var vesselTypeRepo = new InMemoryVesselTypeRepository();
            var uow = new InMemoryUnitOfWork();
            var service = new VesselService(uow, vesselRepo, vesselTypeRepo);

            var dto = new CreatingVesselDto
            {
                ImoNumber = "IMO1234567",
                Name = "Test Vessel",
                VesselTypeId = Guid.NewGuid(),  // Non-existent vessel type
                Owner = "Test Owner",
                Operator = "Test Operator"
            };

            Func<Task> act = async () => await service.AddAsync(dto);

            await Assert.ThrowsAsync<BusinessRuleValidationException>(act);
            uow.CommitCallCount.Should().Be(0);
        }

        [Fact]
        public async Task CreateVessel_EmptyName_ThrowsBusinessRule()
        {
            var vesselRepo = new InMemoryVesselRepository();
            var vesselTypeRepo = new InMemoryVesselTypeRepository();
            var uow = new InMemoryUnitOfWork();
            var service = new VesselService(uow, vesselRepo, vesselTypeRepo);

            var vesselType = new VesselType("Container Ship", "For containers", 5000, 10, 20, 8);
            await vesselTypeRepo.AddAsync(vesselType);

            var dto = new CreatingVesselDto
            {
                ImoNumber = "IMO1234567",
                Name = "",
                VesselTypeId = vesselType.Id.AsGuid(),
                Owner = "Test Owner",
                Operator = "Test Operator"
            };

            Func<Task> act = async () => await service.AddAsync(dto);

            await Assert.ThrowsAsync<BusinessRuleValidationException>(act);
            uow.CommitCallCount.Should().Be(0);
        }

        [Fact]
        public async Task UpdateVessel_ChangesAllFields()
        {
            var vesselRepo = new InMemoryVesselRepository();
            var vesselTypeRepo = new InMemoryVesselTypeRepository();
            var uow = new InMemoryUnitOfWork();
            var service = new VesselService(uow, vesselRepo, vesselTypeRepo);

            // Create vessel types
            var oldType = new VesselType("Old Type", "Old Description", 1000, 5, 10, 5);
            var newType = new VesselType("New Type", "New Description", 2000, 8, 15, 7);
            await vesselTypeRepo.AddAsync(oldType);
            await vesselTypeRepo.AddAsync(newType);

            // Create vessel
            var entity = new Vessel("IMO1234567", "Old Name", oldType.Id, "Old Owner", "Old Operator");
            await vesselRepo.AddAsync(entity);
            var id = entity.Id.AsGuid();

            var updateDto = new UpdatingVesselDto
            {
                Name = "New Name",
                VesselTypeId = newType.Id.AsGuid(),
                Owner = "New Owner",
                Operator = "New Operator"
            };
            var result = await service.UpdateAsync(id, updateDto);

            result.Should().NotBeNull();
            result.Id.Should().Be(id);
            result.Name.Should().Be("New Name");
            result.VesselTypeId.Should().Be(newType.Id.AsGuid());
            result.Owner.Should().Be("New Owner");
            result.Operator.Should().Be("New Operator");
            result.ImoNumber.Should().Be("IMO1234567"); // IMO should not change
            uow.CommitCallCount.Should().Be(1);

            var stored = await vesselRepo.GetByIdAsync(new VesselId(id));
            stored.Should().NotBeNull();
            stored.Name.Should().Be("New Name");
            stored.Owner.Should().Be("New Owner");
        }

        [Fact]
        public async Task UpdateVessel_NonExistentId_ReturnsNull()
        {
            var vesselRepo = new InMemoryVesselRepository();
            var vesselTypeRepo = new InMemoryVesselTypeRepository();
            var uow = new InMemoryUnitOfWork();
            var service = new VesselService(uow, vesselRepo, vesselTypeRepo);

            var vesselType = new VesselType("Test Type", "Test", 1000, 5, 10, 5);
            await vesselTypeRepo.AddAsync(vesselType);

            var updateDto = new UpdatingVesselDto
            {
                Name = "Test",
                VesselTypeId = vesselType.Id.AsGuid(),
                Owner = "Test",
                Operator = "Test"
            };

            var result = await service.UpdateAsync(Guid.NewGuid(), updateDto);

            result.Should().BeNull();
            uow.CommitCallCount.Should().Be(0);
        }

        [Fact]
        public async Task SearchByName_ReturnsMatches()
        {
            var vesselRepo = new InMemoryVesselRepository();
            var vesselTypeRepo = new InMemoryVesselTypeRepository();
            var uow = new InMemoryUnitOfWork();
            var service = new VesselService(uow, vesselRepo, vesselTypeRepo);

            var vesselType = new VesselType("Container Ship", "For containers", 5000, 10, 20, 8);
            await vesselTypeRepo.AddAsync(vesselType);

            // seed
            await vesselRepo.AddAsync(new Vessel("IMO1111111", "Atlantic Explorer", vesselType.Id, "Owner1", "Operator1"));
            await vesselRepo.AddAsync(new Vessel("IMO2222222", "Pacific Navigator", vesselType.Id, "Owner2", "Operator2"));
            await vesselRepo.AddAsync(new Vessel("IMO3333333", "Arctic Voyager", vesselType.Id, "Owner3", "Operator3"));

            var results = await service.SearchByNameAsync("atlantic");

            results.Should().HaveCount(1);
            results.First().Name.Should().Contain("Atlantic");
        }

        [Fact]
        public async Task SearchByOwner_ReturnsMatches()
        {
            var vesselRepo = new InMemoryVesselRepository();
            var vesselTypeRepo = new InMemoryVesselTypeRepository();
            var uow = new InMemoryUnitOfWork();
            var service = new VesselService(uow, vesselRepo, vesselTypeRepo);

            var vesselType = new VesselType("Container Ship", "For containers", 5000, 10, 20, 8);
            await vesselTypeRepo.AddAsync(vesselType);

            // seed
            await vesselRepo.AddAsync(new Vessel("IMO1111111", "Vessel1", vesselType.Id, "Atlantic Shipping", "Operator1"));
            await vesselRepo.AddAsync(new Vessel("IMO2222222", "Vessel2", vesselType.Id, "Pacific Lines", "Operator2"));

            var results = await service.SearchByOwnerAsync("atlantic");

            results.Should().HaveCount(1);
            results.First().Owner.Should().Contain("Atlantic");
        }

        [Fact]
        public async Task SearchByOperator_ReturnsMatches()
        {
            var vesselRepo = new InMemoryVesselRepository();
            var vesselTypeRepo = new InMemoryVesselTypeRepository();
            var uow = new InMemoryUnitOfWork();
            var service = new VesselService(uow, vesselRepo, vesselTypeRepo);

            var vesselType = new VesselType("Container Ship", "For containers", 5000, 10, 20, 8);
            await vesselTypeRepo.AddAsync(vesselType);

            // seed
            await vesselRepo.AddAsync(new Vessel("IMO1111111", "Vessel1", vesselType.Id, "Owner1", "Global Marine"));
            await vesselRepo.AddAsync(new Vessel("IMO2222222", "Vessel2", vesselType.Id, "Owner2", "Local Shipping"));

            var results = await service.SearchByOperatorAsync("global");

            results.Should().HaveCount(1);
            results.First().Operator.Should().Contain("Global");
        }

        [Fact]
        public async Task GetByImoNumber_ReturnsVessel()
        {
            var vesselRepo = new InMemoryVesselRepository();
            var vesselTypeRepo = new InMemoryVesselTypeRepository();
            var uow = new InMemoryUnitOfWork();
            var service = new VesselService(uow, vesselRepo, vesselTypeRepo);

            var vesselType = new VesselType("Container Ship", "For containers", 5000, 10, 20, 8);
            await vesselTypeRepo.AddAsync(vesselType);

            await vesselRepo.AddAsync(new Vessel("IMO1234567", "Test Vessel", vesselType.Id, "Owner", "Operator"));

            var result = await service.GetByImoNumberAsync("IMO1234567");

            result.Should().NotBeNull();
            result.ImoNumber.Should().Be("IMO1234567");
            result.Name.Should().Be("Test Vessel");
        }

        [Fact]
        public async Task GetAllVessels_ReturnsAll()
        {
            var vesselRepo = new InMemoryVesselRepository();
            var vesselTypeRepo = new InMemoryVesselTypeRepository();
            var uow = new InMemoryUnitOfWork();
            var service = new VesselService(uow, vesselRepo, vesselTypeRepo);

            var vesselType = new VesselType("Container Ship", "For containers", 5000, 10, 20, 8);
            await vesselTypeRepo.AddAsync(vesselType);

            // seed
            await vesselRepo.AddAsync(new Vessel("IMO1111111", "Vessel1", vesselType.Id, "Owner1", "Operator1"));
            await vesselRepo.AddAsync(new Vessel("IMO2222222", "Vessel2", vesselType.Id, "Owner2", "Operator2"));

            var results = await service.GetAllAsync();

            results.Should().HaveCount(2);
        }

        [Fact]
        public async Task InactivateVessel_MarksAsInactive()
        {
            var vesselRepo = new InMemoryVesselRepository();
            var vesselTypeRepo = new InMemoryVesselTypeRepository();
            var uow = new InMemoryUnitOfWork();
            var service = new VesselService(uow, vesselRepo, vesselTypeRepo);

            var vesselType = new VesselType("Test Type", "Test", 1000, 5, 10, 5);
            await vesselTypeRepo.AddAsync(vesselType);

            var entity = new Vessel("IMO1234567", "Test Vessel", vesselType.Id, "Owner", "Operator");
            await vesselRepo.AddAsync(entity);
            var id = entity.Id;

            var result = await service.InactivateAsync(id);

            result.Should().NotBeNull();
            result.Active.Should().BeFalse();
            uow.CommitCallCount.Should().Be(1);

            var stored = await vesselRepo.GetByIdAsync(id);
            stored.Should().NotBeNull();
            stored.Active.Should().BeFalse();
        }

        [Fact]
        public async Task UpdateVessel_InvalidName_ThrowsBusinessRule()
        {
            var vesselRepo = new InMemoryVesselRepository();
            var vesselTypeRepo = new InMemoryVesselTypeRepository();
            var uow = new InMemoryUnitOfWork();
            var service = new VesselService(uow, vesselRepo, vesselTypeRepo);

            var vesselType = new VesselType("Test Type", "Test", 1000, 5, 10, 5);
            await vesselTypeRepo.AddAsync(vesselType);

            var entity = new Vessel("IMO1234567", "Original Name", vesselType.Id, "Owner", "Operator");
            await vesselRepo.AddAsync(entity);
            var id = entity.Id.AsGuid();

            var updateDto = new UpdatingVesselDto
            {
                Name = "",
                VesselTypeId = vesselType.Id.AsGuid(),
                Owner = "Updated Owner",
                Operator = "Updated Operator"
            };

            Func<Task> act = async () => await service.UpdateAsync(id, updateDto);

            await Assert.ThrowsAsync<BusinessRuleValidationException>(act);
            uow.CommitCallCount.Should().Be(0);
        }
    }
}
