using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.Docks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Vessels;
using DDDSample1.Infrastructure.Docks;
using FluentAssertions;
using Xunit;

namespace DDDNetCore.Tests.Application
{
    public class DockApplicationTests
    {
        // In-memory fake DockRepository
        private class InMemoryDockRepository : IDockRepository
        {
            private readonly List<Dock> _items = new();

            public Task<List<Dock>> GetByIdsAsync(List<DockID> ids)
            {
                if (ids == null || !ids.Any())
                    return Task.FromResult(new List<Dock>());

                var guidIds = ids.Select(i => i.AsGuid()).ToHashSet();
                var result = _items
                    .Where(d => guidIds.Contains(d.Id.AsGuid()))
                    .ToList();

                return Task.FromResult(result);
            }

            public Task<Dock> AddAsync(Dock obj)
            {
                _items.Add(obj);
                return Task.FromResult(obj);
            }

            public void Remove(Dock obj) => _items.Remove(obj);

            public Task UpdateAsync(Dock obj) => Task.CompletedTask;

            public Task<List<Dock>> GetAllAsync() => Task.FromResult(_items.ToList());

            public Task<Dock> GetByIdAsync(DockID id)
            {
                var found = _items.FirstOrDefault(d => d.Id.AsGuid() == id.AsGuid());
                return Task.FromResult(found);
            }

            public Task<List<Dock>> SearchByNameAsync(string name)
            {
                var lower = name?.ToLowerInvariant() ?? "";
                var result = _items.Where(d => d.Name.ToLowerInvariant().Contains(lower)).ToList();
                return Task.FromResult(result);
            }

            public Task<List<Dock>> FilterByVesselTypeAsync(VesselTypeId typeId)
            {
                var result = _items
                    .Where(d => d.AllowedVesselTypes.Any(v => v.Id.AsGuid() == typeId.AsGuid()))
                    .ToList();
                return Task.FromResult(result);
            }

            public Task<List<Dock>> FilterByLocationAsync(string locationQuery)
            {
                var lower = locationQuery?.ToLowerInvariant() ?? "";
                var result = _items
                    .Where(d => d.Location.Description.ToLowerInvariant().Contains(lower))
                    .ToList();
                return Task.FromResult(result);
            }

            public Task<bool> ExistsByNameAsync(string name)
            {
                var exists = _items.Any(d => d.Name.Equals(name, StringComparison.OrdinalIgnoreCase));
                return Task.FromResult(exists);
            }
        }

        // In-memory fake VesselTypeRepository
        private class InMemoryVesselTypeRepository : IVesselTypeRepository
        {
            private readonly List<VesselType> _types = new();

            public Task<VesselType> AddAsync(VesselType obj)
            {
                _types.Add(obj);
                return Task.FromResult(obj);
            }

            public Task<List<VesselType>> GetAllAsync() => Task.FromResult(_types.ToList());

            public Task<VesselType> GetByIdAsync(VesselTypeId id)
            {
                var found = _types.FirstOrDefault(t => t.Id.AsGuid() == id.AsGuid());
                return Task.FromResult(found);
            }

            public void Remove(VesselType obj) => _types.Remove(obj);
            public Task UpdateAsync(VesselType obj) => Task.CompletedTask;

            public Task<List<VesselType>> GetByIdsAsync(List<VesselTypeId> ids)
            {
                var guids = ids.Select(i => i.AsGuid()).ToHashSet();
                var result = _types.Where(t => guids.Contains(t.Id.AsGuid())).ToList();
                return Task.FromResult(result);
            }

            // Not used in these tests
            public Task<List<VesselType>> SearchByNameAsync(string name) => Task.FromResult(new List<VesselType>());
            public Task<List<VesselType>> SearchByDescriptionAsync(string description) => Task.FromResult(new List<VesselType>());
            public Task<List<VesselType>> SearchByNameOrDescriptionAsync(string searchTerm) => Task.FromResult(new List<VesselType>());
        }

        [Fact]
        public async Task AddDock_ShouldCreateDockSuccessfully()
        {
            // Arrange
            var dockRepo = new InMemoryDockRepository();
            var vesselRepo = new InMemoryVesselTypeRepository();

            var vesselType = new VesselType("Cargo", "For cargo ships", 5000, 10, 15, 8);
            await vesselRepo.AddAsync(vesselType);

            var service = new DockService(dockRepo, vesselRepo);

            var dto = new DockDto
            {
                Name = "Main Dock",
                Length = 200,
                Depth = 15,
                MaxDraft = 8,
                Coordinates = "41.123,-8.611",
                LocationDescription = "North terminal",
                VesselTypeIds = new List<Guid> { vesselType.Id.AsGuid() }
            };

            // Act
            var result = await service.AddAsync(dto);

            // Assert
            result.Should().NotBeNull();
            result.Name.Should().Be("Main Dock");
            result.Length.Should().Be(200);
            result.AllowedVesselTypes.Should().Contain("Cargo");

            var all = await dockRepo.GetAllAsync();
            all.Should().HaveCount(1);
        }

        [Fact]
        public async Task AddDock_WithNoVesselTypes_ShouldThrowBusinessRuleException()
        {
            var dockRepo = new InMemoryDockRepository();
            var vesselRepo = new InMemoryVesselTypeRepository();
            var service = new DockService(dockRepo, vesselRepo);

            var dto = new DockDto
            {
                Name = "Empty Dock",
                Length = 100,
                Depth = 10,
                MaxDraft = 5,
                Coordinates = "41.123,-8.611",
                LocationDescription = "South",
                VesselTypeIds = new List<Guid>() // empty
            };

            Func<Task> act = async () => await service.AddAsync(dto);

            await Assert.ThrowsAsync<BusinessRuleValidationException>(act);
        }

        [Fact]
        public async Task UpdateDock_ShouldModifyValues()
        {
            var dockRepo = new InMemoryDockRepository();
            var vesselRepo = new InMemoryVesselTypeRepository();
            var vesselType = new VesselType("Cargo", "For cargo ships", 5000, 10, 15, 8);
            await vesselRepo.AddAsync(vesselType);

            var dock = new Dock("Old Dock", 150, 12, 6, new Location("0,0", "Old Location"), new List<VesselType> { vesselType });
            await dockRepo.AddAsync(dock);

            var service = new DockService(dockRepo, vesselRepo);

            var updateDto = new DockDto
            {
                Name = "Updated Dock",
                Length = 180,
                Depth = 14,
                MaxDraft = 7,
                Coordinates = "40.999,-8.600",
                LocationDescription = "Updated Location",
                VesselTypeIds = new List<Guid> { vesselType.Id.AsGuid() }
            };

            var result = await service.UpdateAsync(dock.Id.AsString(), updateDto);

            result.Should().NotBeNull();
            result.Name.Should().Be("Updated Dock");
            result.Length.Should().Be(180);
            result.LocationDescription.Should().Be("Updated Location");
        }

        [Fact]
        public async Task InactivateDock_ShouldMarkDockAsInactive()
        {
            var dockRepo = new InMemoryDockRepository();
            var vesselRepo = new InMemoryVesselTypeRepository();
            var vesselType = new VesselType("Cargo", "For cargo ships", 5000, 10, 15, 8);
            await vesselRepo.AddAsync(vesselType);

            var dock = new Dock("Active Dock", 100, 10, 5, new Location("0,0", "Location"), new List<VesselType> { vesselType });
            await dockRepo.AddAsync(dock);

            var service = new DockService(dockRepo, vesselRepo);

            var result = await service.InactivateAsync(dock.Id);

            result.Should().NotBeNull();
            //result.Active.Should().BeFalse();

            var stored = await dockRepo.GetByIdAsync(dock.Id);
            stored.Active.Should().BeFalse();
        }

        [Fact]
        public async Task SearchByName_ShouldReturnMatchingDock()
        {
            var dockRepo = new InMemoryDockRepository();
            var vesselRepo = new InMemoryVesselTypeRepository();
            var vesselType = new VesselType("Cargo", "For cargo ships", 5000, 10, 15, 8);
            await vesselRepo.AddAsync(vesselType);

            await dockRepo.AddAsync(new Dock("East Dock", 200, 15, 8, new Location("41,-8", "East Zone"), new List<VesselType> { vesselType }));
            await dockRepo.AddAsync(new Dock("West Dock", 180, 12, 7, new Location("41,-9", "West Zone"), new List<VesselType> { vesselType }));

            var service = new DockService(dockRepo, vesselRepo);

            var result = await service.SearchByNameAsync("East");

            result.Should().HaveCount(1);
            result.First().Name.Should().Be("East Dock");
        }
                [Fact]
        public async Task GetAllAsync_ShouldReturnAllDocks()
        {
            // Arrange
            var dockRepo = new InMemoryDockRepository();
            var vesselRepo = new InMemoryVesselTypeRepository();
            var vesselType = new VesselType("Tanker", "Oil ships", 8000, 12, 20, 10);
            await vesselRepo.AddAsync(vesselType);

            await dockRepo.AddAsync(new Dock("Dock A", 100, 8, 5, new Location("0,0", "A"), new List<VesselType> { vesselType }));
            await dockRepo.AddAsync(new Dock("Dock B", 120, 9, 6, new Location("1,1", "B"), new List<VesselType> { vesselType }));

            var service = new DockService(dockRepo, vesselRepo);

            // Act
            var result = await service.GetAllAsync();

            // Assert
            result.Should().HaveCount(2);
            result.Select(d => d.Name).Should().Contain(new[] { "Dock A", "Dock B" });
        }

        [Fact]
        public async Task GetByIdAsync_ShouldReturnCorrectDock()
        {
            // Arrange
            var dockRepo = new InMemoryDockRepository();
            var vesselRepo = new InMemoryVesselTypeRepository();
            var vesselType = new VesselType("Cargo", "Cargo ships", 5000, 10, 15, 8);
            await vesselRepo.AddAsync(vesselType);

            var dock = new Dock("Target Dock", 150, 12, 6, new Location("0,0", "Target"), new List<VesselType> { vesselType });
            await dockRepo.AddAsync(dock);

            var service = new DockService(dockRepo, vesselRepo);

            // Act
            var result = await service.GetByIdAsync(dock.Id);

            // Assert
            result.Should().NotBeNull();
            result.Name.Should().Be("Target Dock");
            result.LocationDescription.Should().Be("Target");
        }

        [Fact]
        public async Task DeleteAsync_ShouldRemoveDock()
        {
            // Arrange
            var dockRepo = new InMemoryDockRepository();
            var vesselRepo = new InMemoryVesselTypeRepository();
            var vesselType = new VesselType("Cargo", "Cargo ships", 5000, 10, 15, 8);
            await vesselRepo.AddAsync(vesselType);

            var dock = new Dock("To Delete", 100, 10, 5, new Location("0,0", "To Delete"), new List<VesselType> { vesselType });
            await dockRepo.AddAsync(dock);

            var service = new DockService(dockRepo, vesselRepo);

            // Act
            var result = await service.DeleteAsync(dock.Id);
            var remaining = await dockRepo.GetAllAsync();

            // Assert
            result.Should().NotBeNull();
            remaining.Should().BeEmpty();
        }

        [Fact]
        public async Task FilterByVesselType_ShouldReturnMatchingDocks()
        {
            // Arrange
            var dockRepo = new InMemoryDockRepository();
            var vesselRepo = new InMemoryVesselTypeRepository();

            var cargo = new VesselType("Cargo", "Cargo ships", 5000, 10, 15, 8);
            var tanker = new VesselType("Tanker", "Oil ships", 8000, 12, 20, 10);
            await vesselRepo.AddAsync(cargo);
            await vesselRepo.AddAsync(tanker);

            await dockRepo.AddAsync(new Dock("Cargo Dock", 200, 15, 8, new Location("0,0", "A"), new List<VesselType> { cargo }));
            await dockRepo.AddAsync(new Dock("Tanker Dock", 250, 20, 10, new Location("1,1", "B"), new List<VesselType> { tanker }));

            var service = new DockService(dockRepo, vesselRepo);

            // Act
            var result = await service.FilterByVesselTypeAsync(tanker.Id.AsGuid());

            // Assert
            result.Should().HaveCount(1);
            result.First().Name.Should().Be("Tanker Dock");
        }

        [Fact]
        public async Task FilterByLocation_ShouldReturnMatchingDocks()
        {
            // Arrange
            var dockRepo = new InMemoryDockRepository();
            var vesselRepo = new InMemoryVesselTypeRepository();
            var vesselType = new VesselType("Cargo", "Cargo ships", 5000, 10, 15, 8);
            await vesselRepo.AddAsync(vesselType);

            await dockRepo.AddAsync(new Dock("Dock North", 100, 10, 5, new Location("0,0", "Northern Bay"), new List<VesselType> { vesselType }));
            await dockRepo.AddAsync(new Dock("Dock South", 100, 10, 5, new Location("1,1", "Southern Bay"), new List<VesselType> { vesselType }));

            var service = new DockService(dockRepo, vesselRepo);

            // Act
            var result = await service.FilterByLocationAsync("South");

            // Assert
            result.Should().HaveCount(1);
            result.First().Name.Should().Be("Dock South");
        }

        [Fact]
        public async Task GetByIdsAsync_ShouldReturnCorrectSubsetOfDocks()
        {
            // Arrange
            var dockRepo = new InMemoryDockRepository();
            var vesselRepo = new InMemoryVesselTypeRepository();
            var vesselType = new VesselType("Cargo", "Cargo ships", 5000, 10, 15, 8);
            await vesselRepo.AddAsync(vesselType);

            var dock1 = new Dock("Dock 1", 150, 12, 6, new Location("0,0", "A"), new List<VesselType> { vesselType });
            var dock2 = new Dock("Dock 2", 160, 13, 7, new Location("0,1", "B"), new List<VesselType> { vesselType });
            var dock3 = new Dock("Dock 3", 170, 14, 8, new Location("0,2", "C"), new List<VesselType> { vesselType });
            await dockRepo.AddAsync(dock1);
            await dockRepo.AddAsync(dock2);
            await dockRepo.AddAsync(dock3);

            var service = new DockService(dockRepo, vesselRepo);

            // Act
            var result = await dockRepo.GetByIdsAsync(new List<DockID> { dock1.Id, dock3.Id });

            // Assert
            result.Should().HaveCount(2);
            result.Select(d => d.Name).Should().Contain(new[] { "Dock 1", "Dock 3" });
        }

    }
}
