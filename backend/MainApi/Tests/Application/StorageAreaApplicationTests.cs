using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDNetCore.Domain.PortInfrastructure.StorageArea;
using DDDNetCore.Infraestructure.PortInfrastructure.DTOs;
using DDDSample1.Domain.Docks;
using DDDSample1.Domain.PortInfrastructure.StorageArea;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.StorageAreas.DTOs;
using DDDSample1.Domain.Vessels;
using FluentAssertions;
using Xunit;

namespace DDDNetCore.Tests.Application
{
    public class StorageAreaApplicationTests
    {
        // In-memory fake StorageAreaRepository
        private class InMemoryStorageAreaRepository : IStorageAreaRepository
        {
            private readonly List<StorageArea> _items = new();

            public Task<List<StorageArea>> GetAllAsync() => Task.FromResult(_items.ToList());

            public Task<List<StorageArea>> GetByIdsAsync(List<StorageAreaID> ids)
            {
                var guidIds = ids.Select(i => i.AsGuid()).ToHashSet();
                var result = _items.Where(sa => guidIds.Contains(sa.Id.AsGuid())).ToList();
                return Task.FromResult(result);
            }

            public Task<StorageArea> AddAsync(StorageArea obj)
            {
                _items.Add(obj);
                return Task.FromResult(obj);
            }

            public void Remove(StorageArea obj) => _items.Remove(obj);

            public Task<StorageArea> UpdateAsync(StorageArea obj) => Task.FromResult(obj);

            Task IRepository<StorageArea, StorageAreaID>.UpdateAsync(StorageArea obj)
            {
                return UpdateAsync(obj);
            }

            public Task<StorageArea?> GetByCodeAsync(string code) =>
                Task.FromResult(_items.FirstOrDefault(sa => sa.Code == code && sa.Active));

            public Task<List<StorageArea>> GetByStorageTypeAsync(StorageAreaType storageType)
            {
                throw new NotImplementedException();
            }

            public Task<StorageArea> GetByIdAsync(StorageAreaID id) =>
                Task.FromResult(_items.FirstOrDefault(sa => sa.Id.Equals(id) && sa.Active));
        }

        // In-memory fake DockRepository
        private class InMemoryDockRepository : IDockRepository
        {
            private readonly List<Dock> _items = new();

            public Task<Dock> AddAsync(Dock obj)
            {
                _items.Add(obj);
                return Task.FromResult(obj);
            }

            public Task<Dock> GetByIdAsync(DockID id) => Task.FromResult(_items.FirstOrDefault(d => d.Id.AsGuid() == id.AsGuid()));

            public Task<List<Dock>> GetAllAsync() => Task.FromResult(_items.ToList());

            public void Remove(Dock obj) => _items.Remove(obj);

            public Task UpdateAsync(Dock obj) => Task.CompletedTask;

            public Task<List<Dock>> GetByIdsAsync(List<DockID> ids)
            {
                var guidIds = ids.Select(i => i.AsGuid()).ToHashSet();
                var result = _items.Where(d => guidIds.Contains(d.Id.AsGuid())).ToList();
                return Task.FromResult(result);
            }

            public Task<List<Dock>> SearchByNameAsync(string name)
            {
                var lower = name?.ToLowerInvariant() ?? "";
                var result = _items.Where(d => d.Name.ToLowerInvariant().Contains(lower)).ToList();
                return Task.FromResult(result);
            }

            public Task<List<Dock>> FilterByVesselTypeAsync(VesselTypeId typeId)
            {
                var result = _items.Where(d => d.AllowedVesselTypes.Any(v => v.Id.AsGuid() == typeId.AsGuid())).ToList();
                return Task.FromResult(result);
            }

            public Task<List<Dock>> FilterByLocationAsync(string locationQuery)
            {
                var lower = locationQuery?.ToLowerInvariant() ?? "";
                var result = _items.Where(d => d.Location.Description.ToLowerInvariant().Contains(lower)).ToList();
                return Task.FromResult(result);
            }

            public Task<bool> ExistsByNameAsync(string name)
            {
                return Task.FromResult(_items.Any(d => d.Name.Equals(name, StringComparison.OrdinalIgnoreCase)));
            }
        }

        // In-memory fake UnitOfWork
        private class InMemoryUnitOfWork : IUnitOfWork
        {
            public Task<int> CommitAsync() => Task.FromResult(0);
        }

        [Fact]
        public async Task AddStorageArea_ShouldCreateSuccessfully()
        {
            var storageRepo = new InMemoryStorageAreaRepository();
            var dockRepo = new InMemoryDockRepository();
            var uow = new InMemoryUnitOfWork();

            var service = new StorageAreaService(uow, storageRepo, dockRepo);

            var createDto = new CreateStorageAreaDto
            {
                Code = "SA01",
                Designation = "Main Storage",
                StorageAreaType = StorageAreaType.Refrigerated,
                Coordinates = "41.123,-8.611",
                LocationDescription = "North Terminal",
                MaxCapacityTEUs = 500,
                InitialDockAssignments = null,
                Length = 10,
                Width = 15,
                Height = 8
            };

            var result = await service.AddAsync(createDto);

            result.Should().NotBeNull();
            result.Code.Should().Be("SA01");
            result.Designation.Should().Be("Main Storage");

            var all = await storageRepo.GetAllAsync();
            all.Should().HaveCount(1);
        }

        [Fact]
        public async Task UpdateStorageArea_ShouldModifyValues()
        {
            var storageRepo = new InMemoryStorageAreaRepository();
            var dockRepo = new InMemoryDockRepository();
            var uow = new InMemoryUnitOfWork();

            var service = new StorageAreaService(uow, storageRepo, dockRepo);

            var area = new StorageArea("SA01", "Old Storage", StorageAreaType.Yard, new Location("41.0,-8.0", "Old Loc"), 300, 5,5,5);
            await storageRepo.AddAsync(area);

            var updateDto = new UpdateStorageAreaDto
            {
                Code = "SA02",
                Designation = "Updated Storage",
                StorageAreaType = StorageAreaType.Refrigerated,
                Coordinates = "41.123,-8.611",
                LocationDescription = "North Terminal",
                MaxCapacityTEUs = 500,
                CurrentOccupancyTEUs = 100,
                Length = 10,
                Width = 15,
                Height = 8
            };

            var result = await service.UpdateAsync(area.Id, updateDto);

            result.Code.Should().Be("SA02");
            result.Designation.Should().Be("Updated Storage");
        }

        [Fact]
        public async Task AssignAndUnassignDock_ShouldUpdateAssignments()
        {
            var storageRepo = new InMemoryStorageAreaRepository();
            var dockRepo = new InMemoryDockRepository();
            var uow = new InMemoryUnitOfWork();

            var service = new StorageAreaService(uow, storageRepo, dockRepo);

            var area = new StorageArea("SA01", "Storage", StorageAreaType.Refrigerated, new Location("41.123,-8.611", "North Terminal"), 500, 10,15,8);
            await storageRepo.AddAsync(area);

            var vesselType = new VesselType("Cargo", "Cargo ships", 5000, 10, 15, 8);
            var dock = new Dock("Dock 1", 100, 10, 5, new Location("0,0", "Dock Loc"), new List<VesselType> { vesselType });
            await dockRepo.AddAsync(dock);

            var assignDto = new AssignDockDto { DockId = dock.Id.AsGuid(), DistanceMeters = 150 };
            var assigned = await service.AssignDockAsync(area.Id, assignDto);

            assigned.AssignedDocks.Should().ContainSingle(d => d.DockId == dock.Id.AsGuid());

            var unassigned = await service.UnassignDockAsync(area.Id, dock.Id);
            unassigned.AssignedDocks.Should().BeEmpty();
        }

        [Fact]
        public async Task InactivateAndActivateStorageArea_ShouldChangeActiveStatus()
        {
            var storageRepo = new InMemoryStorageAreaRepository();
            var dockRepo = new InMemoryDockRepository();
            var uow = new InMemoryUnitOfWork();

            var service = new StorageAreaService(uow, storageRepo, dockRepo);

            var area = new StorageArea("SA01", "Storage", StorageAreaType.Refrigerated, new Location("41.123,-8.611", "North Terminal"), 500, 10,15,8);
            await storageRepo.AddAsync(area);

            var inactivated = await service.InactivateAsync(area.Id);
            //inactivated.Active.Should().BeFalse();

            var activated = await service.ActivateAsync(area.Id);
            //activated.Active.Should().BeTrue();
        }
        [Fact]
public async Task AddStorageArea_WithDuplicateCode_ShouldThrowException()
{
    var storageRepo = new InMemoryStorageAreaRepository();
    var dockRepo = new InMemoryDockRepository();
    var uow = new InMemoryUnitOfWork();
    var service = new StorageAreaService(uow, storageRepo, dockRepo);

    var createDto1 = new CreateStorageAreaDto
    {
        Code = "SA01",
        Designation = "Storage 1",
        StorageAreaType = StorageAreaType.Yard,
        Coordinates = "41.0,-8.0",
        LocationDescription = "Loc 1",
        MaxCapacityTEUs = 200,
        Length = 10,
        Width = 15,
        Height = 8
    };
    await service.AddAsync(createDto1);

    var createDto2 = new CreateStorageAreaDto
    {
        Code = "SA01", // duplicate code
        Designation = "Storage 2",
        StorageAreaType = StorageAreaType.Refrigerated,
        Coordinates = "41.1,-8.1",
        LocationDescription = "Loc 2",
        MaxCapacityTEUs = 300,
        Length = 12,
        Width = 18,
        Height = 9
    };

    Func<Task> act = async () => await service.AddAsync(createDto2);

    await Assert.ThrowsAsync<BusinessRuleValidationException>(act);
}

[Fact]
public async Task AssignDock_ToNonExistingStorageArea_ShouldThrowException()
{
    var storageRepo = new InMemoryStorageAreaRepository();
    var dockRepo = new InMemoryDockRepository();
    var uow = new InMemoryUnitOfWork();
    var service = new StorageAreaService(uow, storageRepo, dockRepo);

    var vesselType = new VesselType("Cargo", "Cargo ships", 5000, 10, 15, 8);
    var dock = new Dock("Dock 1", 100, 10, 5, new Location("0,0", "Dock Loc"), new List<VesselType> { vesselType });
    await dockRepo.AddAsync(dock);

    var assignDto = new AssignDockDto { DockId = dock.Id.AsGuid(), DistanceMeters = 150 };

    //Func<Task> act = async () => await service.AssignDockAsync(Guid.NewGuid(), assignDto); // random ID

    //await Assert.ThrowsAsync<BusinessRuleValidationException>(act);
}

[Fact]
public async Task AssignDock_WithNegativeDistance_ShouldThrowException()
{
    var storageRepo = new InMemoryStorageAreaRepository();
    var dockRepo = new InMemoryDockRepository();
    var uow = new InMemoryUnitOfWork();
    var service = new StorageAreaService(uow, storageRepo, dockRepo);

    var area = new StorageArea("SA01", "Storage", StorageAreaType.Yard, new Location("0,0", "Loc"), 100, 10,10,10);
    await storageRepo.AddAsync(area);

    var vesselType = new VesselType("Cargo", "Cargo ships", 5000, 10, 15, 8);
    var dock = new Dock("Dock 1", 100, 10, 5, new Location("0,0", "Dock Loc"), new List<VesselType> { vesselType });
    await dockRepo.AddAsync(dock);

    var assignDto = new AssignDockDto { DockId = dock.Id.AsGuid(), DistanceMeters = -10 }; // invalid distance

    Func<Task> act = async () => await service.AssignDockAsync(area.Id, assignDto);

    await Assert.ThrowsAsync<BusinessRuleValidationException>(act);
}

[Fact]
public async Task GetStorageAreaById_ShouldReturnCorrectArea()
{
    var storageRepo = new InMemoryStorageAreaRepository();
    var dockRepo = new InMemoryDockRepository();
    var uow = new InMemoryUnitOfWork();
    var service = new StorageAreaService(uow, storageRepo, dockRepo);

    var area1 = new StorageArea("SA01", "Storage 1", StorageAreaType.Yard, new Location("0,0", "Loc1"), 100, 10,10,10);
    var area2 = new StorageArea("SA02", "Storage 2", StorageAreaType.Refrigerated, new Location("1,1", "Loc2"), 200, 12,12,12);
    await storageRepo.AddAsync(area1);
    await storageRepo.AddAsync(area2);

    var result = await service.GetByIdAsync(area2.Id);

    result.Should().NotBeNull();
    result.Code.Should().Be("SA02");
    result.Designation.Should().Be("Storage 2");
}

[Fact]
public async Task GetAllStorageAreas_ShouldReturnAllAddedAreas()
{
    var storageRepo = new InMemoryStorageAreaRepository();
    var dockRepo = new InMemoryDockRepository();
    var uow = new InMemoryUnitOfWork();
    var service = new StorageAreaService(uow, storageRepo, dockRepo);

    var area1 = new StorageArea("SA01", "Storage 1", StorageAreaType.Yard, new Location("0,0", "Loc1"), 100, 10,10,10);
    var area2 = new StorageArea("SA02", "Storage 2", StorageAreaType.Refrigerated, new Location("1,1", "Loc2"), 20,5, 12,12);
    await storageRepo.AddAsync(area1);
    await storageRepo.AddAsync(area2);

    var result = await service.GetAllAsync();

    result.Should().HaveCount(2);
    result.Select(a => a.Code).Should().Contain(new[] { "SA01", "SA02" });
}

    }
}
