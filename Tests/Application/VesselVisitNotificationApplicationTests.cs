using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Threading.Tasks;
using FluentAssertions;
using Xunit;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Vessels;
using DDDSample1.Domain.Vessels.VesselInformation;
using DDDSample1.Domain.Vessels.VesselVisitNotification;
using DDDSample1.Domain.Organizations;

namespace DDDNetCore.Tests.Application
{
    public class VesselVisitNotificationApplicationTests
    {
        // In-memory vessel repository
        private class InMemoryVesselRepository : IVesselRepository
        {
            private readonly List<Vessel> _items = new List<Vessel>();

            public Task<Vessel> AddAsync(Vessel obj)
            {
                _items.Add(obj);
                return Task.FromResult(obj);
            }

            public void Remove(Vessel obj) => _items.Remove(obj);

            public Task UpdateAsync(Vessel obj) => Task.CompletedTask;

            public Task<List<Vessel>> GetAllAsync() => Task.FromResult(_items.ToList());

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
                if (string.IsNullOrWhiteSpace(name)) return Task.FromResult(new List<Vessel>());
                var lower = name.ToLowerInvariant();
                var found = _items.Where(x => x.Name != null && x.Name.ToLowerInvariant().Contains(lower)).ToList();
                return Task.FromResult(found);
            }

            public Task<List<Vessel>> SearchByOwnerAsync(string owner) => Task.FromResult(_items.Where(x => x.Owner == owner).ToList());

            public Task<List<Vessel>> SearchByOperatorAsync(string operatorName) => Task.FromResult(_items.Where(x => x.Operator == operatorName).ToList());

            public Task<List<Vessel>> SearchAsync(string searchTerm)
            {
                if (string.IsNullOrWhiteSpace(searchTerm)) return Task.FromResult(new List<Vessel>());
                var lower = searchTerm.ToLowerInvariant();
                var found = _items.Where(x => (x.Name != null && x.Name.ToLowerInvariant().Contains(lower)) ||
                                             (x.ImoNumber != null && x.ImoNumber.ToLowerInvariant().Contains(lower))).ToList();
                return Task.FromResult(found);
            }

            public Task<bool> ExistsByImoNumberAsync(string imoNumber) => Task.FromResult(_items.Any(x => x.ImoNumber == imoNumber));
        }

        // In-memory vessel type repository
        private class InMemoryVesselTypeRepository : IVesselTypeRepository
        {
            private readonly List<VesselType> _items = new List<VesselType>();

            public Task<VesselType> AddAsync(VesselType obj)
            {
                _items.Add(obj);
                return Task.FromResult(obj);
            }

            public void Remove(VesselType obj) => _items.Remove(obj);

            public Task UpdateAsync(VesselType obj) => Task.CompletedTask;

            public Task<List<VesselType>> GetAllAsync() => Task.FromResult(_items.ToList());

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
                if (string.IsNullOrWhiteSpace(name)) return Task.FromResult(new List<VesselType>());
                var lower = name.ToLowerInvariant();
                var found = _items.Where(x => x.Name != null && x.Name.ToLowerInvariant().Contains(lower)).ToList();
                return Task.FromResult(found);
            }

            public Task<List<VesselType>> SearchByDescriptionAsync(string description)
            {
                if (string.IsNullOrWhiteSpace(description)) return Task.FromResult(new List<VesselType>());
                var lower = description.ToLowerInvariant();
                var found = _items.Where(x => x.Description != null && x.Description.ToLowerInvariant().Contains(lower)).ToList();
                return Task.FromResult(found);
            }

            public Task<List<VesselType>> SearchByNameOrDescriptionAsync(string searchTerm)
            {
                if (string.IsNullOrWhiteSpace(searchTerm)) return Task.FromResult(new List<VesselType>());
                var lower = searchTerm.ToLowerInvariant();
                var found = _items.Where(x => (x.Name != null && x.Name.ToLowerInvariant().Contains(lower)) || (x.Description != null && x.Description.ToLowerInvariant().Contains(lower))).ToList();
                return Task.FromResult(found);
            }
        }

        // In-memory vessel visit notification repository
        private class InMemoryVesselVisitNotificationRepository : IVesselVisitNotificationRepository
        {
            private readonly List<VesselVisitNotification> _items = new List<VesselVisitNotification>();

            public Task<VesselVisitNotification> AddAsync(VesselVisitNotification obj)
            {
                _items.Add(obj);
                return Task.FromResult(obj);
            }

            public void Remove(VesselVisitNotification obj) => _items.Remove(obj);

            public Task UpdateAsync(VesselVisitNotification obj) => Task.CompletedTask;

            public Task<List<VesselVisitNotification>> GetAllAsync() => Task.FromResult(_items.ToList());

            public Task<VesselVisitNotification> GetByIdAsync(VesselVisitNotificationID id)
            {
                var found = _items.FirstOrDefault(x => x.Id.AsGuid() == id.AsGuid());
                return Task.FromResult(found);
            }

            public Task<List<VesselVisitNotification>> GetByIdsAsync(List<VesselVisitNotificationID> ids)
            {
                var guids = ids.Select(i => i.AsGuid()).ToHashSet();
                var found = _items.Where(x => guids.Contains(x.Id.AsGuid())).ToList();
                return Task.FromResult(found);
            }

            public Task<List<VesselVisitNotification>> GetCompletedNotificationsAsync()
            {
                var found = _items.Where(x => x.Status == NotificationStatus.Completed).ToList();
                return Task.FromResult(found);
            }

            public Task<List<VesselVisitNotification>> GetByStateAsync(NotificationStatus status)
            {
                var found = _items.Where(x => x.Status == status).ToList();
                return Task.FromResult(found);
            }

            public Task<List<VesselVisitNotification>> SearchNotificationsAsync(VesselId vesselId = null, NotificationStatus? status = null, RepresentativeId representativeId = null, OrganizationId organizationId = null, DateTime? startDate = null, DateTime? endDate = null)
            {
                var query = _items.AsEnumerable();
                if (vesselId != null)
                    query = query.Where(x => x.Vessel != null && x.Vessel.Id.AsGuid() == vesselId.AsGuid());
                if (status.HasValue)
                    query = query.Where(x => x.Status == status.Value);
                if (representativeId != null)
                    query = query.Where(x => x.RepresentativeId != null && x.RepresentativeId.AsString() == representativeId.AsString());
                if (startDate.HasValue)
                    query = query.Where(x => x.CreatedAt >= startDate.Value);
                if (endDate.HasValue)
                    query = query.Where(x => x.CreatedAt <= endDate.Value);

                return Task.FromResult(query.ToList());
            }
        }

        // In-memory unit of work
        private class InMemoryUnitOfWork : IUnitOfWork
        {
            public int CommitCallCount { get; private set; } = 0;

            public Task<int> CommitAsync()
            {
                CommitCallCount++;
                return Task.FromResult(1);
            }
        }

        private CargoManifest CreateManifestWithContainer(double weight)
        {
            var manifest = CargoManifest.Create(Guid.NewGuid().ToString());

            // Generate a container ID that conforms to ISO 6346 using the same algorithm
            // used in domain tests (letters + 6 digits + check digit)
            string baseId = "ABCD" + new Random().Next(0, 999999).ToString("D6");
            int sum = 0;
            for (int i = 0; i < 10; i++)
            {
                char c = baseId[i];
                int charValue = char.IsLetter(c) ? (c - 'A' + 10) : (c - '0');
                sum += charValue * (int)Math.Pow(2, i);
            }
            int checkDigit = (sum % 11) % 10;
            string containerId = baseId + checkDigit.ToString();

            var container = Container.Create(containerId, weight, "contents");
            manifest.AddContainer(container);
            return manifest;
        }

        [Fact]
        public async Task CreateNotification_CreatesAndReturnsDto()
        {
            var vesselRepo = new InMemoryVesselRepository();
            var vesselTypeRepo = new InMemoryVesselTypeRepository();
            var notifRepo = new InMemoryVesselVisitNotificationRepository();
            var uow = new InMemoryUnitOfWork();

            var vesselType = new VesselType("TypeA", "desc", 10000, 1, 1, 1);
            await vesselTypeRepo.AddAsync(vesselType);

            var vessel = new Vessel("IMO1234567", "Test Vessel", vesselType.Id, "Owner", "Operator");
            await vesselRepo.AddAsync(vessel);

            var service = new VesselVisitNotificationService(uow, notifRepo, vesselRepo, vesselTypeRepo);

            var loading = new List<CargoManifest>{ CreateManifestWithContainer(1000) };
            var unloading = new List<CargoManifest>{ CreateManifestWithContainer(500) };

            var result = await service.CreateAsync(vessel.Id.AsGuid(), "REP001", loading, unloading, null);

            result.Should().NotBeNull();
            result.VesselId.Should().Be(vessel.Id.AsGuid());
            var all = await notifRepo.GetAllAsync();
            all.Should().ContainSingle();
            uow.CommitCallCount.Should().Be(1);
        }

        [Fact]
        public async Task CreateNotification_UnloadingExceedsCapacity_Throws()
        {
            var vesselRepo = new InMemoryVesselRepository();
            var vesselTypeRepo = new InMemoryVesselTypeRepository();
            var notifRepo = new InMemoryVesselVisitNotificationRepository();
            var uow = new InMemoryUnitOfWork();

            var vesselType = new VesselType("Small", "desc", 100, 1, 1, 1);
            await vesselTypeRepo.AddAsync(vesselType);

            var vessel = new Vessel("IMO7654321", "Small Vessel", vesselType.Id, "Owner", "Operator");
            await vesselRepo.AddAsync(vessel);

            var service = new VesselVisitNotificationService(uow, notifRepo, vesselRepo, vesselTypeRepo);

            // create unloading manifest whose weight exceeds capacity
            var heavyManifest = CreateManifestWithContainer(1000);
            var unloading = new List<CargoManifest>{ heavyManifest };

            Func<Task> act = async () => await service.CreateAsync(vessel.Id.AsGuid(), "REP002", null, unloading, null);

            await Assert.ThrowsAsync<BusinessRuleValidationException>(act);
            uow.CommitCallCount.Should().Be(0);
        }

        [Fact]
        public async Task ApproveAfterCompleted_AllowsApproval()
        {
            var vesselRepo = new InMemoryVesselRepository();
            var vesselTypeRepo = new InMemoryVesselTypeRepository();
            var notifRepo = new InMemoryVesselVisitNotificationRepository();
            var uow = new InMemoryUnitOfWork();

            var vesselType = new VesselType("TypeB", "desc", 5000, 1, 1, 1);
            await vesselTypeRepo.AddAsync(vesselType);
            var vessel = new Vessel("IMO2222222", "Approve Vessel", vesselType.Id, "Owner", "Operator");
            await vesselRepo.AddAsync(vessel);

            var service = new VesselVisitNotificationService(uow, notifRepo, vesselRepo, vesselTypeRepo);

            var loading = new List<CargoManifest>{ CreateManifestWithContainer(100) };
            var unloading = new List<CargoManifest>{ CreateManifestWithContainer(50) };

            var dto = await service.CreateAsync(vessel.Id.AsGuid(), "REP003", loading, unloading, null);
            var stored = await notifRepo.GetByIdAsync(new VesselVisitNotificationID(dto.Id));

            // set status to Completed via reflection (domain tests do the same)
            var statusProp = typeof(VesselVisitNotification).GetProperty("Status", BindingFlags.Instance | BindingFlags.Public);
            statusProp.SetValue(stored, NotificationStatus.Completed);

            var approved = await service.ApproveAsync(stored.Id.AsGuid(), "D1", "O1");
            approved.Status.Should().Be(NotificationStatus.Approved);
            approved.AssignedDock.Should().Be("D1");
            uow.CommitCallCount.Should().Be(2); // one from Create, one from Approve
        }
        
        /*
        [Fact]
        public async Task UpdateInProgress_TotalWeightExceedsCapacity_Throws()
        {
            var vesselRepo = new InMemoryVesselRepository();
            var vesselTypeRepo = new InMemoryVesselTypeRepository();
            var notifRepo = new InMemoryVesselVisitNotificationRepository();
            var uow = new InMemoryUnitOfWork();

            var vesselType = new VesselType("TypeC", "desc", 200, 1, 1, 1);
            await vesselTypeRepo.AddAsync(vesselType);

            var vessel = new Vessel("IMO3333333", "Update Vessel", vesselType.Id, "Owner", "Operator");
            await vesselRepo.AddAsync(vessel);

            var service = new VesselVisitNotificationService(uow, notifRepo, vesselRepo, vesselTypeRepo);

            var loading = new List<CargoManifest> { CreateManifestWithContainer(10) };
            var unloading = new List<CargoManifest> { CreateManifestWithContainer(20) };

            var dto = await service.CreateAsync(vessel.Id.AsGuid(), "REP004", loading, unloading, null);
            var stored = await notifRepo.GetByIdAsync(new VesselVisitNotificationID(dto.Id));

            // Novo DTO com excesso de peso (usa UnloadingCargoMaterialDto!)
            var updateDto = new UpdateNotificationDto
            {
                UnloadingCargo = new UnloadingCargoMaterialDto
                {
                    Manifests = new List<CargoManifestDto>
                    {
                        new CargoManifestDto
                        {
                            Id = Guid.NewGuid().ToString(),
                            Containers = new List<ContainerDto>
                            {
                                new ContainerDto
                                {
                                    Id = Guid.NewGuid().ToString(),
                                    PayloadWeight = 1000, // peso excede a capacidade
                                    ContentsDescription = "Too Heavy"
                                }
                            }
                        }
                    }
                }
            };
            

            Func<Task> act = async () => await service.UpdateInProgressAsync(stored.Id.AsGuid().ToString(), updateDto);

            await Assert.ThrowsAsync<BusinessRuleValidationException>(act);

            // Commit não deve ser chamado novamente, pois o update falhou
            uow.CommitCallCount.Should().Be(1);
        }
        */

        [Fact]
        public async Task SearchNotifications_FilterByVessel_ReturnsMatch()
        {
            var vesselRepo = new InMemoryVesselRepository();
            var vesselTypeRepo = new InMemoryVesselTypeRepository();
            var notifRepo = new InMemoryVesselVisitNotificationRepository();
            var uow = new InMemoryUnitOfWork();

            var vesselType = new VesselType("TypeD", "desc", 10000, 1, 1, 1);
            await vesselTypeRepo.AddAsync(vesselType);
            var vessel1 = new Vessel("IMO4444444", "Search Vessel A", vesselType.Id, "Owner", "Operator");
            var vessel2 = new Vessel("IMO5555555", "Search Vessel B", vesselType.Id, "Owner", "Operator");
            await vesselRepo.AddAsync(vessel1);
            await vesselRepo.AddAsync(vessel2);

            var service = new VesselVisitNotificationService(uow, notifRepo, vesselRepo, vesselTypeRepo);

            var loading = new List<CargoManifest>{ CreateManifestWithContainer(10) };
            var unloading = new List<CargoManifest>{ CreateManifestWithContainer(5) };

            var dto1 = await service.CreateAsync(vessel1.Id.AsGuid(), "REP005", loading, unloading, null);
            var dto2 = await service.CreateAsync(vessel2.Id.AsGuid(), "REP006", loading, unloading, null);

            var filter = new NotificationFilterDto { VesselId = vessel1.Id.AsGuid() };
            var results = await service.SearchNotificationsAsync(filter);

            results.Should().ContainSingle(r => r.VesselId == vessel1.Id.AsGuid());
        }
    }
}
