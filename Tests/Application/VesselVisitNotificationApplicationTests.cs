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
using DDDSample1.Domain.Vessels.VesselVisitNotification.DTOs;
using CargoManifestDto = DDDSample1.Domain.Vessels.VesselInformation.CargoManifestDto;

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
        
        private class InMemoryRepresentativeRepository : IRepresentativeRepository
        {
            private readonly List<Representative> _items = new();

            public Task<Representative> AddAsync(Representative obj)
            {
                _items.Add(obj);
                return Task.FromResult(obj);
            }

            public void Remove(Representative obj) => _items.Remove(obj);
            public Task UpdateAsync(Representative obj) => Task.CompletedTask;
            public Task<List<Representative>> GetAllAsync() => Task.FromResult(_items.ToList());
            public Task<Representative> GetByIdAsync(RepresentativeId id)
            {
                var found = _items.FirstOrDefault(r => r.Id.AsString() == id.AsString());
                return Task.FromResult(found);
            }
            public Task<List<Representative>> GetByIdsAsync(List<RepresentativeId> ids)
            {
                var set = ids.Select(i => i.AsString()).ToHashSet();
                var found = _items.Where(r => set.Contains(r.Id.AsString())).ToList();
                return Task.FromResult(found);
            }
            public Task<List<Representative>> GetActiveRepresentativesAsync() =>
                Task.FromResult(_items.Where(r => r.Status == RepresentativeStatus.Active).ToList());
            public Task<List<Representative>> GetByOrganizationAsync(OrganizationId organizationId) =>
                Task.FromResult(_items.Where(r => r.OrganizationId.AsString() == organizationId.AsString()).ToList());
            public Task<Representative> GetByEmailAsync(string email) =>
                Task.FromResult(_items.FirstOrDefault(r => r.Email == email));
            public Task<bool> ExistsWithEmailAsync(string email) =>
                Task.FromResult(_items.Any(r => r.Email == email));
            public Task<bool> ExistsWithPhoneAsync(string phone) =>
                Task.FromResult(_items.Any(r => r.PhoneNumber == phone));
            public Task<bool> ExistsWithCidAsync(string cid) =>
                Task.FromResult(_items.Any(r => r.Id.AsString() == cid));
            public Task DeleteAsync(Representative rep)
            {
                _items.Remove(rep);
                return Task.CompletedTask;
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
            var manifest = new CargoManifest(new List<Container>() );

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

            var container = new Container( weight, "contents");
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
            var rep = new InMemoryRepresentativeRepository();

            var vesselType = new VesselType("TypeA", "desc", 10000, 1, 1, 1);
            await vesselTypeRepo.AddAsync(vesselType);

            var vessel = new Vessel("IMO1234567", "Test Vessel", vesselType.Id, "Owner", "Operator");
            await vesselRepo.AddAsync(vessel);

            var service = new VesselVisitNotificationService(uow, notifRepo, vesselRepo, vesselTypeRepo,rep);

            var loading = new List<CargoManifest>{ CreateManifestWithContainer(1000) };
            var unloading = new List<CargoManifest>{ CreateManifestWithContainer(500) };
            var representative = new Representative(
                "John Doe",
                "REP001",
                "PT",
                "john1231.doe@gmail.com",
                "+351923888777"
            );
           
            await rep.AddAsync(representative);
            var createDto = new CreateNotificationDto()
            {
                VesselId = vessel.Id.AsGuid(),
                RepresentativeId = "REP001",
                LoadingManifests = loading,
                UnloadingManifests = unloading,
                Crew = null
            };
            

            // 2. Call the service with the DTO
            var result = await service.CreateAsync(createDto);

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
            var rep = new InMemoryRepresentativeRepository();
            
            var vesselType = new VesselType("Small", "desc", 100, 1, 1, 1);
            await vesselTypeRepo.AddAsync(vesselType);

            var vessel = new Vessel("IMO7654321", "Small Vessel", vesselType.Id, "Owner", "Operator");
            await vesselRepo.AddAsync(vessel);

            var service = new VesselVisitNotificationService(uow, notifRepo, vesselRepo, vesselTypeRepo,rep);

            // create unloading manifest whose weight exceeds capacity
            var heavyManifest = CreateManifestWithContainer(1000);
            var unloading = new List<CargoManifest>{ heavyManifest };
            
            var createDto = new CreateNotificationDto()
            {
                VesselId = vessel.Id.AsGuid(),
                RepresentativeId = "REP002",
                LoadingManifests = null,
                UnloadingManifests = unloading,
                Crew = null
            };
            Func<Task> act = async () => await service.CreateAsync(createDto);

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
            var rep = new InMemoryRepresentativeRepository();
            
            var vesselType = new VesselType("TypeB", "desc", 5000, 1, 1, 1);
            await vesselTypeRepo.AddAsync(vesselType);
            var vessel = new Vessel("IMO2222222", "Approve Vessel", vesselType.Id, "Owner", "Operator");
            await vesselRepo.AddAsync(vessel);

            var service = new VesselVisitNotificationService(uow, notifRepo, vesselRepo, vesselTypeRepo,rep);

            var loading = new List<CargoManifest>{ CreateManifestWithContainer(100) };
            var unloading = new List<CargoManifest>{ CreateManifestWithContainer(50) };
            
            // Criar representante associado à organização
            var representative = new Representative(
                "John Doe",
                "REP003",
                "PT",
                "john123.doe@gmail.com",
                "+351921888777"
            );
           
            await rep.AddAsync(representative);
            var createDto = new CreateNotificationDto()
            {
                VesselId = vessel.Id.AsGuid(),
                RepresentativeId = "REP003",
                LoadingManifests = loading,
                UnloadingManifests = unloading,
                Crew = null
            };
            
            var dto = await service.CreateAsync(createDto);
            var stored = await notifRepo.GetByIdAsync(new VesselVisitNotificationID(dto.Id));

            // set status to Completed via reflection (domain tests do the same)
            var statusProp = typeof(VesselVisitNotification).GetProperty("Status", BindingFlags.Instance | BindingFlags.Public);
            statusProp.SetValue(stored, NotificationStatus.Completed);

            var approved = await service.ApproveAsync(stored.Id.AsGuid(), "D1", "O1");
            approved.Status.Should().Be(NotificationStatus.Approved);
            approved.AssignedDock.Should().Be("D1");
            uow.CommitCallCount.Should().Be(2); // one from Create, one from Approve
        }

        [Fact]
        public async Task ApproveWithValidDockAssignment_ChangesStatusAndStoresDock()
        {
            var vesselRepo = new InMemoryVesselRepository();
            var vesselTypeRepo = new InMemoryVesselTypeRepository();
            var notifRepo = new InMemoryVesselVisitNotificationRepository();
            var uow = new InMemoryUnitOfWork();
            var rep = new InMemoryRepresentativeRepository();
            
            var vesselType = new VesselType("TypeE", "desc", 5000, 1, 1, 1);
            await vesselTypeRepo.AddAsync(vesselType);
            var vessel = new Vessel("IMO7777777", "Dock Assign Vessel", vesselType.Id, "Owner", "Operator");
            await vesselRepo.AddAsync(vessel);

            var service = new VesselVisitNotificationService(uow, notifRepo, vesselRepo, vesselTypeRepo,rep);

            var loading = new List<CargoManifest>{ CreateManifestWithContainer(100) };
            var unloading = new List<CargoManifest>{ CreateManifestWithContainer(50) };
            
            // Criar representante associado à organização
            var representative = new Representative(
                "John Doe",
                "REP010",
                "PT",
                "john12.doe@gmail.com",
                "+351911888777"
            );
            await rep.AddAsync(representative);
            var createDto = new CreateNotificationDto()
            {
                VesselId = vessel.Id.AsGuid(),
                RepresentativeId = "REP010",
                LoadingManifests = loading,
                UnloadingManifests = unloading,
                Crew = null
            };
            
            var dto = await service.CreateAsync(createDto);
            var stored = await notifRepo.GetByIdAsync(new VesselVisitNotificationID(dto.Id));

            // set to Completed to allow approval
            var statusProp = typeof(VesselVisitNotification).GetProperty("Status", BindingFlags.Instance | BindingFlags.Public);
            statusProp.SetValue(stored, NotificationStatus.Completed);

            var approved = await service.ApproveAsync(stored.Id.AsGuid(), "Dock42", "OfficerA");
            approved.Status.Should().Be(NotificationStatus.Approved);
            approved.AssignedDock.Should().Be("Dock42");
            approved.DecisionOutcome.Should().Be("Approved");
            uow.CommitCallCount.Should().Be(2);
        }

        [Fact]
        public async Task RejectWithValidReason_ChangesStatusAndStoresReason()
        {
            var vesselRepo = new InMemoryVesselRepository();
            var vesselTypeRepo = new InMemoryVesselTypeRepository();
            var notifRepo = new InMemoryVesselVisitNotificationRepository();
            var uow = new InMemoryUnitOfWork();
            var rep = new InMemoryRepresentativeRepository();
            
            var vesselType = new VesselType("TypeF", "desc", 5000, 1, 1, 1);
            await vesselTypeRepo.AddAsync(vesselType);
            var vessel = new Vessel("IMO8888888", "Reject Vessel", vesselType.Id, "Owner", "Operator");
            await vesselRepo.AddAsync(vessel);
            
            var service = new VesselVisitNotificationService(uow, notifRepo, vesselRepo, vesselTypeRepo,rep);

            var loading = new List<CargoManifest>{ CreateManifestWithContainer(100) };
            var unloading = new List<CargoManifest>{ CreateManifestWithContainer(50) };
            
            // Criar representante associado à organização
            var representative = new Representative(
                "John Doe",
                "REP011",
                "PT",
                "john1.doe@gmail.com",
                "+351991888777"
            );
            await rep.AddAsync(representative);
            var createDto = new CreateNotificationDto()
            {
                VesselId = vessel.Id.AsGuid(),
                RepresentativeId = "REP011",
                LoadingManifests = loading,
                UnloadingManifests = unloading,
                Crew = null
            };
            
            var dto = await service.CreateAsync(createDto);
            var stored = await notifRepo.GetByIdAsync(new VesselVisitNotificationID(dto.Id));

            // set to Completed to allow rejection
            var statusProp = typeof(VesselVisitNotification).GetProperty("Status", BindingFlags.Instance | BindingFlags.Public);
            statusProp.SetValue(stored, NotificationStatus.Completed);

            var rejected = await service.RejectAsync(stored.Id.AsGuid(), "Invalid cargo", "OfficerB");
            rejected.Status.Should().Be(NotificationStatus.Rejected);
            rejected.RejectedReason.Should().Be("Invalid cargo");
            rejected.DecisionOutcome.Should().Be("Rejected");
            uow.CommitCallCount.Should().Be(2);
        }

        [Fact]
        public async Task RejectWithoutReason_ThrowsValidationException()
        {
            var vesselRepo = new InMemoryVesselRepository();
            var vesselTypeRepo = new InMemoryVesselTypeRepository();
            var notifRepo = new InMemoryVesselVisitNotificationRepository();
            var uow = new InMemoryUnitOfWork();
            var rep = new InMemoryRepresentativeRepository();
            
            var vesselType = new VesselType("TypeG", "desc", 5000, 1, 1, 1);
            await vesselTypeRepo.AddAsync(vesselType);
            var vessel = new Vessel("IMO9999999", "Reject No Reason", vesselType.Id, "Owner", "Operator");
            await vesselRepo.AddAsync(vessel);
    
            var service = new VesselVisitNotificationService(uow, notifRepo, vesselRepo, vesselTypeRepo,rep);
            var rep1 = new Representative("Atum","REP012","PT","atum@gmail.com","1234567890");
            
            var loading = new List<CargoManifest>{ CreateManifestWithContainer(100) };
            var unloading = new List<CargoManifest>{ CreateManifestWithContainer(50) };
            
            var representative = new Representative(
                "John Doe",
                "REP012",
                "PT",
                "johnA.doe@gmail.com",
                "+351911111111"
            );
            
            await rep.AddAsync(representative);
            
            var createDto = new CreateNotificationDto()
            {
                VesselId = vessel.Id.AsGuid(),
                RepresentativeId = "REP012",
                LoadingManifests = loading,
                UnloadingManifests = unloading,
                Crew = null
            };
            
            var dto = await service.CreateAsync(createDto);
            var stored = await notifRepo.GetByIdAsync(new VesselVisitNotificationID(dto.Id));

            // set to Completed to allow rejection
            var statusProp = typeof(VesselVisitNotification).GetProperty("Status", BindingFlags.Instance | BindingFlags.Public);
            statusProp.SetValue(stored, NotificationStatus.Completed);

            Func<Task> act = async () => await service.RejectAsync(stored.Id.AsGuid(), "", "OfficerC");

            await Assert.ThrowsAsync<BusinessRuleValidationException>(act);
            uow.CommitCallCount.Should().Be(1); // only Create committed
        }

       [Fact]
        public async Task ApproveWithoutDockAssignment_ThrowsValidationException()
        {
            var vesselRepo = new InMemoryVesselRepository();
            var vesselTypeRepo = new InMemoryVesselTypeRepository();
            var notifRepo = new InMemoryVesselVisitNotificationRepository();
            var uow = new InMemoryUnitOfWork();
            var rep = new InMemoryRepresentativeRepository();
            
            // Criar representante associado à organização
            var representative = new Representative(
                "John Doe",
                "REP013",
                    "PT",
                "john.doe@gmail.com",
                "+351999888777"
            );
            
            await rep.AddAsync(representative);

            var vesselType = new VesselType("TypeH", "desc", 5000, 1, 1, 1);
            await vesselTypeRepo.AddAsync(vesselType);

            var vessel = new Vessel("IMO1010101", "Approve No Dock", vesselType.Id, "Owner", "Operator");
            await vesselRepo.AddAsync(vessel);

            var service = new VesselVisitNotificationService(uow, notifRepo, vesselRepo, vesselTypeRepo, rep);

            var loading = new List<CargoManifest> { CreateManifestWithContainer(100) };
            var unloading = new List<CargoManifest> { CreateManifestWithContainer(50) };

            var createDto = new CreateNotificationDto()
            {
                VesselId = vessel.Id.AsGuid(),
                RepresentativeId = "REP013",
                LoadingManifests = loading,
                UnloadingManifests = unloading,
                Crew = null
            };

            var dto = await service.CreateAsync(createDto);
            var stored = await notifRepo.GetByIdAsync(new VesselVisitNotificationID(dto.Id));

            // Forçar estado Completed
            var statusProp = typeof(VesselVisitNotification).GetProperty("Status", BindingFlags.Instance | BindingFlags.Public);
            statusProp.SetValue(stored, NotificationStatus.Completed);

            Func<Task> act = async () => await service.ApproveAsync(stored.Id.AsGuid(), "", "OfficerD");

            await Assert.ThrowsAsync<BusinessRuleValidationException>(act);
            uow.CommitCallCount.Should().Be(1);
        }


        [Fact]
        public async Task DecisionOnAlreadyReviewedNotification_PreventsDuplicateDecisions()
        {
            var vesselRepo = new InMemoryVesselRepository();
            var vesselTypeRepo = new InMemoryVesselTypeRepository();
            var notifRepo = new InMemoryVesselVisitNotificationRepository();
            var uow = new InMemoryUnitOfWork();
            var rep = new InMemoryRepresentativeRepository();
            
            var vesselType = new VesselType("TypeI", "desc", 5000, 1, 1, 1);
            await vesselTypeRepo.AddAsync(vesselType);
            var vessel = new Vessel("IMO1212121", "Already Reviewed", vesselType.Id, "Owner", "Operator");
            await vesselRepo.AddAsync(vessel);

            var service = new VesselVisitNotificationService(uow, notifRepo, vesselRepo, vesselTypeRepo,rep);

            var loading = new List<CargoManifest>{ CreateManifestWithContainer(100) };
            var unloading = new List<CargoManifest>{ CreateManifestWithContainer(50) };
            
            var representative = new Representative(
                "John Doe",
                "REP014",
                "PT",
                "john12311.doe@gmail.com",
                "+351923818777"
            );
           
            await rep.AddAsync(representative);
            
            var createDto = new CreateNotificationDto()
            {
                VesselId = vessel.Id.AsGuid(),
                RepresentativeId = "REP014",
                LoadingManifests = loading,
                UnloadingManifests = unloading,
                Crew = null
            };
            
            var dto = await service.CreateAsync(createDto);
            var stored = await notifRepo.GetByIdAsync(new VesselVisitNotificationID(dto.Id));

            // set to Completed and approve
            var statusProp = typeof(VesselVisitNotification).GetProperty("Status", BindingFlags.Instance | BindingFlags.Public);
            statusProp.SetValue(stored, NotificationStatus.Completed);

            var approved = await service.ApproveAsync(stored.Id.AsGuid(), "D9", "OfficerE");
            approved.Status.Should().Be(NotificationStatus.Approved);

            // Attempting to approve again should fail because status is no longer Completed
            Func<Task> actApproveAgain = async () => await service.ApproveAsync(stored.Id.AsGuid(), "D9", "OfficerE");
            await Assert.ThrowsAsync<BusinessRuleValidationException>(actApproveAgain);

            // Attempting to reject after approval should also fail
            Func<Task> actRejectAfter = async () => await service.RejectAsync(stored.Id.AsGuid(), "some reason", "OfficerF");
            await Assert.ThrowsAsync<BusinessRuleValidationException>(actRejectAfter);

            uow.CommitCallCount.Should().Be(2);
        }
        
        
        [Fact]
        public async Task UpdateInProgress_TotalWeightExceedsCapacity_Throws()
        {
        var vesselRepo = new InMemoryVesselRepository();
        var vesselTypeRepo = new InMemoryVesselTypeRepository();
        var notifRepo = new InMemoryVesselVisitNotificationRepository();
        var uow = new InMemoryUnitOfWork();
        var rep = new InMemoryRepresentativeRepository();
        
        // VesselType com capacidade pequena (200 kg)
        var vesselType = new VesselType("TypeC", "desc", 200, 1, 1, 1);
        await vesselTypeRepo.AddAsync(vesselType);

        var vessel = new Vessel("IMO3333333", "Update Vessel", vesselType.Id, "Owner", "Operator");
        await vesselRepo.AddAsync(vessel);

        var service = new VesselVisitNotificationService(uow, notifRepo, vesselRepo, vesselTypeRepo,rep);

        // Cria notificação inicial
        var loading = new List<CargoManifest> { CreateManifestWithContainer(10) };
        var unloading = new List<CargoManifest> { CreateManifestWithContainer(20) };
        
        var representative = new Representative(
            "John Doe",
            "REP004",
            "PT",
            "john123111.doe@gmail.com",
            "+351923818177"
        );
           
        await rep.AddAsync(representative);
        
        var createDto = new CreateNotificationDto()
        {
            VesselId = vessel.Id.AsGuid(),
            RepresentativeId = "REP004",
            LoadingManifests = loading,
            UnloadingManifests = unloading,
            Crew = null
        };
        
        var dto = await service.CreateAsync(createDto);
        var stored = await notifRepo.GetByIdAsync(new VesselVisitNotificationID(dto.Id));

        // DTO de atualização com peso que excede capacidade (usa IDs válidos ISO 6346)
        var updateDto = new UpdateNotificationDto
        {
            VesselId = vessel.Id.AsString(),
            UnloadingCargo = new UnloadingCargoMaterialDTO
            {
                Manifests = new List<CargoManifestDto>
                {
                    new CargoManifestDto
                    {
                        Containers = new List<ContainerDTO>
                        {
                            new ContainerDTO
                            {
                                Id = "ABCD1234567", // ID válido ISO 6346
                                PayloadWeight = 1000, // Excede a capacidade
                                ContentsDescription = "Too Heavy"
                            }
                        }
                    }
                }
            }
        };

        // Act
        Func<Task> act = async () => await service.UpdateInProgressAsync(stored.Id.AsGuid(), updateDto);

        // Assert → deve lançar exceção porque ultrapassa capacidade
        await Assert.ThrowsAsync<BusinessRuleValidationException>(act);

        // Commit não deve ser chamado novamente (update falhou)
        uow.CommitCallCount.Should().Be(1);
        }
        

        [Fact]
        public async Task SearchNotifications_FilterByVessel_ReturnsMatch()
        {
            var vesselRepo = new InMemoryVesselRepository();
            var vesselTypeRepo = new InMemoryVesselTypeRepository();
            var notifRepo = new InMemoryVesselVisitNotificationRepository();
            var uow = new InMemoryUnitOfWork();
            var rep = new InMemoryRepresentativeRepository();
            
            var vesselType = new VesselType("TypeD", "desc", 10000, 1, 1, 1);
            await vesselTypeRepo.AddAsync(vesselType);
            var vessel1 = new Vessel("IMO4444444", "Search Vessel A", vesselType.Id, "Owner", "Operator");
            var vessel2 = new Vessel("IMO5555555", "Search Vessel B", vesselType.Id, "Owner", "Operator");
            await vesselRepo.AddAsync(vessel1);
            await vesselRepo.AddAsync(vessel2);

            var service = new VesselVisitNotificationService(uow, notifRepo, vesselRepo, vesselTypeRepo,rep);

            var loading = new List<CargoManifest>{ CreateManifestWithContainer(10) };
            var unloading = new List<CargoManifest>{ CreateManifestWithContainer(5) };
            var representative = new Representative(
                "John Doe",
                "REP005",
                "PT",
                "john1231111.doe@gmail.com",
                "+351923818117"
            );
            
            await rep.AddAsync(representative);
            var representative2 = new Representative(
                "John Doe",
                "REP006",
                "PT",
                "john1231112.doe@gmail.com",
                "+351923818171"
            );
           
            await rep.AddAsync(representative2);
            var createDto = new CreateNotificationDto()
            {
                VesselId = vessel1.Id.AsGuid(),
                RepresentativeId = "REP005",
                LoadingManifests = loading,
                UnloadingManifests = unloading,
                Crew = null
            };
            var createDto2 = new CreateNotificationDto()
            {
                VesselId = vessel2.Id.AsGuid(),
                RepresentativeId = "REP006",
                LoadingManifests = loading,
                UnloadingManifests = unloading,
                Crew = null
            };
            var dto1 = await service.CreateAsync(createDto);
            var dto2 = await service.CreateAsync(createDto2);

            var filter = new NotificationFilterDto { VesselId = vessel1.Id.AsGuid() };
            var results = await service.SearchNotificationsAsync(filter);

            results.Should().ContainSingle(r => r.VesselId == vessel1.Id.AsGuid());
        }
    }
    
}
