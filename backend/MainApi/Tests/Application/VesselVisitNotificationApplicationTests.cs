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

            public Task<List<VesselVisitNotification>> GetSubmittedNotificationsAsync()
            {
                // Tests expect completed/ready-for-review notifications collection; current domain uses 'Submitted' as that state,
                // so return items with NotificationStatus.Submitted.
                var found = _items.Where(x => x.Status == NotificationStatus.Submitted).ToList();
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

            // set status to Submitted via reflection (domain tests do the same)
            var statusProp = typeof(VesselVisitNotification).GetProperty("Status", BindingFlags.Instance | BindingFlags.Public);
            statusProp.SetValue(stored, NotificationStatus.Submitted);

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

            // set to Submitted to allow approval
            var statusProp = typeof(VesselVisitNotification).GetProperty("Status", BindingFlags.Instance | BindingFlags.Public);
            statusProp.SetValue(stored, NotificationStatus.Submitted);

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

            // set to Submitted to allow rejection
            var statusProp = typeof(VesselVisitNotification).GetProperty("Status", BindingFlags.Instance | BindingFlags.Public);
            statusProp.SetValue(stored, NotificationStatus.Submitted);

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

            // set to Submitted to allow rejection
            var statusProp = typeof(VesselVisitNotification).GetProperty("Status", BindingFlags.Instance | BindingFlags.Public);
            statusProp.SetValue(stored, NotificationStatus.Submitted);

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
            statusProp.SetValue(stored, NotificationStatus.Submitted);

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

            // set to Submitted and approve
            var statusProp = typeof(VesselVisitNotification).GetProperty("Status", BindingFlags.Instance | BindingFlags.Public);
            statusProp.SetValue(stored, NotificationStatus.Submitted);

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

        // ===== US 2.2.10 Tests: Search and Filter Notifications =====
        
        [Fact]
        public async Task SearchNotifications_WithoutFilters_ReturnsAllNotifications()
        {
            // Arrange
            var vesselRepo = new InMemoryVesselRepository();
            var vesselTypeRepo = new InMemoryVesselTypeRepository();
            var notifRepo = new InMemoryVesselVisitNotificationRepository();
            var uow = new InMemoryUnitOfWork();
            var rep = new InMemoryRepresentativeRepository();
            
            var vesselType = new VesselType("TypeSearch1", "desc", 10000, 1, 1, 1);
            await vesselTypeRepo.AddAsync(vesselType);
            var vessel = new Vessel("IMO1111111", "Search Test Vessel", vesselType.Id, "Owner", "Operator");
            await vesselRepo.AddAsync(vessel);

            var representative = new Representative("Rep Name", "REP100", "PT", "rep100@gmail.com", "+351911111100");
            await rep.AddAsync(representative);

            var service = new VesselVisitNotificationService(uow, notifRepo, vesselRepo, vesselTypeRepo, rep);

            var loading = new List<CargoManifest>{ CreateManifestWithContainer(10) };
            var createDto1 = new CreateNotificationDto()
            {
                VesselId = vessel.Id.AsGuid(),
                RepresentativeId = "REP100",
                LoadingManifests = loading,
                UnloadingManifests = null,
                Crew = null
            };
            var createDto2 = new CreateNotificationDto()
            {
                VesselId = vessel.Id.AsGuid(),
                RepresentativeId = "REP100",
                LoadingManifests = loading,
                UnloadingManifests = null,
                Crew = null
            };
            
            await service.CreateAsync(createDto1);
            await service.CreateAsync(createDto2);

            // Act
            var filter = new NotificationFilterDto(); // No filters
            var results = await service.SearchNotificationsAsync(filter);

            // Assert
            results.Should().HaveCount(2);
            results.Should().OnlyContain(n => n.RepresentativeId == "REP100");
        }

        [Fact]
        public async Task SearchNotifications_FilterByStatus_ReturnsMatchingNotifications()
        {
            // Arrange
            var vesselRepo = new InMemoryVesselRepository();
            var vesselTypeRepo = new InMemoryVesselTypeRepository();
            var notifRepo = new InMemoryVesselVisitNotificationRepository();
            var uow = new InMemoryUnitOfWork();
            var rep = new InMemoryRepresentativeRepository();
            
            var vesselType = new VesselType("TypeSearch2", "desc", 10000, 1, 1, 1);
            await vesselTypeRepo.AddAsync(vesselType);
            var vessel = new Vessel("IMO2222222", "Status Test Vessel", vesselType.Id, "Owner", "Operator");
            await vesselRepo.AddAsync(vessel);

            var representative = new Representative("Rep Name", "REP101", "PT", "rep101@gmail.com", "+351911111101");
            await rep.AddAsync(representative);

            var service = new VesselVisitNotificationService(uow, notifRepo, vesselRepo, vesselTypeRepo, rep);

            var loading = new List<CargoManifest>{ CreateManifestWithContainer(10) };
            var createDto = new CreateNotificationDto()
            {
                VesselId = vessel.Id.AsGuid(),
                RepresentativeId = "REP101",
                LoadingManifests = loading,
                UnloadingManifests = null,
                Crew = null
            };
            
            var dto1 = await service.CreateAsync(createDto);
            var notification1 = await notifRepo.GetByIdAsync(new VesselVisitNotificationID(dto1.Id));
            
            // Set one to Completed using reflection
            var statusProp = typeof(VesselVisitNotification).GetProperty("Status", BindingFlags.Instance | BindingFlags.Public);
            statusProp.SetValue(notification1, NotificationStatus.Completed);

            // Create another that stays InProgress
            var dto2 = await service.CreateAsync(createDto);

            // Act
            var filter = new NotificationFilterDto { Status = NotificationStatus.Completed };
            var results = await service.SearchNotificationsAsync(filter);

            // Assert
            results.Should().ContainSingle();
            results.First().Id.Should().Be(dto1.Id);
            results.First().Status.Should().Be(NotificationStatus.Completed);
        }

        [Fact]
        public async Task SearchNotifications_FilterByRepresentative_ReturnsOnlyTheirNotifications()
        {
            // Arrange
            var vesselRepo = new InMemoryVesselRepository();
            var vesselTypeRepo = new InMemoryVesselTypeRepository();
            var notifRepo = new InMemoryVesselVisitNotificationRepository();
            var uow = new InMemoryUnitOfWork();
            var rep = new InMemoryRepresentativeRepository();
            
            var vesselType = new VesselType("TypeSearch3", "desc", 10000, 1, 1, 1);
            await vesselTypeRepo.AddAsync(vesselType);
            var vessel = new Vessel("IMO3333334", "Rep Filter Vessel", vesselType.Id, "Owner", "Operator");
            await vesselRepo.AddAsync(vessel);

            var representative1 = new Representative("Rep One", "REP102", "PT", "rep102@gmail.com", "+351911111102");
            var representative2 = new Representative("Rep Two", "REP103", "PT", "rep103@gmail.com", "+351911111103");
            await rep.AddAsync(representative1);
            await rep.AddAsync(representative2);

            var service = new VesselVisitNotificationService(uow, notifRepo, vesselRepo, vesselTypeRepo, rep);

            var loading = new List<CargoManifest>{ CreateManifestWithContainer(10) };
            var createDto1 = new CreateNotificationDto()
            {
                VesselId = vessel.Id.AsGuid(),
                RepresentativeId = "REP102",
                LoadingManifests = loading,
                UnloadingManifests = null,
                Crew = null
            };
            var createDto2 = new CreateNotificationDto()
            {
                VesselId = vessel.Id.AsGuid(),
                RepresentativeId = "REP103",
                LoadingManifests = loading,
                UnloadingManifests = null,
                Crew = null
            };
            
            await service.CreateAsync(createDto1);
            await service.CreateAsync(createDto2);

            // Act
            var filter = new NotificationFilterDto { RepresentativeId = "REP102" };
            var results = await service.SearchNotificationsAsync(filter);

            // Assert
            results.Should().ContainSingle();
            results.First().RepresentativeId.Should().Be("REP102");
        }

        [Fact]
        public async Task SearchNotifications_FilterByDateRange_ReturnsNotificationsInRange()
        {
            // Arrange
            var vesselRepo = new InMemoryVesselRepository();
            var vesselTypeRepo = new InMemoryVesselTypeRepository();
            var notifRepo = new InMemoryVesselVisitNotificationRepository();
            var uow = new InMemoryUnitOfWork();
            var rep = new InMemoryRepresentativeRepository();
            
            var vesselType = new VesselType("TypeSearch4", "desc", 10000, 1, 1, 1);
            await vesselTypeRepo.AddAsync(vesselType);
            var vessel = new Vessel("IMO4444445", "Date Filter Vessel", vesselType.Id, "Owner", "Operator");
            await vesselRepo.AddAsync(vessel);

            var representative = new Representative("Rep Name", "REP104", "PT", "rep104@gmail.com", "+351911111104");
            await rep.AddAsync(representative);

            var service = new VesselVisitNotificationService(uow, notifRepo, vesselRepo, vesselTypeRepo, rep);

            var loading = new List<CargoManifest>{ CreateManifestWithContainer(10) };
            var createDto = new CreateNotificationDto()
            {
                VesselId = vessel.Id.AsGuid(),
                RepresentativeId = "REP104",
                LoadingManifests = loading,
                UnloadingManifests = null,
                Crew = null
            };
            
            // Create notifications
            var dto1 = await service.CreateAsync(createDto);
            await Task.Delay(100); // Small delay to ensure different timestamps
            var dto2 = await service.CreateAsync(createDto);

            // Act - Search for notifications created today
            var filter = new NotificationFilterDto 
            { 
                StartDate = DateTime.UtcNow.Date,
                EndDate = DateTime.UtcNow.Date.AddDays(1)
            };
            var results = await service.SearchNotificationsAsync(filter);

            // Assert
            results.Should().HaveCount(2);
            results.Should().Contain(n => n.Id == dto1.Id);
            results.Should().Contain(n => n.Id == dto2.Id);
        }

        [Fact]
        public async Task SearchNotifications_CombinedFilters_ReturnsMatchingNotifications()
        {
            // Arrange
            var vesselRepo = new InMemoryVesselRepository();
            var vesselTypeRepo = new InMemoryVesselTypeRepository();
            var notifRepo = new InMemoryVesselVisitNotificationRepository();
            var uow = new InMemoryUnitOfWork();
            var rep = new InMemoryRepresentativeRepository();
            
            var vesselType = new VesselType("TypeSearch5", "desc", 10000, 1, 1, 1);
            await vesselTypeRepo.AddAsync(vesselType);
            var vessel1 = new Vessel("IMO5555556", "Combined Filter Vessel 1", vesselType.Id, "Owner", "Operator");
            var vessel2 = new Vessel("IMO6666666", "Combined Filter Vessel 2", vesselType.Id, "Owner", "Operator");
            await vesselRepo.AddAsync(vessel1);
            await vesselRepo.AddAsync(vessel2);

            var representative = new Representative("Rep Name", "REP105", "PT", "rep105@gmail.com", "+351911111105");
            await rep.AddAsync(representative);

            var service = new VesselVisitNotificationService(uow, notifRepo, vesselRepo, vesselTypeRepo, rep);

            var loading = new List<CargoManifest>{ CreateManifestWithContainer(10) };
            
            // Create notification for vessel1 with status InProgress
            var createDto1 = new CreateNotificationDto()
            {
                VesselId = vessel1.Id.AsGuid(),
                RepresentativeId = "REP105",
                LoadingManifests = loading,
                UnloadingManifests = null,
                Crew = null
            };
            var dto1 = await service.CreateAsync(createDto1);
            
            // Create notification for vessel2 with status Completed
            var createDto2 = new CreateNotificationDto()
            {
                VesselId = vessel2.Id.AsGuid(),
                RepresentativeId = "REP105",
                LoadingManifests = loading,
                UnloadingManifests = null,
                Crew = null
            };
            var dto2 = await service.CreateAsync(createDto2);
            var notification2 = await notifRepo.GetByIdAsync(new VesselVisitNotificationID(dto2.Id));
            
            // Set notification2 to Completed
            var statusProp = typeof(VesselVisitNotification).GetProperty("Status", BindingFlags.Instance | BindingFlags.Public);
            statusProp.SetValue(notification2, NotificationStatus.Completed);

            // Act - Search for vessel1 AND InProgress status
            var filter = new NotificationFilterDto 
            { 
                VesselId = vessel1.Id.AsGuid(),
                Status = NotificationStatus.InProgress
            };
            var results = await service.SearchNotificationsAsync(filter);

            // Assert - Should only return the notification for vessel1 with InProgress status
            results.Should().ContainSingle();
            results.First().Id.Should().Be(dto1.Id);
            results.First().VesselId.Should().Be(vessel1.Id.AsGuid());
            results.First().Status.Should().Be(NotificationStatus.InProgress);
        }

        [Fact]
        public async Task SearchNotifications_NoMatchingFilters_ReturnsEmptyList()
        {
            // Arrange
            var vesselRepo = new InMemoryVesselRepository();
            var vesselTypeRepo = new InMemoryVesselTypeRepository();
            var notifRepo = new InMemoryVesselVisitNotificationRepository();
            var uow = new InMemoryUnitOfWork();
            var rep = new InMemoryRepresentativeRepository();
            
            var vesselType = new VesselType("TypeSearch6", "desc", 10000, 1, 1, 1);
            await vesselTypeRepo.AddAsync(vesselType);
            var vessel = new Vessel("IMO7777778", "Empty Search Vessel", vesselType.Id, "Owner", "Operator");
            await vesselRepo.AddAsync(vessel);

            var representative = new Representative("Rep Name", "REP106", "PT", "rep106@gmail.com", "+351911111106");
            await rep.AddAsync(representative);

            var service = new VesselVisitNotificationService(uow, notifRepo, vesselRepo, vesselTypeRepo, rep);

            var loading = new List<CargoManifest>{ CreateManifestWithContainer(10) };
            var createDto = new CreateNotificationDto()
            {
                VesselId = vessel.Id.AsGuid(),
                RepresentativeId = "REP106",
                LoadingManifests = loading,
                UnloadingManifests = null,
                Crew = null
            };
            
            await service.CreateAsync(createDto);

            // Act - Search for notifications with status Approved (none exist)
            var filter = new NotificationFilterDto { Status = NotificationStatus.Approved };
            var results = await service.SearchNotificationsAsync(filter);

            // Assert
            results.Should().BeEmpty();
        }

        [Fact]
        public async Task SearchNotifications_FilterByStartDateOnly_ReturnsNotificationsAfterDate()
        {
            // Arrange
            var vesselRepo = new InMemoryVesselRepository();
            var vesselTypeRepo = new InMemoryVesselTypeRepository();
            var notifRepo = new InMemoryVesselVisitNotificationRepository();
            var uow = new InMemoryUnitOfWork();
            var rep = new InMemoryRepresentativeRepository();
            
            var vesselType = new VesselType("TypeSearch8", "desc", 10000, 1, 1, 1);
            await vesselTypeRepo.AddAsync(vesselType);
            var vessel = new Vessel("IMO9999990", "Start Date Vessel", vesselType.Id, "Owner", "Operator");
            await vesselRepo.AddAsync(vessel);

            var representative = new Representative("Rep Name", "REP108", "PT", "rep108@gmail.com", "+351911111108");
            await rep.AddAsync(representative);

            var service = new VesselVisitNotificationService(uow, notifRepo, vesselRepo, vesselTypeRepo, rep);

            var loading = new List<CargoManifest>{ CreateManifestWithContainer(10) };
            var createDto = new CreateNotificationDto()
            {
                VesselId = vessel.Id.AsGuid(),
                RepresentativeId = "REP108",
                LoadingManifests = loading,
                UnloadingManifests = null,
                Crew = null
            };
            
            var dto1 = await service.CreateAsync(createDto);

            // Act - Search for notifications created from today onwards
            var filter = new NotificationFilterDto { StartDate = DateTime.UtcNow.Date };
            var results = await service.SearchNotificationsAsync(filter);

            // Assert
            results.Should().NotBeEmpty();
            results.Should().Contain(n => n.Id == dto1.Id);
            results.Should().OnlyContain(n => n.CreatedAt >= DateTime.UtcNow.Date);
        }

        [Fact]
        public async Task SearchNotifications_FilterByEndDateOnly_ReturnsNotificationsBeforeDate()
        {
            // Arrange
            var vesselRepo = new InMemoryVesselRepository();
            var vesselTypeRepo = new InMemoryVesselTypeRepository();
            var notifRepo = new InMemoryVesselVisitNotificationRepository();
            var uow = new InMemoryUnitOfWork();
            var rep = new InMemoryRepresentativeRepository();
            
            var vesselType = new VesselType("TypeSearch9", "desc", 10000, 1, 1, 1);
            await vesselTypeRepo.AddAsync(vesselType);
            var vessel = new Vessel("IMO0000001", "End Date Vessel", vesselType.Id, "Owner", "Operator");
            await vesselRepo.AddAsync(vessel);

            var representative = new Representative("Rep Name", "REP109", "PT", "rep109@gmail.com", "+351911111109");
            await rep.AddAsync(representative);

            var service = new VesselVisitNotificationService(uow, notifRepo, vesselRepo, vesselTypeRepo, rep);

            var loading = new List<CargoManifest>{ CreateManifestWithContainer(10) };
            var createDto = new CreateNotificationDto()
            {
                VesselId = vessel.Id.AsGuid(),
                RepresentativeId = "REP109",
                LoadingManifests = loading,
                UnloadingManifests = null,
                Crew = null
            };
            
            var dto1 = await service.CreateAsync(createDto);

            // Act - Search for notifications created up to tomorrow
            var filter = new NotificationFilterDto { EndDate = DateTime.UtcNow.Date.AddDays(2) };
            var results = await service.SearchNotificationsAsync(filter);

            // Assert
            results.Should().NotBeEmpty();
            results.Should().Contain(n => n.Id == dto1.Id);
            results.Should().OnlyContain(n => n.CreatedAt <= DateTime.UtcNow.Date.AddDays(2));
        }

        [Fact]
        public async Task SearchNotifications_FilterByMultipleStatuses_ReturnsMatchingNotifications()
        {
            // Arrange
            var vesselRepo = new InMemoryVesselRepository();
            var vesselTypeRepo = new InMemoryVesselTypeRepository();
            var notifRepo = new InMemoryVesselVisitNotificationRepository();
            var uow = new InMemoryUnitOfWork();
            var rep = new InMemoryRepresentativeRepository();
            
            var vesselType = new VesselType("TypeSearch10", "desc", 10000, 1, 1, 1);
            await vesselTypeRepo.AddAsync(vesselType);
            var vessel = new Vessel("IMO1010102", "Multi Status Vessel", vesselType.Id, "Owner", "Operator");
            await vesselRepo.AddAsync(vessel);

            var representative = new Representative("Rep Name", "REP110", "PT", "rep110@gmail.com", "+351911111110");
            await rep.AddAsync(representative);

            var service = new VesselVisitNotificationService(uow, notifRepo, vesselRepo, vesselTypeRepo, rep);

            var loading = new List<CargoManifest>{ CreateManifestWithContainer(10) };
            var createDto = new CreateNotificationDto()
            {
                VesselId = vessel.Id.AsGuid(),
                RepresentativeId = "REP110",
                LoadingManifests = loading,
                UnloadingManifests = null,
                Crew = null
            };
            
            // Create InProgress notification
            var dto1 = await service.CreateAsync(createDto);
            
            // Create and set to Completed
            var dto2 = await service.CreateAsync(createDto);
            var notification2 = await notifRepo.GetByIdAsync(new VesselVisitNotificationID(dto2.Id));
            var statusProp = typeof(VesselVisitNotification).GetProperty("Status", BindingFlags.Instance | BindingFlags.Public);
            statusProp.SetValue(notification2, NotificationStatus.Submitted);
            
            // Create and set to Approved
            var dto3 = await service.CreateAsync(createDto);
            var notification3 = await notifRepo.GetByIdAsync(new VesselVisitNotificationID(dto3.Id));
            statusProp.SetValue(notification3, NotificationStatus.Submitted);
            await service.ApproveAsync(dto3.Id, "Dock1", "Officer1");

            // Act - Search for InProgress notifications
            var filterInProgress = new NotificationFilterDto { Status = NotificationStatus.InProgress };
            var resultsInProgress = await service.SearchNotificationsAsync(filterInProgress);

            // Assert
            resultsInProgress.Should().ContainSingle();
            resultsInProgress.First().Status.Should().Be(NotificationStatus.InProgress);

            // Act - Search for Approved notifications
            var filterApproved = new NotificationFilterDto { Status = NotificationStatus.Approved };
            var resultsApproved = await service.SearchNotificationsAsync(filterApproved);

            // Assert
            resultsApproved.Should().ContainSingle();
            resultsApproved.First().Status.Should().Be(NotificationStatus.Approved);
        }

        [Fact]
        public async Task SearchNotifications_ApprovedStatus_IncludesDockAssignment()
        {
            // Arrange
            var vesselRepo = new InMemoryVesselRepository();
            var vesselTypeRepo = new InMemoryVesselTypeRepository();
            var notifRepo = new InMemoryVesselVisitNotificationRepository();
            var uow = new InMemoryUnitOfWork();
            var rep = new InMemoryRepresentativeRepository();
            
            var vesselType = new VesselType("TypeSearch11", "desc", 10000, 1, 1, 1);
            await vesselTypeRepo.AddAsync(vesselType);
            var vessel = new Vessel("IMO1111103", "Approved Dock Vessel", vesselType.Id, "Owner", "Operator");
            await vesselRepo.AddAsync(vessel);

            var representative = new Representative("Rep Name", "REP111", "PT", "rep111@gmail.com", "+351911111111");
            await rep.AddAsync(representative);

            var service = new VesselVisitNotificationService(uow, notifRepo, vesselRepo, vesselTypeRepo, rep);

            var loading = new List<CargoManifest>{ CreateManifestWithContainer(10) };
            var createDto = new CreateNotificationDto()
            {
                VesselId = vessel.Id.AsGuid(),
                RepresentativeId = "REP111",
                LoadingManifests = loading,
                UnloadingManifests = null,
                Crew = null
            };
            
            var dto = await service.CreateAsync(createDto);
            var notification = await notifRepo.GetByIdAsync(new VesselVisitNotificationID(dto.Id));
            
            // Set to Completed and approve
            var statusProp = typeof(VesselVisitNotification).GetProperty("Status", BindingFlags.Instance | BindingFlags.Public);
            statusProp.SetValue(notification, NotificationStatus.Submitted);
            await service.ApproveAsync(dto.Id, "DockA", "OfficerX");

            // Act
            var filter = new NotificationFilterDto { Status = NotificationStatus.Approved };
            var results = await service.SearchNotificationsAsync(filter);

            // Assert
            results.Should().ContainSingle();
            results.First().Status.Should().Be(NotificationStatus.Approved);
            results.First().AssignedDock.Should().Be("DockA");
            results.First().OfficerId.Should().Be("OfficerX");
            results.First().DecisionOutcome.Should().Be("Approved");
        }

        [Fact]
        public async Task SearchNotifications_RejectedStatus_IncludesRejectionReason()
        {
            // Arrange
            var vesselRepo = new InMemoryVesselRepository();
            var vesselTypeRepo = new InMemoryVesselTypeRepository();
            var notifRepo = new InMemoryVesselVisitNotificationRepository();
            var uow = new InMemoryUnitOfWork();
            var rep = new InMemoryRepresentativeRepository();
            
            var vesselType = new VesselType("TypeSearch12", "desc", 10000, 1, 1, 1);
            await vesselTypeRepo.AddAsync(vesselType);
            var vessel = new Vessel("IMO1212104", "Rejected Reason Vessel", vesselType.Id, "Owner", "Operator");
            await vesselRepo.AddAsync(vessel);

            var representative = new Representative("Rep Name", "REP112", "PT", "rep112@gmail.com", "+351911111112");
            await rep.AddAsync(representative);

            var service = new VesselVisitNotificationService(uow, notifRepo, vesselRepo, vesselTypeRepo, rep);

            var loading = new List<CargoManifest>{ CreateManifestWithContainer(10) };
            var createDto = new CreateNotificationDto()
            {
                VesselId = vessel.Id.AsGuid(),
                RepresentativeId = "REP112",
                LoadingManifests = loading,
                UnloadingManifests = null,
                Crew = null
            };
            
            var dto = await service.CreateAsync(createDto);
            var notification = await notifRepo.GetByIdAsync(new VesselVisitNotificationID(dto.Id));
            
            // Set to Completed and reject
            var statusProp = typeof(VesselVisitNotification).GetProperty("Status", BindingFlags.Instance | BindingFlags.Public);
            statusProp.SetValue(notification, NotificationStatus.Submitted);
            await service.RejectAsync(dto.Id, "Insufficient documentation", "OfficerY");

            // Act
            var filter = new NotificationFilterDto { Status = NotificationStatus.Rejected };
            var results = await service.SearchNotificationsAsync(filter);

            // Assert
            results.Should().ContainSingle();
            results.First().Status.Should().Be(NotificationStatus.Rejected);
            results.First().RejectedReason.Should().Be("Insufficient documentation");
            results.First().OfficerId.Should().Be("OfficerY");
            results.First().DecisionOutcome.Should().Be("Rejected");
        }
    }
    
}
