using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using FluentAssertions;
using Xunit;
using DDDSample1.Domain.StaffMembers;
using DDDSample1.Domain.Qualifications;
using DDDSample1.Domain.Shared;

namespace DDDNetCore.Tests.Application
{
    public class StaffMemberApplicationTests
    {
        // In-memory staff repository
        private class InMemoryStaffMemberRepository : IStaffMemberRepository
        {
            private readonly List<StaffMember> _items = new List<StaffMember>();

            public Task<StaffMember> AddAsync(StaffMember obj)
            {
                _items.Add(obj);
                return Task.FromResult(obj);
            }

            public void Remove(StaffMember obj)
            {
                _items.Remove(obj);
            }

            public Task UpdateAsync(StaffMember obj)
            {
                // no-op for in-memory
                return Task.CompletedTask;
            }

            public Task<List<StaffMember>> GetAllAsync()
            {
                return Task.FromResult(_items.ToList());
            }

            public Task<StaffMember> GetByIdAsync(StaffMemberID id)
            {
                var found = _items.FirstOrDefault(x => x.Id.AsGuid() == id.AsGuid());
                return Task.FromResult(found);
            }

            public Task<List<StaffMember>> GetByIdsAsync(List<StaffMemberID> ids)
            {
                var guids = ids.Select(i => i.AsGuid()).ToHashSet();
                var found = _items.Where(x => guids.Contains(x.Id.AsGuid())).ToList();
                return Task.FromResult(found);
            }

            public Task<List<StaffMember>> GetByNameAsync(string name)
            {
                if (string.IsNullOrWhiteSpace(name))
                    return Task.FromResult(new List<StaffMember>());

                var lower = name.ToLowerInvariant();
                var found = _items.Where(x => x.Name != null && x.Name.ToLowerInvariant().Contains(lower)).ToList();
                return Task.FromResult(found);
            }

            public Task<List<StaffMember>> GetByStatusAsync(MemberStatus status)
            {
                var found = _items.Where(x => x.Status == status).ToList();
                return Task.FromResult(found);
            }

            public Task<List<StaffMember>> GetByQualificationAsync(Guid qualificationId)
            {
                var found = _items.Where(x => x.Qualifications != null && x.Qualifications.Any(q => q.Id.AsGuid() == qualificationId)).ToList();
                return Task.FromResult(found);
            }

            public Task<List<StaffMember>> SearchAsync(string name, MemberStatus? status, Guid? qualificationId)
            {
                var query = _items.AsEnumerable();
                if (!string.IsNullOrWhiteSpace(name))
                    query = query.Where(x => x.Name != null && x.Name.Contains(name, StringComparison.OrdinalIgnoreCase));
                if (status.HasValue)
                    query = query.Where(x => x.Status == status.Value);
                if (qualificationId.HasValue)
                    query = query.Where(x => x.Qualifications != null && x.Qualifications.Any(q => q.Id.AsGuid() == qualificationId.Value));

                return Task.FromResult(query.ToList());
            }

            public Task<List<StaffMember>> GetActiveStaffAsync()
            {
                var found = _items.Where(x => x.Status == MemberStatus.Available).ToList();
                return Task.FromResult(found);
            }

            public Task<List<StaffMember>> GetAllForAuditAsync()
            {
                return Task.FromResult(_items.ToList());
            }
        }

        // Minimal in-memory qualification repo used by the service
        private class InMemoryQualificationRepository : IQualificationRepository
        {
            private readonly List<Qualification> _items = new List<Qualification>();

            public Task<Qualification> AddAsync(Qualification obj)
            {
                _items.Add(obj);
                return Task.FromResult(obj);
            }

            public void Remove(Qualification obj)
            {
                _items.Remove(obj);
            }

            public Task UpdateAsync(Qualification obj)
            {
                return Task.CompletedTask;
            }

            public Task<List<Qualification>> GetAllAsync()
            {
                return Task.FromResult(_items.ToList());
            }

            public Task<Qualification> GetByIdAsync(QualificationID id)
            {
                var found = _items.FirstOrDefault(x => x.Id.AsGuid() == id.AsGuid());
                return Task.FromResult(found);
            }

            public Task<List<Qualification>> GetByIdsAsync(List<QualificationID> ids)
            {
                var guids = ids.Select(i => i.AsGuid()).ToHashSet();
                var found = _items.Where(x => guids.Contains(x.Id.AsGuid())).ToList();
                return Task.FromResult(found);
            }

            public Task<List<Qualification>> GetByNameAsync(string name)
            {
                if (string.IsNullOrWhiteSpace(name))
                    return Task.FromResult(new List<Qualification>());

                var lower = name.ToLowerInvariant();
                var found = _items.Where(x => x.Name != null && x.Name.ToLowerInvariant().Contains(lower)).ToList();
                return Task.FromResult(found);
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

        [Fact]
        public async Task CreateStaffMember_CreatesAndReturnsDto()
        {
            var repo = new InMemoryStaffMemberRepository();
            var qualRepo = new InMemoryQualificationRepository();
            var uow = new InMemoryUnitOfWork();
            var service = new StaffMemberService(uow, repo, qualRepo);

            var dto = new CreateStaffMemberDto { Name = "John Doe", Email = "john@example.com", PhoneNumber = 912345678, OperationalWindow = "08:00-17:00" };

            var result = await service.CreateAsync(dto);

            result.Should().NotBeNull();
            result.Name.Should().Be("John Doe");
            var all = await repo.GetAllAsync();
            all.Should().ContainSingle().Which.Name.Should().Be("John Doe");
            uow.CommitCallCount.Should().Be(1);
        }

        [Fact]
        public async Task SearchByName_ReturnsMatches()
        {
            var repo = new InMemoryStaffMemberRepository();
            var qualRepo = new InMemoryQualificationRepository();
            var uow = new InMemoryUnitOfWork();
            var service = new StaffMemberService(uow, repo, qualRepo);

            await repo.AddAsync(new StaffMember("Alice Smith", "alice@example.com", 912345670, "08:00-17:00"));
            await repo.AddAsync(new StaffMember("Bob Jones", "bob@example.com", 912345671, "08:00-17:00"));

            var results = await service.SearchByNameAsync("alice");

            results.Should().HaveCount(1);
            results.First().Name.Should().Contain("Alice");
        }

        [Fact]
        public async Task UpdateStaffMember_ChangesFields()
        {
            var repo = new InMemoryStaffMemberRepository();
            var qualRepo = new InMemoryQualificationRepository();
            var uow = new InMemoryUnitOfWork();
            var service = new StaffMemberService(uow, repo, qualRepo);

            var entity = new StaffMember("Old Name","old@example.com",912345672,"08:00-17:00");
            await repo.AddAsync(entity);
            var id = entity.Id.AsGuid();

            var updateDto = new UpdateStaffMemberDto { Name = "New Name", Email = "new@example.com", PhoneNumber = 912345679, OperationalWindow = "09:00-18:00", Status = MemberStatus.Available };

            var result = await service.UpdateAsync(id, updateDto);

            result.Should().NotBeNull();
            result.Id.Should().Be(id);
            result.Name.Should().Be("New Name");
            uow.CommitCallCount.Should().Be(1);

            var stored = await repo.GetByIdAsync(new StaffMemberID(id));
            stored.Should().NotBeNull();
            stored.Name.Should().Be("New Name");
            stored.Email.Should().Be("new@example.com");
        }

        [Fact]
        public async Task DeactivateAndReactivate_Works()
        {
            var repo = new InMemoryStaffMemberRepository();
            var qualRepo = new InMemoryQualificationRepository();
            var uow = new InMemoryUnitOfWork();
            var service = new StaffMemberService(uow, repo, qualRepo);

            var entity = new StaffMember("Active","act@example.com",912345673,"08:00-17:00");
            await repo.AddAsync(entity);
            var id = entity.Id.AsGuid();

            var deactivated = await service.DeactivateAsync(id);
            deactivated.Status.Should().Be(MemberStatus.Unavailable);
            uow.CommitCallCount.Should().Be(1);

            var reactivated = await service.ReactivateAsync(id);
            reactivated.Status.Should().Be(MemberStatus.Available);
            uow.CommitCallCount.Should().Be(2);
        }

        [Fact]
        public async Task AddAndRemoveQualification_Works()
        {
            var repo = new InMemoryStaffMemberRepository();
            var qualRepo = new InMemoryQualificationRepository();
            var uow = new InMemoryUnitOfWork();
            var service = new StaffMemberService(uow, repo, qualRepo);

            var staff = new StaffMember("Worker","worker@example.com",912345674,"08:00-17:00");
            await repo.AddAsync(staff);

            var qualification = new Qualification("Forklift Operator");
            await qualRepo.AddAsync(qualification);

            var staffId = staff.Id.AsGuid();
            var qualId = qualification.Id.AsGuid();

            var afterAdd = await service.AddQualificationAsync(staffId, qualId);
            afterAdd.Qualifications.Should().ContainSingle(q => q.Id == qualId);
            uow.CommitCallCount.Should().Be(1);

            var afterRemove = await service.RemoveQualificationAsync(staffId, qualId);
            afterRemove.Qualifications.Should().BeEmpty();
            uow.CommitCallCount.Should().Be(2);
        }
    }
}
