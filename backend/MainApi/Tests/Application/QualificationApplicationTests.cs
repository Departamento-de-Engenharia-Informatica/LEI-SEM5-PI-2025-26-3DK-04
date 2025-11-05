using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using FluentAssertions;
using Xunit;
using DDDSample1.Domain.Qualifications;
using DDDSample1.Domain.Shared;

namespace DDDNetCore.Tests.Application
{
	public class QualificationApplicationTests
	{
		// Fake in-memory repository for unit tests
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
				// no-op for in-memory
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
		public async Task CreateQualification_CreatesAndReturnsDto()
		{
			var repo = new InMemoryQualificationRepository();
			var uow = new InMemoryUnitOfWork();
			var service = new QualificationService(uow, repo);

			var dto = new CreateQualificationDto { Name = "Docker Operator" };

			var result = await service.CreateAsync(dto);

			result.Should().NotBeNull();
			result.Name.Should().Be("Docker Operator");
			// repository should contain one item
			var all = await repo.GetAllAsync();
			all.Should().ContainSingle().Which.Name.Should().Be("Docker Operator");
			uow.CommitCallCount.Should().Be(1);
		}

		[Fact]
		public async Task SearchByName_ReturnsMatches()
		{
			var repo = new InMemoryQualificationRepository();
			var uow = new InMemoryUnitOfWork();
			var service = new QualificationService(uow, repo);

			// seed
			await repo.AddAsync(new Qualification("Crane Operator"));
			await repo.AddAsync(new Qualification("Truck Driver"));

			var results = await service.SearchByNameAsync("crane");

			results.Should().HaveCount(1);
			results.First().Name.Should().Contain("Crane");
		}

		[Fact]
		public async Task CreateQualification_InvalidName_ThrowsBusinessRule()
		{
			var repo = new InMemoryQualificationRepository();
			var uow = new InMemoryUnitOfWork();
			var service = new QualificationService(uow, repo);

			var dto = new CreateQualificationDto { Name = "SingleWord" };

			Func<Task> act = async () => await service.CreateAsync(dto);

			await Assert.ThrowsAsync<BusinessRuleValidationException>(act);
			// no commit should have happened
			uow.CommitCallCount.Should().Be(0);
		}

		[Fact]
		public async Task UpdateQualification_ChangesName()
		{
			var repo = new InMemoryQualificationRepository();
			var uow = new InMemoryUnitOfWork();
			var service = new QualificationService(uow, repo);

			var entity = new Qualification("Old Name Test");
			await repo.AddAsync(entity);
			var id = entity.Id.AsGuid();

			var updateDto = new UpdateQualificationDto { Name = "New Name Test" };
			var result = await service.UpdateAsync(id, updateDto);

			result.Should().NotBeNull();
			result.Id.Should().Be(id);
			result.Name.Should().Be("New Name Test");
			uow.CommitCallCount.Should().Be(1);

			var stored = await repo.GetByIdAsync(new QualificationID(id));
			stored.Should().NotBeNull();
			stored.Name.Should().Be("New Name Test");
		}

		[Fact]
		public async Task DeleteQualification_RemovesEntity()
		{
			var repo = new InMemoryQualificationRepository();
			var uow = new InMemoryUnitOfWork();
			var service = new QualificationService(uow, repo);

			var entity = new Qualification("To Delete Test");
			await repo.AddAsync(entity);
			var id = entity.Id.AsGuid();

			await service.DeleteAsync(id);

			uow.CommitCallCount.Should().Be(1);
			var stored = await repo.GetByIdAsync(new QualificationID(id));
			stored.Should().BeNull();
		}
	}
}
