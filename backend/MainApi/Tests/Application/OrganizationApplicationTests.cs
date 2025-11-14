using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using FluentAssertions;
using Xunit;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Organizations;

namespace DDDNetCore.Tests.Application
{
public class OrganizationServiceTests
{
// ------------------------------
// Repositórios em memória
// ------------------------------
private class InMemoryOrganizationRepository : IOrganizationRepository
{
private readonly List<Organization> _items = new();

        public Task<Organization> AddAsync(Organization obj)
        {
            _items.Add(obj);
            return Task.FromResult(obj);
        }

        public void Remove(Organization obj) => _items.Remove(obj);
        public Task UpdateAsync(Organization obj) => Task.CompletedTask;
        public Task<List<Organization>> GetAllAsync() => Task.FromResult(_items.ToList());

        public Task<Organization> GetByIdAsync(OrganizationId id)
        {
            var found = _items.FirstOrDefault(o => o.Id.AsString() == id.AsString());
            return Task.FromResult(found);
        }

        public Task<List<Organization>> GetByIdsAsync(List<OrganizationId> ids)
        {
            var set = ids.Select(i => i.AsString()).ToHashSet();
            var found = _items.Where(o => set.Contains(o.Id.AsString())).ToList();
            return Task.FromResult(found);
        }

        public Task<bool> GetByTaxNumberAsync(string taxNumber)
        {
            var found = _items.Any(o => o.TaxNumber == taxNumber);
            return Task.FromResult(found);
        }

        public Task<bool> ExistsWithLegalNameAsync(string legalName)
        {
            return Task.FromResult(_items.Any(o => o.LegalName.Equals(legalName, StringComparison.OrdinalIgnoreCase)));
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

        public Task<Representative> GetRepresentativeByEmailAsync(string email)
        {
            throw new NotImplementedException();
        }

        public Task<Representative> GetRepresentativeByCitizenCardAsync(string citizenCard)
        {
            throw new NotImplementedException();
        }

        public Task<Representative> GetRepresentativeByPhoneAsync(string phone)
        {
            throw new NotImplementedException();
        }
    }

    private class InMemoryUnitOfWork : IUnitOfWork
    {
        public int CommitCount { get; private set; }
        public Task<int> CommitAsync()
        {
            CommitCount++;
            return Task.FromResult(1);
        }
    }
    
    [Fact]
    public async Task RegisterOrganization_SuccessfullyCreatesOrganization()
    {
        var orgRepo = new InMemoryOrganizationRepository();
        var repRepo = new InMemoryRepresentativeRepository();
        var uow = new InMemoryUnitOfWork();

        var service = new OrganizationService(uow, orgRepo, repRepo);

        var dto = new OrganizationDto
        {
            Id = "ORG01",
            LegalName = "Ocean Trade Ltd.",
            AlternativeName = "OceanTrade",
            Address = "Rua do Porto 123",
            TaxNumber = "PT123456789",
            Representatives = new List<AddRepresentativeToOrgDto>
            {
                new AddRepresentativeToOrgDto
                {
                    Name = "Maria Silva",
                    CitizenId = "CID001",
                    Nationality = "PT",
                    Email = "maria@gmail.com",
                    PhoneNumber = "910000000"
                }
            }
        };

        var result = await service.RegisterOrganizationAsync(dto);

        result.Should().NotBeNull();
        result.LegalName.Should().Be("Ocean Trade Ltd.");
        result.Representatives.Should().ContainSingle(r => r.Name == "Maria Silva");
        uow.CommitCount.Should().Be(1);
    }

    [Fact]
    public async Task RegisterOrganization_DuplicateLegalName_ThrowsException()
    {
        var orgRepo = new InMemoryOrganizationRepository();
        var repRepo = new InMemoryRepresentativeRepository();
        var uow = new InMemoryUnitOfWork();

        var service = new OrganizationService(uow, orgRepo, repRepo);

        var dto = new OrganizationDto
        {
            Id = "ORG01",
            LegalName = "Ocean Trade Ltd.",
            AlternativeName = "OceanTrade",
            Address = "Rua do Porto 123",
            TaxNumber = "PT123454789",
            Representatives = new List<AddRepresentativeToOrgDto>
            {
                new AddRepresentativeToOrgDto
                {
                    Name = "Maria Silva",
                    CitizenId = "CID002",
                    Nationality = "PT",
                    Email = "maria2@gmail.com",
                    PhoneNumber = "920000000"
                }
            }
        };

        await service.RegisterOrganizationAsync(dto);

        var duplicate = new OrganizationDto
        {
            Id = "ORG01",
            LegalName = "Ocean Trade Ltd.",
            AlternativeName = "OceanTrade",
            Address = "Rua do Porto 123",
            TaxNumber = "PT123456789",
            Representatives = new List<AddRepresentativeToOrgDto>
            {
                new AddRepresentativeToOrgDto
                {
                    Name = "Maria Silva",
                    CitizenId = "CID001",
                    Nationality = "PT",
                    Email = "maria@gmail.com",
                    PhoneNumber = "910000000"
                }
            }
        };

        Func<Task> act = async () => await service.RegisterOrganizationAsync(duplicate);

        await Assert.ThrowsAsync<BusinessRuleValidationException>(act);
        uow.CommitCount.Should().Be(1);
    }
}

}
