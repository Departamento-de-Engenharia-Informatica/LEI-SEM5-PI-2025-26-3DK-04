/*

using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using FluentAssertions;
using Xunit;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Organizations;
using DDDSample1.Domain.Authentication;


namespace DDDNetCore.Tests.Application
{
    public class RepresentativeServiceTests
    {
        // -----------------------------
        // InMemory Repositories & UoW
        // -----------------------------
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
                => Task.FromResult(_items.FirstOrDefault(o => o.Id.AsString() == id.AsString()));

            public Task<List<Organization>> GetByIdsAsync(List<OrganizationId> ids)
                => Task.FromResult(_items.Where(o => ids.Any(i => i.AsString() == o.Id.AsString())).ToList());

            public Task<Organization> GetByTaxNumberAsync(string taxNumber)
                => Task.FromResult(_items.FirstOrDefault(o => o.TaxNumber == taxNumber));

            public Task<bool> ExistsWithLegalNameAsync(string legalName)
                => Task.FromResult(_items.Any(o => o.LegalName.Equals(legalName, StringComparison.OrdinalIgnoreCase)));
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
                => Task.FromResult(_items.FirstOrDefault(r => r.Id.AsString() == id.AsString()));

            public Task<List<Representative>> GetByIdsAsync(List<RepresentativeId> ids)
            {
                var set = ids.Select(i => i.AsString()).ToHashSet();
                return Task.FromResult(_items.Where(r => set.Contains(r.Id.AsString())).ToList());
            }

            public Task<List<Representative>> GetActiveRepresentativesAsync()
                => Task.FromResult(_items.Where(r => r.Status == RepresentativeStatus.Active).ToList());

            public Task<List<Representative>> GetByOrganizationAsync(OrganizationId organizationId)
                => Task.FromResult(_items.Where(r => r.OrganizationId.AsString() == organizationId.AsString())
                    .ToList());

            public Task<Representative> GetByEmailAsync(string email)
                => Task.FromResult(_items.FirstOrDefault(r => r.Email == email));

            public Task<bool> ExistsWithEmailAsync(string email)
                => Task.FromResult(_items.Any(r => r.Email == email));

            public Task<bool> ExistsWithPhoneAsync(string phone)
                => Task.FromResult(_items.Any(r => r.PhoneNumber == phone));

            public Task<bool> ExistsWithCidAsync(string cid)
                => Task.FromResult(_items.Any(r => r.Id.AsString() == cid));

            public Task DeleteAsync(Representative rep)
            {
                _items.Remove(rep);
                return Task.CompletedTask;
            }
        }

        private class InMemoryUserRepository : IUserRepository
        {
            private readonly List<User> _users = new();

            public Task<List<User>> GetByIdsAsync(List<UserID> ids)
            {
                var set = ids.Select(i => i.Value).ToHashSet();
                return Task.FromResult(_users.Where(u => set.Contains(u.Id.Value)).ToList());
            }

            public Task<User> AddAsync(User user)
            {
                _users.Add(user);
                return Task.FromResult(user);
            }

            public void Remove(User user)
            {
                _users.Remove(user);
            }

            public Task<User> GetByIdAsync(UserID id)
                => Task.FromResult(_users.FirstOrDefault(u => u.Id.Value == id.Value));

            public Task<User> GetByEmailAsync(string email)
                => Task.FromResult(_users.FirstOrDefault(u => u.Id.Value == email));

            public Task UpdateAsync(User user)
            {
                // Em memória já está atualizado automaticamente
                return Task.CompletedTask;
            }

            public Task<List<User>> GetAllAsync() => Task.FromResult(_users.ToList());
        }

        private class InMemoryUnitOfWork : IUnitOfWork
        {
            public int CommitCount { get; private set; } = 0;

            public Task<int> CommitAsync()
            {
                CommitCount++;
                return Task.FromResult(1);
            }
        }


        private class InMemoryUserActivationRepository : IUserActivationRepository
        {
            private readonly List<UserActivation> _activations = new();

            public Task<List<UserActivation>> GetAllAsync() => Task.FromResult(_activations.ToList());

            public Task<UserActivation> GetByIdAsync(UserActivationID id)
                => Task.FromResult(_activations.FirstOrDefault(a => a.Id.Equals(id)));

            public Task<List<UserActivation>> GetByIdsAsync(List<UserActivationID> ids)
            {
                var set = ids.Select(i => i.Value).ToHashSet();
                return Task.FromResult(_activations.Where(a => set.Contains(a.Id.Value)).ToList());
            }

            public Task<UserActivation> AddAsync(UserActivation activation)
            {
                _activations.Add(activation);
                return Task.FromResult(activation);
            }

            public void Remove(UserActivation activation)
            {
                _activations.Remove(activation);
            }

            public Task UpdateAsync(UserActivation activation)
            {
                // Em memória já atualizado
                return Task.CompletedTask;
            }
            
            public Task DeleteAsync(UserActivation activation)
            {
                _activations.Remove(activation);
                return Task.CompletedTask;
            }

            public Task<UserActivation> GetByTokenAsync(string token)
            {
                return Task.FromResult(_activations.FirstOrDefault(a => a.Token == token));
            }
        }

        // Mock do EmailService para testes
        private class MockEmailService : EmailService
        {
            public List<string> SentEmails { get; } = new();

            public MockEmailService() : base()
            {
            }

            public new void SendActivationEmail(string toEmail, string activationLink)
            {
                // Apenas registra o email enviado sem realmente enviar
                SentEmails.Add($"{toEmail}|{activationLink}");
            }
        }

        private class InMemoryUserService : UserService
        {
            private readonly IUserRepository _userRepo;
            private readonly IUserActivationRepository _activationRepo;
            private readonly IUnitOfWork _unitOfWork;
            private readonly MockEmailService _testEmailService;

            public InMemoryUserService(
                IUserRepository userRepo,
                IUserActivationRepository activationRepo,
                IUnitOfWork unitOfWork,
                MockEmailService emailService)
                : base(unitOfWork, userRepo, activationRepo, emailService)
            {
                _userRepo = userRepo;
                _activationRepo = activationRepo;
                _unitOfWork = unitOfWork;
                _testEmailService = emailService;
            }

            public async Task<(bool Success, string Error)> CreateRepresentativeUserAsync(string email, string name)
            {
                if (string.IsNullOrWhiteSpace(email))
                    return (false, "Invalid email.");

                // 1. Criar o User
                var user = new User(email, name, "", Roles.Representative, Status.Inactive);
                await _userRepo.AddAsync(user);

                // 2. Criar token de ativação
                var activation = new UserActivation(user.Id);
                await _activationRepo.AddAsync(activation);

                // 3. Commit da UoW
                await _unitOfWork.CommitAsync();

                // 4. Enviar email
                if (_testEmailService != null)
                {
                    var link = $"https://localhost:5001/api/UserManagement/activate?token={activation.Token}";
                    _testEmailService.SendActivationEmail(email, link);
                }

                return (true, null);
            }
        }


        // ----------------------------------------
        // TESTES EXISTENTES
        // ----------------------------------------

        [Fact]
        public async Task AddRepresentative_AssignsToExistingOrganization()
        {
            var orgRepo = new InMemoryOrganizationRepository();
            var repRepo = new InMemoryRepresentativeRepository();
            var uow = new InMemoryUnitOfWork();
            var userRepo = new InMemoryUserRepository();
            var mockEmailService = new MockEmailService();
            var userService = new InMemoryUserService(userRepo, new InMemoryUserActivationRepository(), uow, mockEmailService);
            var service = new RepresentativeService(uow, repRepo, orgRepo, userRepo, userService);
            var org = new Organization("ORG10", "Harbor Corp", "Harbor", "Rua Central", "PT123456789");
            await orgRepo.AddAsync(org);


            var dto = new AddRepresentativeDto
            {
                Name = "João Costa",
                CitizenId = "CID900",
                Nationality = "PT",
                Email = "joao@gmail.com",
                PhoneNumber = "920000000",
                OrganizationId = org.Id.AsString()
            };

            var rep = await service.AddRepresentativeAsync(dto);

            rep.Should().NotBeNull();
            rep.OrganizationId.Should().Be("ORG10");
            rep.Status.Should().Be("Active");
            uow.CommitCount.Should().Be(2);
        }

        [Fact]
        public async Task AddRepresentative_DuplicateEmail_ThrowsException()
        {
            var orgRepo = new InMemoryOrganizationRepository();
            var repRepo = new InMemoryRepresentativeRepository();
            var uow = new InMemoryUnitOfWork();
            var userRepo = new InMemoryUserRepository();
            var mockEmailService = new MockEmailService();
            var userService = new InMemoryUserService(userRepo, new InMemoryUserActivationRepository(), uow, mockEmailService);
            var service = new RepresentativeService(uow, repRepo, orgRepo, userRepo, userService);

            var org = new Organization("ORG20", "PortCorp", "Port", "Rua 1", "PT123456710");
            await orgRepo.AddAsync(org);

            var dto1 = new AddRepresentativeDto
            {
                Name = "Ana Sousa",
                CitizenId = "CID100",
                Nationality = "PT",
                Email = "ana@gmail.com",
                PhoneNumber = "930000000",
                OrganizationId = "ORG20"
            };

            await service.AddRepresentativeAsync(dto1);

            var dto2 = new AddRepresentativeDto
            {
                Name = "Pedro Lima",
                CitizenId = "CID101",
                Nationality = "PT",
                Email = "ana@gmail.com", // duplicado
                PhoneNumber = "940000000",
                OrganizationId = "ORG20"
            };

            Func<Task> act = async () => await service.AddRepresentativeAsync(dto2);
            await Assert.ThrowsAsync<BusinessRuleValidationException>(act);
            uow.CommitCount.Should().Be(2);
        }

        // ----------------------------------------
        // NOVOS TESTES DE DUPLICAÇÃO
        // ----------------------------------------

        [Fact]
        public async Task AddRepresentative_DuplicatePhone_ThrowsException()
        {
            var orgRepo = new InMemoryOrganizationRepository();
            var repRepo = new InMemoryRepresentativeRepository();
            var uow = new InMemoryUnitOfWork();
            var userRepo = new InMemoryUserRepository();
            var mockEmailService = new MockEmailService();
            var userService = new InMemoryUserService(userRepo, new InMemoryUserActivationRepository(), uow, mockEmailService);
            var service = new RepresentativeService(uow, repRepo, orgRepo, userRepo, userService);

            var org = new Organization("ORG30", "PhoneCorp", "Phone", "Rua 2", "PT123456720");
            await orgRepo.AddAsync(org);

            var dto1 = new AddRepresentativeDto
            {
                Name = "Bruno Silva",
                CitizenId = "CID200",
                Nationality = "PT",
                Email = "bruno@gmail.com",
                PhoneNumber = "911111111",
                OrganizationId = "ORG30"
            };

            await service.AddRepresentativeAsync(dto1);

            var dto2 = new AddRepresentativeDto
            {
                Name = "Carla Torres",
                CitizenId = "CID201",
                Nationality = "PT",
                Email = "carla@gmail.com",
                PhoneNumber = "911111111", // telefone duplicado
                OrganizationId = "ORG30"
            };

            Func<Task> act = async () => await service.AddRepresentativeAsync(dto2);
            await Assert.ThrowsAsync<BusinessRuleValidationException>(act);
            uow.CommitCount.Should().Be(2);
        }

        [Fact]
        public async Task AddRepresentative_DuplicateCitizenId_ThrowsException()
        {
            var orgRepo = new InMemoryOrganizationRepository();
            var repRepo = new InMemoryRepresentativeRepository();
            var uow = new InMemoryUnitOfWork();
            var userRepo = new InMemoryUserRepository();
            var mockEmailService = new MockEmailService();
            var userService = new InMemoryUserService(userRepo, new InMemoryUserActivationRepository(), uow, mockEmailService);
            var service = new RepresentativeService(uow, repRepo, orgRepo, userRepo, userService);

            var org = new Organization("ORG40", "CidCorp", "Cid", "Rua 3", "PT123456730");
            await orgRepo.AddAsync(org);

            var dto1 = new AddRepresentativeDto
            {
                Name = "Rita Gomes",
                CitizenId = "CID300",
                Nationality = "PT",
                Email = "rita@gmail.com",
                PhoneNumber = "922222222",
                OrganizationId = "ORG40"
            };

            await service.AddRepresentativeAsync(dto1);

            var dto2 = new AddRepresentativeDto
            {
                Name = "Tiago Alves",
                CitizenId = "CID300", // citizen id duplicado
                Nationality = "PT",
                Email = "tiago@gmail.com",
                PhoneNumber = "933333333",
                OrganizationId = "ORG40"
            };

            Func<Task> act = async () => await service.AddRepresentativeAsync(dto2);
            await Assert.ThrowsAsync<BusinessRuleValidationException>(act);
            uow.CommitCount.Should().Be(2);
        }

        // ----------------------------------------
        // RESTANTES TESTES (sem alterações)
        // ----------------------------------------

        [Fact]
        public async Task UpdateRepresentative_ChangesNameAndPhone()
        {
            var orgRepo = new InMemoryOrganizationRepository();
            var repRepo = new InMemoryRepresentativeRepository();
            var uow = new InMemoryUnitOfWork();
            var userRepo = new InMemoryUserRepository();
            var mockEmailService = new MockEmailService();
            var userService = new InMemoryUserService(userRepo, new InMemoryUserActivationRepository(), uow, mockEmailService);
            var service = new RepresentativeService(uow, repRepo, orgRepo, userRepo, userService);

            var org = new Organization("ORG50", "ShipLine", "Ship", "Rua das Docas", "PT123456100");
            await orgRepo.AddAsync(org);

            var rep = new Representative("Carlos Ramos", "CID500", "PT", "carlos@gmail.com", "950000000");
            rep.AssignToOrganization(org.Id);
            await repRepo.AddAsync(rep);


            var update = new UpdateRepresentativeDto
            {
                Name = "Carlos R.",
                CitizenId = "CID500",
                Nationality = "PT",
                Email = "carlos@gmail.com",
                PhoneNumber = "955555555"
            };

            var updated = await service.UpdateRepresentativeAsync("CID500", update);

            updated.Name.Should().Be("Carlos R.");
            updated.PhoneNumber.Should().Be("955555555");
            uow.CommitCount.Should().Be(1);
        }

        [Fact]
        public async Task DeactivateRepresentative_SetsStatusToInactive()
        {
            var orgRepo = new InMemoryOrganizationRepository();
            var repRepo = new InMemoryRepresentativeRepository();
            var uow = new InMemoryUnitOfWork();
            var userRepo = new InMemoryUserRepository();
            var mockEmailService = new MockEmailService();
            var userService = new InMemoryUserService(userRepo, new InMemoryUserActivationRepository(), uow, mockEmailService);
            var service = new RepresentativeService(uow, repRepo, orgRepo, userRepo, userService);
            var org = new Organization("ORG60", "Oceanic", "Ocean", "Rua Azul", "PT123451000");
            await orgRepo.AddAsync(org);

            var rep = new Representative("Laura Mendes", "CID600", "PT", "laura@gmail.com", "960000000");
            rep.AssignToOrganization(org.Id);
            await repRepo.AddAsync(rep);


            var result = await service.DeactivateRepresentativeAsync("CID600");

            result.Status.Should().Be("Inactive");
            uow.CommitCount.Should().Be(1);
        }
    }
}
*/