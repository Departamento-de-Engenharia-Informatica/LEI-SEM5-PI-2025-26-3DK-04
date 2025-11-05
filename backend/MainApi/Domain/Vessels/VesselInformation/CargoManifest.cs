using System;
using System.Collections.Generic;
using System.Linq;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Vessels.VesselInformation
{
    public class CargoManifest : Entity<CargoManifestID>, IAggregateRoot
    {
        // EF Core precisa que a lista seja uma propriedade simples, não privada com {get;set;}
        private readonly List<Container> _containers = new();

        // Exposição apenas leitura
        public IReadOnlyCollection<Container> Containers => _containers.AsReadOnly();

        // Construtor privado exigido por EF Core
        private CargoManifest() { }

        // Construtor principal
        public CargoManifest(IEnumerable<Container> initialContainers)
        {
            Id = new CargoManifestID(Guid.NewGuid());

            if (initialContainers == null)
                throw new BusinessRuleValidationException("Initial container list cannot be null.");

            _containers = new List<Container>(initialContainers);
        }

        // Adicionar contentor
        public void AddContainer(Container container)
        {
            if (container == null)
                throw new BusinessRuleValidationException("Cannot add a null container.");

            // Verifica duplicado
            if (_containers.Any(c => c.Id.Equals(container.Id)))
                throw new BusinessRuleValidationException($"Container with ID {container.Id.AsString()} already exists.");

            _containers.Add(container);
        }

        // Remover contentor
        public void RemoveContainer(ContainerID containerId)
        {
            if (containerId == null)
                throw new BusinessRuleValidationException("Container ID cannot be null.");

            var containerToRemove = _containers.FirstOrDefault(c => c.Id.Equals(containerId));
            if (containerToRemove == null)
                throw new BusinessRuleValidationException($"Container with ID {containerId.AsString()} not found.");

            _containers.Remove(containerToRemove);
        }

        // Peso total
        public double TotalWeightKg()
        {
            return _containers.Sum(c => c.PayloadWeight);
        }
    }
}
