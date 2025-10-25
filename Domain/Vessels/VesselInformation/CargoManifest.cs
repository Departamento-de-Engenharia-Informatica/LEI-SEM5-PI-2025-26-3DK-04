using System; 
using System.Collections.Generic;
using System.Linq;
using DDDSample1.Domain.Shared; 

namespace DDDSample1.Domain.Vessels.VesselInformation 
{
    public class CargoManifest : Entity<CargoManifestID>, IAggregateRoot 
    {
        
        private List<Container> _containers { get; set; }

       
        public IReadOnlyCollection<Container> Containers => _containers.AsReadOnly();

        
        private CargoManifest()
        {
            
             _containers = new List<Container>();
        }

       
        public CargoManifest(IEnumerable<Container> initialContainers)
        {
            
            this.Id = new CargoManifestID(Guid.NewGuid());

            
            if (initialContainers == null)
            {
                 
                throw new BusinessRuleValidationException("Initial container list cannot be null when creating a Cargo Manifest.");
            }
            var containerList = initialContainers.ToList();

            _containers = containerList;
        }

       
        public void AddContainer(Container container)
        {
            if (container == null)
            {
                throw new BusinessRuleValidationException("Cannot add a null container.");
            }
             // Check if container already exists
             // if (_containers.Any(c => c.Id.Equals(container.Id)))
             // {
             //    throw new BusinessRuleValidationException($"Container with ID {container.Id.AsString()} already exists in the manifest.");
             // }

            _containers.Add(container);
        }

       
        public void RemoveContainer(ContainerID containerId)
        {
             if (containerId == null)
            {
                throw new BusinessRuleValidationException("Container ID cannot be null.");
            }
            var containerToRemove = _containers.FirstOrDefault(c => c.Id.Equals(containerId));
            if (containerToRemove == null)
            {
                 throw new BusinessRuleValidationException($"Container with ID {containerId.AsString()} not found in the manifest.");
            }
            _containers.Remove(containerToRemove);
        }


        
        public double TotalWeightKg()
        {
            // Ensure _containers is not null before summing
            return _containers?.Sum(c => c.PayloadWeight) ?? 0;
        }
    }
}