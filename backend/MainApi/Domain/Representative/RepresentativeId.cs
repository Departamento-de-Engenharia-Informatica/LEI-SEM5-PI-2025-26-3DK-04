using System;
using System.Text.RegularExpressions;
using Newtonsoft.Json;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Organizations
{
    public class RepresentativeId : EntityId
    {
        [JsonConstructor]
        public RepresentativeId(string value) : base(ValidateAndReturn(value))
        {
        }

        private static string ValidateAndReturn(string value)
        {
            if (string.IsNullOrWhiteSpace(value))
                throw new BusinessRuleValidationException("Citizen ID cannot be empty.");

            // exemplo: validação de formato (ajusta conforme necessário)
            Validators.ValidateCitizenId(value);

            if (value.Length < 5 || value.Length > 20)
                throw new BusinessRuleValidationException("Citizen ID must be between 5 and 20 characters.");
            
            return value;
        }
        
        protected override object createFromString(string text)
        {
            // Validar aqui mas retornar a string diretamente
            return ValidateAndReturn(text);
        }

        public override string AsString()
        {
            return (string)base.ObjValue;
        }
    }
}