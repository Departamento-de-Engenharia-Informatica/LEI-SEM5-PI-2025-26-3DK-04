using System;
using System.Text.RegularExpressions;
using Newtonsoft.Json;
using DDDSample1.Domain.Shared;


namespace DDDSample1.Domain.Organizations
{
    public class OrganizationId : EntityId
    {
        [JsonConstructor]
        public OrganizationId(string value) : base(ValidateAndReturn(value))
        {
        }

        private static string ValidateAndReturn(string value)
        {
            if (string.IsNullOrWhiteSpace(value))
                throw new BusinessRuleValidationException("Organization ID cannot be empty.");

            if (value.Length > 10)
                throw new BusinessRuleValidationException("Organization ID must be at most 10 characters.");

            if (!IsAlphanumeric(value))
                throw new BusinessRuleValidationException("Organization ID must be alphanumeric (letters and numbers only).");

            return value;
        }

        private static bool IsAlphanumeric(string value)
        {
            return Regex.IsMatch(value, @"^[a-zA-Z0-9]+$");
        }

        protected override object createFromString(string text)
        {
            // Validar aqui mas retornar a string diretamente, não criar um novo OrganizationId
            return ValidateAndReturn(text);
        }

        public override string AsString()
        {
            return (string)base.ObjValue;
        }
        
    }
}