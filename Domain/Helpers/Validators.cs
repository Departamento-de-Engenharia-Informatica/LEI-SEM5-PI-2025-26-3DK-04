using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace DDDSample1.Domain.Shared
{
    public static class Validators
    {
        // Lista simplificada de códigos de países europeus ISO 3166-1 alpha-2
        private static readonly HashSet<string> EuropeanCountries = new()
        {
            "AT","BE","BG","CY","CZ","DE","DK","EE","ES","FI","FR","GR","HR",
            "HU","IE","IT","LT","LU","LV","MT","NL","PL","PT","RO","SE","SI","SK"
        };

        // Validação do Tax Number (para Organization)
        public static void ValidateTaxNumber(string taxNumber)
        {
            if (string.IsNullOrWhiteSpace(taxNumber))
                throw new BusinessRuleValidationException("Tax Number is required.");

            if (taxNumber.Length < 2)
                throw new BusinessRuleValidationException("Tax Number too short.");

            string countryCode = taxNumber.Substring(0, 2).ToUpper();

            if (!EuropeanCountries.Contains(countryCode))
                throw new BusinessRuleValidationException($"Country code '{countryCode}' is not a valid European country.");

            string numberPart = taxNumber.Substring(2);
            if (!Regex.IsMatch(numberPart, @"^[A-Z0-9]+$"))
                throw new BusinessRuleValidationException("Tax Number must be alphanumeric after the country code.");
        }

        // Validação do Citizen ID (para Representative)
        public static void ValidateCitizenId(string citizenId)
        {
            if (string.IsNullOrWhiteSpace(citizenId))
                throw new BusinessRuleValidationException("Citizen ID is required.");

            // Apenas exemplo de validação simples: alfanumérico e comprimento entre 5 e 20
            if (!Regex.IsMatch(citizenId, @"^[A-Z0-9]+$", RegexOptions.IgnoreCase))
                throw new BusinessRuleValidationException("Citizen ID must be alphanumeric.");

            if (citizenId.Length < 5 || citizenId.Length > 20)
                throw new BusinessRuleValidationException("Citizen ID must be between 5 and 20 characters.");
        }

        public static void ValidateEmail(string email)
        {
            if (string.IsNullOrWhiteSpace(email))
                throw new BusinessRuleValidationException("Email is required.");

            // Expressão regular para emails dos domínios permitidos
            var regex = new Regex(@"^[\w\.-]+@(gmail|hotmail|email)\.com$", RegexOptions.IgnoreCase);

            if (!regex.IsMatch(email))
                throw new BusinessRuleValidationException("Email must be from @gmail.com, @hotmail.com, or @email.com domains.");
        }

        public static void ValidatePhoneNumber(string phoneNumber)
        {
            if (string.IsNullOrWhiteSpace(phoneNumber))
                throw new BusinessRuleValidationException("Phone number is required.");

            var regex = new Regex(@"^\d{9}$");

            if (!regex.IsMatch(phoneNumber))
                throw new BusinessRuleValidationException("Phone number must have exactly 9 digits.");
        }

        // Validação do Name da Qualification
        // Name: free text with at least two words and a maximum length of 150
        public static void ValidateQualificationName(string name)
        {
            if (string.IsNullOrWhiteSpace(name))
                throw new BusinessRuleValidationException("Qualification name is required.");

            if (name.Length > 150)
                throw new BusinessRuleValidationException("Qualification name must have a maximum length of 150 characters.");

            // Verificar se tem pelo menos 2 palavras
            string[] words = name.Trim().Split(new[] { ' ', '\t', '\n', '\r' }, StringSplitOptions.RemoveEmptyEntries);
            if (words.Length < 2)
                throw new BusinessRuleValidationException("Qualification name must contain at least two words.");
        }
    }
}
