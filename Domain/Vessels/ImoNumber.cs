using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Vessels
{
    /// <summary>
    /// Value Object representing a valid IMO (International Maritime Organization) number.
    /// IMO numbers consist of the three letters "IMO" followed by a seven-digit number.
    /// The seventh digit is a check digit calculated from the first six digits.
    /// </summary>
    public class ImoNumber : IValueObject
    {
        public string Value { get; private set; }

        // Required for EF Core
        private ImoNumber()
        {
        }

        public ImoNumber(string value)
        {
            if (string.IsNullOrWhiteSpace(value))
                throw new BusinessRuleValidationException("IMO number is required.");

            // Remove "IMO" prefix if present and trim whitespace
            var cleanedValue = value.Trim().ToUpper();
            if (cleanedValue.StartsWith("IMO"))
            {
                cleanedValue = cleanedValue.Substring(3).Trim();
            }

            // Validate format: must be 7 digits
            if (!IsValidFormat(cleanedValue))
                throw new BusinessRuleValidationException(
                    "IMO number must be 7 digits in the format: 1234567 (where the last digit is a check digit).");

            // Validate check digit
            if (!IsValidCheckDigit(cleanedValue))
                throw new BusinessRuleValidationException(
                    "Invalid IMO number: check digit validation failed.");

            this.Value = cleanedValue;
        }

        private bool IsValidFormat(string value)
        {
            return value.Length == 7 && long.TryParse(value, out _);
        }

        private bool IsValidCheckDigit(string imoNumber)
        {
            if (imoNumber.Length != 7)
                return false;

            // The check digit algorithm:
            // Multiply each of the first six digits by their position (7,6,5,4,3,2)
            // Sum the results and take modulo 10
            // The result should equal the seventh digit
            int sum = 0;
            for (int i = 0; i < 6; i++)
            {
                int digit = int.Parse(imoNumber[i].ToString());
                int multiplier = 7 - i; // Position weights: 7,6,5,4,3,2
                sum += digit * multiplier;
            }

            int checkDigit = sum % 10;
            int providedCheckDigit = int.Parse(imoNumber[6].ToString());

            return checkDigit == providedCheckDigit;
        }

        public override string ToString()
        {
            return $"IMO{Value}";
        }

        public override bool Equals(object obj)
        {
            if (obj == null || obj.GetType() != GetType())
                return false;

            var other = (ImoNumber)obj;
            return Value == other.Value;
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }

        public static bool operator ==(ImoNumber left, ImoNumber right)
        {
            if (ReferenceEquals(left, null) && ReferenceEquals(right, null))
                return true;
            if (ReferenceEquals(left, null) || ReferenceEquals(right, null))
                return false;
            return left.Equals(right);
        }

        public static bool operator !=(ImoNumber left, ImoNumber right)
        {
            return !(left == right);
        }
    }
}
