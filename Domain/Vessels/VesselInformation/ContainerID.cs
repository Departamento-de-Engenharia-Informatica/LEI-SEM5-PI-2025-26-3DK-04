using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Vessels.VesselInformation;

public class ContainerID : EntityId
{
    public ContainerID(Guid value) : base(value)
    {
    }
    public ContainerID(string value) : base(value)
    {
    }

    protected override object createFromString(string text)
    {
        if (!IsValid(text))
            throw new BusinessRuleValidationException("Invalid container ID format. Must follow ISO 6346:2022 standard.");
        
        return text.ToUpper();
    }

    public override string AsString()
    {
        return ObjValue.ToString();
    }

    private static bool IsValid(string value)
    {
        if (string.IsNullOrEmpty(value) || value.Length != 11)
            return false;

        // Check first 4 characters are letters
        for (int i = 0; i < 4; i++)
        {
            if (!char.IsLetter(value[i]))
                return false;
        }

        // Check next 6 characters are digits
        for (int i = 4; i < 10; i++)
        {
            if (!char.IsDigit(value[i]))
                return false;
        }

        // Validate check digit
        return ValidateCheckDigit(value);
    }

    private static bool ValidateCheckDigit(string value)
    {
        int sum = 0;
        for (int i = 0; i < 10; i++)
        {
            int charValue = value[i] >= 'A' ? value[i] - 'A' + 10 : value[i] - '0';
            sum += charValue * (int)Math.Pow(2, i);
        }
        int checkDigit = sum % 11 % 10;
        return (value[10] - '0') == checkDigit;
    }

    public Guid AsGuid()
    {
        return (Guid)base.ObjValue;
    }
}