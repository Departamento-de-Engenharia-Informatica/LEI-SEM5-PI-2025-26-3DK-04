using System;

namespace DDDSample1.Domain.StaffMembers
{
    public class OperationalWindowDto
    {
        // Represented as strings in "HH:mm" format for JSON friendliness
        public string StartTime { get; set; }
        public string EndTime { get; set; }

        public OperationalWindowDto()
        {
        }

        public OperationalWindowDto(string startTime, string endTime)
        {
            StartTime = startTime;
            EndTime = endTime;
        }

        // Allow assigning a string like "08:00-16:00" directly to an OperationalWindowDto property
        public static implicit operator OperationalWindowDto(string s)
        {
            if (s == null) return null;

            var parts = s.Split('-');
            if (parts.Length != 2)
                throw new ArgumentException("OperationalWindow string must be in format 'HH:mm-HH:mm'.");

            return new OperationalWindowDto(parts[0].Trim(), parts[1].Trim());
        }

        public static OperationalWindowDto FromDomain(OperationalWindow ow)
        {
            if (ow == null) return null;
            return new OperationalWindowDto(
                ow.StartTime.ToString(@"hh\:mm"),
                ow.EndTime.ToString(@"hh\:mm")
            );
        }

        public OperationalWindow ToDomain()
        {
            if (string.IsNullOrWhiteSpace(StartTime) || string.IsNullOrWhiteSpace(EndTime))
                throw new ArgumentException("StartTime and EndTime must be provided in OperationalWindowDto.");

            var window = $"{StartTime}-{EndTime}";
            return new OperationalWindow(window);
        }
    }
}
