using System;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.StaffMembers
{
    public class OperationalWindow : ValueObject
    {
        public TimeSpan StartTime { get; private set; }
        public TimeSpan EndTime { get; private set; }

        // Parameterless ctor for ORM
        private OperationalWindow()
        {
            StartTime = TimeSpan.Zero;
            EndTime = TimeSpan.Zero;
        }

        public OperationalWindow(TimeSpan startTime, TimeSpan endTime)
        {
            // Allow windows that cross midnight (e.g. 18:00-02:00). Do not require endTime >= startTime.
            StartTime = startTime;
            EndTime = endTime;
        }

        public OperationalWindow(string window)
        {
            if (string.IsNullOrWhiteSpace(window))
                throw new BusinessRuleValidationException("Operational window cannot be empty.");

            // Expect format "HH:mm-HH:mm"
            var parts = window.Split('-');
            if (parts.Length != 2)
                throw new BusinessRuleValidationException("Operational window must be in format 'HH:mm-HH:mm'.");

            try
            {
                var start = TimeSpan.Parse(parts[0]);
                var end = TimeSpan.Parse(parts[1]);

                // Allow windows that cross midnight (e.g. 18:00-02:00). Do not require end >= start.
                StartTime = start;
                EndTime = end;
            }
            catch (FormatException)
            {
                throw new BusinessRuleValidationException("Operational window times must be valid times in 'HH:mm' format.");
            }
        }

        public override string ToString()
        {
            return string.Format("{0:hh\\:mm}-{1:hh\\:mm}", StartTime, EndTime);
        }

        protected override IEnumerable<object> GetEqualityComponents()
        {
            yield return StartTime;
            yield return EndTime;
        }
    }
}
