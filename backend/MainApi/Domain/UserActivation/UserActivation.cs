using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Authentication
{
    public class UserActivation : Entity<UserActivationID>, IAggregateRoot
    {
        public UserID UserId { get; private set; }
        public string Token { get; private set; }
        public DateTime CreatedAt { get; private set; }
        public DateTime ExpiresAt { get; private set; }

        private UserActivation() {}

        public UserActivation(UserID userId)
        {
            Id = new UserActivationID(Guid.NewGuid());
            UserId = userId;
            Token = Guid.NewGuid().ToString("N");
            CreatedAt = DateTime.UtcNow;
            ExpiresAt = CreatedAt.AddHours(24); // token válido por 24h
        }

        public bool IsValid(string token)
        {
            return Token == token && DateTime.UtcNow <= ExpiresAt;
        }
    }
}