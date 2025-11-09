using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Authentication
{
    public class User : Entity<UserID>, IAggregateRoot
    {
        public string Name { get; private set; }
        public string Picture { get; private set; }
        public Roles Role { get; set; }
        public Status Status { get; set; }

        public User(string email, string name, string picture, Roles role,Status status)  
        {
            this.Id = new UserID(email);
            this.Name = name;
            this.Picture = picture;
            this.Role = role;
            this.Status = status;
        }

        public User(string email, string name, string picture) 
        {
            this.Id = new  UserID(email);
            this.Name = name;
            this.Picture = picture;
            Role = Roles.Unknown;
            Status = Status.Active;
        }

        private User() { }

        public UserID GetEmail()
        {
            return Id;
        }

        public string GetName()
        {
            return Name;
        }

        public string GetPicture()
        {
            return Picture;
        }

        public Roles GetRole()
        {
            return Role;
        }

        public void SetRole(Roles role)
        {
            this.Role = role;
        }
        public Status GetStatus()
        {
            return Status;
        }
        public void SetStatus(Status status)
        {
            this.Status = status;
        }
    }
}

public enum Roles
{
    Unknown,
    Admin,
    NoRole
}

public enum Status
{
    Active,
    Inactive
}