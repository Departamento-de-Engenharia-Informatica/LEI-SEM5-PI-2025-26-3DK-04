using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Authentication
{
    public class User : Entity<UserID>, IAggregateRoot
    {
        public string Name { get; private set; }
        public string Picture { get; private set; }
        public Roles Role { get; set; }

        public User(string email, string name, string picture, Roles role) 
        {
            this.Id = new UserID(email);
            this.Name = name;
            this.Picture = picture;
            this.Role = role;
        }

        public User(string email, string name, string picture) 
        {
            this.Id = new  UserID(email);
            this.Name = name;
            this.Picture = picture;
            Role = Roles.Unknown;
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
    }
}

public enum Roles
{
    Unknown,
    Admin
}