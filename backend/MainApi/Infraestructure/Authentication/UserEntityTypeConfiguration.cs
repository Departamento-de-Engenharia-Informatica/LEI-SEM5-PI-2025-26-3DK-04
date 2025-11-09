using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.Authentication;

namespace DDDSample1.Infrastructure.Authentication
{
    internal class UserEntityTypeConfiguration : IEntityTypeConfiguration<User>
    {
        public void Configure(EntityTypeBuilder<User> builder)
        {
            builder.HasKey(b => b.Id);

            builder.Property(p => p.Id)
                .HasConversion(
                    id => id.AsString(),          // Para o banco (string email)
                    value => new UserID(value) // Para a aplicação
                );
        }
    }
}