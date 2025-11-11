using DDDSample1.Domain.Authentication;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace DDDSample1.Infrastructure.Authentication
{
    internal class UserActivationEntityTypeConfiguration : IEntityTypeConfiguration<UserActivation>
    {
        public void Configure(EntityTypeBuilder<UserActivation> builder)
        {
            // Nome da tabela
            builder.ToTable("UserActivations");

            // Chave primária
            builder.HasKey(a => a.Id);

            // Configuração do ID (UserActivationID)
            builder.Property(a => a.Id)
                .HasConversion(
                    id => id.AsString(),             // Para guardar na BD como string (GUID)
                    value => new UserActivationID(value)) // Para recriar o objeto
                .IsRequired();

            // Configuração do Email associado ao user
            builder.OwnsOne(a => a.UserId, email =>
            {
                email.Property(e => e.Value)
                    .HasColumnName("UserEmail")
                    .IsRequired();
            });

            // Token e Data de Expiração
            builder.Property(a => a.Token)
                .IsRequired()
                .HasMaxLength(100);

            builder.Property(a => a.ExpiresAt)
                .IsRequired();

            // Índice único no Token (garante que o token não se repete)
            builder.HasIndex(a => a.Token)
                .IsUnique();
        }
    }
}