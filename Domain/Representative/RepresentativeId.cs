using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Organizations
{
    public class RepresentativeId : EntityId
    {
        // Construtor a partir de Guid
        public RepresentativeId(Guid value) : base(value) { }

        // Construtor a partir de string
        public RepresentativeId(string value) : base(Guid.Parse(value)) { }

        // Retorna o Guid real do Value Object
        public Guid AsGuid()
        {
            // Se o EntityId base armazena Guid em ObjValue, faz cast seguro
            return (Guid)base.ObjValue;
        }

        // Converte para string
        public override string AsString()
        {
            return AsGuid().ToString();
        }

        // Sobrescreve o método abstrato da base
        protected override object createFromString(string text)
        {
            return new RepresentativeId(Guid.Parse(text));
        }
    }
}