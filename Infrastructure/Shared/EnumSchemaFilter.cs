using System;
using System.Linq;
using Microsoft.OpenApi.Any;
using Microsoft.OpenApi.Models;
using Swashbuckle.AspNetCore.SwaggerGen;

namespace DDDSample1.Infrastructure.Shared
{
    /// <summary>
    /// Filtro para o Swagger exibir enums como strings em vez de números
    /// </summary>
    public class EnumSchemaFilter : ISchemaFilter
    {
        public void Apply(OpenApiSchema schema, SchemaFilterContext context)
        {
            if (context.Type.IsEnum)
            {
                schema.Enum.Clear();
                
                // Adiciona os nomes dos valores do enum como strings
                foreach (var enumValue in Enum.GetValues(context.Type))
                {
                    schema.Enum.Add(new OpenApiString(enumValue.ToString()));
                }
                
                schema.Type = "string";
                schema.Format = null;
            }
        }
    }
}
