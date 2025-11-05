using System;
using System.Linq;
using Microsoft.OpenApi.Any;
using Microsoft.OpenApi.Models;
using Swashbuckle.AspNetCore.SwaggerGen;

namespace DDDSample1.Infrastructure.Shared
{
    /// <summary>
    /// Filtro para o Swagger exibir enums em parâmetros de query como strings em vez de números
    /// </summary>
    public class EnumParameterFilter : IParameterFilter
    {
        public void Apply(OpenApiParameter parameter, ParameterFilterContext context)
        {
            var type = context.ParameterInfo?.ParameterType;
            
            // Verifica se é enum ou enum nullable
            if (type == null) return;
            
            var underlyingType = Nullable.GetUnderlyingType(type) ?? type;
            
            if (underlyingType.IsEnum)
            {
                parameter.Schema.Enum.Clear();
                
                // Adiciona os nomes dos valores do enum como strings
                foreach (var enumValue in Enum.GetValues(underlyingType))
                {
                    parameter.Schema.Enum.Add(new OpenApiString(enumValue.ToString()));
                }
                
                parameter.Schema.Type = "string";
                parameter.Schema.Format = null;
            }
        }
    }
}
