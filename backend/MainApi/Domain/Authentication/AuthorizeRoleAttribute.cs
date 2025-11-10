
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.Filters;
using System;
using System.Linq;
using Microsoft.AspNetCore.Http;


namespace DDDSample1.Domain.Authentication
{
    [AttributeUsage(AttributeTargets.Method | AttributeTargets.Class, AllowMultiple = true)]
    public class AuthorizeRoleAttribute : Attribute, IAuthorizationFilter
    {
        private readonly Roles[] _roles;

        public AuthorizeRoleAttribute(params Roles[] roles)
        {
            _roles = roles;
        }

        public void OnAuthorization(AuthorizationFilterContext context)
        {
            var userRole = context.HttpContext.User.FindFirst("role")?.Value;

            if (string.IsNullOrEmpty(userRole))
            {
                context.Result = new ForbidResult();
                return;
            }

            if (!_roles.Any(r => r.ToString().Equals(userRole, StringComparison.OrdinalIgnoreCase)))
            {
                
                Console.WriteLine($"[SECURITY] Unauthorized access attempt by role '{userRole}' on {context.HttpContext.Request.Path}");

                context.Result = new ObjectResult(new { error = "Access Denied" })
                {
                    StatusCode = StatusCodes.Status403Forbidden
                };
            }
        }
    }
}
