using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.Filters;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Logging;
using System;
using System.IdentityModel.Tokens.Jwt;
using System.Linq;
using System.Threading.Tasks;

namespace DDDSample1.Domain.Authentication
{
    [AttributeUsage(AttributeTargets.Method | AttributeTargets.Class, AllowMultiple = true)]
    public class AuthorizeRoleAttribute : Attribute, IAsyncAuthorizationFilter
    {
        private readonly Roles[] _roles;

        public AuthorizeRoleAttribute(params Roles[] roles)
        {
            _roles = roles;
        }

        public async Task OnAuthorizationAsync(AuthorizationFilterContext context)
        {
            var httpContext = context.HttpContext;
            var logger = (ILogger<AuthorizeRoleAttribute>)httpContext.RequestServices.GetService(typeof(ILogger<AuthorizeRoleAttribute>));

            // Ler token do header
            var authHeader = httpContext.Request.Headers["Authorization"].FirstOrDefault();
            if (string.IsNullOrEmpty(authHeader) || !authHeader.StartsWith("Bearer "))
            {
                logger?.LogWarning("Unauthorized request: missing or invalid Authorization header. Path: {Path}", httpContext.Request.Path);
                context.Result = new UnauthorizedResult(); // 401
                return;
            }

            var idToken = authHeader.Substring("Bearer ".Length).Trim();

            // Decodificar IdToken do Google
            JwtSecurityToken token;
            try
            {
                var handler = new JwtSecurityTokenHandler();
                token = handler.ReadJwtToken(idToken);
            }
            catch (Exception ex)
            {
                logger?.LogWarning(ex, "Unauthorized request: invalid JWT token. Path: {Path}", httpContext.Request.Path);
                context.Result = new UnauthorizedResult(); // 401
                return;
            }

            var email = token.Claims.FirstOrDefault(c => c.Type == "email")?.Value;
            if (string.IsNullOrEmpty(email))
            {
                logger?.LogWarning("Unauthorized request: token missing email claim. Path: {Path}", httpContext.Request.Path);
                context.Result = new UnauthorizedResult(); // 401
                return;
            }

            // Consultar user no DB
            var userRepo = (IUserRepository)httpContext.RequestServices.GetService(typeof(IUserRepository));
            var user = await userRepo.GetByEmailAsync(email);

            if (user == null || user.GetRole() == Roles.NoRole || user.GetStatus() == Status.Inactive)
            {
                logger?.LogWarning("Unauthorized request: user not found or inactive. Email: {Email}, Path: {Path}", email, httpContext.Request.Path);
                context.Result = new UnauthorizedResult(); // 401
                return;
            }

            // Verificar roles permitidos
            if (!_roles.Any(r => r == user.GetRole()))
            {
                logger?.LogWarning("Forbidden request: user role {Role} not allowed. Email: {Email}, Path: {Path}", user.GetRole(), email, httpContext.Request.Path);
                context.Result = new ObjectResult(new { error = "Access Denied" })
                {
                    StatusCode = StatusCodes.Status403Forbidden // 403
                };
                return;
            }

            // Popular HttpContext.User (opcional)
            var claims = token.Claims.ToList();
            var identity = new System.Security.Claims.ClaimsIdentity(claims, "Google");
            httpContext.User = new System.Security.Claims.ClaimsPrincipal(identity);
        }
    }
}
