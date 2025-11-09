using System;
using System.Collections.Generic;
using System.IdentityModel.Tokens.Jwt;
using System.Linq;
using System.Net.Http;
using System.Text.Json;
using System.Text.Json.Serialization;
using System.Threading.Tasks;
using Microsoft.Extensions.Configuration;

namespace DDDSample1.Domain.Authentication
{
    public class GoogleAuthService
    {
        private readonly IConfiguration _configuration;
        
        private readonly IUserRepository _repo;
        
        public GoogleAuthService(IConfiguration configuration,IUserRepository repo)
        {
            _configuration = configuration;
            _repo = repo;
        }

        public async Task<TokenResponse> ExchangeCodeForToken(string code)
        {
            var clientId = _configuration["GoogleAuth:ClientId"];
            var clientSecret = _configuration["GoogleAuth:ClientSecret"];
            var redirectUri = _configuration["GoogleAuth:RedirectUri"];

            using var client = new HttpClient();
            var values = new Dictionary<string, string>
            {
                { "code", code },
                { "client_id", clientId },
                { "client_secret", clientSecret },
                { "redirect_uri", redirectUri },
                { "grant_type", "authorization_code" }
            };
            var content = new FormUrlEncodedContent(values);
            var response = await client.PostAsync("https://oauth2.googleapis.com/token", content);
            response.EnsureSuccessStatusCode();
            var json = await response.Content.ReadAsStringAsync();
            return JsonSerializer.Deserialize<TokenResponse>(json);
        }
        
        public async Task<User> GetUser(TokenResponse tokenResponse)
        {
            
            var handler = new JwtSecurityTokenHandler();
            var token = handler.ReadJwtToken(tokenResponse.IdToken);
            
            var email = token.Claims.FirstOrDefault(c => c.Type == "email")?.Value;

            if (string.IsNullOrEmpty(email))
                throw new Exception("Couldn't  to obtain an email from token");
            
            var user = await _repo.GetByEmailAsync(email);
            
            if (user == null)
            {
                throw new Exception("User not on database");
            }

            return user;
        }
    }

    public class TokenResponse
    {
        [JsonPropertyName("id_token")]
        public string IdToken { get; set; }

        [JsonPropertyName("access_token")]
        public string AccessToken { get; set; }
    }
}