using System;
using System.Collections.Generic;
using System.Net.Http;
using System.Threading.Tasks;
using Microsoft.Extensions.Configuration;

namespace IartiApi.Services
{
    /// <summary>
    /// Service responsible for communicating with the Prolog server
    /// </summary>
    public class PrologService
    {
        private readonly HttpClient _httpClient;
        private readonly string _prologBaseUrl;

        public PrologService(HttpClient httpClient, IConfiguration configuration)
        {
            _httpClient = httpClient;
            _prologBaseUrl = configuration["PrologApi:BaseUrl"] ?? "http://localhost:5000";
        }

        /// <summary>
        /// Tests the connection to the Prolog server
        /// </summary>
        public async Task<string> TestConnectionAsync()
        {
            var response = await _httpClient.GetAsync($"{_prologBaseUrl}/lapr5");
            response.EnsureSuccessStatusCode();
            return await response.Content.ReadAsStringAsync();
        }

        /// <summary>
        /// Registers a user via the Prolog server (example endpoint)
        /// </summary>
        public async Task<string> RegisterUserAsync(string name, string sex, int birthYear)
        {
            var formData = new FormUrlEncodedContent(new[]
            {
                new KeyValuePair<string, string>("name", name),
                new KeyValuePair<string, string>("sex", sex),
                new KeyValuePair<string, string>("birth_year", birthYear.ToString())
            });

            var response = await _httpClient.PostAsync($"{_prologBaseUrl}/register_user", formData);
            response.EnsureSuccessStatusCode();
            return await response.Content.ReadAsStringAsync();
        }

        /// <summary>
        /// Sends a file to the Prolog server (example endpoint)
        /// </summary>
        public async Task<string> SendFileAsync(string fileContent)
        {
            var formData = new FormUrlEncodedContent(new[]
            {
                new KeyValuePair<string, string>("file", fileContent)
            });

            var response = await _httpClient.PostAsync($"{_prologBaseUrl}/send_file_post", formData);
            response.EnsureSuccessStatusCode();
            return await response.Content.ReadAsStringAsync();
        }
    }
}
