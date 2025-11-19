using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Logging;
using System;
using System.Linq;
using System.Net;
using System.Threading.Tasks;

namespace DDDNetCore.Middleware
{
    public class NetworkAuthorizationMiddleware
    {
        private readonly RequestDelegate _next;
        private readonly ILogger<NetworkAuthorizationMiddleware> _logger;
        private static readonly string[] AllowedNetworks = new[]
        {
            "10.0.0.0/8",           // DEI VPN
            "127.0.0.0/8",          // Localhost
            "::1",                  // IPv6 localhost
        };

        public NetworkAuthorizationMiddleware(
            RequestDelegate next,
            ILogger<NetworkAuthorizationMiddleware> logger)
        {
            _next = next;
            _logger = logger;
        }

        public async Task InvokeAsync(HttpContext context)
        {
            var remoteIp = context.Connection.RemoteIpAddress;

            // Skip check for development environment
            var environment = Environment.GetEnvironmentVariable("ASPNETCORE_ENVIRONMENT");
            if (environment == "Development")
            {
                await _next(context);
                return;
            }

            // Check if IP is allowed
            if (!IsIpAllowed(remoteIp))
            {
                // Log unauthorized attempt
                _logger.LogWarning(
                    "SECURITY ALERT: Unauthorized access attempt blocked. " +
                    "IP: {IpAddress}, Path: {Path}, Method: {Method}, Time: {Time}",
                    remoteIp,
                    context.Request.Path,
                    context.Request.Method,
                    DateTime.UtcNow
                );

                // Return 403 Forbidden
                context.Response.StatusCode = StatusCodes.Status403Forbidden;
                context.Response.ContentType = "application/json";
                await context.Response.WriteAsync(
                    "{\"error\":\"Access denied: Connection must be from DEI network or VPN\"}"
                );
                return;
            }

            // IP is allowed, continue
            await _next(context);
        }

        private bool IsIpAllowed(IPAddress ipAddress)
        {
            if (ipAddress == null)
                return false;

            // Check localhost
            if (IPAddress.IsLoopback(ipAddress))
                return true;

            // Check each allowed network
            foreach (var network in AllowedNetworks)
            {
                if (IsInSubnet(ipAddress, network))
                    return true;
            }

            return false;
        }

        private bool IsInSubnet(IPAddress address, string subnetMask)
        {
            var parts = subnetMask.Split('/');
            var baseAddress = IPAddress.Parse(parts[0]);
            var prefixLength = int.Parse(parts[1]);

            // Convert to bytes
            var addressBytes = address.GetAddressBytes();
            var baseBytes = baseAddress.GetAddressBytes();

            // Handle IPv4/IPv6 mismatch
            if (addressBytes.Length != baseBytes.Length)
                return false;

            // Calculate subnet mask
            var maskBytes = new byte[baseBytes.Length];
            var fullBytes = prefixLength / 8;
            var remainingBits = prefixLength % 8;

            // Set full bytes
            for (int i = 0; i < fullBytes; i++)
                maskBytes[i] = 0xFF;

            // Set remaining bits
            if (remainingBits > 0 && fullBytes < maskBytes.Length)
                maskBytes[fullBytes] = (byte)(0xFF << (8 - remainingBits));

            // Compare
            for (int i = 0; i < maskBytes.Length; i++)
            {
                if ((addressBytes[i] & maskBytes[i]) != (baseBytes[i] & maskBytes[i]))
                    return false;
            }

            return true;
        }
    }
}
