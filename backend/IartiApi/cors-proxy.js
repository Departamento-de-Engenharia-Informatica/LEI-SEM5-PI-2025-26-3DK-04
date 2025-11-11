const http = require('http');

const PROLOG_PORT = 5003; // Prolog vai correr aqui
const PROXY_PORT = 5002;  // Angular vai chamar este

const server = http.createServer((req, res) => {
  console.log(`${req.method} ${req.url}`);

  // Configurar CORS headers
  res.setHeader('Access-Control-Allow-Origin', '*');
  res.setHeader('Access-Control-Allow-Methods', 'GET, POST, OPTIONS');
  res.setHeader('Access-Control-Allow-Headers', '*');

  // Handle preflight
  if (req.method === 'OPTIONS') {
    res.writeHead(204);
    res.end();
    return;
  }

  // Proxy para Prolog
  const options = {
    hostname: 'localhost',
    port: PROLOG_PORT,
    path: req.url,
    method: req.method,
    headers: req.headers
  };

  const proxyReq = http.request(options, (proxyRes) => {
    res.writeHead(proxyRes.statusCode, proxyRes.headers);
    proxyRes.pipe(res);
  });

  proxyReq.on('error', (err) => {
    console.error('Proxy error:', err);
    res.writeHead(500);
    res.end('Proxy error');
  });

  req.pipe(proxyReq);
});

server.listen(PROXY_PORT, () => {
  console.log(`CORS Proxy running on port ${PROXY_PORT}`);
  console.log(`Forwarding to Prolog on port ${PROLOG_PORT}`);
});
