import { serve } from "bun";
import index from "./index.html";

const server = serve({
  routes: {
    // Serve index.html for all unmatched static/page routes.
    "/*": index,

    // Proxy API requests to the C# backend running on port 5070
    "/api/*": async (req) => {
      const url = new URL(req.url);
      const targetUrl = new URL(url.pathname + url.search, "http://localhost:5070");
      
      console.log(`[Proxy] ${req.method} ${url.pathname}${url.search} -> ${targetUrl.toString()}`);
      
      try {
        const body = req.method !== "GET" && req.method !== "HEAD" ? await req.blob() : undefined;
        
        const response = await fetch(targetUrl.toString(), {
          method: req.method,
          headers: req.headers,
          body: body,
        });
        
        return response;
      } catch (error) {
        console.error(`[Proxy Error] failed to connect to C# backend:`, error);
        return Response.json({ error: "C# Backend is not running or unreachable" }, { status: 502 });
      }
    },
  },

  development: process.env.NODE_ENV !== "production" && {
    // Enable browser hot reloading in development
    hmr: true,

    // Echo console logs from the browser to the server
    console: true,
  },
});

console.log(`[RUN] Frontend server running at ${server.url}`);
console.log(`[API] Proxying /api/* requests to http://localhost:5070`);
