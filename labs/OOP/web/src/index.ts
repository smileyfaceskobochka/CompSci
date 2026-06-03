import { serve } from "bun";
import index from "./index.html";

const BACKENDS: Record<string, { port: number; name: string }> = {
  lab2: { port: 5070, name: "C# (.NET)" },
  lab3: { port: 8080, name: "Java (Spring Boot)" },
  lab4: { port: 5000, name: "Python + Go" },
};

const selected = process.env.BACKEND ?? "lab3";
const backend = BACKENDS[selected];
if (!backend) {
  console.error(`[ERROR] Unknown BACKEND "${selected}". Valid options: ${Object.keys(BACKENDS).join(", ")}`);
  process.exit(1);
}

const target = `http://localhost:${backend.port}`;

const server = serve({
  routes: {
    // Serve index.html for all unmatched static/page routes.
    "/*": index,

    // Proxy API requests to the selected backend
    "/api/*": async (req) => {
      const url = new URL(req.url);
      const targetUrl = new URL(url.pathname + url.search, target);

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
        console.error(`[Proxy Error] failed to connect to ${backend.name} backend:`, error);
        return Response.json({ error: `${backend.name} Backend is not running or unreachable` }, { status: 502 });
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
console.log(`[API] Proxying /api/* requests to ${target} (${backend.name})`);
