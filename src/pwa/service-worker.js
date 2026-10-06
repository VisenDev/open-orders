// LLM Generated Code
//
// Because I cannot be bothered to write javascript

const CACHE = "open-orders-v2";

const STATIC = [
    "/",
    "/icon-128.png",
    "/icon-512.png"
];

self.addEventListener("install", event => {
    event.waitUntil(
        caches.open(CACHE)
            .then(cache => cache.addAll(STATIC))
    );
});

self.addEventListener("activate", event => {
    event.waitUntil(
        caches.keys().then(keys =>
            Promise.all(
                keys.filter(key => key !== CACHE)
                    .map(key => caches.delete(key))
            )
        )
    );
});

self.addEventListener("fetch", event => {
    const request = event.request;
    const url = new URL(request.url);

    if (request.method !== "GET" ||
        url.origin !== self.location.origin)
        return;

    const staticResource =
        url.pathname === "/css" ||
        /\.(css|js|json|png)$/.test(url.pathname);

    event.respondWith(
        caches.open(CACHE).then(async cache => {
            if (staticResource) {
                // Cache-first
                const cached = await cache.match(request);
                if (cached) return cached;
            }

            try {
                const response = await fetch(request);

                if (response.ok) {
                    await cache.put(request, response.clone());
                }

                return response;
            } catch (error) {
                // Offline fallback
                const cached = await cache.match(request);
                if (cached) return cached;
                throw error;
            }
        })
    );
});
