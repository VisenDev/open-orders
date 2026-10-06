const CACHE = "open-orders-v1";

const STATIC = [
    "/",
    // "/orders.css",
    "/icon-128.png",
    "/icon-512.png"
];

self.addEventListener("install", event => {
    event.waitUntil(
        caches.open(CACHE)
            .then(cache => cache.addAll(STATIC))
    );
});

self.addEventListener("fetch", event => {
    if (event.request.method !== "GET")
        return;

    event.respondWith(
        fetch(event.request)
            .then(response => {
                const copy = response.clone();

                caches.open(CACHE)
                    .then(cache => cache.put(event.request, copy));

                return response;
            })
            .catch(() => caches.match(event.request))
    );
});
