const scrollKey = "scroll:" + location.pathname;

if (sessionStorage.getItem(scrollKey) !== null) {
    history.scrollRestoration = "manual";
}

document.addEventListener("DOMContentLoaded", () => {
    const saved = sessionStorage.getItem(scrollKey);

    if (saved !== null) {
        window.scrollTo(0, Number(saved));
        sessionStorage.removeItem(scrollKey);
    }
});

document.addEventListener("submit", event => {
    sessionStorage.setItem(scrollKey, window.scrollY);
}, true);
