// LLM Generated Code
//
// Because I cannot be bothered to write javascript

document.addEventListener("DOMContentLoaded", () => {
    const form = document.querySelector("form");
    if (!form) return;

    let dirty = false;

    form.addEventListener("input", () => {
        dirty = true;
    });

    form.addEventListener("change", () => {
        dirty = true;
    });

    form.addEventListener("submit", () => {
        dirty = false;
    });

    window.addEventListener("beforeunload", (event) => {
        if (dirty) {
            event.preventDefault();
            event.returnValue = "";
        }
    });
});
