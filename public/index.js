import { Elm } from "../src/Main.elm"

const seed = process.env.NODE_ENV === 'development' ? 0 : new Date().valueOf();

Elm.Main.init({
    node: document.getElementById("root"),
    flags: seed
});