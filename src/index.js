import "./main.css";
import { Elm } from "./Main.elm";
import registerServiceWorker from "./registerServiceWorker";

const storageKey = "cache";

const app = Elm.Main.init({
  node: document.getElementById("root"),
  flags: localStorage.getItem(storageKey)
});

app.ports.store.subscribe(data => localStorage.setItem(storageKey, data));

registerServiceWorker();
