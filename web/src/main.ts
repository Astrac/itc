import "./style.css";
import Snap from "snapsvg";
// import { ITCNetwork } from "scalajs:main.js";

// console.dir(ITCNetwork.withInitialNode());

document.querySelector<HTMLDivElement>("#app")!.innerHTML = `
  <div>
    <p>Test!</p>
    <svg width="400" height="400" id="#drawing"></svg>
  </div>
`;

Snap("#drawing").circle(200, 200, 100)