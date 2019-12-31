const button = document.getElementById("but");
const canvas = document.getElementById("canvas");
const offCanvas = document.getElementById("offcanvas");
const ctx = canvas.getContext("2d");
const offCtx = offCanvas.getContext("2d");

function setpixelated(context) {
  context["imageSmoothingEnabled"] = false; /* standard */
  context["mozImageSmoothingEnabled"] = false; /* Firefox */
  context["oImageSmoothingEnabled"] = false; /* Opera */
  context["webkitImageSmoothingEnabled"] = false; /* Safari */
  context["msImageSmoothingEnabled"] = false; /* IE */
}

const socket = new WebSocket("ws://127.0.0.1:8080/connect");

socket.onopen = e => {
  console.log("OPEN!", e);
};

button.addEventListener("click", () => {
  socket.send("Hey");
});

const initCanvas = (w, h) => {
  canvas.width = w;
  canvas.height = h;
  offCanvas.width = w;
  offCanvas.height = h;
  ctx.fillStyle = "magenta";
  ctx.fillRect(0, 0, w, h);
};

const changeSize = (w, h) => {
  setpixelated(offCtx);
  offCtx.drawImage(canvas, 0, 0);
  canvas.width = w;
  canvas.height = h;
  setpixelated(ctx);
  ctx.drawImage(offCanvas, 0, 0, w, h);
  offCanvas.width = w;
  offCanvas.height = h;
};

const pixel = (x, y, r, g, b) => {
  ctx.fillStyle = `rgb(${r * 255}, ${g * 255}, ${b * 255})`;
  ctx.fillRect(x, y, 1, 1);
};

socket.onmessage = e => {
  let data = JSON.parse(e.data);
  console.log("MSG", data);
  let { w, h } = data;
  if (!canvas.style.width) {
    initCanvas(w, h);
  }
  pixel(0, 0, 1, 1, 1);
  pixel(0, 1, 0, 0, 0);
  pixel(0, 2, 1, 1, 1);
  pixel(0, 3, 0, 0, 0);
  changeSize(16, 8);
  pixel(0, 0, 1, 0, 0);
};
