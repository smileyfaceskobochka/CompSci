const express = require("express");
const path = require("path");
const bodyParser = require("body-parser");

// init
const app = express();
const port = 8080;

console.log("[INIT] Starting server...");
console.log("[PATH] __dirname =", __dirname);
console.log("[PATH] static dir =", path.resolve(__dirname, "public"));

// логируем каждый запрос
app.use((req, res, next) => {
  console.log(`[REQ] ${req.method} ${req.url}`);
  next();
});

// логируем раздачу статики
app.use(
  express.static(path.resolve(__dirname, "public"), {
    setHeaders(res, filePath) {
      console.log(`[STATIC] Sent file: ${filePath}`);
    },
  })
);

app.use(bodyParser.urlencoded({ extended: true }));

const list = [];

// логируем GET /list
app.get("/list", (req, res) => {
  console.log("[GET /list] returning list:", list);
  res.send(list);
});

// логируем отдачу index.html
app.get("/", (req, res) => {
  const file = path.resolve(__dirname, "index.html");
  console.log("[GET /] sending", file);
  res.sendFile(file);
});

// логируем POST
app.post("/", (req, res) => {
  console.log("[POST /] BODY =", req.body);
  list.push(req.body);
  console.log("[POST /] list now =", list);
  res.send("OK");
});

// запуск сервера
app.listen(port, () => {
  console.log(`Server running at http://localhost:${port}`);
});
