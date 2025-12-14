const express = require("express");
const path = require("path");
const multer = require("multer");
const fs = require("fs");

const app = express();
const port = 8080;

// Static files
app.use(express.static(path.join(__dirname, "public")));
app.use(express.json());
app.use(express.urlencoded({ extended: true }));

// Ensure uploads directory
const uploadsDir = path.join(__dirname, 'public', 'uploads');
if (!fs.existsSync(uploadsDir)) fs.mkdirSync(uploadsDir, { recursive: true });

// Multer config
const storage = multer.diskStorage({
  destination: uploadsDir,
  filename: (req, file, cb) => cb(null, Date.now() + '-' + Math.round(Math.random() * 1E9) + path.extname(file.originalname))
});
const upload = multer({ storage });

// JSON storage
const imagesFile = path.join(__dirname, 'images.json');
const votesFile = path.join(__dirname, 'votes.json');
let imagesData = fs.existsSync(imagesFile) ? JSON.parse(fs.readFileSync(imagesFile)) : [];
let votesData = fs.existsSync(votesFile) ? JSON.parse(fs.readFileSync(votesFile)) : {};

function saveImages() { fs.writeFileSync(imagesFile, JSON.stringify(imagesData)); }
function saveVotes() { fs.writeFileSync(votesFile, JSON.stringify(votesData)); }

// Routes
app.get("/images", (req, res) => res.json(imagesData.sort((a, b) => b.votes - a.votes).slice(0, 10).map(img => img.path)));
app.get("/gallery", (req, res) => res.json(imagesData.sort((a, b) => b.votes - a.votes)));

app.post("/vote", (req, res) => {
  const { id, vote } = req.body;
  const img = imagesData.find(i => i.id == id);
  if (!img) return res.status(404).send("Not found");
  const ip = req.ip || 'unknown';
  if (!votesData[ip]) votesData[ip] = {};
  if (votesData[ip][id]) return res.status(429).send("Already voted");
  votesData[ip][id] = true;
  saveVotes();
  img.votes += vote;
  saveImages();
  res.send("OK");
});

app.post("/", upload.single('meme'), (req, res) => {
  if (!req.file) return res.status(400).send("No file");
  const newImage = {
    id: Date.now(),
    path: 'uploads/' + req.file.filename,
    uploader_name: req.body.name,
    uploader_email: req.body.email,
    votes: 0
  };
  imagesData.push(newImage);
  saveImages();
  res.send("OK");
});

app.get("/", (req, res) => res.sendFile(path.join(__dirname, "public", "index.html")));
app.get("/gallery.html", (req, res) => res.sendFile(path.join(__dirname, "public", "gallery.html")));

app.listen(port, () => console.log(`Server on http://localhost:${port}`));
