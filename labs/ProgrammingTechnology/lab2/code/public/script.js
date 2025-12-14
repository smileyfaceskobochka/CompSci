const form = document.getElementById('form');
const responseBox = document.getElementById('response');
const submitBtn = form.querySelector('.btn');

const handleSubmit = async (e) => {
  e.preventDefault();
  const data = new FormData(form);
  submitBtn.disabled = true;
  submitBtn.textContent = 'Отправка...';

  try {
    const res = await fetch('/', { method: 'POST', body: data });
    const text = await res.text();

    if (res.ok) {
      responseBox.textContent = `Сервер ответил: ${text}`;
      responseBox.classList.add('show');
      form.reset();
      setTimeout(() => loadBackgroundImages(), 1000);
    } else {
      responseBox.textContent = `Ошибка сервера: ${text}`;
      responseBox.classList.add('show');
    }
  } catch (error) {
    responseBox.textContent = `Ошибка: ${error.message}`;
    responseBox.classList.add('show');
  } finally {
    submitBtn.disabled = false;
    submitBtn.textContent = 'Отправить';
  }

  setTimeout(() => responseBox.classList.remove('show'), 5000);
};

function loadBackgroundImages() {
  fetch('/images')
    .then(res => res.json())
    .then(images => {
      const spinningMemes = document.querySelectorAll('.spinning-meme');
      const positions = [];

      spinningMemes.forEach(() => {
        let randomTop, randomLeft;
        let attempts = 0;
        do {
          randomTop = Math.random() * 80 + 5;
          randomLeft = Math.random() * 80 + 5;
          attempts++;
        } while (isOverlapping(randomTop, randomLeft, positions) && attempts < 50);
        positions.push({ top: randomTop, left: randomLeft });
      });

      spinningMemes.forEach((meme, index) => {
        meme.style.display = images.length > 0 ? 'block' : 'none';
        if (images.length > 0) {
          meme.style.top = positions[index].top + '%';
          meme.style.left = positions[index].left + '%';
          const randomImage = images[Math.floor(Math.random() * images.length)];
          // Add cache busting parameter
          const cacheBust = Date.now();
          meme.innerHTML = `<img src="/${randomImage}?t=${cacheBust}" alt="Meme" style="width: 100%; height: 100%; object-fit: cover;" onerror="this.style.display='none'">`;
          meme.style.animationDelay = (Math.random() * 10) + 's';
          meme.style.animationDuration = (8 + Math.random() * 4) + 's';
        }
      });
    })
    .catch(err => {
      console.error('Failed to fetch images:', err);
      document.querySelectorAll('.spinning-meme').forEach(meme => meme.style.display = 'none');
    });
}

function isOverlapping(top, left, positions) {
  const minDistance = 15;
  for (const pos of positions) {
    const distance = Math.sqrt(Math.pow(pos.top - top, 2) + Math.pow(pos.left - left, 2));
    if (distance < minDistance) return true;
  }
  return false;
}

if (form) {
  loadBackgroundImages();
  form.addEventListener('submit', handleSubmit);
}
