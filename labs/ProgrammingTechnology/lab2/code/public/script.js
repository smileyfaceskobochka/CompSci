const form = document.getElementById('form');
const responseBox = document.getElementById('response');
const submitBtn = form.querySelector('.btn');

const handleSubmit = async (e) => {
  e.preventDefault();

  const data = new FormData(form);
  showLoadingState();

  try {
    const res = await fetch('/', {
      method: 'POST',
      body: new URLSearchParams(data)
    });

    const text = await res.text();
    showResponse(`Сервер ответил: ${text}`);
    form.reset();

  } catch (error) {
    showResponse(`Ошибка: ${error.message}`);
  } finally {
    resetButtonState();
  }
};

const showLoadingState = () => {
  submitBtn.disabled = true;
  submitBtn.textContent = 'Отправка...';
};

const showResponse = (message) => {
  responseBox.textContent = message;
  responseBox.classList.add('show');

  setTimeout(() => {
    responseBox.classList.remove('show');
  }, 5000);
};

const resetButtonState = () => {
  submitBtn.disabled = false;
  submitBtn.textContent = 'Отправить';
};

form.addEventListener('submit', handleSubmit);
