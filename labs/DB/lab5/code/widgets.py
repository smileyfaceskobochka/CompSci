from PyQt6.QtWidgets import QLineEdit
from PyQt6.QtCore import Qt


class SearchLineEdit(QLineEdit):
    """Поле поиска с поддержкой Esc для очистки"""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.clear_callback = None

    def keyPressEvent(self, event):
        """Обработчик нажатий клавиш"""
        if event.key() == Qt.Key.Key_Escape:
            # Очищаем поле при нажатии Esc
            if self.clear_callback:
                self.clear_callback()
        else:
            # Обрабатываем остальные нажатия стандартно
            super().keyPressEvent(event)
