import json
from PyQt6.QtWidgets import (
    QDialog,
    QVBoxLayout,
    QFormLayout,
    QDialogButtonBox,
    QMessageBox,
    QLineEdit,
    QComboBox,
    QCheckBox,
    QPushButton,
    QLabel,
)
from database import Database


class LoginDialog(QDialog):
    """Диалог входа в систему"""

    def __init__(self, parent=None, db=None):
        super().__init__(parent)
        self.db = db
        self.user_id = None
        self.setWindowTitle("Вход в систему")
        self.setModal(True)
        self.resize(350, 200)

        layout = QVBoxLayout(self)

        # Создаем форму
        form_layout = QFormLayout()

        self.username_edit = QLineEdit()
        self.username_edit.setPlaceholderText("Введите логин")
        self.password_edit = QLineEdit()
        self.password_edit.setEchoMode(QLineEdit.EchoMode.Password)
        self.password_edit.setPlaceholderText("Введите пароль")

        form_layout.addRow("Логин:", self.username_edit)
        form_layout.addRow("Пароль:", self.password_edit)

        layout.addLayout(form_layout)

        # Кнопки
        buttons = QDialogButtonBox(
            QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel
        )
        buttons.accepted.connect(self.accept_login)
        buttons.rejected.connect(self.reject)
        layout.addWidget(buttons)

        # Кнопка регистрации
        self.register_button = QPushButton("Регистрация")
        self.register_button.clicked.connect(self.open_registration)
        layout.addWidget(self.register_button)

    def accept_login(self):
        """Обработка входа"""
        username = self.username_edit.text().strip()
        password = self.password_edit.text()

        if not username or not password:
            QMessageBox.warning(self, "Предупреждение", "Введите логин и пароль")
            return

        try:
            # Получаем данные пользователя
            with self.db.conn.cursor() as cur:
                cur.execute(
                    "SELECT id, username, email, is_admin, password_hash FROM users WHERE username = %s",
                    (username,),
                )
                user_data = cur.fetchone()

            if user_data and (
                Database.verify_password(password, user_data[4]) or user_data[4] == password
            ):
                self.user_id = user_data[0]
                self.is_admin = user_data[3]
                self.accept()
            else:
                QMessageBox.critical(self, "Ошибка", "Неверный логин или пароль")

        except Exception as e:
            QMessageBox.critical(self, "Ошибка", f"Ошибка входа: {str(e)}")

    def open_registration(self):
        """Открывает диалог регистрации"""
        dialog = RegisterDialog(db=self.db)
        if dialog.exec() == QDialog.DialogCode.Accepted:
            # После успешной регистрации заполняем поля
            self.username_edit.setText(dialog.username_edit.text())


class RegisterDialog(QDialog):
    """Диалог регистрации"""

    def __init__(self, parent=None, db=None):
        super().__init__(parent)
        self.db = db
        self.setWindowTitle("Регистрация")
        self.setModal(True)
        self.resize(350, 250)

        layout = QVBoxLayout(self)

        # Создаем форму
        form_layout = QFormLayout()

        self.username_edit = QLineEdit()
        self.username_edit.setPlaceholderText("Введите логин")
        self.email_edit = QLineEdit()
        self.email_edit.setPlaceholderText("Введите email")
        self.password_edit = QLineEdit()
        self.password_edit.setEchoMode(QLineEdit.EchoMode.Password)
        self.password_edit.setPlaceholderText("Введите пароль")
        self.confirm_password_edit = QLineEdit()
        self.confirm_password_edit.setEchoMode(QLineEdit.EchoMode.Password)
        self.confirm_password_edit.setPlaceholderText("Повторите пароль")

        form_layout.addRow("Логин:", self.username_edit)
        form_layout.addRow("Email:", self.email_edit)
        form_layout.addRow("Пароль:", self.password_edit)
        form_layout.addRow("Повтор пароля:", self.confirm_password_edit)

        layout.addLayout(form_layout)

        # Кнопки
        buttons = QDialogButtonBox(
            QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel
        )
        buttons.accepted.connect(self.accept_registration)
        buttons.rejected.connect(self.reject)
        layout.addWidget(buttons)

    def accept_registration(self):
        """Обработка регистрации"""
        username = self.username_edit.text().strip()
        email = self.email_edit.text().strip()
        password = self.password_edit.text()
        confirm_password = self.confirm_password_edit.text()

        # Валидация
        if not username or not email or not password:
            QMessageBox.warning(self, "Предупреждение", "Заполните все поля")
            return

        if password != confirm_password:
            QMessageBox.warning(self, "Предупреждение", "Пароли не совпадают")
            return

        if len(password) < 6:
            QMessageBox.warning(
                self, "Предупреждение", "Пароль должен быть не менее 6 символов"
            )
            return

        try:
            # Хэшируем пароль
            password_hash = Database.hash_password(password)

            # Создаем пользователя
            user_id = self.db.create_user(username, email, password_hash)

            QMessageBox.information(
                self, "Успех", f"Пользователь {username} успешно зарегистрирован!"
            )
            self.accept()

        except Exception as e:
            QMessageBox.critical(self, "Ошибка", f"Ошибка регистрации: {str(e)}")


class DeviceDialog(QDialog):
    """Диалог для добавления/редактирования устройства"""

    def __init__(self, parent=None, device_id=None, db=None, user_id=None):
        super().__init__(parent)
        self.device_id = device_id
        self.db = db
        self.user_id = user_id
        self.setWindowTitle(
            "Добавить устройство" if device_id is None else "Редактировать устройство"
        )
        self.setModal(True)
        self.resize(400, 300)

        layout = QVBoxLayout(self)

        # Создаем форму
        form_layout = QFormLayout()

        # Поля формы
        self.name_edit = QLineEdit()
        form_layout.addRow("Название:", self.name_edit)

        self.hub_combo = QComboBox()
        self.type_combo = QComboBox()
        self.status_edit = QLineEdit()

        form_layout.addRow("Хаб:", self.hub_combo)
        form_layout.addRow("Тип устройства:", self.type_combo)
        form_layout.addRow("Статус (JSON):", self.status_edit)

        layout.addLayout(form_layout)

        # Кнопки
        buttons = QDialogButtonBox(
            QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel
        )
        buttons.accepted.connect(self.accept)
        buttons.rejected.connect(self.reject)
        layout.addWidget(buttons)

        # Загружаем данные
        self.load_data()

        # Если редактирование, загружаем данные устройства
        if device_id:
            self.load_device_data()

    def load_data(self):
        """Загружает хабы и типы устройств пользователя"""
        try:
            # Загружаем хабы пользователя
            hubs = self.db.get_user_hubs(self.user_id)
            self.hub_combo.clear()
            for hub_id, hub_name in hubs:
                self.hub_combo.addItem(hub_name, hub_id)

            # Загружаем типы устройств
            device_types = self.db.get_device_types()
            self.type_combo.clear()
            for type_id, type_name in device_types:
                self.type_combo.addItem(type_name, type_id)

        except Exception as e:
            QMessageBox.critical(
                self, "Ошибка", f"Не удалось загрузить данные: {str(e)}"
            )

    def load_device_data(self):
        """Загружает данные устройства для редактирования"""
        try:
            # Получаем данные устройства (простой запрос)
            with self.db.conn.cursor() as cur:
                cur.execute(
                    """
                    SELECT d.name, d.hub_id, d.type_id, d.status
                    FROM devices d
                    WHERE d.id = %s
                """,
                    (self.device_id,),
                )
                device = cur.fetchone()

            if device:
                self.name_edit.setText(device[0])
                self.hub_combo.setCurrentIndex(self.hub_combo.findData(device[1]))
                self.type_combo.setCurrentIndex(self.type_combo.findData(device[2]))
                self.status_edit.setText(json.dumps(device[3], ensure_ascii=False))

        except Exception as e:
            QMessageBox.critical(
                self, "Ошибка", f"Не удалось загрузить данные устройства: {str(e)}"
            )

    def get_data(self):
        """Возвращает данные из формы"""
        return {
            "name": self.name_edit.text().strip(),
            "hub_id": self.hub_combo.currentData(),
            "type_id": self.type_combo.currentData(),
            "status": self.status_edit.text().strip(),
        }


class HubDialog(QDialog):
    """Диалог для добавления хаба"""

    def __init__(self, parent=None, db=None, user_id=None):
        super().__init__(parent)
        self.db = db
        self.user_id = user_id
        self.setWindowTitle("Добавить хаб")
        self.setModal(True)
        self.resize(400, 200)

        layout = QVBoxLayout(self)

        # Создаем форму
        form_layout = QFormLayout()

        self.name_edit = QLineEdit()
        self.name_edit.setPlaceholderText("Введите название хаба")
        self.location_edit = QLineEdit()
        self.location_edit.setPlaceholderText("Введите местоположение")
        self.serial_edit = QLineEdit()
        self.serial_edit.setPlaceholderText("Введите серийный номер")

        form_layout.addRow("Название:", self.name_edit)
        form_layout.addRow("Местоположение:", self.location_edit)
        form_layout.addRow("Серийный номер:", self.serial_edit)

        layout.addLayout(form_layout)

        # Кнопки
        buttons = QDialogButtonBox(
            QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel
        )
        buttons.accepted.connect(self.accept)
        buttons.rejected.connect(self.reject)
        layout.addWidget(buttons)

    def get_data(self):
        """Возвращает данные из формы"""
        return {
            "name": self.name_edit.text().strip(),
            "location": self.location_edit.text().strip(),
            "serial_number": self.serial_edit.text().strip(),
        }


class UserDialog(QDialog):
    """Диалог для добавления/редактирования пользователя"""

    def __init__(self, parent=None, db=None, user_data=None):
        super().__init__(parent)
        self.db = db
        self.user_data = user_data  # For editing
        self.setWindowTitle("Добавить пользователя" if user_data is None else "Редактировать пользователя")
        self.setModal(True)
        self.resize(350, 200)

        layout = QVBoxLayout(self)
        form = QFormLayout()

        self.username_edit = QLineEdit()
        self.email_edit = QLineEdit()
        self.password_edit = QLineEdit()
        self.password_edit.setEchoMode(QLineEdit.EchoMode.Password)
        self.admin_check = QCheckBox("Администратор")

        if user_data:
            self.username_edit.setText(user_data[1])  # username
            self.email_edit.setText(user_data[2])     # email
            self.admin_check.setChecked(user_data[3]) # is_admin
            self.password_edit.setPlaceholderText("Оставьте пустым, чтобы не менять")
        else:
            self.password_edit.setPlaceholderText("Введите пароль")

        form.addRow("Логин:", self.username_edit)
        form.addRow("Email:", self.email_edit)
        form.addRow("Пароль:", self.password_edit)
        form.addRow(self.admin_check)

        layout.addLayout(form)

        buttons = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        buttons.accepted.connect(self.accept)
        buttons.rejected.connect(self.reject)
        layout.addWidget(buttons)

    def get_data(self):
        """Возвращает данные из формы"""
        return {
            "username": self.username_edit.text().strip(),
            "email": self.email_edit.text().strip(),
            "password": self.password_edit.text(),
            "is_admin": self.admin_check.isChecked(),
        }


class HubAdminDialog(QDialog):
    """Диалог для добавления/редактирования хаба (админ)"""

    def __init__(self, parent=None, db=None, hub_data=None):
        super().__init__(parent)
        self.db = db
        self.hub_data = hub_data  # For editing
        self.setWindowTitle("Добавить хаб" if hub_data is None else "Редактировать хаб")
        self.setModal(True)
        self.resize(400, 200)

        layout = QVBoxLayout(self)
        form = QFormLayout()

        self.user_combo = QComboBox()
        self.name_edit = QLineEdit()
        self.location_edit = QLineEdit()
        self.serial_edit = QLineEdit()

        # Populate users
        users = self.db.get_all_users()
        for user_id, username, email, is_admin in users:
            self.user_combo.addItem(username, user_id)

        if hub_data:
            self.user_combo.setCurrentIndex(self.user_combo.findData(hub_data[1]))  # user_id
            self.name_edit.setText(hub_data[3])      # name
            self.location_edit.setText(hub_data[4])  # location
            # Показываем серийный номер как статический текст
            self.serial_label = QLabel(hub_data[5])  # serial

        form.addRow("Владелец:", self.user_combo)
        form.addRow("Название:", self.name_edit)
        form.addRow("Местоположение:", self.location_edit)
        if hub_data:
            form.addRow("Серийный номер:", self.serial_label)
        else:
            form.addRow("Серийный номер:", self.serial_edit)

        layout.addLayout(form)

        buttons = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        buttons.accepted.connect(self.accept)
        buttons.rejected.connect(self.reject)
        layout.addWidget(buttons)

    def get_data(self):
        """Возвращает данные из формы"""
        return {
            "user_id": self.user_combo.currentData(),
            "name": self.name_edit.text().strip(),
            "location": self.location_edit.text().strip(),
            "serial_number": self.serial_edit.text().strip(),
        }


class DeviceAdminDialog(QDialog):
    """Диалог для добавления/редактирования устройства (админ)"""

    def __init__(self, parent=None, db=None, device_data=None):
        super().__init__(parent)
        self.db = db
        self.device_data = device_data  # For editing
        self.setWindowTitle("Добавить устройство" if device_data is None else "Редактировать устройство")
        self.setModal(True)
        self.resize(400, 300)

        layout = QVBoxLayout(self)
        form = QFormLayout()

        self.hub_combo = QComboBox()
        self.type_combo = QComboBox()
        self.name_edit = QLineEdit()
        self.status_edit = QLineEdit()

        # Populate combos
        hubs = self.db.get_all_hubs()
        for hub_id, user_id, username, name, location, serial in hubs:
            self.hub_combo.addItem(name, hub_id)

        types = self.db.get_device_types()
        for type_id, type_name in types:
            self.type_combo.addItem(type_name, type_id)

        if device_data:
            self.hub_combo.setCurrentIndex(self.hub_combo.findData(device_data[1]))  # hub_id
            self.name_edit.setText(device_data[3])    # name
            self.type_combo.setCurrentIndex(self.type_combo.findData(device_data[4]))  # type_name -> need to map
            self.status_edit.setText(device_data[5])  # status

        form.addRow("Хаб:", self.hub_combo)
        form.addRow("Тип:", self.type_combo)
        form.addRow("Название:", self.name_edit)
        form.addRow("Статус (JSON):", self.status_edit)

        layout.addLayout(form)

        buttons = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        buttons.accepted.connect(self.accept)
        buttons.rejected.connect(self.reject)
        layout.addWidget(buttons)

    def get_data(self):
        """Возвращает данные из формы"""
        return {
            "hub_id": self.hub_combo.currentData(),
            "type_id": self.type_combo.currentData(),
            "name": self.name_edit.text().strip(),
            "status": self.status_edit.text().strip(),
        }
