import sys
import json
from PyQt6.QtWidgets import (
    QApplication,
    QMainWindow,
    QWidget,
    QVBoxLayout,
    QHBoxLayout,
    QTableWidget,
    QTableWidgetItem,
    QPushButton,
    QLineEdit,
    QComboBox,
    QLabel,
    QDialog,
    QFormLayout,
    QDialogButtonBox,
    QMessageBox,
    QHeaderView,
    QAbstractItemView,
    QMenuBar,
    QStatusBar,
    QTabWidget,
    QCheckBox,
)
from PyQt6.QtGui import QAction
from PyQt6.QtCore import Qt, QTimer
from database import Database
from widgets import SearchLineEdit
from dialogs import (
    LoginDialog,
    RegisterDialog,
    DeviceDialog,
    HubDialog,
    UserDialog,
    HubAdminDialog,
    DeviceAdminDialog,
)



class MainWindow(QMainWindow):
    """Главное окно приложения"""

    def __init__(self):
        super().__init__()
        self.db = None
        self.current_user = None
        self.current_user_id = None
        self.is_admin = False

        # Показываем диалог входа перед инициализацией основного окна
        self.show_login_dialog()

    def show_login_dialog(self):
        """Показывает диалог входа"""
        login_dialog = LoginDialog(db=Database(), parent=self)
        if login_dialog.exec() == QDialog.DialogCode.Accepted:
            self.current_user_id = login_dialog.user_id
            self.is_admin = login_dialog.is_admin
            self.initialize_main_window()
        else:
            sys.exit(0)  # Выход если пользователь отменил вход

    def initialize_main_window(self):
        """Инициализация главного окна после успешного входа"""
        if self.is_admin:
            # Admin panel
            self.setWindowTitle("Админ панель")
            self.setGeometry(100, 100, 1200, 700)

            menubar = self.menuBar()
            user_menu = menubar.addMenu("Пользователь")
            refresh_action = QAction("Обновить данные", self)
            refresh_action.triggered.connect(self.refresh_admin_data)
            logout_action = QAction("Выйти", self)
            logout_action.triggered.connect(self.logout)
            user_menu.addAction(refresh_action)
            user_menu.addAction(logout_action)

            tab_widget = QTabWidget(self)
            self.setCentralWidget(tab_widget)

            # Users tab
            users_widget = QWidget()
            tab_widget.addTab(users_widget, "Пользователи")
            users_layout = QVBoxLayout(users_widget)

            # Search for users
            users_filter_layout = QHBoxLayout()

            self.users_search_edit = SearchLineEdit()
            self.users_search_edit.setPlaceholderText("Поиск по логину или email...")
            self.users_search_edit.returnPressed.connect(self.search_users)
            self.users_search_edit.clear_callback = self.clear_users_search

            self.users_clear_button = QPushButton("×")
            self.users_clear_button.setMaximumWidth(30)
            self.users_clear_button.setToolTip("Очистить поиск")
            self.users_clear_button.clicked.connect(self.clear_users_search)
            self.users_clear_button.setStyleSheet(
                """
                QPushButton {
                    font-size: 16px;
                    font-weight: bold;
                    color: #666;
                    border: none;
                    background: transparent;
                }
                QPushButton:hover {
                    color: #000;
                    background: #f0f0f0;
                }
            """
            )

            self.users_search_button = QPushButton("Поиск")
            self.users_search_button.clicked.connect(self.search_users)
            self.users_search_button.setMaximumWidth(100)

            users_filter_layout.addWidget(QLabel("Фильтр:"))
            users_filter_layout.addWidget(self.users_search_edit, 1)

            # Container for clear and search buttons
            users_buttons_layout = QHBoxLayout()
            users_buttons_layout.addWidget(self.users_clear_button)
            users_buttons_layout.addWidget(self.users_search_button)

            users_filter_layout.addLayout(users_buttons_layout)

            users_layout.addLayout(users_filter_layout)

            self.users_table = QTableWidget()
            self.users_table.setColumnCount(4)
            self.users_table.setHorizontalHeaderLabels(["ID", "Логин", "Email", "Админ"])
            self.users_table.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeMode.Stretch)
            users_layout.addWidget(self.users_table)
            users_btns = QHBoxLayout()
            add_user_btn = QPushButton("Добавить пользователя")
            add_user_btn.clicked.connect(self.add_user)
            edit_user_btn = QPushButton("Редактировать")
            edit_user_btn.clicked.connect(self.edit_user)
            delete_user_btn = QPushButton("Удалить")
            delete_user_btn.clicked.connect(self.delete_user)
            refresh_users_btn = QPushButton("Обновить")
            refresh_users_btn.clicked.connect(self.refresh_users_table)
            users_btns.addWidget(add_user_btn)
            users_btns.addWidget(edit_user_btn)
            users_btns.addWidget(delete_user_btn)
            users_btns.addWidget(refresh_users_btn)
            users_layout.addLayout(users_btns)

            # Hubs tab
            hubs_widget = QWidget()
            tab_widget.addTab(hubs_widget, "Хабы")
            hubs_layout = QVBoxLayout(hubs_widget)

            # Search for hubs
            hubs_filter_layout = QHBoxLayout()

            self.hubs_search_edit = SearchLineEdit()
            self.hubs_search_edit.setPlaceholderText("Поиск по названию, местоположению, серийному номеру или владельцу...")
            self.hubs_search_edit.returnPressed.connect(self.search_hubs)
            self.hubs_search_edit.clear_callback = self.clear_hubs_search

            self.hubs_clear_button = QPushButton("×")
            self.hubs_clear_button.setMaximumWidth(30)
            self.hubs_clear_button.setToolTip("Очистить поиск")
            self.hubs_clear_button.clicked.connect(self.clear_hubs_search)
            self.hubs_clear_button.setStyleSheet(
                """
                QPushButton {
                    font-size: 16px;
                    font-weight: bold;
                    color: #666;
                    border: none;
                    background: transparent;
                }
                QPushButton:hover {
                    color: #000;
                    background: #f0f0f0;
                }
            """
            )

            self.hubs_search_button = QPushButton("Поиск")
            self.hubs_search_button.clicked.connect(self.search_hubs)
            self.hubs_search_button.setMaximumWidth(100)

            hubs_filter_layout.addWidget(QLabel("Фильтр:"))
            hubs_filter_layout.addWidget(self.hubs_search_edit, 1)

            # Container for clear and search buttons
            hubs_buttons_layout = QHBoxLayout()
            hubs_buttons_layout.addWidget(self.hubs_clear_button)
            hubs_buttons_layout.addWidget(self.hubs_search_button)

            hubs_filter_layout.addLayout(hubs_buttons_layout)

            hubs_layout.addLayout(hubs_filter_layout)

            self.hubs_table = QTableWidget()
            self.hubs_table.setColumnCount(5)
            self.hubs_table.setHorizontalHeaderLabels(["ID", "Владелец", "Название", "Местоположение", "Серийный номер"])
            self.hubs_table.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeMode.Stretch)
            hubs_layout.addWidget(self.hubs_table)
            hubs_btns = QHBoxLayout()
            add_hub_btn = QPushButton("Добавить хаб")
            add_hub_btn.clicked.connect(self.add_admin_hub)
            edit_hub_btn = QPushButton("Редактировать")
            edit_hub_btn.clicked.connect(self.edit_hub)
            delete_hub_btn = QPushButton("Удалить")
            delete_hub_btn.clicked.connect(self.delete_hub)
            refresh_hubs_btn = QPushButton("Обновить")
            refresh_hubs_btn.clicked.connect(self.refresh_hubs_table)
            hubs_btns.addWidget(add_hub_btn)
            hubs_btns.addWidget(edit_hub_btn)
            hubs_btns.addWidget(delete_hub_btn)
            hubs_btns.addWidget(refresh_hubs_btn)
            hubs_layout.addLayout(hubs_btns)

            # Devices tab
            devices_widget = QWidget()
            tab_widget.addTab(devices_widget, "Устройства")
            devices_layout = QVBoxLayout(devices_widget)

            # Search for devices
            devices_filter_layout = QHBoxLayout()

            self.devices_search_edit = SearchLineEdit()
            self.devices_search_edit.setPlaceholderText("Поиск по названию, типу или хабу...")
            self.devices_search_edit.returnPressed.connect(self.search_devices)
            self.devices_search_edit.clear_callback = self.clear_devices_search

            self.devices_clear_button = QPushButton("×")
            self.devices_clear_button.setMaximumWidth(30)
            self.devices_clear_button.setToolTip("Очистить поиск")
            self.devices_clear_button.clicked.connect(self.clear_devices_search)
            self.devices_clear_button.setStyleSheet(
                """
                QPushButton {
                    font-size: 16px;
                    font-weight: bold;
                    color: #666;
                    border: none;
                    background: transparent;
                }
                QPushButton:hover {
                    color: #000;
                    background: #f0f0f0;
                }
            """
            )

            self.devices_search_button = QPushButton("Поиск")
            self.devices_search_button.clicked.connect(self.search_devices)
            self.devices_search_button.setMaximumWidth(100)

            devices_filter_layout.addWidget(QLabel("Фильтр:"))
            devices_filter_layout.addWidget(self.devices_search_edit, 1)

            # Container for clear and search buttons
            devices_buttons_layout = QHBoxLayout()
            devices_buttons_layout.addWidget(self.devices_clear_button)
            devices_buttons_layout.addWidget(self.devices_search_button)

            devices_filter_layout.addLayout(devices_buttons_layout)

            devices_layout.addLayout(devices_filter_layout)

            self.devices_table = QTableWidget()
            self.devices_table.setColumnCount(5)
            self.devices_table.setHorizontalHeaderLabels(["ID", "Хаб", "Название", "Тип", "Статус"])
            self.devices_table.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeMode.Stretch)
            devices_layout.addWidget(self.devices_table)
            devices_btns = QHBoxLayout()
            add_device_btn = QPushButton("Добавить устройство")
            add_device_btn.clicked.connect(self.add_admin_device)
            edit_device_btn = QPushButton("Редактировать")
            edit_device_btn.clicked.connect(self.edit_admin_device)
            delete_device_btn = QPushButton("Удалить")
            delete_device_btn.clicked.connect(self.delete_admin_device)
            refresh_devices_btn = QPushButton("Обновить")
            refresh_devices_btn.clicked.connect(self.refresh_devices_table)
            devices_btns.addWidget(add_device_btn)
            devices_btns.addWidget(edit_device_btn)
            devices_btns.addWidget(delete_device_btn)
            devices_btns.addWidget(refresh_devices_btn)
            devices_layout.addLayout(devices_btns)

            self.connect_to_database()
            self.refresh_admin_data()
        else:
            # Regular user UI
            self.setWindowTitle(
                f"Управление устройствами - Пользователь: {self.get_current_username()}"
            )
            self.setGeometry(100, 100, 1000, 600)

            # Создаем меню
            menubar = self.menuBar()

            user_menu = menubar.addMenu("Пользователь")
            profile_action = QAction("Профиль", self)
            profile_action.triggered.connect(self.edit_profile)
            logout_action = QAction("Выйти", self)
            logout_action.triggered.connect(self.logout)
            user_menu.addAction(profile_action)
            user_menu.addAction(logout_action)

            hub_menu = menubar.addMenu("Хабы")
            add_hub_action = QAction("Добавить хаб", self)
            add_hub_action.triggered.connect(self.add_hub)
            manage_hubs_action = QAction("Управление хабами", self)
            manage_hubs_action.triggered.connect(self.manage_hubs)
            hub_menu.addAction(add_hub_action)
            hub_menu.addAction(manage_hubs_action)

            # Создаем центральный виджет
            central_widget = QWidget()
            self.setCentralWidget(central_widget)

            # Создаем layout
            layout = QVBoxLayout(central_widget)

            # Панель поиска и фильтров
            filter_layout = QHBoxLayout()

            self.filter_edit = SearchLineEdit()
            self.filter_edit.setPlaceholderText("Поиск по названию устройства...")
            self.filter_edit.returnPressed.connect(self.perform_search)  # Enter key
            self.filter_edit.clear_callback = (
                self.clear_search
            )  # Connect clear callback for Esc key

            self.clear_button = QPushButton("×")  # X button for clearing
            self.clear_button.setMaximumWidth(30)
            self.clear_button.setToolTip("Очистить поиск")
            self.clear_button.clicked.connect(self.clear_search)
            self.clear_button.setStyleSheet(
                """
                QPushButton {
                    font-size: 16px;
                    font-weight: bold;
                    color: #666;
                    border: none;
                    background: transparent;
                }
                QPushButton:hover {
                    color: #000;
                    background: #f0f0f0;
                }
            """
            )

            self.search_button = QPushButton("Поиск")
            self.search_button.clicked.connect(self.perform_search)
            self.search_button.setMaximumWidth(100)

            filter_layout.addWidget(QLabel("Фильтр:"))
            filter_layout.addWidget(self.filter_edit, 1)

            # Container for clear and search buttons
            buttons_layout = QHBoxLayout()
            buttons_layout.addWidget(self.clear_button)
            buttons_layout.addWidget(self.search_button)

            filter_layout.addLayout(buttons_layout)

            layout.addLayout(filter_layout)

            # Таблица устройств
            self.table = QTableWidget()
            self.table.setColumnCount(4)
            self.table.setHorizontalHeaderLabels(["ID", "Название", "Тип", "Хаб"])
            self.table.horizontalHeader().setSectionResizeMode(
                QHeaderView.ResizeMode.Stretch
            )
            self.table.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
            self.table.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)

            layout.addWidget(self.table)

            # Панель кнопок
            buttons_layout = QHBoxLayout()

            self.add_button = QPushButton("Добавить устройство")
            self.add_button.clicked.connect(self.add_device)

            self.edit_button = QPushButton("Редактировать устройство")
            self.edit_button.clicked.connect(self.edit_device)
            self.edit_button.setEnabled(False)

            self.delete_button = QPushButton("Удалить устройство")
            self.delete_button.clicked.connect(self.delete_device)
            self.delete_button.setEnabled(False)

            self.refresh_button = QPushButton("Обновить")
            self.refresh_button.clicked.connect(self.refresh_data)

            buttons_layout.addWidget(self.add_button)
            buttons_layout.addWidget(self.edit_button)
            buttons_layout.addWidget(self.delete_button)
            buttons_layout.addStretch()
            buttons_layout.addWidget(self.refresh_button)

            layout.addLayout(buttons_layout)

            # Статус бар
            self.statusBar().showMessage(f"Вошел как: {self.get_current_username()}")

            # Подключаемся к базе данных
            self.connect_to_database()

            # Загружаем данные
            self.refresh_data()

            # Подключаем сигналы
            self.table.itemSelectionChanged.connect(self.on_selection_changed)

    def add_hub(self):
        """Добавляет новый хаб"""
        if not self.db or not self.current_user_id:
            return

        dialog = HubDialog(db=self.db, user_id=self.current_user_id)
        if dialog.exec() == QDialog.DialogCode.Accepted:
            data = dialog.get_data()

            # Проверяем данные
            if not data["name"] or not data["location"] or not data["serial_number"]:
                QMessageBox.warning(
                    self, "Предупреждение", "Заполните все поля"
                )
                return

            try:
                # Добавляем хаб
                with self.db.conn.cursor() as cur:
                    cur.execute(
                        "INSERT INTO hubs (user_id, name, location, serial_number) VALUES (%s, %s, %s, %s) RETURNING id",
                        (self.current_user_id, data['name'], data['location'], data['serial_number'])
                    )
                    hub_id = cur.fetchone()[0]
                self.db.conn.commit()

                QMessageBox.information(self, "Успех", f"Хаб '{data['name']}' добавлен с ID: {hub_id}")

            except Exception as e:
                QMessageBox.critical(
                    self, "Ошибка", f"Не удалось добавить хаб: {str(e)}"
                )

    def get_current_username(self):
        """Получить имя текущего пользователя"""
        if self.current_user_id:
            try:
                with self.db.conn.cursor() as cur:
                    cur.execute(
                        "SELECT username FROM users WHERE id = %s",
                        (self.current_user_id,),
                    )
                    result = cur.fetchone()
                    return result[0] if result else "Неизвестный"
            except:
                pass
        return "Неизвестный"

    def connect_to_database(self):
        """Подключение к базе данных"""
        try:
            self.db = Database()
            if not self.is_admin:  # Only show status for regular users
                self.statusBar().showMessage(
                    f"Подключено к базе данных. Пользователь: {self.get_current_username()}"
                )
        except Exception as e:
            QMessageBox.critical(
                self,
                "Ошибка подключения",
                f"Не удалось подключиться к базе данных: {str(e)}",
            )
            if not self.is_admin:  # Only show status for regular users
                self.statusBar().showMessage("Ошибка подключения к базе данных")

    def refresh_data(self):
        """Обновляет данные в таблице - показывает все устройства пользователя"""
        if not self.db or not self.current_user_id:
            return

        try:
            # Показываем все устройства пользователя без фильтрации
            devices = self.db.get_devices_for_user(self.current_user_id, "")

            self.table.setRowCount(len(devices))

            for row, device in enumerate(devices):
                for col, value in enumerate(device):
                    self.table.setItem(row, col, QTableWidgetItem(str(value)))

            device_count = self.db.get_user_devices_count(self.current_user_id)
            self.statusBar().showMessage(
                f"Устройств: {len(devices)} (всего: {device_count})"
            )

            # Очищаем поле поиска
            self.filter_edit.clear()

        except Exception as e:
            QMessageBox.critical(
                self, "Ошибка", f"Не удалось загрузить данные: {str(e)}"
            )
            self.statusBar().showMessage("Ошибка загрузки данных")

    def perform_search(self):
        """Выполняет поиск устройств"""
        if not self.db or not self.current_user_id:
            return

        try:
            filter_text = self.filter_edit.text().strip()
            devices = self.db.get_devices_for_user(self.current_user_id, filter_text)

            if not devices and filter_text:
                # Показываем ошибку если поиск не дал результатов
                QMessageBox.warning(
                    self, "Поиск", f"Устройства с названием '{filter_text}' не найдены"
                )
                return

            self.table.setRowCount(len(devices))

            for row, device in enumerate(devices):
                for col, value in enumerate(device):
                    self.table.setItem(row, col, QTableWidgetItem(str(value)))

            device_count = self.db.get_user_devices_count(self.current_user_id)
            self.statusBar().showMessage(
                f"Найдено устройств: {len(devices)} (всего: {device_count})"
            )

        except Exception as e:
            QMessageBox.critical(
                self, "Ошибка", f"Не удалось выполнить поиск: {str(e)}"
            )
            self.statusBar().showMessage("Ошибка поиска")

    def clear_search(self):
        """Очищает поле поиска и показывает все устройства"""
        self.filter_edit.clear()
        self.refresh_data()

    def on_selection_changed(self):
        """Обработчик изменения выделения в таблице"""
        selected = len(self.table.selectionModel().selectedRows()) > 0
        self.edit_button.setEnabled(selected)
        self.delete_button.setEnabled(selected)

    def add_device(self):
        """Добавляет новое устройство"""
        if not self.db or not self.current_user_id:
            return

        dialog = DeviceDialog(db=self.db, user_id=self.current_user_id)
        if dialog.exec() == QDialog.DialogCode.Accepted:
            data = dialog.get_data()

            # Проверяем данные
            if not data["name"]:
                QMessageBox.warning(
                    self, "Предупреждение", "Название устройства обязательно"
                )
                return

            # Проверяем дубликат имени
            if self.db.device_exists(data["name"]):
                QMessageBox.warning(
                    self,
                    "Предупреждение",
                    f"Устройство с названием '{data['name']}' уже существует",
                )
                return

            try:
                # Парсим JSON статус
                status = json.loads(data["status"]) if data["status"] else {}

                # Сохраняем устройство
                device_id = self.db.save_device(
                    None,  # Новый device_id
                    data["hub_id"],
                    data["type_id"],
                    data["name"],
                    json.dumps(status, ensure_ascii=False),
                )

                self.refresh_data()
                self.statusBar().showMessage(f"Устройство добавлено с ID: {device_id}")

            except json.JSONDecodeError:
                QMessageBox.warning(
                    self, "Ошибка", "Неверный формат JSON в поле статуса"
                )
            except Exception as e:
                QMessageBox.critical(
                    self, "Ошибка", f"Не удалось добавить устройство: {str(e)}"
                )

    def edit_device(self):
        """Редактирует выбранное устройство"""
        if not self.db or not self.current_user_id:
            return

        current_row = self.table.currentRow()
        if current_row < 0:
            return

        device_id = int(self.table.item(current_row, 0).text())

        dialog = DeviceDialog(
            db=self.db, device_id=device_id, user_id=self.current_user_id
        )
        if dialog.exec() == QDialog.DialogCode.Accepted:
            data = dialog.get_data()

            # Проверяем данные
            if not data["name"]:
                QMessageBox.warning(
                    self, "Предупреждение", "Название устройства обязательно"
                )
                return

            # Проверяем дубликат имени (исключая текущее устройство)
            if self.db.device_exists(data["name"], exclude_id=device_id):
                QMessageBox.warning(
                    self,
                    "Предупреждение",
                    f"Устройство с названием '{data['name']}' уже существует",
                )
                return

            try:
                # Парсим JSON статус
                status = json.loads(data["status"]) if data["status"] else {}

                # Сохраняем устройство
                device_id = self.db.save_device(
                    device_id,
                    data["hub_id"],
                    data["type_id"],
                    data["name"],
                    json.dumps(status, ensure_ascii=False),
                )

                self.refresh_data()
                self.statusBar().showMessage(f"Устройство обновлено с ID: {device_id}")

            except json.JSONDecodeError:
                QMessageBox.warning(
                    self, "Ошибка", "Неверный формат JSON в поле статуса"
                )
            except Exception as e:
                QMessageBox.critical(
                    self, "Ошибка", f"Не удалось обновить устройство: {str(e)}"
                )

    def delete_device(self):
        """Удаляет выбранное устройство"""
        if not self.db or not self.current_user_id:
            return

        current_row = self.table.currentRow()
        if current_row < 0:
            return

        device_id = int(self.table.item(current_row, 0).text())
        device_name = self.table.item(current_row, 1).text()

        reply = QMessageBox.question(
            self,
            "Подтверждение удаления",
            f"Вы действительно хотите удалить устройство '{device_name}'?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
            QMessageBox.StandardButton.No,
        )

        if reply == QMessageBox.StandardButton.Yes:
            try:
                # Используем безопасное удаление
                self.db.delete_device_safe(device_id)
                self.refresh_data()
                self.statusBar().showMessage(f"Устройство '{device_name}' удалено")

            except Exception as e:
                QMessageBox.critical(
                    self, "Ошибка", f"Не удалось удалить устройство: {str(e)}"
                )

    def logout(self):
        """Выход из системы"""
        reply = QMessageBox.question(
            self,
            "Подтверждение выхода",
            "Вы действительно хотите выйти?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
            QMessageBox.StandardButton.No,
        )

        if reply == QMessageBox.StandardButton.Yes:
            if self.db:
                self.db.close()
            # Сбрасываем состояние и показываем диалог входа снова
            self.reset_and_show_login()

    def reset_and_show_login(self):
        """Сбрасывает состояние окна и показывает диалог входа"""
        # Очищаем центральный виджет
        central_widget = self.centralWidget()
        if central_widget:
            central_widget.setParent(None)
            central_widget.deleteLater()

        # Очищаем меню
        menubar = self.menuBar()
        if menubar:
            menubar.clear()

        # Сбрасываем статус бар
        status_bar = self.statusBar()
        if status_bar:
            status_bar.clearMessage()

        # Сбрасываем состояние
        self.db = None
        self.current_user = None
        self.current_user_id = None
        self.is_admin = False

        # Убираем все атрибуты таблиц и других виджетов
        for attr in ['users_table', 'hubs_table', 'devices_table', 'table',
                     'filter_edit', 'clear_button', 'search_button',
                     'add_button', 'edit_button', 'delete_button', 'refresh_button',
                     'users_search_edit', 'hubs_search_edit', 'devices_search_edit']:
            if hasattr(self, attr):
                delattr(self, attr)

        # Показываем диалог входа снова
        self.show_login_dialog()

    def edit_profile(self):
        """Редактирование профиля пользователя"""
        if not self.db or not self.current_user_id:
            return

        # Get current user data
        try:
            with self.db.conn.cursor() as cur:
                cur.execute("SELECT username, email FROM users WHERE id = %s", (self.current_user_id,))
                user_data = cur.fetchone()
        except Exception as e:
            QMessageBox.critical(self, "Ошибка", f"Не удалось загрузить данные профиля: {str(e)}")
            return

        if not user_data:
            QMessageBox.critical(self, "Ошибка", "Пользователь не найден")
            return

        # Dialog for editing profile
        dialog = QDialog(self)
        dialog.setWindowTitle("Редактирование профиля")
        dialog.setModal(True)
        dialog.resize(350, 200)

        layout = QVBoxLayout(dialog)
        form = QFormLayout()

        username_edit = QLineEdit(user_data[0])
        email_edit = QLineEdit(user_data[1])
        password_edit = QLineEdit()
        password_edit.setEchoMode(QLineEdit.EchoMode.Password)
        password_edit.setPlaceholderText("Оставьте пустым, чтобы не менять")

        form.addRow("Логин:", username_edit)
        form.addRow("Email:", email_edit)
        form.addRow("Новый пароль:", password_edit)
        layout.addLayout(form)

        buttons = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        buttons.accepted.connect(dialog.accept)
        buttons.rejected.connect(dialog.reject)
        layout.addWidget(buttons)

        if dialog.exec() == QDialog.DialogCode.Accepted:
            username = username_edit.text().strip()
            email = email_edit.text().strip()
            password = password_edit.text()

            if not username or not email:
                QMessageBox.warning(self, "Ошибка", "Логин и email обязательны")
                return

            try:
                # Update user
                if password:
                    password_hash = Database.hash_password(password)
                    with self.db.conn.cursor() as cur:
                        cur.execute(
                            "UPDATE users SET username = %s, email = %s, password_hash = %s WHERE id = %s",
                            (username, email, password_hash, self.current_user_id)
                        )
                else:
                    with self.db.conn.cursor() as cur:
                        cur.execute(
                            "UPDATE users SET username = %s, email = %s WHERE id = %s",
                            (username, email, self.current_user_id)
                        )
                self.db.conn.commit()
                QMessageBox.information(self, "Успех", "Профиль обновлен")
                self.setWindowTitle(f"Управление устройствами - Пользователь: {username}")
            except Exception as e:
                QMessageBox.critical(self, "Ошибка", f"Не удалось обновить профиль: {str(e)}")

    def manage_hubs(self):
        """Управление хабами пользователя"""
        if not self.db or not self.current_user_id:
            return

        # Dialog with table of user's hubs
        dialog = QDialog(self)
        dialog.setWindowTitle("Управление хабами")
        dialog.setModal(True)
        dialog.resize(600, 400)

        layout = QVBoxLayout(dialog)

        table = QTableWidget()
        table.setColumnCount(4)
        table.setHorizontalHeaderLabels(["ID", "Название", "Местоположение", "Серийный номер"])
        table.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeMode.Stretch)

        # Load user's hubs with full data
        try:
            with self.db.conn.cursor() as cur:
                cur.execute("SELECT id, name, location, serial_number FROM hubs WHERE user_id = %s ORDER BY name", (self.current_user_id,))
                hubs = cur.fetchall()
            table.setRowCount(len(hubs))
            for row, (hub_id, hub_name, location, serial) in enumerate(hubs):
                table.setItem(row, 0, QTableWidgetItem(str(hub_id)))
                table.setItem(row, 1, QTableWidgetItem(hub_name))
                table.setItem(row, 2, QTableWidgetItem(location or ""))
                table.setItem(row, 3, QTableWidgetItem(serial or ""))
        except Exception as e:
            QMessageBox.critical(dialog, "Ошибка", f"Не удалось загрузить хабы: {str(e)}")
            return

        layout.addWidget(table)

        buttons = QHBoxLayout()
        edit_btn = QPushButton("Редактировать")
        edit_btn.clicked.connect(lambda: self.edit_user_hub(table, dialog))
        delete_btn = QPushButton("Удалить")
        delete_btn.clicked.connect(lambda: self.delete_user_hub(table, dialog))
        close_btn = QPushButton("Закрыть")
        close_btn.clicked.connect(dialog.accept)

        buttons.addWidget(edit_btn)
        buttons.addWidget(delete_btn)
        buttons.addStretch()
        buttons.addWidget(close_btn)
        layout.addLayout(buttons)

        dialog.exec()

    def edit_user_hub(self, table, parent_dialog):
        """Edit selected hub"""
        row = table.currentRow()
        if row < 0:
            QMessageBox.warning(parent_dialog, "Ошибка", "Выберите хаб")
            return

        hub_id = int(table.item(row, 0).text())
        name = table.item(row, 1).text()

        # Simple edit dialog
        dialog = QDialog(parent_dialog)
        dialog.setWindowTitle("Редактировать хаб")
        dialog.setModal(True)
        dialog.resize(350, 150)

        layout = QVBoxLayout(dialog)
        form = QFormLayout()

        name_edit = QLineEdit(name)
        location_edit = QLineEdit()
        # Показываем серийный номер как статический текст
        serial_label = QLabel()

        # Загружаем текущий серийный номер
        try:
            with self.db.conn.cursor() as cur:
                cur.execute("SELECT serial_number FROM hubs WHERE id = %s", (hub_id,))
                current_serial = cur.fetchone()
                if current_serial:
                    serial_label.setText(current_serial[0])
        except:
            serial_label.setText("Неизвестен")

        form.addRow("Название:", name_edit)
        form.addRow("Местоположение:", location_edit)
        form.addRow("Серийный номер:", serial_label)
        layout.addLayout(form)

        buttons = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        buttons.accepted.connect(dialog.accept)
        buttons.rejected.connect(dialog.reject)
        layout.addWidget(buttons)

        if dialog.exec() == QDialog.DialogCode.Accepted:
            new_name = name_edit.text().strip()
            location = location_edit.text().strip()
            # Используем текущий серийный номер из базы данных
            serial = serial_label.text()

            if not new_name:
                QMessageBox.warning(dialog, "Ошибка", "Название обязательно")
                return

            try:
                with self.db.conn.cursor() as cur:
                    cur.execute(
                        "UPDATE hubs SET name = %s, location = %s, serial_number = %s WHERE id = %s AND user_id = %s",
                        (new_name, location, serial, hub_id, self.current_user_id)
                    )
                self.db.conn.commit()
                QMessageBox.information(dialog, "Успех", "Хаб обновлен")
                # Refresh table
                table.item(row, 1).setText(new_name)
                table.item(row, 2).setText(location)
                table.item(row, 3).setText(serial)
            except Exception as e:
                QMessageBox.critical(dialog, "Ошибка", f"Не удалось обновить хаб: {str(e)}")

    def delete_user_hub(self, table, parent_dialog):
        """Delete selected hub"""
        row = table.currentRow()
        if row < 0:
            QMessageBox.warning(parent_dialog, "Ошибка", "Выберите хаб")
            return

        hub_id = int(table.item(row, 0).text())
        name = table.item(row, 1).text()

        reply = QMessageBox.question(
            parent_dialog, "Подтверждение",
            f"Удалить хаб '{name}'? Все устройства в этом хабе также будут удалены.",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
        )

        if reply == QMessageBox.StandardButton.Yes:
            try:
                with self.db.conn.cursor() as cur:
                    cur.execute("DELETE FROM hubs WHERE id = %s AND user_id = %s", (hub_id, self.current_user_id))
                self.db.conn.commit()
                QMessageBox.information(parent_dialog, "Успех", "Хаб удален")
                table.removeRow(row)
            except Exception as e:
                QMessageBox.critical(parent_dialog, "Ошибка", f"Не удалось удалить хаб: {str(e)}")

    # Admin methods
    def refresh_admin_data(self):
        """Refresh all admin tables"""
        try:
            # Users
            users = self.db.get_all_users()
            self.users_table.setRowCount(0)  # Clear table first
            self.users_table.setRowCount(len(users))
            for row, (user_id, username, email, is_admin) in enumerate(users):
                self.users_table.setItem(row, 0, QTableWidgetItem(str(user_id)))
                self.users_table.setItem(row, 1, QTableWidgetItem(username))
                self.users_table.setItem(row, 2, QTableWidgetItem(email))
                self.users_table.setItem(row, 3, QTableWidgetItem("Да" if is_admin else "Нет"))

            # Hubs
            hubs = self.db.get_all_hubs()
            self.hubs_table.setRowCount(0)  # Clear table first
            self.hubs_table.setRowCount(len(hubs))
            for row, (hub_id, user_id, username, name, location, serial) in enumerate(hubs):
                self.hubs_table.setItem(row, 0, QTableWidgetItem(str(hub_id)))
                self.hubs_table.setItem(row, 1, QTableWidgetItem(username))
                self.hubs_table.setItem(row, 2, QTableWidgetItem(name))
                self.hubs_table.setItem(row, 3, QTableWidgetItem(location))
                self.hubs_table.setItem(row, 4, QTableWidgetItem(serial))

            # Devices
            devices = self.db.get_all_devices()
            self.devices_table.setRowCount(0)  # Clear table first
            for device in devices:
                row = self.devices_table.rowCount()
                self.devices_table.insertRow(row)
                device_id, hub_id, hub_name, name, type_name, status = device
                self.devices_table.setItem(row, 0, QTableWidgetItem(str(device_id)))
                self.devices_table.setItem(row, 1, QTableWidgetItem(str(hub_name) if hub_name else "Хаб удален"))
                self.devices_table.setItem(row, 2, QTableWidgetItem(str(name) if name else ""))
                self.devices_table.setItem(row, 3, QTableWidgetItem(str(type_name) if type_name else "Тип неизвестен"))
                self.devices_table.setItem(row, 4, QTableWidgetItem(str(status) if status else ""))

        except Exception as e:
            QMessageBox.critical(self, "Ошибка", f"Не удалось загрузить данные: {str(e)}")

    def add_user(self):
        """Add new user"""
        dialog = UserDialog(db=self.db)
        if dialog.exec() == QDialog.DialogCode.Accepted:
            data = dialog.get_data()

            if not data["username"] or not data["email"] or not data["password"]:
                QMessageBox.warning(self, "Ошибка", "Заполните все поля")
                return

            try:
                password_hash = Database.hash_password(data["password"])
                self.db.create_user(data["username"], data["email"], password_hash)
                self.refresh_admin_data()
                QMessageBox.information(self, "Успех", "Пользователь добавлен")
            except Exception as e:
                QMessageBox.critical(self, "Ошибка", f"Не удалось добавить пользователя: {str(e)}")

    def edit_user(self):
        """Edit selected user"""
        row = self.users_table.currentRow()
        if row < 0:
            QMessageBox.warning(self, "Ошибка", "Выберите пользователя")
            return

        user_id = int(self.users_table.item(row, 0).text())
        old_username = self.users_table.item(row, 1).text()
        old_email = self.users_table.item(row, 2).text()
        old_admin = self.users_table.item(row, 3).text() == "Да"

        dialog = QDialog(self)
        dialog.setWindowTitle("Редактировать пользователя")
        dialog.setModal(True)
        dialog.resize(350, 150)

        layout = QVBoxLayout(dialog)
        form = QFormLayout()
        username_edit = QLineEdit(old_username)
        email_edit = QLineEdit(old_email)
        admin_check = QCheckBox("Администратор")
        admin_check.setChecked(old_admin)

        form.addRow("Логин:", username_edit)
        form.addRow("Email:", email_edit)
        form.addRow(admin_check)
        layout.addLayout(form)

        buttons = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        buttons.accepted.connect(dialog.accept)
        buttons.rejected.connect(dialog.reject)
        layout.addWidget(buttons)

        if dialog.exec() == QDialog.DialogCode.Accepted:
            username = username_edit.text().strip()
            email = email_edit.text().trimmed()
            is_admin = admin_check.isChecked()

            if not username or not email:
                QMessageBox.warning(dialog, "Ошибка", "Заполните все поля")
                return

            try:
                self.db.update_user(user_id, username, email, is_admin)
                self.refresh_admin_data()
                QMessageBox.information(dialog, "Успех", "Пользователь обновлен")
            except Exception as e:
                QMessageBox.critical(dialog, "Ошибка", f"Не удалось обновить пользователя: {str(e)}")

    def delete_user(self):
        """Delete selected user"""
        row = self.users_table.currentRow()
        if row < 0:
            QMessageBox.warning(self, "Ошибка", "Выберите пользователя")
            return

        user_id = int(self.users_table.item(row, 0).text())
        username = self.users_table.item(row, 1).text()

        reply = QMessageBox.question(self, "Подтверждение", f"Удалить пользователя '{username}'?")
        if reply == QMessageBox.StandardButton.Yes:
            try:
                self.db.delete_user(user_id)
                self.refresh_admin_data()
                QMessageBox.information(self, "Успех", "Пользователь удален")
            except Exception as e:
                QMessageBox.critical(self, "Ошибка", f"Не удалось удалить пользователя: {str(e)}")

    def add_admin_hub(self):
        """Add hub as admin"""
        dialog = HubAdminDialog(db=self.db)
        if dialog.exec() == QDialog.DialogCode.Accepted:
            data = dialog.get_data()

            if not data["name"] or not data["location"] or not data["serial_number"]:
                QMessageBox.warning(self, "Ошибка", "Заполните все поля")
                return

            try:
                self.db.create_hub(data["user_id"], data["name"], data["location"], data["serial_number"])
                self.refresh_admin_data()
                QMessageBox.information(self, "Успех", "Хаб добавлен")
            except Exception as e:
                QMessageBox.critical(self, "Ошибка", f"Не удалось добавить хаб: {str(e)}")

    def edit_hub(self):
        """Edit selected hub"""
        row = self.hubs_table.currentRow()
        if row < 0:
            QMessageBox.warning(self, "Ошибка", "Выберите хаб")
            return

        hub_id = int(self.hubs_table.item(row, 0).text())
        old_username = self.hubs_table.item(row, 1).text()
        old_name = self.hubs_table.item(row, 2).text()
        old_location = self.hubs_table.item(row, 3).text()
        old_serial = self.hubs_table.item(row, 4).text()

        # Get user_id from username
        users = self.db.get_all_users()
        old_user_id = None
        for uid, uname, email, is_admin in users:
            if uname == old_username:
                old_user_id = uid
                break

        if old_user_id is None:
            QMessageBox.critical(self, "Ошибка", "Не удалось найти пользователя")
            return

        dialog = HubAdminDialog(db=self.db, hub_data=(hub_id, old_user_id, old_username, old_name, old_location, old_serial))
        if dialog.exec() == QDialog.DialogCode.Accepted:
            data = dialog.get_data()

            if not data["name"] or not data["location"] or not data["serial_number"]:
                QMessageBox.warning(self, "Ошибка", "Заполните все поля")
                return

            try:
                self.db.update_hub(hub_id, data["user_id"], data["name"], data["location"], data["serial_number"])
                self.refresh_admin_data()
                QMessageBox.information(self, "Успех", "Хаб обновлен")
            except Exception as e:
                QMessageBox.critical(self, "Ошибка", f"Не удалось обновить хаб: {str(e)}")

    def delete_hub(self):
        """Delete selected hub"""
        row = self.hubs_table.currentRow()
        if row < 0:
            QMessageBox.warning(self, "Ошибка", "Выберите хаб")
            return

        hub_id = int(self.hubs_table.item(row, 0).text())
        name = self.hubs_table.item(row, 2).text()

        reply = QMessageBox.question(self, "Подтверждение", f"Удалить хаб '{name}'?")
        if reply == QMessageBox.StandardButton.Yes:
            try:
                self.db.delete_hub(hub_id)
                self.refresh_admin_data()
                QMessageBox.information(self, "Успех", "Хаб удален")
            except Exception as e:
                QMessageBox.critical(self, "Ошибка", f"Не удалось удалить хаб: {str(e)}")

    def add_admin_device(self):
        """Add device as admin"""
        dialog = DeviceAdminDialog(db=self.db)
        if dialog.exec() == QDialog.DialogCode.Accepted:
            data = dialog.get_data()

            if not data["name"]:
                QMessageBox.warning(self, "Ошибка", "Введите название")
                return

            try:
                self.db.save_device(None, data["hub_id"], data["type_id"], data["name"], data["status"])
                self.refresh_admin_data()
                QMessageBox.information(self, "Успех", "Устройство добавлено")
            except Exception as e:
                QMessageBox.critical(self, "Ошибка", f"Не удалось добавить устройство: {str(e)}")

    def edit_admin_device(self):
        """Edit device as admin"""
        row = self.devices_table.currentRow()
        if row < 0:
            QMessageBox.warning(self, "Ошибка", "Выберите устройство")
            return

        item = self.devices_table.item(row, 0)
        if item is None:
            QMessageBox.warning(self, "Ошибка", "Выберите устройство с данными")
            return

        device_id = int(item.text())
        hub_name = self.devices_table.item(row, 1).text()
        name = self.devices_table.item(row, 2).text()
        type_name = self.devices_table.item(row, 3).text()
        status = self.devices_table.item(row, 4).text()

        # Get hub_id from hub_name
        try:
            with self.db.conn.cursor() as cur:
                cur.execute("SELECT id FROM hubs WHERE name = %s", (hub_name,))
                hub_result = cur.fetchone()
                hub_id = hub_result[0] if hub_result else None
        except:
            hub_id = None

        # Get type_id from type_name
        try:
            with self.db.conn.cursor() as cur:
                cur.execute("SELECT id FROM device_types WHERE type_name = %s", (type_name,))
                type_result = cur.fetchone()
                type_id = type_result[0] if type_result else None
        except:
            type_id = None

        if hub_id is None or type_id is None:
            QMessageBox.critical(self, "Ошибка", "Не удалось найти хаб или тип")
            return

        dialog = DeviceAdminDialog(db=self.db, device_data=(device_id, hub_id, hub_name, name, type_id, status))
        if dialog.exec() == QDialog.DialogCode.Accepted:
            data = dialog.get_data()

            if not data["name"]:
                QMessageBox.warning(self, "Ошибка", "Введите название")
                return

            try:
                self.db.update_device(device_id, data["hub_id"], data["type_id"], data["name"], data["status"])
                self.refresh_admin_data()
                QMessageBox.information(self, "Успех", "Устройство обновлено")
            except Exception as e:
                QMessageBox.critical(self, "Ошибка", f"Не удалось обновить устройство: {str(e)}")

    def delete_admin_device(self):
        """Delete device as admin"""
        row = self.devices_table.currentRow()
        if row < 0:
            QMessageBox.warning(self, "Ошибка", "Выберите устройство")
            return

        device_id = int(self.devices_table.item(row, 0).text())
        name = self.devices_table.item(row, 2).text()

        reply = QMessageBox.question(self, "Подтверждение", f"Удалить устройство '{name}'?")
        if reply == QMessageBox.StandardButton.Yes:
            try:
                self.db.delete_device_admin(device_id)
                self.refresh_admin_data()
                QMessageBox.information(self, "Успех", "Устройство удалено")
            except Exception as e:
                QMessageBox.critical(self, "Ошибка", f"Не удалось удалить устройство: {str(e)}")

    def refresh_users_table(self):
        """Refresh only the users table"""
        try:
            users = self.db.get_all_users()
            self.users_table.setRowCount(0)
            self.users_table.setRowCount(len(users))
            for row, (user_id, username, email, is_admin) in enumerate(users):
                self.users_table.setItem(row, 0, QTableWidgetItem(str(user_id)))
                self.users_table.setItem(row, 1, QTableWidgetItem(username))
                self.users_table.setItem(row, 2, QTableWidgetItem(email))
                self.users_table.setItem(row, 3, QTableWidgetItem("Да" if is_admin else "Нет"))
        except Exception as e:
            QMessageBox.critical(self, "Ошибка", f"Не удалось обновить таблицу пользователей: {str(e)}")

    def refresh_hubs_table(self):
        """Refresh only the hubs table"""
        try:
            hubs = self.db.get_all_hubs()
            self.hubs_table.setRowCount(0)
            self.hubs_table.setRowCount(len(hubs))
            for row, (hub_id, user_id, username, name, location, serial) in enumerate(hubs):
                self.hubs_table.setItem(row, 0, QTableWidgetItem(str(hub_id)))
                self.hubs_table.setItem(row, 1, QTableWidgetItem(username))
                self.hubs_table.setItem(row, 2, QTableWidgetItem(name))
                self.hubs_table.setItem(row, 3, QTableWidgetItem(location))
                self.hubs_table.setItem(row, 4, QTableWidgetItem(serial))
        except Exception as e:
            QMessageBox.critical(self, "Ошибка", f"Не удалось обновить таблицу хабов: {str(e)}")

    def refresh_devices_table(self):
        """Refresh only the devices table"""
        try:
            devices = self.db.get_all_devices()
            self.devices_table.setRowCount(0)
            for device in devices:
                row = self.devices_table.rowCount()
                self.devices_table.insertRow(row)
                device_id, hub_id, hub_name, name, type_name, status = device
                self.devices_table.setItem(row, 0, QTableWidgetItem(str(device_id)))
                self.devices_table.setItem(row, 1, QTableWidgetItem(str(hub_name) if hub_name else "Хаб удален"))
                self.devices_table.setItem(row, 2, QTableWidgetItem(str(name) if name else ""))
                self.devices_table.setItem(row, 3, QTableWidgetItem(str(type_name) if type_name else "Тип неизвестен"))
                self.devices_table.setItem(row, 4, QTableWidgetItem(str(status) if status else ""))
        except Exception as e:
            QMessageBox.critical(self, "Ошибка", f"Не удалось обновить таблицу устройств: {str(e)}")

    # Search methods for admin panel
    def search_users(self):
        """Search users in admin panel"""
        try:
            filter_text = self.users_search_edit.text().strip()
            if filter_text:
                users = self.db.get_users_filtered(filter_text)
            else:
                users = self.db.get_all_users()

            self.users_table.setRowCount(0)
            self.users_table.setRowCount(len(users))
            for row, (user_id, username, email, is_admin) in enumerate(users):
                self.users_table.setItem(row, 0, QTableWidgetItem(str(user_id)))
                self.users_table.setItem(row, 1, QTableWidgetItem(username))
                self.users_table.setItem(row, 2, QTableWidgetItem(email))
                self.users_table.setItem(row, 3, QTableWidgetItem("Да" if is_admin else "Нет"))
        except Exception as e:
            QMessageBox.critical(self, "Ошибка", f"Не удалось выполнить поиск пользователей: {str(e)}")

    def clear_users_search(self):
        """Clear users search and show all users"""
        self.users_search_edit.clear()
        self.search_users()

    def search_hubs(self):
        """Search hubs in admin panel"""
        try:
            filter_text = self.hubs_search_edit.text().strip()
            if filter_text:
                hubs = self.db.get_hubs_filtered(filter_text)
            else:
                hubs = self.db.get_all_hubs()

            self.hubs_table.setRowCount(0)
            self.hubs_table.setRowCount(len(hubs))
            for row, (hub_id, user_id, username, name, location, serial) in enumerate(hubs):
                self.hubs_table.setItem(row, 0, QTableWidgetItem(str(hub_id)))
                self.hubs_table.setItem(row, 1, QTableWidgetItem(username))
                self.hubs_table.setItem(row, 2, QTableWidgetItem(name))
                self.hubs_table.setItem(row, 3, QTableWidgetItem(location))
                self.hubs_table.setItem(row, 4, QTableWidgetItem(serial))
        except Exception as e:
            QMessageBox.critical(self, "Ошибка", f"Не удалось выполнить поиск хабов: {str(e)}")

    def clear_hubs_search(self):
        """Clear hubs search and show all hubs"""
        self.hubs_search_edit.clear()
        self.search_hubs()

    def search_devices(self):
        """Search devices in admin panel"""
        try:
            filter_text = self.devices_search_edit.text().strip()
            if filter_text:
                devices = self.db.get_devices_filtered(filter_text)
            else:
                devices = self.db.get_all_devices()

            self.devices_table.setRowCount(0)
            for device in devices:
                row = self.devices_table.rowCount()
                self.devices_table.insertRow(row)
                device_id, hub_id, hub_name, name, type_name, status = device
                self.devices_table.setItem(row, 0, QTableWidgetItem(str(device_id)))
                self.devices_table.setItem(row, 1, QTableWidgetItem(str(hub_name) if hub_name else "Хаб удален"))
                self.devices_table.setItem(row, 2, QTableWidgetItem(str(name) if name else ""))
                self.devices_table.setItem(row, 3, QTableWidgetItem(str(type_name) if type_name else "Тип неизвестен"))
                self.devices_table.setItem(row, 4, QTableWidgetItem(str(status) if status else ""))
        except Exception as e:
            QMessageBox.critical(self, "Ошибка", f"Не удалось выполнить поиск устройств: {str(e)}")

    def clear_devices_search(self):
        """Clear devices search and show all devices"""
        self.devices_search_edit.clear()
        self.search_devices()

    def closeEvent(self, event):
        """Обработчик закрытия окна"""
        if self.db:
            self.db.close()
        event.accept()


def main():
    """Главная функция"""
    app = QApplication(sys.argv)

    # Устанавливаем стиль
    app.setStyle("Fusion")

    # Создаем главное окно (вход будет показан автоматически)
    window = MainWindow()
    window.show()

    sys.exit(app.exec())


if __name__ == "__main__":
    main()
