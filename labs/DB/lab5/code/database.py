import psycopg2
from psycopg2 import sql
import hashlib
import os

class Database:
    def __init__(self):
        self.conn = psycopg2.connect(
            host="localhost",
            port=5433,
            database="pozordom",
            user="pozordom_user",
            password="pozordom_pass"
        )
        self.conn.autocommit = True

    def close(self):
        self.conn.close()

    def get_hubs(self):
        """Получить список хабов: [(id, name), ...]"""
        with self.conn.cursor() as cur:
            cur.execute("SELECT id, name FROM hubs ORDER BY name")
            return cur.fetchall()

    def get_devices(self, filter_text=""):
        """Получить устройства с фильтрацией по имени"""
        query = """
            SELECT d.id, d.name, dt.type_name, h.name AS hub_name
            FROM devices d
            JOIN device_types dt ON d.type_id = dt.id
            JOIN hubs h ON d.hub_id = h.id
            WHERE d.name ILIKE %s
            ORDER BY d.id
        """
        with self.conn.cursor() as cur:
            cur.execute(query, (f"%{filter_text}%",))
            return cur.fetchall()

    def save_device(self, device_id, hub_id, type_id, name, status):
        """Вызов функции save_devices"""
        with self.conn.cursor() as cur:
            cur.callproc('save_devices', [device_id, hub_id, type_id, name, status])
            return cur.fetchone()[0]

    def delete_device(self, device_id):
        """Удаление напрямую (или через функцию, если реализована)"""
        with self.conn.cursor() as cur:
            cur.execute("DELETE FROM devices WHERE id = %s", (device_id,))

    def device_exists(self, name, exclude_id=None):
        """Проверка дубликата имени"""
        query = "SELECT 1 FROM devices WHERE name = %s"
        params = [name]
        if exclude_id:
            query += " AND id != %s"
            params.append(exclude_id)
        with self.conn.cursor() as cur:
            cur.execute(query, params)
            return cur.fetchone() is not None

    def get_user_by_credentials(self, username, password_hash):
        """Получить пользователя по логину и паролю"""
        with self.conn.cursor() as cur:
            cur.execute("""
                SELECT id, username, email FROM users
                WHERE username = %s AND password_hash = %s
            """, (username, password_hash))
            return cur.fetchone()

    def create_user(self, username, email, password_hash):
        """Создать нового пользователя"""
        with self.conn.cursor() as cur:
            cur.execute("""
                INSERT INTO users (username, email, password_hash)
                VALUES (%s, %s, %s)
                RETURNING id
            """, (username, email, password_hash))
            return cur.fetchone()[0]

    def get_user_hubs(self, user_id):
        """Получить хабы пользователя"""
        with self.conn.cursor() as cur:
            cur.execute("""
                SELECT id, name FROM hubs WHERE user_id = %s ORDER BY name
            """, (user_id,))
            return cur.fetchall()

    def delete_device_safe(self, device_id):
        """Безопасное удаление устройства с очисткой логов"""
        with self.conn.cursor() as cur:
            # Сначала удаляем записи из log_devices
            cur.execute("DELETE FROM log_devices WHERE device_id = %s", (device_id,))
            # Затем удаляем само устройство
            cur.execute("DELETE FROM devices WHERE id = %s", (device_id,))

    def get_device_types(self):
        with self.conn.cursor() as cur:
            cur.execute("SELECT id, type_name FROM device_types ORDER BY type_name")
            return cur.fetchall()

    @staticmethod
    def hash_password(password):
        """Хэширование пароля с солью"""
        salt = os.urandom(32)
        pwdhash = hashlib.pbkdf2_hmac('sha256', password.encode('utf-8'), salt, 100000)
        return salt + pwdhash

    @staticmethod
    def verify_password(password, stored_hash):
        """Проверка пароля"""
        try:
            salt = stored_hash[:32]
            stored_password_hash = stored_hash[32:]
            pwdhash = hashlib.pbkdf2_hmac('sha256', password.encode('utf-8'), salt, 100000)
            return pwdhash == stored_password_hash
        except Exception:
            return False

    def get_devices_for_user(self, user_id, filter_text=""):
        """Получить устройства пользователя с фильтрацией по имени"""
        query = """
            SELECT d.id, d.name, dt.type_name, h.name AS hub_name
            FROM devices d
            JOIN device_types dt ON d.type_id = dt.id
            JOIN hubs h ON d.hub_id = h.id
            WHERE h.user_id = %s AND d.name ILIKE %s
            ORDER BY d.id
        """
        with self.conn.cursor() as cur:
            cur.execute(query, (user_id, f"%{filter_text}%"))
            return cur.fetchall()

    def get_user_devices_count(self, user_id):
        """Получить количество устройств пользователя"""
        with self.conn.cursor() as cur:
            cur.execute("""
                SELECT COUNT(*) FROM devices d
                JOIN hubs h ON d.hub_id = h.id
                WHERE h.user_id = %s
            """, (user_id,))
            return cur.fetchone()[0]
