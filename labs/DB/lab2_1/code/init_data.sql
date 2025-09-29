-- Лаб 2.1 — наполнение БД

-- Добавляем пользователей
insert into users(username, email, password_hash)
values 
('lisachenko', 'me@lsnko.com', 'hash1'),
('bobby', 'bob@example.com', 'hash2'),
('alice', 'alice@example.com', 'hash3');

-- Добавляем хабы
insert into hubs(user_id, name, location, serial_number)
values
(1, 'Living Room Hub', 'Living Room', 'HUB-001'),
(1, 'Kitchen Hub', 'Kitchen', 'HUB-002'),
(2, 'Bedroom Hub', 'Bedroom', 'HUB-003');

-- Типы устройств
insert into device_types(type_name)
values
('Lamp'),
('Thermometer'),
('Camera');

-- Устройства
insert into devices(hub_id, type_id, name, status)
values
(1, 1, 'Ceiling Lamp', '{"power": "off"}'),
(1, 2, 'ThermoSensor', '{"temperature": 22.5}'),
(2, 1, 'Kitchen Lamp', '{"power": "on"}'),
(3, 3, 'Security Camera', '{"status": "active"}');

-- События
insert into events(device_id, event_type, event_data)
values
(1, 'switch_on', '{"power": "on"}'),
(2, 'temperature_change', '{"temperature": 23.0}'),
(3, 'switch_off', '{"power": "off"}'),
(4, 'motion_detected', '{"movement": true}');

-- UPDATE-примеры

-- пользователь "alice"
update users
set email = 'alice@newmail.com'
where username = 'alice';

-- Включим лампу в гостиной
update devices
set status = '{"power": "on"}'
where name = 'Ceiling Lamp';

-- DELETE-примеры

-- Удалим выключение
delete from events
where event_type = 'switch_off';
