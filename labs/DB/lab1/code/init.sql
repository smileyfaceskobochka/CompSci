-- Таблица пользователей
create table users (
  id serial primary key,                -- Уникальный идентификатор пользователя
  username varchar(50) unique not null, -- Логин пользователя (уникальный)
  email varchar(100) unique not null,   -- Электронная почта (уникальная)
  password_hash varchar(255) not null,  -- Хэш пароля
  created_at timestamp default now()    -- Дата и время создания записи
);

-- Таблица хабов
create table hubs (
  id serial primary key,                      -- Уникальный идентификатор хаба
  -- Владелец хаба (связь с пользователем), при удалении пользователя хабы тоже удаляются
  user_id int not null references users(id) on delete cascade, 
  name varchar(100) not null,                 -- Название хаба
  location varchar(100),                      -- Местоположение хаба
  serial_number varchar(100) unique not null, -- Серийный номер хаба (уникальный)
  created_at timestamp default now()          -- Дата и время создания хаба
);

-- Таблица типов устройств (лампы, датчики и т.д.)
create table device_types (
  id serial primary key,                -- Уникальный идентификатор типа устройства
  type_name varchar(50) unique not null -- Название типа устройства
);

-- Таблица устройств, подключённых к хабам
create table devices (
  id serial primary key,      -- Уникальный идентификатор устройства
  -- Связь с хабом, при удалении хаба устройство удаляется
  hub_id int not null references hubs(id) on delete cascade,
  -- Тип устройства, нельзя удалить тип если есть устройства этого типа
  type_id int not null references device_types(id) on delete restrict,
  name varchar(100) not null, -- Название устройства
  status JSONB default '{}'   -- Текущий статус устройства в JSON {"power": "on"}
);

-- Таблица событий, генерируемых устройствами
create table events (
  id serial primary key,             -- Уникальный идентификатор события
  -- Связь с устройством, при удалении устройства события удаляются
  device_id int not null references devices(id) on delete cascade,
  event_type varchar(50) not null,   -- Тип события ("switch_on", "temperature_change")
  event_data JSONB default '{}',     -- Данные события в JSON {"temperature": 22.5}
  created_at timestamp default now() -- Дата и время создания события
);
