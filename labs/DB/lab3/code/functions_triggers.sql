-- 1. функция save_devices: вставка или обновление устройства
create or replace function save_devices(
  _id bigint,
  _hub_id int,
  _type_id int,
  _name varchar(100),
  _status jsonb
)
returns bigint as $$
declare
  used_id bigint;
begin
  if _id is null then
    insert into devices (hub_id, type_id, name, status)
    values (_hub_id, _type_id, _name, _status)
    returning id into used_id;
  else
    update devices
    set hub_id = _hub_id,
      type_id = _type_id,
      name = _name,
      status = _status
    where id = _id;
    used_id := _id;
  end if;
  return used_id;
end;
$$ language plpgsql;

-- 2. функция delete_hubs с проверкой внешних ссылок
alter table devices drop constraint if exists devices_hub_id_fkey;
alter table devices add constraint devices_hub_id_fkey
  foreign key (hub_id) references hubs(id) on delete restrict;

create or replace function delete_hubs(_id bigint)
returns void as $$
begin
  delete from hubs where id = _id;
exception
  when foreign_key_violation then
    raise exception 'невозможно выполнить удаление, так как есть внешние ссылки.';
end;
$$ language plpgsql;

-- 3. функция фильтрации устройств по id
create or replace function filter_devices_by_id(min_id bigint)
returns setof devices as $$
begin
  return query
  select * from devices where id >= min_id
  order by id;
end;
$$ language plpgsql;

-- 4. составной тип и функция фильтрации массива
create type device_info as (
  id bigint,
  name varchar(100),
  type_name varchar(50),
  hub_name varchar(100)
);

create or replace function filter_device_array(
  arr device_info[],
  min_id bigint
)
returns device_info[] as $$
begin
  return array(
    select (id, name, type_name, hub_name)::device_info
    from unnest(arr)
    where id >= min_id
  );
end;
$$ language plpgsql;

-- 5. таблица лога и триггер
create table if not exists log_devices (
  id bigserial primary key,
  device_id bigint references devices(id),
  change_time timestamp default now(),
  old_name varchar(100),
  new_name varchar(100)
);

create or replace function log_device_change()
returns trigger as $$
begin
  if tg_op = 'insert' then
    insert into log_devices (device_id, new_name)
    values (new.id, new.name);
  elsif tg_op = 'update' then
    insert into log_devices (device_id, old_name, new_name)
    values (new.id, old.name, new.name);
  end if;
  return new;
end;
$$ language plpgsql;

drop trigger if exists trigger_log_devices on devices;
create trigger trigger_log_devices
after insert or update on devices
for each row
execute function log_device_change();

-- 6. функция с динамическим SQL
create or replace function get_column_value(
  table_name text,
  column_name text,
  record_id bigint
)
returns text as $$
declare
  result text;
begin
  execute format('select %I from %I where id = $1', column_name, table_name)
  into result
  using record_id;
  return result;
end;
$$ language plpgsql;
