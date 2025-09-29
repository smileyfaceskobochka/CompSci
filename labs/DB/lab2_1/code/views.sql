-- 1. Представление устройств с их хабами и владельцами
create view device_full_info as
select d.id as device_id,
       d.name as device_name,
       dt.type_name,
       h.name as hub_name,
       u.username as owner
from devices d
join device_types dt on d.type_id = dt.id
join hubs h on d.hub_id = h.id
join users u on h.user_id = u.id;

-- 2. Представление событий с инфо об устройствах
create view event_info as
select e.id as event_id,
       e.event_type,
       e.event_data,
       d.name as device_name,
       h.name as hub_name
from events e
join devices d on e.device_id = d.id
join hubs h on d.hub_id = h.id;

-- 3. Статистика по числовому столбцу (id устройств)
create view device_id_stats as
select 'Минимальное значение' as metric,
       min(id)::text as value,
       (select id from devices order by id asc limit 1) as ref_id
from devices
union all
select 'Максимальное значение',
       max(id)::text,
       (select id from devices order by id desc limit 1)
from devices
union all
select 'Среднее значение',
       round(avg(id),2)::text,
       null
from devices
union all
select 'Сумма значений',
       sum(id)::text,
       null
from devices;
