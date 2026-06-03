-- ============================================================
-- Consolidated init script for shared PostgreSQL across Lab2-4
-- Merged from Flyway migrations V1–V5
-- ============================================================

-- V1: Core schema
CREATE TABLE IF NOT EXISTS racing_series (
    id BIGSERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    official_name VARCHAR(512),
    headquarters VARCHAR(255) NOT NULL,
    governing_body VARCHAR(255),
    founded_year INTEGER,
    rounds INTEGER
);

CREATE TABLE IF NOT EXISTS racing_teams (
    id BIGSERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    principal_name VARCHAR(255) NOT NULL,
    base_location VARCHAR(255) NOT NULL,
    power_unit VARCHAR(255),
    points INTEGER NOT NULL DEFAULT 0,
    series_id BIGINT NOT NULL REFERENCES racing_series(id)
);

CREATE TABLE IF NOT EXISTS racing_drivers (
    id BIGSERIAL PRIMARY KEY,
    first_name VARCHAR(255) NOT NULL,
    last_name VARCHAR(255) NOT NULL,
    racing_number INTEGER NOT NULL,
    points INTEGER NOT NULL DEFAULT 0,
    team_id BIGINT NOT NULL REFERENCES racing_teams(id)
);

-- V1: Series — F1, F2, FE
INSERT INTO racing_series (name, official_name, headquarters, governing_body, founded_year, rounds) VALUES
('Formula 1',
 'FIA Formula One World Championship',
 'Paris, France',
 'Fédération Internationale de l''Automobile (FIA)',
 1950, 24),
('Formula 2',
 'FIA Formula 2 Championship',
 'Paris, France',
 'Fédération Internationale de l''Automobile (FIA)',
 2017, 14),
('Formula E',
 'ABB FIA Formula E World Championship',
 'Geneva, Switzerland',
 'Fédération Internationale de l''Automobile (FIA)',
 2014, 16);

-- V1: F1 Teams (series_id=1)
INSERT INTO racing_teams (name, principal_name, base_location, power_unit, points, series_id) VALUES
('McLaren F1 Team',      'Andrea Stella',    'Woking, UK',              'Mercedes',  291, 1),
('Ferrari',              'Fred Vasseur',     'Maranello, Italy',        'Ferrari',   249, 1),
('Red Bull Racing',      'Christian Horner', 'Milton Keynes, UK',       'Ford RBPT', 206, 1),
('Mercedes-AMG Petronas','Toto Wolff',       'Brackley, UK',            'Mercedes',  138, 1),
('Aston Martin Aramco',  'Mike Krack',       'Silverstone, UK',         'Honda',      96, 1),
('Haas F1 Team',         'Ayao Komatsu',     'Kannapolis, USA',         'Ferrari',    58, 1),
('Alpine F1 Team',       'Oliver Oakes',     'Enstone, UK',             'Renault',    49, 1),
('Williams Racing',      'James Vowles',     'Grove, UK',               'Mercedes',   17, 1),
('Racing Bulls',         'Laurent Mekies',   'Faenza, Italy',           'Ford RBPT',  14, 1),
('Cadillac Racing',      'Michael Andretti', 'Fishers, USA',            'Cadillac',    6, 1),
('Audi F1 Team',         'Mattia Binotto',   'Hinwil, Switzerland',     'Audi',        4, 1);

-- V1: F2 Teams (series_id=2)
INSERT INTO racing_teams (name, principal_name, base_location, points, series_id) VALUES
('Prema Racing',         'René Rosin',        'Grisignano, Italy',       89, 2),
('Hitech Grand Prix',    'Oliver Oakes',      'Silverstone, UK',         74, 2),
('ART Grand Prix',       'Sébastien Philippe','Villeneuve-la-Garenne, France', 58, 2),
('DAMS',                 'François Sicard',   'Le Mans, France',         41, 2);

-- V1: FE Teams (series_id=3)
INSERT INTO racing_teams (name, principal_name, base_location, points, series_id) VALUES
('Jaguar TCS Racing',          'James Barclay',      'Silverstone, UK',      201, 3),
('Porsche TAG Heuer Formula E', 'Florian Modlinger', 'Weissach, Germany',    178, 3),
('DS Penske',                  'Mark Preston',       'Versailles, France',   134, 3),
('Nissan Formula E Team',      'Tommaso Volpe',      'Yokohama, Japan',      98, 3);

-- V1: F1 Drivers (team IDs: 1-11)
INSERT INTO racing_drivers (first_name, last_name, racing_number, points, team_id) VALUES
('Oscar',        'Piastri',    81, 168, 1),
('Lando',        'Norris',      4, 123, 1),
('Charles',      'Leclerc',    16, 141, 2),
('Lewis',        'Hamilton',   44, 108, 2),
('Max',          'Verstappen',  1, 127, 3),
('Isack',        'Hadjar',      6,  79, 3),
('George',       'Russell',    63,  98, 4),
('Kimi',         'Antonelli',  12,  40, 4),
('Fernando',     'Alonso',     14,  62, 5),
('Lance',        'Stroll',     18,  34, 5),
('Esteban',      'Ocon',       31,  33, 6),
('Oliver',       'Bearman',    87,  25, 6),
('Pierre',       'Gasly',      10,  28, 7),
('Franco',       'Colapinto',  43,  21, 7),
('Alexander',    'Albon',      23,  10, 8),
('Carlos',       'Sainz',      55,   7, 8),
('Liam',         'Lawson',     30,   9, 9),
('Arvid',        'Lindblad',    6,   5, 9),
('Sergio',       'Perez',      11,   6, 10),
('Valtteri',     'Bottas',     77,   0, 10),
('Nico',         'Hulkenberg', 27,   4, 11),
('Gabriel',      'Bortoleto',   5,   0, 11);

-- V1: F2 Drivers (team IDs: 12-15)
INSERT INTO racing_drivers (first_name, last_name, racing_number, points, team_id) VALUES
('Paul',           'Aron',       7,  56, 12),
('Ugo',         'Ugochukwu',    3,  33, 12),
('Jak',          'Crawford',    8,  44, 13),
('Isack',          'Hadjar',    6,  30, 13),
('James',         'Wharton',    1,  35, 14),
('Tuukka',        'Taponen',    2,  23, 14),
('Oliver',        'Bearman',    5,  28, 15),
('Victor',         'Martins',   6,  13, 15);

-- V1: FE Drivers (team IDs: 16-19)
INSERT INTO racing_drivers (first_name, last_name, racing_number, points, team_id) VALUES
('Nick',         'Cassidy',    37, 118, 16),
('Mitch',          'Evans',     9,  83, 16),
('Pascal',       'Wehrlein',   94, 111, 17),
('Antonio Felix','da Costa',   13,  67, 17),
('Jean-Eric',     'Vergne',    25,  89, 18),
('Stoffel',    'Vandoorne',     4,  45, 18),
('Oliver',       'Rowland',    22,  62, 19),
('Norman',          'Nato',    17,  36, 19);

-- V2: Add WEC & IndyCar series + teams + drivers
INSERT INTO racing_series (name, official_name, headquarters, governing_body, founded_year, rounds) VALUES
('WEC', 'FIA World Endurance Championship', 'Paris, France', 'FIA / ACO', 2012, 8),
('IndyCar', 'NTT IndyCar Series', 'Indianapolis, USA', 'INDYCAR, LLC', 1996, 17);

INSERT INTO racing_teams (name, principal_name, base_location, power_unit, points, series_id) VALUES
('Toyota Gazoo Racing', 'Kamui Kobayashi', 'Cologne, Germany', 'Toyota', 132, (SELECT id FROM racing_series WHERE name = 'WEC' LIMIT 1)),
('Ferrari AF Corse', 'Antonello Coletta', 'Piacenza, Italy', 'Ferrari', 110, (SELECT id FROM racing_series WHERE name = 'WEC' LIMIT 1)),
('Porsche Penske Motorsport', 'Jonathan Diuguid', 'Mannheim, Germany', 'Porsche', 158, (SELECT id FROM racing_series WHERE name = 'WEC' LIMIT 1)),
('Heart of Racing Team', 'Ian James', 'Florida, USA', 'Aston Martin', 20, (SELECT id FROM racing_series WHERE name = 'WEC' LIMIT 1));

INSERT INTO racing_teams (name, principal_name, base_location, power_unit, points, series_id) VALUES
('Chip Ganassi Racing', 'Chip Ganassi', 'Indianapolis, USA', 'Honda', 450, (SELECT id FROM racing_series WHERE name = 'IndyCar' LIMIT 1)),
('Team Penske', 'Tim Cindric', 'Mooresville, USA', 'Chevrolet', 410, (SELECT id FROM racing_series WHERE name = 'IndyCar' LIMIT 1)),
('Arrow McLaren', 'Gavin Ward', 'Indianapolis, USA', 'Chevrolet', 280, (SELECT id FROM racing_series WHERE name = 'IndyCar' LIMIT 1)),
('PREMA Racing', 'Rene Rosin', 'Fishers, USA', 'Chevrolet', 150, (SELECT id FROM racing_series WHERE name = 'IndyCar' LIMIT 1));

INSERT INTO racing_drivers (first_name, last_name, racing_number, points, team_id) VALUES
('Nyck', 'de Vries', 7, 120, (SELECT id FROM racing_teams WHERE name = 'Toyota Gazoo Racing' LIMIT 1)),
('Kamui', 'Kobayashi', 7, 115, (SELECT id FROM racing_teams WHERE name = 'Toyota Gazoo Racing' LIMIT 1)),
('Antonio', 'Fuoco', 50, 110, (SELECT id FROM racing_teams WHERE name = 'Ferrari AF Corse' LIMIT 1)),
('Kevin', 'Estre', 6, 130, (SELECT id FROM racing_teams WHERE name = 'Porsche Penske Motorsport' LIMIT 1)),
('Alex', 'Riberas', 27, 24, (SELECT id FROM racing_teams WHERE name = 'Heart of Racing Team' LIMIT 1));

INSERT INTO racing_drivers (first_name, last_name, racing_number, points, team_id) VALUES
('Alex', 'Palou', 10, 310, (SELECT id FROM racing_teams WHERE name = 'Chip Ganassi Racing' LIMIT 1)),
('Scott', 'Dixon', 9, 290, (SELECT id FROM racing_teams WHERE name = 'Chip Ganassi Racing' LIMIT 1)),
('Josef', 'Newgarden', 2, 285, (SELECT id FROM racing_teams WHERE name = 'Team Penske' LIMIT 1)),
('Scott', 'McLaughlin', 3, 305, (SELECT id FROM racing_teams WHERE name = 'Team Penske' LIMIT 1)),
('Pato', 'O''Ward', 5, 250, (SELECT id FROM racing_teams WHERE name = 'Arrow McLaren' LIMIT 1)),
('Christian', 'Lundgaard', 7, 180, (SELECT id FROM racing_teams WHERE name = 'Arrow McLaren' LIMIT 1)),
('Callum', 'Ilott', 83, 110, (SELECT id FROM racing_teams WHERE name = 'PREMA Racing' LIMIT 1)),
('Robert', 'Shwartzman', 90, 95, (SELECT id FROM racing_teams WHERE name = 'PREMA Racing' LIMIT 1));

-- V3: Add MotoGP
INSERT INTO racing_series (name, official_name, headquarters, governing_body, founded_year, rounds) VALUES
('MotoGP', 'FIM Grand Prix World Championship', 'Madrid, Spain', 'FIM (Dorna Sports)', 1949, 21);

INSERT INTO racing_teams (name, principal_name, base_location, power_unit, points, series_id) VALUES
('Ducati Lenovo Team', 'Davide Tardozzi', 'Bologna, Italy', 'Ducati', 420, (SELECT id FROM racing_series WHERE name = 'MotoGP' LIMIT 1)),
('Prima Pramac Yamaha', 'Gino Borsoi', 'Campinola, Italy', 'Yamaha', 380, (SELECT id FROM racing_series WHERE name = 'MotoGP' LIMIT 1)),
('Aprilia Racing', 'Massimo Rivola', 'Noale, Italy', 'Aprilia', 240, (SELECT id FROM racing_series WHERE name = 'MotoGP' LIMIT 1)),
('Red Bull KTM', 'Francesco Guidotti', 'Munderfing, Austria', 'KTM', 230, (SELECT id FROM racing_series WHERE name = 'MotoGP' LIMIT 1));

INSERT INTO racing_drivers (first_name, last_name, racing_number, points, team_id) VALUES
('Francesco', 'Bagnaia', 1, 310, (SELECT id FROM racing_teams WHERE name = 'Ducati Lenovo Team' LIMIT 1)),
('Marc', 'Marquez', 93, 290, (SELECT id FROM racing_teams WHERE name = 'Ducati Lenovo Team' LIMIT 1)),
('Miguel', 'Oliveira', 88, 105, (SELECT id FROM racing_teams WHERE name = 'Prima Pramac Yamaha' LIMIT 1)),
('Jack', 'Miller', 43, 90, (SELECT id FROM racing_teams WHERE name = 'Prima Pramac Yamaha' LIMIT 1)),
('Jorge', 'Martin', 89, 290, (SELECT id FROM racing_teams WHERE name = 'Aprilia Racing' LIMIT 1)),
('Marco', 'Bezzecchi', 72, 140, (SELECT id FROM racing_teams WHERE name = 'Aprilia Racing' LIMIT 1)),
('Brad', 'Binder', 33, 160, (SELECT id FROM racing_teams WHERE name = 'Red Bull KTM' LIMIT 1)),
('Pedro', 'Acosta', 31, 135, (SELECT id FROM racing_teams WHERE name = 'Red Bull KTM' LIMIT 1));

-- V4: team_color column + color values
ALTER TABLE racing_teams ADD COLUMN IF NOT EXISTS team_color VARCHAR(10) DEFAULT '#FFFFFF';

UPDATE racing_teams SET team_color = '#FF8700' WHERE name = 'McLaren F1 Team';
UPDATE racing_teams SET team_color = '#E80020' WHERE name = 'Ferrari';
UPDATE racing_teams SET team_color = '#0600EF' WHERE name = 'Red Bull Racing';
UPDATE racing_teams SET team_color = '#27F4D2' WHERE name = 'Mercedes-AMG Petronas';
UPDATE racing_teams SET team_color = '#229971' WHERE name = 'Aston Martin Aramco';
UPDATE racing_teams SET team_color = '#B6BABD' WHERE name = 'Haas F1 Team';
UPDATE racing_teams SET team_color = '#0093CC' WHERE name = 'Alpine F1 Team';
UPDATE racing_teams SET team_color = '#64C4FF' WHERE name = 'Williams Racing';
UPDATE racing_teams SET team_color = '#6692FF' WHERE name = 'Racing Bulls';
UPDATE racing_teams SET team_color = '#FFD700' WHERE name = 'Cadillac Racing';
UPDATE racing_teams SET team_color = '#F5002C' WHERE name = 'Audi F1 Team';
UPDATE racing_teams SET team_color = '#E80020' WHERE name = 'Prema Racing';
UPDATE racing_teams SET team_color = '#821ED3' WHERE name = 'Hitech Grand Prix';
UPDATE racing_teams SET team_color = '#229971' WHERE name = 'ART Grand Prix';
UPDATE racing_teams SET team_color = '#00E1FF' WHERE name = 'DAMS';
UPDATE racing_teams SET team_color = '#FFFFFF' WHERE name = 'Jaguar TCS Racing';
UPDATE racing_teams SET team_color = '#E80020' WHERE name = 'Porsche TAG Heuer Formula E';
UPDATE racing_teams SET team_color = '#D1AB3E' WHERE name = 'DS Penske';
UPDATE racing_teams SET team_color = '#C3002F' WHERE name = 'Nissan Formula E Team';
UPDATE racing_teams SET team_color = '#ED1C24' WHERE name = 'Toyota Gazoo Racing';
UPDATE racing_teams SET team_color = '#E80020' WHERE name = 'Ferrari AF Corse';
UPDATE racing_teams SET team_color = '#B6BABD' WHERE name = 'Porsche Penske Motorsport';
UPDATE racing_teams SET team_color = '#005743' WHERE name = 'Heart of Racing Team';
UPDATE racing_teams SET team_color = '#003DA5' WHERE name = 'Chip Ganassi Racing';
UPDATE racing_teams SET team_color = '#E80020' WHERE name = 'Team Penske';
UPDATE racing_teams SET team_color = '#FF8700' WHERE name = 'Arrow McLaren';
UPDATE racing_teams SET team_color = '#E80020' WHERE name = 'PREMA Racing';
UPDATE racing_teams SET team_color = '#E80020' WHERE name = 'Ducati Lenovo Team';
UPDATE racing_teams SET team_color = '#003DA5' WHERE name = 'Prima Pramac Yamaha';
UPDATE racing_teams SET team_color = '#000000' WHERE name = 'Aprilia Racing';
UPDATE racing_teams SET team_color = '#FF6600' WHERE name = 'Red Bull KTM';

-- V5: Lab2 extra fields
ALTER TABLE racing_teams ADD COLUMN IF NOT EXISTS wins INT DEFAULT 0;
ALTER TABLE racing_teams ADD COLUMN IF NOT EXISTS podiums INT DEFAULT 0;
ALTER TABLE racing_teams ADD COLUMN IF NOT EXISTS budget_cap DOUBLE PRECISION DEFAULT 0;
ALTER TABLE racing_teams ADD COLUMN IF NOT EXISTS constructor_pos INT DEFAULT 0;
ALTER TABLE racing_teams ADD COLUMN IF NOT EXISTS chassis_model VARCHAR(255);
ALTER TABLE racing_teams ADD COLUMN IF NOT EXISTS graduates INT DEFAULT 0;
ALTER TABLE racing_teams ADD COLUMN IF NOT EXISTS is_feeder BOOLEAN DEFAULT FALSE;
ALTER TABLE racing_teams ADD COLUMN IF NOT EXISTS energy_partner VARCHAR(255);
ALTER TABLE racing_teams ADD COLUMN IF NOT EXISTS battery_kwh DOUBLE PRECISION DEFAULT 0;
ALTER TABLE racing_teams ADD COLUMN IF NOT EXISTS sustain_score INT DEFAULT 0;
