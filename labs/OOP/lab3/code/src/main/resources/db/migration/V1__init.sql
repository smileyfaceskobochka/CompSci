CREATE TABLE racing_series (
    id BIGSERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    official_name VARCHAR(512),
    headquarters VARCHAR(255) NOT NULL,
    governing_body VARCHAR(255),
    founded_year INTEGER,
    rounds INTEGER
);

CREATE TABLE racing_teams (
    id BIGSERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    principal_name VARCHAR(255) NOT NULL,
    base_location VARCHAR(255) NOT NULL,
    power_unit VARCHAR(255),
    points INTEGER NOT NULL DEFAULT 0,
    series_id BIGINT NOT NULL REFERENCES racing_series(id)
);

CREATE TABLE racing_drivers (
    id BIGSERIAL PRIMARY KEY,
    first_name VARCHAR(255) NOT NULL,
    last_name VARCHAR(255) NOT NULL,
    racing_number INTEGER NOT NULL,
    points INTEGER NOT NULL DEFAULT 0,
    team_id BIGINT NOT NULL REFERENCES racing_teams(id)
);

-- Series
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

-- F1 Teams (series_id=1) — constructors points = sum of both drivers
INSERT INTO racing_teams (name, principal_name, base_location, power_unit, points, series_id) VALUES
('McLaren F1 Team',      'Andrea Stella',    'Woking, UK',              'Mercedes',  291, 1),   -- 1st
('Ferrari',              'Fred Vasseur',     'Maranello, Italy',        'Ferrari',   249, 1),   -- 2nd
('Red Bull Racing',      'Christian Horner', 'Milton Keynes, UK',       'Ford RBPT', 206, 1),   -- 3rd
('Mercedes-AMG Petronas','Toto Wolff',       'Brackley, UK',            'Mercedes',  138, 1),   -- 4th
('Aston Martin Aramco',  'Mike Krack',       'Silverstone, UK',         'Honda',      96, 1),   -- 5th
('Haas F1 Team',         'Ayao Komatsu',     'Kannapolis, USA',         'Ferrari',    58, 1),   -- 6th
('Alpine F1 Team',       'Oliver Oakes',     'Enstone, UK',             'Renault',    49, 1),   -- 7th
('Williams Racing',      'James Vowles',     'Grove, UK',               'Mercedes',   17, 1),   -- 8th
('Racing Bulls',         'Laurent Mekies',   'Faenza, Italy',           'Ford RBPT',  14, 1),   -- 9th
('Cadillac Racing',      'Michael Andretti', 'Fishers, USA',            'Cadillac',    6, 1),   -- 10th
('Audi F1 Team',         'Mattia Binotto',   'Hinwil, Switzerland',     'Audi',        4, 1);   -- 11th

-- F2 Teams (series_id=2)
INSERT INTO racing_teams (name, principal_name, base_location, points, series_id) VALUES
('Prema Racing',         'René Rosin',        'Grisignano, Italy',                 89, 2),
('Hitech Grand Prix',    'Oliver Oakes',      'Silverstone, UK',                   74, 2),
('ART Grand Prix',       'Sébastien Philippe','Villeneuve-la-Garenne, France',      58, 2),
('DAMS',                 'François Sicard',   'Le Mans, France',                    41, 2);

-- FE Teams (series_id=3)
INSERT INTO racing_teams (name, principal_name, base_location, points, series_id) VALUES
('Jaguar TCS Racing',          'James Barclay',      'Silverstone, UK',          201, 3),
('Porsche TAG Heuer Formula E', 'Florian Modlinger', 'Weissach, Germany',        178, 3),
('DS Penske',                  'Mark Preston',       'Versailles, France',        134, 3),
('Nissan Formula E Team',      'Tommaso Volpe',      'Yokohama, Japan',            98, 3);

-- F1 Drivers (scoring: 25-18-15-12-10-8-6-4-2-1 per race, ~8 races done)
-- Team IDs: McLaren=1, Ferrari=2, Red Bull=3, Mercedes=4, Aston Martin=5, Haas=6, Alpine=7, Williams=8, Racing Bulls=9, Cadillac=10, Audi=11
INSERT INTO racing_drivers (first_name, last_name, racing_number, points, team_id) VALUES
('Oscar',        'Piastri',    81, 168, 1),   -- P1 WDC 
('Lando',        'Norris',      4, 123, 1),   -- P2
('Charles',      'Leclerc',    16, 141, 2),   -- P3
('Lewis',        'Hamilton',   44, 108, 2),   -- P4
('Max',          'Verstappen',  1, 127, 3),   -- P5
('Isack',        'Hadjar',      6,  79, 3),   -- P6
('George',       'Russell',    63,  98, 4),   -- P7
('Kimi',         'Antonelli',  12,  40, 4),   -- P8
('Fernando',     'Alonso',     14,  62, 5),   -- P9
('Lance',        'Stroll',     18,  34, 5),   -- P10
('Esteban',      'Ocon',       31,  33, 6),   -- P11
('Oliver',       'Bearman',    87,  25, 6),   -- P12
('Pierre',       'Gasly',      10,  28, 7),   -- P13
('Franco',       'Colapinto',  43,  21, 7),   -- P14
('Alexander',    'Albon',      23,  10, 8),   -- P15
('Carlos',       'Sainz',      55,   7, 8),   -- P16
('Liam',         'Lawson',     30,   9, 9),   -- P17
('Arvid',        'Lindblad',    6,   5, 9),   -- P18
('Sergio',       'Perez',      11,   6,10),   -- P19
('Valtteri',     'Bottas',     77,   0,10),   -- P20
('Nico',         'Hulkenberg', 27,   4,11),   -- P21
('Gabriel',      'Bortoleto',   5,   0,11);   -- P22

-- F2 Drivers (~6 rounds done, scoring 10-9-8-7-6-5-4-3-2-1)
INSERT INTO racing_drivers (first_name, last_name, racing_number, points, team_id) VALUES
('Paul',           'Aron',       7,  56, 12),
('Ugo',         'Ugochukwu',    3,  33, 12),
('Jak',          'Crawford',    8,  44, 13),
('Isack',          'Hadjar',    6,  30, 13),
('James',         'Wharton',    1,  35, 14),
('Tuukka',        'Taponen',    2,  23, 14),
('Oliver',        'Bearman',    5,  28, 15),
('Victor',         'Martins',   6,  13, 15);

-- FE Drivers (~10 rounds done, scoring 25-18-15-12-10-8-6-4-2-1)
INSERT INTO racing_drivers (first_name, last_name, racing_number, points, team_id) VALUES
('Nick',         'Cassidy',    37, 118, 16),
('Mitch',          'Evans',     9,  83, 16),
('Pascal',       'Wehrlein',   94, 111, 17),
('Antonio Felix','da Costa',   13,  67, 17),
('Jean-Eric',     'Vergne',    25,  89, 18),
('Stoffel',    'Vandoorne',     4,  45, 18),
('Oliver',       'Rowland',    22,  62, 19),
('Norman',          'Nato',    17,  36, 19);
