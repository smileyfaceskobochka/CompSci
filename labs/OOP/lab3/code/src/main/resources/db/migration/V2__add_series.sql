INSERT INTO racing_series (name, official_name, headquarters, governing_body, founded_year, rounds) VALUES
('WEC', 'FIA World Endurance Championship', 'Paris, France', 'FIA / ACO', 2012, 8),
('IndyCar', 'NTT IndyCar Series', 'Indianapolis, USA', 'INDYCAR, LLC', 1996, 17);

-- Insert WEC Teams
INSERT INTO racing_teams (name, principal_name, base_location, power_unit, points, series_id) VALUES
('Toyota Gazoo Racing', 'Kamui Kobayashi', 'Cologne, Germany', 'Toyota', 132, (SELECT id FROM racing_series WHERE name = 'WEC')),
('Ferrari AF Corse', 'Antonello Coletta', 'Piacenza, Italy', 'Ferrari', 110, (SELECT id FROM racing_series WHERE name = 'WEC')),
('Porsche Penske Motorsport', 'Jonathan Diuguid', 'Mannheim, Germany', 'Porsche', 158, (SELECT id FROM racing_series WHERE name = 'WEC')),
('Heart of Racing Team', 'Ian James', 'Florida, USA', 'Aston Martin', 20, (SELECT id FROM racing_series WHERE name = 'WEC'));

-- Insert IndyCar Teams
INSERT INTO racing_teams (name, principal_name, base_location, power_unit, points, series_id) VALUES
('Chip Ganassi Racing', 'Chip Ganassi', 'Indianapolis, USA', 'Honda', 450, (SELECT id FROM racing_series WHERE name = 'IndyCar')),
('Team Penske', 'Tim Cindric', 'Mooresville, USA', 'Chevrolet', 410, (SELECT id FROM racing_series WHERE name = 'IndyCar')),
('Arrow McLaren', 'Gavin Ward', 'Indianapolis, USA', 'Chevrolet', 280, (SELECT id FROM racing_series WHERE name = 'IndyCar')),
('PREMA Racing', 'Rene Rosin', 'Fishers, USA', 'Chevrolet', 150, (SELECT id FROM racing_series WHERE name = 'IndyCar'));

-- Insert WEC Drivers
INSERT INTO racing_drivers (first_name, last_name, racing_number, points, team_id) VALUES
('Nyck', 'de Vries', 7, 120, (SELECT id FROM racing_teams WHERE name = 'Toyota Gazoo Racing' LIMIT 1)),
('Kamui', 'Kobayashi', 7, 115, (SELECT id FROM racing_teams WHERE name = 'Toyota Gazoo Racing' LIMIT 1)),
('Antonio', 'Fuoco', 50, 110, (SELECT id FROM racing_teams WHERE name = 'Ferrari AF Corse' LIMIT 1)),
('Kevin', 'Estre', 6, 130, (SELECT id FROM racing_teams WHERE name = 'Porsche Penske Motorsport' LIMIT 1)),
('Alex', 'Riberas', 27, 24, (SELECT id FROM racing_teams WHERE name = 'Heart of Racing Team' LIMIT 1));

-- Insert IndyCar Drivers
INSERT INTO racing_drivers (first_name, last_name, racing_number, points, team_id) VALUES
('Alex', 'Palou', 10, 310, (SELECT id FROM racing_teams WHERE name = 'Chip Ganassi Racing' LIMIT 1)),
('Scott', 'Dixon', 9, 290, (SELECT id FROM racing_teams WHERE name = 'Chip Ganassi Racing' LIMIT 1)),
('Josef', 'Newgarden', 2, 285, (SELECT id FROM racing_teams WHERE name = 'Team Penske' LIMIT 1)),
('Scott', 'McLaughlin', 3, 305, (SELECT id FROM racing_teams WHERE name = 'Team Penske' LIMIT 1)),
('Pato', 'O''Ward', 5, 250, (SELECT id FROM racing_teams WHERE name = 'Arrow McLaren' LIMIT 1)),
('Christian', 'Lundgaard', 7, 180, (SELECT id FROM racing_teams WHERE name = 'Arrow McLaren' LIMIT 1)),
('Callum', 'Ilott', 83, 110, (SELECT id FROM racing_teams WHERE name = 'PREMA Racing' LIMIT 1)),
('Robert', 'Shwartzman', 90, 95, (SELECT id FROM racing_teams WHERE name = 'PREMA Racing' LIMIT 1));
