-- Insert MotoGP Series
INSERT INTO racing_series (name, official_name, headquarters, governing_body, founded_year, rounds) VALUES
('MotoGP', 'FIM Grand Prix World Championship', 'Madrid, Spain', 'FIM (Dorna Sports)', 1949, 21);

-- Insert MotoGP Teams
INSERT INTO racing_teams (name, principal_name, base_location, power_unit, points, series_id) VALUES
('Ducati Lenovo Team', 'Davide Tardozzi', 'Bologna, Italy', 'Ducati', 420, (SELECT id FROM racing_series WHERE name = 'MotoGP')),
('Prima Pramac Yamaha', 'Gino Borsoi', 'Campinola, Italy', 'Yamaha', 380, (SELECT id FROM racing_series WHERE name = 'MotoGP')),
('Aprilia Racing', 'Massimo Rivola', 'Noale, Italy', 'Aprilia', 240, (SELECT id FROM racing_series WHERE name = 'MotoGP')),
('Red Bull KTM', 'Francesco Guidotti', 'Munderfing, Austria', 'KTM', 230, (SELECT id FROM racing_series WHERE name = 'MotoGP'));

-- Insert MotoGP Riders (Drivers)
INSERT INTO racing_drivers (first_name, last_name, racing_number, points, team_id) VALUES
('Francesco', 'Bagnaia', 1, 310, (SELECT id FROM racing_teams WHERE name = 'Ducati Lenovo Team' LIMIT 1)),
('Marc', 'Marquez', 93, 290, (SELECT id FROM racing_teams WHERE name = 'Ducati Lenovo Team' LIMIT 1)),
('Miguel', 'Oliveira', 88, 105, (SELECT id FROM racing_teams WHERE name = 'Prima Pramac Yamaha' LIMIT 1)),
('Jack', 'Miller', 43, 90, (SELECT id FROM racing_teams WHERE name = 'Prima Pramac Yamaha' LIMIT 1)),
('Jorge', 'Martin', 89, 290, (SELECT id FROM racing_teams WHERE name = 'Aprilia Racing' LIMIT 1)),
('Marco', 'Bezzecchi', 72, 140, (SELECT id FROM racing_teams WHERE name = 'Aprilia Racing' LIMIT 1)),
('Brad', 'Binder', 33, 160, (SELECT id FROM racing_teams WHERE name = 'Red Bull KTM' LIMIT 1)),
('Pedro', 'Acosta', 31, 135, (SELECT id FROM racing_teams WHERE name = 'Red Bull KTM' LIMIT 1));
