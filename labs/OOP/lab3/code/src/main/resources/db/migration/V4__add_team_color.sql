ALTER TABLE racing_teams ADD COLUMN team_color VARCHAR(10) DEFAULT '#FFFFFF';

-- F1
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

-- F2
UPDATE racing_teams SET team_color = '#E80020' WHERE name = 'Prema Racing';
UPDATE racing_teams SET team_color = '#821ED3' WHERE name = 'Hitech Grand Prix';
UPDATE racing_teams SET team_color = '#229971' WHERE name = 'ART Grand Prix';
UPDATE racing_teams SET team_color = '#00E1FF' WHERE name = 'DAMS';

-- FE
UPDATE racing_teams SET team_color = '#FFFFFF' WHERE name = 'Jaguar TCS Racing';
UPDATE racing_teams SET team_color = '#E80020' WHERE name = 'Porsche TAG Heuer Formula E';
UPDATE racing_teams SET team_color = '#D1AB3E' WHERE name = 'DS Penske';
UPDATE racing_teams SET team_color = '#C3002F' WHERE name = 'Nissan Formula E Team';

-- WEC / Indy / MotoGP (from previous migrations)
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
