CREATE TABLE IF NOT EXISTS racing_series (
    id BIGSERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    official_name VARCHAR(255),
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
    series_id BIGINT NOT NULL REFERENCES racing_series(id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS racing_drivers (
    id BIGSERIAL PRIMARY KEY,
    first_name VARCHAR(255) NOT NULL,
    last_name VARCHAR(255) NOT NULL,
    racing_number INTEGER NOT NULL,
    points INTEGER NOT NULL DEFAULT 0,
    team_id BIGINT NOT NULL REFERENCES racing_teams(id) ON DELETE CASCADE
);
