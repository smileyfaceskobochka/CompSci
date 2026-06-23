CREATE TABLE IF NOT EXISTS f1_teams (
    id BIGINT PRIMARY KEY REFERENCES racing_teams(id) ON DELETE CASCADE,
    power_unit VARCHAR(255),
    budget_cap DOUBLE PRECISION DEFAULT 0.0,
    constructor_pos INTEGER DEFAULT 0
);

CREATE TABLE IF NOT EXISTS f2_teams (
    id BIGINT PRIMARY KEY REFERENCES racing_teams(id) ON DELETE CASCADE,
    chassis_model VARCHAR(255),
    graduates INTEGER NOT NULL DEFAULT 0,
    is_feeder BOOLEAN DEFAULT FALSE
);

CREATE TABLE IF NOT EXISTS formula_e_teams (
    id BIGINT PRIMARY KEY REFERENCES racing_teams(id) ON DELETE CASCADE,
    energy_partner VARCHAR(255),
    battery_kwh DOUBLE PRECISION DEFAULT 0.0,
    sustain_score INTEGER DEFAULT 0
);

-- Copy existing data from flat table to subclasses tables
INSERT INTO f1_teams (id, power_unit, budget_cap, constructor_pos)
SELECT id, power_unit, budget_cap, constructor_pos FROM racing_teams WHERE series_id = 1
ON CONFLICT (id) DO NOTHING;

INSERT INTO f2_teams (id, chassis_model, graduates, is_feeder)
SELECT id, chassis_model, graduates, is_feeder FROM racing_teams WHERE series_id = 2
ON CONFLICT (id) DO NOTHING;

INSERT INTO formula_e_teams (id, energy_partner, battery_kwh, sustain_score)
SELECT id, energy_partner, battery_kwh, sustain_score FROM racing_teams WHERE series_id = 3
ON CONFLICT (id) DO NOTHING;

-- Drop obsolete subclass columns from the parent table
ALTER TABLE racing_teams 
    DROP COLUMN IF EXISTS power_unit,
    DROP COLUMN IF EXISTS budget_cap,
    DROP COLUMN IF EXISTS constructor_pos,
    DROP COLUMN IF EXISTS chassis_model,
    DROP COLUMN IF EXISTS graduates,
    DROP COLUMN IF EXISTS is_feeder,
    DROP COLUMN IF EXISTS energy_partner,
    DROP COLUMN IF EXISTS battery_kwh,
    DROP COLUMN IF EXISTS sustain_score;
